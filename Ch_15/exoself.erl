%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(exoself).
-compile(export_all).
-include("records.hrl").
-record(state,{
	agent_id,
	generation,
	pm_pid,
	idsNpids,
	cx_pid,
	specie_id,
	spids=[],
	npids=[],
	nids=[],
	apids=[],
	scape_pids=[],
	highest_fitness=-1,
	eval_acc=0,
	cycle_acc=0,
	time_acc=0,
	max_attempts=10,
	attempt=1,
	tuning_duration_f,
	tuning_selection_f,
	annealing_parameter,
	perturbation_range
}).

start(Agent_Id)->
	case whereis(monitor) of
		undefined ->
			io:format("start(Agent_Id):: 'monitor' is not registered~n");
		PId ->
			start(Agent_Id,PId)
	end.
start(Agent_Id,PM_PId)->
	spawn(exoself,prep,[Agent_Id,PM_PId]).
%The start/2 function spawns a new Agent_Id exoself process, belonging to the population_monitor process with the pid PM_PId.

prep(Agent_Id,PM_PId)->
	random:seed(now()),
	IdsNPIds = ets:new(idsNpids,[set,private]), 
	A = genotype:dirty_read({agent,Agent_Id}),
	HeredityType = A#agent.heredity_type,
	Cx = genotype:dirty_read({cortex,A#agent.cx_id}),
	SIds = Cx#cortex.sensor_ids,
	AIds = Cx#cortex.actuator_ids,
	NIds = Cx#cortex.neuron_ids,
	ScapePIds = spawn_Scapes(IdsNPIds,SIds,AIds),
	spawn_CerebralUnits(IdsNPIds,cortex,[Cx#cortex.id]),
	spawn_CerebralUnits(IdsNPIds,sensor,SIds),
	spawn_CerebralUnits(IdsNPIds,actuator,AIds),
	spawn_CerebralUnits(IdsNPIds,neuron,NIds),
	link_Sensors(SIds,IdsNPIds),
	link_Actuators(AIds,IdsNPIds),
	link_Neurons(NIds,IdsNPIds,HeredityType),
	{SPIds,NPIds,APIds}=link_Cortex(Cx,IdsNPIds),
	Cx_PId = ets:lookup_element(IdsNPIds,Cx#cortex.id,2),
	{TuningDurationFunction,Parameter} = A#agent.tuning_duration_f,
	S = #state{
		agent_id=Agent_Id,
		generation=A#agent.generation,
		pm_pid=PM_PId,
		idsNpids=IdsNPIds,
		cx_pid=Cx_PId,
		specie_id=A#agent.specie_id,
		spids=SPIds,
		npids=NPIds,
		nids=NIds,
		apids=APIds,
		scape_pids=ScapePIds,
		max_attempts= tuning_duration:TuningDurationFunction(Parameter,NIds,A#agent.generation),
		tuning_selection_f=A#agent.tuning_selection_f,
		annealing_parameter=A#agent.annealing_parameter,
		tuning_duration_f=A#agent.tuning_duration_f,
		perturbation_range=A#agent.perturbation_range
	},
	loop(S).
%The prep/2 function prepares and sets up the exoself's state before dropping into the main loop. The function first reads the agent and cortex records belonging to the Agent_Id NN based system. The function then reads the sensor, actuator, and neuron ids, then spawns the private scapes using the spawn_Scapes/3 function, spawns the cortex, sensor, actuator, and neuron processes, and then finally links up all these processes together using the link_.../2 processes. Once the phenotype has been generated from the genotype, the exoself drops into its main loop.

loop(S)->
	receive
		{Cx_PId,evaluation_completed,Fitness,Cycles,Time,GoalReachedFlag}->
			%io:format("E Msg:~p~n E S:~p~n",[{Cx_PId,evaluation_completed,Fitness,Cycles,Time,GoalReachedFlag},S]),
			IdsNPIds = S#state.idsNpids,
			{U_HighestFitness,U_Attempt}=case Fitness > S#state.highest_fitness of
				true ->
					[NPId ! {self(),weight_backup} || NPId <- S#state.npids],
					{Fitness,0};
				false ->
					Perturbed_NIdPs=get(perturbed),
					[ets:lookup_element(IdsNPIds,NId,2) ! {self(),weight_restore} || {NId,_Spread} <- Perturbed_NIdPs],
					{S#state.highest_fitness,S#state.attempt+1}
			end,
			[PId ! {self(), reset_prep} || PId <- S#state.npids],
			gather_acks(length(S#state.npids)),
			[PId ! {self(), reset} || PId <- S#state.npids],
			%io:format("HighestFitness:~p U_Attempt:~p~n",[U_HighestFitness,U_Attempt]),
			U_CycleAcc = S#state.cycle_acc+Cycles,
			U_TimeAcc = S#state.time_acc+Time,
			U_EvalAcc = S#state.eval_acc+1,
			gen_server:cast(S#state.pm_pid,{self(),evaluations,S#state.specie_id,1,Cycles,Time}),
			case (U_Attempt >= S#state.max_attempts) or (GoalReachedFlag == true) of
				true ->	%End training
					A=genotype:dirty_read({agent,S#state.agent_id}),
					genotype:write(A#agent{fitness=U_HighestFitness}),
					backup_genotype(S#state.idsNpids,S#state.npids),
					terminate_phenotype(S#state.cx_pid,S#state.spids,S#state.npids,S#state.apids,S#state.scape_pids),
					io:format("Agent:~p terminating. Genotype has been backed up.~n Fitness:~p~n TotEvaluations:~p~n TotCycles:~p~n TimeAcc:~p~n",[self(),U_HighestFitness,U_EvalAcc,U_CycleAcc,U_TimeAcc]),
					case GoalReachedFlag of
						true ->
							gen_server:cast(S#state.pm_pid,{S#state.agent_id,goal_reached});
						_ ->
							ok
					end,
					gen_server:cast(S#state.pm_pid,{S#state.agent_id,terminated,U_HighestFitness});
				false -> %Continue training
					%io:format("exoself state:~p~n",[S]),
					TuningSelectionFunction=S#state.tuning_selection_f,
					PerturbationRange = S#state.perturbation_range,
					AnnealingParameter = S#state.annealing_parameter,
					ChosenNIdPs=tuning_selection:TuningSelectionFunction(S#state.nids,S#state.generation,PerturbationRange,AnnealingParameter),
					[ets:lookup_element(IdsNPIds,NId,2) ! {self(),weight_perturb,Spread} || {NId,Spread} <- ChosenNIdPs],
					%io:format("ChosenNPIds:~p~n",[ChosenNIdPs]),
					put(perturbed,ChosenNIdPs),
					Cx_PId ! {self(),reactivate},
					U_S =S#state{
						cycle_acc=U_CycleAcc,
						time_acc=U_TimeAcc,
						eval_acc=U_EvalAcc,
						attempt=U_Attempt,
						highest_fitness=U_HighestFitness
					},
					exoself:loop(U_S)
			end
		%after 10000 ->
		%	io:format("exoself:~p stuck.~n",[S#state.agent_id])
	end.
%The exoself process' main loop awaits from its cortex proccess the evoluation_completed message. Once the message is received, based on the fitness achieved, exoself decides whether to continue tunning the weights or terminate the system. Exoself tries to improve the fitness by perturbing/tuning the weights of its neurons, after each tuning session, the Neural Network based system performs another evaluation by interacting with the scape until completion (the NN solves a problem, or dies within the scape or...). The order of events is important: When evaluation_completed message is received, the function first checks whether the newly achieved fitness is higher than the highest fitness achieved so far. If it is not, the function sends the neurons a message to restore their weights to previous state, during which it last acehived the highest fitness instead of their current state which yielded the current lower fitness score. If on the other hand the new fitness is higher than the previously highest achieved fitness, then the function tells the neurons to backup their current weights, as these weights represent the NN's best, most fit form yet. Exoself then tells all the neurons to prepare for a reset by sending each neuron the {self(),reset_prep} message. Since the NN can have recursive connections, and the manner in which initial recursive messages are sent, it is important for each neuron to flush their buffers to be reset into an initial fresh state, which is achieved after the neurons receive the reset_prep message. The function then sends the reset message to the neurons, which returns them into their main loop. Finally, the function checks whether exoself has already tried to improve the NN's fitness a maximum S#state.max_attempts number of times. If that is the case, the exoself process backs up the updated NN (the updated, tuned weights) to database using the backup_genotype/2 function, prints to screen that it is terminating, and sends to the population_monitor the acumulated statistics (highest fitness, evaluation count, cycle count...). On the other hand, if the exoself is not yet done tuning the neural weights, it has not yet reached its ending condition, it uses a tuning_selection_function to compose a list of tuples: [{NId,Spread}...] of neuron ids and the perturbation spread values, where the spread is the range from which the perturbation is randomly chosen. The spread itself is based on the age of the slected neuron, using the annealing_factor value, which when set to 1 implies that there is no annealing, and when set to a value less than 1, decreases the Spread. Once this list of elements is composed, the exoself sends each of the neurons a message to perturb their synaptic weights using the Spread value. The exoself then reactivates the cortex, and drops back into its main loop.

	spawn_CerebralUnits(IdsNPIds,CerebralUnitType,[Id|Ids])-> 
		PId = CerebralUnitType:gen(self(),node()),
		ets:insert(IdsNPIds,{Id,PId}), 
		ets:insert(IdsNPIds,{PId,Id}), 
		spawn_CerebralUnits(IdsNPIds,CerebralUnitType,Ids); 
	spawn_CerebralUnits(IdsNPIds,_CerebralUnitType,[])-> 
		ets:insert(IdsNPIds,{bias,bias}).
%We spawn the process for each element based on its type: CerebralUnitType, and the gen function that belongs to the CerebralUnitType module. We then enter the {Id,PId} tuple into our ETS table for later use.

	spawn_Scapes(IdsNPIds,Sensor_Ids,Actuator_Ids)-> 
		Sensor_Scapes = [(genotype:dirty_read({sensor,Id}))#sensor.scape || Id<-Sensor_Ids], 
		Actuator_Scapes = [(genotype:dirty_read({actuator,Id}))#actuator.scape || Id<-Actuator_Ids], 
		Unique_Scapes = Sensor_Scapes++(Actuator_Scapes--Sensor_Scapes), 
		SN_Tuples=[{scape:gen(self(),node()),ScapeName} || {private,ScapeName}<-Unique_Scapes], 
		[ets:insert(IdsNPIds,{ScapeName,PId}) || {PId,ScapeName} <- SN_Tuples], 
		[ets:insert(IdsNPIds,{PId,ScapeName}) || {PId,ScapeName} <-SN_Tuples], 
		[PId ! {self(),ScapeName} || {PId,ScapeName} <- SN_Tuples],
		[PId || {PId,_ScapeName} <-SN_Tuples].
%The spawn_Scapes/3 function first extracts all the scapes that the sensors and actuators interface with, it then creates a filtered scape list which only holds unique scape records, after which it further only selects those scapes that are private, and spawns them.

	link_Sensors([SId|Sensor_Ids],IdsNPIds) ->
		S=genotype:dirty_read({sensor,SId}),
		SPId = ets:lookup_element(IdsNPIds,SId,2),
		Cx_PId = ets:lookup_element(IdsNPIds,S#sensor.cx_id,2),
		SName = S#sensor.name,
		Fanout_Ids = S#sensor.fanout_ids,
		Fanout_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Fanout_Ids],
		Scape=case S#sensor.scape of
			{private,ScapeName}->
				ets:lookup_element(IdsNPIds,ScapeName,2)
		end,
		SPId ! {self(),{SId,Cx_PId,Scape,SName,S#sensor.vl,S#sensor.parameters,Fanout_PIds}},
		link_Sensors(Sensor_Ids,IdsNPIds);
	link_Sensors([],_IdsNPIds)->
		ok.
%The link_Sensors/2 function sends to the already spawned and waiting sensors their states, composed of the PId lists and other information which are needed by the sensors to link up and interface with other elements in the distributed phenotype.

	link_Actuators([AId|Actuator_Ids],IdsNPIds) ->
		A=genotype:dirty_read({actuator,AId}),
		APId = ets:lookup_element(IdsNPIds,AId,2),
		Cx_PId = ets:lookup_element(IdsNPIds,A#actuator.cx_id,2),
		AName = A#actuator.name,
		Fanin_Ids = A#actuator.fanin_ids,
		Fanin_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Fanin_Ids],
		Scape=case A#actuator.scape of
			{private,ScapeName}->
				ets:lookup_element(IdsNPIds,ScapeName,2)
		end,
		APId ! {self(),{AId,Cx_PId,Scape,AName,A#actuator.parameters,Fanin_PIds}},
		link_Actuators(Actuator_Ids,IdsNPIds);
	link_Actuators([],_IdsNPIds)->
		ok.
%The link_Actuators2 function sends to the already spawned and waiting actuators their states, composed of the PId lists and other information which are needed by the actuators to link up and interface with other elements in the distributed phenotype.

	link_Neurons([NId|Neuron_Ids],IdsNPIds,HeredityType) ->
		N=genotype:dirty_read({neuron,NId}),
		NPId = ets:lookup_element(IdsNPIds,NId,2),
		Cx_PId = ets:lookup_element(IdsNPIds,N#neuron.cx_id,2),
		AFName = N#neuron.af,
		PFName = N#neuron.pf,
		AggrFName = N#neuron.aggr_f,
		Input_IdPs = N#neuron.input_idps,
		Input_IdPs_Modulation = N#neuron.input_idps_modulation,
		Output_Ids = N#neuron.output_ids,
		RO_Ids = N#neuron.ro_ids,
		SI_PIdPs = convert_IdPs2PIdPs(IdsNPIds,Input_IdPs,[]),
		MI_PIdPs = convert_IdPs2PIdPs(IdsNPIds,Input_IdPs_Modulation,[]),
		O_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Output_Ids],
		RO_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- RO_Ids],
		NPId ! {self(),{NId,Cx_PId,AFName,PFName,AggrFName,HeredityType,SI_PIdPs,MI_PIdPs,O_PIds,RO_PIds}},
		link_Neurons(Neuron_Ids,IdsNPIds,HeredityType);
	link_Neurons([],_IdsNPIds,HeredityType)->
		ok.
%The link_Neurons/2 function sends to the already spawned and waiting neurons their states, composed of the PId lists and other information needed by the neurons to link up and interface with other elements in the distributed phenotype.

		convert_IdPs2PIdPs(IdsNPIds,[{Id,WeightsP}|Fanin_IdPs],Acc)->
			convert_IdPs2PIdPs(IdsNPIds,Fanin_IdPs,[{ets:lookup_element(IdsNPIds,Id,2),WeightsP}|Acc]);
		convert_IdPs2PIdPs(_IdsNPIds,[],Acc)->
			lists:reverse(Acc).
%The convert_IdPs2PIdPs/3 converts the IdPs tuples into tuples that use PIds instead of Ids, such that the Neuron will know which weights are to be associated with which incoming vector signals. The last element is the bias, which is added to the list in a non tuple form. Afterwards, the list is reversed to take its proper order.

	link_Cortex(Cx,IdsNPIds) ->
		Cx_Id = Cx#cortex.id,
		Cx_PId = ets:lookup_element(IdsNPIds,Cx_Id,2),
		SIds = Cx#cortex.sensor_ids,
		AIds = Cx#cortex.actuator_ids,
		NIds = Cx#cortex.neuron_ids,
		SPIds = [ets:lookup_element(IdsNPIds,SId,2) || SId <- SIds],
		NPIds = [ets:lookup_element(IdsNPIds,NId,2) || NId <- NIds],
		APIds = [ets:lookup_element(IdsNPIds,AId,2) || AId <- AIds],
		Cx_PId ! {self(),Cx_Id,SPIds,NPIds,APIds},
		{SPIds,NPIds,APIds}.
%The link_Cortex/2 function sends to the already spawned and waiting cortex its state, composed of the PId lists and other information which is needed by the cortex to link up and interface with other elements in the distributed phenotype.

backup_genotype(IdsNPIds,NPIds)->
	Neuron_IdsNWeights = get_backup(NPIds,[]),
	update_genotype(IdsNPIds,Neuron_IdsNWeights),
	io:format("Finished updating genotype~n").

	get_backup([NPId|NPIds],Acc)->
		NPId ! {self(),get_backup},
		receive
			{NPId,NId,SWeightTuples,MWeightTuples}->
				get_backup(NPIds,[{NId,SWeightTuples,MWeightTuples}|Acc])
		end;
	get_backup([],Acc)->
		Acc.
%The backup_genotype/2 uses get_backup/2 to contact all the neurons in its NN and request for the neuron's Ids and their Input_IdPs. Once the updated Input_IdPs from all the neurons have been accumulated, they are passed through the update_genotype/2 function to produce updated neurons, and write them to database.

	update_genotype(IdsNPIds,[{N_Id,SI_PIdPs,MI_PIdPs}|WeightPs])->
		N = genotype:dirty_read({neuron,N_Id}),
		Updated_SI_IdPs = convert_PIdPs2IdPs(IdsNPIds,SI_PIdPs,[]),
		Updated_MI_IdPs = convert_PIdPs2IdPs(IdsNPIds,MI_PIdPs,[]),
		U_N = N#neuron{input_idps = Updated_SI_IdPs,input_idps_modulation=Updated_MI_IdPs},
		genotype:write(U_N),
		%io:format("N:~p~n U_N:~p~n Genotype:~p~n U_Genotype:~p~n",[N,U_N,Genotype,U_Genotype]),
		update_genotype(IdsNPIds,WeightPs);
	update_genotype(_IdsNPIds,[])->
		ok.
%For every {N_Id,PIdPs} tuple the update_genotype/3 function extracts the neuron with the id: N_Id, updates the neuron's input_IdPs, and writes the updated neuron to database.

		convert_PIdPs2IdPs(IdsNPIds,[{PId,WeightsP}|Input_PIdPs],Acc)->
			convert_PIdPs2IdPs(IdsNPIds,Input_PIdPs,[{ets:lookup_element(IdsNPIds,PId,2),WeightsP}|Acc]);
		convert_PIdPs2IdPs(_IdsNPIds,[],Acc)->
			lists:reverse(Acc).
%The convert_PIdPs2IdPs/3 performs the conversion from PIds to Ids of every {PId,Weights} tuple in the Input_PIdPs list. The updated Input_IdPs are then returned to the caller.
	
terminate_phenotype(Cx_PId,SPIds,NPIds,APIds,ScapePIds)->
	io:format("Terminating the phenotype:~nCx_PId:~p~nSPIds:~p~nNPIds:~p~nAPIds:~p~nScapePids:~p~n",[Cx_PId,SPIds,NPIds,APIds,ScapePIds]),
	[PId ! {self(),terminate} || PId <- SPIds],
	[PId ! {self(),terminate} || PId <- APIds],
	[PId ! {self(),terminate} || PId <- NPIds],
	[PId ! {self(),terminate} || PId <- ScapePIds],
	Cx_PId ! {self(),terminate}.
%The terminate_phenotype/5 function termiantes sensors, actuators, neurons, all private scapes, and the cortex which composes the NN based system.

gather_acks(0)->
	done;	
gather_acks(PId_Index)->
	receive
		{_From,ready}->
			gather_acks(PId_Index-1)
		after 100000 ->
			io:format("******** Not all acks received:~p~n",[PId_Index])
	end.
%gather_acks/1 ensures that the X number of {From,ready} messages are sent to it, before it returns with done. X is set by the caller of the function.
