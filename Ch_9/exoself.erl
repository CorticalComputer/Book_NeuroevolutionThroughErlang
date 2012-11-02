%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(exoself).
-compile(export_all).
-include("records.hrl").
-record(state,{file_name,genotype,idsNpids,cx_pid,spids,npids,apids,highest_fitness,tot_evaluations,tot_cycles}).
-define(MAX_ATTEMPTS,10).

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
	link_Neurons(NIds,IdsNPIds),
	{SPIds,NPIds,APIds}=link_Cortex(Cx,IdsNPIds),
	Cx_PId = ets:lookup_element(IdsNPIds,Cx#cortex.id,2),
	loop(Agent_Id,PM_PId,IdsNPIds,Cx_PId,SPIds,NPIds,APIds,ScapePIds,0,0,0,0,1).
%The prep/2 function prepares and sets up the exoself's state before dropping into the main loop. The function first reads the agent and cortex records belonging to the Agent_Id NN based system. The function then reads the sensor, actuator, and neuron ids, then spawns the private scapes using the spawn_Scapes/3 function, spawns the cortex, sensor, actuator, and neuron processes, and then finally links up all these processes together using the link_.../2 processes. Once the phenotype has been generated from the genotype, the exoself drops into its main loop.

loop(Agent_Id,PM_PId,IdsNPIds,Cx_PId,SPIds,NPIds,APIds,ScapePIds,HighestFitness,EvalAcc,CycleAcc,TimeAcc,Attempt)->
	receive
		{Cx_PId,evaluation_completed,Fitness,Cycles,Time}->
			{U_HighestFitness,U_Attempt}=case Fitness > HighestFitness of
				true ->
					[NPId ! {self(),weight_backup} || NPId <- NPIds],
					{Fitness,0};
				false ->
					Perturbed_NPIds=get(perturbed),
					[NPId ! {self(),weight_restore} || NPId <- Perturbed_NPIds],
					{HighestFitness,Attempt+1}
			end,
			[PId ! {self(), reset_prep} || PId <- NPIds],
			gather_acks(length(NPIds)),
			[PId ! {self(), reset} || PId <- NPIds],
			%io:format("HighestFitness:~p U_Attempt:~p~n",[HighestFitness,U_Attempt]),
			case U_Attempt >= ?MAX_ATTEMPTS of
				true ->	%End training
					U_CycleAcc = CycleAcc+Cycles,
					U_TimeAcc = TimeAcc+Time,
					A=genotype:dirty_read({agent,Agent_Id}),
					genotype:write(A#agent{fitness=U_HighestFitness}),
					backup_genotype(IdsNPIds,NPIds),
					terminate_phenotype(Cx_PId,SPIds,NPIds,APIds,ScapePIds),
					io:format("Agent:~p terminating. Genotype has been backed up.~n Fitness:~p~n TotEvaluations:~p~n TotCycles:~p~n TimeAcc:~p~n",
						[self(),U_HighestFitness,EvalAcc,U_CycleAcc,U_TimeAcc]),
					gen_server:cast(PM_PId,{self(),terminated,U_HighestFitness,EvalAcc+1,U_CycleAcc,U_TimeAcc});
				false -> %Continue training
					Tot_Neurons = length(NPIds),
					MP = 1/math:sqrt(Tot_Neurons),
					Perturb_NPIds=[NPId || NPId <- NPIds,random:uniform()<MP],
					%io:format("Perturb_NPIds:~p~n",[Perturb_NPIds]),
					put(perturbed,Perturb_NPIds),
					[NPId ! {self(),weight_perturb} || NPId <- Perturb_NPIds],
					Cx_PId ! {self(),reactivate},
					loop(Agent_Id,PM_PId,IdsNPIds,Cx_PId,SPIds,NPIds,APIds,ScapePIds,U_HighestFitness,EvalAcc+1,CycleAcc+Cycles,TimeAcc+Time,U_Attempt)
			end
	end.
%The exoself process' main loop awaits from its cortex proccess the evoluation_completed message. Once the message is received, based on the fitness achieved, exoself decides whether to continue tunning the weights or terminate the system. Exoself tries to improve the fitness by perturbing/tuning the weights of its neurons, after each tuning session, the Neural Network based system performs another evaluation by interacting with the scape until completion (the NN solves a problem, or dies within the scape or...). The order of events is important: When evaluation_completed message is received, the function first checks whether the newly achieved fitness is higher than the highest fitness achieved so far. If it is not, the function sends the neurons a message to restore their weights to previous state, during which it last acehived the highest fitness instead of their current state which yielded the current lower fitness score. If on the other hand the new fitness is higher than the previously highest achieved fitness, then the function tells the neurons to backup their current weights, as these weights represent the NN's best, most fit form yet. Exoself then tells all the neurons to prepare for a reset by sending each neuron the {self(),reset_prep} message. Since the NN can have recursive connections, and the manner in which initial recursive messages are sent, it is important for each neuron to flush their buffers to be reset into an initial fresh state, which is achieved after the neurons receive the reset_prep message. The function then sends the reset message to the neurons, which returns them into their main loop. Finally, the function checks whether exoself has already tried to improve the NN's fitness a maximum (?MAX_ATTEMPTS) number of times. If that is the case, the exoself process backs up the updated NN (the updated, tuned weights) to database using the backup_genotype/2 function, prints to screen that it is terminating, and sends to the population_monitor the acumulated statistics (highest fitness, evaluation count, cycle count...). On the other hand, if the exoself is not yet done tuning the neural weights, it has not yet reached its ending condition, it then randomly selects a set of neurons from its NPIds list, and request that they perturb their weights, and then reactivates the cortex, and drops back into the main loop. Each neuron in the NPId list has a probability 1/math(sqrt(Tot_Neurons) of being selected for weight perturbation, a number that is proportional to the total number of neurons in the NN, and grows with the NN size.

	spawn_CerebralUnits(IdsNPIds,CerebralUnitType,[Id|Ids])-> 
		PId = CerebralUnitType:gen(self(),node()),
		ets:insert(IdsNPIds,{Id,PId}), 
		ets:insert(IdsNPIds,{PId,Id}), 
		spawn_CerebralUnits(IdsNPIds,CerebralUnitType,Ids); 
	spawn_CerebralUnits(_IdsNPIds,_CerebralUnitType,[])-> 
		true.
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
		SPId ! {self(),{SId,Cx_PId,Scape,SName,S#sensor.vl,Fanout_PIds}},
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
		APId ! {self(),{AId,Cx_PId,Scape,AName,Fanin_PIds}},
		link_Actuators(Actuator_Ids,IdsNPIds);
	link_Actuators([],_IdsNPIds)->
		ok.
%The link_Actuators2 function sends to the already spawned and waiting actuators their states, composed of the PId lists and other information which are needed by the actuators to link up and interface with other elements in the distributed phenotype.

	link_Neurons([NId|Neuron_Ids],IdsNPIds) ->
		N=genotype:dirty_read({neuron,NId}),
		NPId = ets:lookup_element(IdsNPIds,NId,2),
		Cx_PId = ets:lookup_element(IdsNPIds,N#neuron.cx_id,2),
		AFName = N#neuron.af,
		Input_IdPs = N#neuron.input_idps,
		Output_Ids = N#neuron.output_ids,
		RO_Ids = N#neuron.ro_ids,
		Input_PIdPs = convert_IdPs2PIdPs(IdsNPIds,Input_IdPs,[]),
		Output_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Output_Ids],
		RO_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- RO_Ids],
		NPId ! {self(),{NId,Cx_PId,AFName,Input_PIdPs,Output_PIds,RO_PIds}},
		link_Neurons(Neuron_Ids,IdsNPIds);
	link_Neurons([],_IdsNPIds)->
		ok.
%The link_Neurons/2 function sends to the already spawned and waiting neurons their states, composed of the PId lists and other information needed by the neurons to link up and interface with other elements in the distributed phenotype.

		convert_IdPs2PIdPs(_IdsNPIds,[{bias,[Bias]}],Acc)->
			lists:reverse([Bias|Acc]);
		convert_IdPs2PIdPs(IdsNPIds,[{Id,Weights}|Fanin_IdPs],Acc)->
			convert_IdPs2PIdPs(IdsNPIds,Fanin_IdPs,[{ets:lookup_element(IdsNPIds,Id,2),Weights}|Acc]);
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
			{NPId,NId,WeightTuples}->
				get_backup(NPIds,[{NId,WeightTuples}|Acc])
		end;
	get_backup([],Acc)->
		Acc.
%The backup_genotype/2 uses get_backup/2 to contact all the neurons in its NN and request for the neuron's Ids and their Input_IdPs. Once the updated Input_IdPs from all the neurons have been accumulated, they are passed through the update_genotype/2 function to produce updated neurons, and write them to database.

	update_genotype(IdsNPIds,[{N_Id,PIdPs}|WeightPs])->
		N = genotype:dirty_read({neuron,N_Id}),
		Updated_InputIdPs = convert_PIdPs2IdPs(IdsNPIds,PIdPs,[]),
		U_N = N#neuron{input_idps = Updated_InputIdPs},
		genotype:write(U_N),
		%io:format("N:~p~n U_N:~p~n Genotype:~p~n U_Genotype:~p~n",[N,U_N,Genotype,U_Genotype]),
		update_genotype(IdsNPIds,WeightPs);
	update_genotype(_IdsNPIds,[])->
		ok.
%For every {N_Id,PIdPs} tuple the update_genotype/3 function extracts the neuron with the id: N_Id, updates the neuron's input_IdPs, and writes the updated neuron to database.

		convert_PIdPs2IdPs(IdsNPIds,[{PId,Weights}|Input_PIdPs],Acc)->
			convert_PIdPs2IdPs(IdsNPIds,Input_PIdPs,[{ets:lookup_element(IdsNPIds,PId,2),Weights}|Acc]);
		convert_PIdPs2IdPs(_IdsNPIds,[Bias],Acc)->
			lists:reverse([{bias,[Bias]}|Acc]);
		convert_PIdPs2IdPs(_IdsNPIds,[],Acc)->
			lists:reverse(Acc).
%The convert_PIdPs2IdPs/3 performs the conversion from PIds to Ids of every {PId,Weights} tuple in the Input_PIdPs list. The updated Input_IdPs are then returned to the caller.
	
terminate_phenotype(Cx_PId,SPIds,NPIds,APIds,ScapePIds)->
	io:format("Terminating the phenotype:~nCx_PId:~p~nSPIds:~p~nNPIds:~p~nAPIds:~p~nScapePids:~p~n",[Cx_PId,SPIds,NPIds,APIds,ScapePIds]),
	[PId ! {self(),terminate} || PId <- SPIds],
	[PId ! {self(),terminate} || PId <- APIds],
	[PId ! {self(),termiante} || PId <- NPIds],
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
