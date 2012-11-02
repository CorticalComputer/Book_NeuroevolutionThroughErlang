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
-define(MAX_ATTEMPTS,50).

map()-> map(ffnn).
map(FileName)->
	Genotype=genotype:load_from_file(FileName),
	spawn(exoself,prep,[FileName,Genotype]).

prep(FileName,Genotype)->
	{V1,V2,V3} = now(),
	random:seed(V1,V2,V3),
	IdsNPIds = ets:new(idsNpids,[set,private]), 
	Cx = genotype:read(Genotype,cortex),
	Sensor_Ids = Cx#cortex.sensor_ids,
	Actuator_Ids = Cx#cortex.actuator_ids,
	NIds = Cx#cortex.nids,
	ScapePIds = spawn_Scapes(IdsNPIds,Genotype,Sensor_Ids,Actuator_Ids),
	spawn_CerebralUnits(IdsNPIds,cortex,[Cx#cortex.id]),
	spawn_CerebralUnits(IdsNPIds,sensor,Sensor_Ids),
	spawn_CerebralUnits(IdsNPIds,actuator,Actuator_Ids),
	spawn_CerebralUnits(IdsNPIds,neuron,NIds),
	link_Sensors(Genotype,Sensor_Ids,IdsNPIds),
	link_Actuators(Genotype,Actuator_Ids,IdsNPIds),
	link_Neurons(Genotype,NIds,IdsNPIds),
	{SPIds,NPIds,APIds}=link_Cortex(Cx,IdsNPIds),
	Cx_PId = ets:lookup_element(IdsNPIds,Cx#cortex.id,2),
	loop(FileName,Genotype,IdsNPIds,Cx_PId,SPIds,NPIds,APIds,ScapePIds,0,0,0,0,1).
%Once the FileName and the Genotype are dropped into the prep/2 function, the function uses the current time to create a new random seed. Then the cortex is extracted from the genotype and the Sensor, Actuator, and Neural Ids are extracted from it. The sensors and actuators are dropped into the spawn_Scapes/4, which extracts the scapes that need to be spawned, and then spawns them. Afterwards, the sensor, actuator, neuron, and the cortex elements are spawned. Then the exoself process sends these spawned elements the PIds of the elements they are connected to, thus linking all the elements together into a proper interconnected structure. The cortex element is the last one to be linked, because once it receives the message from the exoself with all the data, it immediately starts synchronizing the NN by prompting the sensors to action. Afterwards, prep/2 drops into the exoself’s main process loop.


loop(FileName,Genotype,IdsNPIds,Cx_PId,SPIds,NPIds,APIds,ScapePIds,HighestFitness,EvalAcc,CycleAcc,TimeAcc,Attempt)->
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
			%io:format("HighestFitness:~p U_Attempt:~p~n",[HighestFitness,U_Attempt]),
			case U_Attempt >= ?MAX_ATTEMPTS of
				true ->	%End training
					U_CycleAcc = CycleAcc+Cycles,
					U_TimeAcc = TimeAcc+Time,
					backup_genotype(FileName,IdsNPIds,Genotype,NPIds),
					terminate_phenotype(Cx_PId,SPIds,NPIds,APIds,ScapePIds),
					io:format("Cortex:~p finished training. Genotype has been backed up.~n Fitness:~p~n TotEvaluations:~p~n TotCycles:~p~n TimeAcc:~p~n",
						[Cx_PId,U_HighestFitness,EvalAcc,U_CycleAcc,U_TimeAcc]),
					
					case whereis(trainer) of
						undefined ->
							ok;
						PId -> 
							PId ! {self(),U_HighestFitness,EvalAcc,U_CycleAcc,U_TimeAcc}
					end;
				false -> %Continue training
					Tot_Neurons = length(NPIds),
					MP = 1/math:sqrt(Tot_Neurons),
					Perturb_NPIds=[NPId || NPId <- NPIds,random:uniform()<MP],
					%io:format("Perturb_NPIds:~p~n",[Perturb_NPIds]),
					put(perturbed,Perturb_NPIds),
					[NPId ! {self(),weight_perturb} || NPId <- Perturb_NPIds],
					Cx_PId ! {self(),reactivate},
					loop(FileName,Genotype,IdsNPIds,Cx_PId,SPIds,NPIds,APIds,ScapePIds,U_HighestFitness,EvalAcc+1,CycleAcc+Cycles,TimeAcc+Time,U_Attempt)
			end
	end.
%The map/1 function maps the tuple encoded genotype into a process based phenotype. The map function expects for the Cx record to be the leading tuple in the tuple list it reads from the FileName. We create an ets table to map Ids to PIds and back again. Since the Cortex element contains all the Sensor, Actuator, and Neuron Ids, we are able to spawn each neuron using its own gen function, and in the process construct a map from Ids to PIds. We then use link_CerebralUnits to link all non Cortex elements to each other by sending each spawned process the information contained in its record, but with Ids converted to Pids where appropriate. Finally, we provide the Cortex process with all the PIds in the NN system by executing the link_Cortex/2 function. Once the NN is up and running, exoself starts its wait until the NN has finished its job and is ready to backup. When the cortex initiates the backup process it sends exoself the updated Input_PIdPs from its neurons. Exoself uses the update_genotype/3 function to update the old genotype with new weights, and then stores the updated version back to its file.

	spawn_CerebralUnits(IdsNPIds,CerebralUnitType,[Id|Ids])-> 
		PId = CerebralUnitType:gen(self(),node()),
		ets:insert(IdsNPIds,{Id,PId}), 
		ets:insert(IdsNPIds,{PId,Id}), 
		spawn_CerebralUnits(IdsNPIds,CerebralUnitType,Ids); 
	spawn_CerebralUnits(_IdsNPIds,_CerebralUnitType,[])-> 
		true.
%We spawn the process for each element based on its type: CerebralUnitType, and the gen function that belongs to the CerebralUnitType module. We then enter the {Id,PId} tuple into our ETS table for later use.
%-record(sensor,{id,name,cx_id,format,scape,vl,fanout_ids,parameters,objects=[],vis=[]}).
%-record(actuator,{id,name,cx_id,format,scape,vl,fanin_ids,parameters,objects=[],vis=[]}).

	spawn_Scapes(IdsNPIds,Genotype,Sensor_Ids,Actuator_Ids)-> 
		Sensor_Scapes = [(genotype:read(Genotype,Id))#sensor.scape || Id<-Sensor_Ids], 
		Actuator_Scapes = [(genotype:read(Genotype,Id))#actuator.scape || Id<-Actuator_Ids], 
		Unique_Scapes = Sensor_Scapes++(Actuator_Scapes--Sensor_Scapes), 
		SN_Tuples=[{scape:gen(self(),node()),ScapeName} || {private,ScapeName}<-Unique_Scapes], 
		[ets:insert(IdsNPIds,{ScapeName,PId}) || {PId,ScapeName} <- SN_Tuples], 
		[ets:insert(IdsNPIds,{PId,ScapeName}) || {PId,ScapeName} <-SN_Tuples], 
		[PId ! {self(),ScapeName} || {PId,ScapeName} <- SN_Tuples],
		[PId || {PId,_ScapeName} <-SN_Tuples].

	link_Sensors(Genotype,[SId|Sensor_Ids],IdsNPIds) ->
		R=genotype:read(Genotype,SId),
		SPId = ets:lookup_element(IdsNPIds,SId,2),
		Cx_PId = ets:lookup_element(IdsNPIds,R#sensor.cx_id,2),
		SName = R#sensor.name,
		Fanout_Ids = R#sensor.fanout_ids,
		Fanout_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Fanout_Ids],
		Scape=case R#sensor.scape of
			{private,ScapeName}->
				ets:lookup_element(IdsNPIds,ScapeName,2)
		end,
		SPId ! {self(),{SId,Cx_PId,Scape,SName,R#sensor.vl,Fanout_PIds}},
		link_Sensors(Genotype,Sensor_Ids,IdsNPIds);
	link_Sensors(_Genotype,[],_IdsNPIds)->
		ok.
	link_Actuators(Genotype,[AId|Actuator_Ids],IdsNPIds) ->
		R=genotype:read(Genotype,AId),
		APId = ets:lookup_element(IdsNPIds,AId,2),
		Cx_PId = ets:lookup_element(IdsNPIds,R#actuator.cx_id,2),
		AName = R#actuator.name,
		Fanin_Ids = R#actuator.fanin_ids,
		Fanin_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Fanin_Ids],
		Scape=case R#actuator.scape of
			{private,ScapeName}->
				ets:lookup_element(IdsNPIds,ScapeName,2)
		end,
		APId ! {self(),{AId,Cx_PId,Scape,AName,Fanin_PIds}},
		link_Actuators(Genotype,Actuator_Ids,IdsNPIds);
	link_Actuators(_Genotype,[],_IdsNPIds)->
		ok.
	link_Neurons(Genotype,[NId|Neuron_Ids],IdsNPIds) ->
		R=genotype:read(Genotype,NId),
		NPId = ets:lookup_element(IdsNPIds,NId,2),
		Cx_PId = ets:lookup_element(IdsNPIds,R#neuron.cx_id,2),
		AFName = R#neuron.af,
		Input_IdPs = R#neuron.input_idps,
		Output_Ids = R#neuron.output_ids,
		Input_PIdPs = convert_IdPs2PIdPs(IdsNPIds,Input_IdPs,[]),
		Output_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Output_Ids],
		NPId ! {self(),{NId,Cx_PId,AFName,Input_PIdPs,Output_PIds}},
		link_Neurons(Genotype,Neuron_Ids,IdsNPIds);
	link_Neurons(_Genotype,[],_IdsNPIds)->
		ok.

		convert_IdPs2PIdPs(_IdsNPIds,[{bias,Bias}],Acc)->
			lists:reverse([Bias|Acc]);
		convert_IdPs2PIdPs(IdsNPIds,[{Id,Weights}|Fanin_IdPs],Acc)->
			convert_IdPs2PIdPs(IdsNPIds,Fanin_IdPs,[{ets:lookup_element(IdsNPIds,Id,2),Weights}|Acc]).
%The link_CerebralUnits/2 converts the Ids to PIds using the created IdsNPids ETS table. At this point all the elements are spawned, and the processes are waiting for their initial states. 'convert_IdPs2PIdPs' converts the IdPs tuples into tuples that use PIds instead of Ids, such that the Neuron will know which weights are to be associated with which incoming vector signals. The last element is the bias, which is added to the list in a non tuple form. Afterwards, the list is reversed to take its proper order.

	link_Cortex(Cx,IdsNPIds) ->
		Cx_Id = Cx#cortex.id,
		Cx_PId = ets:lookup_element(IdsNPIds,Cx_Id,2),
		SIds = Cx#cortex.sensor_ids,
		AIds = Cx#cortex.actuator_ids,
		NIds = Cx#cortex.nids,
		SPIds = [ets:lookup_element(IdsNPIds,SId,2) || SId <- SIds],
		NPIds = [ets:lookup_element(IdsNPIds,NId,2) || NId <- NIds],
		APIds = [ets:lookup_element(IdsNPIds,AId,2) || AId <- AIds],
		Cx_PId ! {self(),Cx_Id,SPIds,NPIds,APIds},
		{SPIds,NPIds,APIds}.
%The cortex is initialized to its proper state just as other elements. Because we have not yet implemented a learning algorithm for our NN system, we need to specify when the NN should shutdown. We do this by specifying the total number of cycles the NN should execute before terminating, which is 1000 in this case.

backup_genotype(FileName,IdsNPIds,Genotype,NPIds)->
	Neuron_IdsNWeights = get_backup(NPIds,[]),
	update_genotype(IdsNPIds,Genotype,Neuron_IdsNWeights),
	genotype:save_to_file(Genotype,FileName),
	io:format("Finished updating genotype to file:~p~n",[FileName]).

	get_backup([NPId|NPIds],Acc)->
		NPId ! {self(),get_backup},
		receive
			{NPId,NId,WeightTuples}->
				get_backup(NPIds,[{NId,WeightTuples}|Acc])
		end;
	get_backup([],Acc)->
		Acc.
%The backup_genotype/4 uses get_backup/2 to contact all the neurons in its NN and request for the neuron's Ids and their Input_IdPs. Once the updated Input_IdPs from all the neurons have been accumulated, they are passed through the update_genotype/3 function to produce the genotype with updated weights, which is then saved to file.

	update_genotype(IdsNPIds,Genotype,[{N_Id,PIdPs}|WeightPs])->
		N = genotype:read(Genotype,N_Id),
		Updated_InputIdPs = convert_PIdPs2IdPs(IdsNPIds,PIdPs,[]),
		U_N = N#neuron{input_idps = Updated_InputIdPs},
		genotype:write(Genotype,U_N),
		update_genotype(IdsNPIds,Genotype,WeightPs);
	update_genotype(_IdsNPIds,_Genotype,[])->
		ok.
	
		convert_PIdPs2IdPs(IdsNPIds,[{PId,Weights}|Input_PIdPs],Acc)->
			convert_PIdPs2IdPs(IdsNPIds,Input_PIdPs,[{ets:lookup_element(IdsNPIds,PId,2),Weights}|Acc]);
		convert_PIdPs2IdPs(_IdsNPIds,[Bias],Acc)->
			lists:reverse([{bias,Bias}|Acc]).
%For every {N_Id,PIdPs} tuple the update_genotype/3 function extracts the neuron with the id: N_Id, and updates its weights. The convert_PIdPs2IdPs/3 performs the conversion from PIds to Ids of every {PId,Weights} tuple in the Input_PIdPs list. The updated Genotype is then returned back to the caller.
	
terminate_phenotype(Cx_PId,SPIds,NPIds,APIds,ScapePIds)->
	[PId ! {self(),terminate} || PId <- SPIds],
	[PId ! {self(),terminate} || PId <- APIds],
	[PId ! {self(),termiante} || PId <- NPIds],
	[PId ! {self(),terminate} || PId <- ScapePIds],
	Cx_PId ! {self(),terminate}.
