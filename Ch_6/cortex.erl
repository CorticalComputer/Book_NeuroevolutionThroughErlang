%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(cortex).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,loop,[ExoSelf_PId]).

loop(ExoSelf_PId) ->
	receive 
		{ExoSelf_PId,{Id,SPIds,APIds,NPIds},TotSteps} ->
			put(start_time,now()),
			[SPId ! {self(),sync} || SPId <- SPIds],
			loop(Id,ExoSelf_PId,SPIds,{APIds,APIds},NPIds,TotSteps)
	end.
%The gen/2 function spawns the cortex element, which immediately starts to wait for a the state message from the same process that spawned it, exoself. The initial state message contains the sensor, actuator, and neuron PId lists. The message also specifies how many total Sense-Think-Act cycles the Cortex should execute before terminating the NN system. Once we implement the learning algorithm, the termination criteria will depend on the fitness of the NN, or some other useful property

loop(Id,ExoSelf_PId,SPIds,{_APIds,MAPIds},NPIds,0) ->
	TimeDif = timer:now_diff(now(),get(start_time)),
	io:format("Cortex:~p is backing up and terminating.~n",[Id]),
	io:format("Operational time:~p~n",[TimeDif]),
	Neuron_IdsNWeights = get_backup(NPIds,[]),
	ExoSelf_PId ! {self(),backup,Neuron_IdsNWeights},
	[PId ! {self(),terminate} || PId <- SPIds],
	[PId ! {self(),terminate} || PId <- MAPIds],
	[PId ! {self(),termiante} || PId <- NPIds];
loop(Id,ExoSelf_PId,SPIds,{[APId|APIds],MAPIds},NPIds,Step) ->
	receive 
		{APId,sync} ->
			loop(Id,ExoSelf_PId,SPIds,{APIds,MAPIds},NPIds,Step);
		terminate ->
			io:format("Cortex:~p is terminating.~n",[Id]),
			[PId ! {self(),terminate} || PId <- SPIds],
			[PId ! {self(),terminate} || PId <- MAPIds],
			[PId ! {self(),termiante} || PId <- NPIds]
	end;
loop(Id,ExoSelf_PId,SPIds,{[],MAPIds},NPIds,Step)->
	[PId ! {self(),sync} || PId <- SPIds],
	loop(Id,ExoSelf_PId,SPIds,{MAPIds,MAPIds},NPIds,Step-1).
%The cortex's goal is to synchronize the the NN system such that when the actuators have received all their control signals, the sensors are once again triggered to gather new sensory information. Thus the cortex waits for the sync messages from the actuator PIds in its system, and once it has received all the sync messages, it triggers the sensors and then drops back to waiting for a new set of sync messages. The cortex stores 2 copies of the actuator PIds: the APIds, and the MemoryAPIds (MAPIds). Once all the actuators have sent it the sync messages, it can restore the APIds list from the MAPIds. Finally, there is also the Step variable which decrements every time a full cycle of Sense-Think-Act completes, once this reaches 0, the NN system begins its termination and backup process.

	get_backup([NPId|NPIds],Acc)->
		NPId ! {self(),get_backup},
		receive
			{NPId,NId,WeightTuples}->
				get_backup(NPIds,[{NId,WeightTuples}|Acc])
		end;
	get_backup([],Acc)->
		Acc.
%During backup, cortex contacts all the neurons in its NN and requests for the neuron's Ids and their Input_IdPs. Once the updated Input_IdPs from all the neurons have been accumulated, the list is sent to exoself for the actual backup and storage.
