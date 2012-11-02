%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(sensor).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,prep,[ExoSelf_PId]).

prep(ExoSelf_PId) ->
	receive 
		{ExoSelf_PId,{Id,Cx_PId,Scape,SensorName,VL,Parameters,Fanout_PIds,OpMode}} ->
			put(opmode,OpMode),
			loop(Id,ExoSelf_PId,Cx_PId,Scape,SensorName,VL,Parameters,Fanout_PIds)
	end.
%When gen/2 is executed it spawns the sensor element and immediately begins to wait for its initial state message.

loop(Id,ExoSelf_PId,Cx_PId,Scape,SensorName,VL,Parameters,Fanout_PIds)->
	receive
		{Cx_PId,sync}->
			SensoryVector = sensor:SensorName(ExoSelf_PId,VL,Parameters,Scape),
			[Pid ! {self(),forward,SensoryVector} || Pid <- Fanout_PIds],
			loop(Id,ExoSelf_PId,Cx_PId,Scape,SensorName,VL,Parameters,Fanout_PIds);
		{ExoSelf_PId,terminate} ->
			%io:format("Sensor:~p is terminating.~n",[Id]),
			ok
	end.
%The sensor process accepts only 2 types of messages, both from the cortex. The sensor can either be triggered to begin gathering sensory data based on its sensory role, or terminate if the cortex requests so.

rng(ExoSelf_PId,VL,_Scape)->
	rng1(VL,[]).
rng1(0,Acc)->
	Acc;
rng1(VL,Acc)-> 
	rng1(VL-1,[random:uniform()|Acc]).
%rng/2 is a simple random number generator that produces a vector of random values, each between 0 and 1. The length of the vector is defined by the VL, which itself is specified within the sensor record.

xor_GetInput(ExoSelf_PId,VL,_Parameters,Scape)->
	Scape ! {self(),sense},
	receive
		{Scape,percept,SensoryVector}->
			case length(SensoryVector)==VL of
				true ->
					SensoryVector;
				false ->
					io:format("Error in sensor:xor_sim/3, VL:~p SensoryVector:~p~n",[VL,SensoryVector]),
					lists:duplicate(VL,0)
			end
	end.
%xor_GetInput/2 contacts the XOR simulator and requests the sensory vector, which in this case should be a binary vector of length 2. The sensor checks that the incoming sensory signal, the percept, is indeed of length 2. If the vector length differs, then this is printed to the console and a dummy vector of appropriate length is constructed.

pb_GetInput(ExoSelf_PId,VL,Parameters,Scape)->
	Scape ! {self(),sense,Parameters},
	receive
		{Scape,percept,SensoryVector}->
			case length(SensoryVector)==VL of
				true ->
					SensoryVector;
				false ->
					io:format("Error in sensor:pb_GetInput/3, VL:~p SensoryVector:~p~n",[VL,SensoryVector]),
					lists:duplicate(VL,0)
			end
	end.
	
dtm_GetInput(ExoSelf_PId,VL,Parameters,Scape)->
	Scape ! {self(),sense,Parameters},
	receive
		{Scape,percept,SensoryVector}->
			%io:format("self():~p SensoryVector:~p~n",[self(),SensoryVector]),
			case length(SensoryVector)==VL of
				true ->
					SensoryVector;
				false ->
					io:format("Error in sensor:dtm_GetInput/3, VL:~p SensoryVector:~p~n",[VL,SensoryVector]),
					lists:duplicate(VL,0)
			end
	end.
	
fx_PCI(Exoself_Id,VL,Parameters,Scape)->
	[HRes,VRes] = Parameters,
	case get(opmode) of
		gt	->
			%Normal, assuming we have 10000 rows, we start from 1000 to 6000
			Scape ! {self(),sense,'EURUSD15',close,[HRes,VRes,graph_sensor],1000,200};
		benchmark ->
			Scape ! {self(),sense,'EURUSD15',close,[HRes,VRes,graph_sensor],200,last}
	end,
	receive 
		{_From,Result}->
			Result
	end.

fx_PLI(Exoself_Id,VL,Parameters,Scape)->
	[HRes,Type] = Parameters,%Type=open|close|high|low
	case get(opmode) of
		gt	->
			%Normal, assuming we have 10000 rows, we start from 1000 to 6000
			Scape ! {self(),sense,'EURUSD15',close,[HRes,list_sensor],1000,200};
		benchmark ->
			Scape ! {self(),sense,'EURUSD15',close,[HRes,list_sensor],200,last}
	end,
	receive 
		{_From,Result}->
			normalize(Result)
	end.
	
	normalize(Vector)->
		Normalizer=math:sqrt(lists:sum([Val*Val||Val<-Vector])),
		[Val/Normalizer || Val <- Vector].
	
fx_Internals(Exoself_Id,VL,Parameters,Scape)->
	Scape ! {self(),sense,internals,Parameters},
	receive
		{PId,Result}->
			Result
	end.
