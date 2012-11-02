%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(neuron).
-compile(export_all).
-include("records.hrl").
-define(DELTA_MULTIPLIER,math:pi()*2).
-define(SAT_LIMIT,math:pi()*2).
-define(RO_SIGNAL,0).

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,prep,[ExoSelf_PId]).

prep(ExoSelf_PId) ->
	random:seed(now()),
	receive 
		{ExoSelf_PId,{Id,Cx_PId,AF,PF,AggrF,Input_PIdPs,Output_PIds,RO_PIds}} ->
			fanout(RO_PIds,{self(),forward,[?RO_SIGNAL]}),
			IPIds = [IPId || {IPId,_W} <- Input_PIdPs, IPId =/= bias],
			%io:format("IPIdPs:~p~n",[Input_PIdPs]),
			loop(Id,ExoSelf_PId,Cx_PId,AF,PF,AggrF,{IPIds,IPIds},[],{Input_PIdPs,Input_PIdPs},Output_PIds,RO_PIds)
	end.
%When gen/2 is executed, it spawns the neuron element and immediately begins to wait for its initial state message from the exoself. Once the state message arrives, the neuron sends out the default forward signals to any elements in its ro_ids list, if any. Afterwards, prep drops into the neuron's main loop.

loop(Id,ExoSelf_PId,Cx_PId,AF,PF,AggrF,{[Input_PId|IPIds],MIPIds},IAcc,{Input_PIdPs,MInput_PIdPs},Output_PIds,RO_PIds)->
	receive
		{Input_PId,forward,Input}->
			loop(Id,ExoSelf_PId,Cx_PId,AF,PF,AggrF,{IPIds,MIPIds},[{Input_PId,Input}|IAcc],{Input_PIdPs,MInput_PIdPs},Output_PIds,RO_PIds);
		{ExoSelf_PId,weight_backup}->
			loop(Id,ExoSelf_PId,Cx_PId,AF,PF,AggrF,{[Input_PId|IPIds],MIPIds},IAcc,{Input_PIdPs,Input_PIdPs},Output_PIds,RO_PIds);
		{ExoSelf_PId,weight_restore}->
			loop(Id,ExoSelf_PId,Cx_PId,AF,PF,AggrF,{[Input_PId|IPIds],MIPIds},IAcc,{MInput_PIdPs,MInput_PIdPs},Output_PIds,RO_PIds);
		{ExoSelf_PId,weight_perturb,Spread}->
			Perturbed_IPIdPs=perturb_IPIdPs(Spread,MInput_PIdPs),
			loop(Id,ExoSelf_PId,Cx_PId,AF,PF,AggrF,{[Input_PId|IPIds],MIPIds},IAcc,{Perturbed_IPIdPs,MInput_PIdPs},Output_PIds,RO_PIds);
		{ExoSelf,reset_prep}->
			neuron:flush_buffer(),
			ExoSelf ! {self(),ready},
			receive 
				{ExoSelf, reset}->
					fanout(RO_PIds,{self(),forward,[?RO_SIGNAL]})
			end,
			loop(Id,ExoSelf_PId,Cx_PId,AF,PF,AggrF,{MIPIds,MIPIds},[],{Input_PIdPs,MInput_PIdPs},Output_PIds,RO_PIds);
		{ExoSelf_PId,get_backup}->
			ExoSelf_PId ! {self(),Id,MInput_PIdPs},
			loop(Id,ExoSelf_PId,Cx_PId,AF,PF,AggrF,{[Input_PId|IPIds],MIPIds},IAcc,{Input_PIdPs,MInput_PIdPs},Output_PIds,RO_PIds);
		{ExoSelf_PId,terminate}->
			%io:format("Neuron:~p is terminating.~n",[self()]),
			ok
		after 10000 ->
			io:format("neuron:~p stuck.~n",[Id])
	end;
loop(Id,ExoSelf_PId,Cx_PId,AF,PF,AggrF,{[],MIPIds},IAcc,{Input_PIdPs,MInput_PIdPs},Output_PIds,RO_PIds)->
	Ordered_IAcc = lists:reverse(IAcc),
	Aggregation_Product = signal_aggregator:AggrF(Ordered_IAcc,Input_PIdPs),
	Output = functions:AF(Aggregation_Product),
	U_IPIdPs = plasticity:PF(Ordered_IAcc,Input_PIdPs,Output),
	[Output_PId ! {self(),forward,[Output]} || Output_PId <- Output_PIds],
	loop(Id,ExoSelf_PId,Cx_PId,AF,PF,AggrF,{MIPIds,MIPIds},[],{U_IPIdPs,MInput_PIdPs},Output_PIds,RO_PIds).
%The neuron process waits for vector signals from all the processes that it's connected from, taking the dot product of the input and weight vectors, and then adding it to the accumulator. Once all the signals from Input_PIds are received, the accumulator contains the dot product to which the neuron then adds the bias and executes the activation function. After fanning out the output signal, the neuron again returns to waiting for incoming signals. When the neuron receives the {ExoSelf_PId,get_backup} message, it forwards to the exoself its full MInput_PIdPs list, and its Id. The MInput_PIdPs contains the modified, tuned and most effective version of the input_idps. The neuron process is also accepts weight_backup signal, when receiving it the neuon saves to process dictionary the current MInput_PIdPs. When the neuron receives the weight_restore signal, it reads back from the process dictionary the stored INput_PIdPs, and switches over to using it as its active Input_PIdPs list. When the neuron receives the weight_perturb signal from the exoself, it perturbs the weights by executing the peturb_IPIdPs/1 function, which returns the updated list. Finally, the neuron can also accept a reset_prep signal, which makes the neuron flush its buffer in the off chance that it has a recursivly sent signal in its inbox. After flushing its buffer, the neuron waits for the exoself to send it the reset signal, at which point the neuron, now fully refreshed after the flush_buffer/0, outputs a default forward signal to its recursivly connected elements, if any, and then drops back into the main loop.

	fanout([Pid|Pids],Msg)->
		Pid ! Msg,
		fanout(Pids,Msg);
	fanout([],_Msg)->
		true.
%The fanout/2 function fans out th Msg to all the PIds in its list.

	flush_buffer()->
		receive 
			_ ->
				flush_buffer()
		after 0 ->
			done
	end.
%The flush_buffer/0 cleans out the element's inbox.

perturb_IPIdPs(Spread,Input_PIdPs)->
	Tot_Weights=lists:sum([length(Weights) || {_Input_PId,Weights}<-Input_PIdPs]),
	MP = 1/math:sqrt(Tot_Weights),
	perturb_IPIdPs(Spread,MP,Input_PIdPs,[]).
perturb_IPIdPs(Spread,MP,[{Input_PId,Weights}|Input_PIdPs],Acc)->
	U_Weights = perturb_weights(Spread,MP,Weights,[]),
	perturb_IPIdPs(Spread,MP,Input_PIdPs,[{Input_PId,U_Weights}|Acc]);
perturb_IPIdPs(_Spread,_MP,[],Acc)->
	lists:reverse(Acc).
%The perturb_IPIdPs/1 function calculates the probablity with which each neuron in the Input_PIdPs is chosen to be perturbed. The probablity is based on the total number of weights in the Input_PIdPs list, with the actual mutation probablity equatling to the inverse of square root of total number of weights. The perturb_IPIdPs/3 function goes through each weights block and calls the perturb_weights/3 to perturb the weights.

	perturb_weights(Spread,MP,[W|Weights],Acc)->
		U_W = case random:uniform() < MP of
			true->
				sat((random:uniform()-0.5)*2*Spread+W,-?SAT_LIMIT,?SAT_LIMIT);
			false ->
				W
		end,
		perturb_weights(Spread,MP,Weights,[U_W|Acc]);
	perturb_weights(_Spread,_MP,[],Acc)->
		lists:reverse(Acc).
%The perturb_weights/3 function is the function that actually goes through each weight block, and perturbs each weight with a probablity of MP. If the weight is chosen to be perturbed, the perturbation intensity is chosen uniformly between -Spread and Spread.

		sat(Val,Min,Max)->
			if
				Val < Min -> Min;
				Val > Max -> Max;
				true -> Val
			end.
%sat/3 function simply ensures that the Val is neither less than min or greater than max.
