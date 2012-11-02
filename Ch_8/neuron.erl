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
	{V1,V2,V3} = now(),
	random:seed(V1,V2,V3),
	receive 
		{ExoSelf_PId,{Id,Cx_PId,AF,Input_PIdPs,Output_PIds,RO_PIds}} ->
			fanout(RO_PIds,{self(),forward,[?RO_SIGNAL]}),
			loop(Id,ExoSelf_PId,Cx_PId,AF,{Input_PIdPs,Input_PIdPs},Output_PIds,RO_PIds,0)
	end.
%When gen/2 is executed, it spawns the neuron element and immediately begins to wait for its initial state message from the exoself. Once the state message arrives, the neuron sends out the default forward signals to any elements in its ro_ids list, if any. Afterwards, prep drops into the neuron's main loop.

loop(Id,ExoSelf_PId,Cx_PId,AF,{[{Input_PId,Weights}|Input_PIdPs],MInput_PIdPs},Output_PIds,RO_PIds,Acc)->
	receive
		{Input_PId,forward,Input}->
			Result = dot(Input,Weights,0),
			loop(Id,ExoSelf_PId,Cx_PId,AF,{Input_PIdPs,MInput_PIdPs},Output_PIds,RO_PIds,Result+Acc);
		{ExoSelf_PId,weight_backup}->
			put(weights,MInput_PIdPs),
			loop(Id,ExoSelf_PId,Cx_PId,AF,{[{Input_PId,Weights}|Input_PIdPs],MInput_PIdPs},Output_PIds,RO_PIds,Acc);
		{ExoSelf_PId,weight_restore}->
			RInput_PIdPs = get(weights),
			loop(Id,ExoSelf_PId,Cx_PId,AF,{RInput_PIdPs,RInput_PIdPs},Output_PIds,RO_PIds,Acc);
		{ExoSelf_PId,weight_perturb}->
			PInput_PIdPs=perturb_IPIdPs(MInput_PIdPs),
			loop(Id,ExoSelf_PId,Cx_PId,AF,{[{Input_PId,Weights}|Input_PIdPs],PInput_PIdPs},Output_PIds,RO_PIds,Acc);
		{ExoSelf,reset_prep}->
			neuron:flush_buffer(),
			ExoSelf ! {self(),ready},
			receive 
				{ExoSelf, reset}->
					fanout(RO_PIds,{self(),forward,[?RO_SIGNAL]})
			end,
			loop(Id,ExoSelf_PId,Cx_PId,AF,{MInput_PIdPs,MInput_PIdPs},Output_PIds,RO_PIds,0);
		{ExoSelf_PId,get_backup}->
			ExoSelf_PId ! {self(),Id,MInput_PIdPs},
			loop(Id,ExoSelf_PId,Cx_PId,AF,{[{Input_PId,Weights}|Input_PIdPs],MInput_PIdPs},Output_PIds,RO_PIds,Acc);
		{ExoSelf_PId,terminate}->
			io:format("Neuron:~p has termianted.~n",[self()]),
			ok
	end;
loop(Id,ExoSelf_PId,Cx_PId,AF,{[Bias],MInput_PIdPs},Output_PIds,RO_PIds,Acc)->
	Output = functions:AF(Acc+Bias),
	[Output_PId ! {self(),forward,[Output]} || Output_PId <- Output_PIds],
	loop(Id,ExoSelf_PId,Cx_PId,AF,{MInput_PIdPs,MInput_PIdPs},Output_PIds,RO_PIds,0);
loop(Id,ExoSelf_PId,Cx_PId,AF,{[],MInput_PIdPs},Output_PIds,RO_PIds,Acc)->
	Output = functions:AF(Acc),
	[Output_PId ! {self(),forward,[Output]} || Output_PId <- Output_PIds],
	loop(Id,ExoSelf_PId,Cx_PId,AF,{MInput_PIdPs,MInput_PIdPs},Output_PIds,RO_PIds,0).
%The neuron process waits for vector signals from all the processes that it’s connected from, taking the dot product of the input and weight vectors, and then adding it to the accumulator. Once all the signals from Input_PIds are received, the accumulator contains the dot product to which the neuron then adds the bias and executes the activation function. After fanning out the output signal, the neuron again returns to waiting for incoming signals. When the neuron receives the {ExoSelf_PId,get_backup} message, it forwards to the exoself its full MInput_PIdPs list, and its Id. The MInput_PIdPs contains the modified, tuned and most effective version of the input_idps. The neuron process also accepts the weight_backup signal, when receiving it, the neuron saves to process dictionary the current MInput_PIdPs. When the neuron receives the weight_restore signal, it reads back from the process dictionary the stored Input_PIdPs, and switches over to using it as its active Input_PIdPs list. When the neuron receives the weight_perturb signal from the exoself, it perturbs the weights by executing the perturb_IPIdPs/1 function, which returns the updated/perturbed weight list. Finally, the neuron can also accept a reset_prep signal, which makes the neuron flush its buffer in the off chance that it has a recursively sent to it signal in its inbox. After flushing its buffer, the neuron waits for the exoself to send it the reset signal, at which point the neuron, now fully refreshed after the flush_buffer/0, outputs a default forward signal to its recursively connected elements (ro_ids), if any, and then drops back into its main receive loop.


	dot([I|Input],[W|Weights],Acc) ->
		dot(Input,Weights,I*W+Acc);
	dot([],[],Acc)->
		Acc;
	dot([],[Bias],Acc)->
		Acc+Bias.
%The dot/3 function accepts an input vector and a weight list, and computes the dot product of the two vectors.

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

perturb_IPIdPs(Input_PIdPs)->
	Tot_Weights=lists:sum([length(Weights) || {_Input_PId,Weights}<-Input_PIdPs]),
	MP = 1/math:sqrt(Tot_Weights),
	perturb_IPIdPs(MP,Input_PIdPs,[]).
perturb_IPIdPs(MP,[{Input_PId,Weights}|Input_PIdPs],Acc)->
	U_Weights = perturb_weights(MP,Weights,[]),
	perturb_IPIdPs(MP,Input_PIdPs,[{Input_PId,U_Weights}|Acc]);
perturb_IPIdPs(MP,[Bias],Acc)->
	U_Bias = case random:uniform() < MP of
		true-> sat((random:uniform()-0.5)*?DELTA_MULTIPLIER+Bias,-?SAT_LIMIT,?SAT_LIMIT);
		false -> Bias
	end,
	lists:reverse([U_Bias|Acc]);
perturb_IPIdPs(_MP,[],Acc)->
	lists:reverse(Acc).
%The perturb_IPIdPs/1 function calculates the probablity with which each neuron in the Input_PIdPs is chosen to be perturbed. The probablity is based on the total number of weights in the Input_PIdPs list, with the actual mutation probablity equatling to the inverse of square root of total number of weights. The perturb_IPIdPs/3 function goes through each weights block and calls the perturb_weights/3 to perturb the weights.

	perturb_weights(MP,[W|Weights],Acc)->
		U_W = case random:uniform() < MP of
			true->
				sat((random:uniform()-0.5)*?DELTA_MULTIPLIER+W,-?SAT_LIMIT,?SAT_LIMIT);
			false ->
				W
		end,
		perturb_weights(MP,Weights,[U_W|Acc]);
	perturb_weights(_MP,[],Acc)->
		lists:reverse(Acc).
%The perturb_weights/3 function is the function that actually goes through each weight block, and perturbs each weight with a probablity of MP. If the weight is chosen to be perturbed, the perturbation intensity is chosen uniformly between -Pi and Pi.

		sat(Val,Min,Max)->
			if
				Val < Min -> Min;
				Val > Max -> Max;
				true -> Val
			end.
%The sat/3 function simply ensures that the Val is neither less than min or greater than max. When used with synaptic weights (or other parameters), this function makes sure that the synaptic weights get saturated at the Min and Max values, rather than growing in magnitude without bound.
