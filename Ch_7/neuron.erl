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

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,prep,[ExoSelf_PId]).

prep(ExoSelf_PId) ->
	{V1,V2,V3} = now(),
	random:seed(V1,V2,V3),
	receive 
		{ExoSelf_PId,{Id,Cx_PId,AF,Input_PIdPs,Output_PIds}} ->
			loop(Id,ExoSelf_PId,Cx_PId,AF,{Input_PIdPs,Input_PIdPs},Output_PIds,0)
	end.
%When gen/2 is executed it spawns the neuron element, which seeds the pseudo random number generator, and immediately begins to wait for its initial state message. It is essential that we seed the random number generator to make sure that every NN will have a different set of mutation probabilities and different combination of perturbation intensities. Once the initial state signal from the exoself is received, the neuron drops into its main loop.

loop(Id,ExoSelf_PId,Cx_PId,AF,{[{Input_PId,Weights}|Input_PIdPs],MInput_PIdPs},Output_PIds,Acc)->
	receive
		{Input_PId,forward,Input}->
			Result = dot(Input,Weights,0),
			loop(Id,ExoSelf_PId,Cx_PId,AF,{Input_PIdPs,MInput_PIdPs},Output_PIds,Result+Acc);
		{ExoSelf_PId,weight_backup}->
			put(weights,MInput_PIdPs),
			loop(Id,ExoSelf_PId,Cx_PId,AF,{[{Input_PId,Weights}|Input_PIdPs],MInput_PIdPs},Output_PIds,Acc);
		{ExoSelf_PId,weight_restore}->
			RInput_PIdPs = get(weights),
			loop(Id,ExoSelf_PId,Cx_PId,AF,{RInput_PIdPs,RInput_PIdPs},Output_PIds,Acc);
		{ExoSelf_PId,weight_perturb}->
			PInput_PIdPs=perturb_IPIdPs(MInput_PIdPs),
			loop(Id,ExoSelf_PId,Cx_PId,AF,{PInput_PIdPs,PInput_PIdPs},Output_PIds,Acc);
		{ExoSelf_PId,get_backup}->
			ExoSelf_PId ! {self(),Id,MInput_PIdPs},
			loop(Id,ExoSelf_PId,Cx_PId,AF,{[{Input_PId,Weights}|Input_PIdPs],MInput_PIdPs},Output_PIds,Acc);
		{ExoSelf_PId,terminate}->
			ok
	end;
loop(Id,ExoSelf_PId,Cx_PId,AF,{[Bias],MInput_PIdPs},Output_PIds,Acc)->
	Output = neuron:AF(Acc+Bias),
	[Output_PId ! {self(),forward,[Output]} || Output_PId <- Output_PIds],
	loop(Id,ExoSelf_PId,Cx_PId,AF,{MInput_PIdPs,MInput_PIdPs},Output_PIds,0);
loop(Id,ExoSelf_PId,Cx_PId,AF,{[],MInput_PIdPs},Output_PIds,Acc)->
	Output = neuron:AF(Acc),
	[Output_PId ! {self(),forward,[Output]} || Output_PId <- Output_PIds],
	loop(Id,ExoSelf_PId,Cx_PId,AF,{MInput_PIdPs,MInput_PIdPs},Output_PIds,0).
	
	dot([I|Input],[W|Weights],Acc) ->
		dot(Input,Weights,I*W+Acc);
	dot([],[],Acc)->
		Acc.
%The neuron process waits for vector signals from all the processes that it’s connected from. As the presynaptic signals fanin, the neuron takes the dot product of the input and their associated weight vectors, and then adds it to the accumulator. Once all the signals from Input_PIds are received, the accumulator contains the dot product to which the neuron then adds the bias (if it exists) and executes the activation function. After fanning out the output signal, the neuron again returns to waiting for incoming signals. When the neuron receives the {ExoSelf_PId, get_backup} message, it forwards to the exoself its full MInput_PIdPs list, and its Id. The MInput_PIdPs contains the current version of the neural weights. When the neuron receives the {ExoSelf_PId,weight_perturb} message, it executes the perturb_IPIdPs/1, after which the neuron drops back into the loop but with MInput_PIdPs replaced by the new PInput_PIdPs. It is important to note that the neuron expects to be synchronized, and expects that it has at this point not received any signals from the other elements it is connected from, because if it has and it then changes out the Input_PIdPs with PInput_PIdPs, it might start waiting for signals from the elements from which it has already received the signals. When the neuron receives the {ExoSelf_PId,weight_backup}, it stores its weights in its process dictionary. When the neuron receives the {ExoSelf,weight_restore}, it restores its weights to the state they were before being perturbed by restoring the saved synaptic weights from its process dictionary.

	tanh(Val)->
		math:tanh(Val).
%The activation function is a sigmoid function, tanh.

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
%perturb_IPIdPs/1 first calculates the probability that a weight will be perturbed, the probability being the inverse square root of the total number of weights in the neuron. The function then drops into perturb_IPIdPs/3, which executes perturb_weights/3 for every set of weights associated with a particular Input_PId in the Input_PIdPs list. If bias is present in the weights list, it is reached last and perturbed just as any other weight, based on the probability. Afterwards, the perturbed and inverted version of the Input_PIdPs is reversed back to the proper order and returned to the calling function.
	
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
		
		sat(Val,Min,Max)->
			if
				Val < Min -> Min;
				Val > Max -> Max;
				true -> Val
			end.
%perturb_weights/3 accepts a probability value, a list of weights, and an empty list to act as an accumulator. The function then goes through the weight list perturbing each weight with a probability of MP. The weights are constrained to be within the range of -?SAT_LIMIT and SAT_LIMIT through the use of the sat/3 function.
