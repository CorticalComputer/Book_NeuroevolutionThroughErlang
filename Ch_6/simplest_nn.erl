%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(simplest_nn).
-compile(export_all).

create() ->
	Weights = [random:uniform()-0.5,random:uniform()-0.5,random:uniform()-0.5],
	N_PId = spawn(?MODULE,neuron,[Weights,undefined,undefined]),
	S_PId = spawn(?MODULE,sensor,[N_PId]),
	A_PId = spawn(?MODULE,actuator,[N_PId]),
	N_PId ! {init,S_PId,A_PId},
	register(cortex,spawn(?MODULE,cortex,[S_PId,N_PId,A_PId])).
%The create function first generates 3 weights, with the 3rd weight being the Bias. The Neuron is spawned first, and is then sent the PIds of the Sensor and Actuator that it's connected with. Then the Cortex element is registered and provided with the PIds of all the elements in the NN system.

neuron(Weights,S_PId,A_PId) ->
	receive 
		{S_PId,forward, Input} ->
			io:format("****Thinking****~n Input:~p~n with Weights:~p~n",[Input,Weights]),
			Dot_Product = dot(Input,Weights,0),
			Output = [math:tanh(Dot_Product)],
			A_PId ! {self(),forward,Output},
			neuron(Weights,S_PId,A_PId);
		{init,New_SPId,New_APId} ->
			neuron(Weights,New_SPId,New_APId);
		terminate ->
			ok
	end.
%After the neuron finishes setting its SPId and APId to that of the Sensor and Actuator respectively, it starts waiting for the incoming signals. The neuron expects a vector of length 2 as input, and as soon as the input arrives, the neuron processes the signal and passes the output vector to the outgoing APId.

	dot([I|Input],[W|Weights],Acc) ->
		dot(Input,Weights,I*W+Acc);
	dot([],[],Acc)->
		Acc;
	dot([],[Bias],Acc)->
		Acc + Bias.
%The dot function takes a dot product of two vectors, it can operate on a weight vector with and without a bias. When there is no bias in the weight list, both the Input vector and the Weight vector are of the same length. When Bias is present, then when the Input list empties out, the Weights list still has 1 value remaining, its Bias.

sensor(N_PId) ->
	receive
		sync ->
			Sensory_Signal = [random:uniform(),random:uniform()],
			io:format("****Sensing****:~n Signal from the environment ~p~n",[Sensory_Signal]),
			N_PId ! {self(),forward,Sensory_Signal},
			sensor(N_PId);
		terminate ->
			ok
	end.
%The Sensor function waits to be triggered by the Cortex element, and then produces a random vector of length 2, which it passes to the connected neuron. In a proper system the sensory signal would not be a random vector but instead would be produced by a function associated with the sensor, a function that for example reads and vector-encodes a signal coming from a GPS attached to a robot.

actuator(N_PId) ->
	receive
		{N_PId,forward,Control_Signal}->
			pts(Control_Signal),
			actuator(N_PId);
		terminate ->
			ok
	end.

	pts(Control_Signal)->
		io:format("****Acting****:~n Using:~p to act on environment.~n",[Control_Signal]).
%The Actuator function waits for a control signal coming from a Neuron. As soon as the signal arrives, the actuator executes its function, pts/1, which prints the value to the screen. 

cortex(Sensor_PId,Neuron_PId,Actuator_PId)->
	receive
		sense_think_act ->
			Sensor_PId ! sync,
			cortex(Sensor_PId,Neuron_PId,Actuator_PId);
		terminate ->
			Sensor_PId ! terminate,
			Neuron_PId ! terminate,
			Actuator_PId ! terminate,
			ok
	end.
%The Cortex function triggers the sensor to action when commanded by the user. This process also has all the PIds of the elements in the NN system, so that it can terminate the whole system when requested.
