%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(constructor).
-compile(export_all).
-include("records.hrl").

construct_Genotype(SensorName,ActuatorName,HiddenLayerDensities)->
	construct_Genotype(ffnn,SensorName,ActuatorName,HiddenLayerDensities).
construct_Genotype(FileName,SensorName,ActuatorName,HiddenLayerDensities)->
	S = create_Sensor(SensorName),
	A = create_Actuator(ActuatorName),
	Output_VL = A#actuator.vl,
	LayerDensities = lists:append(HiddenLayerDensities,[Output_VL]),
	Cx_Id = {cortex,generate_id()},
	
	Neurons = create_NeuroLayers(Cx_Id,S,A,LayerDensities), 
	[Input_Layer|_] = Neurons, 
	[Output_Layer|_] = lists:reverse(Neurons), 
	FL_NIds = [N#neuron.id || N <- Input_Layer], 
	LL_NIds = [N#neuron.id || N <-  Output_Layer], 
	NIds = [N#neuron.id || N <- lists:flatten(Neurons)],
	Sensor = S#sensor{cx_id = Cx_Id, fanout_ids = FL_NIds},
	Actuator = A#actuator{cx_id=Cx_Id,fanin_ids = LL_NIds},
	Cortex = create_Cortex(Cx_Id,[S#sensor.id],[A#actuator.id],NIds), 
	Genotype = lists:flatten([Cortex,Sensor,Actuator|Neurons]),
	{ok, File} = file:open(FileName, write),
	lists:foreach(fun(X) -> io:format(File, "~p.~n",[X]) end, Genotype),
	file:close(File),
	Genotype.
%The construct_Genotype function accepts the name of the file to which we'll save the genotype, sensor name, actuator name, and the hidden layer density parameters. We have to generate unique Ids for every sensor and actuator. The sensor and actuator names are used as input to the create_Sensor and create_Actuator functions, which in turn generate the actual Sensor and Actuator representing tuples. We create unique Ids for sensors and actuators so that when in the future a NN uses 2 or more sensors or actuators of the same type, we will be able to differentiate between them using their Ids. After the Sensor and Actuator tuples are generated, we extract the NN's input and output vector lengths from the sensor and actuator used by the system. The Input_VL is then used to specify how many weights the neurons in the input layer will need, and the Output_VL specifies how many neurons are in the output layer of the NN. After appending the HiddenLayerDensites to the now known number of neurons in the last layer to generate the full LayerDensities list, we use the create_NeuroLayers function to generate the Neuron representing tuples. We then update the Sensor and Actuator records with proper fanin and fanout ids from the freshly created Neuron tuples, composes the Cortex, and write the genotype to file.

	create_Sensor(SensorName) -> 
		case SensorName of
			rng ->
				#sensor{id={sensor,generate_id()},name=rng,vl=2};
			_ ->
				exit("System does not yet support a sensor by the name:~p.",[SensorName])
		end.
	
	create_Actuator(ActuatorName) ->
		case ActuatorName of
			pts ->
				#actuator{id={actuator,generate_id()},name=pts,vl=1};
			_ ->
				exit("System does not yet support an actuator by the name:~p.",[ActuatorName])
		end.
%Every sensor and actuator uses some kind of function associated with it. A function that either polls the environment for sensory signals (in the case of a sensor) or acts upon the environment (in the case of an actuator). It is a function that we need to define and program before it is used, and the name of the function is the same as the name of the sensor or actuator it self. For example, the create_Sensor/1 has specified only the rng sensor, because that is the only sensor function we've finished developing. The rng function has its own vl specification, which will determine the number of weights that a neuron will need to allocate if it is to accept this sensor's output vector. The same principles apply to the create_Actuator function. Both, create_Sensor and create_Actuator function, given the name of the sensor or actuator, will return a record with all the specifications of that element, each with its own unique Id.

	create_NeuroLayers(Cx_Id,Sensor,Actuator,LayerDensities) ->
		Input_IdPs = [{Sensor#sensor.id,Sensor#sensor.vl}],
		Tot_Layers = length(LayerDensities),
		[FL_Neurons|Next_LDs] = LayerDensities, 
		NIds = [{neuron,{1,Id}}|| Id <- generate_ids(FL_Neurons,[])],
		create_NeuroLayers(Cx_Id,Actuator#actuator.id,1,Tot_Layers,Input_IdPs,NIds,Next_LDs,[]). 
%The function create_NeuroLayers/3 prepares the initial step before starting the recursive create_NeuroLayers/7 function which will create all the Neuron records. We first generate the place holder Input Ids “plus”(Input_IdPs), which are tuples composed of Ids and the vector lengths of the incoming signals associated with them. The proper input_idps will have a weight list in the tuple instead of the vector length. Because we are only building NNs each with only a single Sensor and Actuator, the IdP to the first layer is composed of the single Sensor Id with the vector length of its sensory signal, likewise in the case of the Actuator. We then generate unique ids for the neurons in the first layer, and drop into the recursive create_NeuroLayers/7 function.

	create_NeuroLayers(Cx_Id,Actuator_Id,LayerIndex,Tot_Layers,Input_IdPs,NIds,[Next_LD|LDs],Acc) ->
		Output_NIds = [{neuron,{LayerIndex+1,Id}} || Id <- generate_ids(Next_LD,[])], 
		Layer_Neurons = create_NeuroLayer(Cx_Id,Input_IdPs,NIds,Output_NIds,[]), 
		Next_InputIdPs = [{NId,1}|| NId <- NIds],
		create_NeuroLayers(Cx_Id,Actuator_Id,LayerIndex+1,Tot_Layers,Next_InputIdPs,Output_NIds,LDs,[Layer_Neurons|Acc]);
	create_NeuroLayers(Cx_Id,Actuator_Id,Tot_Layers,Tot_Layers,Input_IdPs,NIds,[],Acc) -> 
		Output_Ids = [Actuator_Id], 
		Layer_Neurons = create_NeuroLayer(Cx_Id,Input_IdPs,NIds,Output_Ids,[]), 
		lists:reverse([Layer_Neurons|Acc]).
%During the first iteration, the first layer neuron ids constructed in create_NeuroLayers/3 are held in the NIds variable. In create_NeuroLayers/7, with every iteration we generate the Output_NIds, which are the Ids of the neurons in the next layer. The last layer is a special case which occurs when LayerIndex == Tot_Layers. Having the Input_IdPs, and the Output_NIds, we are able to construct a neuron record for every Id in NIds using the function create_layer/4. The Ids of the constructed Output_NIds will become the NIds variable of the next iteration, and the Ids of the neurons in the current layer will be extended and become Next_InputIdPs. We then drop into the next iteration with the newly prepared Next_InputIdPs and Output_NIds. Finally, when we reach the last layer, the Output_Ids is the list containing a single Id of the Actuator element. We use the same function, create_NeuroLayer/4, to construct the last layer and return the result.

		create_NeuroLayer(Cx_Id,Input_IdPs,[Id|NIds],Output_Ids,Acc) ->
			Neuron = create_Neuron(Input_IdPs,Id,Cx_Id,Output_Ids), 
			create_NeuroLayer(Cx_Id,Input_IdPs,NIds,Output_Ids,[Neuron|Acc]); 
		create_NeuroLayer(_Cx_Id,_Input_IdPs,[],_Output_Ids,Acc) ->
			Acc.
%To create neurons from the same layer, all that is needed are the Ids for those neurons, a list of Input_IdPs for every neuron so that we can create the proper number of weights, and a list of Output_Ids. Since in our simple feed forward neural network all neurons are fully connected to the neurons in the next layer, the Input_IdPs and Output_Ids are the same for every neuron belonging to the same layer.
		
		create_Neuron(Input_IdPs,Id,Cx_Id,Output_Ids)-> 
			Proper_InputIdPs = create_NeuralInput(Input_IdPs,[]), 
			#neuron{id=Id,cx_id = Cx_Id,af=tanh,input_idps=Proper_InputIdPs,output_ids=Output_Ids}. 

			create_NeuralInput([{Input_Id,Input_VL}|Input_IdPs],Acc) ->
				Weights = create_NeuralWeights(Input_VL,[]),
				create_NeuralInput(Input_IdPs,[{Input_Id,Weights}|Acc]); 
			create_NeuralInput([],Acc)-> 
				lists:reverse([{bias,random:uniform()-0.5}|Acc]).
			 
				create_NeuralWeights(0,Acc) ->
					Acc; 
				create_NeuralWeights(Index,Acc) ->
					W = random:uniform()-0.5, 
					create_NeuralWeights(Index-1,[W|Acc]). 
%Each neuron record is composed by the create_Neuron/3 function. The create_Neuron/3 function creates the Input list from the tuples [{Id,Weights}...] using the vector lengths specified in the place holder Input_IdPs. The create_NeuralInput/2 function uses create_NeuralWeights/2 to generate the random weights in the range of -0.5 to 0.5, adding the bias to the end of the list.

			generate_ids(0,Acc) ->
				Acc; 
			generate_ids(Index,Acc)-> 
				Id = generate_id(), 
				generate_ids(Index-1,[Id|Acc]). 
	 
			generate_id() ->
				{MegaSeconds,Seconds,MicroSeconds} = now(), 
				1/(MegaSeconds*1000000 + Seconds + MicroSeconds/1000000). 
%The generate_id/0 creates a unique Id using current time, the Id is a floating point value. The generate_ids/2 function creates a list of unique Ids.

	create_Cortex(Cx_Id,S_Ids,A_Ids,NIds) ->
		#cortex{id = Cx_Id, sensor_ids=S_Ids, actuator_ids=A_Ids, nids = NIds}.
%The create_Cortex/4 function generates the record encoded genotypical representation of the cortex element. The Cortex element needs to know the Id of every Neuron, Sensors, and Actuator in the NN. 
