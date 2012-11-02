%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(benchmarker).
-compile(export_all).
-include("records.hrl").
-define(MAX_ATTEMPTS,5).
-define(EVAL_LIMIT,inf).
-define(FITNESS_TARGET,inf).
-define(TOT_RUNS,100).
-define(MORPHOLOGY,xor_mimic).

go(Morphology,HiddenLayerDensities)->
	go(Morphology,HiddenLayerDensities,?TOT_RUNS).
go(Morphology,HiddenLayerDensities,TotRuns)->
	go(Morphology,HiddenLayerDensities,?MAX_ATTEMPTS,?EVAL_LIMIT,?FITNESS_TARGET,TotRuns).
go(Morphology,HiddenLayerDensities,MaxAttempts,EvalLimit,FitnessTarget,TotRuns)->
	PId = spawn(benchmarker,loop,[Morphology,HiddenLayerDensities,MaxAttempts,EvalLimit,FitnessTarget,TotRuns,[],[],[],[]]),
	register(benchmarker,PId).
% The benchmarker is started through the go/2, go/3, or go/6 function. The parameters the benchmark uses can be specified through the macros, and then used by executing go/2 or go/3 for which the researcher simply specifies the Morphology (the problem on which the NN will be benchmarked) and the HiddenLayerDensities (NN topology). The go/2 and go/3 functions execute go/6 function with default parameters. The benchmarker can also be started through go/6, using which the researcher can manually specify all the parameters: morphology, NN topology, Max Attempts, Max Evaluations, target fitness, and the total number of times to run the trainer. Before dropping into the main loop, go/6 registers the benchmarker process so that the trainer can send it the performance stats when it finishes.
	
loop(Morphology,_HiddenLayerDensities,_MA,_EL,_FT,0,FitnessAcc,EvalsAcc,CyclesAcc,TimeAcc)->
	io:format("Benchmark results for:~p~n",[Morphology]),
	io:format("Fitness::~n Max:~p~n Min:~p~n Avg:~p~n Std:~p~n",[lists:max(FitnessAcc),lists:min(FitnessAcc),avg(FitnessAcc),std(FitnessAcc)]),
	io:format("Evals::~n Max:~p~n Min:~p~n Avg:~p~n Std:~p~n",[lists:max(EvalsAcc),lists:min(EvalsAcc),avg(EvalsAcc),std(EvalsAcc)]),
	io:format("Cycles::~n Max:~p~n Min:~p~n Avg:~p~n Std:~p~n",[lists:max(CyclesAcc),lists:min(CyclesAcc),avg(CyclesAcc),std(CyclesAcc)]),
	io:format("Time::~n Max:~p~n Min:~p~n Avg:~p~n Std:~p~n",[lists:max(TimeAcc),lists:min(TimeAcc),avg(TimeAcc),std(TimeAcc)]);
loop(Morphology,HiddenLayerDensities,MA,EL,FT,BenchmarkIndex,FitnessAcc,EvalsAcc,CyclesAcc,TimeAcc)->
	Trainer_PId = trainer:go(Morphology,HiddenLayerDensities,MA,EL,FT),
	receive
		{Trainer_PId,Fitness,Evals,Cycles,Time}->
			loop(Morphology,HiddenLayerDensities,MA,EL,FT,BenchmarkIndex-1,[Fitness|FitnessAcc],[Evals|EvalsAcc],[Cycles|CyclesAcc],[Time|TimeAcc]);
		terminate ->
			loop(Morphology,HiddenLayerDensities,MA,EL,FT,0,FitnessAcc,EvalsAcc,CyclesAcc,TimeAcc)
	end.
% Once the benchmarker is started, it drops into its main loop. The main loop spawns the trainer and waits for it to finish optimizing the NN system, after which it sends to the benchmarker the performance based statistics. The benchmarker accumulates these performance statistics in lists, rerunning the trainer TotRuns number of times. Once the benchmarker has ran the trainer TotRuns number of times, indicated to be so when BenchmarkIndex reaches 0, it calculates the Max, Min, Average, and Standard Deviation values for every statistic list it accumulated.


avg(List)->
	lists:sum(List)/length(List).
std(List)->
	Avg = avg(List),
	std(List,Avg,[]).
	
	std([Val|List],Avg,Acc)->
		std(List,Avg,[math:pow(Avg-Val,2)|Acc]);
	std([],_Avg,Acc)->
		Variance = lists:sum(Acc)/length(Acc),
		math:sqrt(Variance).
%avg/1 and std/1 functions calculate the average and the standard deviation values of the lists passed to them.
