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
%
	
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
%

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
%
