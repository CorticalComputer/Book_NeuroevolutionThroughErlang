%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(scape).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,prep,[ExoSelf_PId]).

prep(ExoSelf_PId) ->
	receive 
		{ExoSelf_PId,Name} ->
			scape:Name(ExoSelf_PId)
	end.

xor_sim(ExoSelf_PId)->
	XOR = [{[-1,-1],[-1]},{[1,-1],[1]},{[-1,1],[1]},{[1,1],[-1]}],
	xor_sim(ExoSelf_PId,{XOR,XOR},0).
	
xor_sim(ExoSelf_PId,{[{Input,CorrectOutput}|XOR],MXOR},ErrAcc) ->
	receive 
		{From,sense} ->
			From ! {self(),percept,Input},
			xor_sim(ExoSelf_PId,{[{Input,CorrectOutput}|XOR],MXOR},ErrAcc);
		{From,action,Output}->
			Error = sse(Output,CorrectOutput,0),
			%io:format("{Output,TargetOutput}:~p~n",[{Output,CorrectOutput}]),
			case XOR of
				[] ->
					SSE = ErrAcc+Error,
					Fitness = 1/(SSE+0.000001),
					From ! {self(),Fitness,1},
					xor_sim(ExoSelf_PId,{MXOR,MXOR},0);
				_ ->
					From ! {self(),0,0},
					xor_sim(ExoSelf_PId,{XOR,MXOR},ErrAcc+Error)
			end;
		{ExoSelf_PId,terminate}->
			ok
	end.

		
	sse([T|Target],[O|Output],SSEAcc)->
		SSE = math:pow(T-O,2),
		sse(Target,Output,SSE+SSEAcc);
	sse([],[],SSEAcc)->
		SSEAcc.
