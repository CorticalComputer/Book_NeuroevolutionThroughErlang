%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(functions).
-compile(export_all).
	
saturation(Val)->
	case Val > 1000 of
		true ->
			1000;
		false ->
			case Val < -1000 of
				true ->
					-1000;
				false ->
					Val
			end
	end.
%The function saturation/1 accepts a value Val, and returns the same if its magnitude is below 1000. Otherwise it returns -1000 or 1000, if it’s less than or greater than -1000 or 1000 respectively. Thus Val saturates at -1000 and 1000.
	
saturation(Val,Spread)->
	case Val > Spread of
		true ->
			Spread;
		false ->
			case Val < -Spread of
				true ->
					-Spread;
				false ->
					Val
			end
	end.
%The saturation/2 function is similar to saturation/1, but here the spread (symmetric Max and Min values) is specified by the caller.

scale([H|T],Max,Min)->
	[scale(Val,Max,Min)||Val<-[H|T]];
scale(Val,Max,Min)-> %Nm = (Y*2 - (Max + Min))/(Max-Min)
	case Max == Min of
		true ->
			0;
		false ->
			(Val*2 - (Max+Min))/(Max-Min)
	end.
%The scale/3 function accepts a list of values, and scales them to be between the specified Min and Max values.

sat(Val,Max,Min)->
	case Val > Max of
		true ->
			Max;
		false ->
			case Val < Min of
				true ->
					Min;
				false ->
					Val
			end
	end.
%The sat/3 function is similar to saturation/2 function, but here the Max and Min can be different, and are specified by the caller.

sat_dzone(Val,Max,Min,DZMax,DZMin)->
	case (Val < DZMax) and (Val > DZMin) of
		true ->
			0;
		false ->
			sat(Val,Max,Min)
	end.
%The sat_dzone/5 function is similar to the sat/3 function, but here, if Val is between DZMin and DZMax, it is zeroed.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Activation Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tanh(Val)->		
	math:tanh(Val).
		
cos(Val)->
	math:cos(Val).

sin(Val)->
	math:sin(Val).

sgn(0)->
	0;
sgn(Val)->
	case Val > 0 of
		true -> 1;
		false -> -1
	end.

bin(Val)->
	case Val > 0 of
		true -> 1;
		false -> 0
	end.
% The bin/1 function converts Val into a binary value, 1 if Val > 0, and 0 if Val = < 0.

trinary(Val)->
	if
		(Val < 0.33) and (Val > -0.33) -> 0;
		Val >= 0.33 -> 1;
		Val =< -0.33 -> -1
	end.
%The trinary/1 function converts Val into a trinary value.
	
multiquadric(Val)->
	math:pow(Val*Val + 0.01,0.5).

absolute(Val)->
	abs(Val).
	
linear(Val)->
	Val.

quadratic(Val)->
	sgn(Val)*Val*Val.

gaussian(Val)->
	gaussian(2.71828183,Val).

gaussian(Const,Val)->
	V = case Val > 10 of
		true ->
			10;
		false ->
			case Val < -10 of
				true ->
					-10;
				false ->
					Val
			end
	end,
	math:pow(Const,-V*V).

sqrt(Val)->
	sgn(Val)*math:sqrt(abs(Val)).
	
log(Val)->
	case Val == 0 of
		true ->
			0;
		false ->
			sgn(Val)*math:log(abs(Val))
	end.

sigmoid(Val)-> %(-1 : 1)--Der:Y*(1-Y)
	V = case Val > 10 of
		true ->
			10;
		false ->
			case Val < -10 of
				true ->
					-10;
				false ->
					Val
			end
	end,
	2/(1+math:pow(2.71828183,-V)) - 1.

sigmoid1(Val)-> %(-1 : 1) -- Der:1/((1+abs(val))*(1+abs(val)))
	Val/(1+abs(Val)).

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
