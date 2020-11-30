%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Use bic to parse c code and pass to unroll
%%% @end
%%% Created : 29 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(unrollc).

-export([file/1]).
-compile(export_all).

file(File) ->
    case bic:file(File) of
	{ok,CForms} ->
	    simple(CForms);
	Error ->
	    Error
    end.

simple(CForms) ->
    {ok,CForms}.




