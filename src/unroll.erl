%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Code unrolling
%%% @end
%%% Created : 25 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(unroll).

-export([expand/2]).

-compile(export_all). %% test

%%
%% Code primitives
%%   

expand({'for',Var,Start,Stop,Code}, E) when Start =< Stop ->
    expand_loop(Var,Start,Stop,1,Code,E);
expand({'for',Var,Start,Stop,Code}, E) when Start > Stop ->
    expand_loop(Var,Stop,Start,-1,Code,E);
expand({'for',Var,Start,Stop,Step,Code}, E) ->
    expand_loop(Var,Start,Stop,Step,Code,E);
expand({'if',Cond,Then,Else}, E) when not is_list(Cond) ->
    case eval_expr(Cond, E) of
	true -> expand(Then,E);
	false -> expand(Else,E);
	Cond1 -> {'if',Cond1,expand(Then,E),expand(Else,E)}
    end;
expand({'if',Cond,Then}, E) ->
    case eval_expr(Cond, E) of
	true -> expand(Then,E);
	false -> []
    end;
expand({'ret',Expr}, E) ->
    {'ret', expand(Expr, E)};
expand({'decl',Var,Expr}, E) ->
    {'decl', Var, expand(Expr, E)};
expand({'decl',Var}, _E) ->
    {'decl', Var};
expand({'defun',Fun,Args,Code}, E) ->
    {'defun', Fun, Args, expand(Code, E)};
expand(Expr, _E) when is_number(Expr); is_boolean(Expr) ->
    Expr;
expand(Expr, E) when is_tuple(Expr) ->
    case is_expr(Expr) of
	false ->
	    case Expr of
		{Name,Output,Input} ->
		    XOutput = expand_var_list(Output,E),
		    XInput = expand_var_list(Input,E),
		    {Name,XOutput,XInput}
	    end;
	_Prio ->
	    eval_expr(Expr, E)
    end;
expand([H|T], E) ->
    [expand(H,E)|expand(T,E)];
expand([], _Env) ->
    [].

%% check if tuple is and expression 
%% return priority on success and false on failure
is_expr(true) -> 0;
is_expr(false) -> 0;
is_expr(Const) when is_number(Const) -> 0;
is_expr({var,_}) -> 0;
is_expr({subscript,_,_}) -> 0;
is_expr({'+',_}) -> 5;
is_expr({'-',_}) -> 5;
is_expr({'!',_}) -> 5;
is_expr({'~',_}) -> 5;
is_expr({'*',_,_}) -> 10;
is_expr({'/',_,_}) -> 10;
is_expr({'%',_,_}) -> 10;
is_expr({'+',_,_}) -> 20;
is_expr({'-',_,_}) -> 20;
is_expr({'<<',_,_}) -> 30;
is_expr({'>>',_,_}) -> 30;
is_expr({'<',_,_}) -> 40;
is_expr({'<=',_,_}) -> 40;
is_expr({'>',_,_}) -> 40;
is_expr({'>=',_,_}) -> 40;
is_expr({'==',_,_}) -> 50;
is_expr({'!=',_,_}) -> 50;
is_expr({'&',_,_}) -> 60;
is_expr({'^',_,_}) -> 70;
is_expr({'|',_,_}) -> 80;
is_expr({'&&',_,_}) -> 90;
is_expr({'||',_,_}) -> 100;
is_expr({'?:',_,_,_}) -> 110;
is_expr({'=',_,_}) -> 120;
is_expr(_) -> false.

    
%% for I=1 TO n [step 1] ..
%% for I=m DOWNTO 1 [step -1]
expand_loop(Var,I,N,S,Code,E) ->
    if S > 0, I =< N;
       S < 0, I >= N ->
	    [expand(Code, E#{ Var => I}) |
	     expand_loop(Var,I+S,N,S,Code,E)];
       true ->
	    []
    end.

expand_var_list(Vs,E) -> 
    [eval_expr(V,E) || V <- Vs].

%% meta expressions
eval_expr(Const, _E) when is_number(Const) -> Const;
eval_expr(Const, _E) when is_boolean(Const) -> Const;
eval_expr(Var={var,_}, E) -> 
    maps:get(Var, E, Var);
eval_expr({subscript,Var,Index},E) ->
    Is = [eval_expr(I,E) || I <- Index],
    case eval_expr(Var,E) of
	V1 = {var,_} ->
	    maps:get({subscript,V1,Is}, E, {subscript,V1,Is});
	Map when is_map(Map) ->
	    maps:get(Is, Map);
	V1 ->
	    {subscript,V1,Is}
    end;
eval_expr({'-',A},E) -> fx('-', '-', A, E);
eval_expr({'~',A},E) -> fx('bnot', '~', A, E);
eval_expr({'!',A},E) -> fx('not', '!', A, E);
eval_expr({'+',A,B},E) -> fx('+', '+', A, B, E);
eval_expr({'-',A,B},E) -> fx('-', '-', A, B, E);
eval_expr({'*',A,B},E) -> fx('*', '*', A, B, E);
eval_expr({'/',A,B},E) -> fx('div', '/', A, B, E);
eval_expr({'%',A,B},E) -> fx('rem', '%', A, B, E);
eval_expr({'&',A,B},E) -> fx('band', '&', A, B, E);
eval_expr({'|',A,B},E) -> fx('bor', '|', A, B, E);
eval_expr({'^',A,B},E) -> fx('bxor', '^', A, B, E);
eval_expr({'<<',A,B},E) -> fx('bsl', '<<', A, B, E);
eval_expr({'>>',A,B},E) -> fx('bsr', '>>', A, B, E);
eval_expr({'<',A,B},E) -> fx('<', '<', A, B, E);
eval_expr({'<=',A,B},E) -> fx('=<', '<=', A, B, E);
eval_expr({'>',A,B},E) -> fx('>', '>', A, B, E);
eval_expr({'>=',A,B},E) -> fx('>=', '>=', A, B, E);
eval_expr({'==',A,B},E) -> fx('=:=', '==', A, B, E);
eval_expr({'!=',A,B},E) -> fx('=/=', '!=', A, B, E);
eval_expr({'&&',A,B},E) -> fx('and', '&&', A, B, E);
eval_expr({'||',A,B},E) -> fx('or', '||', A, B, E);
eval_expr({'?:',C,A,B},E) -> ite('?:', C, A, B, E);
eval_expr({'=',Var,V},E) -> fa('=',Var,V,E).
    
is_constant(Value) ->
    is_number(Value) orelse is_boolean(Value).

fa(Op,Var,V,E) ->
    {Op,eval_expr(Var,E),eval_expr(V,E)}.

ite(_Op,C, A, B, E) ->
    case eval_expr(C,E) of
	true -> eval_expr(A,E);
	false -> eval_expr(B,E);
	C1 -> {'?:',C1,eval_expr(A,E),eval_expr(B,E)}
    end.

fx(EOp,Op,A,E) ->
    A1 = eval_expr(A,E),
    case is_constant(A1) of
	true -> apply(erlang, EOp, [A1]);
	false -> {Op,A1}
    end.

fx(EOp,Op,A,B,E) ->
    A1 = eval_expr(A,E),
    B1 = eval_expr(B,E),
    case is_constant(A1) andalso is_constant(B1) of
	true -> apply(erlang, EOp, [A1,B1]);
	false -> {Op,A1,B1}
    end.

format({'if',Cond,Then,Else}) when not is_list(Cond) ->
    ["  ","if (",format_expr(Cond),")", 
     "{", format(Then), ";", "}",
     "{", format(Else), ";", "}",
     "\n"];
format({'if',Cond,Then}) ->
    ["  ","if (",format_expr(Cond),")", 
     "{", format(Then), ";", "}",
     "\n"];
format({'ret',Expr}) ->
    ["  ", "return ", format_expr(Expr), ";\n"];
format({'decl',Var,Expr}) ->
    ["  ", "UINT_T ", format_var(Var), "=", format_expr(Expr), ";\n"];
format({'decl',Var}) ->
    ["  ", "UINT_T ", format_var(Var), ";\n"];
format({'defun',Fun,Args,Code}) when is_atom(Fun) ->
    ["UINT_T ",atom_to_list(Fun),"(",
     lists:join(",",[format_formal(A) || A <- Args]),
     ")","\n",
     "{\n",
     format(Code),
     "}\n"];
format(Expr) when is_tuple(Expr) ->
    case is_expr(Expr) of
	false ->
	    case Expr of
		{Op,Output,Input} when is_atom(Op) ->
		    ["  ",atom_to_list(Op),"(",
		     lists:join(",", 
				[format_expr(I)||I<-Input] ++
				    [["&",format_expr(O)]||O<-Output]),
		     ");\n"]
	    end;
	_Prio ->
	    ["  ",format_expr(Expr), ";\n"]
    end;
format([H|T]) ->
    [format(H) | format(T)];
format([]) ->
    [].

format_formal({decl,Var}) ->
    ["UINT_T ", format_var(Var)].

format_var({var,V}) -> 
    atom_to_list(V);
format_var({subscript,{var,V},Is}) ->
    [atom_to_list(V),"[",lists:join(",",[format_expr(I)||I<-Is]),"]"].

format_expr(Const) when is_integer(Const) -> integer_to_list(Const);
format_expr(true) -> "true";
format_expr(false) -> "false";
format_expr({var,V}) -> atom_to_list(V);
format_expr({subscript,{var,V},Is}) ->
    [atom_to_list(V),"[",lists:join(",",[format_expr(I)||I<-Is]),"]"];
format_expr({Op,A}) when is_atom(Op) ->
    Ap = is_expr(A),
    if Ap > 0 ->
	    [atom_to_list(Op),"(",format_expr(A),")"];
       true ->
	    [atom_to_list(Op),format_expr(A)]
    end;
format_expr(E={Op,A,B}) ->
    Ep = is_expr(E),
    Ap = is_expr(A),
    Bp = is_expr(B),
    Af0 = format_expr(A),
    Bf0 = format_expr(B),
    Af = if Ap > Ep -> ["(",Af0,")"];
	    true -> Af0
	 end,
    Bf = if Bp > Ep -> ["(",Bf0,")"];
	    true -> Bf0
	 end,
    [Af,atom_to_list(Op),Bf];
format_expr({'?:',C,A,B}) ->
    [format_expr(C),"?",format_expr(A),":",format_expr(B)].

%%
%% Simple tests
%%

code1() ->
    X = {var,x},
    Y = {var,x},
    I = {var,i},
    {'for', I, 1, 10,
     {add,[{subscript,Y,[I]}],[{subscript,X,[I]},
			       {subscript,X,[{'-',I,1}]}]}}.

test1() ->
    X = expand(code1(), #{}),
    io:put_chars(format(X)).


code2() ->
    X = {var,x},
    I = {var,i},
    {'for', I, 1, 10,
     {'=', {subscript,X,[I]}, 0}}.

test2() ->
    X = expand(code2(), #{}),
    io:put_chars(format(X)).

test3() ->
    test3(4, 4).

test4() ->
    test3(16, 16).

test3(Xl, Yl) ->
    I = {var,i},
    J = {var,j},
    IJ = {var,ij},
    X = {var,x},
    Y = {var,y},
    R = {var,r},
    Cp = {var,cp},
    C  = {var,c},
    P  = {var,p},

    Name = "mul_"++ integer_to_list(Xl)++"x"++
	integer_to_list(Yl),
    FuncName = list_to_atom(Name),
    Code =
	[{defun,FuncName,[{decl,{subscript,X,[Xl]}},
			  {decl,{subscript,Y,[Yl]}},
			  {decl,{subscript,R,[Xl+Yl]}}],
	  [
	   {decl, Cp},
	   {decl, C},
	   {decl, IJ},
	   {decl,P},
	 
	   {'for',I,0,Xl-1,1,
	    [
	     {'=', Cp, 0},
	     {'=', C, 0},
	     {'=', IJ, I},
	     {'for',J,0,Yl-1,1,
	      [
	       {mula,[Cp,P],[{subscript,X,[I]},{subscript,Y,[J]},Cp]},
	       {addc,[C,{subscript,R,[IJ]}],[P,{subscript,R,[IJ]},C]},
	       {'=', IJ, {'+',IJ,1}}
	      ]},
	     {add0,[{subscript,R,[IJ]}],[C,Cp]}
	    ]},
	   {'ret',{'?:',{'==',{subscript,R,[Xl+Yl-1]},0},Xl+Yl-1,Xl+Yl}}
	  ]}],
    XCode = expand(Code, #{}),
    file:write_file("unroll_"++Name++".c",
		    ["#include \"unroll_auto_config.h\"","\n",
		     "#include \"unroll.i\"","\n",
		     "\n",
		     format(XCode)]).


