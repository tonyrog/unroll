%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Code unrolling
%%% @end
%%% Created : 25 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(unroll).

-export([expand/2]).

-compile(export_all). %% test

%% fixme add a cleanup pass
%% remove variable assignments not used, keep the last one, if used
%% remove declarations not used (all references are expanded)

%% expand/unroll code
expand(Stmt) ->
    expand(Stmt, new_env()).

expand(Stmt, Env) ->
    {Stmt1, _Env} = statement(Stmt, Env),
    Stmt1.

%% loop (var,init,stop,step) statement
statement({'loop',Var,Start,Stop,Code}, E) ->
    {Start1,E1} = expr(Start,E),
    {Stop1,E1} = expr(Stop,E),
    if Start1 =< Stop1 ->
	    loop(Var,Start1,Stop1,1,Code,E1);
       Start1 > Stop1 ->
	    loop(Var,Stop1,Start1,-1,Code,E1)
    end;
statement({'loop',Var,Start,Stop,Step,Code}, E) ->
    {Start1,E1} = expr(Start,E),
    {Stop1,E1} = expr(Stop,E),
    {Step1,E1} = expr(Step,E),
    loop(Var,Start1,Stop1,Step1,Code,E1);
%% for (init, cond, update)
statement({'for',Init,Cond,Update,Code},E) ->
    for(Init,Cond,Update,Code,E);
statement({'while',Cond,Code},E) ->
    while(Cond,Code,E);
statement({'do',Code,Cond},E) ->
    do(Code,Cond,E);

statement({'if',Cond,Then,Else}, E) when not is_list(Cond) ->
    {Value1,E1} = expr(Cond, E),
    if is_integer(Value1), Value1 =/= 0; Value1 =:= true ->
	    block(Then,E1);
       is_integer(Value1), Value1 =:= 0; Value1 =:= false ->
	    block(Else,E1);
       true ->
	    {Then1,E2} = block(Then,E1),
	    {Else1,E2} = block(Else,E1), %% match E2!
	    {{'if',Value1,Then1,Else1},E2}
    end;
statement({'if',Cond,Then}, E) ->
    {Value1,E1} = expr(Cond, E),
    if is_integer(Value1), Value1 =/= 0; Value1 =:= true ->
	    block(Then,E1);
       is_integer(Value1), Value1 =:= 0; Value1 =:= false ->
	    {[],E1};
       true ->
	    {Then1,E2} = block(Then,E1),
	    {{'if',Value1,Then1},E2}
    end;
statement({'ret',Expr}, E) ->
    {Expr1, E1} = expr(Expr, E),
    {{'ret', Expr1}, E1};
statement({'decl',Var,Expr}, E) ->
    E1 = decl(Var,E),
    {Expr1, E2} = expr(Expr, E1),
    io:format("decl ~w ~w\n", [Var, Expr1]),
    {{'decl', Var, Expr1}, E2#{ Var => Expr1 }};
statement({'decl',Var}, E) ->
    E1 = decl(Var,E),
    io:format("decl ~w\n", [Var]),
    {{'decl', Var}, E1#{ Var => undefined }};
statement({'defun',Fun,Args,Code}, E) ->
    {Code1,E1} = statement(Code, E),
    {{'defun', Fun, Args, Code1}, E1};
statement(Expr, E) when is_number(Expr); is_boolean(Expr) ->
    {Expr,E};
statement(Expr, E) when is_tuple(Expr) ->
    case is_expr(Expr) of
	false ->
	    case Expr of
		{Name,Output,Input} ->
		    {XInput,E1}  = expr_list(Input,E),
		    {XOutput,E2} = ref_expr_list(Output,E1),
		    {{Name,XOutput,XInput},E2}
	    end;
	_Prio ->
	    expr(Expr, E)
    end;
statement([H|T], E) ->
    {H1,E1} = statement(H,E),
    {T1,E2} = statement(T,E1),
    {[H1|T1],E2};
statement([], E) ->
    {[],E}.

%% loop I=1 TO n [step 1] ..
%% loop I=m DOWNTO 1 [step -1]
loop(Var,I,N,S,Code,E) ->
    loop_(Var,I,N,S,Code,E,[]).

loop_(Var,I,N,S,Code,E,Acc) ->
    if S > 0, I =< N;
       S < 0, I >= N ->
	    {Code1,E1} = block([{decl,Var,I}|Code], E),
	    loop_(Var,I+S,N,S,Code,E1,[Code1|Acc]);
       true ->
	    {lists:reverse(Acc), E}
    end.

for(Init, Cond, Update, Code, E) ->
    {Init1,E1} = statement(Init, E),
    for_(Cond,Update,Code,E1,[Init1]).

for_(Cond,Update,Code,E,Acc) ->
    {Cond1,E1} = expr(Cond, E),
    if is_integer(Cond1), Cond1 =/= 0; Cond1 =:= true ->
	    {Code1,E2} = block(Code,E1),
	    {Update1,E3} = statement(Update,E2),
	    for_(Cond,Update,Code,E3,[Update1,Code1|Acc]);
       is_integer(Cond1), Cond1 =:= 0; Cond1 =:= false ->
	    {lists:reverse(Acc), E1};
       true ->
	    {lists:reverse([{'while',Cond1,cat(Code,Update)}|Acc]), E1}
    end.
    
while(Cond, Code, E) ->
    while_(Cond, Code, E, []).

while_(Cond, Code, E, Acc) ->
    {Cond1,E1} = expr(Cond, E),
    if is_integer(Cond1), Cond1 =/= 0; Cond1 =:= true ->
	    {Code1,E2} = block(Code,E1),
	    while_(Cond, Code, E2, [Code1|Acc]);
       is_integer(Cond1), Cond1 =:= 0; Cond1 =:= false ->
	    {lists:reverse(Acc), E1};
       true ->
	    {lists:reverse([{'while',Cond1,Code}|Acc]), E1}
    end.

do(Code, Cond, E) ->
    do_(Code, Cond, E, []).

do_(Code, Cond, E, Acc) ->
    {Code1,E1} = block(Code,E),
    {Cond1,E2} = expr(Cond, E1),
    if is_integer(Cond1), Cond1 =/= 0; Cond1 =:= true ->
	    do_(Code, Cond, E2, [Code1|Acc]);
       is_integer(Cond1), Cond1 =:= 0; Cond1 =:= false ->
	    {lists:reverse(Acc), E1};
       true ->
	    {lists:reverse([{'do',Code,Cond1}|Acc]), E2}
    end.

cat(Code1,Code2) when is_list(Code1), is_list(Code2) ->
    Code1 ++ Code2;
cat(Code1,Code2) when is_list(Code1) ->
    Code1 ++ [Code2];
cat(Code1,Code2) when is_list(Code2) ->
    [Code1|Code2];
cat(Code1,Code2) ->
    [Code1,Code2].


%% scope keeps current declarations
%% store keeps previous values
%% stack keeps store stack for reference (not used right now)
new_env() ->
    #{ scope => #{}, store => #{}, stack => [] }.

block(Code, E=#{ scope := Scope0, stack := Stack }) ->
    {Code1, E1} = statement(Code, E#{ scope => #{},
				      store => #{},
				      stack => [Scope0|Stack]}),
    Store = maps:get(store,E1),
    Scope = maps:get(scope,E1),
    %% restore hidden globals 
    E2 = maps:fold(fun(Var,_Type,Ei) ->
			   case maps:get(Var,Scope0,undefined) of
			       undefined ->
				   io:format("remove var ~w\n", [Var]),
				   maps:remove(Var, Ei);
			       _Type ->
				   Value0 = maps:get(Var,Store,undefined),
				   io:format("restore var ~w = ~w\n", 
					     [Var,Value0]),
				   Ei#{ Var => Value0 }
			   end
		   end, E1, Scope),
    {Code1, E2#{ scope => Scope0 }}.

decl(Var, E = #{ scope := Scope, store := Store }) ->
    Value = maps:get(Var, E, undefined),   %% previous value, if any
    io:format("save ~w = ~w\n", [Var, Value]),
    E#{ store => Store# { Var => Value },  %% save previous value in store
	scope => Scope#{ Var => unsigned }}.

    
%% check if tuple is and expression 
%% return priority on success and false on failure
is_expr(true) -> 0;
is_expr(false) -> 0;
is_expr(Const) when is_number(Const) -> 0;
is_expr({var,_}) -> 0;
is_expr({index,_,_}) -> 0;
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

expr_list(As, E) ->
    expr_list_(As, E, []).

expr_list_([A|As], E, Acc) ->
    {A1,E1} = expr(A, E),
    expr_list_(As, E1, [A1|Acc]);
expr_list_([], E, Acc) ->
    {lists:reverse(Acc), E}.


ref_expr_list(As, E) ->
    ref_expr_list_(As, E, []).

ref_expr_list_([A|As], E, Acc) ->
    {A1,E1} = ref_expr(A, E),
    ref_expr_list_(As, E1, [A1|Acc]);
ref_expr_list_([], E, Acc) ->
    {lists:reverse(Acc), E}.

ref_expr(Var={var,_}, E) ->
    {Var,E#{Var => undefined}};
ref_expr({index,Var,Index},E) ->
    {Index1,E1} = expr_list(Index,E),
    Ref = {index,Var,Index1},
    {Ref,E1#{ Ref => undefined}}.


%% meta expressions
expr(Const, E) when is_number(Const) -> {Const,E};
expr(Const, E) when is_boolean(Const) -> {Const,E};
expr(Var={var,_}, E) -> 
    {value(Var, E, Var), E};
expr({index,Var,Index},E) ->
    {Index1,E1} = expr_list(Index,E),
    case Var of
	{var,_} ->
	    A = {index,Var,Index1},
	    {value(A, E, A),E1};
	Map when is_map(Map) -> %% fixme: eval Var to map..
	    {maps:get(Index1, Map),E1};
	V1 ->
	    {{index,V1,Index1},E1}
    end;
expr({'-',A},E) -> fx('-', '-', A, E);
expr({'~',A},E) -> fx('bnot', '~', A, E);
expr({'!',A},E) -> fx('not', '!', A, E);
expr({'+',A,B},E) -> fx('+', '+', A, B, E);
expr({'-',A,B},E) -> fx('-', '-', A, B, E);
expr({'*',A,B},E) -> fx('*', '*', A, B, E);
expr({'/',A,B},E) -> fx('div', '/', A, B, E);
expr({'%',A,B},E) -> fx('rem', '%', A, B, E);
expr({'&',A,B},E) -> fx('band', '&', A, B, E);
expr({'|',A,B},E) -> fx('bor', '|', A, B, E);
expr({'^',A,B},E) -> fx('bxor', '^', A, B, E);
expr({'<<',A,B},E) -> fx('bsl', '<<', A, B, E);
expr({'>>',A,B},E) -> fx('bsr', '>>', A, B, E);
expr({'<',A,B},E) -> fx('<', '<', A, B, E);
expr({'<=',A,B},E) -> fx('=<', '<=', A, B, E);
expr({'>',A,B},E) -> fx('>', '>', A, B, E);
expr({'>=',A,B},E) -> fx('>=', '>=', A, B, E);
expr({'==',A,B},E) -> fx('=:=', '==', A, B, E);
expr({'!=',A,B},E) -> fx('=/=', '!=', A, B, E);
expr({'&&',A,B},E) -> fx('and', '&&', A, B, E);
expr({'||',A,B},E) -> fx('or', '||', A, B, E);
expr({'?:',C,A,B},E) ->
    case expr(C,E) of
	{true,E1} ->
	    expr(A,E1);
	{false,E1} ->
	    expr(B,E1);
	{C1,E1} ->
	    {A1,E3} = expr(A,E1),
	    {B1,E3} = expr(B,E1),  %% match E3!!
	    {{'?:',C1,A1,B1},E3}
    end;
expr({'=',Lhs,V},E) -> 
    {V1,E1} = expr(V, E),
    case Lhs of
	{var,_X} ->
	    %% fixme: check that {var,X} is declared
	    case is_constant(V1) of
		true ->
		    io:format("assign ~w = ~w\n", [Lhs, V1]),
		    {{'=',Lhs,V1}, E1#{ Lhs => V1 }};
		false ->
		    {{'=',Lhs,V1}, E1}
	    end;
	{index,X,Index} ->
	    %% fixme: check that X is declared array
	    {Index1,E2} = expr_list(Index, E1), %% fixme order?
	    Lhs1 = {index,X,Index1},
	    case is_constant(V1) of
		true ->
		    io:format("assign ~w = ~w\n", [Lhs1, V1]),
		    {{'=',Lhs1,V1}, E2#{ Lhs1 => V1}};
		false ->
		    {{'=',Lhs1,V1}, E2}
	    end
    end.

value(Var, E, Default) ->
    case maps:get(Var,E,undefined) of
	undefined -> Default;
	Value -> Value
    end.
	     
	
is_constant(Value) ->
    is_number(Value) orelse is_boolean(Value).


fx(EOp,Op,A,E) ->
    {A1,E1} = expr(A,E),
    case is_constant(A1) of
	true -> 
	    {apply(erlang, EOp, [A1]), E1};
	false -> 
	    {{Op,A1},E1}
    end.

fx(EOp,Op,A,B,E) ->
    {A1,E1} = expr(A,E),
    {B1,E2} = expr(B,E1),
    case is_constant(A1) andalso is_constant(B1) of
	true ->
	    {apply(erlang, EOp, [A1,B1]),E2};
	false -> 
	    {{Op,A1,B1}, E2}
    end.

format({'if',Cond,Then,Else}) ->
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
format(Expr)  when is_number(Expr); is_boolean(Expr) ->
    format_expr(Expr);
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

format_formal({'decl',Var}) ->
    ["UINT_T ", format_var(Var)].

format_var({var,V}) -> 
    atom_to_list(V);
format_var({array,{var,V},Is}) ->
    [atom_to_list(V),"[",lists:join(",",[format_expr(I)||I<-Is]),"]"].

format_expr(Const) when is_integer(Const) -> integer_to_list(Const);
format_expr(true) -> "true";
format_expr(false) -> "false";
format_expr({var,V}) -> atom_to_list(V);
format_expr({index,{var,V},Is}) ->
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
    {'loop', I, 1, 10,
     {add,[{index,Y,[I]}],[{index,X,[I]},
			   {index,X,[{'-',I,1}]}]}}.

test1() ->
    X = expand(code1()),
    io:put_chars(format(X)).


code2() ->
    X = {var,x},
    I = {var,i},
    {'loop', I, 1, 10,
     {'=', {index,X,[I]}, 0}}.

test2() ->
    X = expand(code2()),
    io:put_chars(format(X)).

code21() ->
    X = {var,x},
    I = {var,i},
    {'loop', I, 1, 10,
     {'=', {index,X,[I]}, I}}.

test21() ->
    X = expand(code21()),
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
	[{defun,FuncName,[{'decl',{array,X,[Xl]}},
			  {'decl',{array,Y,[Yl]}},
			  {'decl',{array,R,[Xl+Yl]}}],
	  [
	   {'decl', Cp},
	   {'decl', C},
	   {'decl', IJ},
	   {'decl',P},
	 
	   {'loop',I,0,Xl-1,1,
	    [
	     {'=', Cp, 0},
	     {'=', C, 0},
	     {'=', IJ, I},
	     {'loop',J,0,Yl-1,1,
	      [
	       {mula,[Cp,P],[{index,X,[I]},{index,Y,[J]},Cp]},
	       {addc,[C,{index,R,[IJ]}],[P,{index,R,[IJ]},C]},
	       {'=', IJ, {'+',IJ,1}}
	      ]},
	     {add0,[{index,R,[IJ]}],[C,Cp]}
	    ]},
	   {'ret',{'?:',{'==',{index,R,[Xl+Yl-1]},0},Xl+Yl-1,Xl+Yl}}
	  ]}],
    XCode = expand(Code),
    FCode = format(XCode),
    io:put_chars(FCode),
    file:write_file("unroll_"++Name++".c",
		    ["#include \"unroll_auto_config.h\"","\n",
		     "#include \"unroll.i\"","\n",
		     "\n",
		     FCode]).

test5() ->
    I = {var,i},
    X = {var,x},
    Code = [{decl,I, 0},
	    {decl,X, 2},
	    {'while', {'<', I, 5},
	     [{'=',X,{'+',X,1}},
	      {'=',I,{'+',I,1}}]}
	   ],
    expand(Code).

test6() ->
    I = {var,i},
    X = {var,x},
    Code = [{decl,I,13},
	    {decl,X,2},
	    {'for', [{'=',I,0}],{'<',I,5},[{'=',I,{'+',I,1}}],
	     [{decl,I,3},
	      {'while', {'<', I, 5},
	       [{'=',X,{'+',X,1}},
		{'=',I,{'+',I,1}}
	       ]}
	     ]}
	   ],
    expand(Code).
