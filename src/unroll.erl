%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Code expansion/unrolling
%%% @end
%%% Created : 25 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(unroll).

-export([file/1]).
-export([expand/1, expand/2]).
-export([new_env/0, push_env/1, pop_env/1]).

-compile(export_all). %% test

-include_lib("bic/include/bic.hrl").

-define(dbg(F,A), ok).

%% fixme add a cleanup pass
%% remove variable assignments not used, keep the last one, if used
%% remove declarations not used (all references are expanded)
%%
%% Environment:
-type value() :: undefined | integer() | float().
-type scope() :: #{ VarName::string() => bic_type() }.
-type store() :: #{ VarName::string() => value() }.

-type env() :: #{ 
		  store => store(),   %%  value store (previous scopes)
		  scope => scope(),   %% current scope 
		  stack => [scope()], %% stack of scopes
		  VarName::string() => value  %% current values
		}.

file(File) ->
    case bic:file(File) of
	{ok,Forms} ->
	    Forms1 = expand(Forms),
	    io:put_chars(bic:format_definitions(Forms1)),
	    {ok,Forms1};
	Error ->
	    Error
    end.

%% expand/unroll code
expand(Stmt) ->
    expand(Stmt, new_env()).

expand(Stmt, Env) ->
    {Stmt1, _Env} = statement(Stmt, Env),
    Stmt1.

%% for (init, cond, update)
statement(S=#bic_for{},E) ->
    for(S,E);
statement(S=#bic_while{},E) ->
    while(S,E);
statement(S=#bic_do{},E) ->
    do(S,E);
statement(S=#bic_if {test=Cond,then=Then,else=undefined}, E) ->
    {Value1,E1} = expr(Cond, E),
    if is_integer(Value1), Value1 =/= 0; Value1 =:= true ->
	    statement(Then,E1);
       is_integer(Value1), Value1 =:= 0; Value1 =:= false ->
	    {#bic_empty{},E1};
       true ->
	    {Then1,E2} = statement(Then,E1),
	    {S#bic_if{test=Value1,then=Then1},E2}
    end;
statement(S=#bic_if {test=Cond,then=Then,else=Else}, E) ->
    {Value1,E1} = expr(Cond, E),
    if is_integer(Value1), Value1 =/= 0; Value1 =:= true ->
	    statement(Then,E1);
       is_integer(Value1), Value1 =:= 0; Value1 =:= false ->
	    statement(Else,E1);
       true ->
	    {Then1,E2} = statement(Then,E1),
	    {Else1,E2} = statement(Else,E1), %% match E2!
	    {S#bic_if{test=Value1,then=Then1,else=Else1},E2}
    end;
statement(S=#bic_return{expr=Expr}, E) ->
    {Expr1, E1} = expr(Expr, E),
    {S#bic_return{expr=Expr1}, E1};

statement(S=#bic_typedef{}, E) ->
    %% FIXME: introduce type ! in env.
    {S, E};
statement(S=#bic_decl{name=Var,type=Type,value=Expr}, E) ->
    E1 = decl(Var,Type,E),
    {Expr1, E2} = expr(Expr, E1),
    {S#bic_decl{value=Expr1}, E2#{ Var => Expr1 }};
statement(S=#bic_function{name=Fun, params=Params, body=Code}, E) ->
    E0 = push_env(E),
    %% declare all parameters
    E1 = lists:foldl(fun(#bic_decl{name=Var,type=Type}, Ei) ->
			     decl(Var,Type,Ei)
		     end, E0, Params),
    %% partial eval body 
    {Code1, E2} = statement_list_(Code, E1),
    E3 = pop_env(E2),
    S1 = S#bic_function{params=Params,body=Code1},
    {S1, E3#{ Fun => S1}};
statement(Const=#bic_constant{}, E) ->
    {Const,E};
statement(A=#bic_assign{}, E) ->
    expr(A,E);
statement(A=#bic_call{}, E) ->
    expr(A,E);
statement(A=#bic_binary{}, E) ->
    expr(A,E);
statement(A=#bic_unary{}, E) ->
    expr(A,E);
statement(A=#bic_compound{code=Stmts},E) ->
    case compound(Stmts,E) of
	{[],E1} ->
	    {#bic_empty{}, E1};
	{Stmts1,E1} ->
	    {A#bic_compound{code=Stmts1},E1}
    end;
statement(A, E) when is_list(A) ->
    case compound(A, E) of
	{[], E1} ->
	    {#bic_empty{}, E1};
	{A1,E1} -> 
	    {A1,E1}
    end.

compound(Code, E) when is_list(Code) ->
    E0 = push_env(E),
    {Code1, E1} = statement_list(Code, E0),
    {Code1, pop_env(E1)}.

statement_list(List, E) ->
    case statement_list_(List,E) of
	{[], E1} -> {#bic_empty{}, E1};
	{[H],E1} -> {H, E1};
	{List1,E1} -> {List1,E1}
    end.

statement_list_([H|T], E) ->
    case statement(H,E) of
	{#bic_empty{}, E1} ->
	    statement_list_(T,E1);
	{H1,E1} ->
	    {T1,E2} = statement_list_(T,E1),
	    {[H1|T1],E2}
    end;
statement_list_([], E) ->
    {[],E}.

for(For=#bic_for{init=Init, test=Cond, update=Update, body=Code}, E) ->
    {Init1,E1} = statement(Init, E),
    for_(For,Cond,Update,Code,E1,[Init1]).

for_(For,Cond,Update,Code,E,Acc) ->
    {Cond1,E1} = expr(Cond, E),
    if is_integer(Cond1), Cond1 =/= 0 ->
	    {Code1,E2} = statement(Code,E1),
	    {Update1,E3} = statement(Update,E2),
	    for_(For,Cond,Update,Code,E3,[Update1,Code1|Acc]);
       is_integer(Cond1), Cond1 =:= 0 ->
	    {lists:reverse(Acc), E1};
       length(Acc) =:= 1 -> %% only init ignore unroll
	    {_,E2} = unset_values(Code,E1),
	    {For, E2};
       true -> %% loop a bit
	    {_, E2} = unset_values(Code,E),
	    {rcat(#bic_while{test=Cond,body=cat(Code,Update)},Acc), E2}
    end.

while(While=#bic_while{test=Cond, body=Code}, E) ->
    while_(While, Cond, Code, E, []).

while_(While, Cond, Code, E, Acc) ->
    {Cond1,E1} = expr(Cond, E),
    if is_integer(Cond1), Cond1 =/= 0 ->
	    {Code1,E2} = statement(Code,E1),
	    while_(While, Cond, Code, E2, [Code1|Acc]);
       is_integer(Cond1), Cond1 =:= 0 ->
	    {lists:reverse(Acc), E1};
       Acc =:= [] ->
	    {_,E2} = unset_values(Code,E1),
	    {While, E2};
       true ->
	    {_, E2} = unset_values(Code,E),
	    {rcat(While#bic_while{test=Cond,body=Code},Acc), E2}
    end.

do(Do=#bic_do{body=Code, test=Cond}, E) ->
    do_(Do,Code, Cond, E, []).

do_(Do, Code, Cond, E, Acc) ->
    {Code1,E1} = statement(Code,E),
    {Cond1,E2} = expr(Cond, E1),
    if is_integer(Cond1), Cond1 =/= 0 ->
	    do_(Do,Code, Cond, E2, [Code1|Acc]);
       is_integer(Cond1), Cond1 =:= 0 ->
	    {lists:reverse(Acc), E1};
       Acc =:= [] ->
	    {_,E3} = unset_values(Code,E),
	    {Do, E3};
       true ->
	    {_, E3} = unset_values(Code,E),
	    {rcat(Do#bic_do{body=Code,test=Cond},Acc), E3}
    end.

cat(Code1,Code2) ->
    if Code1 =:= [] -> Code2;
       Code2 =:= [] -> Code1;
       is_list(Code1) ->
	    if is_list(Code2) ->  Code1++Code2;
	       true -> Code1 ++ [Code2]
	    end;
       true ->
	    if is_list(Code2) ->  [Code1|Code2];
	       true -> [Code1,Code2]
	    end
    end.

rcat(Code1,Code2) ->
    if Code1 =:= [] ->
	    if Code2 =:= [] -> [];
	       is_list(Code2) ->  lists:reverse(Code2);
	       true -> Code2
	    end;
       is_list(Code1) ->
	    if Code2 =:= [] -> lists:reverse(Code1);
	       is_list(Code2) ->  lists:reverse(Code1++Code2);
	       true -> lists:reverse(Code1++[Code2])
	    end;
       true ->
	    if Code2 =:= [] -> Code1;
	       is_list(Code2) ->  lists:reverse([Code1|Code2]);
	       true -> [Code2,Code1]
	    end
    end.

%% scope keeps current declarations
%% store keeps previous values
%% stack keeps store stack for reference (not used right now)

-spec new_env() -> env().

new_env() ->
    #{ scope => #{}, store => #{}, stack => [] }.

push_env(E=#{ scope := Scope0, store := Store0, stack := Stack }) ->
    E#{ scope => #{},
	store => #{},
	stack => [Scope0,Store0|Stack]}.

pop_env(E=#{ scope := Scope, store := Store, stack := [Scope0,Store0|Stack]}) ->
    %% update then "hidden" globals with the new vales
    E1 = maps:fold(fun(Var,_Type,Ei) ->
			   case maps:get(Var,Scope0,undefined) of
			       undefined ->
				   maps:remove(Var, Ei);
			       _Type ->
				   Value0 = maps:get(Var,Store,undefined),
				   Ei#{ Var => Value0 }
			   end
		   end, E, Scope),
    E1#{ scope => Scope0, store => Store0, stack => Stack }.


decl(Var, Type, E = #{ scope := Scope, store := Store }) ->
    Value = maps:get(Var, E, undefined),   %% previous value, if any
    E#{ store => Store#{ Var => Value },   %% save previous value in store
	scope => Scope#{ Var => Type },
	Var => undefined }.
    

-spec expr(bic_expr(), env()) ->
	  {bic_expr()|value(), env()}.

%% meta expressions
expr(undefined, E) -> {undefined,E};
expr(Const,E) when is_number(Const) -> {Const,E};
expr(#bic_constant{base=10,value=V},E) -> 
    {list_to_integer(V, 10),E};
expr(#bic_constant{base=16,value=[$0,$x|V]},E) ->
    {list_to_integer(V, 16),E};
expr(#bic_constant{base=16,value=[$0,$X|V]},E) ->
    {list_to_integer(V, 16),E};
expr(#bic_constant{base=2,value=[$0,$b|V]},E) ->
    {list_to_integer(V, 2),E};
expr(#bic_constant{base=2,value=[$0,$B|V]},E) ->
    {list_to_integer(V, 2),E};
expr(#bic_constant{base=8,value=[$0|V]},E) -> 
    {list_to_integer(V, 8),E};
expr(#bic_constant{base=float,value=V=[$.|_]},E) -> 
    {list_to_float([$0|V]),E};
expr(#bic_constant{base=float,value=V},E) -> 
    {list_to_float(V),E};
expr(X=#bic_id{name=Vx}, E) ->
    Value = value(Vx, E, X),
    {Value, E};
expr(X=#bic_call{func=Func,args=Args}, E) ->
    {Args1, E1} = expr_list(Args, E),
    %% FIXME: check if call is to be expanded / unrolled
    {X#bic_call{func=Func, args=Args1}, E1};
expr(X=#bic_binary{op=',',arg1=A,arg2=B},E) ->
    {A1,E1} = expr(A,E),
    {B1,E2} = expr(B,E1),
    if is_number(A1),is_number(B1) ->
	    {[A1,B1], E2};
       is_number(A1),is_list(B1) ->
	    {[A1|B1], E2};
       true ->
	    {X#bic_binary{arg1=A1,arg2=B1}, E2}
    end;
expr(X=#bic_binary{op='[]',arg1=Var,arg2=Index},E) ->
    {Index1,E1} = expr(Index,E),
    case Var of
	#bic_id{name=Vx} ->
	    X1 = X#bic_binary{arg2=Index1},
	    Value = value({Vx,Index1}, E, X1),
	    {Value,E1};
	Map when is_map(Map) -> %% FIXME: eval Var to map..
	    {maps:get(Index1, Map),E1};
	_V1 ->
	    {X#bic_binary{arg2=Index1},E1}
    end;
expr(X=#bic_unary{op='&',arg=A}, E) ->
    case A of
	#bic_binary{op='[]',arg2=Index} ->
	    {Index1,E1} = expr(Index,E),
	    {X#bic_unary{arg=A#bic_binary{arg2=Index1}}, E1};
	_ -> %% fix more lhs expressions
	    {X, E}
    end;

%% fixme: must be transformed
expr(X=#bic_unary{op='+++',arg=A}, E) ->
    {A1,Ref,E1} = lhs_ref(A, E),
    {X#bic_unary{arg=A1}, add_value(Ref, E1, 1)};
expr(X=#bic_unary{op='---',arg=A}, E) ->
    {A1,Ref,E1} = lhs_ref(A, E),
    {X#bic_unary{arg=A1}, add_value(Ref, E1, -1)};
%% fixme: must be transformed
expr(X=#bic_unary{op='++',arg=A}, E) ->
    {A1,Ref,E1} = lhs_ref(A, E),
    {X#bic_unary{arg=A1}, add_value(Ref, E1, 1)};
expr(X=#bic_unary{op='--',arg=A}, E) ->
    {A1,Ref,E1} = lhs_ref(A, E),
    {X#bic_unary{arg=A1}, add_value(Ref, E1, -1)};

expr(X=#bic_unary{op=Op,arg=A}, E) ->
    {A1,E1} = expr(A,E),
    case is_number(A1) of
	true ->
	    Value = case Op of
			'+' -> +A1;
			'-' -> -A1;
			'~' -> bnot A1;
			'!' -> bool(A1 =/= 0)
		    end,
	    {Value, E1};
	false ->
	    {X#bic_unary{arg=A1}, E1}
    end;

expr(X=#bic_assign{op='=',lhs=Lhs,rhs=V},E) -> 
    {V1,E1} = expr(V, E),
    {Lhs1,Ref,E2} = lhs_ref(Lhs, E1),
    case is_number(V1) of
	true ->
	    {X#bic_assign{lhs=Lhs1,rhs=V1}, set_value(Ref, E2, V1)};
	false ->
	    {X#bic_assign{lhs=Lhs1,rhs=V1}, E2}
    end;
expr(X=#bic_assign{op='+=',lhs=Lhs,rhs=V},E) -> 
    {V1,E1} = expr(V, E),
    {Lhs1,Ref,E2} = lhs_ref(Lhs, E1),
    case is_number(V1) of
	true ->
	    {X#bic_assign{lhs=Lhs1,rhs=V1}, add_value(Ref, E2, V1)};
	false ->
	    {X#bic_assign{lhs=Lhs1,rhs=V1}, E2}
    end;

expr(X=#bic_binary{op=Op,arg1=A,arg2=B}, E) ->
    {A1,E1} = expr(A,E),
    {B1,E2} = expr(B,E1),
    case is_number(A1) andalso is_number(B1) of
	true ->
	    Value = case Op of
			'+' -> A1 + B1;
			'-' -> A1 - B1;
			'*' -> A1 * B1;
			'/' -> A1 div B1;
			'%' -> A1 rem B1;
			'&' -> A1 band B1;
			'|' -> A1 bor B1;
			'^' -> A1 bxor B1;
			'<<' -> A1 bsl B1;
			'>>' -> A1 bsr B1;
			'<' -> bool(A1 < B1);
			'<=' -> bool(A1 =< B1);
			'>' -> bool(A1 > B1);
			'>=' -> bool(A1 >= B1);
			'==' -> bool(A1 =:= B1);
			'!=' -> bool(A1 =/= B1);
			'&&' -> bool(A1=/=0 andalso B1=/=0);
			'||' -> bool(A1=/=0 orelse B1=/=0)
		    end,
	    {Value, E2};
	false -> 
	    {X#bic_binary{arg1=A1,arg2=B1}, E2}
    end;
expr(X=#bic_ifexpr{test=C,then=A,else=B},E) ->
    case expr(C,E) of
	{1,E1} -> expr(A,E1);
	{0,E1} -> expr(B,E1);
	{C1,E1} ->
	    {A1,E3} = expr(A,E1),
	    {B1,E3} = expr(B,E1),  %% match E3!!
	    {X#bic_ifexpr{test=C1,then=A1,else=B1},E3}
    end.

expr_list(As, E) ->
    expr_list_(As, E, []).
    
expr_list_([A|As], E, Acc) ->
    {A1,E1} = expr(A, E),
    expr_list_(As, E1, [A1|Acc]);
expr_list_([], E, Acc) ->
    {lists:reverse(Acc), E}.

%% currently supported LHS simple var and simple array
lhs_ref(Lhs=#bic_id{name=V}, E) ->
    {Lhs,V,E};
lhs_ref(Lhs=#bic_binary{op='[]',arg1=#bic_id{name=V},arg2=Index}, E) ->
    {Index1,E1} = expr(Index, E),
    {Lhs#bic_binary{arg2=Index},{V,Index1},E1}.

bool(true) -> 1;
bool(false) -> 0.

%% if loop unroll fail then variables in body etc need
%% to be unmarked (set to undefined) since the value are 
%% unknown.
%% FIXME: keep track on begin/end ?
unset_values(Code, E) ->
    bic_transform:fold(
      fun(F=#bic_id{name=Var}, Ei) ->
	      {F,Ei#{ Var => undefined }};
	 (F, Ei) ->
	      {F, Ei}
      end, E, Code).

-spec value(Var::string() | {string(),[integer()]}, env(), value()) ->
	  value().

value(Var, E, Default) when 
      is_list(Var); is_list(element(1,Var)) ->
    case maps:get(Var,E,undefined) of
	undefined ->
	    ?dbg("value: ~p default ~p\n", [Var, Default]),
	    Default;
	Value -> 
	    ?dbg("value: ~p is ~p\n", [Var, Value]),
	    Value
    end.

-spec set_value(Var::string() | {string(),[integer()]}, env(), value()) ->
	  value().

set_value(Var, E, Value) when 
      is_list(Var); is_list(element(1,Var)) ->
    ?dbg("set_value: ~p = ~p\n", [Var, Value]),
    E#{ Var => Value }.

-spec add_value(Var::string() | {string(),[integer()]}, env(), value()) ->
	  value().

add_value(Var, E, Value) when 
      is_list(Var); is_list(element(1,Var)) ->
    ?dbg("add_value: ~p = ~p\n", [Var, Value]),
    case maps:find(Var, E) of
	{ok,undefined} ->
	    E;
	{ok,PrevValue} ->
	    E#{ Var => PrevValue + Value };
	error ->
	    E#{ Var => undefined }
    end.
