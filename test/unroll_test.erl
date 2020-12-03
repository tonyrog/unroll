%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Test cases for unroll
%%% @end
%%% Created :  2 Dec 2020 by Tony Rogvall <tony@rogvall.se>

-module(unroll_test).

-compile(export_all).

-include_lib("bic/include/bic.hrl").

%%
%% Simple tests
%%
-define(EXPR(Op,A1,A2), #bic_binary{line=0,op=(Op),arg1=(A1),arg2=(A2)}).
-define(EXPR(Op,A), #bic_unary{line=0,op=(Op),arg=(A)}).
-define(INDEX(A,I), ?EXPR('[]',A,I)).
-define(INDEX(A,I,J), ?EXPR('[]',A,?EXPR(',',I,J))).
-define(UNSIGNED, #bic_type{line=0,sign=unsigned}).
-define(VOID,     #bic_type{line=0,type=void}).
-define(INT,      #bic_type{line=0,type=int}).
-define(IFEXPR(C,T,E),#bic_ifexpr{line=0,test=(C),then=(T),else=(E)}).
-define(ASSIGN(Op,L,R), #bic_assign{line=0,op=(Op),lhs=(L),rhs=(R)}).
-define(ID(Name), #bic_id{line=0,name=(Name)}).
-define(DECL(N), #bic_decl{line=0,name=(N),type=?UNSIGNED}).
-define(DECL(N,V), #bic_decl{line=0,name=(N),type=?UNSIGNED,value=(V)}).

-define(DECLA(N,D), #bic_decl{line=0,
			      name=(N),
			      type=#bic_array{type=?UNSIGNED,dim=D}}).
-define(FOR(I,T,U,B), #bic_for{line=0,init=(I),test=(T),update=(U),body=(B)}).
-define(WHILE(T,B), #bic_while{line=0,test=(T),body=(B)}).
-define(CALL(Name,Args),#bic_call{line=0,func=?ID(Name),args=(Args)}).
-define(RETURN(X),#bic_return{line=0,expr=(X)}).
-define(FUNCTION(Type,Name,Params,Body),
	#bic_function{line=0,type=Type,
		      name=(Name),params=(Params),body=(Body)}).
						 
code1() ->
    X = ?ID("x"),
    Y = ?ID("y"),
    I = ?ID("i"),
    ?FOR(?ASSIGN('=',I,0),
	 ?EXPR('<',I,10),
	 ?ASSIGN('=',I,?EXPR('+',I,1)),
	 [
	  ?CALL("add",[?INDEX(Y,?EXPR('+',I,1)),?INDEX(X,I)])
	 ]).

format1() ->
    io:put_chars(bic:format_statement(code1())).

test1() ->
    X = unroll:expand(code1()),
    io:put_chars(bic:format_statement(X)).

code2() ->
    X = ?ID("x"),
    I = ?ID("i"),
    ?FOR(?ASSIGN('=',I,0),
	 ?EXPR('<',I,10),
	 ?ASSIGN('=',I,?EXPR('+',I,1)),
	 [
	  ?ASSIGN('=',?INDEX(X,I), 0)
	 ]).

format2() ->
    io:put_chars(bic:format_statement(code2())).

test2() ->
    X = unroll:expand(code2()),
    io:put_chars(bic:format_statement(X)).

code21() ->
    X = ?ID("x"),
    I = ?ID("i"),
    ?FOR(?ASSIGN('=',I,0),
	 ?EXPR('<',I,10),
	 ?ASSIGN('=',I,?EXPR('+',I,1)),
	 [
	  ?ASSIGN('=',?INDEX(X,I), I)
	 ]).

format21() ->
    io:put_chars(bic:format_statement(code21())).
    

unrollt21() ->
    X = unroll:expand(code21()),
    io:put_chars(bic:format_statement(X)).

test3() ->
    test3(4, 4).

test4() ->
    test3(16, 16).

code3(Xl, Yl) ->
    I = ?ID("i"),
    J = ?ID("j"),
    IJ = ?ID("ij"),
    X = ?ID("x"),
    Y = ?ID("y"),
    R = ?ID("r"),
    Cp = ?ID("cp"),
    C  = ?ID("c"),
    P  = ?ID("p"),
    Name = "mul_"++ integer_to_list(Xl)++"x"++integer_to_list(Yl),
    ?FUNCTION(?INT,
	      Name,
	      [?DECLA("x",Xl),
	       ?DECLA("y",Yl),
	       ?DECLA("r",Xl+Yl)],
	      [
	       ?DECL("cp"),
	       ?DECL("c"),
	       ?DECL("ij"),
	       ?DECL("p"),
	       ?FOR(?ASSIGN('=',I,0),
		    ?EXPR('<',I,Xl),
		    ?ASSIGN('=',I,?EXPR('+',I,1)),
		    [
		     ?ASSIGN('=',Cp,0),
		     ?ASSIGN('=',C,0),
		     ?ASSIGN('=', IJ, I),
		     ?FOR(?ASSIGN('=',J,0),
			  ?EXPR('<',J,Yl),
			  ?ASSIGN('=',J,?EXPR('+',J,1)),
			  [
			   ?CALL("mula",
				 [?INDEX(X,I),?INDEX(Y,J),Cp,
				  ?EXPR('&',Cp), ?EXPR('&',P)]),
			   ?CALL("addc",
				 [P,?INDEX(R,IJ),C,
				  ?EXPR('&',C),?EXPR('&',?INDEX(R,IJ))]),
			   ?ASSIGN('=',IJ,?EXPR('+',IJ,1))
			  ]),
		     ?CALL("add0",[C,Cp,?EXPR('&',?INDEX(R,IJ))])
		    ]),
	       ?RETURN(?IFEXPR(?EXPR('==',?INDEX(R,Xl+Yl-1),0),
			       Xl+Yl-1,Xl+Yl))
	      ]).

format3(Xl, Yl) ->
    io:put_chars(bic:format_definitions([code3(Xl,Yl)])).

test3(Xl, Yl) ->
    Code = code3(Xl,Yl),
    XCode = unroll:expand(Code),
    FCode = bic:format_definitions([XCode]),
    Name = "mul_"++ integer_to_list(Xl)++"x"++integer_to_list(Yl),
    io:put_chars(FCode),
    file:write_file("unroll_"++Name++".c",
		    ["#include \"unroll_auto_config.h\"","\n",
		     "#include \"unroll.i\"","\n",
		     "\n",
		     FCode]).

code5() ->
    I = ?ID("i"),
    X = ?ID("x"),
    [?DECL("i",0),
     ?DECL("x",2),
     #bic_while{test=?EXPR('<',I,5),
		body=[?ASSIGN('?',X,?EXPR('+',X,1)),
		       ?ASSIGN('?',I,?EXPR('+',I,1))
		     ]}
    ].

format5() ->
    io:put_chars(bic:format_statement(code5())).

unroll5() ->
    io:put_chars(bic:format_statement(unroll:expand(code5()))).


code6() ->
    I = ?ID("i"),
    X = ?ID("x"),
    [?DECL("i", 13),
     ?DECL("x", 2),
     #bic_for{init=?ASSIGN('=',I,0),
	      test=?EXPR('<',I,5),
	      update=?ASSIGN('=',I,?EXPR('+',I,1)),
	      body=
		  [?DECL("i",3),
		   #bic_while{line=0,
			      test=?EXPR('<', I, 5),
			      body=
				  [
				   ?ASSIGN('=',X,?EXPR('+',X,1)),
				   ?ASSIGN('=',I,?EXPR('+',I,1))
				  ]
			     }
		  ]
	      }
     ].

format6() ->
    io:put_chars(bic:format_statement(code6())).

unroll6() ->
    io:put_chars(bic:format_statement(unroll:expand(code6()))).
