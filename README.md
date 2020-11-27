# unroll - Code unrolling and generation

Initially very special code fragments are accepted

Declarations

    {defun,Name,[Decl],Code}

    {decl, {var,x}}
    {decl, {subscript,{var,x},[{var,i}]}}

Variables and subscripts

    {var,x}
    {subscript,{var,x},[5]}
    {subscript,{var,x},[{var,i}]}
    {subscript,{var,x},[{'+',{var,i},1}]}
    
Basic arithmetic operations, default to operate on unsigned word size elements

    {add0, [D], [X,Y]}             -- d = x+y
    {add,  [D1,D0], [X,Y]}         -- (d1,d0) = x+y
    {addc, [D1,D0], [X,Y,Ci]}      -- (d1,d0) = x+y+ci
    {sub0, [D], [X,Y]}             -- d = x-y
    {sub,  [Bo, D0], [X,Y]}        -- (bo,d) = x-y
    {subb, [Bo, D0], [X,Y,Bi]}     -- (bo,d) = x-(bi+y)
    {mul0, [P0], [X,Y]}            -- p0 = x*y
    {mul,  [P1,P0], [X,Y]}         -- (p1,p0) = x*y
    {mula, [P1,P0], [X,Y,A]}       -- (p1,p0) = x*y + a
    {mulab, [P1,P0], [X,Y,A,B]}    -- (p1,p0) = x*y + a + b
    {sqr,  [P1,P0], [X]}           -- (p1,p0) = x^2
    {sqra, [P1,P0], [X,A]}         -- (p1,p0) = x^2	+ a

The X,Y... are either coded as {var,x} or {subscript,{var,},[Index]} 

Structured operations

    {'for',I,Start,Stop,Step,Code}
    {'for',I,Start,Stop,Code}	
    {'if',Condition,Then,Else}
    {'if',Condition,Then}

index expression and conditions may include

    integer constants  ...-2,-1,0,1,2...
    boolean constants  true, false
    {'-',X}
    {'~',X}
    {'!',X}
    {'+',X,Y}
    {'-',X,Y}
    {'*',X,Y}
    {'/',X,Y}
    {'%',X,Y}
    {'&',X,Y}
    {'|',X,Y}
    {'^',X,Y}
    {'>>',X,Y}
    {'<<',X,Y}
    {'<',X,Y}
    {'<=',X,Y}
    {'>',X,Y}
    {'>=',X,Y}
    {'==',X,Y}
    {'!=',X,Y}
    {'&&',X,Y}
    {'||',X,Y}
    {'=',Lhs,Rhs}
    {'?:',Cond,Then,Else}
    {var,Name}
    {subscript,{var,Name},[Expr...]}

Environment

For loop variables in 'for' statement the variables
is put into an environment for each lap during the
expansion. The expand function is called with an environment
that may hold other "variables" (constants) and constant
arrays (maps).

Example

    > E = #{ {var,a} => 12,
             {var,x} => #{ [0] => 3, [1] => 5, [2] => 7, [3] => 11 } }.

This defines a=12 and x[] = {3,5,7,11}

Expand through calling

    > unroll:expand({var,a}, E).
    > 12

    > unroll:expand({subscript,{var,x},[2]},E).
    > 7

unroll init of array a[0]..a[9]

    > unroll:expand({'for',{var,i},0,9,1,
       [{'=',{subscript,{var,a},[{var,i}]},0}]}, #{}).

