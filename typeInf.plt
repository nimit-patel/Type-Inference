:- begin_tests(typeInf).
:- include(typeInf). 

% tests for typeExp
test(typeExp_iop):- 
    typeExp(iop(int,int), int).

test(typeExp_fop, [true(T == float)]):- 
    typeExp(fop(X,float), T),
    assertion(X == float).

test(typeExp_ifop, [true(T == float)]):- 
    typeExp(ifop(int,float), T).

test(typeExp_toStr, [true(T == string)]):- 
    typeExp(toStr(int), T).

test(typeExp_less_than, [true(T == bool)]):-
    typeExp(X < float, T),
    assertion(X == float).

test(typeExp_not, [true(T == bool)]):-
    typeExp(!(X), T),
    assertion(X == bool).

test(typeExp_iop_F, [fail]) :-
    typeExp(iop(int, int), float).

test(typeExp_iop_T, [true(T == int)]) :-
    typeExp(iop(int, int), T).

% test gvLet
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, iop(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int). % make sure the global variable is defined

test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, iop(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

% test global functions
test(typeStatement_gfLet_T1, [nondet]):-
    deleteGVars(),
    typeStatement(gfLet(foo, [int, float], [iop(X, Y)]), T),
    assertion(X == int),
    assertion(Y == int),
    assertion(T == int),
    gvar(foo, [int, float, int]).

test(typeStatement_gfLet_T2, [nondet]):-
    deleteGVars(),
    typeStatement(gfLet(foo, [int], [iop(int, int), fop(X,Y)]), T),
    assertion(X == float),
    assertion(Y == float),
    assertion(T == float),
    gvar(foo, [int, float]).

test(typeStatement_gfLet_F1, [fail]):-
    deleteGVars(),
    typeStatement(gfLet(foo, [int, int], [unit]), int).

test(typeStatement_gfLet_F2, [fail]):-
    deleteGVars(),
    typeStatement(gfLet(foo, [int], [unit, float]), unit).

% test if and else statements
test(test_infer_bool_1, [true(T == bool)]):-
    deleteGVars(),
    typeExp(float < float, T).

test(test_infer_bool_2, [true(T == bool)]):-
    deleteGVars(),
    typeExp('!='(float,float), T).

test(test_infer_bool_3, [true(T == bool)]):-
    deleteGVars(),
    typeExp('&&'(X, Y), T),
    assertion(X == bool),
    assertion(Y == bool).

test(infer_if_else_T_1, [nondet],  true(T == float)):-
    deleteGVars(),
    infer([if(float < Y, [float], [ELSE])], T),
    assertion(Y == float),
    assertion(ELSE == float).

test(infer_if_else_T_2, [nondet, true(T == unit)]):-
    deleteGVars(),
    infer([if(float < X, [unit], [iop(int, int), print(string)])], T),
    assertion(X == float).

test(infer_if_else_F, [fail]):-
    deleteGVars(),
    infer([if(float < float, [float], [int])], float).

test(infer_if_T, [nondet, true(T == float)]):-
    deleteGVars(),
    infer([if(fop(X,Y) < Z, [float])], T),
    assertion(X == float),
    assertion(Y == float),
    assertion(Z == float).

test(infer_if_F, [fail]):-
    deleteGVars(),
    infer([if(bool, [int])], unit).

% test for
test(typeStatement_for_T1, [nondet, true(T == unit)]) :- 
    deleteGVars(), 
    typeStatement(for(X, int, [Y, print(string)]), T),
    assertion(X == int), 
    assertion(Y == unit).

test(typeStatement_for_T2, [nondet, true(T == unit)]) :- 
    deleteGVars(), 
    typeStatement(for(X, int, [print(string), print(int), Y]), T),
    assertion(X == int), 
    assertion(Y == unit).

test(typeStatement_for_F, [fail]) :-
    deleteGVars(), 
    typeStatement(for(int, int, [int]), unit).

% test typeCodeUnit
test(test_infer_unit_all_T, [nondet, true(T == unit)]):-
    typeCodeUnit([unit, unit, T]).

test(test_infer_unit_all_F, [fail]):-
    typeCodeUnit([unit, int, unit]).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct

:-end_tests(typeInf).
