/* 
    match functions by unifying with arguments 
    and infering the result.
    functor - ensures that the name of the function and the number of arguments matches
*/
typeExp(Fct, T):-
    \+ var(Fct),                /* make sure Fct is not a variable */ 
    \+ atom(Fct),               /* an atom starts with a lowercase later, could be a string in single quotes */
    functor(Fct, Fname, _Nargs), 
    !,                          /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args],       /* Extract Fname and Args from Fct */
    append(Args, [T], FType),   /* Concatenate list Args, [T] into FType */
    functionType(Fname, TArgs), /* get type of arguments from definition */
    typeExpList(FType, TArgs).  /* recurisvely match types */

/* propogate types */
typeExp(T, T).

/* 
Code is simply a list of statements. 
The type is the type of the last statement 
*/
typeCode([], _T).  /* type matches anything */
typeCode([S], T):-  typeStatement(S, T).
typeCode([S, S1|Code], T):-
    typeStatement(S,_T),
    typeCode([S1|Code], T).

/* 
    Check if all statements have type unit
*/
typeCodeUnit([], unit).
typeCodeUnit([S]):- typeStatement(S, unit).
typeCodeUnit([S, S1|Code]):-
    typeStatement(S, unit),
    typeCodeUnit([S1|Code]).


/* list version to allow function mathine */
typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]):-
    typeExp(Hin, Hout), /* type infer the head */
    typeExpList(Tin, Tout). /* recurse */

/* 
    Statement that are just an Expression has type T.
    Make sure they are from basic type.
*/
typeStatement(Expr, T):- typeExp(Expr, T), 
                         bType(T).

/* 
    global variable definition
    Example:
        gvLet(v, T, int) -> let v = 3;
 */
typeStatement(gvLet(Name, T, Code), unit):-
    atom(Name),              /* make sure we have a bound name */
    typeExp(Code, T),        /* infer the type of Code and ensure it is T */
    bType(T),                /* make sure we have an infered type */
    asserta(gvar(Name, T)).  /* add definition to database */

/*
    Global functions
    Example: gfLet(foo, [int], [int]) -> foo int = int
*/
typeStatement(gfLet(Fname, Args, Code), T):-
    atom(Fname),
    is_list(Args),
    typeCode(Code, T),           /* infer code type */
    bType(T),                    /* must be from one of the basic type */
    append(Args, [T], FType),    /* type for a function ...args, return type */
    asserta(gvar(Fname, FType)).

/*
    If and else statement
*/
typeStatement(if(Cond, TCode, FCode), T):-
    typeExp(Cond, bool),        /* Cond must be of type bool */
    typeCode(TCode, T),         /* Bind type of TCode to T */
    typeCode(FCode, T),         /* Bind type of FCode to T */
    bType(T).                   /* Verify if is one of the basic type */

/*
    If statement
*/
typeStatement(if(Cond, TCode), T):-
    typeExp(Cond, bool),        /* Cond must be of type bool */
    typeCode(TCode, T),         /* Bind type of TCode to T */
    bType(T).                   /* Verify if is one of the basic type */

/*
    For statement
    for i = int to int do
        unit type statements
    done

    The start and end value must be an int.
    The expression inside the for loop must return unit.
    The for loop as a whole returns unit.
*/
typeStatement(for(Start, End, Code), unit):-
    typeExp(Start, int),        /* Start must be of type int */
    typeExp(End,   int),        /* End must be of type int */
    typeCodeUnit(Code).         /* all statements must be of type unit */

/* 
    Basic types
 */
bType(int).
bType(float).
bType(string).
bType(bool).
bType(unit). /* unit type for things that are not expressions */
bType(T):- var(T).

/*  
    functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
bType([H]):- bType(H).
bType([H|T]):- bType(H), bType(T).

/*
    Call the predicate deleveGVars() to delete all global 
    variables. Best wy to do this is in your top predicate
*/

deleteGVars():-
    retractall(gvar), 
    asserta(gvar(_X,_Y):-false()).

/*  
    builtin functions
    Each definition specifies the name and the type as a function type
*/

fType(iop,  [int, int, int]).
fType(fop,  [float, float, float]).
fType(ifop, [int, float, float]).
fType(fiop, [float, int, float]).
fType(fToInt, [float, int]).
fType(iToFloat, [int, float]).
fType(toStr, [_X, string]).
fType(print, [_X, unit]). /* simple print */
fType(identity, [T, T]).

/* relational operator */
fType(<,   [float, float, bool]).
fType(>,   [float, float, bool]).
fType(=,   [float, float, bool]).
fType(<=,  [float, float, bool]).
fType(>=,  [float, float, bool]).
fType('!=',[float, float, bool]).

/* logical operators */
fType('&&',  [bool, bool, bool]).
fType('||',  [bool, bool, bool]).
fType('!',   [bool, bool]).

/* Find function signature
   A function is either buld in using fType or
   added as a user definition with gvar(fct, List)
*/

/*
   Unifying global user defined functions.
   gvar(Name, [...args, return type])
*/
functionType(Name, Args):-
    gvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(Name, Args), !. % make deterministic

% This gets wiped out but we have it here to make the linter happy
% gvar(_, _) :- false().

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    typeCode(Code, T).

% run at compile time
:-dynamic(gvar/2).
:-dynamic(gfunc/3).
