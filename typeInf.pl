/* 
    match functions by unifying with arguments 
    and infering the result.
    functor - ensures that the name of the function and the number of arguments matches

*/
typeExp(Fct, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */
    functor(Fct, Fname, _Nargs), 
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args],       /* Extract Fname and Args from Fct */
    append(Args, [T], FType),   /* make it loook like a function signature */
    functionType(Fname, TArgs), /* get type of arguments from definition */
    typeExpList(FType, TArgs).  /* recurisvely match types */

/* propagate types */
typeExp(T, T).

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
TODO: add statements types and their type checking
add statement for  for, if, etc
 */
/* global variable definition
    Example:
        gvLet(v, T, int) ~ let v = 3;
 */
typeStatement(gvLet(Name, T, Code), unit):-
    atom(Name), /* make sure we have a bound name */
    typeExp(Code, T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    asserta(gvar(Name, T)). /* add definition to database */

/*
    If statement
*/
typeStatement(if(Cond, TCode, FCode), T):-
    typeExp(Cond, bool),        /* Cond must be of type bool */
    typeCode(TCode, T),         /* Bind type of TCode to T */
    typeCode(FCode, T),         /* Bind type of FCode to T */
    bType(T).                   /* Verify if is one of the basic type */


/* 
Code is simply a list of statements. 
The type is the type of the last statement 
*/
typeCode([], _T).  /* type matches anything */
typeCode([S], T):-typeStatement(S, T).
typeCode([S, S2|Code], T):-
    typeStatement(S,_T),
    typeCode([S2|Code], T).

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    typeCode(Code, T).

/* 
    Basic types
    TODO: add more types if needed
 */
bType(int).
bType(float).
bType(string).
bType(bool).
bType(unit). /* unit type for things that are not expressions */
bType(T):- var(T).


/*  functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
bType([H]):- bType(H).
bType([H|T]):- bType(H), bType(T).

/*
    Type match
*/
is_same_type(X, Y):- X = Y.

/*
    TODO: as you encounter global variable definitions
    or global functions add their definitions to 
    the database using:
        asserta( gvar(Name, Type) )
    To check the types as you encounter them in the code
    use:
        gvar(Name, Type) with the Name bound to the name.
    Type will be bound to the global type
    Examples:
        g

    Call the predicate deleveGVars() to delete all global 
    variables. Best wy to do this is in your top predicate
*/

deleteGVars():-retractall(gvar), asserta(gvar(_X,_Y):-false()).

/*  builtin functions
    Each definition specifies the name and the 
    type as a function type

    TODO: add more functions
*/

fType(iplus, [int, int, int]).
fType(fplus, [float, float, float]).
fType(fToInt, [float, int]).
fType(iToFloat, [int, float]).
fType(print, [_X, unit]). /* simple print */
fType(identity, [T, T]).

/* relational expression */
fType(<,   [float, float, bool]).
fType(>,   [float, float, bool]).
fType(=,   [float, float, bool]).
fType(<=,  [float, float, bool]).
fType(>=,  [float, float, bool]).
fType('!=',[float, float, bool]).

/* Find function signature
   A function is either buld in using fType or
   added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-
    gvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(Name, Args), !. % make deterministic

% This gets wiped out but we have it here to make the linter happy
% gvar(_, _) :- false().

% run at compile time
:-dynamic(gvar/2).
