% TDA de operaciones varias
%--------------------------------------------
%:- module(operaciones, [len/2, reverse/2]).
%--------------------------------------------
len(Lista,Resultado):-
    contar(Lista,Resultado).

contar([],0).
contar([_|Resto], N) :-
    contar(Resto, Acc),
    N is Acc + 1.
%--------------------------------------------
reverse(List1, List2) :-
        rev(List1, [], List2).

rev([], List, List).
rev([Head|List1], List2, List3) :-
        rev(List1, [Head|List2], List3).
%--------------------------------------------
