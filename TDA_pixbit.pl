%---------------------------------------------------------------
% Creacion de modulo pixbit
:- module(pixbit/1, [pixbit/1]).
%---------------------------------------------------------------
% Pixbit
% [PosX, PosY, Bit, Depth]
pixbitd(PosX, PosY, Bit, Depth,[PosX, PosY, Bit, Depth]):-
    integer(PosX),
    integer(PosY),
    integer(Bit),
    Bit == 0 ; Bit == 1,
    integer(Depth).
% --------------------------------------------------------------