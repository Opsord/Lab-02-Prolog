% Pixhex
% [PosX, PosY, Hex, Depth]
pixhex(PosX, PosY, Hex, Depth,[PosX, PosY, Hex, Depth]):-
    integer(PosX),
    integer(PosY),
    string(Hex),
    integer(Depth).
