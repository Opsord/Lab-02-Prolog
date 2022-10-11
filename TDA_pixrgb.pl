% Pixrgb
% [PosX, PosY, R, G, B, Depth]
pixrgb(PosX, PosY, R, G, B, Depth,[PosX, PosY, R, G, B, Depth]):-
    integer(PosX),
    integer(PosY),
    integer(R),
    R >= 0 ; R =< 255,
    integer(G),
    G >= 0 ; G =< 255,
    integer(B),
    B >= 0 ; B =< 255,
    integer(Depth).