




%-----------------------------------------------------------------
% Pixbit
% [PosX, PosY, Bit, Depth]
pixbitd(PosX, PosY, Bit, Depth,[PosX, PosY, Bit, Depth]):-
    integer(PosX),
    integer(PosY),
    integer(Bit),
    Bit == 0 ; Bit == 1,
    integer(Depth).
% ----------------------------------------------------------------
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
% ----------------------------------------------------------------
% Pixhex
% [PosX, PosY, Hex, Depth]
pixhex(PosX, PosY, Hex, Depth,[PosX, PosY, Hex, Depth]):-
    integer(PosX),
    integer(PosY),
    string(Hex),
    integer(Depth).
% ----------------------------------------------------------------

% Constructor de imagen
image(Largo, Ancho, Pixeles, [Largo, Ancho, Pixeles]).

% Verificador Bitmap
imageIsBitmap([]).
imageIsBitmap([Pixbitd | Cdr]) :-
    pixbitd(_, _, ContBit, _, Pixbitd),
    ContBit = 0 ; ContBit = 1,
    imageIsBitmap(Cdr).

% Verificador Pixmap
imageIsPixmap([]).
imageIsPixmap([Pixrgbd | Cdr]) :-
    pixrgbd(_, _, R, G, B, _, Pixrgbd),
    integer(R),
    R >= 0 ; R =< 255,
    integer(G),
    G >= 0 ; G =< 255,
    integer(B),
    B >= 0 ; B =< 255,
    imageIsPixmap(Cdr).

% Verificador Hexmap
imageIsHexmap([]).
imageIsHexmap([Pixhexd | Cdr]) :-
    pixbitd(_, _, ContHex, _, Pixhexd),
    string(ContHex),
    imageIsHexmap(Cdr).