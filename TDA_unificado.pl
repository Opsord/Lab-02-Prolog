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
pixrgbd(PosX, PosY, R, G, B, Depth,[PosX, PosY, R, G, B, Depth]):-
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
% ----------------------------------------------------------------
% Verificador Bitmap
imageIsBitmap(Image):-
    image(_,_,Pixeles,Image),
    pixelsAreBitmap(Pixeles).

pixelsAreBitmap([]).
pixelsAreBitmap([Pixbitd | Cdr]) :-
    pixbitd(_, _, Bit, _, Pixbitd),
    (Bit == 0 ; Bit == 1),
    pixelsAreBitmap(Cdr).
% ----------------------------------------------------------------
% Verificador Pixmap
imageIsPixmap(Image):-
    image(_,_,Pixeles,Image),
    pixelsArePixmap(Pixeles).

pixelsArePixmap([]).
pixelsArePixmap([Pixrgbd | Cdr]):-
    pixrgbd(_, _, R, G, B, _, Pixrgbd),
    R >= 0 , R =< 255,
    G >= 0 , G =< 255,
    B >= 0 , B =< 255,
    pixelsArePixmap(Cdr).
% ----------------------------------------------------------------
% Verificador Hexmap
imageIsHexmap(Image):-
    image(_,_,Pixeles,Image),
    pixelsAreHexmap(Pixeles).

pixelsAreHexmap([]).
pixelsAreHexmap([Pixhexd | Cdr]) :-
    pixhexd(_, _, ContHex, _, Pixhexd),
    string(ContHex),
    pixelsAreHexmap(Cdr).
% ----------------------------------------------------------------