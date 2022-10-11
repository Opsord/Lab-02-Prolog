% --------------------------------------------------------------
% Importacion de modulos de TDA_pixbitd y TDA_operaciones
%:- use_module(TDA_pixbitd).
%:- use_module(TDA_operaciones).
% --------------------------------------------------------------
% Constructor de imagen
image(Largo, Ancho, Pixeles, [Largo, Ancho, Pixeles]).

pixelsAreBitmap?([]).
pixelsAreBitmap?([Pixbitd | Rest]) :-
    pixbitd(_, _, Bit, _, Pixbitd),
    (Bit == 0 ; Bit == 1),
    pixelsAreBitmap?(Rest).