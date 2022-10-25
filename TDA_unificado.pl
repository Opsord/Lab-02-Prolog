%-----------------------------------------------------------------
% Operaciones simples

% Calcular el largo de una lista
contar([],0).
contar([_|Resto], N) :- 
    contar(Resto, Acumulador),
    N is Acumulador + 1.

% Insertar un elemento al principio de una lista
insertarPrincipio(Elemento, [], [Elemento] ).
insertarPrincipio(Elemento, Lista, [Elemento|Lista]).

% Index por contador
index(0, CAR, [CAR|_]).
index(Contador, Elemento, [_|CDR]):-
    NewContador is Contador - 1,
    index(NewContador, Elemento, CDR).
    

% Unir 2 elementos
unir(Elemento, Elemento).

% Contar la frecuencia de un elemento en una lista
frecuenciaElementoEnLista(_, [], Frecuencia, Resultado):-
    unir(Frecuencia, Resultado).
%frecuenciaElementoEnLista(Elemento, ListaElementos, Frecuencia, Resultado)
frecuenciaElementoEnLista(Elemento, [CAR|CDR], Frecuencia, Resultado):-
    %If
    (Elemento == CAR -> 
    (NewFrecuencia is Frecuencia + 1, frecuenciaElementoEnLista(Elemento, CDR, NewFrecuencia, Resultado))
    ;
    %Else
    frecuenciaElementoEnLista(Elemento, CDR, Frecuencia, Resultado)).
%-----------------------------------------------------------------
% Pixbit
% [PosX, PosY, Bit, Depth]
pixbitd(PosX, PosY, Bit, Depth,[PosX, PosY, Bit, Depth]):-
    integer(PosX),
    integer(PosY),
    integer(Bit),
    (Bit == 0 ; Bit == 1),
    integer(Depth).
% ----------------------------------------------------------------
% Pixrgb
% [PosX, PosY, R, G, B, Depth]
pixrgbd(PosX, PosY, R, G, B, Depth,[PosX, PosY, R, G, B, Depth]):-
    integer(PosX),
    integer(PosY),
    integer(R),
    (R >= 0 , R =< 255),
    integer(G),
    (G >= 0 , G =< 255),
    integer(B),
    (B >= 0 , B =< 255),
    integer(Depth).
% ----------------------------------------------------------------
% Pixhex
% [PosX, PosY, Hex, Depth]
pixhexd(PosX, PosY, Hex, Depth,[PosX, PosY, Hex, Depth]):-
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
pixelsAreBitmap([Pixbitd | CDR]) :-
    pixbitd(_, _, Bit, _, Pixbitd),
    (Bit == 0 ; Bit == 1),
    pixelsAreBitmap(CDR).
% ----------------------------------------------------------------
% Verificador Pixmap
imageIsPixmap(Image):-
    image(_,_,Pixeles,Image),
    pixelsArePixmap(Pixeles).

pixelsArePixmap([]).
pixelsArePixmap([Pixrgbd | CDR]):-
    pixrgbd(_, _, R, G, B, _, Pixrgbd),
    R >= 0 , R =< 255,
    G >= 0 , G =< 255,
    B >= 0 , B =< 255,
    pixelsArePixmap(CDR).
% ----------------------------------------------------------------
% Verificador Hexmap
imageIsHexmap(Image):-
    image(_,_,Pixeles,Image),
    pixelsAreHexmap(Pixeles).

pixelsAreHexmap([]).
pixelsAreHexmap([Pixhexd | CDR]) :-
    pixhexd(_, _, ContHex, _, Pixhexd),
    string(ContHex),
    pixelsAreHexmap(CDR).
% ----------------------------------------------------------------
% Verificador de comprimido
imageIsCompressed(Image):-
    image(X, Y, Pixeles, Image),
    contar(Pixeles, Largo),
    not(Largo  =:= (X*Y)).
% ----------------------------------------------------------------
% Invertir horizontalmente
imageFlipH(Image, NewImage):-
    image(X, Y, Pixeles, Image),
    flipHPixeles(X, Pixeles, [], ListPixOut),
    image(X, Y, ListPixOut, NewImage).

% Caso base
flipHPixeles(_, [], PixsBase, PixsBase).
% Llamado recursivo
flipHPixeles(MaxPosX, [Pix|CDR], ListaCacheInicial, ListPixOut):-
    %Se le aplica Flip al pixel individualmente
    flipH_BIT(Pix, MaxPosX, PixOut),
    %(flipH_BIT(Pix, MaxPosX, PixOut);
    %flipH_RGB(Pix, MaxPosX, PixOut);
    %flipH_HEX(Pix, MaxPosX, PixOut)),
    %Se inserta el PixOut en una lista cache
    insertarPrincipio(PixOut, ListaCacheInicial, ListaCache02),
    %Se llama recursivamente al flipH
    flipHPixeles(MaxPosX, CDR, ListaCache02, ListPixOut),
    !.

% Flips inviduales por tipo de pixel
flipH_BIT(PixIn, MaxPosXImage, PixOut):-
    pixbitd(PosX, PosY, Bit, Depth, PixIn),
    NewPosX is abs(PosX - MaxPosXImage),
    pixbitd(NewPosX, PosY, Bit, Depth, PixOut).
flipH_RGB(PixIn, MaxPosXImage, PixOut):-
    pixrgbd(PosX, PosY, R, G, B, Depth, PixIn),
    NewPosX is abs(PosX - MaxPosXImage),
    pixrgbd(NewPosX, PosY, R, G, B, Depth, PixOut).
flipH_HEX(PixIn, MaxPosXImage, PixOut):-
    pixhexd(PosX, PosY, Hex, Depth, PixIn),
    NewPosX is abs(PosX - MaxPosXImage),
    pixhexd(NewPosX, PosY, Hex, Depth, PixOut).
% ----------------------------------------------------------------
% Invertir verticalmente
imageFlipV(Image, NewImage):-
    image(X, Y, Pixeles, Image),
    flipVPixeles(Y, Pixeles, [], ListPixOut),
    image(X, Y, ListPixOut, NewImage).

% Caso base
flipVPixeles(_, [], PixsBase, PixsBase).
% Llamado recursivo
flipVPixeles(MaxPosY, [Pix|CDR], ListaCacheInicial, ListPixOut):-
    %Se le aplica Flip al pixel individualmente
    flipV_BIT(Pix, MaxPosY, PixOut),
    %(flipV_BIT(Pix, MaxPosY, PixOut);
    %flipV_RGB(Pix, MaxPosY, PixOut);
    %flipV_HEX(Pix, MaxPosY, PixOut)),
    %Se inserta el PixOut en una lista cache
    insertarPrincipio(PixOut, ListaCacheInicial, ListaCache02),
    %Se llama recursivamente al flipV
    flipVPixeles(MaxPosY, CDR, ListaCache02, ListPixOut),
    !.

% Flips inviduales por tipo de pixel
flipV_BIT(PixIn, MaxPosYImage, PixOut):-
    pixbitd(PosX, PosY, Bit, Depth, PixIn),
    NewPosY is abs(PosY - MaxPosYImage),
    pixbitd(PosX, NewPosY, Bit, Depth, PixOut).
flipV_RGB(PixIn, MaxPosYImage, PixOut):-
    pixrgbd(PosX, PosY, R, G, B, Depth, PixIn),
    NewPosY is abs(PosY - MaxPosYImage),
    pixrgbd(PosX, NewPosY, R, G, B, Depth, PixOut).
flipV_HEX(PixIn, MaxPosYImage, PixOut):-
    pixhexd(PosX, PosY, Hex, Depth, PixIn),
    NewPosY is abs(PosY - MaxPosYImage),
    pixhexd(PosX, NewPosY, Hex, Depth, PixOut).
% ----------------------------------------------------------------
% Crop
imageCrop(Image, X1, Y1, X2, Y2, NewImage):-
    image(X, Y, Pixeles, Image),
    %Funcion interna que verifica los pixeles del cuadrante
    cropPix(Pixeles, X1, Y1, X2, Y2, [], ListPixOut),
    image(X, Y, ListPixOut, NewImage).

% Caso base de la recursion
cropPix([], _, _, _, _, ListaCache, ListaCache).
% Funcion principal recursiva
cropPix([Pixel|CDR], X1, Y1, X2, Y2, ListaCache, ListPixOut):-
    (verificadorCrop(Pixel, X1, Y1, X2, Y2)
    ->(insertarPrincipio(Pixel, ListaCache, ListaCache2),
          cropPix(CDR, X1, Y1, X2, Y2, ListaCache2, ListPixOut));
    cropPix(CDR, X1, Y1, X2, Y2, ListaCache, ListPixOut)),
    !.
% Verificador de pixel individual
verificadorCrop(Pixel, X1, Y1, X2, Y2):-
    pixbitd(PosX, PosY, _, _, Pixel),
    X1 =< PosX,
    Y1 =< PosY,
    X2 >= PosX,
    Y2 >= PosY.
% ----------------------------------------------------------------
% Conversor RGB a HEX
imageRGBToHex(Image, NewImage):-
    image(X, Y, Pixeles, Image),
    conversorRGBaHEX(Pixeles, [], ListPixOut),
    image(X, Y, ListPixOut, NewImage).

conversorRGBaHEX([], ListaCache, ListaCache).
conversorRGBaHEX([PixRGB|CDR], ListaCache, ListPixOut):-
    conversorIndividualRGBaHEX(PixRGB, PixHEX),
    insertarPrincipio(PixHEX, ListaCache, ListaCache2),
    conversorRGBaHEX(CDR, ListaCache2, ListPixOut),
    !.
% Conversor de argumento RGB a HEX
conversorIndividualRGBaHEX(PixRGB,PixHEX):-
    pixrgbd(PosX, PosY, R, G, B, Depth, PixRGB),
    rgbAhex(R,HexR),
    rgbAhex(G,HexG),
    rgbAhex(B,HexB),
    atom_concat(HexR, HexG, HEXCache),
    atom_concat(HEXCache, HexB, HEX),
    atom_concat("#", HEX, HexPreFinal),
    atom_string(HexPreFinal, HexFinal),
    pixhexd(PosX, PosY, HexFinal, Depth, PixHEX).
% Conversor de numero a HEX
rgbAhex(Num,Hex):-
    ParteEntera is truncate(Num/16),
    ParteDec is truncate((Num/16 - ParteEntera)*16),
    index(ParteEntera, HEX1, ["0","1","2","3","4","5","6","7","8","9","A","B","C","D","F"]),
    index(ParteDec, HEX2, ["0","1","2","3","4","5","6","7","8","9","A","B","C","D","F"]),
    atom_concat(HEX1, HEX2, Hex).
% ----------------------------------------------------------------
% Histograma de una imagen
imageToHistogram(Image, Histograma):-
    image(_, _, Pixeles, Image),
    getContenidoPix(Pixeles, [], ContenidoPix),
    getAparicionesPix(Pixeles, [], ListaApariciones),
    histogramador(ContenidoPix, ListaApariciones, [], Histograma),
    !.

histogramador([], _, ListaCache, ListaCache).
histogramador([PixelEvaluado|CDR], ListaApariciones, ListaCache, Histograma):-
    %getContenidoPix(Pixeles, [], [CAR|CDR]),
    %getAparicionesPix(Pixeles, [], ListaApariciones),
    frecuenciaElementoEnLista(PixelEvaluado, ListaApariciones, 0, Repeticiones),
    insertarPrincipio([PixelEvaluado,Repeticiones], ListaCache, ListaCache2),
    histogramador(CDR, ListaApariciones, ListaCache2, Histograma),
    !.
% Lista con los pixeles filtrado
getContenidoPix([], ListaCache, ListaCache).
getContenidoPix([Pixel|CDR], ListaCache, ListaContenido):-
    %Funciona con PIXBIT (por ahora)
    pixbitd(_, _, Bit, _, Pixel),
    %If
    (not(member(Bit, ListaCache)) ->  
    (insertarPrincipio(Bit, ListaCache, ListaCache2), getContenidoPix(CDR, ListaCache2, ListaContenido))
    ;   
    %Else
    getContenidoPix(CDR, ListaCache, ListaContenido)).
% Lista con todas las apariciones de los pixeles
getAparicionesPix([], ListaCache, ListaCache).
getAparicionesPix([Pixel|CDR], ListaCache, ListaContenido):-
    %Funciona con PIXBIT (por ahora)
    pixbitd(_, _, Bit, _, Pixel), 
    insertarPrincipio(Bit, ListaCache, ListaCache2),
    getAparicionesPix(CDR, ListaCache2, ListaContenido).
% ----------------------------------------------------------------
% Rotar una imagen en 90 grados de forma cartesiana en sentido horario
imageRotate90(Image, NewImage):-
    image(Largo, Ancho, Pixeles, Image),
    Area is Largo*Ancho,
    rotador90Pixeles(Pixeles, [], PixelesRotados, Area),
    image(Largo, Ancho, PixelesRotados, NewImage).
% Se sigue la formula (X,Y) -> (Y, (Area - X))'
rotador90Pixeles([], ListaCache, ListaCache, _).
rotador90Pixeles([Pixel|CDR], ListaCache, PixelesRotados, Area):-
    pixbitd(PosX, PosY, Bit, Depth, Pixel),
    NewPosY is Area - PosX,
    pixbitd(PosY, NewPosY, Bit, Depth, NewPixel),
    insertarPrincipio(NewPixel, ListaCache, ListaCache2),
    rotador90Pixeles(CDR, ListaCache2, PixelesRotados, Area),
    !.