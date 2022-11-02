%-----------------------------------------------------------------
% Operaciones simples

% Calcular el Alto de una lista
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

% Obtiene la posicion de culquier tipo de pixel
getPosPix(Pixel, Posicion):-
    (pixbit(PosX, PosY, _, _, Pixel) ->  unir([PosX,PosY], Posicion)
    ;   
    (pixrgb(PosX, PosY, _, _, _, _, Pixel) ->  unir([PosX,PosY], Posicion)
    ;   
    (pixhex(PosX, PosY, _, _, Pixel) ->  unir([PosX,PosY], Posicion)))).

% Obtiene el contenido de cualquier tipo de pixel
getContenidoPix(Pixel, Contenido):-
    (pixbit(_, _, Bit, _, Pixel) ->  unir(Bit, Contenido)
    ;   
    (pixrgb(_, _, R, G, B, _, Pixel) ->  unir([R,G,B], Contenido)
    ;   
    (pixhex(_, _, Hex, _, Pixel) ->  unir(Hex, Contenido)))).

% Obtiene la profunidad de cualqueir tipo de pixel
getDepthPix(Pixel, Depth):-
    pixbit(_, _, _, Depth, Pixel)
    ;   
    pixrgb(_, _, _, _, _, Depth, Pixel)
    ;   
    pixhex(_, _, _, Depth, Pixel).


% Dar vuelta una lista
% Recursion interna
inversor([], ListaCache, ListaCache).
inversor([CAR|CDR], ListaCache, ListaFinal):-
    insertarPrincipio(CAR, ListaCache, ListaCache2),
    inversor(CDR, ListaCache2, ListaFinal),
    !.
% Consulta principal
reverse(ListaOriginal, ListaInvertida):-
    inversor(ListaOriginal, [], ListaInvertida).
%-----------------------------------------------------------------
% Pixbit
% [PosX, PosY, Bit, Depth]
pixbit(PosX, PosY, Bit, Depth,[PosX, PosY, Bit, Depth]):-
    integer(PosX),
    integer(PosY),
    integer(Bit),
    (Bit == 0 ; Bit == 1),
    integer(Depth).
% ----------------------------------------------------------------
% Pixrgb
% [PosX, PosY, R, G, B, Depth]
pixrgb(PosX, PosY, R, G, B, Depth,[PosX, PosY, R, G, B, Depth]):-
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
pixhex(PosX, PosY, Hex, Depth,[PosX, PosY, Hex, Depth]):-
    integer(PosX),
    integer(PosY),
    string(Hex),
    integer(Depth).
% ----------------------------------------------------------------
% Constructor de imagen
image(Alto, Largo, Pixeles, [Alto, Largo, Pixeles]).
% ----------------------------------------------------------------
% Recursion interna del verificador
pixelsAreBitmap([]).
pixelsAreBitmap([Pixel|CDR]) :-
    pixbit(_, _, Bit, _, Pixel),
    (Bit == 0 ; Bit == 1),
    pixelsAreBitmap(CDR),
    !.
         
% Verificador Bitmap
imageIsBitmap(Image):-
    image(_, _, Pixeles, Image),
    pixelsAreBitmap(Pixeles).
% ----------------------------------------------------------------
% Recursion interna del verificador
pixelsArePixmap([]).
pixelsArePixmap([Pixel|CDR]):-
    pixrgb(_, _, R, G, B, _, Pixel),
    R >= 0 , R =< 255,
    G >= 0 , G =< 255,
    B >= 0 , B =< 255,
    pixelsArePixmap(CDR), 
    !.

% Verificador Pixmap
imageIsPixmap(Image):-
    image(_, _, Pixeles, Image),
    pixelsArePixmap(Pixeles).
% ----------------------------------------------------------------
% Recursion interna del verificador
pixelsAreHexmap([]).
pixelsAreHexmap([Pixel|CDR]) :-
    pixhex(_, _, ContHex, _, Pixel),
    string(ContHex),
    pixelsAreHexmap(CDR),
    !.

% Verificador Hexmap
imageIsHexmap(Image):-
    image(_, _, Pixeles, Image),
    pixelsAreHexmap(Pixeles).
% ----------------------------------------------------------------
% Verificador de comprimido
imageIsCompressed(Image):-
    image(X, Y, Pixeles, Image),
    contar(Pixeles, Alto),
    not(Alto  =:= (X*Y)).
% ----------------------------------------------------------------
% Caso base
flipHPixeles(_, [], PixsBase, PixsBase).
% Llamado recursivo
flipHPixeles(MaxPosX, [Pix|CDR], ListaCacheInicial, ListPixOut):-
    %Se le aplica Flip al pixel individualmente
    flipH_Universal(Pix, MaxPosX, PixOut),
    %Se inserta el PixOut en una lista cache
    insertarPrincipio(PixOut, ListaCacheInicial, ListaCache02),
    %Se llama recursivamente al flipH
    flipHPixeles(MaxPosX, CDR, ListaCache02, ListPixOut),
    !.
% Flip interno universal
flipH_Universal(PixIn, MaxPosXImage, PixOut):-
    getPosPix(PixIn, [PosX, PosY]),
    getContenidoPix(PixIn, Contenido),
    getDepthPix(PixIn, Depth),
    NewPosX is abs(PosX - MaxPosXImage),
    unir([NewPosX, PosY, Contenido, Depth], PixOut).

% Invertir horizontalmente (Logica Principal)
imageFlipH(Image, NewImage):-
    image(X, Y, Pixeles, Image),
    flipHPixeles(X, Pixeles, [], ListPixOut),
    image(X, Y, ListPixOut, NewImage).
% ----------------------------------------------------------------
% Caso base
flipVPixeles(_, [], PixsBase, PixsBase).
% Llamado recursivo
flipVPixeles(MaxPosY, [Pix|CDR], ListaCacheInicial, ListPixOut):-
    %Se le aplica Flip al pixel individualmente
    flipV_Universal(Pix, MaxPosY, PixOut),
    %Se inserta el PixOut en una lista cache
    insertarPrincipio(PixOut, ListaCacheInicial, ListaCache02),
    %Se llama recursivamente al flipV
    flipVPixeles(MaxPosY, CDR, ListaCache02, ListPixOut),
    !.

% FlipV universal
flipV_Universal(PixIn, MaxPosY, PixOut):-
    getPosPix(PixIn, [PosX, PosY]),
    getContenidoPix(PixIn, Contenido),
    getDepthPix(PixIn, Depth),
    NewPosY is abs(PosY - MaxPosY),
    unir([PosX, NewPosY, Contenido, Depth], PixOut).

% Invertir verticalmente
imageFlipV(Image, NewImage):-
    image(X, Y, Pixeles, Image),
    flipVPixeles(Y, Pixeles, [], ListPixOut),
    image(X, Y, ListPixOut, NewImage).
% ----------------------------------------------------------------
% Crop
imageCrop(Image, X1, Y1, X2, Y2, NewImage):-
    image(X, Y, Pixeles, Image),
    cropPix(Pixeles, X1, Y1, X2, Y2, [], ListPixOut),
    image(X, Y, ListPixOut, NewImage).

% Caso base de la recursion
cropPix([], _, _, _, _, ListaCache, ListaCache).
% Recursiva interna
cropPix([Pixel|CDR], X1, Y1, X2, Y2, ListaCache, ListPixOut):-
    (verificadorCrop(Pixel, X1, Y1, X2, Y2) ->
    (insertarPrincipio(Pixel, ListaCache, ListaCache2), cropPix(CDR, X1, Y1, X2, Y2, ListaCache2, ListPixOut))
    ;
    cropPix(CDR, X1, Y1, X2, Y2, ListaCache, ListPixOut)),
    !.
% Verificador de pixel individual
verificadorCrop(Pixel, X1, Y1, X2, Y2):-
    getPosPix(Pixel, [PosX, PosY]),
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
    pixrgb(PosX, PosY, R, G, B, Depth, PixRGB),
    rgbAhex(R,HexR),
    rgbAhex(G,HexG),
    rgbAhex(B,HexB),
    atom_concat(HexR, HexG, HEXCache),
    atom_concat(HEXCache, HexB, HEX),
    atom_concat("#", HEX, HexPreFinal),
    atom_string(HexPreFinal, HexFinal),
    pixhex(PosX, PosY, HexFinal, Depth, PixHEX).
% Conversor de numero a HEX
rgbAhex(Num,Hex):-
    ParteEntera is truncate(Num/16),
    ParteDec is truncate((Num/16 - ParteEntera)*16),
    index(ParteEntera, HEX1, ["0","1","2","3","4","5","6","7","8","9","A","B","C","D","F"]),
    index(ParteDec, HEX2, ["0","1","2","3","4","5","6","7","8","9","A","B","C","D","F"]),
    atom_concat(HEX1, HEX2, Hex).
% ----------------------------------------------------------------
% Lista con los pixeles filtrado
getContPixHist([], ListaCache, ListaCache).
getContPixHist([Pixel|CDR], ListaCache, ListaContenido):-
    getContenidoPix(Pixel, Contenido),
    %If
    (not(member(Contenido, ListaCache)) ->  
    (insertarPrincipio(Contenido, ListaCache, ListaCache2), getContPixHist(CDR, ListaCache2, ListaContenido))
    ;   
    %Else
    getContPixHist(CDR, ListaCache, ListaContenido)).

% Lista con todas las apariciones de los pixeles de la imagen
getAparicionesPix([], ListaCache, ListaCache).
getAparicionesPix([Pixel|CDR], ListaCache, ListaContenido):-
    getContenidoPix(Pixel, Contenido),
    insertarPrincipio(Contenido, ListaCache, ListaCache2),
    getAparicionesPix(CDR, ListaCache2, ListaContenido).

histogramador([], _, ListaCache, ListaCache).
histogramador([PixelEvaluado|CDR], ListaApariciones, ListaCache, Histograma):-
    %getContenidoPix(Pixeles, [], [CAR|CDR]),
    %getAparicionesPix(Pixeles, [], ListaApariciones),
    frecuenciaElementoEnLista(PixelEvaluado, ListaApariciones, 0, Repeticiones),
    insertarPrincipio([PixelEvaluado,Repeticiones], ListaCache, ListaCache2),
    histogramador(CDR, ListaApariciones, ListaCache2, Histograma),
    !.
% Histograma de una imagen
imageToHistogram(Image, Histograma):-
    image(_, _, Pixeles, Image),
    getContPixHist(Pixeles, [], ContenidoPix),
    getAparicionesPix(Pixeles, [], ListaApariciones),
    histogramador(ContenidoPix, ListaApariciones, [], Histograma),
    !.
% ----------------------------------------------------------------
% Recursion interna para rotar imagen
% Se sigue la formula (X,Y) -> (Y, (Area - X))'
rotador90Pixeles([], ListaCache, ListaCache, _).
rotador90Pixeles([Pixel|CDR], ListaCache, PixelesRotados, Area):-
    getPosPix(Pixel, [PosX, _]),
    getContenidoPix(Pixel, Contenido),
    getDepthPix(Pixel, Depth),
    NewPosY is Area - PosX,
    unir([PosX, NewPosY, Contenido, Depth], NewPixel),
    insertarPrincipio(NewPixel, ListaCache, ListaCache2),
    rotador90Pixeles(CDR, ListaCache2, PixelesRotados, Area),
    !.

% Rotar una imagen en 90 grados de forma cartesiana en sentido horario
imageRotate90(Image, NewImage):-
    image(Alto, Largo, Pixeles, Image),
    Area is Alto*Largo,
    rotador90Pixeles(Pixeles, [], PixelesRotados, Area),
    image(Alto, Largo, PixelesRotados, NewImage).
% ----------------------------------------------------------------
% Formato de imagen comprimida
imagenComprimida(Alto, Largo, Pixeles, PixelComprimido, ListaProfundidades, [Alto, Largo, Pixeles, PixelComprimido, ListaProfundidades]).
% Metodo para ordenar el histograma y colocar primero la mayor frecuencia
sortHistogram(Histogram, SortedHistogram):-
    predsort(compareAvg, Histogram, SortedHistogram).
compareAvg(X,  [_,A1], [_,A2]) :- 
    compare(X, A2, A1).
% Recursion interna para comprimir los pixeles
comprimir([], _, ListaCache, ListaCache).
comprimir([Pixel|CDR], PixelFrecuente, ListaCache, PixelesComprimidos):-
    getContenidoPix(Pixel, Contenido),
    (Contenido == PixelFrecuente ->
    comprimir(CDR, PixelFrecuente, ListaCache, PixelesComprimidos)
    ;   
    (insertarPrincipio(Pixel, ListaCache, ListaCache2), comprimir(CDR, PixelFrecuente, ListaCache2, PixelesComprimidos))), !.
% Parte principal de comprimir
imageCompress(Image, ImagenComprimida):-
    image(Alto, Largo, Pixeles, Image),
    %image(_, _, Pixeles, Image),
    imageToHistogram(Image,Histogram),
    %sortHistogram(Histogram, [[PixelFrecuente|FrecuenciaPix]|CDR]),
    sortHistogram(Histogram, [[PixelFrecuente|FrecuenciaPix]|_]),
    comprimir(Pixeles, PixelFrecuente, [], PixelesComprimidos),
    imagenComprimida(Alto, Largo, PixelesComprimidos, PixelFrecuente, FrecuenciaPix, ImagenComprimida).
% ----------------------------------------------------------------
% Recursion interna del modificador
modificador([], _, ListaCache, ListaCache).
modificador([PixelOriginal|CDR], PixelAModificar, ListaCache, NewPixeles):-
    getPosPix(PixelOriginal, PosPixelOriginal),
    getPosPix(PixelAModificar, PosPixelAModificar),
    (PosPixelOriginal == PosPixelAModificar ->  
    (insertarPrincipio(PixelAModificar, ListaCache, ListaCache2), modificador(CDR, PixelAModificar, ListaCache2, NewPixeles))
    ;   
    (insertarPrincipio(PixelOriginal, ListaCache, ListaCache2), modificador(CDR, PixelAModificar, ListaCache2, NewPixeles))),
    !.
    

% Parte principal de modificar
imageChangePixel(Image, PixelAModificar, NewImage):-
    image(Alto, Largo, Pixeles, Image),
    modificador(Pixeles, PixelAModificar, [], NewPixeles),
    image(Alto, Largo, NewPixeles, NewImage).
% ----------------------------------------------------------------
% Parte principal del "inversor" de color
imageInvertColorRGB(PixRGB, NewPixRGB):-
    pixrgb(PosX, PosY, R, G, B, Depth, PixRGB),
    NewR is abs(255 - R),
    NewG is abs(255 - G),
    NewB is abs(255 - B),
    pixrgb(PosX, PosY, NewR, NewG, NewB, Depth, NewPixRGB).
% ----------------------------------------------------------------
% Transformador a string
pixToString(Pixel, StringContenido):-
    getContenidoPix(Pixel, Contenido),
    (integer(Contenido) ->  
    atom_string(Contenido, StringContenido)
    ;   
    (string(Contenido) ->  
    unir(Contenido, StringContenido)
    ;   
    atomics_to_string(Contenido, " ", StringContenido))).
% Recursion interna    
convPixString([], _, _, StringCache, StringCache).
convPixString([Pixel|CDR], Contador, TopeImagen, StringCache, StringPixeles):-
    pixToString(Pixel, StringContenido),
    (Contador == 0 ->  
    (atom_concat("\n", StringContenido, PreString1), NewContador is TopeImagen)
    ;   
    (atom_concat("\t", StringContenido, PreString1), NewContador is Contador - 1)),
    atom_concat( PreString1, StringCache, StringCache2),
    convPixString(CDR, NewContador, TopeImagen, StringCache2, StringPixeles),
    !.

% Llamado principal
imageToString(Image, String):-
    image(_, Largo, Pixeles, Image),
    reverse(Pixeles, ReversedPix),
    convPixString(ReversedPix, 1, Largo, "", String).
% ----------------------------------------------------------------