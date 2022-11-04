/*________________________________________________________________________________________________

██╗      █████╗ ██████╗     ██████╗        ██████╗ ██████╗  ██████╗ ██╗      ██████╗  ██████╗ 
██║     ██╔══██╗██╔══██╗    ╚════██╗██╗    ██╔══██╗██╔══██╗██╔═══██╗██║     ██╔═══██╗██╔════╝ 
██║     ███████║██████╔╝     █████╔╝╚═╝    ██████╔╝██████╔╝██║   ██║██║     ██║   ██║██║  ███╗
██║     ██╔══██║██╔══██╗    ██╔═══╝ ██╗    ██╔═══╝ ██╔══██╗██║   ██║██║     ██║   ██║██║   ██║
███████╗██║  ██║██████╔╝    ███████╗╚═╝    ██║     ██║  ██║╚██████╔╝███████╗╚██████╔╝╚██████╔╝
╚══════╝╚═╝  ╚═╝╚═════╝     ╚══════╝       ╚═╝     ╚═╝  ╚═╝ ╚═════╝ ╚══════╝ ╚═════╝  ╚═════╝ 
__________________________________________________________________________________________________*/
% Nombre: Andres Zelaya Droguett
% Seccion: 13204-0-A-1
% Profesor de seccion:  Gonzalo Martinez Ramirez


/*________________________________________________________________________________________________

████████╗██████╗  █████╗     ██████╗ ██╗██╗  ██╗██████╗ ██╗████████╗
╚══██╔══╝██╔══██╗██╔══██╗    ██╔══██╗██║╚██╗██╔╝██╔══██╗██║╚══██╔══╝
   ██║   ██║  ██║███████║    ██████╔╝██║ ╚███╔╝ ██████╔╝██║   ██║   
   ██║   ██║  ██║██╔══██║    ██╔═══╝ ██║ ██╔██╗ ██╔══██╗██║   ██║   
   ██║   ██████╔╝██║  ██║    ██║     ██║██╔╝ ██╗██████╔╝██║   ██║   
   ╚═╝   ╚═════╝ ╚═╝  ╚═╝    ╚═╝     ╚═╝╚═╝  ╚═╝╚═════╝ ╚═╝   ╚═╝   
__________________________________________________________________________________________________*/
% Dominio:
% Pixbit: pixBIT
% PosX, PosY, Bit, Depth: Enteros

% Precidados:  
% pixbit(PosX, PosY, Bit, Depth,[PosX, PosY, Bit, Depth]) (aridad = 5)

% Metas primarias: pixbit
% Metas secundarias: N.A.

% ---------------------------REPRESENTACION-------------------------------------------------------

% El TDA pixBIT se representa a traves de una lista (Integer X Integer X Integer X Integer X List),
% la cual contiene los parametros del pixel y tambien una lista con los mismos
% datos pero que puede ser usada a forma de "ID"

% --------------------------CONSTRUCTOR Y PERTENENCIA---------------------------------------------
% Dominio: (Int X Int X Int X Int X List)
% Descripcion: Predicado que construye un pixel de tipo BIT
pixbit(PosX, PosY, Bit, Depth,[PosX, PosY, Bit, Depth]):-
    integer(PosX),
    integer(PosY),
    integer(Bit),
    (Bit == 0 ; Bit == 1),
    integer(Depth).

%...................................................................................

% Dominio: (List)
% Descripcion: Predicado que verifica que una lista de pixeles sean del tipo BIT
% Metas secundarias: pixbit
% Tipo de recursion: Cola
pixelsAreBitmap([]).
pixelsAreBitmap([Pixel|CDR]) :-
    pixbit(_, _, Bit, _, Pixel),
    (Bit == 0 ; Bit == 1),
    pixelsAreBitmap(CDR),
    !.

/*________________________________________________________________________________________________

████████╗██████╗  █████╗     ██████╗ ██╗██╗  ██╗██████╗  ██████╗ ██████╗ 
╚══██╔══╝██╔══██╗██╔══██╗    ██╔══██╗██║╚██╗██╔╝██╔══██╗██╔════╝ ██╔══██╗
   ██║   ██║  ██║███████║    ██████╔╝██║ ╚███╔╝ ██████╔╝██║  ███╗██████╔╝
   ██║   ██║  ██║██╔══██║    ██╔═══╝ ██║ ██╔██╗ ██╔══██╗██║   ██║██╔══██╗
   ██║   ██████╔╝██║  ██║    ██║     ██║██╔╝ ██╗██║  ██║╚██████╔╝██████╔╝
   ╚═╝   ╚═════╝ ╚═╝  ╚═╝    ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝ ╚═════╝ 
__________________________________________________________________________________________________*/
% Dominio:
% Pixrgb: pixHEX
% PosX, PosY, Depth: Enteros
% Hex: String

% Precidados:  
% pixrgb(PosX, PosY, R, G, B, Depth,[PosX, PosY, R, G, B, Depth]) (aridad = 7)

% Metas primarias: pixrgb
% Metas secundarias: N.A.

% ---------------------------REPRESENTACION-------------------------------------------------------

% El TDA pixRGB se representa a traves de 
% una lista (Integer X Integer X Integer X Integer X Integer X Integer X List),
% la cual contiene los parametros del pixel y tambien una lista con los mismos
% datos pero que puede ser usada a forma de "ID"

% --------------------------CONSTRUCTOR Y PERTENENCIA---------------------------------------------
% Dominio: (Integer X Integer X Integer X Integer X Integer X Integer X List)
% Descripcion: Predicado que construye un pixel de tipo RGB
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

%...................................................................................

% Dominio: (List)
% Descripcion: Predicado que verifica que una lista de pixeles sean del tipo RGB
% Metas secundarias: pixrgb
% Tipo de recursion: Cola
pixelsArePixmap([]).
pixelsArePixmap([Pixel|CDR]):-
    pixrgb(_, _, R, G, B, _, Pixel),
    R >= 0 , R =< 255,
    G >= 0 , G =< 255,
    B >= 0 , B =< 255,
    pixelsArePixmap(CDR), 
    !.
/*________________________________________________________________________________________________

████████╗██████╗  █████╗     ██████╗ ██╗██╗  ██╗██╗  ██╗███████╗██╗  ██╗
╚══██╔══╝██╔══██╗██╔══██╗    ██╔══██╗██║╚██╗██╔╝██║  ██║██╔════╝╚██╗██╔╝
   ██║   ██║  ██║███████║    ██████╔╝██║ ╚███╔╝ ███████║█████╗   ╚███╔╝ 
   ██║   ██║  ██║██╔══██║    ██╔═══╝ ██║ ██╔██╗ ██╔══██║██╔══╝   ██╔██╗ 
   ██║   ██████╔╝██║  ██║    ██║     ██║██╔╝ ██╗██║  ██║███████╗██╔╝ ██╗
   ╚═╝   ╚═════╝ ╚═╝  ╚═╝    ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝
__________________________________________________________________________________________________*/
% Dominio:
% PixHEX: pixHEX
% PosX, PosY, Hex, Depth: Enteros

% Precidados:  
% pixhex(PosX, PosY, HEX Depth,[PosX, PosY, Hex, Depth]) (aridad = 5)

% Metas primarias: pixhex
% Metas secundarias: N.A.

% ---------------------------REPRESENTACION-------------------------------------------------------

% El TDA pixHEX se representa a traves de 
% una lista (Integer X Integer X String X Integer X List),
% la cual contiene los parametros del pixel y tambien una lista con los mismos
% datos pero que puede ser usada a forma de "ID"

% --------------------------CONSTRUCTOR Y PERTENENCIA---------------------------------------------
% Dominio: (Integer X Integer X String X Integer X List)
% Descripcion: Predicado que construye un pixel de tipo Hex
pixhex(PosX, PosY, HEX, Depth,[PosX, PosY, HEX, Depth]):-
    integer(PosX),
    integer(PosY),
    string(HEX),
    integer(Depth).

%...................................................................................

% Dominio: (List)
% Descripcion: Predicado que verifica que una lista de pixeles sean del tipo HEX
% Metas secundarias: pixhex
% Tipo de recursion: Cola
pixelsAreHexmap([]).
pixelsAreHexmap([Pixel|CDR]) :-
    pixhex(_, _, ContHEX, _, Pixel),
    string(ContHEX),
    pixelsAreHexmap(CDR),
    !.
/*________________________________________________________________________________________________

████████╗██████╗  █████╗     ███████╗██╗  ██╗████████╗██████╗  █████╗ ███████╗
╚══██╔══╝██╔══██╗██╔══██╗    ██╔════╝╚██╗██╔╝╚══██╔══╝██╔══██╗██╔══██╗██╔════╝
   ██║   ██║  ██║███████║    █████╗   ╚███╔╝    ██║   ██████╔╝███████║███████╗
   ██║   ██║  ██║██╔══██║    ██╔══╝   ██╔██╗    ██║   ██╔══██╗██╔══██║╚════██║
   ██║   ██████╔╝██║  ██║    ███████╗██╔╝ ██╗   ██║   ██║  ██║██║  ██║███████║
   ╚═╝   ╚═════╝ ╚═╝  ╚═╝    ╚══════╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝                                                                              
__________________________________________________________________________________________________*/
% Dominios:
% Elemento, CAR 																:Atom
% Lista, ListaCache CDR, Posicion, Contenido 									:List
% Contador, NewContador, PosX, PosY												:Integer
% Pixel																			:Pix(BIT/RGB/HEX)
	

% Precidados:  
% insertarPrincipio(Elemento, Lista, [Elemento|Lista])  						(aridad = 3)
% index(Contador, Elemento, Lista)                  							(aridad = 3)
% unir(Elemento, Elemento)														(aridad = 2)
% frecuenciaElementoEnLista(Elemento, Lista, Contador, Resultado)				(aridad = 4)
% getPosPix(Pixel, Posicion) 													(aridad = 2)
% getContenidoPix(Pixel, Contenido)												(aridad = 2)
% getDepthPix(Pixel, Depth)														(aridad = 2)
% reverse(Lista, Lista) 														(aridad = 2)
% inversor(Lista, ListaCache, ListaFinal)										(aridad = 3)

% Metas priarias: insertarPrincipio, index, unir, frecuenciaElementoEnLista, getPosPix,
% 				  getContenidoPix, getDepthPix, reverse.

% Metas Secundarias: inversor, pixbit, pixrgb, pixhex.
% 
%-------------------------------SELECTORES---------------------------------------------------------
% Dominio: (Pixel X List)
% Descripcion: Obtiene la posicion de cualquier tipo de pixel en forma de lista
% Metas secundarias: pixbit, pixrgb, pixhex
getPosPix(Pixel, Posicion):-
    (pixbit(PosX, PosY, _, _, Pixel) ->  unir([PosX,PosY], Posicion)
    ;   
    (pixrgb(PosX, PosY, _, _, _, _, Pixel) ->  unir([PosX,PosY], Posicion)
    ;   
    (pixhex(PosX, PosY, _, _, Pixel) ->  unir([PosX,PosY], Posicion)))).

%...................................................................................

% Dominio: (Pixel X List)
% Descripcion: Obtiene el contenido de cualquier tipo de pixel en forma de lista
% Metas secundarias: pixbit, pixrgb, pixhex
getContenidoPix(Pixel, Contenido):-
    (pixbit(_, _, Bit, _, Pixel) ->  unir(Bit, Contenido)
    ;   
    (pixrgb(_, _, R, G, B, _, Pixel) ->  unir([R,G,B], Contenido)
    ;   
    (pixhex(_, _, HEX, _, Pixel) ->  unir(HEX, Contenido)))).

%...................................................................................

% Dominio: (Pixel X Integer)
% Descripcion: Obtiene la profundidad de un pixel
% Meta secundaria: pixbit. pixrgb, pixhex
getDepthPix(Pixel, Depth):-
    pixbit(_, _, _, Depth, Pixel)
    ;   
    pixrgb(_, _, _, _, _, Depth, Pixel)
    ;   
    pixhex(_, _, _, Depth, Pixel).

%...................................................................................
%
% Dominio: (Atom X List X Integer X Integer)
% Descripcion: Contar la frecuencia de un elemento en una lista
frecuenciaElementoEnLista(_, [], Contador, Contador).
frecuenciaElementoEnLista(Elemento, [CAR|CDR], Contador, Resultado):-
    (Elemento == CAR -> 
    (NewContador is Contador + 1, frecuenciaElementoEnLista(Elemento, CDR, NewContador, Resultado))
    ;
    frecuenciaElementoEnLista(Elemento, CDR, Contador, Resultado)).

%...................................................................................

% Dominio: (Integer X Atom X List)
% Descripcion: Encontrar un elemento de una lista buscado por index
index(0, CAR, [CAR|_]).
index(Contador, Elemento, [_|CDR]):-
    NewContador is Contador - 1,
    index(NewContador, Elemento, CDR).

%...................................................................................

% Dominio: (Atom X Atom)
% Descripcion: Se igualan 2 elementos
unir(Elemento, Elemento).

%...................................................................................

% Dominio: (Atom X List X List)
% Descripcion: Insertar un elemento al principio de una lista
insertarPrincipio(Elemento, [], [Elemento]).
insertarPrincipio(Elemento, Lista, [Elemento|Lista]).

%...................................................................................

% Dominio: (List X List)
% Descripcion: Genera una lista invertida de la que se consulta
% Meta secundaria: inversor
reverse(ListaOriginal, ListaInvertida):-
    inversor(ListaOriginal, [], ListaInvertida).% Dar vuelta una lista
% Dominio: (List X List X List)
% Descripcion: Genera una lista invertida de la que se consulta
% Tipo recursion: cola
inversor([], ListaCache, ListaCache).
inversor([CAR|CDR], ListaCache, ListaFinal):-
    insertarPrincipio(CAR, ListaCache, ListaCache2),
    inversor(CDR, ListaCache2, ListaFinal),
    !.

/*________________________________________________________________________________________________

████████╗██████╗  █████╗     ██╗███╗   ███╗ █████╗  ██████╗ ███████╗
╚══██╔══╝██╔══██╗██╔══██╗    ██║████╗ ████║██╔══██╗██╔════╝ ██╔════╝
   ██║   ██║  ██║███████║    ██║██╔████╔██║███████║██║  ███╗█████╗  
   ██║   ██║  ██║██╔══██║    ██║██║╚██╔╝██║██╔══██║██║   ██║██╔══╝  
   ██║   ██████╔╝██║  ██║    ██║██║ ╚═╝ ██║██║  ██║╚██████╔╝███████╗
   ╚═╝   ╚═════╝ ╚═╝  ╚═╝    ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝ ╚══════╝
__________________________________________________________________________________________________*/
% Dominios:
% Image, NewImage 																	:Imagen
% Elemento, CAR 																	:Atom
% Lista, ListaCache, NewListaCache, CDR, Posicion, Contenido, ListaPixeles			:List
% Contador, NewContador, PosX, PosY, MaxPosX, MaxPosY, NewPosX, NewPosY				:Integer
% Pixel, NewPixel																	:PixBIT/RGB/HEX


% Precidados:  
% image(Largo, Ancho, Pixeles, [Largo, Ancho, Pixeles])		  						(aridad = 4)
% imageIsBitmap(Image)																(aridad = 1)
% imageIsPixmap(Image)																(aridad = 1)
% imageIsHexmap(Image)																(aridad = 1)
% imageIsCompressed(Image)															(aridad = 1)
% flipH_Universal(Pixel, MaxPosX, NewPixel) 										(aridad = 3)
% flipHPixeles(MaxPosX, [Pixel|CDR], ListaCache, ListPixeles)                       (aridad = 4)
% imageFlipH(Image, NewImage)														(aridad = 2)
% flipV_Universal(Pixel, MaxPosY, NewPixel)											(aridad = 3)
% flipVPixeles(MaxPosY, [Pixel|CDR], ListaCache, ListaPixeles						(aridad = 4)
% imageFlipV(Image, NewImage)														(aridad = 2)
% verificadorCrop(Pixel, X1, Y1, X2, Y2)											(aridad = 5)
% cropPix([Pixel|CDR], X1, Y1, X2, Y2, ListaCache, ListaPixeles) 					(aridad = 7)
% imageCrop(Image, X1, Y1, X2, Y2, NewImage) 										(aridad = 6)
% rgbAhex(Num,Hex)																	(aridad = 2)
% conversorIndividualRGBaHEX(PixRGB,PixHEX)											(aridad = 2)
% conversorRGBaHEX([PixRGB|CDR], ListaCache, ListaPixeles) 							(aridad = 3)
% imageRGBToHex(Image, NewImage)													(aridad = 2)
% getContPixHist([Pixel|CDR], ListaCache, ListaContenido)							(aridad = 3)
% getAparicionesPix([Pixel|CDR], ListaCache, ListaContenido)						(aridad = 3)
% histogramador([PixelEvaluado|CDR], ListaApariciones, ListaCache, Histograma)		(aridad = 4)
% imageToHistogram(Image, Histograma)												(aridad = 2)
% rotador90Pixeles([Pixel|CDR], ListaCache, PixelesRotados, Area)					(aridad = 4)
% imageRotate90(Image, NewImage)													(aridad = 2)
% sortHistogram(Histogram, SortedHistogram)											(aridad = 2)
% comprimir([Pixel|CDR], PixelFrecuente, ListaCache, PixelesComprimidos)			(aridad = 4)
% imagenComprimida(Alto, Largo, Pixeles, PixelComprimido, ListaProfundidades, [Alto, Largo, Pixeles, PixelComprimido, ListaProfundidades]) (aridad = 6)
% imageCompress(Image, ImagenComprimida)											(aridad = 2)
% modificador([PixelOriginal|CDR], PixelAModificar, ListaCache, NewPixeles)			(aridad = 4)
% imageChangePixel(Image, PixelAModificar, NewImage)								(aridad = 3)
% pixToString(Pixel, StringContenido)												(aridad = 2)
% convPixString([Pixel|CDR], Contador, TopeImagen, StringCache, StringPixeles)		(aridad = 5)
% imageToString(Image, String)														(aridad = 2)

% Metas priarias: image, imageIsBitmap, imageIsPixmap, imageIsHexmap, imageIsCompressed, imageFlipH
%				  imageFlipV, imageCrop, imageRGBToHex, imageToHistogram, imageRotate90, imageCompress
%				  imageChangePixel, imageToString(Image, String)

% Metas Secundarias: pixelsAreBitmap, pixelsArePixmap, pixelsAreHexmap, image, flipHPixeles, flipH_Universal
% 					 flipVPixeles, flipV_Universal, verificadorCrop, cropPix, rgbAhex, conversorIndividualRGBaHEX
%					 conversorRGBaHEX, getContPixHist, getAparicionesPix, histogramador, rotador90Pixeles, sortHistogram
%					 comprimir, imagenComprimida, modificador, imageChangePixel, convPixString
 					 
% --------------------------CONSTRUCTOR Y PERTENENCIA---------------------------------------------

% Dominio: (Integer X Integer X List X List)
% Descripcion:  Genera una lista con los parametros de la iamgen y tiene
%				un parametro que es la misma lista que se ocupa como "ID"
image(Largo, Ancho, Pixeles, [Largo, Ancho, Pixeles]).

%...................................................................................

% Dominio: (Image)
% Descripcion: Verifica si una imagen tiene pixeles del tipo BIT
% Meta secundaria: pixelsAreBitmap
imageIsBitmap(Image):-
    image(_, _, Pixeles, Image),
    pixelsAreBitmap(Pixeles).

/* Ejemplos
 * 
pixbit( 0, 0, 1, 10, PA), 
pixbit( 0, 1, 0, 20, PB), 
pixbit( 1, 0, 0, 30, PC), 
pixbit( 1, 1, 1, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsBitmap(I), 
imageToString(I, Str),
write(Str).

pixbit( 0, 0, 1, 10, PA), 
pixbit( 0, 1, 0, 20, PB), 
pixbit( 1, 0, 0, 30, PC), 
pixbit( 1, 1, 1, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I),
imageIsBitmap( I ).

--Dan falso:

pixhex( 0, 0, "FF0000", 10, PA),
pixhex( 0, 1, "FF0000", 20, PB), 
pixhex( 1, 0, "0000FF", 30, PC),
pixhex( 1, 1, "0000FF", 4, PD),
image( 2, 2, [PA, PB, PC, PD], I),
imageIsBitmap( I ).

pixrgb( 0, 0, 200, 200, 200, 10, PA),
pixrgb( 0, 1, 200, 200, 200, 20, PB),
pixrgb( 1, 0, 190, 190, 190, 30, PC), 
pixrgb( 1, 1, 190, 190, 190, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsBitmap( I ).
*/

%...................................................................................

% Dominio: (Image)
% Descripcion: Verifica si una imagen tiene pixeles del tipo RGB
% Meta secundaria: pixelsArePixmap
imageIsPixmap(Image):-
    image(_, _, Pixeles, Image),
    pixelsArePixmap(Pixeles).

/* Ejemplos
 * 
pixrgb( 0, 0, 255, 0, 0, 10, PA), 
pixrgb( 0, 1, 255, 0, 0, 20, PB),
pixrgb( 1, 0, 0, 0, 255, 30, PC), 
pixrgb( 1, 1, 0, 0, 255, 4, PD),
image( 2, 2, [PA, PB, PC, PD], I), 
imageToString(I, Str),
write(Str).

pixrgb( 0, 0, 200, 200, 200, 10, PA), 
pixrgb( 0, 1, 200, 200, 200, 20, PB), 
pixrgb( 1, 0, 190, 190,190, 30, PC), 
pixrgb( 1, 1, 190, 190, 190, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsPixmap( I ).

--Dan falso:

pixbit( 0, 0, 1, 10, PA), 
pixbit( 0, 1, 0, 20, PB), 
pixbit( 1, 0, 0, 30, PC), 
pixbit( 1, 1, 1, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I),
imageIsPixmap( I ).

pixhex( 0, 0, "#FF0000", 10, PA), 
pixhex( 0, 1, "#FF0000", 20, PB),
pixhex( 1, 0, "#0000FF", 30, PC),
pixhex( 1, 1, "#0000FF", 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsPixmap( I ).

*/

%...................................................................................

% Dominio: (Image)
% Descripcion: Verifica si una imagen tiene pixeles del tipo HEX
% Meta secundaria: pixelsAreHexmap
imageIsHexmap(Image):-
    image(_, _, Pixeles, Image),
    pixelsAreHexmap(Pixeles).

/* Ejemplos
 * 
pixhex( 0, 0, "#FF0000", 10, PA), 
pixhex( 0, 1, "#FF0000", 20, PB), 
pixhex( 1, 0, "#0000FF", 30, PC), 
pixhex( 1, 1, "#0000FF", 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageToString(I, Str),
write(Str).

pixhex( 0, 0, "#FF0000", 10, PA), 
pixhex( 0, 1, "#FF0000", 20, PB), 
pixhex( 1, 0, "#0000FF", 30, PC), 
pixhex( 1, 1, "#0000FF", 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsHexmap( I ).

--Dan falso:

pixbit( 0, 0, 1, 10, PA), 
pixbit( 0, 1, 0, 20, PB), 
pixbit( 1, 0, 0, 30, PC), 
pixbit( 1, 1, 1, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsHexmap( I ).

pixrgb( 0, 0, 200, 200, 200, 10, PA), 
pixrgb( 0, 1, 200, 200, 200, 20, PB), 
pixrgb( 1, 0, 190, 190, 190, 30, PC), 
pixrgb( 1, 1, 190, 190, 190, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsHexmap( I ).

*/

%...................................................................................

% Dominio: (Image)
% Descripcion: Verifica si una imagen ha sido comprimida
% Meta secundaria: image
imageIsCompressed(Image):-
    image(X, Y, Pixeles, Image),
    length(Pixeles, Largo),
    TotalPixeles is X*Y,
    Largo < TotalPixeles.
% --------------------------------------MODIFICADORES---------------------------------------------
% Dominio: (Pixel X Integer X Pixel)
% Descripcion: Gira horizontalmente un pixel de a cuerdo a un tope
% Meta secundaria: getPosPix, getContenidoPix, getDepthPix, unir
flipH_Universal(Pixel, MaxPosX, NewPixel):-
    getPosPix(Pixel, [PosX, PosY]),
    getContenidoPix(Pixel, Contenido),
    getDepthPix(Pixel, Depth),
    NewPosX is abs(PosX - MaxPosX),
    unir([NewPosX, PosY, Contenido, Depth], NewPixel).

% Dominio: (Integer X List X List X List)
% Descripcion: Gira horizontalmente una lista de pixeles
% Metas secundarias: flipH_Universal, insertarPrincipio
% Tipo de recurcion: cola
flipHPixeles(_, [], LisatPixeles, LisatPixeles).
flipHPixeles(MaxPosX, [Pixel|CDR], ListaCache, ListaPixeles):-
    flipH_Universal(Pixel, MaxPosX, NewPixel),
    insertarPrincipio(NewPixel, ListaCache, NewListaCache),
    flipHPixeles(MaxPosX, CDR, NewListaCache, ListaPixeles),
    !.

% Dominio: (Image X Image)
% Descripcion: Gira una imagen horizontalmente
% Metas secundarias: image, flipHPixeles
imageFlipH(Image, NewImage):-
    image(Largo, Ancho, Pixeles, Image),
    flipHPixeles(Largo, Pixeles, [], ListPixeles),
    image(Largo, Ancho, ListPixeles, NewImage).

%...................................................................................

% Dominio: (Pixel X Integer X Pixel)
% Descripcion: Gira verticalmente un pixel de a cuerdo a un tope
% Meta secundaria: getPosPix, getContenidoPix, getDepthPix, unir
flipV_Universal(Pixel, MaxPosY, NewPixel):-
    getPosPix(Pixel, [PosX, PosY]),
    getContenidoPix(Pixel, Contenido),
    getDepthPix(Pixel, Depth),
    NewPosY is abs(PosY - MaxPosY),
    unir([PosX, NewPosY, Contenido, Depth], NewPixel).

% Dominio: (Integer X List X List X List)
% Descripcion: Gira verticalmente una lista de pixeles
% Metas secundarias: flipV_Universal, insertarPrincipio
% Tipo de recurcion: cola
flipVPixeles(_, [], ListaPixeles, ListaPixeles).
flipVPixeles(MaxPosY, [Pixel|CDR], ListaCache, ListaPixeles):-
    flipV_Universal(Pixel, MaxPosY, NewPixel),
    insertarPrincipio(NewPixel, ListaCache, NewListaCache),
    flipVPixeles(MaxPosY, CDR, NewListaCache, ListaPixeles),
    !.

% Dominio: (Image X Image)
% Descripcion: Gira una imagen verticalmente
% Metas secundarias: image, flipVPixeles
imageFlipV(Image, NewImage):-
    image(X, Y, Pixeles, Image),
    flipVPixeles(Y, Pixeles, [], ListaPixeles),
    image(X, Y, ListaPixeles, NewImage).

/* Ejemplos

pixhex( 0, 0, "#FF0000", 10, PA), 
pixhex( 0, 1, "#FF0000", 20, PB), 
pixhex( 1, 0, "#0000FF", 30, PC),
pixhex( 1, 1, "#0000FF", 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I),
imageFlipV(I, I2), 
imageFlipV(I2, I3).

*/
%...................................................................................

% Dominio: (Pixel X  Integer X Integer X Integer X Integer)
% Descripcion: Verifca que la posicion de un pixel este dentro de parametros X
% Metas secundarias: getPosPix
verificadorCrop(Pixel, X1, Y1, X2, Y2):-
    getPosPix(Pixel, [PosX, PosY]),
    X1 =< PosX,
    Y1 =< PosY,
    X2 >= PosX,
    Y2 >= PosY.

% Dominio: (Pixel X  Integer X Integer X Integer X Integer X List X List)
% Descripcion: Verifca que la posicion de una lista de pixeles este dentro de parametros X
% Metas secundarias: verificadorCrop, insertarPrincipio
cropPix([], _, _, _, _, ListaCache, ListaCache).
cropPix([Pixel|CDR], X1, Y1, X2, Y2, ListaCache, ListaPixeles):-
    (verificadorCrop(Pixel, X1, Y1, X2, Y2) ->
    (insertarPrincipio(Pixel, ListaCache, NewListaCache), cropPix(CDR, X1, Y1, X2, Y2, NewListaCache, ListaPixeles))
    ;
    cropPix(CDR, X1, Y1, X2, Y2, ListaCache, ListaPixeles)),
    !.

% Dominio: (Image X  Integer X Integer X Integer X Integer X Image)
% Descripcion: Verifca que la posicion de los pixeles de una imagen este dentro de parametros X
% Metas secundarias: image, cropPix
imageCrop(Image, X1, Y1, X2, Y2, NewImage):-
    image(X, Y, Pixeles, Image),
    cropPix(Pixeles, X1, Y1, X2, Y2, [], ListPixOut),
    image(X, Y, ListPixOut, NewImage).

/* Ejemplos

pixhex( 0, 0, "#FF0000", 20, PA), 
pixhex( 0, 1, "#FF0000", 20, PB), 
pixhex( 0, 2, "#FF0000", 20, PC),
pixhex( 1, 0, "#0000FF", 30, PD), 
pixhex( 1, 1, "#0000FF", 4, PE), 
pixhex( 1, 2, "#0000FF", 4, PF), 
pixhex( 2, 0, "#0000FF", 4, PG), 
pixhex( 2, 1, "#0000FF", 4, PH), 
pixhex( 2, 2, "#0000FF", 4, PI), 
image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), 
imageCrop( I, 1, 1, 2, 2, I2), 
pixhex( 0, 0, "#0000FF", 4, PE2),
pixhex( 0, 1, "#0000FF", 4, PF2),
pixhex( 1, 0, "#0000FF", 4, PH2), 
pixhex( 1, 1, "#0000FF", 4, PI2), 
image( 2, 2, [PE2, PF2, PH2, PI2], I3).

*/
%...................................................................................

% Dominio: (Integer X String)
% Descripcion: Convierte un numero a hexadecimal
% Metas secundarias: truncate, index, atom_concat
rgbAhex(Num,Hex):-
    ParteEntera is truncate(Num/16),
    ParteDec is truncate((Num/16 - ParteEntera)*16),
    index(ParteEntera, HEX1, ["0","1","2","3","4","5","6","7","8","9","A","B","C","D","F"]),
    index(ParteDec, HEX2, ["0","1","2","3","4","5","6","7","8","9","A","B","C","D","F"]),
    atom_concat(HEX1, HEX2, Hex).

% Dominio: (Integer X Integer X Integer X Integer X Integer X Integer X Pixel)
% Descripcion: Convierte un pixBIT a pixHEX
% Metas secundarias: pixrgb, rgbAhex, atom_concat, pixhex
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

% Dominio: (List X List X List)
% Descripcion: Convierte una lista de pixRGB a una lista pixHEX
% Metas secundarias: truncate, index, atom_concat
conversorRGBaHEX([], ListaCache, ListaCache).
conversorRGBaHEX([PixRGB|CDR], ListaCache, ListaPixeles):-
    conversorIndividualRGBaHEX(PixRGB, PixHEX),
    insertarPrincipio(PixHEX, ListaCache, NewListaCache),
    conversorRGBaHEX(CDR, NewListaCache, ListaPixeles),
    !.

% Dominio: (Image X Image)
% Descripcion: Transforma un Pixmap en un Hexmap
% Metas secundarias: image, conversorRGBaHEX
imageRGBToHex(Image, NewImage):-
    image(X, Y, Pixeles, Image),
    conversorRGBaHEX(Pixeles, [], ListPixOut),
    image(X, Y, ListPixOut, NewImage).

/* Ejemplos

pixrgb( 0, 0, 200, 200, 200, 10, PA), 
pixrgb( 0, 1, 200, 200, 200, 20, PB), 
pixrgb( 1, 0, 190, 190,190, 30, PC), 
pixrgb( 1, 1, 190, 190, 190, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageIsPixmap( I ), 
imageRGBToHex(I, I2),
imageIsHexmap(I2), 
imageToString(I2, Str), 
write(Str).

*/
%...................................................................................

% Dominio: (List X List X List)
% Descripcion: Obtiene una lista con el contenido de los pixeles sin repetir
% Metas secundarias: getContenidoPix, member, insertarPrincipio
getContPixHist([], ListaCache, ListaCache).
getContPixHist([Pixel|CDR], ListaCache, ListaContenido):-
    getContenidoPix(Pixel, Contenido),
    (not(member(Contenido, ListaCache)) ->  
    (insertarPrincipio(Contenido, ListaCache, ListaCache2), getContPixHist(CDR, ListaCache2, ListaContenido))
    ;   
    getContPixHist(CDR, ListaCache, ListaContenido)).

% Dominio: (List X List X List)
% Descripcion: Obtiene una lista con el contenido de los pixeles repitiendo apariciones
% Metas secundarias: getContenidoPix, insertarPrincipio
getAparicionesPix([], ListaCache, ListaCache).
getAparicionesPix([Pixel|CDR], ListaCache, ListaContenido):-
    getContenidoPix(Pixel, Contenido),
    insertarPrincipio(Contenido, ListaCache, ListaCache2),
    getAparicionesPix(CDR, ListaCache2, ListaContenido).

% Dominio: (List X List X List)
% Descripcion: Cuenta el numero de apariciones por contenido y los une
% Metas secundarias: frecuenciaElementoEnLista, insertarPrincipio
histogramador([], _, ListaCache, ListaCache).
histogramador([PixelEvaluado|CDR], ListaApariciones, ListaCache, Histograma):-
    frecuenciaElementoEnLista(PixelEvaluado, ListaApariciones, 0, Repeticiones),
    insertarPrincipio([PixelEvaluado,Repeticiones], ListaCache, ListaCache2),
    histogramador(CDR, ListaApariciones, ListaCache2, Histograma),
    !.

% Dominio: (Image X List)
% Descripcion: Genera el histograma
% Metas secundarias: image, getContPixHist, getAparicionesPix, histogramador
imageToHistogram(Image, Histograma):-
    image(_, _, Pixeles, Image),
    getContPixHist(Pixeles, [], ContenidoPix),
    getAparicionesPix(Pixeles, [], ListaApariciones),
    histogramador(ContenidoPix, ListaApariciones, [], Histograma),
    !.

%...................................................................................

% Dominio: (List X List X List)
% Descripcion: Aplica una formula a cada pixel de una lista
% Metas secundarias: getPosPix, getContenidoPix, getDepthPix, unir, insertarPrincipio
rotador90Pixeles([], ListaCache, ListaCache, _).
rotador90Pixeles([Pixel|CDR], ListaCache, PixelesRotados, Area):-
    getPosPix(Pixel, [PosX, PosY]),
    getContenidoPix(Pixel, Contenido),
    getDepthPix(Pixel, Depth),
    NewPosX is -PosY,
    unir([NewPosX, PosX, Contenido, Depth], NewPixel),
    insertarPrincipio(NewPixel, ListaCache, ListaCache2),
    rotador90Pixeles(CDR, ListaCache2, PixelesRotados, Area),
    !.

% Dominio: (Image X Image)
% Descripcion: Rota una imagen en 90 grados
% Metas secundarias: image, rotador90Pixeles
imageRotate90(Image, NewImage):-
    image(Alto, Largo, Pixeles, Image),
    Area is Alto*Largo,
    rotador90Pixeles(Pixeles, [], PixelesRotados, Area),
    image(Alto, Largo, PixelesRotados, NewImage).

/* Ejemplos

pixhex( 0, 0, "#FF0000", 10, PA), 
pixhex( 0, 1, "#FF0000", 20, PB), 
pixhex( 1, 0, "#0000FF", 30, PC), 
pixhex( 1, 1, "#0000FF", 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageRotate90(I, I2),
imageRotate90(I2, I3), 
imageRotate90(I3, I4), 
imageRotate90(I4, I5).


pixhex( 0, 0, "#FF0000", 30, PA), 
pixhex( 0, 1, "#FF0000", 30, PB), 
pixhex( 1, 0, "#FF0000", 30, PC),
pixhex( 1, 1, "#FF0000", 30, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageRotate90(I, I2).
*/

%...................................................................................

% Dominio: (List X List)
% Descripcion: Ordena un histograma
% Metas secundarias: predsort
sortHistogram(Histogram, SortedHistogram):-
    predsort(compareAvg, Histogram, SortedHistogram).
compareAvg(X,  [_,A1], [_,A2]) :- 
    compare(X, A2, A1).

% Dominio: (List X List X List)
% Descripcion: Comprime un histograma
% Metas secundarias: getContenidoPix, comprimir
comprimir([], _, ListaCache, ListaCache).
comprimir([Pixel|CDR], PixelFrecuente, ListaCache, PixelesComprimidos):-
    getContenidoPix(Pixel, Contenido),
    (Contenido == PixelFrecuente ->
    comprimir(CDR, PixelFrecuente, ListaCache, PixelesComprimidos)
    ;   
    (insertarPrincipio(Pixel, ListaCache, ListaCache2), comprimir(CDR, PixelFrecuente, ListaCache2, PixelesComprimidos))), !.

% Constructor de imagen comprimida
imagenComprimida(Alto, Largo, Pixeles, PixelComprimido, ListaProfundidades, [Alto, Largo, Pixeles, PixelComprimido, ListaProfundidades]).

% Dominio: (Image X Image)
% Descripcion: Comprime una imagen
% Metas secundarias: image, imageToHistogram, sortHistogram, comprimir, imagenComprimida
imageCompress(Image, ImagenComprimida):-
    image(Alto, Largo, Pixeles, Image),
    imageToHistogram(Image,Histogram),
    sortHistogram(Histogram, [[PixelFrecuente|FrecuenciaPix]|_]),
    comprimir(Pixeles, PixelFrecuente, [], PixelesComprimidos),
    imagenComprimida(Alto, Largo, PixelesComprimidos, PixelFrecuente, FrecuenciaPix, ImagenComprimida).

/* Ejemplos

pixbit( 0, 0, 1, 10, PA), 
pixbit( 0, 1, 0, 20, PB), 
pixbit( 1, 0, 0, 30, PC), 
pixbit( 1, 1, 1, 4, PD), 
image( 2, 2, [PA, PB, PC, PD], I), 
imageCompress(I, I2).

*/
%...................................................................................

% Dominio: (List X Pixel X List X List)
% Descripcion: Cambia un elemento de una lista de pixeles
% Metas secundarias: getPosPix, insertarPrincipio, modificador
modificador([], _, ListaCache, ListaCache).
modificador([PixelOriginal|CDR], PixelAModificar, ListaCache, NewPixeles):-
    getPosPix(PixelOriginal, PosPixelOriginal),
    getPosPix(PixelAModificar, PosPixelAModificar),
    (PosPixelOriginal == PosPixelAModificar ->  
    (insertarPrincipio(PixelAModificar, ListaCache, ListaCache2), modificador(CDR, PixelAModificar, ListaCache2, NewPixeles))
    ;   
    (insertarPrincipio(PixelOriginal, ListaCache, ListaCache2), modificador(CDR, PixelAModificar, ListaCache2, NewPixeles))),
    !.
    
% Dominio: (Image X Pixel X Image)
% Descripcion: Cambia un pixel de una imagen
% Metas secundarias: image, modificador
imageChangePixel(Image, PixelAModificar, NewImage):-
    image(Alto, Largo, Pixeles, Image),
    modificador(Pixeles, PixelAModificar, [], NewPixeles),
    image(Alto, Largo, NewPixeles, NewImage).

/* Ejemplos

pixrgb( 0, 0, 10, 10, 10, 10, P1), 
pixrgb( 0, 1, 20, 20, 20, 20, P2), 
pixrgb( 1, 0, 30, 30, 30, 30, P3), 
pixrgb( 1, 1, 40, 40, 40, 40, P4), 
image( 2, 2, [P1, P2, P3, P4], I1), 
pixrgb( 0, 1, 54, 54, 54, 20, P2_modificado), 
imageChangePixel(I1, P2_modificado, I2).

*/
%...................................................................................

% Dominio: (Pixel X String)
% Descripcion: Cambia un pixel a un string
% Metas secundarias: getContenidoPix, integer, atom_string, unir, atomics_to_string
pixToString(Pixel, StringContenido):-
    getContenidoPix(Pixel, Contenido),
    (integer(Contenido) ->  
    atom_string(Contenido, StringContenido)
    ;   
    (string(Contenido) ->  
    unir(Contenido, StringContenido)
    ;   
    atomics_to_string(Contenido, " ", StringContenido))).

% Dominio: (List X Integer X Integer X String X String)
% Descripcion: Cambia una lista de pixeles en un string
% Metas secundarias: pixToString, atom_concat 
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

% Dominio: (Image X String)
% Descripcion: Transforma una imagen en un string
% Metas secundarias: image, reverse, convPixString
imageToString(Image, String):-
    image(_, Largo, Pixeles, Image),
    reverse(Pixeles, ReversedPix),
    convPixString(ReversedPix, 1, Largo, "", String).