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
% reverse(Lista, Lista) 														(aridad = 2)
% inversor(Lista, ListaCache, ListaFinal)										(aridad = 3)

% Metas priarias: insertarPrincipio, index, unir, frecuenciaElementoEnLista, getPosPix,
% 				  getContenidoPix, reverse.

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

% Dominio: (Pixel X List)
% Descripcion: Obtiene el contenido de cualquier tipo de pixel en forma de lista
% Metas secundarias: pixbit, pixrgb, pixhex
getContenidoPix(Pixel, Contenido):-
    (pixbit(_, _, Bit, _, Pixel) ->  unir(Bit, Contenido)
    ;   
    (pixrgb(_, _, R, G, B, _, Pixel) ->  unir([R,G,B], Contenido)
    ;   
    (pixhex(_, _, HEX, _, Pixel) ->  unir(HEX, Contenido)))).

% Dominio: (Atom X List X Integer X Integer)
% Descripcion: Contar la frecuencia de un elemento en una lista
frecuenciaElementoEnLista(_, [], Contador, Contador).
frecuenciaElementoEnLista(Elemento, [CAR|CDR], Contador, Resultado):-
    (Elemento == CAR -> 
    (NewContador is Contador + 1, frecuenciaElementoEnLista(Elemento, CDR, NewContador, Resultado))
    ;
    frecuenciaElementoEnLista(Elemento, CDR, Contador, Resultado)).

% Dominio: (Integer X Atom X List)
% Descripcion: Encontrar un elemento de una lista buscado por index
index(0, CAR, [CAR|_]).
index(Contador, Elemento, [_|CDR]):-
    NewContador is Contador - 1,
    index(NewContador, Elemento, CDR).

% Dominio: (Atom X Atom)
% Descripcion: Se igualan 2 elementos
unir(Elemento, Elemento).

% Dominio: (Atom X List X List)
% Descripcion: Insertar un elemento al principio de una lista
insertarPrincipio(Elemento, [], [Elemento]).
insertarPrincipio(Elemento, Lista, [Elemento|Lista]).

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
% Image, NewImage 																:Imagen
% Elemento, CAR 																:Atom
% Lista, ListaCache CDR, Posicion, Contenido 									:List
% Contador, NewContador, PosX, PosY												:Integer
% Pixel																			:PixBIT/RGB/HEX
	

% Precidados:  
% image(Largo, Ancho, Pixeles, [Largo, Ancho, Pixeles])		  						(aridad = 4)
% imageIsBitmap(Image)																(aridad = 1)
% imageIsPixmap(Image)																(aridad = 1)
% imageIsHexmap(Image)																(aridad = 1)
% imageIsCompressed(Image)															(aridad = 1)


% Metas priarias: image, imageIsBitmap, imageIsPixmap, imageIsHexmap, imageIsCompressed

% Metas Secundarias: pixelsAreBitmap, pixelsArePixmap, pixelsAreHexmap, image
% 
% --------------------------CONSTRUCTOR Y PERTENENCIA---------------------------------------------

% Dominio: (Integer X Integer X List X List)
% Descripcion:  Genera una lista con los parametros de la iamgen y tiene
%				un parametro que es la misma lista que se ocupa como "ID"
image(Largo, Ancho, Pixeles, [Largo, Anc	ho, Pixeles]).

% Dominio: (Image)
% Descripcion: Verifica si una imagen tiene pixeles del tipo BIT
% Meta secundaria: pixelsAreBitmap
imageIsBitmap(Image):-
    image(_, _, Pixeles, Image),
    pixelsAreBitmap(Pixeles).

% Dominio: (Image)
% Descripcion: Verifica si una imagen tiene pixeles del tipo RGB
% Meta secundaria: pixelsArePixmap
imageIsPixmap(Image):-
    image(_, _, Pixeles, Image),
    pixelsArePixmap(Pixeles).

% Dominio: (Image)
% Descripcion: Verifica si una imagen tiene pixeles del tipo HEX
% Meta secundaria: pixelsAreHexmap
imageIsHexmap(Image):-
    image(_, _, Pixeles, Image),
    pixelsAreHexmap(Pixeles).

% Dominio: (Image)
% Descripcion: Verifica si una imagen ha sido comprimida
% Meta secundaria: image
imageIsCompressed(Image):-
    image(X, Y, Pixeles, Image),
    length(Pixeles, Largo),
    TotalPixeles is X*Y,
    Largo < TotalPixeles.
% --------------------------------------MODIFICADORES---------------------------------------------

                                                                                                       