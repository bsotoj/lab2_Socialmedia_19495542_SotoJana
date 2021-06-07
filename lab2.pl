%fecha = [dia,mes,anio]
fecha(Day,Month,Year,[Day,Month,Year]):- number(Day),number(Month),number(Year), Day > 0,
    Month > 0, Year > 0, Day =< 31, Month =< 12.


%socialnetwork=[nombre,fecha,sesionActiva,usuarios,publicaciones,comentarios]
%comentarios es para utilizarse en funciones opcionales
%se usa como referencia el tda visto en clases del profesor Roberto Gonzalez (vespertino)

%usuario = [ID,username,password,date,listaSeguidores(contactos)]
%usuarios= [IDultimoU,usuario1,usuario2,...]

%publicacion = [ID,date,cantVecescompartidas,tipoContenido,contenido,listaUsernames,personasCompartidas,likes]
%publicaciones= [IDultimoP,publicacion1,publicacion2,.....]

%cantVecescompartidas y likes son para las funciones opcionales
%comentario= [IDPost,ID,[ID,contenido,likes]]
%---------------------------------------------------------------------------------------------------------------------------
existeUsuario(Uid,U,P,D,UF,[[Uid,U,P,D,UF] |_]).
existeUsuario(Uid,U,P,D,UF,[_|Us]):- existeUsuario(Uid,U,P,D,UF,Us).



%--------------------------------------------------------------------------------------------------------------------------
socialNetwork(N,Date,[N,Date,-1,[0],[0],["comentarios"]]):- string(N), fecha(_,_,_,Date).

%encabezado
%socialNetworkRegister(S1, “2021-05-01”, “user”, “pass”, S2)

socialNetworkRegister([N,D,-1,[0],Ps,Cm],NewD,NU,NP,[N,D,-1,[1,[1,NU,NP,NewD,["seguidores"]]],Ps,Cm]).
socialNetworkRegister([N,D,-1, [Lid,[Lid,U,P,UD,UF]|Us], Ps, Cm],NewD,NU,NP,[N,D,-1,[NLid,[NLid,NU,NP,NewD,["seguidores"]],[Lid,U,P,UD,UF]|Us],Ps,Cm]):-
not(existeUsuario(_,NU,_,_,_,Us)),
NLid is Lid + 1,
not(NU = U).
