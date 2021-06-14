%fecha = [dia,mes,anio]
%fecha(Day,Month,Year)
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
%existeUsuario(Uid,U,P,D,UF,Users)
existeUsuario(Uid,U,P,D,UF,[[Uid,U,P,D,UF] |_]).
existeUsuario(Uid,U,P,D,UF,[_|Us]):- existeUsuario(Uid,U,P,D,UF,Us).

searchUser(Uid,U,P,D,UF,[[Uid,U,P,D,UF]|_], [Uid,U,P,D,UF]).
searchUser(Uid,U,P,D,UF,[_|Us],User):- searchUser(Uid,U,P,D,UF,Us,User).
%--------------------------------------------------------------------------------------------------------------------------
%listaFollowers(Usuario,ListaVerificar)
%esto es para el post dirigido a otros usuario
listaFollowers([],_).
listaFollowers([Head|Tail],ListaVerificar):- member(Head,ListaVerificar),
    listaFollowers(Tail,ListaVerificar).

%---------------------------------------------------------PREDICADOS QUE AUN NO HAN SIDO ORDENADOS PERO QUE FUNCIONAN---------------------------------------
    %orden
    %usuario
    %contactos
    %usuario
    %usuarios
    %tda completo

    %selectores
    %usuario = [ID,username,password,date,listaSeguidores(contactos)]
    %funciona
    %[[5,mako],[4,bastian],[3,pedro],[2,pablo],[1,juan]]
    %id: 3 -> [3,pedro]
    getUserbyID([[ID,Username,Password,Date,Followers]|_],ID,[ID,Username,Password,Date,Followers]):- !.
    getUserbyID([_|Tail],ID,Usuario):-
        getUserbyID(Tail,ID,Usuario).


    %funciona
    %getListbyPosition([[5,mako,[]],[4,bastian],[3,pedro],[2,pablo],[1,juan]],5,R).
    %R:[1,juan]
    getListbyPosition([Cabeza|_],1,Cabeza).
    getListbyPosition([_|Cola],Posicion,Resultado):-
        PosicionSiguiente is Posicion-1,
        getListbyPosition(Cola,PosicionSiguiente,Resultado).

    %modificadores
    %set por posicion -> listo
    %set por id -> listo


    %funciona
    %[5,mako,["juan"]]
    %siempre se elige como posicion la 5
    %["pedro","carlos","david"]

    set_UserFollowersupdate([Cabeza|Cola],Nuevosseguidores,1,[Nuevalista|Cola]):-
        append(Cabeza,Nuevosseguidores,Nuevalista).

    set_UserFollowersupdate([Cabeza|Cola],Nuevosseguidores,Posicion,[Cabeza|Resultado]):-
        PosicionSiguiente is Posicion-1,
        set_UserFollowersupdate(Cola,Nuevosseguidores,PosicionSiguiente,Resultado).

    %funciona
    %modificador los usuarios con el user con los followers agregados
    %modificador([[IDU,_,_]|Cola],IDU,Elemento,[Elemento|Cola]).
    %modificador([[ID,Nombre,Followers]|Cola],IDU,Elemento,[[ID,Nombre,Followers]|Resultado]):-
    %    modificador(Cola,IDU,Elemento,Resultado).

    %usuario = [ID,username,password,date,listaSeguidores(contactos)]
    set_UsersUpdate([[IDU,_,_,_,_]|Cola],IDU,UsuarioModificado,[UsuarioModificado|Cola]).
    set_UsersUpdate([[ID,Username,Password,Date,Followers]|Cola],IDU,UsuarioModificado,[[ID,Username,Password,Date,Followers]|Resultado]):-
        set_UsersUpdate(Cola,IDU,UsuarioModificado,Resultado).

    %para modificar el TDA SocialNetwork
    set_ActualizarLista([_|Colalista],Elemento,1,[Elemento|Colalista]).
    set_ActualizarLista([Cabezalista|Colalista],Elemento,Posicion,[Cabezalista|Resultado]):- ContPosicion is Posicion-1,
    	set_ActualizarLista(Colalista,Elemento,ContPosicion,Resultado).
%--------------------------------------------------------------------------------------------------------------------------------------------
%-------------------------------------------------------REGISTER--------------------------------------------------
%socialNetwork(N,Date,SocialNetwork)
socialNetwork(N,Date,[N,Date,-1,[0],[0],[0]]):- string(N), fecha(_,_,_,Date).

%encabezado
%socialNetworkRegister(SocialNetworkIn,NewD,NU,NP,SocialNetworkOut)

socialNetworkRegister([N,D,-1,[0],Ps,Cm],NewD,NU,NP,[N,D,-1,[1,[1,NU,NP,NewD,[]]],Ps,Cm]).
socialNetworkRegister([N,D,-1, [Lid,[Lid,U,P,UD,UF]|Us], Ps, Cm],NewD,NU,NP,[N,D,-1,[NLid,[NLid,NU,NP,NewD,[]],[Lid,U,P,UD,UF]|Us],Ps,Cm]):-
not(existeUsuario(_,NU,_,_,_,Us)),
NLid is Lid + 1,
not(NU = U).


%---------------------------------------------------LOGIN------------------------------------------------------------
%socialNetworkLogin(SocialNetworkIn,U,P,SocialNetworkOut)
socialNetworkLogin([N,D,-1,Us,Ps,Cm],U,P,[N,D,Uid,Us,Ps,Cm]):- existeUsuario(Uid,U,P,_,_,Us).
socialNetworkLogout([N, D, UId, Us, Ps, Cm],[N, D, -1, Us, Ps, Cm]):-UId > -1.
%-----------------------------------------------POST-------------------------------------------------------------
%ENCABEZADO
%socialNetworkPost(Sn1, Fecha, Texto, ListaUsernamesDest,Sn2).
%publicacion = [ID,Username,date,cantVecescompartidas,tipoContenido,contenido,listaUsernames,personasCompartidas,likes]
%caso ListaUsernamesDest = []
%socialNetworkPost([N,D,Uid,Us,[0],Cm],F,TipoT,T,LU,)
%CASO -> CUANDO ES DIRIGIDO HACIA EL MISMO
%CUANDO ES EL PRIMER POST
socialNetworkPost([N,D,Uid,[Lid|Us],[0],Cm],F,TipoT,T,[],[N,D,-1,[Lid|Us],[1,[1,U,F,0,TipoT,T,["dirigido a el mismo"],[0],[0]]],Cm]):-
Uid > 0,
string(TipoT),
string(T),
fecha(_,_,_,F),
fecha(_,_,_,D),
existeUsuario(Uid,_,_,_,_,Us),
searchUser(Uid,_,_,_,_,Us,[_,U,_,_,_]).
%existeUsuario(Uid,U,P,D,UF,Users)
%CUANDO YA EXISTEN POST

socialNetworkPost([N,D,Uid,[Lid|Us],[LPid,[LPid,LUser,LD,CS,LTT,LT,LF,LS,LL]|Ps],Cm],F,TipoT,T,[],[N,D,-1,[Lid|Us],[NLPid,[NLPid,U,F,0,TipoT,T,["dirigido a el mismo"],[0],[0]],[LPid,LUser,LD,CS,LTT,LT,LF,LS,LL]|Ps],Cm]):-
  Uid > 0,
  NLPid is LPid + 1,
  string(TipoT),
  string(T),
  fecha(_,_,_,F),
  fecha(_,_,_,D),
  existeUsuario(Uid,_,_,_,_,Us),
  searchUser(Uid,_,_,_,_,Us,[_,U,_,_,_]).

%CASO -> CUANDO ES DIRIGIDO A USUARIOS
%socialNetworkPost([N,D,Uid,[Lid|Us],[LPid,[LPid,LP]|Ps],Cm],F,TipoT,T,LU,[N,D,-1,[Lid|Us],[NLPid,[NLPid,U,F,0,TipoT,T,LU,[0],[0]],[LPid,LP]|Ps],Cm]):-
%  Uid > 0,
%  NLPid is LPid + 1,
%  existeUsuario(Uid,U,_,_,_,Us),
%  searchUser(Uid,U,_,_,UF,Us,_),
%  listaFollowers(LU,UF).

%------------------------------------------------------FOLLOW------------------------------------------------------------------------------

%encabezado socialNetworkFollow(Sn1, Username, Sn2).
socialNetworkFollow([N,D,Uid,[Lid|Us],Ps,Cm],Username,SOut):-
  Uid > 0,
  existeUsuario(_,Username,_,_,_,Us),
  existeUsuario(Uid,_,_,_,_,Us),
  getUserbyID(Us,Uid,Usuario),
  set_UserFollowersupdate(Usuario,[Username],5,UsuarioModificado),
  set_UsersUpdate(Us,Uid,UsuarioModificado,NewUsers),
  set_ActualizarLista([N,D,Uid,[Lid|Us],Ps,Cm],[Lid|NewUsers],4,Salida),
  socialNetworkLogout(Salida,SOut).
