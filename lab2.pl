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

%searchUser(Uid,U,P,D,UF,[[Uid,U,P,D,UF]|_], [Uid,U,P,D,UF]).
%searchUser(Uid,U,P,D,UF,[_|Us],User):- searchUser(Uid,U,P,D,UF,Us,User).
%--------------------------------------------------------------------------------------------------------------------------
%seEncuentraenFollowers(UsuariosAverificar,ListaVerificar)
%esto es para el post dirigido a otros usuario
seEncuentraenFollowers([],_).
seEncuentraenFollowers([Head|Tail],ListaVerificar):- member(Head,ListaVerificar),
    seEncuentraenFollowers(Tail,ListaVerificar).
%----------------------------------------------------------------------------------------------------------------------------------------------------------
%seEncuentraenFollowers(UsuariosAverificar,ListaVerificar)

%esto es para el post dirigido a otros usuario

seEncuentraenFollowers([],_).

seEncuentraenFollowers([Head|Tail],ListaVerificar):- member(Head,ListaVerificar),

    seEncuentraenFollowers(Tail,ListaVerificar).





%agregar a atom

existePost(IDPost,Uid,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,Listafollowers,Personascompartidas,Likes,

           [[IDPost,Uid,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,Listafollowers,Personascompartidas,Likes]|_]).

existePost(IDPost,Uid,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,Listafollowers,Personascompartidas,Likes,[_|Ps]):-

    existePost(IDPost,Uid,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,Listafollowers,Personascompartidas,Likes,Ps).

%agregar a atom

getPostbyID([[IDPost,Uid,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,Listafollowers,Personascompartidas,Likes]|_],IDPost,[IDPost,Uid,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,Listafollowers,Personascompartidas,Likes]):- !.

getPostbyID([_|Ps],IDPost,Post):-

    getPostbyID(Ps,IDPost,Post).
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

%--------------------------------------------------------------------------------------------------------------------------------------------
%---------------------------ESTO ES DEL POST---------------------------------

    %[ID,Uid,date,cantVecescompartidas,tipoContenido,contenido,listaUsernames,personasCompartidas,likes]

    %funciona

	%[[22, 6, 2021], "camilo", "juan", "pedro", [9, 8, 3001], "camilo", "pedro"]





    %set_insertarNuevoshare([],Personasdirigidas,Date,Nuevalista):-

    %append([Date],Personasdirigidas,Nuevalista).

	set_insertarNuevoshare(Lista,Personasdirigidas,Date,[Nuevalista|Lista]):-

    append([Date],[Personasdirigidas],Nuevalista).



	set_PostsshareUpdate([[IDPost,_,_,_,_,_,_,_,_,_]|Cola],IDPost,Postmodificado,[Postmodificado|Cola]).

    set_PostsshareUpdate([[ID,Uid,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,Listafollowers,Personascompartidas,Likes]|Cola],IDPost,Postmodificado,[[ID,Uid,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,Listafollowers,Personascompartidas,Likes]|Resultado]):-

    	set_PostsshareUpdate(Cola,IDPost,Postmodificado,Resultado).



%[2, 2, "camilo", [9, 9, 9999], 0, "photo", "primer post a amigos", ["juan", "pedro"], [[[21, 6, 2021], "camilo", "Todos"], 0], [0]],

%-------------------------------------------------------------------------

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

%-----------------------------------------------------------------------------------------------------------------
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

%publicacion = [ID,IDUser,Username,date,cantVecescompartidas,tipoContenido,contenido,listaUsernames,personasCompartidas,likes]

%caso ListaUsernamesDest = []

%socialNetworkPost([N,D,Uid,Us,[0],Cm],F,TipoT,T,LU,)





%CASO -> CUANDO ES DIRIGIDO HACIA EL MISMO

%CUANDO ES EL PRIMER POST

socialNetworkPost([N,D,Uid,[Lid|Us],[0],Cm],F,TipoT,T,[],[N,D,-1,[Lid|Us],[1,[1,Uid,U,F,0,TipoT,T,["Todos"],[],["likes"]]],Cm]):-

Uid > 0,

string(TipoT),

string(T),

fecha(_,_,_,F),

fecha(_,_,_,D),

existeUsuario(Uid,_,_,_,_,Us),

getUserbyID(Us,Uid,[_,U,_,_,_]).



%[ID,Username,Password,Date,Followers]

%existeUsuario(Uid,U,P,D,UF,Users)

%CUANDO YA EXISTEN POST



socialNetworkPost([N,D,Uid,[Lid|Us],[LPid,[LPid,LIDUser,LUser,LD,CS,LTT,LT,LF,LS,LL]|Ps],Cm],F,TipoT,T,[],[N,D,-1,[Lid|Us],[NLPid,[NLPid,Uid,U,F,0,TipoT,T,["Todos"],[],["likes"]],[LPid,LIDUser,LUser,LD,CS,LTT,LT,LF,LS,LL]|Ps],Cm]):-

  Uid > 0,

  NLPid is LPid + 1,

  string(TipoT),

  string(T),

  fecha(_,_,_,F),

  fecha(_,_,_,D),

  existeUsuario(Uid,_,_,_,_,Us),

  getUserbyID(Us,Uid,[_,U,_,_,_]).



%CASO -> CUANDO ES DIRIGIDO A USUARIOS

%PRIMER POST

%seEncuentraenFollowers(Usuario,ListaVerificar)

%socialNetworkPost(Sn1, Fecha, Texto, ListaUsernamesDest,Sn2).

socialNetworkPost([N,D,Uid,[Lid|Us],[0],Cm],F,TipoT,T,LU,[N,D,-1,[Lid|Us],[1,[1,Uid,U,F,0,TipoT,T,LU,[],["likes"]]],Cm]):-

  Uid > 0,

  existeUsuario(Uid,_,_,_,_,Us),

  getUserbyID(Us,Uid,[_,U,_,_,Followers]),

  seEncuentraenFollowers(LU,Followers).



%CUANDO YA EXISTEN POST

socialNetworkPost([N,D,Uid,[Lid|Us],[LPid,[LPid,LIDUser,LUser,LD,CS,LTT,LT,LF,LS,LL]|Ps],Cm],F,TipoT,T,LU,[N,D,-1,[Lid|Us],[NLPid,[NLPid,Uid,U,F,0,TipoT,T,LU,[],["likes"]],[LPid,LIDUser,LUser,LD,CS,LTT,LT,LF,LS,LL]|Ps],Cm]):-

  Uid > 0,

  NLPid is LPid + 1,

  existeUsuario(Uid,_,_,_,_,Us),

  getUserbyID(Us,Uid,[_,U,_,_,Followers]),

  seEncuentraenFollowers(LU,Followers).





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

  getUserbyID(Us,Uid,Usuario),

  set_UserFollowersupdate(Usuario,[Username],5,UsuarioModificado),

  set_UsersUpdate(Us,Uid,UsuarioModificado,NewUsers),

  set_ActualizarLista([N,D,Uid,[Lid|Us],Ps,Cm],[Lid|NewUsers],4,Salida),

  socialNetworkLogout(Salida,SOut).


  %--------------------------------------------------SHARE-----------------------------------------------------------------------------------


  %publicacion = [ID,Uid,Username,date,cantVecescompartidas,tipoContenido,contenido,listaUsernames,personasCompartidas,likes]



  %cuando tiene usuarios

  socialNetworkShare([N,D,Uid,Us,[LPid|Ps],Cm],Date,IDPost,[],SOut):-

      Uid > 0,

      getUserbyID(Us,Uid,[_,Username,_,_,_]),

      existePost(IDPost,_,_,_,_,_,_,_,_,_,Ps),

      getPostbyID(Ps,IDPost,[IDPost,Uid,U,Fecha,Cvc,TT,T,LU,PC,L]),

      set_insertarNuevoshare(PC,[Username,"Todos"],Date,NuevoShare),

      set_ActualizarLista([IDPost,Uid,U,Fecha,Cvc,TT,T,LU,PC,L],NuevoShare,9,PostActualizado),

      set_PostsshareUpdate(Ps,IDPost,PostActualizado,Newposts),

      set_ActualizarLista([N,D,Uid,Us,[LPid|Ps],Cm],[LPid|Newposts],5,Salida),

      socialNetworkLogout(Salida,SOut).







  socialNetworkShare([N,D,Uid,Us,[LPid|Ps],Cm],Date,IDPost,ListaUsernamesDest,SOut):-

      Uid > 0,

       existeUsuario(Uid,_,_,_,_,Us),

      getUserbyID(Us,Uid,[_,Username,_,_,Followers]),

      seEncuentraenFollowers(ListaUsernamesDest,Followers),

      existePost(IDPost,_,_,_,_,_,_,_,_,_,Ps),

      getPostbyID(Ps,IDPost,[IDPost,Uid,U,Fecha,Cvc,TT,T,LU,PC,L]),

      append([Username],ListaUsernamesDest,NuevosUsuarios),

      set_insertarNuevoshare(PC,NuevosUsuarios,Date,NuevoShare),

      set_ActualizarLista([IDPost,Uid,U,Fecha,Cvc,TT,T,LU,PC,L],NuevoShare,9,PostActualizado),

      set_PostsshareUpdate(Ps,IDPost,PostActualizado,Newposts),

      set_ActualizarLista([N,D,Uid,Us,[LPid|Ps],Cm],[LPid|Newposts],5,Salida),

      socialNetworkLogout(Salida,SOut).

%-----------------------------------------------------------------------------------------------------------------------------------------
%SOCIAL NETWORK TO STRING

userTostring([_,Username,_,_,ListaSeguidores],STRuser):-

    	atomics_to_string([Username,"Sigue a: "], '\n' , UserStrRepr),

        atomics_to_string(ListaSeguidores,'\n', FollowersStrRepr),

        atomics_to_string([UserStrRepr, FollowersStrRepr],'\n', STRuser).

%----------------------------------------------------------------------------------------------------------------------------------------
usersToSTR([], []) :- !.

usersToSTR([UserActual|UserSiguiente],[StruserActual|StruserSgte]):-

    userTostring(UserActual,StrUser),

    string_concat(StrUser,"\n",StruserActual),

    usersToSTR(UserSiguiente,StruserSgte).
%----------------------------------------------------------------------------------------------------------------------------------------
dirigidos_to_string([],[]):- !.

dirigidos_to_string([PersonaActual|PersonasSgtes],[StrPersonaActual|StrPersonaSgte]):-

  string_concat(PersonaActual," ",StrPersonaActual),

  dirigidos_to_string(PersonasSgtes,StrPersonaSgte).
%-----------------------------------------------------------------------------------------------------------------------------------------
compartidosTostring([],[]):- !.

compartidosTostring([[Date,[Creador|Dirigidos]]|ShareSgte],[ShrActualSTR|ShrSgteSTR]):-

  atomics_to_string(Date,'/',Datestr),

  atomics_to_string(["El dia",Datestr,"por",Creador,"hacia:"],' ',Fechastr),

  dirigidos_to_string(Dirigidos,DirigidosStr),

  atomics_to_string(DirigidosStr,',',StrDirigidos),

  atomics_to_string([Fechastr,StrDirigidos],'\n', STR),

  string_concat(STR,"\n",ShrActualSTR),

  compartidosTostring(ShareSgte,ShrSgteSTR).
%----------------------------------------------------------------------------------------------------------------------------------------
postTostring([ID,_,Username,Date,_,_,Mensaje,Destinatarios,Compartidos,_],STRpost):-

  string_concat("ID:",ID,IDstr),

  atomics_to_string(Date,'/',Datestr),

  atomics_to_string(["El dia",Datestr,Username,"publico\n",Mensaje],' ',Fechastr),

  atomics_to_string(Destinatarios,' ', DSTR),

  atomics_to_string(["Destinatarios:",DSTR],' ', DestinatariosStr),

  string_concat("Compartidos:"," ",CompartidoStr),

  compartidosTostring(Compartidos,CompartidosSTR),

  atomics_to_string(CompartidosSTR,' ', ShareSTR),

  atomic_list_concat([IDstr, Fechastr,DestinatariosStr,CompartidoStr,ShareSTR],'\n',STRpost).
%--------------------------------------------------------------------------------------------------------------------------------------
psTostring([],[]):- !.

psTostring([PostActual|PostSgte],[StrpostActual|StrpostSgte]):-

  postTostring(PostActual,STRpost),

  string_concat(STRpost,'\n',StrpostActual),

  psTostring(PostSgte,StrpostSgte).
%--------------------------------------------------------------------------------------------------------------------------------------
%ToString para un usuario en especifico

usPosttoString([ID,_,Username,Date,_,_,Mensaje,Destinatarios,_,_],STRpost):-

    string_concat("ID:",ID,IDstr),

    atomics_to_string(Date,'/',Datestr),

    atomics_to_string(["El dia",Datestr,"el usuario",Username, "publico\n",Mensaje],' ',Fechastr),

    atomics_to_string(Destinatarios,' ', DSTR),

    atomics_to_string(["Destinatarios:",DSTR],' ', DestinatariosStr),

    atomic_list_concat([IDstr,Fechastr,DestinatariosStr],'\n',STRpost).



%[id,user,pass,date,followers]

%userInfotoString(User,InfoSTR),

userInfotoString([ID,Username,_,Date,_],STRinfo):-

    string_concat("ID:",ID,IDstr),

    atomics_to_string(Date,'/', Datestr),

    string_concat(Datestr,'\n',NewDateSTR),

    atomics_to_string([IDstr,"Nombre de usuario:",Username,"Fecha de creacion de cuenta:",NewDateSTR],

                      '\n',STRinfo).



%[2, 2, "camilo", [9, 9, 9999], 0, "photo", "primer post a amigos", ["juan", "pedro"], [[[9, 8, 3001], ["camilo", "pedro"]], [[22, 6, 2021], ["camilo", "juan", "pedro"]]]



%ESTO ES PARA EL SHARE CUANDO UID > 0



usPersonasCompartidas([],_,_,_,_,[]):- !.

usPersonasCompartidas([[Date,[Username|PersonasCompartidas]]|CompartidosSgte],IDPost,CreadorPost,Username,Mensaje,[StrCompartidoActual|StrCompartidoSgte]):-

    string_concat("ID:",IDPost,IDstr),

    atomics_to_string(Date,'/',Datestr),

    dirigidos_to_string(PersonasCompartidas,PersonasCompartidasStr),

    atomics_to_string(PersonasCompartidasStr,',',StrPersonasCompartidas),

    atomics_to_string(["El dia",Datestr,"compartio con", StrPersonasCompartidas, "la publicacion con",IDstr,"de",CreadorPost,":",

                        Mensaje],' ',StrCompartidoActual),



    usPersonasCompartidas(CompartidosSgte,IDPost,CreadorPost,Username,Mensaje,StrCompartidoSgte).



usPersonasCompartidas([_|CompartidosSgte],IDPost,CreadorPost,Username,Mensaje,STR):-

    usPersonasCompartidas(CompartidosSgte,IDPost,CreadorPost,Username,Mensaje,STR).





%publicacion = [ID,IDUser,Username,date,cantVecescompartidas,tipoContenido,contenido,listaUsernames,personasCompartidas,likes]



usShareActivity([IDPost,_,CreadorPost,_,_,_,Mensaje,_,Compartidos,_],Username,STRpost):-

    usPersonasCompartidas(Compartidos,IDPost,CreadorPost,Username,Mensaje,CompartidosSTRlist),

    atomics_to_string(CompartidosSTRlist,'\n',STRpost).





%[id,user,pass,date,followers]

usPostCompartidos([],_,[]):- !.

usPostCompartidos([PostActual|PostSgte],[_,Username,_,_,_],[StrpostActual|StrpostSgte]):-

    usShareActivity(PostActual,Username,STRpost),

    string_concat(STRpost,'\n',StrpostActual),

    usPostCompartidos(PostSgte,[_,Username,_,_,_],StrpostSgte).



%----------------------------------------------------------------------------------------------------------------------------------

%----------------------------------------------------------------------------------------------------------------------------------

userPoststoString([],_,[]):- !.



userPoststoString([[ID,Uid,Username,Date,_,_,Mensaje,Destinatarios,_,_]|PostSgte],[Uid,Username,_,_,_],[StrpostActual|StrpostSgte]):-

    usPosttoString([ID,Uid,Username,Date,_,_,Mensaje,Destinatarios,_,_],STRpost),

    string_concat(STRpost,'\n',StrpostActual),

    userPoststoString(PostSgte,[Uid,Username,_,_,_],StrpostSgte).



userPoststoString([_|Ps],Uid,User,StrPs):-

    userPoststoString(Ps,Uid,User,StrPs).
%----------------------------------------------------------------------------------------------------------------------------------
socialnetworkToString([Name,Date,-1,[_|Users],[_|Posts],_],StrOut):-

    atomics_to_string(["#####","Red Social",Name,"#####"],' ', NameSTR),

    atomics_to_string(Date,'/', DateSTR),

    string_concat(DateSTR,"\n", NewDate),

    atomics_to_string([NameSTR,NewDate],'\n', PresentacionSTR),



    string_concat("***Usuarios Registrados***","\n",UsRegistrados),

    usersToSTR(Users,UsersListStr),

    atomic_list_concat(UsersListStr,'',UsersAtom),

   atom_string(UsersAtom,UsersStr),

    psTostring(Posts,PostsListStr),

    atomic_list_concat(PostsListStr,'',PostsAtom),

    atom_string(PostsAtom,PostsStr),

    atomics_to_string([PresentacionSTR,UsRegistrados,UsersStr,"\n-----------------\n",

                        "***Publicaciones***\n",PostsStr,"\n***Fin Publicaciones***\n",

                        "\n-----------------\n"],'',StrOut).



%user con sesion activa

 socialnetworkToString([Name,Date,Uid,[_|Users],[_|Posts],_],StrOut):-

    Uid > 0,

    atomics_to_string(["#####","Red Social",Name,"#####"],' ', NameSTR),

    atomics_to_string(Date,'/', DateSTR),

    string_concat(DateSTR,"\n", NewDate),

    atomics_to_string([NameSTR,NewDate],'\n', PresentacionSTR),



    string_concat("***Usuario con sesion iniciada***","\n",UsRegistrados),

    getUserbyID(Users,Uid,User),

    %usuario y sus seguidores

    userInfotoString(User,InfoSTR),

    userTostring(User,UserStr),

    %publicaciones

    %[id,user,pass,date,followers]

    userPoststoString(Posts,User,PostsListStr),

    atomic_list_concat(PostsListStr,'',PostsAtom),

    atom_string(PostsAtom,PostsStr),

    usPostCompartidos(Posts,User,ShareListStr),

    atomic_list_concat(ShareListStr,'',ShareAtom),

    atomics_to_string([PresentacionSTR,UsRegistrados,InfoSTR,UserStr,"\n-----------------\n","***Publicaciones***\n",

                        PostsStr,"\n***Fin Publicaciones***\n","\n-----------------\n","***Publicaciones compartidas***\n",

                        ShareAtom,"\n***Fin Publicaciones compartidas***\n","\n-----------------\n"],'',StrOut).
