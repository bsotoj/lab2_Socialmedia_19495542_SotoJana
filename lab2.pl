%TDAs
%Fecha
%Usuario
%Publicacion
%Comentario
%Social Network

%-------------------------------------------------------------------------------------------------------
%Implementacion TDAs
%Representacion
/*
Fecha: [Day,Month,Year] => Entero X Entero X Entero
SocialNetwork: [Name,Date,SesionActiva,ListaUsuarios, ListaPublicaciones, ListaComentarios] => String X Date X Entero X ListaUsuarios X ListaPublicaciones X Comentarios
Usuario: [ID,Username,Password,Date,ListaSeguidores] => Entero X String X String X Date X ListaSeguidores
Publicacion: [ID,Date,CantidadvecesCompartidas,Tipocontenido,Contenido,ListaUsernames,PersonasCompartidas,Likes]
                => Entero X Date X Entero X String X String X ListaUsernames X PersonasCompartidas X Likes
Comentario: [CommentID,IDPost,IDcomentarioQueseComenta,[Fecha,TextoContenido],Likes,UserID] => Entero X Entero X Entero X [Fecha X String] X Likes
like: [Fecha,UserID,"Me gusta"]

ListaSeguidores: []; Username X Usernames// String X Usernames
PersonasCompartidas:[Fecha|ListaSeguidores]
Usuarios: []; Usuario X Usuarios
Publicaciones: []; Publicacion X Publicaciones
Comentarios: []; Comentario X Comentarios
likes:[]; like X likes

ListaUsuarios: [LastIdU|Usuarios]
ListaPublicaciones: [LastPostID|Publicaciones]
ListaComentarios: [LastCommentID|Comentarios]


*/
%-------------------------------------------------------------------------------------------------------
/*

Dominios
SocialNetworkIn: SocialNetwork
SocialNetworkOut: SocialNetwork
Day, Month, Year: Entero
N: String
UserID: Entero
Uid: Entero
Elemento: Symbol
ID: Entero
CreadorPost: String
Username, Password: String
Share: PersonasCompartidas
Shares: Lista de PersonasCompartidas
NewShare: PersonasCompartidas
Date: Fecha
F: Fecha
UserFollowers: ListaSeguidores
IDPost: Entero
CantvecesCompartidas: Entero
Tipocontenido: String
Contenido: String
User: Usuario
Post: Publicacion
Ps: Publicaciones
Us: Usuarios
LPid: Entero
UserDate: Fecha
NewDate: Fecha
NewUser: String
NewPassword: String
LIDUser: Entero
LastUser: String
LastDate: Fecha
LastTipoContenido: String
LastContenido: String
LastFollowers: ListaSeguidores
LastShare: PersonasCompartidas
NewLastPostID: Entero
ListaUsuarios: Lista de strings
NuevaListaUsuarios: Lista de strings
Personascompartidas: [Fecha|ListaUsuarios]
ListaUsernamesDest: Lista de strings
Posicion: Entero
Postmodificado: Publicacion
UsuarioModificado: Usuario
NuevoPs: Publicaciones
NuevoUs: Usuarios
StrOut: String
STRuser: String
STRusers: Lista de strings
STRpost: String
STRps: Lista de strings
InfoSTR: String
STRshares: Lista de strings
LastCommentID: Entero
LastPostID: Entero
LastIDcomentarioQueseComenta: Entero
LastContenido: String
NewLastCommentID: Entero
STRlikes: Lista de Strings
CommentID: Entero
Cm: Comentarios
IDComment: Entero
STRcomment: String
STRcms: Lista de Strings
ComentarioModificado: Comentario
NuevoCm: Comentarios

*/

/*
Predicados

fecha(Day,Month,Year)
existeUsuario(UserID,Username,Password,Date,UserFollowers)
seEncuentraenFollowers(ListaUsuarios,ListaUsuarios)
existePost(IDPost,UserID,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,UserFollowers,PersonasCompartidas,Likes,Ps)
getPostbyID(Ps,IDPost,Post)
getUserbyID(Users,UserID,User)
getListbyPosition(Lista,Posicion,Lista)
set_UserFollowersupdate(Lista,ListaUsuarios,Posicion,NuevaListaUsuarios)
set_insertarNuevoshare(Lista,ListaUsuarios,Date,NuevaLista)
set_PostsshareUpdate(Ps,IDPost,Postmodificado,NuevoPs)
set_UsersUpdate(Us,UserID,UsuarioModificado, NuevoUs)
set_ActualizarLista(Lista,Elemento,Posicion,NuevaLista)
userTostring(User,STRuser)
usersToSTR(Us,STRusers)
dirigidos_to_string(ListaUsuarios,STRusers)
compartidosTostring(Share,STRusers)
postTostring(Post,STRpost)
psTostring(Ps,STRps)
usPosttoString(User,STRpost)
userInfotoString(User,InfoSTR)
usPersonasCompartidas(Shares,IDPost,CreadorPost,Username,Contenido,STRshares)
usShareActivity(Post,Username,STRpost)
usPostCompartidos(Ps,STRps)
userPoststoString(Ps,UserID,User,STRps)
perteneceComentarioApublicacion(Cm,CommentID,IDPost)
getCommentbyID(Cm,CommentID,Comentario)
likePosttoString(Likes,IDPost,STRlikes)
likeCommenttoString(Likes,IDComment,STRlikes)
commentTostring(Comentario,STRcomment)
cmTostring(Cm,STRcms)
userCommentstoString(Cm,STRcms)
insertarNuevoLike(Lista,Date,UserID,NuevaLista)
set_CommentsUpdate(Cm,CommentID,ComentarioModificado,NuevoCm).


socialNetworkRegister(SocialNetworkIn,NewDate,NewUser,NewPassword,SocialNetworkOut)
socialNetworkLogin(SocialNetworkIn,Username, Password,SocialNetworkOut)
socialNetworkLogout(SocialNetworkIn,SocialNetworkOut)
socialNetworkPost(SocialNetworkIn,Date,TipoContenido,Contenido,ListaUsuarios, SocialNetworkOut)
socialNetworkFollow(SocialNetworkIn, Username, SocialNetworkOut)
socialNetworkShare(SocialNetworkIn,Date,IDPost, ListaUsuarios, SocialNetworkOut)
socialnetworkToString(SocialNetworkOut, StrOut)
comment(SocialNetworkIn,Date,IDPost,CommentID,Contenido,SocialNetworkOut)
socialNetworkLike(SocialNetworkIn,Date,IDPost,CommentID,SocialNetworkOut)


*/

/*
Metas

Principales:fecha, socialNetwork, socialNetworkRegister, socialNetworkLogin, socialNetworkPost, socialNetworkFollow, socialNetworkShare, socialnetworkToString, comment,socialNetworkLike
Secundarias: existeUsuario, seEncuentraenFollowers, existePost,getPostbyID,getUserbyID,getListbyPosition,set_UserFollowersupdate,set_PostsshareUpdate,set_UsersUpdate,set_ActualizarLista
            userTostring,usersToSTR,dirigidos_to_string,psTostring,usPosttoString,userInfotoString,usPersonasCompartidas,usShareActivity,usPostCompartidos,userPoststoString,
            perteneceComentarioAPublicacion,getCommentbyID,likePosttoString,commentTostring,cmTostring, userCommentstoString,insertarNuevoLike,set_CommentsUpdate
*/

%----------------------------------------Constructores-------------------------------------------------------
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

%cantVecescompartidas,comentarios y likes son para las funciones opcionales
socialNetwork(N,Date,[N,Date,-1,[0],[0],[0]]):- string(N), fecha(_,_,_,Date).
%----------------------------------------Pertenencia-------------------------------------------------------
%existeUsuario(UserID,Username,Password,Date,Listafollowers,Users)
existeUsuario(UserID,Username,Password,Date,Listafollowers,[[UserID,Username,Password,Date,Listafollowers] |_]).
existeUsuario(UserID,Username,Password,Date,Listafollowers,[_|Us]):- existeUsuario(UserID,Username,Password,Date,Listafollowers,Us).


%seEncuentraenFollowers(UsuariosAverificar,ListaVerificar)
%esto es para el post dirigido a otros usuarios
seEncuentraenFollowers([],_).
seEncuentraenFollowers([Head|Tail],ListaVerificar):- member(Head,ListaVerificar),
    seEncuentraenFollowers(Tail,ListaVerificar).


%existePost(IDPost,UserID,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,UserFollowers,PersonasCompartidas,Likes,Ps)
existePost(IDPost,UserID,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,UserFollowers,Personascompartidas,Likes,

           [[IDPost,UserID,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,UserFollowers,Personascompartidas,Likes]|_]).

existePost(IDPost,UserID,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,UserFollowers,Personascompartidas,Likes,[_|Ps]):-

    existePost(IDPost,UserID,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,UserFollowers,Personascompartidas,Likes,Ps).

%comentario dirigido hacia otro comentario
%perteneceComentarioApublicacion([CommentID,PostID,_,_,_,_],CommentID,PostID)
perteneceComentarioAPublicacion([[CommentID,PostID,_,_,_,_]|_],CommentID,PostID).
perteneceComentarioAPublicacion([_|Cm],CommentID,PostID):-
      perteneceComentarioAPublicacion(Cm,CommentID,PostID).

%----------------------------------------Selectores-------------------------------------------------------
%getPostbyID(Ps,IDPost,Post)
    getPostbyID([[IDPost,UserID,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,UserFollowers,Personascompartidas,Likes]|_],IDPost,[IDPost,UserID,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,UserFollowers,Personascompartidas,Likes]):- !.

    getPostbyID([_|Ps],IDPost,Post):-

        getPostbyID(Ps,IDPost,Post).

%getUserbyID(Users,UserID,User)
%usuario = [ID,username,password,date,listaSeguidores(contactos)]
%[[5,mako],[4,bastian],[3,pedro],[2,pablo],[1,juan]]
%id: 3 -> [3,pedro]

getUserbyID([[UserID,Username,Password,Date,UserFollowers]|_],UserID,[UserID,Username,Password,Date,UserFollowers]):- !.

getUserbyID([_|Tail],UserID,Usuario):-

    getUserbyID(Tail,UserID,Usuario).




%getListbyPosition(Lista,Posicion,Lista)
%getListbyPosition([[5,mako,[]],[4,bastian],[3,pedro],[2,pablo],[1,juan]],5,R).
%R:[1,juan]
getListbyPosition([Cabeza|_],1,Cabeza).

getListbyPosition([_|Cola],Posicion,Resultado):-

            PosicionSiguiente is Posicion-1,

            getListbyPosition(Cola,PosicionSiguiente,Resultado).


getCommentbyID([[CommentID,IDPost,IDcomentarioQueseComenta,Contenido,Likes,UserID]|_],CommentID,[CommentID,IDPost,IDcomentarioQueseComenta,Contenido,Likes,UserID]):- !.
  getCommentbyID([_|Tail],CommentID,Comentario):-
  getCommentbyID(Tail,CommentID,Comentario).
%----------------------------------------Modificadores-------------------------------------------------------
%set_UserFollowersupdate(Lista,ListaUsuarios,Posicion,NuevaListaUsuarios)
set_UserFollowersupdate([Cabeza|Cola],ListaUsuarios,1,[Nuevalista|Cola]):-

    append(Cabeza,ListaUsuarios,Nuevalista).


set_UserFollowersupdate([Cabeza|Cola],ListaUsuarios,Posicion,[Cabeza|Resultado]):-

    PosicionSiguiente is Posicion-1,

    set_UserFollowersupdate(Cola,ListaUsuarios,PosicionSiguiente,Resultado).

%set_insertarNuevoshare(Lista,ListaUsuarios,Date,NuevaLista)
	set_insertarNuevoshare(Lista,ListaUsuarios,Date,[Nuevalista|Lista]):-

    append([Date],[ListaUsuarios],Nuevalista).


%set_PostsshareUpdate(Ps,IDPost,Postmodificado,NuevoPs)
set_PostsshareUpdate([[IDPost,_,_,_,_,_,_,_,_,_]|Cola],IDPost,Postmodificado,[Postmodificado|Cola]).

  set_PostsshareUpdate([[ID,UserID,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,Listafollowers,Personascompartidas,Likes]|Cola],IDPost,Postmodificado,[[ID,UserID,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,Listafollowers,Personascompartidas,Likes]|NuevoPs]):-

    set_PostsshareUpdate(Cola,IDPost,Postmodificado,NuevoPs).



    %usuario = [ID,username,password,date,listaSeguidores(contactos)]
    %set_UsersUpdate(Us,UserID,UsuarioModificado, NuevoUs)
set_UsersUpdate([[UserID,_,_,_,_]|Cola],UserID,UsuarioModificado,[UsuarioModificado|Cola]).

set_UsersUpdate([[ID,Username,Password,Date,Listafollowers]|Cola],UserID,UsuarioModificado,[[ID,Username,Password,Date,Listafollowers]|NuevoUs]):-

    set_UsersUpdate(Cola,UserID,UsuarioModificado,NuevoUs).




    %para modificar el TDA SocialNetwork
    %set_ActualizarLista(Lista,Elemento,Posicion,NuevaLista)
    set_ActualizarLista([_|Colalista],Elemento,1,[Elemento|Colalista]).

    set_ActualizarLista([Cabezalista|Colalista],Elemento,Posicion,[Cabezalista|Resultado]):- ContPosicion is Posicion-1,

    	set_ActualizarLista(Colalista,Elemento,ContPosicion,Resultado).

      %Publicacion: [ID,Date,CantidadvecesCompartidas,Tipocontenido,Contenido,ListaUsernames,PersonasCompartidas,Likes] -> 8
      %Comentario: [CommentID,IDPost,IDcomentarioQueseComenta,[Fecha,TextoContenido],Likes] => 5


  set_CommentsUpdate([[CommentID,_,_,_,_,_]|Cola],CommentID,ComentarioModificado,[ComentarioModificado|Cola]).
  set_CommentsUpdate([[CmID,IDPost,IDcomentarioQueseComenta,Contenido,Likes,UserID]|Cola],CommentID,ComentarioModificado,[[CmID,IDPost,IDcomentarioQueseComenta,Contenido,Likes,UserID]|NuevoCm]):-
      set_CommentsUpdate(Cola,CommentID,ComentarioModificado,NuevoCm).

%----------------------------------------Otras operaciones-------------------------------------------------------

%Predicados para trabajar socialnetworkToString
%userTostring(User,STRuser)
userTostring([_,Username,_,_,Listafollowers],STRuser):-

    	atomics_to_string([Username,"Sigue a: "], '\n' , UserStrRepr),

        atomics_to_string(Listafollowers,'\n', FollowersStrRepr),

        atomics_to_string([UserStrRepr, FollowersStrRepr],'\n', STRuser).

%----------------------------------------------------------------------------------------------------------------------------------------
%usersToSTR(Us,STRusers)
usersToSTR([], []) :- !.

usersToSTR([UserActual|UserSiguiente],[StruserActual|StruserSgte]):-

    userTostring(UserActual,StrUser),

    string_concat(StrUser,"\n",StruserActual),

    usersToSTR(UserSiguiente,StruserSgte).
%----------------------------------------------------------------------------------------------------------------------------------------
%dirigidos_to_string(ListaUsuarios,STRusers)
dirigidos_to_string([],[]):- !.

dirigidos_to_string([PersonaActual|PersonasSgtes],[StrPersonaActual|StrPersonaSgte]):-

  string_concat(PersonaActual," ",StrPersonaActual),

  dirigidos_to_string(PersonasSgtes,StrPersonaSgte).
%-----------------------------------------------------------------------------------------------------------------------------------------
%compartidosTostring(Share,STRusers)
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
%likesToString(LikeActual,IDPost,STR)
likePosttoString([],_,[]):- !.
likePosttoString([[Date,UserID,_]|LikeSgte],IDPost,[STRlikeActual|STRlikeSgte]):-
  atomics_to_string(Date,'/',DateSTR),
  atomics_to_string(["El dia",DateSTR,", el usuario con ID:",UserID,"reacciono a la publicacion con ID:",IDPost],' ', STRlike),
  string_concat(STRlike,"\n",STRlikeActual),
  likePosttoString(LikeSgte,IDPost,STRlikeSgte).

  likeCommenttoString([],_,[]):- !.
  likeCommenttoString([[Date,UserID,_]|LikeSgte],IDComment,[STRlikeActual|STRlikeSgte]):-
    atomics_to_string(Date,'/',DateSTR),
    atomics_to_string(["El dia",DateSTR,", el usuario con ID:",UserID,"reacciono al comentario con ID:",IDComment],' ', STRlike),
    string_concat(STRlike,"\n",STRlikeActual),
    likeCommenttoString(LikeSgte,IDComment,STRlikeSgte).

%----------------------------------------------------------------------------------------------------------------------------------------
%postTostring(Post,STRpost)
%psTostring
%like: [Fecha,UserID,"Me gusta"]
%likesToString
postTostring([ID,_,Username,Date,_,_,Mensaje,Destinatarios,Compartidos,Likes],STRpost):-

  string_concat("Publicacion con ID:",ID,IDstr),

  atomics_to_string(Date,'/',Datestr),

  atomics_to_string(["El dia",Datestr,Username,"publico\n",Mensaje],' ',Fechastr),

  atomics_to_string(Destinatarios,' ', DSTR),

  atomics_to_string(["Destinatarios:",DSTR],' ', DestinatariosStr),

  string_concat("Compartidos:"," ",CompartidoStr),

  compartidosTostring(Compartidos,CompartidosSTR),

  atomics_to_string(CompartidosSTR,' ', ShareSTR),

  string_concat("La publicacion ha recibido me gusta:", " ", LikesSTR ),

  likePosttoString(Likes,ID,LikesToString),

  atomics_to_string(LikesToString, ' ', LKsSTR),

  atomic_list_concat([IDstr, Fechastr,DestinatariosStr,CompartidoStr,ShareSTR,LikesSTR,LKsSTR],'\n',STRpost).

  %-------------------------------------------------------------------------------------------------------------------
  %ToString con sesion inactiva

  %comentario a publicacion
  commentTostring([CommentID,IDPost,0,[Fecha,TextoContenido],Likes,UserID],STRcomment):-
    atomics_to_string(Fecha,'/',Datestr),
    atomics_to_string(["ID Usuario que comenta:",UserID, "," ,"ID comentario: ",CommentID,"\n","El dia ",Datestr,"se comenta la publicacion con ID:",IDPost,"\n",TextoContenido,"\n"],' ',STR),
    string_concat(STR,"\n",STRActual),
    likeCommenttoString(Likes,CommentID,STRlike),
    atomics_to_string(STRlike,' ',LiketoSTR),
    atomic_list_concat([STRActual,LiketoSTR],' ',STRcomment).

  %comentario a comentario
  commentTostring([CommentID,IDPost,IDcomentarioQueseComenta,[Fecha,TextoContenido],Likes,UserID],STRcomment):-
    IDcomentarioQueseComenta > 0,
    atomics_to_string(Fecha,'/',Datestr),
    atomics_to_string(["ID Usuario que comenta:",UserID, "," ,"ID comentario: ",CommentID,"\n","El dia ",Datestr,"se comenta la publicacion con ID:",IDPost,"en particular el comentario con ID:",IDcomentarioQueseComenta,"\n",TextoContenido,"\n"],' ',STR),
    string_concat(STR,"\n",STRActual),
    likeCommenttoString(Likes,CommentID,STRlike),
    atomics_to_string(STRlike,' ',LiketoSTR),
    atomic_list_concat([STRActual,LiketoSTR],' ',STRcomment).

  %comentario: (CommentID,IDPost,IDcomentarioQueseComenta,[Fecha,TextoContenido],Like,UserID)
  cmTostring([],[]):- !.
  cmTostring([CommentActual|CommentSgte],[STRcmActual|STRcmSgte]):-
    commentTostring(CommentActual,STRcomment),
    string_concat(STRcomment,'\n',STRcmActual),
    cmTostring(CommentSgte,STRcmSgte).

  %-----
%--------------------------------------------------------------------------------------------------------------------------------------
%psTostring(Ps,STRps)
psTostring([],[]):- !.

psTostring([PostActual|PostSgte],[StrpostActual|StrpostSgte]):-

  postTostring(PostActual,STRpost),

  string_concat(STRpost,'\n',StrpostActual),

  psTostring(PostSgte,StrpostSgte).
%--------------------------------------------------------------------------------------------------------------------------------------
%ToString para un usuario en especifico
%usPosttoString(User,STRpost)
%  usPosttoString

usPosttoString([ID,_,Username,Date,_,_,Mensaje,Destinatarios,_,Likes],STRpost):-

    string_concat("ID:",ID,IDstr),

    atomics_to_string(Date,'/',Datestr),

    atomics_to_string(["El dia",Datestr,"el usuario",Username, "publico\n",Mensaje],' ',Fechastr),

    atomics_to_string(Destinatarios,' ', DSTR),

    atomics_to_string(["Destinatarios:",DSTR],' ', DestinatariosStr),

    string_concat("Personas que le dieron like a la publicacion:","\n", LikesSTR),

    likePosttoString(Likes,ID,STRlikes),

    atomics_to_string(STRlikes,' ', LKsSTR),

    atomic_list_concat([IDstr,Fechastr,DestinatariosStr,LikesSTR,LKsSTR],'\n',STRpost).



%[id,user,pass,date,followers]
%userInfotoString(User,InfoSTR)
userInfotoString([ID,Username,_,Date,_],STRinfo):-

    string_concat("ID:",ID,IDstr),

    atomics_to_string(Date,'/', Datestr),

    string_concat(Datestr,'\n',NewDateSTR),

    atomics_to_string([IDstr,"Nombre de usuario:",Username,"Fecha de creacion de cuenta:",NewDateSTR],

                      '\n',STRinfo).



%[2, 2, "camilo", [9, 9, 9999], 0, "photo", "primer post a amigos", ["juan", "pedro"], [[[9, 8, 3001], ["camilo", "pedro"]], [[22, 6, 2021], ["camilo", "juan", "pedro"]]]
%ESTO ES PARA EL SHARE CUANDO UID > 0


%usPersonasCompartidas(Shares,IDPost,CreadorPost,Username,Contenido,STRshares)
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
%usShareActivity(Post,Username,STRpost)
usShareActivity([IDPost,_,CreadorPost,_,_,_,Mensaje,_,Compartidos,_],Username,STRpost):-

    usPersonasCompartidas(Compartidos,IDPost,CreadorPost,Username,Mensaje,CompartidosSTRlist),

    atomics_to_string(CompartidosSTRlist,'\n',STRpost).





%[id,user,pass,date,followers]
%usPostCompartidos(Ps,STRps)
usPostCompartidos([],_,[]):- !.

usPostCompartidos([PostActual|PostSgte],[_,Username,_,_,_],[StrpostActual|StrpostSgte]):-

    usShareActivity(PostActual,Username,STRpost),

    string_concat(STRpost,'\n',StrpostActual),

    usPostCompartidos(PostSgte,[_,Username,_,_,_],StrpostSgte).



%----------------------------------------------------------------------------------------------------------------------------------

%----------------------------------------------------------------------------------------------------------------------------------
%userPoststoString
%userPoststoString(Ps,UserID,User,STRps)
userPoststoString([],_,[]):- !.



userPoststoString([[ID,Uid,Username,Date,_,_,Mensaje,Destinatarios,_,Likes]|PostSgte],[Uid,Username,_,_,_],[StrpostActual|StrpostSgte]):-

    usPosttoString([ID,Uid,Username,Date,_,_,Mensaje,Destinatarios,_,Likes],STRpost),

    string_concat(STRpost,'\n',StrpostActual),

    userPoststoString(PostSgte,[Uid,Username,_,_,_],StrpostSgte).



userPoststoString([_|Ps],[Uid,Username,_,_,_],StrPs):-

    userPoststoString(Ps,[Uid,Username,_,_,_],StrPs).



%ToString para los comentarios de un usuario con sesion activa
%Comentario: [CommentID,IDPost,IDcomentarioQueseComenta,[Fecha,TextoContenido],Likes,UserID]

userCommentstoString([],_,[]):- !.
userCommentstoString([[CommentID,IDPost,IDcomentarioQueseComenta,[Fecha,TextoContenido],Likes,UserID]|CmSgte],[UserID,_,_,_,_],[STRcmActual|STRcmSgte]):-
    commentTostring([CommentID,IDPost,IDcomentarioQueseComenta,[Fecha,TextoContenido],Likes,UserID],STRcomment),
    string_concat(STRcomment,'\n',STRcmActual),
    userCommentstoString(CmSgte,[UserID,_,_,_,_],STRcmSgte).

userCommentstoString([_|Cms],[UserID,_,_,_,_],STRcm):-
  userCommentstoString(Cms,[UserID,_,_,_,_],STRcm).




%like: [Fecha,UserID,"Me gusta"]
insertarNuevoLike(Lista,Date,UserID,[Nuevalista|Lista]):-
  append([Date],[UserID,"Me gusta"],Nuevalista).





%-------------------------------------------------Requerimientos obligatorios-------------------------------
%-----------------------------------------------------------------------------------------------------------
%-------------------------------------------------------REGISTER--------------------------------------------



%socialNetworkRegister(SocialNetworkIn,NewD,NU,NP,SocialNetworkOut)

socialNetworkRegister([N,Date,-1,[0],Ps,Cm],NewDate,NewUser,NewPassword,[N,Date,-1,[1,[1,NewUser,NewPassword,NewDate,[]]],Ps,Cm]).

socialNetworkRegister([N,Date,-1, [LIDUser,[LIDUser,Username,Password,UserDate,Listafollowers]|Us], Ps, Cm],NewDate,NewUser,NewPassword,[N,Date,-1,[NewLastUserID,[NewLastUserID,NewUser,NewPassword,NewDate,[]],[LIDUser,Username,Password,UserDate,Listafollowers]|Us],Ps,Cm]):-

not(existeUsuario(_,NewUser,_,_,_,Us)),

NewLastUserID is LIDUser + 1,

not(NewUser = Username).

%---------------------------------------------------LOGIN------------------------------------------------------------

%socialNetworkLogin(SocialNetworkIn,U,P,SocialNetworkOut)

socialNetworkLogin([N,Date,-1,Us,Ps,Cm],Username,Password,[N,Date,UserID,Us,Ps,Cm]):- existeUsuario(UserID,Username,Password,_,_,Us).

socialNetworkLogout([N, Date, UserID, Us, Ps, Cm],[N, Date, -1, Us, Ps, Cm]):-UserID > -1.


%-----------------------------------------------POST-------------------------------------------------------------

%ENCABEZADO

%socialNetworkPost(Sn1, Fecha, Texto, ListaUsernamesDest,Sn2).

%publicacion = [ID,IDUser,Username,date,cantVecescompartidas,tipoContenido,contenido,listaUsernames,personasCompartidas,likes]

%caso ListaUsernamesDest = []

%socialNetworkPost([N,D,Uid,Us,[0],Cm],F,TipoT,T,LU,)




%CASO -> CUANDO ES DIRIGIDO HACIA EL MISMO

%CUANDO ES EL PRIMER POST

socialNetworkPost([N,Date,UserID,[LIDUser|Us],[0],Cm],F,Tipocontenido,Contenido,[],[N,Date,-1,[LIDUser|Us],[1,[1,UserID,Username,F,0,Tipocontenido,Contenido,["Todos"],[],[]]],Cm]):-

UserID > 0,

string(Tipocontenido),

string(Contenido),

fecha(_,_,_,F),

fecha(_,_,_,Date),

existeUsuario(UserID,_,_,_,_,Us),

getUserbyID(Us,UserID,[_,Username,_,_,_]).

%[ID,Username,Password,Date,Followers]

%existeUsuario(Uid,U,P,D,UF,Users)

%CUANDO YA EXISTEN POST

socialNetworkPost([N,Date,UserID,[Lid|Us],[LPid,[LPid,LIDUser,LastUser,LastDate,CantidadvecesCompartidas,LastTipoContenido,LastContenido,LastFollowers,LastShare,LL]|Ps],Cm],F,TipoContenido,Contenido,[],[N,Date,-1,[Lid|Us],[NewLastPostID,[NewLastPostID,UserID,Username,F,0,TipoContenido,Contenido,["Todos"],[],[]],[LPid,LIDUser,LastUser,LastDate,CantidadvecesCompartidas,LastTipoContenido,LastContenido,LastFollowers,LastShare,LL]|Ps],Cm]):-

  UserID > 0,

  NewLastPostID is LPid + 1,

  string(TipoContenido),

  string(Contenido),

  fecha(_,_,_,F),

  fecha(_,_,_,Date),

  existeUsuario(UserID,_,_,_,_,Us),

  getUserbyID(Us,UserID,[_,Username,_,_,_]).


%------------------------------------------------------------------------------------
%CASO -> CUANDO ES DIRIGIDO A USUARIOS

%PRIMER POST

%seEncuentraenFollowers(Usuario,ListaVerificar)

%socialNetworkPost(Sn1, Fecha, Texto, ListaUsernamesDest,Sn2).
socialNetworkPost([N,Date,UserID,[Lid|Us],[0],Cm],F,TipoContenido,Contenido,ListaUsuarios,[N,Date,-1,[Lid|Us],[1,[1,UserID,Username,F,0,TipoContenido,Contenido,ListaUsuarios,[],[]]],Cm]):-

  UserID > 0,

  existeUsuario(UserID,_,_,_,_,Us),

  getUserbyID(Us,UserID,[_,Username,_,_,Followers]),

  seEncuentraenFollowers(ListaUsuarios,Followers).




%CUANDO YA EXISTEN POST
socialNetworkPost([N,Date,UserID,[Lid|Us],[LPid,[LPid,LIDUser,LastUser,LastDate,CantidadvecesCompartidas,LastTipoContenido,LastContenido,LastFollowers,LastShare,LL]|Ps],Cm],F,TipoContenido,Contenido,ListaUsuarios,[N,Date,-1,[Lid|Us],[NLPid,[NLPid,UserID,Username,F,0,TipoContenido,Contenido,ListaUsuarios,[],[]],[LPid,LIDUser,LastUser,LastDate,CantidadvecesCompartidas,LastTipoContenido,LastContenido,LastFollowers,LastShare,LL]|Ps],Cm]):-

  UserID > 0,

  NLPid is LPid + 1,

  existeUsuario(UserID,_,_,_,_,Us),

  getUserbyID(Us,UserID,[_,Username,_,_,Followers]),

  seEncuentraenFollowers(ListaUsuarios,Followers).
%------------------------------------------------------FOLLOW------------------------------------------------------------------------------
socialNetworkFollow([N,Date,UserID,[Lid|Us],Ps,Cm],Username,SOut):-

  UserID > 0,

  existeUsuario(_,Username,_,_,_,Us),

  getUserbyID(Us,UserID,Usuario),

  set_UserFollowersupdate(Usuario,[Username],5,UsuarioModificado),

  set_UsersUpdate(Us,UserID,UsuarioModificado,NewUsers),

  set_ActualizarLista([N,Date,UserID,[Lid|Us],Ps,Cm],[Lid|NewUsers],4,Salida),

  socialNetworkLogout(Salida,SOut).
  %--------------------------------------------------SHARE-----------------------------------------------------------------------------------


  %publicacion = [ID,Uid,Username,date,cantVecescompartidas,tipoContenido,contenido,listaUsernames,personasCompartidas,likes]
  %dirigido a el mismo


socialNetworkShare([N,D,UserID,Us,[LPid|Ps],Cm],F,IDPost,[],SOut):-

    UserID > 0,

    getUserbyID(Us,UserID,[_,Username,_,_,_]),

    existePost(IDPost,_,_,_,_,_,_,_,_,_,Ps),

    getPostbyID(Ps,IDPost,[IDPost,ID,U,Fecha,CantidadvecesCompartidas,TipoContenido,Contenido,ListaUsuarios,PersonasCompartidas,L]),

    set_insertarNuevoshare(PersonasCompartidas,[Username,"Todos"],F,NuevoShare),

    set_ActualizarLista([IDPost,ID,U,Fecha,CantidadvecesCompartidas,TipoContenido,Contenido,ListaUsuarios,PersonasCompartidas,L],NuevoShare,9,PostActualizado),

    set_PostsshareUpdate(Ps,IDPost,PostActualizado,Newposts),

    set_ActualizarLista([N,D,UserID,Us,[LPid|Ps],Cm],[LPid|Newposts],5,Salida),

    socialNetworkLogout(Salida,SOut).







%dirigido a usuarios
socialNetworkShare([N,D,UserID,Us,[LPid|Ps],Cm],F,IDPost,ListaUsernamesDest,SOut):-
    %date: F
    UserID > 0,

     existeUsuario(UserID,_,_,_,_,Us),

    getUserbyID(Us,UserID,[_,Username,_,_,Followers]),

    seEncuentraenFollowers(ListaUsernamesDest,Followers),

    existePost(IDPost,_,_,_,_,_,_,_,_,_,Ps),

    getPostbyID(Ps,IDPost,[IDPost,ID,U,Fecha,CantidadvecesCompartidas,TipoContenido,Contenido,ListaUsuarios,PersonasCompartidas,L]),

    append([Username],ListaUsernamesDest,NuevosUsuarios),

    set_insertarNuevoshare(PersonasCompartidas,NuevosUsuarios,F,NuevoShare),

    set_ActualizarLista([IDPost,ID,U,Fecha,CantidadvecesCompartidas,TipoContenido,Contenido,ListaUsuarios,PersonasCompartidas,L],NuevoShare,9,PostActualizado),

    set_PostsshareUpdate(Ps,IDPost,PostActualizado,Newposts),

    set_ActualizarLista([N,D,UserID,Us,[LPid|Ps],Cm],[LPid|Newposts],5,Salida),

    socialNetworkLogout(Salida,SOut).

%--------------------------------------------------ToString-----------------------------------------------------------------------------------
socialnetworkToString([Name,Date,-1,[_|Users],[_|Posts],[_|Comments]],StrOut):-

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

    %---------------COMENTARIOS----------
    cmTostring(Comments,CmsListStr),
    atomic_list_concat(CmsListStr,'',CmsAtom),
    atom_string(CmsAtom,CmsStr),
    atomics_to_string([PresentacionSTR,UsRegistrados,UsersStr,"\n-----------------\n",

                        "***Publicaciones***\n",PostsStr,"\n***Fin Publicaciones***\n",

                        "\n-----------------\n","\n-----------------\n",

                                            "***Comentarios***\n",CmsStr,"\n***Fin Comentarios***\n",

                                            "\n-----------------\n"],'',StrOut).



%user con sesion activa

 socialnetworkToString([Name,Date,Uid,[_|Users],[_|Posts],[_|Comments]],StrOut):-

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

    atom_string(ShareAtom,SharetoSTR),

    %comentarios
    userCommentstoString(Comments,User,CommentsListStr),

    atomic_list_concat(CommentsListStr,'',CommentsAtom),

    atom_string(CommentsAtom,CmSTR),


    atomics_to_string([PresentacionSTR,UsRegistrados,InfoSTR,UserStr,"\n-----------------\n","***Publicaciones***\n",

                        PostsStr,"\n***Fin Publicaciones***\n","\n-----------------\n","***Publicaciones compartidas***\n",

                        SharetoSTR,"\n***Fin Publicaciones compartidas***\n","\n-----------------\n","\n-----------------\n","***Comentarios***\n",

                                            CmSTR,"\n***Fin Comentarios***\n"],'',StrOut).
%--------------------------------------------------Comment-----------------------------------------------------------------------------------
%existePost(IDPost,UserID,Username,Date,CantvecesCompartidas,Tipocontenido,Contenido,UserFollowers,PersonasCompartidas,Likes,Ps)
%primer comentario -> dirigido a una publicacion
%comment(Sn1,Fecha,PostID,CommentID,TextoContenido,Sn2)
%comentario: (CommentID,IDPost,IDcomentarioQueseComenta,[Fecha,TextoContenido],Like,UserID)
comment([N,D,UserID,Us,Ps,[0]],Fecha,IDPost,0,Contenido,[N,D,-1,Us,Ps,[1,[1,IDPost,0,[Fecha,Contenido],[],UserID]]]):-
    existePost(IDPost,_,_,_,_,_,_,_,_,_,Ps),
    UserID > 0,
    fecha(_,_,_,Fecha),
    string(Contenido).

%dirigido a una publicacion
comment([N,D,UserID,Us,Ps,[LastCommentID,[LastCommentID,LastPostID,LastIDcomentarioQueseComenta,LastContenido,LastLike,LastUserID]|Cm]], Fecha, IDPost, 0, Contenido, [N,D,-1,Us,Ps,[NewLastCommentID,[NewLastCommentID,IDPost,0,[Fecha,Contenido],[],UserID],[LastCommentID,LastPostID,LastIDcomentarioQueseComenta,LastContenido,LastLike,LastUserID]|Cm]]):-
    NewLastCommentID is LastCommentID + 1,
    existePost(IDPost,_,_,_,_,_,_,_,_,_,Ps),
    UserID > 0,
    fecha(_,_,_,Fecha),
    string(Contenido).

%comentario dirigido a otro comentario

comment([N,D,UserID,Us,Ps,[LastCommentID|Cm]],Fecha,IDPost,CommentID,Contenido,[N,D,-1,Us,Ps,[NewLastCommentID,[NewLastCommentID,IDPost,CommentID,[Fecha,Contenido],[],UserID]|Cm]]):-
    UserID > 0,
    NewLastCommentID is LastCommentID + 1,
existePost(IDPost,_,_,_,_,_,_,_,_,_,Ps),
    UserID > 0,
    fecha(_,_,_,Fecha),
    string(Contenido),
    perteneceComentarioAPublicacion(Cm,CommentID,IDPost).

%--------------------------------------------------socialNetworkLike-----------------------------------------------------------------------------------
%like a un comentario proveniente de una publicacion
%Comentario: [CommentID,IDPost,IDcomentarioQueseComenta,[Fecha,TextoContenido],Likes]
%publicacion = [ID,IDUser,Username,date,cantVecescompartidas,tipoContenido,contenido,listaUsernames,personasCompartidas,likes]

%like: [Fecha,UserID,"Me gusta"]

socialNetworkLike([N,D,UserID,Us,Ps,[LastCommentID|Cm]],Fecha,IDPost,CommentID,SocialNetworkOut):-
  UserID > 0,
  perteneceComentarioAPublicacion(Cm,CommentID,IDPost),
  getCommentbyID(Cm,CommentID,Comentario),
  getListbyPosition(Comentario,5,LikesDelComentario),
  insertarNuevoLike(LikesDelComentario,Fecha,UserID,NuevosLikes),
  set_ActualizarLista(Comentario,NuevosLikes,5,ComentarioActualizado),
  set_CommentsUpdate(Cm,CommentID,ComentarioActualizado,CmActualizado),
  set_ActualizarLista([N,D,UserID,Us,Ps,[LastCommentID|Cm]], [LastCommentID|CmActualizado],6,Salida),
  socialNetworkLogout(Salida,SocialNetworkOut).

%like a una publicacion
socialNetworkLike([N,D,UserID,Us,[LPid|Ps],Cm],Fecha,IDPost,0,SocialNetworkOut):-
  UserID > 0,
  getPostbyID(Ps,IDPost,Post),
  getListbyPosition(Post,10,LikesDelaPublicacion),
  insertarNuevoLike(LikesDelaPublicacion,Fecha,UserID,NuevosLikes),
  set_ActualizarLista(Post,NuevosLikes,10,PostActualizado),
  set_PostsshareUpdate(Ps,IDPost,PostActualizado,NuevoPs),
  set_ActualizarLista([N,D,UserID,Us,[LPid|Ps],Cm],[LPid|NuevoPs],5,Salida),
  socialNetworkLogout(Salida,SocialNetworkOut).

  %-----------------------------------------EJEMPLOS---------------------------------------------------------------------------------------------
  %Si se desea ejecutar en SWISH--SWI-PROLOG se debe copiar y pegar la base de conocimientos, luego para hacer la consulta se copia y pega el ejemplo completo y se selecciona la opccion "run!"
/*
EJEMPLO 1
fecha(20,5,2000,F),socialNetwork("failbok",F,Sn1),
fecha(13,4,2001,FechaUsuario1),
fecha(4,1,2001,FechaUsuario2),
fecha(5,2,2001,FechaUsuario3),
socialNetworkRegister(Sn1,FechaUsuario1,"hugo","contrasena1",Sn2),
socialNetworkRegister(Sn2,FechaUsuario2,"paco","contrasena2",Sn3),
socialNetworkRegister(Sn3,FechaUsuario3,"luis","contrasena3",Sn4),
socialNetworkLogin(Sn4,"paco","contrasena2",Sn5),
socialNetworkFollow(Sn5,"luis",Sn6),
socialNetworkLogin(Sn6,"hugo","contrasena1",Sn7),
fecha(10,7,2005,FechaPost1),
socialNetworkPost(Sn7,FechaPost1,"video","miPrimerPost",[],Sn8),
fecha(9,8,2005,FechaShare1),
socialNetworkLogin(Sn8,"paco","contrasena2",Sn9),
socialNetworkShare(Sn9,FechaShare1,1,[],Sn10),
fecha(1,1,2004,FechaComment1),
fecha(1,1,2004,FechaLike1),
socialNetworkLogin(Sn10,"luis","contrasena3",Sn11),
comment(Sn11,FechaComment1,1,0,"Este es un comentario sobre la publicacion con id 1", Sn12),
socialNetworkLogin(Sn12,"luis","contrasena3",Sn13),
socialNetworkLike(Sn13,FechaLike1,1,0,Sn14),
socialnetworkToString(Sn14,SOut),write(SOut).

EJEMPLO 2

fecha(20,5,2000,F),socialNetwork("failbok",F,Sn1),
fecha(13,4,2001,FechaUsuario1),
fecha(4,1,2001,FechaUsuario2),
fecha(5,2,2001,FechaUsuario3),
socialNetworkRegister(Sn1,FechaUsuario1,"hugo","contrasena1",Sn2),
socialNetworkRegister(Sn2,FechaUsuario2,"paco","contrasena2",Sn3),
socialNetworkRegister(Sn3,FechaUsuario3,"luis","contrasena3",Sn4),
socialNetworkLogin(Sn4,"paco","contrasena2",Sn5),
socialNetworkFollow(Sn5,"hugo",Sn6),
socialNetworkLogin(Sn6,"paco","contrasena2",Sn7),
socialNetworkFollow(Sn7,"luis",Sn8),
socialNetworkLogin(Sn8,"paco","contrasena2",Sn9),
fecha(15,1,2000,FechaPost1),
socialNetworkPost(Sn9,FechaPost1,"photo","primer post a amigos",["hugo","luis"],Sn10),
socialNetworkLogin(Sn10,"paco","contrasena2",Sn11),
fecha(30,1,2003,FechaShare1),
socialNetworkShare(Sn11,FechaShare1,1,["luis","hugo"],Sn12),
socialNetworkLogin(Sn12,"luis","contrasena3",Sn13),
fecha(30,2,2002,FechaPost2),
socialNetworkPost(Sn13,FechaPost2,"video","segundo post",[],Sn14),
socialNetworkLogin(Sn14,"hugo","contrasena1",Sn15),
fecha(1,1,2005,FechaComment1),
comment(Sn15,FechaComment1,2,0,"Este es un comentario sobre la publicacion con id 2", Sn16),
socialNetworkLogin(Sn16,"paco","contrasena2",Sn17),
fecha(2,2,2005,FechaLike1),
socialNetworkLike(Sn17,FechaLike1,2,1,Sn18),
socialNetworkLogin(Sn18,"paco","contrasena2",Sn19),
socialnetworkToString(Sn19,SOut),write(SOut).

EJEMPLO 3

fecha(15,1,1998,FechaUsuario1),
fecha(30,1,1996,FechaUsuario2),
fecha(12,1,2001,FechaUsuario3),
fecha(2,2,2005,FechaPost1),
fecha(2,1,2002,FechaPost2),
fecha(4,5,2004,FechaPost3),
fecha(21,6,2007,FechaShare1),
fecha(22,6,2007,FechaShare2),
fecha(9,8,2010,FechaShare3),
fecha(29,6,2011,FechaComment1),
fecha(25,6,2011,FechaComment2),
fecha(1,1,2012,FechaLike1),
fecha(3,1,2012,FechaLike2),
fecha(24, 5, 2021, F), socialNetwork("failbok", F, Sn1),
socialNetworkRegister(Sn1,FechaUsuario1, "pedro", "pass1",Sn2),
socialNetworkRegister(Sn2,FechaUsuario2,"camilo","pass2",Sn3),
socialNetworkRegister(Sn3,FechaUsuario3,"juan","pass3",Sn4),
socialNetworkLogin(Sn4,"camilo","pass2",Sn5),
socialNetworkFollow(Sn5,"juan",Sn6),
socialNetworkLogin(Sn6,"camilo","pass2",Sn7),
socialNetworkFollow(Sn7,"pedro",Sn8),
socialNetworkLogin(Sn8,"camilo","pass2",Sn9),
socialNetworkPost(Sn9,FechaPost1,"video","miPrimerPost",[],Sn10),
socialNetworkLogin(Sn10,"camilo","pass2",Sn11),
socialNetworkPost(Sn11,FechaPost2,"photo","primer post a amigos", ["juan","pedro"],Sn12),
socialNetworkLogin(Sn12,"camilo","pass2",Sn13),
socialNetworkPost(Sn13,FechaPost3,"video","segundo post a amigos", ["pedro"], Sn14),
socialNetworkLogin(Sn14,"camilo","pass2",Sn15),
socialNetworkShare(Sn15,FechaShare1,2,[],Sn16),
socialNetworkLogin(Sn16,"camilo","pass2",Sn17),
socialNetworkShare(Sn17,FechaShare2,1,["juan","pedro"],Sn18),
socialNetworkLogin(Sn18,"camilo","pass2",Sn19),
socialNetworkShare(Sn19,FechaShare3,1,["pedro"],Sn20),
socialNetworkLogin(Sn20,"camilo","pass2",Sn21),
comment(Sn21,FechaComment1,2,0,"Este es un comentario sobre la publicacion con id 2" ,Sn22),
socialNetworkLogin(Sn22,"camilo","pass2",Sn23),
comment(Sn23,FechaComment2,2,1,"Este es un comentario sobre la publicacion con id 2, en particular sobre el comentario con id 1",Sn24),
socialNetworkLogin(Sn24,"camilo","pass2",Sn25),
socialNetworkLike(Sn25,FechaLike1,1,0,Sn26),
socialNetworkLogin(Sn26,"camilo","pass2",Sn27),
socialNetworkLike(Sn27,FechaLike2,2,1,Sn28),
socialNetworkLogin(Sn28,"camilo","pass2",Sn29),
socialnetworkToString(Sn29,SOut),write(SOut).

--> ESTE ES EL EJEMPLO 3 PERO AHORA SOCIALNETWORKTOSTRING MUESTRA TODA LA RED SOCIAL

fecha(15,1,1998,FechaUsuario1),
fecha(30,1,1996,FechaUsuario2),
fecha(12,1,2001,FechaUsuario3),
fecha(2,2,2005,FechaPost1),
fecha(2,1,2002,FechaPost2),
fecha(4,5,2004,FechaPost3),
fecha(21,6,2007,FechaShare1),
fecha(22,6,2007,FechaShare2),
fecha(9,8,2010,FechaShare3),
fecha(29,6,2011,FechaComment1),
fecha(25,6,2011,FechaComment2),
fecha(1,1,2012,FechaLike1),
fecha(3,1,2012,FechaLike2),
fecha(24, 5, 2021, F), socialNetwork("failbok", F, Sn1),
socialNetworkRegister(Sn1,FechaUsuario1, "pedro", "pass1",Sn2),
socialNetworkRegister(Sn2,FechaUsuario2,"camilo","pass2",Sn3),
socialNetworkRegister(Sn3,FechaUsuario3,"juan","pass3",Sn4),
socialNetworkLogin(Sn4,"camilo","pass2",Sn5),
socialNetworkFollow(Sn5,"juan",Sn6),
socialNetworkLogin(Sn6,"camilo","pass2",Sn7),
socialNetworkFollow(Sn7,"pedro",Sn8),
socialNetworkLogin(Sn8,"camilo","pass2",Sn9),
socialNetworkPost(Sn9,FechaPost1,"video","miPrimerPost",[],Sn10),
socialNetworkLogin(Sn10,"camilo","pass2",Sn11),
socialNetworkPost(Sn11,FechaPost2,"photo","primer post a amigos", ["juan","pedro"],Sn12),
socialNetworkLogin(Sn12,"camilo","pass2",Sn13),
socialNetworkPost(Sn13,FechaPost3,"video","segundo post a amigos", ["pedro"], Sn14),
socialNetworkLogin(Sn14,"camilo","pass2",Sn15),
socialNetworkShare(Sn15,FechaShare1,2,[],Sn16),
socialNetworkLogin(Sn16,"camilo","pass2",Sn17),
socialNetworkShare(Sn17,FechaShare2,1,["juan","pedro"],Sn18),
socialNetworkLogin(Sn18,"camilo","pass2",Sn19),
socialNetworkShare(Sn19,FechaShare3,1,["pedro"],Sn20),
socialNetworkLogin(Sn20,"camilo","pass2",Sn21),
comment(Sn21,FechaComment1,2,0,"Este es un comentario sobre la publicacion con id 2" ,Sn22),
socialNetworkLogin(Sn22,"camilo","pass2",Sn23),
comment(Sn23,FechaComment2,2,1,"Este es un comentario sobre la publicacion con id 2, en particular sobre el comentario con id 1",Sn24),
socialNetworkLogin(Sn24,"camilo","pass2",Sn25),
socialNetworkLike(Sn25,FechaLike1,1,0,Sn26),
socialNetworkLogin(Sn26,"camilo","pass2",Sn27),
socialNetworkLike(Sn27,FechaLike2,2,1,Sn28),
socialnetworkToString(Sn28,SOut),write(SOut).
*/
