:- dynamic(diccionario/1).

% Dado un nombre de archivo que contiene todas las palabras que se quieren
% agregar al diccionario (una por linea), vacia diccionario/1 y agrega
% las definiciones nuevas

cargar(NombreDeArchivo) :-
  retractall(diccionario(_)),
  atom_codes(NombreDeArchivo, Arch),
  open(Arch, read, Str),
  read_file(Str,_),
  close(Str).

read_file(Stream,[]) :- at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    not(at_end_of_stream(Stream)),
    read_line_to_codes(Stream,Codes),
    string_codes(X, Codes),
    assertz(diccionario(X)),
    read_file(Stream,L), !.


% listar mensajes secretos de ejemplo.
ej(1, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado]).
% solo debería ser "la cosa" porque cuadrado != triangulo
ej(2, [rombo, cuadrado, espacio, perro, triangulo, sol, cuadrado]).

ej(3, [rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]).

ej(4, [rombo, espacio, perro, espacio, sol, luna, espacio, estrella, espacio, arbol, espacio, gato]).



% Ejercicio 1 %
% diccionario_lista(-S) %
% Si S esta instanciado indica si S esta en el diccionario %
% Si S no esta instanciado, unifica a S con cada una de las palabras del diccionario %
diccionario_lista(S) :- diccionario(D), string_codes(D, S).



% Ejercicio 2 %
% juntar_con(?L, ?J, ?R) %
% Si L y R están sin instanciar, la función llama a append con los tres parámetros sin instanciar y, como este no recorre bien el espacio, tampoco lo hará juntar_con.
% De modo que, L y R no puede estar sin instanciar al mismo tiempo.
juntar_con([X], _, X).
juntar_con([X|Xs], J, R) :- append(X, [J|R2], R), juntar_con(Xs, J, R2).



% Ejercicio 3 %
% palabras(+S, ?P) %
palabras([], []).
palabras(S, P) :- append(PRE, [espacio|TPRE], S), not(member(espacio, PRE)), palabras(TPRE, P2), append([PRE], P2, P).
palabras(S, [S]) :- not(member(espacio, S)).



% Ejercicio 4 %
asignar_var(Atomo, MI, [(Atomo, _) | MI]) :- not((member((Atomo, _), MI))),!.
asignar_var(Atomo, MI, MI) :- member((Atomo, _), MI).

%testeo
testsEj4:- testAs_v1, testAs_v2, testAs_v3.
testAs_v1:- asignar_var(cuadrado, [], [(cuadrado, _G4012)]).
testAs_v2:- asignar_var(cuadrado, [(rombo, _G4012)], [(cuadrado, _G4013),(rombo, _G4012)]).
testAs_v3:- asignar_var(rombo, [(cuadrado, _G4013),(rombo, _G4012)], [ (cuadrado, _G4013), (rombo, _G4012)]).

%reversibilidad:
%Primer argumento:
%asignar_var(A, [], [(cuadrado, _G4012)]).
%asignar_var(A, [(rombo, _G4012)], [(cuadrado, _G4013),(rombo, _G4012)]).
%asignar_var(A, [(cuadrado, _G4013),(rombo, _G4012)], [ (cuadrado, _G4013), (rombo, _G4012)]).
%La función sin el primer valor instanciado devolverá el valor esperado

%asignar_var(cuadrado, A, [(cuadrado, _G4012)]).
%asignar_var(cuadrado, A, [(cuadrado, _G4013),(rombo, _G4012)]).
%devuelve la lista sin el elemento agregado

%asignar_var(A, B, [(cuadrado, _G4012)]).
%A = cuadrado,
%B = [].

%asignar_var(A,B,C).
%B = C, C = [ (A, _G24)|_G27] ;
%B = C, C = [_G26, (A, _G24)|_G30] ;
%B = C, C = [_G26, _G29, (A, _G24)|_G33] ;
%B = C, C = [_G26, _G29, _G32, (A, _G24)|_G36] ;
%...
%este caso es medio turbio, B no tendría que asociar con C...

%asignar_var(?A,?B,?C)

% Ejercicio 5 %
palabras_con_variables(S, V) :- diccionario_var(S, D), palabras_con_variables_y_dicc(S, D, V).

palabras_con_variables_y_dicc([], _, []).
palabras_con_variables_y_dicc([ [] | XSS ] , DICC, [ [] | YSS]) :- palabras_con_variables_y_dicc(XSS, DICC, YSS).
palabras_con_variables_y_dicc([ [X | XS] | XSS], DICC, [ [Y | YS] | YSS]) :- member((X, Y), DICC), palabras_con_variables_y_dicc( [XS | XSS], DICC, [YS | YSS] ).


diccionario_var([], []).
diccionario_var([ [] | XSS], DICC) :- diccionario_var(XSS, DICC).
diccionario_var([ [X | XS] | XSS], DICC) :- diccionario_var([XS|XSS], DICC2), asignar_var(X, DICC2, DICC).


% Ejercicio 6 %
% quitar(?E, +L, -R) %

quitar(_,[],[]).
quitar(E, [X|XS], R) :- X==E, quitar(E, XS, R). 
quitar(E, [X|XS], [X|R] ) :- not(X == E), quitar(E, XS, R).

% Porque funciona "not(X == E)" ?? %



% Ejercicio 7 %
% cant_distintos(?L, ?S) %
cant_distintos([], 0).
cant_distintos([X|XS], N) :- quitar(X, XS, XSsinX), cant_distintos(XSsinX, N2), N is N2+1.

%testeo
testsEj7:-testCD1,testCD2.
testCD1:-cant_distintos([A,B,A], 2).
testCD2:-cant_distintos([], 0).

%reversibilidad:
%cant_distintos(A, 2).
%A = [_G267, _G270].
%luego cuelga buscando mas soluciones
%cant_distintos(A, 0).
%A =[].
%luego cuelga buscando mas soluciones
%cant_distintos(A, B).
%A = [],
%B = 0 ;
%A = [_G282],
%B = 1 ;
%A = [_G282, _G285],
%B = 2 ;
%A = [_G282, _G285, _G297],
%B = 3 ;
%...
%luego, cant_distintos(A,B) es reversible exceptuando el caso A no instanciado y B si.


% Ejercicio 8 %
descifrar(CIFER, MSJ) :- palabras(CIFER, P), palabras_con_variables(P, VARS), flatten(VARS, VARSFLAT), sacar_repe(VARSFLAT, VARSNOREP), asignar(VARS), todos_distintos(VARSNOREP), to_string(VARS, MSJ).

sacar_repe([], []).
sacar_repe([X|XS], YS) :- quitar(X, XS, XSX), sacar_repe(XSX, REC), append([X], REC, YS).

asignar([]).
asignar([XS|XSS]) :- diccionario_lista(YS), XS = YS, asignar(XSS).

todos_distintos(XS) :- sacar_repe(XS, XS).

to_string([], S) :- string_codes(S, []).
to_string([XS], S) :- string_codes(S, XS).
to_string([XS|XSS], S) :- length(XSS, N), N > 0, string_codes(S1, XS), to_string(XSS, S2), string_concat(' ', S2, S3), string_concat(S1, S3, S).


% Ejercicio 9 %
descifrar_sin_espacios(XS, M) :- meter_espacios(XS, CIFER), descifrar(CIFER, M).

meter_espacios([X], [X]).
meter_espacios([X|XS], YS) :- meter_espacios(XS, REC), append([X, espacio], REC, YS).
meter_espacios([X|XS], YS) :- meter_espacios(XS, REC), append([X], REC, YS).


% Ejercicio 10 %
mensajes_mas_parejos(S, M) :- descifrar_sin_espacios(S, M), dev_std(M, DSM), not((descifrar_sin_espacios(S, Y), dev_std(Y, DSY), DSM > DSY)).

dev_std(M, DV) :- juntar_con(L, ' ', M), stddev(L, DV).

stddev(List, STDdev) :- variance(List, Var), STDdev is sqrt(Var).

variance([_], 1).
variance(List, Var) :-  length(List,N),
                        N > 1,
                        longitudes(List, Longs),
                        sum_list(Longs,Sum),
                        squares(Longs,SqList),
                        sum_list(SqList,SumSquares),
                        Prom is Sum / N,
                        PromSq is SumSquares / N,
                        Var is PromSq - (Prom * Prom).

longitudes([],[]).
longitudes([X|Xs],[Y|Ys]) :- longitudes(Xs,Ys), string_length(X, Y).

squares([],[]).
squares([X|Xs],[Y|Ys]) :- squares(Xs,Ys), Y is X*X.

%testeo
testsEj10:-testMMP1, testMMP2.
testMMP1:- cargar("dicc1.txt"),mensajes_mas_parejos([rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato], A), A = 'casa de flor'.
testMMP2:- cargar("dicc1.txt"),mensajes_mas_parejos([rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato], "casa flor de").
%No le gusta!!
