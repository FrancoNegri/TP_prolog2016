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

% Es el ej1 pero sin el espacio.
ej(5, [rombo, cuadrado, perro, cuadrado, sol, cuadrado]).

% Es el ej2 pero sin el espacio.
ej(6, [rombo, cuadrado, perro, triangulo, sol, cuadrado]).




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




% Ejercicio 3
% palabras(+S, ?P)
% Si "S" no esta instanciado, "palabras" entra en un bucle infinito. Esto sucede por que el primer llamado a "append" no puede instanciar a "TPRE" puesto que "S" tampoco
% esta instanciado y luego, se llama recursivamente a "palabras" con "TPRE" sin instanciar, generando así el bucle.
palabras(S, P) :- append(PRE, [espacio|TPRE], S), not(member(espacio, PRE)), palabras(TPRE, P2), append([PRE], P2, P).
palabras(S, [S]) :- not(member(espacio, S)).

% Test ejercicio 3
test_ej3 :- test_3_1, test_3_2, test_3_3, test_3_4, !.
test_3_1 :- ej(1, S), palabras(S, [[rombo, cuadrado], [perro, cuadrado, sol, cuadrado]]).
test_3_2 :- ej(3, S), palabras(S, [[rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]]).
test_3_3 :- ej(4, S), palabras(S, [[rombo], [perro], [sol, luna], [estrella], [arbol], [gato]]).
test_3_4 :- palabras([], [[]]).




% Ejercicio 4
asignar_var(Atomo, MI, [(Atomo, _) | MI]) :- not((member((Atomo, _), MI))).
asignar_var(Atomo, MI, MI) :- member((Atomo, _), MI).

% Test ejercicio 4
test_ej4 :- test_4_1, test_4_2, test_4_3, !.
test_4_1 :- asignar_var(cuadrado, [], [(cuadrado, _G4012)]).
test_4_2 :- asignar_var(cuadrado, [(rombo, _G4012)], [(cuadrado, _G4013),(rombo, _G4012)]).
test_4_3 :- asignar_var(rombo, [(cuadrado, _G4013),(rombo, _G4012)], [ (cuadrado, _G4013), (rombo, _G4012)]).

% Reversibilidad:
%Primer argumento:
%asignar_var(A, [], [(cuadrado, _G4012)]).
%A = [] ;
%A = [ (cuadrado, _G4012)]. 
%No tendría que devolver esto!!

%asignar_var(A, [(rombo, _G4012)], [(cuadrado, _G4013),(rombo, _G4012)]).
%asignar_var(A, [(cuadrado, _G4013),(rombo, _G4012)], [ (cuadrado, _G4013), (rombo, _G4012)]).
%La función sin el primer valor instanciado devolverá el valor esperado

%segundo:
%asignar_var(cuadrado, A, [(cuadrado, _G4012)]).
%asignar_var(cuadrado, A, [(cuadrado, _G4013),(rombo, _G4012)]).
%devuelve la lista sin el elemento agregado

%ambos:
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
%Si A esta sin instanciar, B y C instanciados, la función intentará unificar A con alguno de los elementos del mapeo C, tal que la union entre A y el mapeo de B resulten en C. 
%Si B no esta instanciado, y A y C si, entonces la función intentará unificar a B con C habiendo quitado el elemnto A.
%Si A y B no estan instanciados....




% Ejercicio 5 %
palabras_con_variables(S, V) :- diccionario_var(S, D), palabras_con_variables_y_dicc(S, D, V).

palabras_con_variables_y_dicc([], _, []).
palabras_con_variables_y_dicc([ [] | XSS ] , DICC, [ [] | YSS]) :- palabras_con_variables_y_dicc(XSS, DICC, YSS).
palabras_con_variables_y_dicc([ [X | XS] | XSS], DICC, [ [Y | YS] | YSS]) :- member((X, Y), DICC), palabras_con_variables_y_dicc( [XS | XSS], DICC, [YS | YSS] ).


diccionario_var([], []).
diccionario_var([ [] | XSS], DICC) :- diccionario_var(XSS, DICC).
diccionario_var([ [X | XS] | XSS], DICC) :- diccionario_var([XS|XSS], DICC2), asignar_var(X, DICC2, DICC).




% Ejercicio 6
% quitar(?E, +L, ?R)
% Notar que aunque "E" no este instanciado, la función "quitar" no es reversible en "E". Si "E" no esta instanciado, "quitar" buscara a la variable pasada como
% parámetro en la lista "L" y quitara todas sus ocurrencias.
% El parámetro "L" debe estar instanciado y "R" puede estarlo o no.
% En caso de que "R" este instanciado, "quitar" verificara si la instancia es correcta o no. En caso de que no este instanciado, "R" contendrá la solución.
quitar(_, [], []).
quitar(E, [X|XS], R) :- X == E, quitar(E, XS, R). 
quitar(E, [X|XS], [X|R] ) :- not(X == E), quitar(E, XS, R).

% Test ejercicio 6
test_ej6 :- test_6_1,  test_6_2, test_6_3, test_6_4, test_6_5, test_6_6, test_6_7, !.
test_6_1 :- quitar(z, [A, B, A, z], [A, B, A]).
test_6_2 :- quitar(A, [A, B, A, z], [B, z]).
test_6_3 :- quitar(e, [A, q, w, B, E, d], [A, q, w, B, E, d]).
test_6_4 :- quitar(F, [A, q, w, B, E, d], [A, q, w, B, E, d]).
test_6_5 :- quitar(a, [], []).
test_6_6 :- quitar(A, [], []).
test_6_7 :- quitar(A, [A, A, A, A, A], []).




% Ejercicio 7
% cant_distintos(+L, ?S)
% Si L esta instanciado y S no, devuelve la cantidad de elementos distintos de la lista
% Si S esta instanciado y L no, como primer resultado devuelve una lista con S elementos distintos. Al pedirle otra solución la función quedará ciclando 
% infinitamente explorando el árbol de soluciones en busca de otra lista con S elementos distintos. Esto ocurre ya que "quitar" no soporta que el
% segundo argumento no este instanciado.
% Si S y L no están instanciados, la función irá devolviendo una lista en S con L elementos distintos (es decir, la primera lista será la vacía,
% luego devolverá la lista con un elemento, luego la lista con dos elementos distintos, etc).
% En resumen, "cant_distintos" no funciona solo en el caso en que "L" no esta instanciado y "S" si lo esta.
cant_distintos([], 0).
cant_distintos([X|XS], N) :- quitar(X, XS, XSsinX), cant_distintos(XSsinX, N2), N is N2+1.

% Test ejercicio 7
test_ej7 :- test_7_1, test_7_2, !.
test_7_1 :- cant_distintos([A,B,A], 2).
test_7_2 :- cant_distintos([], 0).

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
% descifrar_sin_espacios(+XS, ?M)
% 
descifrar_sin_espacios(XS, M) :- meter_espacios(XS, CIFER), descifrar(CIFER, M).

meter_espacios([X], [X]).
meter_espacios([X|XS], YS) :- meter_espacios(XS, REC), append([X, espacio], REC, YS).
meter_espacios([X|XS], YS) :- meter_espacios(XS, REC), append([X], REC, YS).

% Test ejercicio 9
test_ej9 :- cargar("dicc1.txt"), test_9_1, test_9_2, test_9_3, cargar("dicc0.txt"), test_9_4, test_9_5, cargar("dicc0.txt"), test_9_6, !.
test_9_1 :- ej(3, S), descifrar_sin_espacios(S, "casa de flor").
test_9_2 :- ej(3, S), descifrar_sin_espacios(S, "casa flor de").
test_9_3 :- ej(3, S), descifrar_sin_espacios(S, "casa miento").
test_9_4 :- ej(5, S), descifrar_sin_espacios(S, "la casa").
test_9_5 :- ej(5, S), descifrar_sin_espacios(S, "casa la").
test_9_6 :- ej(6, S), descifrar_sin_espacios(S, "la cosa").




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
