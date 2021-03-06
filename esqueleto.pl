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

ej(7, [rombo, rombo, rombo, rombo, rombo, rombo]).

% LA CASA EL COSA con dicc0
ej(8, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado, espacio, luna, rombo, espacio, perro, gato, sol, cuadrado]).

% flor de casa miento
ej(9, [rombo, cuadrado, perro, gato, espacio, sol, triangulo, espacio, luna, arbol, casa, arbol, espacio, pelota, hada, triangulo, frutilla, torta, perro]).





% Ejercicio 1 %
% diccionario_lista(-S) %
% Si S esta instanciado indica si S esta en el diccionario %
% Si S no esta instanciado, unifica a S con cada una de las palabras del diccionario %
diccionario_lista(S) :- diccionario(D), string_codes(D, S).




% Ejercicio 2 %
% juntar_con(?L, ?J, ?R) %
% Si L y R están sin instanciar, la función llama a append con los tres parámetros sin instanciar y, como este no recorre bien el espacio, tampoco lo hará juntar_con.
% De modo que, L y R no puede estar sin instanciar al mismo tiempo.
juntar_con([[]], J, []).
juntar_con([X], _, X).
juntar_con([X|Xs], J, R) :- append(X, [J|R2], R), juntar_con(Xs, J, R2).

% Test ejercicio 2
test_ej2 :- test_2_1, test_2_2, test_2_3, test_2_4, !.
test_2_1 :- juntar_con([[x],[x, y],[z]], a, [x, a, x, y, a, z]).
test_2_2 :- juntar_con([[x]], a, [x]).
test_2_3 :- juntar_con([[x, y, z]], a, [x, y, z]).
test_2_4 :- juntar_con([[]], a, []).




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
% asignar_var(?A, ?B, ?C)
% Si A no esta instanciado, y B y C están instanciados, la función intentará unificar A con alguno de los elementos del mapeo C, tal que la unión entre A y el mapeo de B resulten en C. 
% Si B no esta instanciado, y A y C están instanciados, entonces la función intentará unificar a B con C y con C quitando el elemento A del mapeo de C.
% Si C no esta instanciado, y A y B están instanciados, la función unificara a C con B o con el mapeo resultante de agregar A al mapeo de B en caso de que A no estuviera ya en el mapeo.
% Si A y B no están instanciados, y C si lo esta, la función unificara a B con C y a A con cada elemento del mapeo de C. En este caso NO se generan todas las combinaciones posibles de unificaciones.
% Si A y C no están instanciados, y B si lo esta, se unificara a C con B y a A con cada elemento del mapeo de B.
% Si B y C no están instanciados, y A si lo esta, se generaran infinitos mapeos que contengan a A asignado a una variable..
% Si ninguno esta instanciado, se generaran infinitos mapeos que tengan a A asignado a una variable.
asignar_var(Atomo, MI, [(Atomo, _) | MI]) :- not((member((Atomo, _), MI))).
asignar_var(Atomo, MI, MI) :- member((Atomo, _), MI).

% Test ejercicio 4
test_ej4 :- test_4_1, test_4_2, test_4_3, !.
test_4_1 :- asignar_var(cuadrado, [], [(cuadrado, _G4012)]).
test_4_2 :- asignar_var(cuadrado, [(rombo, _G4012)], [(cuadrado, _G4013),(rombo, _G4012)]).
test_4_3 :- asignar_var(rombo, [(cuadrado, _G4013),(rombo, _G4012)], [ (cuadrado, _G4013), (rombo, _G4012)]).

% ¿Por que funciona asignar_var/3? Porque Prolog asigna variables dinamicamente a los elementos que no estan instanciados para, luego, unificar dichas variables en caso de necesitarlo.





% Ejercicio 5 %
% Reversibilidad:
% palabras_con_variables(+S, ?V)
% En la función diccionario_var(S, D), si el D está instanciado y es no vacío, entonces S no es
% reversible.
% En tal caso, se entra por la segunda regla y se agrega una lista vacía a S y se vuelve a llamar a
% la misma función, pues en la llamada recursiva también entrará por la segunda regla y asi 
% indefinidamente, por lo que nunca se va a unificar D.
% En cambio si D no está instanciado o es vacío, entonces entra por la primera regla y 
% devuelve la instancia de S, aunque solo se devuelve listas de listas vacías, 
% ya que nunca entra a la tercera regla porque no termina de recorrer las soluciones de la
% segunda regla.
% Como el parámetro S de la función diccionario_var es el parámetro S de palabras_con_variables,
% entonces S es reversible si D no está instanciado o es vacío, aunque solo construye soluciones
% en donde S es una lista de listas vaciás.
palabras_con_variables(S, V) :- diccionario_var(S, D), palabras_con_variables_y_dicc(S, D, V).

palabras_con_variables_y_dicc([], _, []).
palabras_con_variables_y_dicc([ [] | XSS ] , DICC, [ [] | YSS]) :- palabras_con_variables_y_dicc(XSS, DICC, YSS).
palabras_con_variables_y_dicc([ [X | XS] | XSS], DICC, [ [Y | YS] | YSS]) :- member((X, Y), DICC), palabras_con_variables_y_dicc( [XS | XSS], DICC, [YS | YSS] ).


diccionario_var([], []).
diccionario_var([ [] | XSS], DICC) :- diccionario_var(XSS, DICC).
diccionario_var([ [X | XS] | XSS], DICC) :- diccionario_var([XS|XSS], DICC2), asignar_var(X, DICC2, DICC).

% ej(1, S), palabras(S, P), diccionario_var(P, D), diccionario_var(X, D).

% Test ejercicio 5
test_ej5 :- test_5_1, test_5_2, test_5_3, !.
test_5_1 :- ej(1, S), palabras(S, P), palabras_con_variables(P, [[X, Y], [Z, Y, W, V]]).
test_5_2 :- ej(7, S), palabras(S, P), palabras_con_variables(P, [[X, X, X, X, X, X]]).
test_5_3 :- palabras([], P), palabras_con_variables(P, [[]]).




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





% Ejercicio 8 %
% Reversibilidad:
% descifrar(+CIFER, ?MSJ)
% CIFER debe estar instanciado pues palabras_con_variables necesita que su argumento P esté 
% instanciado.Como P se obtiene a través de CIFER por palabras entonces P tampoco está instanciado,
% con lo cual palabras_con_variables se cuelga salvo casos particulares aclarados en el
% ejercicio 5.
descifrar(CIFER, MSJ) :- palabras(CIFER, P), palabras_con_variables(P, VARS), flatten(VARS, VARSFLAT), sacar_repe(VARSFLAT, VARSNOREP), asignar(VARS), todos_distintos(VARSNOREP), to_string(VARS, MSJ).

sacar_repe([], []).
sacar_repe([X|XS], YS) :- quitar(X, XS, XSX), sacar_repe(XSX, REC), append([X], REC, YS).

asignar([]).
asignar([XS|XSS]) :- diccionario_lista(YS), XS = YS, asignar(XSS).

todos_distintos(XS) :- sacar_repe(XS, XS).

to_string([], S) :- string_codes(S, []).
to_string([XS], S) :- string_codes(S, XS).
to_string([XS|XSS], S) :- length(XSS, N), N > 0, string_codes(S1, XS), to_string(XSS, S2), string_concat(' ', S2, S3), string_concat(S1, S3, S).

% Test ejercicio 8
test_ej8 :- test_8_1, test_8_2, test_8_3, !.
test_8_1 :- cargar("dicc0.txt"), ej(1, S), descifrar(S, "la casa").
test_8_2 :- cargar("dicc0.txt"), ej(8, S), descifrar(S, "la casa el cosa").
test_8_3 :- cargar("dicc1.txt"), ej(9, S), descifrar(S, "flor de casa miento").




% Ejercicio 9 %
% descifrar_sin_espacios(+XS, ?M)
% El parámetro XS debe estar instanciado pues "descifrar_sin_espacios" le agrega espacios (resultando en CIFER) y luego llama a "descifrar",
% quien necesita que su primer parámetro este instanciado.
% El parámetro se M unificara con todas las posibles soluciones en caso de no estar instanciado. En caso de estar instanciado se verificara
% que dicha instancia unifique con alguna de las posibles soluciones.
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
% mensajes_mas_parejos(+S, -M)
% Reversibilidad: S tiene que estar instanciado pues descifrar_sin_espacios necesita que S este instanciado
% Por lo tanto mensajes_mas_parejos cuelga si S no está instanciado.
mensajes_mas_parejos(S, M) :- descifrar_sin_espacios(S, M), dev_std(M, DSM), not((descifrar_sin_espacios(S, Y), dev_std(Y, DSY), DSM > DSY)).

dev_std(M, DV) :- split_string(M, " ", "", L), stddev(L, DV).

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
tests_ej10 :- test_10_1, test_10_2, test_10_3, test_10_4, !.
test_10_1 :- cargar("dicc1.txt"), ej(3, S), mensajes_mas_parejos(S, "casa de flor").
test_10_2 :- cargar("dicc1.txt"), ej(3, S), mensajes_mas_parejos(S, "casa flor de").
test_10_3 :- cargar("dicc0.txt"), ej(5, S), mensajes_mas_parejos(S, "la casa").
test_10_4 :- cargar("dicc0.txt"), ej(5, S), mensajes_mas_parejos(S, "casa la").
