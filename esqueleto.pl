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
diccionario_lista(S) :- diccionario(D), string_codes(D, S).



% Ejercicio 2 %
% juntar_con(L, J, R) %
juntar_con([X], _, X).
juntar_con([X|Xs], J, R) :- juntar_con(Xs, J, R2), append(X, [J|R2], R).



% Ejercicio 3 %
% palabras(S, P) %
palabras([], []).
palabras(S, P) :- append(PRE, [espacio|TPRE], S), not(member(espacio, PRE)), palabras(TPRE, P2), append([PRE], P2, P).
palabras(S, [S]) :- not(member(espacio, S)).



% Ejercicio 4 %
asignar_var(Atomo, MI, [(Atomo, _) | MI]) :- not((member((Atomo, _), MI))).
asignar_var(Atomo, MI, MI) :- member((Atomo, _), MI).



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
% cant_distintos(L, S) %

cant_distintos([], 0).
cant_distintos([X|XS], N) :- quitar(X, XS, XSsinX), cant_distintos(XSsinX, N2), N is N2+1.



% Ejercicio 8 %
descifrar(CIFER, MSJ) :- palabras(CIFER, P), palabras_con_variables(P, VARS), flatten(VARS, VARSFLAT), sacar_repe(VARSFLAT, VARSNOREP), asignar(VARS), todos_distintos(VARSNOREP), to_string(VARS, MSJ).

sacar_repe([], []).
sacar_repe([X|XS], YS) :- quitar(X, XS, XSX), sacar_repe(XSX, REC), append([X], REC, YS).

asignar([]).
asignar([XS|XSS]) :- diccionario_lista(YS), XS = YS, asignar(XSS).

todos_distintos(XS) :- sacar_repe(XS, XS).

to_string([], S) :- string_codes(S, []).
to_string([XS], S) :- string_codes(S, XS).
to_string([XS|XSS], S) :- length(XSS, N), N > 0, string_codes(S1, XS), to_string(XSS, S2), string_concat(" ", S2, S3), string_concat(S1, S3, S).


% Ejercicio 9 %
descifrar_sin_espacios(XS, M) :- meter_espacios(XS, CIFER), descifrar(CIFER, M).

meter_espacios([X], [X]).
meter_espacios([X|XS], YS) :- meter_espacios(XS, REC), append([X, espacio], REC, YS).
meter_espacios([X|XS], YS) :- meter_espacios(XS, REC), append([X], REC, YS).


% Ejercicio 10 %

mensajes_mas_parejos(XS, M, DESV) :- meter_espacios(XS, CIFER),lenPalabras(CIFER, LEN),stddev(LEN, DESV), descifrar(CIFER, M).

%DESV para ver los desvios estandar.

lenPalabras([],[0]).
lenPalabras([espacio|Xs],[0|Rec]):-lenPalabras(Xs,Rec).
lenPalabras([X|Xs],[Len|Y]):- X \= espacio, lenPalabras(Xs,[LenRec|Y]), Len is LenRec + 1.

stddev(List, STddev) :- length(List,N),
                        sum(List,Sum),
                        squares(List,SqList),
                        sum(SqList,SumSquares),
                        variance(N,Sum,SumSquares,Varnaice),
                        STddev = sqrt(Varnaice).

variance(N,_,_;0):- N =< 1, !.
variance(N,Sum,SumSq,Variance):-
    Variance is (SumSq - (Sum+Sum/N)) /(N-1).

sum([],0).
sum([X|Xs],Res):- sum(Xs,REC), Res is REC +1.

squares([],[]).
squares([X|Xs],[Y|Ys]):- squares(Xs,Ys), Y is X*X.



lenPalabras([],[0]).
lenPalabras([espacio|Xs],[0|Rec]):-lenPalabras(Xs,Rec).
lenPalabras([X|Xs],[Len|Y]):- X \= espacio, lenPalabras(Xs,[LenRec|Y]), Len is LenRec + 1.

stddev(List, STddev) :- length(List,N),
                        sum(List,Sum),
                        squares(List,SqList),
                        sum(SqList,SumSquares),
                        variance(N,Sum,SumSquares,Varnaice),
                        STddev = sqrt(Varnaice).

variance(N,_,_;0):- N =< 1, !.
variance(N,Sum,SumSq,Variance):-
    Variance is (SumSq - (Sum+Sum/N)) /(N-1).

sum([],0).
sum([X|Xs],Res):- sum(Xs,REC), Res is REC +1.

squares([],[]).
squares([X|Xs],[Y|Ys]):- squares(Xs,Ys), Y is X*X.

