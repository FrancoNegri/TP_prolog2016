asignar_var(Atomo, MI, [(Atomo, _) | MI]) :- not((member((Atomo, _), MI))).
asignar_var(Atomo, MI, MI) :- member((Atomo, _), MI).

palabras_con_variables(S, V) :- diccionario_var(S, D), palabras_con_variables_y_dicc(S, D, V).

palabras_con_variables_y_dicc([], DICC, []).
palabras_con_variables_y_dicc([ [] | XSS], DICC, [[]|YSS]):- palabras_con_variables_y_dicc( XSS, DICC, YSS).
palabras_con_variables_y_dicc([ [X | XS] | XSS], DICC, [ [Y | YS] | YSS]) :-
member((X, Y), DICC), palabras_con_variables_y_dicc( [XS | XSS], DICC, [YS | YSS] ).


diccionario_var([], []).
diccionario_var([ [] | XSS], DICC) :- diccionario_var(XSS, DICC).
diccionario_var([ [X | XS] | XSS], DICC) :- diccionario_var([XS|XSS], DICC2), asignar_var(X, DICC2, DICC).


% Ejercicio 6 %
% quitar(?E, +L, -R) %

quitar(_,[],[]).
quitar(E, [X|XS], R) :- X==E, quitar(E, XS, R). 
quitar(E, [X|XS], [X|R] ) :- not(X == E), quitar(E, XS, R).  