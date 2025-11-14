% prelaciones.pl
% Prelaciones directas y totales con manejo de ciclos.

:- use_module(library(lists)).

% Hechos base: prela(A, B) significa que A es prelación directa de B.
prela(ci2691, ci3661).
prela(ci2525, ci2691).
prela(ci2691, ci3641).
prela(ci3641, ci3725).
prela(ci3725, ci3825).
prela(ma1111, ci2525).

% Prelación directa.
prelacion_directa(A, B) :-
    prela(A, B), !.

% Prelación total (directa o indirecta), evitando ciclos.
prelacion_total(A, B) :-
    camino_prelacion(A, B, [A]), !.

% Caso directo.
camino_prelacion(A, B, _) :-
    prela(A, B), !.

% Caso recursivo: seguimos la cadena mientras no repitamos nodos.
camino_prelacion(A, B, Visitados) :-
    prela(A, X),
    \+ memberchk(X, Visitados),
    camino_prelacion(X, B, [X|Visitados]),
    !.

% Detecta si hay un ciclo en torno a M.
ciclo(M) :-
    prelacion_total(M, M), !.

% Detecta si existe algún ciclo en el grafo.
hay_ciclo :-
    ciclo(_), !.
