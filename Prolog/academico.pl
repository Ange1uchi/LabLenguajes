% Descripción: Modela una base de datos académica


% Hechos base
dicta(leonardo, ci3661).
dicta(maria, ci3725).
dicta(pedro, ci2691).

cursa(ana, ci3661).
cursa(ana, ci3725).
cursa(juan, ci3661).
cursa(elena, ci2691).
cursa(elena, ci3725).

nota(ana, ci3661, 88).
nota(ana, ci3725, 70).
nota(juan, ci3661, 40).
nota(elena, ci2691, 51).

% 1. profesor_de(Prof, Est)
% Verdadero si Prof enseña alguna materia cursada por Est
profesor_de(Prof, Est) :-
    dicta(Prof, Mat),
    cursa(Est, Mat).

% 2. aprobado(Est, Mat)
% Verdadero si Est cursó Mat y tiene nota >= 50
aprobado(Est, Mat) :-
    nota(Est, Mat, Nota),
    Nota >= 50.

% 3. aplazado(Est)
% Verdadero si Est tiene alguna nota menor a 50
aplazado(Est) :-
    nota(Est, _, Nota),
    Nota < 50.

% 4. cursando_sin_nota(Est, Mat)
% Verdadero si Est cursa Mat pero no tiene nota registrada
cursando_sin_nota(Est, Mat) :-
    cursa(Est, Mat),
    not(nota(Est, Mat, _)).
