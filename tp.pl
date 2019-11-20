:-dynamic professeur/4.
:-dynamic eleve/4.
:-dynamic salle/4.
:-dynamic groupe/4.
:-dynamic seance/6.


professeur(nom, prenom, discipline, identifiant).
eleve(nom, prenom, niveau, identifiant).
salle(nom, capacite, type, identifiant).
groupe(nom, niveau, liste-eleves, identifiant).
seance(matiere, prof, groupe, salle, creneau, identifiant).


add_professeur(N, P, D, I):-
    \+ professeur(N, P, D, I),
    assert(professeur(N, P, D, I)).

del_professeur(N, P, D, I):-
    professeur(N, P, D, I),
    retract(professeur(N, P, D, I)).

add_eleve(N, P, Ni, I):-
    \+ eleve(N, P, Ni, I),
    assert(eleve(N, P, Ni, I)).

del_eleve(N, P, Ni, I):-
    eleve(N, P, Ni, I),
    retract(eleve(N, P, Ni, I)).

add_salle(N, C, T, I):-
    \+ salle(N, C, T, I),
    assert(salle(N, C, T, I)).

del_salle(N, C, T, I):-
    salle(N, C, T, I),
    retract(salle(N, C, T, I)).

add_groupe(N, Ni, L, I):-
    \+ groupe(N, Ni, L, I),
    assert(groupe(N, Ni, L, I)).

del_groupe(N, Ni, L, I):-
    groupe(N, Ni, L, I),
    retract(groupe(N, Ni, L, I)).

add_seance(M, P, G, S, C, I):-
    \+ seance(M, P, G, S, C, I),
    assert(seance(M, P, G, S, C, I)).

del_seance(M, P, G, S, C, I):-
    seance(M, P, G, S, C, I),
    retract(seance(M, P, G, S, C, I)).






