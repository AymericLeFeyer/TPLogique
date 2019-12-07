:-dynamic professeur/4.
:-dynamic eleve/4.
:-dynamic salle/4.
:-dynamic groupe/4.
:-dynamic seance/6.
:-dynamic matiere/5.

professeur(nom, prenom, discipline, identifiant).
eleve(nom, prenom, niveau, identifiant).
salle(nom, capacite, type, identifiant).
groupe(nom, niveau, liste-eleves, identifiant).
seance(matiere, prof, groupe, salle, creneau, identifiant).
matiere(nom, discipline, type, niveau, identifiant).

add_professeur(N, P, D, I):-
    \+ professeur(N, P, D, I),
    \+ professeur(_, _, _, I),
    assert(professeur(N, P, D, I)).

del_professeur(N, P, D, I):-
    professeur(N, P, D, I),
    retract(professeur(N, P, D, I)).

add_eleve(N, P, Ni, I):-
    \+ eleve(N, P, Ni, I),
    \+ eleve(_, _, _, I),
    assert(eleve(N, P, Ni, I)).

del_eleve(N, P, Ni, I):-
    eleve(N, P, Ni, I),
    retract(eleve(N, P, Ni, I)).

add_salle(N, C, T, I):-
    \+ salle(N, C, T, I),
    \+ salle(_, _, _, I),
    assert(salle(N, C, T, I)).

del_salle(N, C, T, I):-
    salle(N, C, T, I),
    retract(salle(N, C, T, I)).

add_groupe(N, Ni, L, I):-
    \+ groupe(N, Ni, L, I),
    \+ groupe(_, _, _, I),
    assert(groupe(N, Ni, L, I)).

del_groupe(N, Ni, L, I):-
    groupe(N, Ni, L, I),
    retract(groupe(N, Ni, L, I)).

add_seance(M, P, G, S, C, I):-
    \+ seance(M, P, G, S, C, I),
    \+ seance(_, _, _, _, _, I),
    assert(seance(M, P, G, S, C, I)).

del_seance(M, P, G, S, C, I):-
    seance(M, P, G, S, C, I),
    retract(seance(M, P, G, S, C, I)).

add_matiere(N, D, T, Ni, I):-
    \+ matiere(N, D, T, Ni, I),
    \+ matiere(_, _, _, _, I),
    assert(matiere(N, D, T, Ni, I)).

del_matiere(N, D, T, Ni, I):-
    matiere(N, D, T, Ni, I),
    retract(matiere(N, D, T, Ni, I)).    

ajouter_eleve_groupe(E, G):-
    eleve(N, P, Ni, E),
    groupe(NG, NiG, LG, G),
    del_groupe(NG, NiG, LG, G),
    append(LG, [eleve(N, P, Ni, E)], LG2),
    add_groupe(NG, NiG, LG2, G).

get_all_eleves_from_groupe(I, E):-
    groupe(_, _, E, I).

get_eleve_from_groupe(G, E):-
	get_all_eleves_from_groupe(G, [E|T]),
	get_eleve_from_groupe([T], E).

get_eleve_from_groupe([], 0).
	
nb_eleves(G, X):-
    groupe(_, _, L, G),
    length(L, X).

problemeCapacite():-
    seance(_, _, G, S, _, I),
    salle(_, C, _, S),
    nb_eleves(G, X),
    C < X,
    format('Seance ~d : NbEleves = ~d et NbPlaces = ~d', [I, X, C]).

listeSeanceProfesseur(P):-
    listing(seance(_,P,_,_,_,_)).

listeSeanceGroupe(G):-
    listing(seance(_,_,G,_,_,_)).

listeSeanceSalle(S):-
    listing(salle(_,_,_,S,_,_)).




jour(c11, "Lundi").
jour(c12, "Lundi").
jour(c13, "Lundi").
jour(c14, "Lundi").
jour(c21, "Mardi").
jour(c22, "Mardi").
jour(c23, "Mardi").
jour(c24, "Mardi").
jour(c31, "Mercredi").
jour(c32, "Mecredi").
jour(c33, "Mecredi").
jour(c34, "Mercredi").
jour(c41, "Jeudi").
jour(c42, "Jeudi").
jour(c43, "Jeudi").
jour(c44, "Jeudi").
jour(c51, "Vendredi").
jour(c52, "Vendredi").
jour(c53, "Vendredi").
jour(c54, "Vendredi").
jour(c61, "Samedi").
jour(c62, "Samedi").
jour(c63, "Samedi").
jour(c64, "Samedi").

creneau(c11, "8h-10h").
creneau(c12, "10h-12h").
creneau(c13, "14h-16h").
creneau(c14, "16h-18h").
creneau(c21, "8h-10h").
creneau(c22, "10h-12h").
creneau(c23, "14h-16h").
creneau(c24, "16h-18h").
creneau(c31, "8h-10h").
creneau(c32, "10h-12h").
creneau(c33, "14h-16h").
creneau(c34, "16h-18h").
creneau(c41, "8h-10h").
creneau(c42, "10h-12h").
creneau(c43, "14h-16h").
creneau(c44, "16h-18h").
creneau(c51, "8h-10h").
creneau(c52, "10h-12h").
creneau(c53, "14h-16h").
creneau(c54, "16h-18h").
creneau(c61, "8h-10h").
creneau(c62, "10h-12h").
creneau(c63, "14h-16h").
creneau(c64, "16h-18h").

creneauFinal(A, X, Y):-
    jour(A,X),
    creneau(A,Y).

conflit():-
	seance(_, P1, _, _, C1, I1),
	seance(_, P2, _, _, C2, I2),
	I1 \= I2,
	P1 = P2,
	C1 = C2,
	format('Conflit: Prof ~d, Creneau ~w', [P1, C1]).
conflit():-
	seance(_, _, G1, _, C1, I1),
	seance(_, _, G2, _, C2, I2),
	I1 \= I2,
	G1 = G2,
	C1 = C2,
	format('Conflit: Groupe ~d, Creneau ~w', [G1, C1]).
conflit():-
	seance(_, _, _, S1, C1, I1),
	seance(_, _, _, S2, C2, I2),
	I1 \= I2,
	S1 = S2,
	C1 = C2,
	format('Conflit: Salle ~d, Creneau ~w', [S1, C1]).












