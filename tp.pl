:-dynamic professeur/4.
:-dynamic eleve/4.
:-dynamic salle/4.
:-dynamic groupe/4.
:-dynamic seance/6.
:-dynamic matiere/5.
:-dynamic classe/7.

%Structures 

professeur(nom, prenom, discipline, identifiant).
eleve(nom, prenom, niveau, identifiant).
salle(nom, capacite, type, identifiant).
groupe(nom, niveau, liste-eleves, identifiant).
seance(matiere, prof, groupe, salle, creneau, identifiant).
matiere(nom, discipline, type, niveau, identifiant).
classe(nom, niveau, groupe, liste-professeurs, liste-matieres, liste-seances, identifiant).

% Question 1

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

% Question 2

ajouter_eleve_groupe(E, G):-
    eleve(_, _, _, E),
    groupe(NG, NiG, LG, G),
    del_groupe(NG, NiG, LG, G),
    append(LG, [E], LG2),
    add_groupe(NG, NiG, LG2, G).

% Question 3

problemeCapacite():-
    seance(_, _, G, S, _, I),
    salle(_, C, _, S),
    nb_eleves(G, X),
    C < X,
    format('Seance ~d : NbEleves = ~d et NbPlaces = ~d', [I, X, C]).

% Question 4

listeSeanceProfesseur(P):-
    listing(seance(_,P,_,_,_,_)).

listeSeanceGroupe(G):-
    listing(seance(_,_,G,_,_,_)).

listeSeanceSalle(S):-
    listing(seance(_,_,_,S,_,_)).

listeSeanceEleve(E):-
	get_id_groupe_eleve(E, I),
	listing(seance(_, _, I, _, _, _)).

get_id_groupe_eleve(Eleve, IdGroupe):-
	groupe(_, _, Liste, IdGroupe),
	eleve(_, _, _, Eleve),
	member(Eleve, Liste).

	
member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).	

nb_eleves(G, X):-
    groupe(_, _, L, G),
    length(L, X).

% Question 5

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

% Question 6

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

% Question 7

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

% Question 8

edt_prof(P):-
	professeur(N, Pr, _, P),
	format('~w ~w', [N, Pr]),
	afficher_seances_prof(P).

afficher_seances_prof(P):-
	seance(M, P, G, S, C, _),
	jour(C, Day),
	creneau(C, Creneau),
	salle(NSalle, _, _, S),
	groupe(NGroupe, _, _, G),
	matiere(NMatiere, _, _, _, M),
	format('Seance de ~w a ~w, ~w, Salle ~w avec le groupe ~w', [Day, Creneau, NMatiere, NSalle, NGroupe]).

edt_salle(S):-
	salle(N, _, _, S),
	writeln(N),
	afficher_seances_salle(S).

afficher_seances_salle(S):-
	seance(M, P, G, S, C, _),
	jour(C, Day),
	creneau(C, Creneau),
	groupe(NomGroupe, _, _, G),
	professeur(NomProf, PrenomProf, _, P),
	matiere(NomMatiere, _, _, _, M),
	format('Seance de ~w a ~w, ~w avec le groupe ~w, par ~w ~w', [Day, Creneau, NomMatiere, NomGroupe, NomProf, PrenomProf]).

edt_eleve(E):-
	eleve(Nom, Prenom, _, E),
	format('~w ~w\n', [Nom, Prenom]),
	afficher_seances_eleve(E).

afficher_seances_eleve(E):-
	get_id_groupe_eleve(E, G),
	seance(M, P, G, S, C, _),
	jour(C, Day),
	salle(NomSalle, _, _, S),
	creneau(C, Creneau),
	professeur(NomProf, PrenomProf, _, P),
	matiere(NomMatiere, _, _, _, M),
	format('Seance de ~w a ~w, ~w dans la salle ~w, par ~w ~w', [Day, Creneau, NomMatiere, NomSalle, NomProf, PrenomProf]).

% Question 9

charge_professeur(P):-
	aggregate_all(count, seance(_, P, _, _, _, _), Count),
	professeur(Nom, _, _, P),
	C is 2*Count,
	format('Charge du professeur ~w : ~dh', [Nom, C]).

charge_salle(S):-
	aggregate_all(count, seance(_, _, _, S, _, _), Count),
	salle(Nom, _, _, S),
	C is 2*Count,
	format('Charge de la salle ~w : ~dh', [Nom,C]).

charge_eleve(E):-
	get_id_groupe_eleve(E, G),
	aggregate_all(count, seance(_, _, G, _, _, _), Count),
	eleve(Nom, Prenom, _, E),
	C is 2*Count,
	format('Charge de ~w ~w : ~dh', [Nom, Prenom, C]).

% Question 10

add_classe(Nom, Niveau, Groupe, LP, LM, LS, I):-
    \+ classe(Nom, Niveau, Groupe, LP, LM, LS, I),
    \+ classe(_, _, _, _, _, _,I),
    assert(classe(Nom, Niveau, Groupe, LP, LM, LS, I)).

del_classe(Nom, Niveau, Groupe, LP, LM, LS, I):-
    classe(Nom, Niveau, Groupe, LP, LM, LS, I),
    retract(classe(Nom, Niveau, Groupe, LP, LM, LS, I)).

% Question 11

verif_classe(Classe):-
	classe(_, _, _, _, LM, LS, Classe),
	seances_to_matieres(LS, A),
	verif_seances(A, LM).

verif_seances(_, []).
verif_seances(A, [H|T]):-
	seance(Matiere, _, _, _, _, H),
	writeln(H),
	count(A, Matiere, X),
	X >= 2,
	verif_seances(A, T).


seance_to_matiere(S, M):-
	seance(M, _, _, _, _, S).

seances_to_matieres([], []).
seances_to_matieres([LSH|LST], [LMH|LMT]):-
	seance_to_matiere(LSH, LMH),
	seances_to_matieres(LST, LMT).

	
count([],X,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).