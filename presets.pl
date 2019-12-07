add_professeur("Maudiau","Rene","Complexite",1).
add_professeur("Lecomte","Sylvain","Bdd",2).
add_professeur("Ridet","Jerome","Reseaux",3).

add_matiere("Data", "Bdd", "type 1", 1, 1).
add_matiere("Data2", "Bdd", "type 1", 1, 2).

add_eleve("Baudelet","Conrad","Super",1).
add_eleve("Le Feyer","Aymeric","Super",2).
add_eleve("Rocq","Thomas","Super",3).
add_eleve("Loiseau","Gabriel","Super",4).

add_groupe("L3","fort",[],1).
add_groupe("L3bis", "moyen", [], 2).

add_salle("E3",3,"type 1",1).
add_salle("E4",4,"type 1",2).
add_salle("E1",1,"type 1",3).
add_salle("E2",2,"type 2",4).

add_seance(1, 2, 1, 1, "c14", 1).
add_seance(1, 2, 2, 3, "c15", 2).

ajouter_eleve_groupe(1, 1).
ajouter_eleve_groupe(2, 1).
ajouter_eleve_groupe(3, 2).
ajouter_eleve_groupe(4, 2).