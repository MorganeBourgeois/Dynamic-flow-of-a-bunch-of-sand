# Dynamic-flow-of-a-bunch-of-sand
Numerical simulation of the dynamic flow of a bunch of sand using a simplified Autonomy bi-dimensional model inspired from the original model of BTW

Afin de pouvoir compiler et exécuter le programme FORTRAN ecoul_gran_v2.f90 veuillez suivre les instructions suivantes sous le système d’exploitation UNIX.
Ouvrez l’invite de commande et tapez-y les instructions suivantes :

mkdir nom_dossier
cd nom_dossier

Rangez le fichier ecoul_gran_v2.f90 et param.dat dans ce dossier

A nouveau, dans l’invite de commande, tapez les instructions suivantes :

g90 -o ecoul_gran_v2 ecoul_gran_v2.f90 (permettant de compiler le programme)
./ecoul_gran_v2 (permettant d’exécuter le programme)

Vous trouverez dans le dossier nom_dossier les fichiers output suivants :
- tas_final.res regroupant les données du nombre de grand par pile ainsi que Figure1.jpeg la représentation graphique de ces résultats
- distrib_taille.res regroupant les données du nombre du nombre d'avalanche en fonction de leur taille ainsi que la réprésentation graphique linéaire de ces résultats
- distib_mass.res regroupent les données du nombre d'avalanhce en fonction de leur masse ainsi que le représentation graphique linéaire de ces résultats 
