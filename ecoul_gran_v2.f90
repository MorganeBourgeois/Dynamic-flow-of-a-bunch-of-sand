! Ce script contient les modules et le programme principal
! .....Module.........................................................................................................

MODULE structure_donnee
	IMPLICIT NONE
	
	! Définition du type dérivé tas
	TYPE tas
		INTEGER :: rayon
		INTEGER :: hauteur
		INTEGER, DIMENSION(:), ALLOCATABLE :: pile
		CHARACTER, DIMENSION(:,:), ALLOCATABLE :: grille
	END TYPE tas
	
	CONTAINS
	! Fonction permettant à l'utilisateur d'entrer les valeurs du rayon et de la hauteur de façon controlée
	INTEGER FUNCTION lecture_controlee(nmin,nmax)
		INTEGER, INTENT(IN) :: nmin, nmax
		
		lecture_controlee=0
		DO 
			IF ((lecture_controlee>nmax) .OR. (lecture_controlee<nmin)) THEN
				READ*, lecture_controlee
			ELSE
				EXIT
			END IF 

		END DO
	
	END FUNCTION lecture_controlee
	
	! Sous-routine affichant un visuel de l'évolution de l'écoulement granulaire au fil des itérations temporelles
	SUBROUTINE affiche(un_tas)
		TYPE(tas), INTENT(INOUT) :: un_tas
		INTEGER :: i, j, a
		
		CALL system('clear') 	! Rend l'affichage plus lisible
		
		un_tas%grille=" "	! Initialisation de la matrice d'affichage
		
		! Boucle modélisant les grains
		DO j=-(un_tas%rayon-1),un_tas%rayon-1
			a = un_tas%pile(j)-1
			DO i= un_tas%hauteur, un_tas%hauteur-a, -1
				un_tas%grille(i,j)='o'
				
			END DO	
		END DO
		
		! Boucle d'affichage
		DO i= 1, un_tas%hauteur
			PRINT*, un_tas%grille(i,:)
		END DO

	END SUBROUTINE affiche
	
END MODULE structure_donnee

! ....Debut du programme......................................................................................

PROGRAM ecoul_gran_v2
	USE structure_donnee
	IMPLICIT NONE
	TYPE(tas) :: tas1
	INTEGER, PARAMETER :: nmin=3, nmax=40, nt=10 ! Initialisation des paramètres du problème
	INTEGER :: nb_grain ,t, i, ok, a, j, n, p, taille, masse, k
	CHARACTER :: c
	CHARACTER(LEN=25) :: nom_fichier1
	CHARACTER(LEN=25) :: nom_fichier2
	CHARACTER(LEN=25) :: nom_fichier3
	REAL	:: e, e1, e2, ecart
	INTEGER, DIMENSION(:), ALLOCATABLE :: tab_masse
	INTEGER, DIMENSION(:), ALLOCATABLE :: tab_taille
	!INTEGER, DIMENSION(:), ALLOCATABLE :: list
	
	CALL RANDOM_SEED

	open(unit=10, file = "param.dat", action= "read")
	read(unit=10, fmt=*)tas1%rayon
	read(unit=10, fmt=*)tas1%hauteur
	read(unit=10, fmt=*)c
	read(unit=10, fmt=*)nom_fichier1
	read(unit=10, fmt=*)nom_fichier2
	read(unit=10, fmt=*)nom_fichier3
	close(unit=10)
	
	!PRINT*, "Entrer la valeur entière de la hauteur maximale comprise entre", nmin, "et", nmax
	!tas1%hauteur = lecture_controlee(nmin, nmax)
	!PRINT*, "Entrer la valeur entière du rayon maximal comprise entre", nmin, "et", nmax
	!tas1%rayon = lecture_controlee(nmin, nmax)
	
	! Test de l'allocation des matrices
	ALLOCATE(tas1%pile(-(tas1%rayon-1):tas1%rayon-1), stat=ok)
	IF (ok/=0) THEN
		PRINT*, "L'allocation a échoué"
		STOP
	END IF
	
	ALLOCATE(tas1%grille(tas1%hauteur,-(tas1%rayon-1):tas1%rayon-1), stat=ok)
	IF (ok/=0) THEN
		PRINT*, "L'allocation a échoué"
		STOP
	END IF
	
	ALLOCATE(tab_taille(tas1%rayon), stat = ok)
	IF (ok/=0) THEN
		PRINT*, "L'allocation a échoué"
		STOP
	END IF
	
	ALLOCATE(tab_masse(tas1%rayon*2), stat = ok)
	IF (ok/=0) THEN
		PRINT*, "L'allocation a échoué"
		STOP
	END IF
		
	! Initialisation des données du problème
	tas1%pile = 0
	nb_grain = 0
	t = 0
	masse =0
	tab_taille = 0
	taille = 1
	tab_masse = 0
	
	
	!PRINT*, "Voulez-vous voir l'évolution de la pile au cours du temps s'afficher ?(y/n)"
	!READ *, c		

	DO WHILE (MAXVAL(tas1%pile)<tas1%hauteur)
	
		! Ajout d'un grain toute les nt itérations temporelles
		IF (mod(t,nt)==0) THEN
			tas1%pile(0)=tas1%pile(0)+1
			nb_grain=nb_grain+1
			IF (c=='y') THEN
				CALL affiche(tas1)
				CALL system('sleep 0.1')
			END IF
		END IF 
		
		 !Introduction d'un grain à droite ou à gauche
		IF (((tas1%pile(0)-tas1%pile(1))>=2) .AND. ((tas1%pile(0)-tas1%pile(-1))>=2)) THEN
			CALL RANDOM_NUMBER(e1)
			IF (e1<0.5) THEN
				CALL RANDOM_NUMBER(e2)
		 		a = (1+FLOOR(0.5*(2+tas1%pile(0)-tas1%pile(1))*e2))
				tas1%pile(1)=tas1%pile(1)+a
				tas1%pile(0)=tas1%pile(0)-a
				IF (c=='y') THEN
					CALL affiche(tas1)
					CALL system('sleep 0.1')
				END IF
			ELSE
				CALL RANDOM_NUMBER(e2)
				a = (1+FLOOR(0.5*(2+tas1%pile(0)-tas1%pile(-1))*e2))
				tas1%pile(-1)=tas1%pile(-1)+a
				tas1%pile(0)=tas1%pile(0)-a
				IF (c=='y') THEN
					CALL affiche(tas1)
					CALL system('sleep 0.1')
				END IF
			END IF
	
		! Possible ecoulement de la pile du milieu à droite
		ELSE IF ((tas1%pile(0)-tas1%pile(1))>=2) THEN
			CALL RANDOM_NUMBER(e2)
			a = (1+FLOOR(0.5*(2+tas1%pile(0)-tas1%pile(1))*e2))
			tas1%pile(1)=tas1%pile(1)+a
			tas1%pile(0)=tas1%pile(0)-a
			IF (c=='y') THEN
					CALL affiche(tas1)
					CALL system('sleep 0.1')
			END IF
	
		! Possible ecoulement de la pile du milieu à gauche
		ELSE IF ((tas1%pile(0)-tas1%pile(-1))>=2) THEN
			CALL RANDOM_NUMBER(e2)
			a = (1+FLOOR(0.5*(2+tas1%pile(0)-tas1%pile(-1))*e2))
			tas1%pile(-1)=tas1%pile(-1)+a
			tas1%pile(0)=tas1%pile(0)-a
			IF (c=='y') THEN
					CALL affiche(tas1)
					CALL system('sleep 0.1')
			END IF
		END IF
		
		! Ecoulement granulaire à droite
		DO i = 0,(tas1%rayon-1)
			IF ((tas1%pile(i)-tas1%pile(i+1))>=2) THEN
				CALL RANDOM_NUMBER(e2)
				a = (1+FLOOR(0.5*(2+tas1%pile(i)-tas1%pile(i+1))*e2))
				tas1%pile(i+1)=tas1%pile(i+1)+a
				tas1%pile(i)=tas1%pile(i)-a
			
				! Boucle tenant compte des avalanche en fonction de leur taille
				IF (((tas1%pile(i+1)-tas1%pile(i+2))>=2) .AND. (i<tas1%rayon-1)) THEN
					taille = taille+1
				ELSE
					tab_taille(taille)=tab_taille(taille)+1
				END IF
				
				! Amélioration numéro 6
				! Boucle tenant le compte des avalanches en fonction de leur masse
				masse = masse+a
				IF ((tas1%pile(i+1)-tas1%pile(i+2))<2) THEN
					tab_masse(masse)=tab_masse(masse)+1
					masse=0
				END IF
			
				IF (c=='y') THEN
					CALL affiche(tas1)
					CALL system('sleep 0.1')
				END IF	
			END IF	
		END DO
	
		! Ecoulement granulaire à gauche
		DO i = 0,-(tas1%rayon-1),-1
			IF ((tas1%pile(i)-tas1%pile(i-1))>=2) THEN
				CALL RANDOM_NUMBER(e2)
				a = (1+FLOOR(0.5*(2+tas1%pile(i)-tas1%pile(i-1))*e2))
				tas1%pile(i-1)=tas1%pile(i-1)+a
				tas1%pile(i)=tas1%pile(i)-a
			
				IF (c=='y') THEN
					CALL affiche(tas1)
					CALL system('sleep 0.1')
				END IF	

			END IF	
		END DO
	
		taille=1
		t=t+1	
	END DO

!	DO i=-(tas1%rayon-1),(tas1%rayon-1)
!		PRINT*, "Nombre de grain dans la colonne n°", i, ":", tas1%pile(i)
!	END DO
	
!	DO i=1, tas1%rayon-1
!		PRINT*, "Nombre d'avalanhe de taille ",i, ":", tab_taille(i)
!	END DO
	
!	DO i=1, tas1%rayon*2
!		PRINT*, "Nombre d'avalanche de masse ",i, ":", tab_masse(i)
!	END DO
	
	PRINT*, "Nombre de grain =", nb_grain
	PRINT*,"Nombre d'itération temporelle =", t

	!Création du fichier "tas_final.res" avec le nombre d'avalanche par rapport à la masse de l'avalanche
	open(unit=11, file=nom_fichier1, ACTION="write", IOSTAT = ok, FORM="formatted")
	IF (ok/=0) STOP "ouverture du fichier problématique"

	DO i=-(tas1%rayon-1),tas1%rayon
		write(unit=11, fmt=*)i,tas1%pile(i)
	END DO

	close(unit=11)
	
	!Création du fichier "distrib_taille.res" avec le nombre d'avalanche par rapport à la taille de l'avalanche	
	open(unit=12, file=nom_fichier2, ACTION="write", IOSTAT = ok, FORM="formatted")
	IF (ok/=0) STOP "ouverture du fichier problématique"

	DO i=1,tas1%rayon
		write(unit=12, fmt=*)i,tab_taille(i)
	END DO

	close(unit=12)
	
	!Création du fichier "distrib_masse.res" avec le nombre d'avalanche par rapport à la masse de l'avalanche
	open(unit=13, file=nom_fichier3, ACTION="write", IOSTAT = ok, FORM="formatted")
	IF (ok/=0) STOP "ouverture du fichier problématique"

	DO i=1,tas1%rayon*2
		write(unit=13, fmt=*)i,tab_masse(i)
	END DO

	close(unit=13)

	!Affichage du graphe avec nombre de grain en fonction du pile
	open(unit=14,file='courbe1.gnu',action='write',iostat=ok)
	IF (ok/=0) STOP "ouverture du fichier problématique"
	
	write(unit=14,fmt=*)"plot '", TRIM(nom_fichier1),"' u 1:2 w l "
	write(unit=14,fmt=*)'set title "Figure 1 : Nombre de grain par pile"'
	write(unit=14,fmt=*)'set xlabel "n° pile"'
	write(unit=14,fmt=*)'set ylabel "nombre de grain"'
	write(unit=14,fmt=*)'set term jpeg'
	write(unit=14,fmt=*)'set output "Figure1.jpeg"'
	write(unit=14,fmt=*)'replot'
	write(unit=14,fmt=*)'quit'
	
	close(unit=14)
	
	call system('gnuplot courbe1.gnu')
	
	!Affichage du graphe avec nombre d'avalanche par rapport à la taille de l'avalanche
	open(unit=15,file='courbe2.gnu',action='write',iostat=ok)
	IF (ok/=0) STOP "ouverture du fichier problématique"
	
	write(unit=15,fmt=*)"plot '", TRIM(nom_fichier2) ,"' u 1:2 w l"
	write(unit=15,fmt=*)'set title "Figure 2 : Distribution des avalanches par taille"'
	write(unit=15,fmt=*)'set xlabel "Taille des avalanches"'
	write(unit=15,fmt=*)'set ylabel "Nombre d avalanche"'
	write(unit=15,fmt=*)'set term jpeg'
	write(unit=15,fmt=*)'set output "Figure2.jpeg"'
	write(unit=15,fmt=*)'replot'
	write(unit=15,fmt=*)'quit'
	
	close(unit=15)
	
	call system('gnuplot courbe2.gnu')
	
	!Affichage du graphe avec le nombre d'avalanche par rapport à la masse de l'avalanche
	open(unit=16,file='courbe3.gnu',action='write',iostat=ok)
	IF (ok/=0) STOP "ouverture du fichier problématique"
	
	write(unit=16,fmt=*)"plot '", TRIM(nom_fichier3),"' u 1:2 w l"
	write(unit=16,fmt=*)'set title "Figure 3 : Distribution des avalanches par masse"'
	write(unit=16,fmt=*)'set xlabel "Masse des avalanches"'
	write(unit=16,fmt=*)'set ylabel "Nombre d avalanche"'
	write(unit=16,fmt=*)'set term jpeg'
	write(unit=16,fmt=*)'set output "Figure3.jpeg"'
	write(unit=16,fmt=*)'replot'
	write(unit=16,fmt=*)'quit'
	
	close(unit=16)
	
	call system('gnuplot courbe3.gnu')
	
	!Deallocation des tableaux 
	DEALLOCATE(tas1%pile)
	DEALLOCATE(tas1%grille)
	DEALLOCATE(tab_taille)
	DEALLOCATE(tab_masse)
	
END PROGRAM ecoul_gran_v2	
