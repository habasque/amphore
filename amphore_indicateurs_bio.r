################################################################################ 
## Programme principal de calcul des indicateurs bio-écologiques 
## Nantes Mars 2011, ABA
##        Avril 2011 LTdM
##        Aout->Decembre 2011, JHA
################################################################################ 
  rm(list=ls())
  
#chargement de l'environnement (reference au site cran.fr car versions stables des packages)

  if(!require("tcltk")){
  install.packages("circular",repos="http://cran.fr.r-project.org")
  require("tcltk")
  }
  if(!require("vegan")){
  install.packages("vegan",repos="http://cran.fr.r-project.org")
  require("vegan")
  }
  if(!require("gplots")){
  install.packages("gplots",repos="http://cran.fr.r-project.org")
  require("gplots")
  }
  if(!require("shape")){
  install.packages("shape",repos="http://cran.fr.r-project.org")
  require("shape")
  }
  if(!require("plotrix")){
  install.packages("plotrix",repos="http://cran.fr.r-project.org")
  require("plotrix")
  }
  if(!require("sp")){
  install.packages("sp",repos="http://cran.fr.r-project.org")
  require("sp")
  }  
  if(!require("maptools")){
  install.packages("maptools",repos="http://cran.fr.r-project.org")
  require("maptools")
  }
  if(!require("maps")){
  install.packages("maps",repos="http://cran.fr.r-project.org")
  require("maps")
  }  
  if(!require("mapdata")){
  install.packages("mapdata",repos="http://cran.fr.r-project.org")
  require("mapdata")
  }          
  if(!require("doBy")){
  install.packages("doBy",repos="http://cran.fr.r-project.org")
  library("doBy")
  }  
  if(!require("XML")){
  install.packages("XML",repos="http://cran.fr.r-project.org")
  library("XML")
  }        
  if(!require("odfWeave")){
  install.packages("odfWeave",repos="http://cran.fr.r-project.org")
  library("odfWeave")
  } 
  if(!require("png")){
  install.packages("png",repos="http://cran.fr.r-project.org")
  library("png")
  } 
  if(!require("RgoogleMaps")){
  install.packages("RgoogleMaps",repos="http://cran.fr.r-project.org")
  library("RgoogleMaps")
  }        
       
  cl=colors()

  #seule variable globale a ne pas pouvoir etre inclue dans la fonction "initialisation"  
  ANALYSE_EN_COURS <- "N"
  IMPORTATION_FICHIERS <- "N"
  indice_Strate <- 8 #champ strate pour fichier captures
  indice_Strate2 <- 11 #champ strate pour fichier tailles
  indice_Time <- 13  
  indice_Time2 <- 16

  #definition du repertoire des scripts et sous-scripts avant source
  repertoire_scripts=tclvalue(tkchooseDirectory(title="Sélectionnez le répertoire de travail où se trouvent les programmes.", initialdir=getwd()))
  setwd(repertoire_scripts)

  #source(list.files(pattern=".r"))
    
  source("choixAnneesMois.r",encoding="latin1")
  source("lecture_verification_fichiers.r",encoding="latin1")
  source("test_interaction_interannuelle.r",encoding="latin1")
  source("indicateurs_communaute.r",encoding="latin1")
  source("indicateurs_population.r",encoding="latin1")
  source("anova_indicateurs_populations.r",encoding="latin1")
  source("anova_indicateurs_communaute.r",encoding="latin1")
  source("anova.1way.r",encoding="latin1")
  source("serie_temporelle.r",encoding="latin1")
  source("radar_plot.r",encoding="latin1")
  source("score_global.r",encoding="latin1")
  source("diagnostic_final.r",encoding="latin1")
  source("carto.r",encoding="latin1")  
  
  #definition des repertoires de travail
  repertoire_refTax=paste(repertoire_scripts,"/Referentiels",sep="")
  assign("repertoire_refTax",repertoire_refTax,envir=.GlobalEnv)       
  repertoire_Scenario=paste(repertoire_scripts,"/Scenarios",sep="")
  assign("repertoire_Scenario",repertoire_Scenario,envir=.GlobalEnv)    
  repertoire_modele=paste(repertoire_scripts,"/Modeles_Rapport",sep="")
  assign("repertoire_modele",repertoire_modele,envir=.GlobalEnv)     
  repertoire_probabilites=paste(repertoire_scripts,"/Probabilites",sep="")
  assign("repertoire_probabilites",repertoire_probabilites,envir=.GlobalEnv)     
  repertoire_images=paste(repertoire_scripts,"/Images",sep="")
  assign("repertoire_images",repertoire_images,envir=.GlobalEnv)     
  repertoire_shapefiles=paste(repertoire_scripts,"/Shapefiles",sep="")
  assign("repertoire_shapefiles",repertoire_shapefiles,envir=.GlobalEnv) 
     
#chargement des shapes
#shape_mondial_amp <- readShapePoly("../Shapes/habasque-search-1318413460764.shp")
#assign("shape_mondial_amp",shape_mondial_amp,envir=.GlobalEnv)      

#shape_trait_cote <- readShapePoly(paste(repertoire_shapefiles,"/Monde_Veridian.shp",sep=""))
#assign("shape_trait_cote",shape_trait_cote,envir=.GlobalEnv) 

################################################################################
# FONCTIONS UTILITAIRES
#     - creation de table de contingence : contingence.fct
#     - attribution de scenario : sce.fct
################################################################################

# initialisation des variables globales, tous les parametres peuvent evoluer durant la session 
initialisation.f <- function() {
  #ANALYSE_EN_COURS <- "N"
  ANNEE_MISE_EN_RESERVE <- 0
  OBJECTIF_ETUDIE <- NULL   
  APPROCHE_CHOISIE <- NULL
  GROUPE_ANNEES_1 <- NA  
  GROUPE_ANNEES_2 <- NA     
  GROUPE_ANNEES_3 <- NA       
  ANNEE_DEBUT <- NA   
  ANNEE_FIN <- NA   
  assign("ANNEE_MISE_EN_RESERVE",ANNEE_MISE_EN_RESERVE,envir=.GlobalEnv)    
  assign("OBJECTIF_ETUDIE",OBJECTIF_ETUDIE,envir=.GlobalEnv)    
  assign("APPROCHE_CHOISIE",APPROCHE_CHOISIE,envir=.GlobalEnv)    
  assign("GROUPE_ANNEES_1",GROUPE_ANNEES_1,envir=.GlobalEnv)    
  assign("GROUPE_ANNEES_2",GROUPE_ANNEES_2,envir=.GlobalEnv)    
  assign("GROUPE_ANNEES_3",GROUPE_ANNEES_3,envir=.GlobalEnv)    
  assign("ANNEE_DEBUT",ANNEE_DEBUT,envir=.GlobalEnv)    
  assign("ANNEE_FIN",ANNEE_FIN,envir=.GlobalEnv)            
  #assign("ANALYSE_EN_COURS",ANALYSE_EN_COURS,envir=.GlobalEnv)     
}  
# fonction de creation de table de contingence
# (densité d'espèce par trait*année) pour le calcul de diversité
contingence.fct <- function(matrice_captures) {
   contingence = tapply(matrice_captures$Nombre,list(matrice_captures$Trait,matrice_captures$Espece),na.rm=TRUE,sum)
   contingence[is.na(contingence)] = 0
   #suppression des especes qui ne sont jamais vues sinon problemes pour les calculs d'indices de diversite
   a = which(apply(contingence,2,sum,na.rm=T)==0)   
   if (length(a) != 0) {
      contingence = contingence[,-a]
   }
   rm(a)

   # idem
   b = which(apply(contingence,1,sum,na.rm=T)==0)  
   if (length(b) != 0) {
      contingence = contingence[-b,]
   }
   rm(b)
   return(contingence)
}
 
# fonction d'attribution de scenario, renvoie 1, 2 ou 3
# entree : - somme de la variable etudiee par trait 
#          - facteur etudie
scenario.f <- function(ind, fact) { 
    mean.ind <- tapply(ind, fact, mean) # calcul de la moyenne de la variable mesuree (abondance par exemple) par facteur
    res.ind <- anova.1way(ind ~ as.factor(fact), nperm=999)
    toto=NULL
      if (res.ind$anova.table[,5][1] < 0.05 & mean.ind[1] < mean.ind[2])
         (toto=1) #facteur significatif et variable du 1er facteur < variable du 2eme facteur
      if (res.ind$anova.table[,5][1] > 0.05) (toto=2) #facteur non significatif       
      if (res.ind$anova.table[,5][1] < 0.05 & mean.ind[1] > mean.ind[2])
         (toto=3) #facteur significatif et variable du 1er facteur >variable du 2eme facteur
    return(toto)
} 
       
################################################################################
## Nom     : saisie_nom_site.f
## Objet   : saisie du nom du site traite, il sera ensuite utilisé pour les noms de fichiers de resultats
## Input   :  
## Output  :  
################################################################################

saisie_nom_site.f = function() {
      aa<-tktoplevel() 
      tkwm.title(aa,"Nom du site etudie")     
      #zone de saisie    
      Year <- tclVar("    ")  
      entry.Year <-tkentry(aa,width="20",textvariable=Year)
      tkgrid(tklabel(aa,text="Quel est le nom du site d'etudes (il sera utilise pour les noms de fichiers de resultats) ?"))  # par exemple Bamboung ou BBG
      tkgrid(entry.Year)
      OnOK <- function(){
      	SITE_ETUDIE <- trim(tclvalue(Year))
	      assign("SITE_ETUDIE",SITE_ETUDIE,envir=.GlobalEnv)    
   	    print(SITE_ETUDIE)
	      tkdestroy(aa)
      }
      OK.but <-tkbutton(aa,text="   OK   ",command=OnOK)
      tkbind(entry.Year, "<Return>",OnOK)
      tkgrid(OK.but)
      tkfocus(aa)
      tkwait.window(aa)
      tkdestroy(aa)
} 

################################################################################
## Nom     : choixRepertoireSortie.f
## Objet   : choix du répertoire de stockage des résultats par l'utilisateur
## Input   :  
## Output  :  
################################################################################

choixRepertoireSortie.f = function() {
    repertoire_resultats=tclvalue(tkchooseDirectory(title="Sélectionnez le répertoire où seront stockés les résultats", initialdir=getwd()))
    setwd(repertoire_resultats)
    assign("repertoire_resultats",repertoire_resultats,envir=.GlobalEnv)           
}
    
################################################################################
## Nom     : import_Donnees.f
## Objet   : importation des donnees, 
##           choix du repertoire de resultats, 
##           verification des fichiers / donnees
##           ecriture du rapport de description des données
## Input   :  
## Output  :  
################################################################################

import_Donnees.f = function() {
    initialisation.f()
    repertoire_donnees=tclvalue(tkchooseDirectory(title="Sélectionnez le répertoire de travail où se trouvent les données de campagne.", initialdir=getwd()))
    setwd(repertoire_donnees)
    assign("repertoire_donnees",repertoire_donnees,envir=.GlobalEnv)               
    IMPORTATION_FICHIERS <- "O"
    assign("IMPORTATION_FICHIERS",IMPORTATION_FICHIERS,envir=.GlobalEnv)       
    saisie_nom_site.f()
    tkwm.deiconify(tm)
    #choixRepertoireSortie.f() #choix du repertoire de stockage des resultats
    #tkwm.deiconify(tm) 
    tkdelete(txt_principal, "1.0", "end")        
    tkinsert(txt_principal,"end","Import des données en cours...")  
    tkconfigure(tm,cursor="watch")    
    lecture_fichiers.f()   
    verification_fichiers.f()    
    
    if (format_fichiers_OK=="O") {
       tkdelete(txt_principal, "1.0", "end")    
       tkinsert(txt_principal,"end","Importation réussie !")    
       tkconfigure(tm,cursor="arrow")         
    #   tkmessageBox(message="Importation réussie",icon="warning",type="ok") 
    }
    tkwm.deiconify(tm)  
    test_difference_interannuelle.f(capt)
    test_difference_mois.f(capt)
    nom_Jeu_De_Donnees <- paste(SITE_ETUDIE, sep="")
    if (! paste("Description_", nom_Jeu_De_Donnees,".odt", sep="") %in% list.files(repertoire_donnees)) {          
       odfWeave(paste(repertoire_modele,"/modele_description_jeu_de_donnees.odt",sep=""),paste(repertoire_donnees,"/Description_", nom_Jeu_De_Donnees,".odt",sep=""))              
    } else {
       tkmessageBox(message="ATTENTION, le rapport de description existe déjà",icon="warning",type="ok")        
    } 
    tkwm.deiconify(tm)  
    transformation_ligne_individu.f()      
} #fin import_Donnees

################################################################################
## Nom     : Avant_Apres.f
## Objet   :  
## Input   :  
## Output  :  
################################################################################

Avant_Apres.f = function() {

  #filtre sur l'annee
  choix_Annee_Mise_En_Reserve.f() 
  ANNEE_MISE_EN_RESERVE <- as.numeric(ANNEE_MISE_EN_RESERVE)  	 

#si l'année de mise en réserve est supérieure ou égale à l'année d'échantillonnage la plus ancienne
if (ANNEE_MISE_EN_RESERVE >= min(trait$Annee)) {  
  
  # Separation des fichiers before and after
  capt_Un <- data.frame(capt_orig[capt_orig$Annee <= ANNEE_MISE_EN_RESERVE,]) 
  capt_Deux <- data.frame(capt_orig[capt_orig$Annee > ANNEE_MISE_EN_RESERVE,])
  
  #filtre sur le(s) mois communs aux jeux de donnees etudies
  listeMois <- unique(capt_orig$Mois)
  listeMoisCommuns <- subset(listeMois, listeMois %in% capt_Un$Mois & listeMois %in% capt_Deux$Mois)
  choix_Mois.f(listeMoisCommuns) 
  
  capt_Un <- subset(capt_Un, Mois %in% MOIS_ETUDIES)
  capt_Deux <- subset(capt_Deux, Mois %in% MOIS_ETUDIES)
  assign("capt_Un",capt_Un,envir=.GlobalEnv)    
  assign("capt_Deux",capt_Deux,envir=.GlobalEnv)
      
  #ajout du champ time permettant de distinguer avant / apres
  capt2 <- data.frame(rbind(cbind(capt_Un,time=c(rep(1,nrow(capt_Un)))),cbind(capt_Deux,time=c(rep(2,nrow(capt_Deux)))))) 
  capt2$Trait <- as.character(capt2$Trait) #cas ou l'identifiant de trait n'est pas numerique (exemple Bamboung)
  assign("capt2",capt2,envir=.GlobalEnv)
    
  size_Un <- data.frame(size[size$Annee <= ANNEE_MISE_EN_RESERVE,])   
  size_Deux <- data.frame(size[size$Annee > ANNEE_MISE_EN_RESERVE,])
  
  size_Un <- subset(size_Un, Mois %in% MOIS_ETUDIES)
  size_Deux <- subset(size_Deux, Mois %in% MOIS_ETUDIES)    
  assign("size_Un",size_Un,envir=.GlobalEnv)  
  assign("size_Deux",size_Deux,envir=.GlobalEnv)  
      
  #ajout du champ time permettant de distinguer avant / apres  
  size2 <- data.frame(rbind(cbind(size_Un,time=c(rep(1,nrow(size_Un)))),cbind(size_Deux,time=c(rep(2,nrow(size_Deux)))))) 
  size2$Trait <- as.character(size2$Trait) #cas ou l'identifiant de trait n'est pas numerique (exemple Bamboung)  
  assign("size2",size2,envir=.GlobalEnv)
  
# Creation des tables de contingence
  capt_Un.mat <- contingence.fct(capt_Un)
  capt_Deux.mat <- contingence.fct(capt_Deux) 
  assign("capt_Un.mat",capt_Un.mat,envir=.GlobalEnv)
  assign("capt_Deux.mat",capt_Deux.mat,envir=.GlobalEnv)   
  
  Ltot <- names(which(summary(capt2$Espece)>2)) #especes observees au moins sur 2 traits
  assign("Ltot", Ltot,envir=.GlobalEnv)  
  Ltot_size <- subset(Ltot, Ltot %in% size$Espece)   
  assign("Ltot_size",Ltot_size,envir=.GlobalEnv)   

  listeEspecesCommunesCapt <- subset(Ltot, Ltot %in% capt_Un$Espece & Ltot %in% capt_Deux$Espece)
  assign("listeEspecesCommunesCapt",listeEspecesCommunesCapt,envir=.GlobalEnv)
  listeEspecesCommunesSize <- subset(Ltot, Ltot %in% size_Un$Espece & Ltot %in% size_Deux$Espece)
  assign("listeEspecesCommunesSize",listeEspecesCommunesSize,envir=.GlobalEnv)  
  listeEspecesManquantes <- subset(Ltot, ! Ltot %in% listeEspecesCommunesCapt)  
  assign("listeEspecesManquantes",listeEspecesManquantes,envir=.GlobalEnv)

# test que les 2 jeux de données contiennent bien des données
  if (length(unique(capt_Un$Annee))!=0 && length(unique(capt_Deux$Annee))!=0) {

     if (length(unique(capt_Un$Annee))==1){
        print("Une seule année présente dans le jeu de données Before")
     }
     if (length(unique(capt_Deux$Annee))==1){
        print("Une seule année présente dans le jeu de données After")  
     }

     titre <- paste(SITE_ETUDIE,"- Avant_Apres - ",ANNEE_MISE_EN_RESERVE)     
     titre1 <- "Before"
     titre2 <- "After"
     
     tkdelete(txt_principal, "1.0", "end")       
     tkinsert(txt_principal, "end", "Calcul des indicateurs...")         
     tkconfigure(tm,cursor="watch")  
          
     # Graphiques   
     if (OBJECTIF_ETUDIE == "P") {  
        Indicateurs_Population_Peche.f(indice_Time, indice_Time2, titre, titre1, titre2)             
     #   Indicateurs_Communaute_Peche.f(indice_Time, indice_Time2, titre, titre1, titre2)
     } else {        
        Indicateurs_Population_Conservation.f(indice_Time, indice_Time2, titre, titre1, titre2) 
     #   Indicateurs_Communaute_Conservation.f(indice_Time, indice_Time2, titre, titre1, titre2)           
     }        
  
     #anova indicateurs population
     Anova_Indicateurs_Population.f(capt2, size2, titre, indice_Time, indice_Time2)
   
     #calcul des scenario (anova indicateurs communaute)
     Anova_Indicateurs_Communaute.f(capt_Un.mat, capt_Deux.mat, capt2, size2, titre, titre1, titre2, indice_Time, indice_Time2)
     
     #Diagnostic
     Diagnostic_Final_Temporel.f(scen, titre)     

     nom_Fichier_Sortie <- paste(titre,"_mois_",toString(MOIS_ETUDIES),".odt", sep="")
     
     tkinsert(txt_principal, "end", "Calcul des indicateurs terminé")     
     tkdelete(txt_principal, "1.0", "end")                 
     tkconfigure(tm,cursor="arrow")                 
     tkinsert(txt_principal, "end", "Ecriture du rapport en cours...")   
            
     if (OBJECTIF_ETUDIE == "P") {  
        Radar_Plot_Peche.f(capt2, size2, titre)
        if (! paste("Resultats_Peche_", nom_Fichier_Sortie, sep="") %in% list.files()) {        
           odfWeave(paste(repertoire_modele,"/modele_peche_avant_apres.odt",sep=""),paste("Resultats_Peche_", nom_Fichier_Sortie,sep=""))         
           tkdelete(txt_principal, "1.0", "end")            
           tkinsert(txt_principal, "end", "Ecriture terminée")            
        } else {
           tkmessageBox(message="ATTENTION, ce rapport existe déjà",icon="warning",type="ok")        
        }           
     } else {     
        Radar_Plot_Conservation.f(capt_Un.mat, capt_Deux.mat, capt2, size2, titre)        
        if (! paste("Resultats_Conservation_", nom_Fichier_Sortie, sep="") %in% list.files()) {                
           odfWeave(paste(repertoire_modele,"/modele_conservation_avant_apres.odt",sep=""),paste("Resultats_Conservation_", nom_Fichier_Sortie,sep=""))         
           tkdelete(txt_principal, "1.0", "end")            
           tkinsert(txt_principal, "end", "Ecriture terminée")             
        } else {
           tkmessageBox(message="ATTENTION, ce rapport existe déjà",icon="warning",type="ok")        
        }        
     }  
     dev.off() 
  } else {
       tkmessageBox(message="ATTENTION, un des 2 jeux de donnees est vide.",icon="warning",type="ok")
  } #fin test fichier vide
     } else {
       tkmessageBox(message="Analyse impossible car toutes les donnees sont posterieures a l annee de mise en reserve",icon="warning",type="ok")   
   }
} # fin Avant_Apres.f

################################################################################
## Nom     : T1_T2.f
## Objet   : comparaison du groupe d'années T1 au groupe d'années T2 
## Input   :  
## Output  :  
################################################################################ 
T1_T2.f = function() {
   
  #selection des 2 annees a comparer
  choixDeuxGroupesAnnees.f()
      	   
  # Selection des donnees du groupe d'années 1 et du groupe d'années 2
  capt_Un <- data.frame(capt_orig[capt_orig$Annee %in% GROUPE_ANNEES_1,])
  capt_Deux <- data.frame(capt_orig[capt_orig$Annee %in% GROUPE_ANNEES_2,])	   
  	   
  #filtre sur le(s) mois communs aux jeux de donnees etudies
  listeMois <- unique(capt_orig$Mois)
  listeMoisCommuns <- subset(listeMois, listeMois %in% capt_Un$Mois & listeMois %in% capt_Deux$Mois)
  if (length(listeMoisCommuns) > 0) {
  choix_Mois.f(listeMoisCommuns) 
  
  capt_Un <- subset(capt_Un, Mois %in% MOIS_ETUDIES)
  capt_Deux <- subset(capt_Deux, Mois %in% MOIS_ETUDIES)  
  assign("capt_Un",capt_Un,envir=.GlobalEnv)  
  assign("capt_Deux",capt_Deux,envir=.GlobalEnv)
  
  #ajout du champ time permettant de distinguer T1 / T2
  capt2 <- data.frame(rbind(cbind(capt_Un,time=c(rep(1,nrow(capt_Un)))),cbind(capt_Deux,time=c(rep(2,nrow(capt_Deux)))))) 
  assign("capt2",capt2,envir=.GlobalEnv) 
  
  size_Un <- data.frame(size[size$Annee %in% GROUPE_ANNEES_1,])  
  size_Deux <- data.frame(size[size$Annee %in% GROUPE_ANNEES_2,])

  size_Un <- subset(size_Un, Mois %in% MOIS_ETUDIES)
  size_Deux <- subset(size_Deux, Mois %in% MOIS_ETUDIES)  
  assign("size_Un",size_Un,envir=.GlobalEnv)  
  assign("size_Deux",size_Deux,envir=.GlobalEnv)  
    
  #ajout du champ time permettant de distinguer avant / apres  
  size2 <- data.frame(rbind(cbind(size_Un,time=c(rep(1,nrow(size_Un)))),cbind(size_Deux,time=c(rep(2,nrow(size_Deux)))))) 
  assign("size2",size2,envir=.GlobalEnv)
    
  # Creation des tables de contingence
  capt_Un.mat <- contingence.fct(capt_Un)
  capt_Deux.mat <- contingence.fct(capt_Deux) 
  assign("capt_Un.mat",capt_Un.mat,envir=.GlobalEnv)
  assign("capt_Deux.mat",capt_Deux.mat,envir=.GlobalEnv)      

  Ltot <- names(which(summary(capt2$Espece)>2)) #especes observees au moins sur 2 traits
  assign("Ltot", Ltot,envir=.GlobalEnv)  
  Ltot_size <- subset(Ltot, Ltot %in% size$Espece)   
  assign("Ltot_size",Ltot_size,envir=.GlobalEnv) 
  
  listeEspecesCommunesCapt <- subset(Ltot, Ltot %in% capt_Un$Espece & Ltot %in% capt_Deux$Espece)
  assign("listeEspecesCommunesCapt",listeEspecesCommunesCapt,envir=.GlobalEnv)
  listeEspecesCommunesSize <- subset(Ltot, Ltot %in% size_Un$Espece & Ltot %in% size_Deux$Espece)
  assign("listeEspecesCommunesSize",listeEspecesCommunesSize,envir=.GlobalEnv)  
  listeEspecesManquantes <- subset(Ltot, ! Ltot %in% listeEspecesCommunesCapt)  
  assign("listeEspecesManquantes",listeEspecesManquantes,envir=.GlobalEnv)
      
  if (length(unique(capt_Un$Annee))!=0 && length(unique(capt_Deux$Annee))!=0) {
     
     titre <- paste(SITE_ETUDIE," - (",trim(toString(GROUPE_ANNEES_1)),") vs (",trim(toString(GROUPE_ANNEES_2)),") - Mois_",trim(toString(MOIS_ETUDIES)) , sep="")
     titre1 <- trim(toString(GROUPE_ANNEES_1))
     titre2 <- trim(toString(GROUPE_ANNEES_2))
     
     tkdelete(txt_principal, "1.0", "end")       
     tkinsert(txt_principal, "end", "Calcul des indicateurs...") 
     tkconfigure(tm,cursor="watch")  
               
     # Graphiques   
     if (OBJECTIF_ETUDIE == "P") {  
        Indicateurs_Population_Peche.f(indice_Time, indice_Time2, titre, titre1, titre2)             
     #   Indicateurs_Communaute_Peche.f(indice_Time, indice_Time2, titre, titre1, titre2)
     } else {        
        Indicateurs_Population_Conservation.f(indice_Time, indice_Time2, titre, titre1, titre2) 
        #Indicateurs_Communaute_Conservation.f(indice_Time, indice_Time2, titre, titre1, titre2)           
     }        

     #anova indicateurs population
     Anova_Indicateurs_Population.f(capt2, size2, titre, indice_Time, indice_Time2)
   
     #anova indicateurs communaute(calcul des scenario)
     Anova_Indicateurs_Communaute.f(capt_Un.mat, capt_Deux.mat, capt2, size2, titre, titre1, titre2, indice_Time, indice_Time2)
     
     #Diagnostic temporel 
     Diagnostic_Final_Temporel.f(scen, titre)     
 
     nom_Fichier_Sortie <- paste(titre,".odt", sep="")

     tkinsert(txt_principal, "end", "Calcul des indicateurs terminé")     
     tkdelete(txt_principal, "1.0", "end")                 
     tkconfigure(tm,cursor="arrow")                
     tkinsert(txt_principal, "end", "Ecriture du rapport en cours...")    
              
     if (OBJECTIF_ETUDIE == "P") {  
        Radar_Plot_Peche.f(capt2, size2, titre)
        if (! paste("Resultats_Peche_", nom_Fichier_Sortie, sep="") %in% list.files()) {
           odfWeave(paste(repertoire_modele,"/modele_peche_T1_T2.odt",sep=""),paste("Resultats_Peche_", nom_Fichier_Sortie, sep=""))         
           tkdelete(txt_principal, "1.0", "end")            
           tkinsert(txt_principal, "end", "Ecriture terminée")             
        } else {
           tkmessageBox(message="ATTENTION, ce rapport existe déjà",icon="warning",type="ok")        
        }
     } else {          
        Radar_Plot_Conservation.f(capt_Un.mat, capt_Deux.mat, capt2, size2, titre)
        if (! paste("Resultats_Conservation_", nom_Fichier_Sortie, sep="") %in% list.files()) {        
           odfWeave(paste(repertoire_modele,"/modele_conservation_T1_T2.odt",sep=""),paste("Resultats_Conservation_", nom_Fichier_Sortie,sep=""))         
           tkdelete(txt_principal, "1.0", "end")            
           tkinsert(txt_principal, "end", "Ecriture terminée")             
        } else {
           tkmessageBox(message="ATTENTION, ce rapport existe déjà",icon="warning",type="ok")        
        }           
     }
     dev.off() 
  } else {
       tkmessageBox(message="ATTENTION, un des 2 jeux de donnees est vide.",icon="warning",type="ok")
  }
  } else {
       tkmessageBox(message="Aucun mois commun entre les 2 groupes d annees",icon="warning",type="ok")
  }
} #fin T1 T2

################################################################################
## Nom     : T1_T2_T3.f
## Objet   :  
## Input   :  
## Output  :  
################################################################################ 
T1_T2_T3.f = function() {
  
if (length(unique(trait$Annee))> 2) {
  
  #selection des 3 groupes d'annees a comparer
  choixTroisGroupesAnnees.f()
  	   
  # Selection des donnees des groupes d'années 1, 2 et 3
  capt_Un <- data.frame(capt_orig[capt_orig$Annee %in% GROUPE_ANNEES_1,])
  capt_Deux <- data.frame(capt_orig[capt_orig$Annee %in% GROUPE_ANNEES_2,])  
  capt_Trois <- data.frame(capt_orig[capt_orig$Annee %in% GROUPE_ANNEES_3,])

  #filtre sur le(s) mois communs aux jeux de donnees etudies
  listeMois <- unique(capt_orig$Mois)
  listeMoisCommuns <- subset(listeMois, listeMois %in% capt_Un$Mois & listeMois %in% capt_Deux$Mois & listeMois %in% capt_Trois$Mois)
  
  if (length(listeMoisCommuns)>0) {
  choix_Mois.f(listeMoisCommuns) 
  
  capt_Un <- subset(capt_Un, Mois %in% MOIS_ETUDIES)
  capt_Deux <- subset(capt_Deux, Mois %in% MOIS_ETUDIES)  
  capt_Trois <- subset(capt_Trois, Mois %in% MOIS_ETUDIES)    
  assign("capt_Un",capt_Un,envir=.GlobalEnv)  
  assign("capt_Deux",capt_Deux,envir=.GlobalEnv)
  assign("capt_Trois",capt_Trois,envir=.GlobalEnv)  
        
 #ajout du champ time permettant de distinguer T1 / T2 / T3
  capt3 <- data.frame(rbind(cbind(capt_Un,time=c(rep(1,nrow(capt_Un)))),cbind(capt_Deux,time=c(rep(2,nrow(capt_Deux)))),cbind(capt_Trois,time=c(rep(3,nrow(capt_Trois)))))) 
  assign("capt3",capt3,envir=.GlobalEnv) 
   
  size_Un <- data.frame(size[size$Annee %in% GROUPE_ANNEES_1,])     
  size_Deux <- data.frame(size[size$Annee %in% GROUPE_ANNEES_2,])
  size_Trois <- data.frame(size[size$Annee %in% GROUPE_ANNEES_3,])

  size_Un <- subset(size_Un, Mois %in% MOIS_ETUDIES)
  size_Deux <- subset(size_Deux, Mois %in% MOIS_ETUDIES)  
  size_Trois <- subset(size_Trois, Mois %in% MOIS_ETUDIES)    
  assign("size_Un",size_Un,envir=.GlobalEnv) 
  assign("size_Deux",size_Deux,envir=.GlobalEnv)  
  assign("size_Trois",size_Trois,envir=.GlobalEnv)  
      
 #ajout du champ time permettant de distinguer T1 / T2 / T3
  size3 <- data.frame(rbind(cbind(size_Un,time=c(rep(1,nrow(size_Un)))),cbind(size_Deux,time=c(rep(2,nrow(size_Deux)))),cbind(size_Trois,time=c(rep(3,nrow(size_Trois)))))) 
  assign("size3",size3,envir=.GlobalEnv)   
  
  # Creation des tables de contingence
  capt_Un.mat <- contingence.fct(capt_Un)
  capt_Deux.mat <- contingence.fct(capt_Deux) 
  capt_Trois.mat <- contingence.fct(capt_Trois) 
  assign("capt_Un.mat",capt_Un.mat,envir=.GlobalEnv)
  assign("capt_Deux.mat",capt_Deux.mat,envir=.GlobalEnv)       
  assign("capt_Trois.mat",capt_Trois.mat,envir=.GlobalEnv)  
    
  Ltot <- names(which(summary(capt3$Espece)>2)) #especes observees au moins sur 2 traits
  assign("Ltot", Ltot,envir=.GlobalEnv)
  Ltot_size <- subset(Ltot, Ltot %in% size$Espece)   
  assign("Ltot_size",Ltot_size,envir=.GlobalEnv)    
  
  listeEspecesCommunesCapt <- subset(Ltot, Ltot %in% capt_Un$Espece & Ltot %in% capt_Deux$Espece & Ltot %in% capt_Trois$Espece)
  assign("listeEspecesCommunesCapt",listeEspecesCommunesCapt,envir=.GlobalEnv)
  listeEspecesCommunesSize <- subset(Ltot, Ltot %in% size_Un$Espece & Ltot %in% size_Deux$Espece & Ltot %in% size_Trois$Espece)
  assign("listeEspecesCommunesSize",listeEspecesCommunesSize,envir=.GlobalEnv)  
  listeEspecesManquantes <- subset(Ltot, ! Ltot %in% listeEspecesCommunesCapt)  
  assign("listeEspecesManquantes",listeEspecesManquantes,envir=.GlobalEnv)
      
  if (length(unique(capt_Un$Annee))!=0 && length(unique(capt_Deux$Annee))!=0 && length(unique(capt_Trois$Annee))!=0) {

     titre <- paste(SITE_ETUDIE," - (",trim(toString(GROUPE_ANNEES_1)),") vs (",trim(toString(GROUPE_ANNEES_2)),") vs (",trim(toString(GROUPE_ANNEES_3)),") - Mois_",trim(toString(MOIS_ETUDIES)) , sep="")
     titre1 <- trim(toString(GROUPE_ANNEES_1))
     titre2 <- trim(toString(GROUPE_ANNEES_2))
     titre3 <- trim(toString(GROUPE_ANNEES_3))
     
     tkdelete(txt_principal, "1.0", "end")       
     tkinsert(txt_principal, "end", "Calcul des indicateurs...") 
     tkconfigure(tm,cursor="watch")  
               
     # Graphiques
     if (OBJECTIF_ETUDIE == "P") {                             
        Indicateurs_Population_Peche_3grp.f(indice_Time, indice_Time2, titre, titre1, titre2, titre3)
        #Indicateurs_Communaute_Peche_3grp.f(indice_Time, indice_Time2, titre, titre1, titre2, titre3)
     } else {        
        Indicateurs_Population_Conservation_3grp.f(indice_Time, indice_Time2, titre, titre1, titre2, titre3)  
        #Indicateurs_Communaute_Conservation_3grp.f(indice_Time, indice_Time2, titre, titre1, titre2, titre3)          
     }        
  
     Anova_Indicateurs_Population_3grp.f(capt3, size3, titre, indice_Time, indice_Time2)
   
     #calcul des scenario (anova indicateurs communaute)
     Anova_Indicateurs_Communaute_3grp.f(capt_Un.mat, capt_Deux.mat, capt_Trois.mat, capt3, size3, titre, titre1, titre2, titre3, indice_Time, indice_Time2)
     
     #Diagnostic
     Diagnostic_Final_Temporel.f(scen, titre)     

     nom_Fichier_Sortie <- paste(SITE_ETUDIE,"_",toString(GROUPE_ANNEES_1),"_vs_",toString(GROUPE_ANNEES_2),"_vs_",toString(GROUPE_ANNEES_3),"_mois_",toString(MOIS_ETUDIES),".odt", sep="")
     
     tkinsert(txt_principal, "end", "Calcul des indicateurs terminé")     
     tkdelete(txt_principal, "1.0", "end")                 
     tkconfigure(tm,cursor="arrow")               
     tkinsert(txt_principal, "end", "Ecriture du rapport en cours...")        
      
     if (OBJECTIF_ETUDIE == "P") {
        Radar_Plot_Peche_3grp.f(capt3, size3, titre)
        if (! paste("Resultats_Peche_", nom_Fichier_Sortie, sep="") %in% list.files()) {
           odfWeave(paste(repertoire_modele,"/modele_peche_T1_T2_T3.odt",sep=""),paste("Resultats_Peche_", nom_Fichier_Sortie,sep=""))         
           tkdelete(txt_principal, "1.0", "end")            
           tkinsert(txt_principal, "end", "Ecriture terminée")             
        } else {
           tkmessageBox(message="ATTENTION, ce rapport existe déjà",icon="warning",type="ok")        
        }        
     } else {
        Radar_Plot_Conservation_3grp.f(capt_Un.mat, capt_Deux.mat, capt_Trois.mat, capt3, size3, titre)
        if (! paste("Resultats_Conservation_", nom_Fichier_Sortie, sep="") %in% list.files()) {        
           odfWeave(paste(repertoire_modele,"/modele_conservation_T1_T2_T3.odt",sep=""),paste("Resultats_Conservation_", nom_Fichier_Sortie,sep=""))         
           tkdelete(txt_principal, "1.0", "end")            
           tkinsert(txt_principal, "end", "Ecriture terminée")             
        } else {
           tkmessageBox(message="ATTENTION, ce rapport existe déjà",icon="warning",type="ok")        
        }               
     }
     dev.off()
  } else {
       tkmessageBox(message="ATTENTION, un des 3 jeux de donnees est vide.",icon="warning",type="ok")
  } 
  } else {
       tkmessageBox(message="Analyse impossible, aucun mois commun aux 3 jeux de données.",icon="warning",type="ok")
  }    
  } else {
       tkmessageBox(message="Analyse impossible, votre jeu de donnees doit contenir au moins 2 annees.",icon="warning",type="ok")
  }     
} #fin T1 T2 T3

################################################################################
## Nom     : Interieur_Exterieur.f
## Objet   :  
## Input   :  
## Output  :  
################################################################################

Interieur_Exterieur.f = function() {

#si il y a au moins 2 strates dans le jeu de donnees
if (length(unique(trait$Strate))>1) {  

   #selection des annees a comparer
   choixUnGroupeAnnees.f()
   capt <- subset(capt_orig, Annee %in% GROUPE_ANNEES_1)
   size <- subset(size_orig, Annee %in% GROUPE_ANNEES_1)
         	   
   #filtre sur le(s) mois
   listeMoisCommuns <- unique(capt$Mois)
   choix_Mois.f(listeMoisCommuns)  
   capt <- subset(capt, Mois %in% MOIS_ETUDIES)
   size <- subset(size, Mois %in% MOIS_ETUDIES)
   assign("capt",capt,envir=.GlobalEnv)   
   assign("size",size,envir=.GlobalEnv)   
      
# Separation des fichiers int et ext  
  
   # Strate 1 : intérieur de l'AMP 
   # Strate 2 : extérieur de l'AMP    
   capt_Un <-  data.frame(capt[capt$Strate==1,])
   assign("capt_Un",capt_Un,envir=.GlobalEnv)
   capt_Deux <-  data.frame(capt[capt$Strate==2,])
   assign("capt_Deux",capt_Deux,envir=.GlobalEnv)
   
   capt2 <- data.frame(rbind(capt_Un,capt_Deux))
   capt2$Trait <- as.character(capt2$Trait) #cas ou l'identifiant de trait n'est pas numerique
   assign("capt2",capt2,envir=.GlobalEnv)
   
   size_Un <- data.frame(size[size$Strate==1,])
   assign("size_Un",size_Un,envir=.GlobalEnv)   
   size_Deux <-  data.frame(size[size$Strate==2,])
   assign("size_Deux",size_Deux,envir=.GlobalEnv)   
   
   size2 <- data.frame(rbind(size_Un,size_Deux)) 
   size2$Trait <- as.character(size2$Trait) #cas ou l'identifiant de trait n'est pas numerique 
   assign("size2",size2,envir=.GlobalEnv)   
    
   for (i in c("Trait", "Mois", "Strate")){
      capt[,i]=as.factor(capt[,i])
   }

   for(i in c("Trait", "Mois", "Strate")){
      size[,i]=as.factor(size[,i])
   }  
    
   # Creation des tables de contingence
   capt_Un.mat <- contingence.fct(capt_Un)
   capt_Deux.mat <- contingence.fct(capt_Deux) 
   assign("capt_Un.mat",capt_Un.mat,envir=.GlobalEnv)
   assign("capt_Deux.mat",capt_Deux.mat,envir=.GlobalEnv)  
   
   Ltot <- names(which(summary(capt2$Espece)>2)) #especes observees au moins sur 2 traits
   assign("Ltot", Ltot,envir=.GlobalEnv)  
   Ltot_size <- subset(Ltot, Ltot %in% size$Espece)   
   assign("Ltot_size",Ltot_size,envir=.GlobalEnv)   

   listeEspecesCommunesCapt <- subset(Ltot, Ltot %in% capt_Un$Espece & Ltot %in% capt_Deux$Espece)
   assign("listeEspecesCommunesCapt",listeEspecesCommunesCapt,envir=.GlobalEnv)
   listeEspecesCommunesSize <- subset(Ltot, Ltot %in% size_Un$Espece & Ltot %in% size_Deux$Espece)
   assign("listeEspecesCommunesSize",listeEspecesCommunesSize,envir=.GlobalEnv)  
   listeEspecesManquantes <- subset(Ltot, ! Ltot %in% listeEspecesCommunesCapt)  
   assign("listeEspecesManquantes",listeEspecesManquantes,envir=.GlobalEnv)
  
   if (length(unique(capt_Un$Annee))!=0 && length(unique(capt_Deux$Annee))!=0) {
     
     if (length(unique(capt_Un$Annee))==1){
        print("Une seule année présente dans le jeu de données Interieur")
     }
     if (length(unique(capt_Deux$Annee))==1){
        print("Une seule année présente dans le jeu de données Exterieur")  
     } 
        
     titre1 <- "Interieur"
     titre2 <- "Exterieur"
     titre <- paste(SITE_ETUDIE)  
                     
     tkdelete(txt_principal, "1.0", "end")                       
     tkinsert(txt_principal, "end", "Calcul des indicateurs...") 
     tkconfigure(tm,cursor="watch")  
                                 
     # Graphiques   
     if (OBJECTIF_ETUDIE == "P") {  
        Indicateurs_Population_Peche.f(indice_Strate, indice_Strate2, titre, titre1, titre2)             
     #   Indicateurs_Communaute_Peche.f(indice_Strate, indice_Strate2, titre, titre1, titre2)
     } else {        
        Indicateurs_Population_Conservation.f(indice_Strate, indice_Strate2, titre, titre1, titre2) 
     #   Indicateurs_Communaute_Conservation.f(indice_Strate, indice_Strate2, titre, titre1, titre2)           
     }  
       
     #anova indicateurs population
     Anova_Indicateurs_Population.f(capt2, size2, titre, indice_Strate, indice_Strate2)
   
     #calcul des scenario (anova indicateurs communaute)
     Anova_Indicateurs_Communaute.f(capt_Un.mat, capt_Deux.mat, capt2, size2, titre, titre1, titre2, indice_Strate, indice_Strate2)
            
     #Diagnostic spatial
     Diagnostic_Final_Spatial.f(scen,titre)     
    
     nom_Fichier_Sortie <- paste(SITE_ETUDIE,"_Interieur_Exterieur_",toString(GROUPE_ANNEES_1),"_mois_",toString(MOIS_ETUDIES),".odt", sep="")
     
     tkinsert(txt_principal, "end", "Calcul des indicateurs terminé")     
     tkdelete(txt_principal, "1.0", "end")                 
     tkconfigure(tm,cursor="arrow")  
     tkinsert(txt_principal, "end", "Ecriture du rapport en cours...")        
    
     if (OBJECTIF_ETUDIE == "P") {  
        Radar_Plot_Peche_Spatial.f(capt2, size2, titre)
        if (! paste("Resultats_Peche_", nom_Fichier_Sortie, sep="") %in% list.files()) {          
           odfWeave(paste(repertoire_modele,"/modele_peche_Int_Ext.odt",sep=""),paste("Resultats_Peche_", nom_Fichier_Sortie,sep=""))         
           tkdelete(txt_principal, "1.0", "end")            
           tkinsert(txt_principal, "end", "Ecriture terminée")             
        } else {
           tkmessageBox(message="ATTENTION, ce rapport existe déjà",icon="warning",type="ok")        
        }    
     } else {     
        Radar_Plot_Conservation_Spatial.f(capt_Un.mat, capt_Deux.mat, capt2, size2, titre)
        if (! paste("Resultats_Conservation_", nom_Fichier_Sortie, sep="") %in% list.files()) {                  
           odfWeave(paste(repertoire_modele,"/modele_conservation_Int_Ext.odt",sep=""),paste("Resultats_Conservation_", nom_Fichier_Sortie,sep=""))          
           tkdelete(txt_principal, "1.0", "end")            
           tkinsert(txt_principal, "end", "Ecriture terminée")             
        } else {
           tkmessageBox(message="ATTENTION, ce rapport existe déjà",icon="warning",type="ok")        
        }            
     }          
     dev.off()
   } else {
       tkmessageBox(message="ATTENTION, un des 2 jeux de donnees est vide.",icon="warning",type="ok")   
   } 
   } else {
       tkmessageBox(message="Une seule strate specifiee dans le jeu de donnees",icon="warning",type="ok")   
   }   
} #fin Interieur_Exterieur

################################################################################
## Nom     : Gradient.f
## Objet   :  
## Input   :  
## Output  :  
################################################################################

Gradient.f = function() {

#si il y a au moins 2 strates dans le jeu de donnees
if (length(unique(trait$Strate))>2) {  

   #selection des annees a comparer
   choixUnGroupeAnnees.f()
   capt <- subset(capt_orig, Annee %in% GROUPE_ANNEES_1)
   size <- subset(size_orig, Annee %in% GROUPE_ANNEES_1)
         	   
   #filtre sur le(s) mois
   listeMoisCommuns <- unique(capt$Mois)
   choix_Mois.f(listeMoisCommuns)  
   capt <- subset(capt, Mois %in% MOIS_ETUDIES)
   size <- subset(size, Mois %in% MOIS_ETUDIES)
   assign("capt",capt,envir=.GlobalEnv)   
   assign("size",size,envir=.GlobalEnv)   
   
# Separation des donnees en 3 strates  
  
   # Strate 1 : intérieur de l'AMP 
   # Strate 2 : extérieur proche de l'AMP    
   # Strate 3 : extérieur lointain de l'AMP    
   capt_Un <-  data.frame(capt[capt$Strate==1,])
   assign("capt_Un",capt_Un,envir=.GlobalEnv)
   capt_Deux <-  data.frame(capt[capt$Strate==2,])
   assign("capt_Deux",capt_Deux,envir=.GlobalEnv)
   capt_Trois <-  data.frame(capt[capt$Strate==3,])
   assign("capt_Trois",capt_Trois,envir=.GlobalEnv)
  
   capt3 <- data.frame(rbind(capt_Un,capt_Deux,capt_Trois))
   capt3$Trait <- as.character(capt3$Trait) #cas ou l'identifiant de trait n'est pas numerique
   assign("capt3",capt3,envir=.GlobalEnv)
   
   size_Un <- data.frame(size[size$Strate==1,])
   assign("size_Un",size_Un,envir=.GlobalEnv)   
   size_Deux <-  data.frame(size[size$Strate==2,])
   assign("size_Deux",size_Deux,envir=.GlobalEnv)   
   size_Trois <-  data.frame(size[size$Strate==3,])
   assign("size_Trois",size_Trois,envir=.GlobalEnv)   
   
   size3 <- data.frame(rbind(size_Un,size_Deux,size_Trois)) 
   size3$Trait <- as.character(size3$Trait) #cas ou l'identifiant de trait n'est pas numerique 
   assign("size3",size3,envir=.GlobalEnv)   
    
   for (i in c("Trait", "Mois", "Strate")){
      capt[,i]=as.factor(capt[,i])
   }

   for(i in c("Trait", "Mois", "Strate")){
      size[,i]=as.factor(size[,i])
   }  
    
   # Creation des tables de contingence
   capt_Un.mat <- contingence.fct(capt_Un)
   capt_Deux.mat <- contingence.fct(capt_Deux) 
   capt_Trois.mat <- contingence.fct(capt_Trois) 
   assign("capt_Un.mat",capt_Un.mat,envir=.GlobalEnv)
   assign("capt_Deux.mat",capt_Deux.mat,envir=.GlobalEnv)  
   assign("capt_Trois.mat",capt_Trois.mat,envir=.GlobalEnv)  
   
   Ltot <- names(which(summary(capt3$Espece)>2)) #especes observees au moins sur 2 traits
   assign("Ltot", Ltot,envir=.GlobalEnv)  
   Ltot_size <- subset(Ltot, Ltot %in% size$Espece)   
   assign("Ltot_size",Ltot_size,envir=.GlobalEnv)   

   listeEspecesCommunesCapt <- subset(Ltot, Ltot %in% capt_Un$Espece & Ltot %in% capt_Deux$Espece & Ltot %in% capt_Trois$Espece)
   assign("listeEspecesCommunesCapt",listeEspecesCommunesCapt,envir=.GlobalEnv)
   listeEspecesCommunesSize <- subset(Ltot, Ltot %in% size_Un$Espece & Ltot %in% size_Deux$Espece & Ltot %in% size_Trois$Espece)
   assign("listeEspecesCommunesSize",listeEspecesCommunesSize,envir=.GlobalEnv)  
   listeEspecesManquantes <- subset(Ltot, ! Ltot %in% listeEspecesCommunesCapt)  
   assign("listeEspecesManquantes",listeEspecesManquantes,envir=.GlobalEnv)
  
   if (length(unique(capt_Un$Annee))!=0 && length(unique(capt_Deux$Annee))!=0 && length(unique(capt_Trois$Annee))!=0) {
     
     if (length(unique(capt_Un$Annee))==1){
        print("Une seule année présente dans le jeu de données Interieur")
     }
     if (length(unique(capt_Deux$Annee))==1){
        print("Une seule année présente dans le jeu de données Exterieur proche")  
     } 
     if (length(unique(capt_Trois$Annee))==1){
        print("Une seule année présente dans le jeu de données Exterieur lointain")  
     } 
        
     titre1 <- "Interieur"
     titre2 <- "Exterieur proche"
     titre3 <- "Exterieur lointain"
     titre <- paste(SITE_ETUDIE)  

     tkdelete(txt_principal, "1.0", "end")  
     tkinsert(txt_principal, "end", "Calcul des indicateurs...") 
     tkconfigure(tm,cursor="watch")  
          
     # Graphiques
     if (OBJECTIF_ETUDIE == "P") {                             
        Indicateurs_Population_Peche_3grp.f(indice_Strate, indice_Strate2, titre, titre1, titre2, titre3)
     #   Indicateurs_Communaute_Peche_3grp.f(indice_Strate, indice_Strate2, titre, titre1, titre2, titre3)
     } else {        
        Indicateurs_Population_Conservation_3grp.f(indice_Strate, indice_Strate2, titre, titre1, titre2, titre3)  
     #   Indicateurs_Communaute_Conservation_3grp.f(indice_Strate, indice_Strate2, titre, titre1, titre2, titre3)          
     }        
  
     Anova_Indicateurs_Population_3grp.f(capt3, size3, titre, indice_Strate, indice_Strate2)
   
     #calcul des scenario (anova indicateurs communaute)
     Anova_Indicateurs_Communaute_3grp.f(capt_Un.mat, capt_Deux.mat, capt_Trois.mat, capt3, size3, titre, titre1, titre2, titre3, indice_Strate, indice_Strate2)
                 
     #Diagnostic spatial
     Diagnostic_Final_Gradient.f(scen,titre)     
    
     nom_Fichier_Sortie <- paste(SITE_ETUDIE,"_Gradient_",toString(GROUPE_ANNEES_1),"_mois_",toString(MOIS_ETUDIES),".odt", sep="")
     
     tkinsert(txt_principal, "end", "Calcul des indicateurs terminé")     
     tkdelete(txt_principal, "1.0", "end")                 
     tkconfigure(tm,cursor="arrow")                
     tkinsert(txt_principal, "end", "Ecriture du rapport en cours...")        
    
     EspLongueurT2eqT1 <- EspLongueurIeqE
     EspAbondanceT2eqT1 <- EspAbondanceIeqE
     assign("EspLongueurT2eqT1",EspLongueurT2eqT1,envir=.GlobalEnv)     
     assign("EspAbondanceT2eqT1",EspAbondanceT2eqT1,envir=.GlobalEnv)      
     
     if (OBJECTIF_ETUDIE == "P") {  
        Radar_Plot_Peche_Spatial.f(capt3, size3, titre)
        if (! paste("Resultats_Peche_", nom_Fichier_Sortie, sep="") %in% list.files()) {        
           odfWeave(paste(repertoire_modele,"/modele_peche_Gradient.odt",sep=""),paste("Resultats_Peche_", nom_Fichier_Sortie,sep=""))         
           tkdelete(txt_principal, "1.0", "end")            
           tkinsert(txt_principal, "end", "Ecriture terminée")             
        } else {
           tkmessageBox(message="ATTENTION, ce rapport existe déjà",icon="warning",type="ok")        
        }            
     } else {      
        Radar_Plot_Conservation_Gradient.f(capt_Un.mat, capt_Deux.mat, capt_Trois.mat, capt3, size3, titre)
        if (! paste("Resultats_Conservation_", nom_Fichier_Sortie, sep="") %in% list.files()) {                
           odfWeave(paste(repertoire_modele,"/modele_conservation_Gradient.odt",sep=""),paste("Resultats_Conservation_", nom_Fichier_Sortie,sep=""))          
           tkdelete(txt_principal, "1.0", "end")            
           tkinsert(txt_principal, "end", "Ecriture terminée")             
        } else {
           tkmessageBox(message="ATTENTION, ce rapport existe déjà",icon="warning",type="ok")        
        }            
     }
     dev.off()              
   } else {
       tkmessageBox(message="ATTENTION, un des 3 jeux de donnees est vide.",icon="warning",type="ok")   
   } 
   } else {
       tkmessageBox(message="Analyse impossible, votre jeu de donnees doit contenir 3 strates.",icon="warning",type="ok")
   }   
} #fin Gradient

################################################################################
## Nom     : aide.f
## Objet   :  
## Input   :  
## Output  :  
################################################################################ 
aide.f = function() {
    tkmessageBox(message="Pour toute question relative au fonctionnement de l'application écrire un message à jeremie.habasque@gmail.com",type="ok")
}
################################################################################
## Nom     :  
## Objet   : fonctions d'attribution de l'objectif et de l'approche  
## Input   :  
## Output  :  
################################################################################ 
Avant_Apres_Peche.f = function() {
if (IMPORTATION_FICHIERS == "O") {
   initialisation.f()
   OBJECTIF_ETUDIE <- "P"
   assign("OBJECTIF_ETUDIE",OBJECTIF_ETUDIE,envir=.GlobalEnv)
   APPROCHE_CHOISIE <- "T"
   assign("APPROCHE_CHOISIE",APPROCHE_CHOISIE,envir=.GlobalEnv)
#   if (ANALYSE_EN_COURS == "O") {
   choixRepertoireSortie.f()
#   } else {
      ANALYSE_EN_COURS <- "O"
      assign("ANALYSE_EN_COURS",ANALYSE_EN_COURS,envir=.GlobalEnv)   
#   }    
   Avant_Apres.f()
} else {
   tkmessageBox(message="Vous devez d'abord choisir un workspace",type="ok")
}
} 
Avant_Apres_Conservation.f = function() {
if (IMPORTATION_FICHIERS == "O") {
   initialisation.f()
   OBJECTIF_ETUDIE <- "C"
   assign("OBJECTIF_ETUDIE",OBJECTIF_ETUDIE,envir=.GlobalEnv)
   APPROCHE_CHOISIE <- "T"
   assign("APPROCHE_CHOISIE",APPROCHE_CHOISIE,envir=.GlobalEnv)
#   if (ANALYSE_EN_COURS == "O") {
   choixRepertoireSortie.f()
#   } else {
      ANALYSE_EN_COURS <- "O"
       assign("ANALYSE_EN_COURS",ANALYSE_EN_COURS,envir=.GlobalEnv)   
#   }       
   Avant_Apres.f()
} else {
   tkmessageBox(message="Vous devez d'abord choisir un workspace",type="ok")
}
} 
T1_T2_Peche.f = function() {
if (IMPORTATION_FICHIERS == "O") {
   initialisation.f()
   OBJECTIF_ETUDIE <- "P"
   assign("OBJECTIF_ETUDIE",OBJECTIF_ETUDIE,envir=.GlobalEnv)
   APPROCHE_CHOISIE <- "T"
   assign("APPROCHE_CHOISIE",APPROCHE_CHOISIE,envir=.GlobalEnv)
#   if (ANALYSE_EN_COURS == "O") {
   choixRepertoireSortie.f()
#   } else {
       ANALYSE_EN_COURS <- "O"
       assign("ANALYSE_EN_COURS",ANALYSE_EN_COURS,envir=.GlobalEnv)   
#   }       
   T1_T2.f()
} else {
   tkmessageBox(message="Vous devez d'abord choisir un workspace",type="ok")
}
} 
T1_T2_Conservation.f = function() {
if (IMPORTATION_FICHIERS == "O") {
   initialisation.f()
   OBJECTIF_ETUDIE <- "C"
   assign("OBJECTIF_ETUDIE",OBJECTIF_ETUDIE,envir=.GlobalEnv)
   APPROCHE_CHOISIE <- "T"
   assign("APPROCHE_CHOISIE",APPROCHE_CHOISIE,envir=.GlobalEnv)
#   if (ANALYSE_EN_COURS == "O") {
   choixRepertoireSortie.f()
#   } else {
     ANALYSE_EN_COURS <- "O"
      assign("ANALYSE_EN_COURS",ANALYSE_EN_COURS,envir=.GlobalEnv)   
#   }       
   T1_T2.f()
} else {
   tkmessageBox(message="Vous devez d'abord choisir un workspace",type="ok")
}
} 
T1_T2_T3_Peche.f = function() {
if (IMPORTATION_FICHIERS == "O") {
   initialisation.f()
   OBJECTIF_ETUDIE <- "P"
   assign("OBJECTIF_ETUDIE",OBJECTIF_ETUDIE,envir=.GlobalEnv)
   APPROCHE_CHOISIE <- "T"
   assign("APPROCHE_CHOISIE",APPROCHE_CHOISIE,envir=.GlobalEnv)
#   if (ANALYSE_EN_COURS == "O") {
   choixRepertoireSortie.f()
#   } else {
      ANALYSE_EN_COURS <- "O"
       assign("ANALYSE_EN_COURS",ANALYSE_EN_COURS,envir=.GlobalEnv)   
#   }       
   T1_T2_T3.f()
} else {
   tkmessageBox(message="Vous devez d'abord choisir un workspace",type="ok")
}
} 
T1_T2_T3_Conservation.f = function() {
if (IMPORTATION_FICHIERS == "O") { 
   initialisation.f()
   OBJECTIF_ETUDIE <- "C"
   assign("OBJECTIF_ETUDIE",OBJECTIF_ETUDIE,envir=.GlobalEnv)
   APPROCHE_CHOISIE <- "T"
   assign("APPROCHE_CHOISIE",APPROCHE_CHOISIE,envir=.GlobalEnv)
#   if (ANALYSE_EN_COURS == "O") {
   choixRepertoireSortie.f()
#   } else {
      ANALYSE_EN_COURS <- "O"
       assign("ANALYSE_EN_COURS",ANALYSE_EN_COURS,envir=.GlobalEnv)   
#   }       
   T1_T2_T3.f()
} else {
   tkmessageBox(message="Vous devez d'abord choisir un workspace",type="ok")
}
} 
Serie_Temporelle_Peche.f = function() {
if (IMPORTATION_FICHIERS == "O") {
   initialisation.f()
   OBJECTIF_ETUDIE <- "P"
   assign("OBJECTIF_ETUDIE",OBJECTIF_ETUDIE,envir=.GlobalEnv)
   APPROCHE_CHOISIE <- "T"
   assign("APPROCHE_CHOISIE",APPROCHE_CHOISIE,envir=.GlobalEnv)
#   if (ANALYSE_EN_COURS == "O") {
   choixRepertoireSortie.f()
#   } else {
      ANALYSE_EN_COURS <- "O"
      assign("ANALYSE_EN_COURS",ANALYSE_EN_COURS,envir=.GlobalEnv)   
#   }       
   Serie_Temporelle.f()
} else {
   tkmessageBox(message="Vous devez d'abord choisir un workspace",type="ok")
}
} 
Serie_Temporelle_Conservation.f = function() {
if (IMPORTATION_FICHIERS == "O") {
   initialisation.f()
   OBJECTIF_ETUDIE <- "C"
   assign("OBJECTIF_ETUDIE",OBJECTIF_ETUDIE,envir=.GlobalEnv)
   APPROCHE_CHOISIE <- "T"
   assign("APPROCHE_CHOISIE",APPROCHE_CHOISIE,envir=.GlobalEnv)
#   if (ANALYSE_EN_COURS == "O") {
   choixRepertoireSortie.f()
#   } else {
       ANALYSE_EN_COURS <- "O"
      assign("ANALYSE_EN_COURS",ANALYSE_EN_COURS,envir=.GlobalEnv)   
#   }       
   Serie_Temporelle.f()
} else {
   tkmessageBox(message="Vous devez d'abord choisir un workspace",type="ok")
}
}   
Interieur_Exterieur_Peche.f = function() {
if (IMPORTATION_FICHIERS == "O") {
   initialisation.f()
   OBJECTIF_ETUDIE <- "P"
   assign("OBJECTIF_ETUDIE",OBJECTIF_ETUDIE,envir=.GlobalEnv)
   APPROCHE_CHOISIE <- "S"
   assign("APPROCHE_CHOISIE",APPROCHE_CHOISIE,envir=.GlobalEnv)
#   if (ANALYSE_EN_COURS == "O") {
   choixRepertoireSortie.f()
#   } else {
      ANALYSE_EN_COURS <- "O"
      assign("ANALYSE_EN_COURS",ANALYSE_EN_COURS,envir=.GlobalEnv)   
#   }       
   Interieur_Exterieur.f()
} else {
   tkmessageBox(message="Vous devez d'abord choisir un workspace",type="ok")
}
}                                      
Interieur_Exterieur_Conservation.f = function() {
if (IMPORTATION_FICHIERS == "O") {
   initialisation.f()
   OBJECTIF_ETUDIE <- "C"
   assign("OBJECTIF_ETUDIE",OBJECTIF_ETUDIE,envir=.GlobalEnv)
   APPROCHE_CHOISIE <- "S"
   assign("APPROCHE_CHOISIE",APPROCHE_CHOISIE,envir=.GlobalEnv)
#   if (ANALYSE_EN_COURS == "O") {
   choixRepertoireSortie.f()
#   } else {
      ANALYSE_EN_COURS <- "O"
       assign("ANALYSE_EN_COURS",ANALYSE_EN_COURS,envir=.GlobalEnv)   
#   }       
   Interieur_Exterieur.f()
} else {
   tkmessageBox(message="Vous devez d'abord choisir un workspace",type="ok")
}
} 
Gradient_Peche.f = function() {
if (IMPORTATION_FICHIERS == "O") {
   initialisation.f()
   OBJECTIF_ETUDIE <- "P"
   assign("OBJECTIF_ETUDIE",OBJECTIF_ETUDIE,envir=.GlobalEnv)
   APPROCHE_CHOISIE <- "S"
   assign("APPROCHE_CHOISIE",APPROCHE_CHOISIE,envir=.GlobalEnv)
#   if (ANALYSE_EN_COURS == "O") {
   choixRepertoireSortie.f()
#   } else {
     ANALYSE_EN_COURS <- "O"
       assign("ANALYSE_EN_COURS",ANALYSE_EN_COURS,envir=.GlobalEnv)   
#   }       
   Gradient.f()
} else {
   tkmessageBox(message="Vous devez d'abord choisir un workspace",type="ok")
}
}                                      
Gradient_Conservation.f = function() {
if (IMPORTATION_FICHIERS == "O") {
   initialisation.f()
   OBJECTIF_ETUDIE <- "C"
   assign("OBJECTIF_ETUDIE",OBJECTIF_ETUDIE,envir=.GlobalEnv)
   APPROCHE_CHOISIE <- "S"
   assign("APPROCHE_CHOISIE",APPROCHE_CHOISIE,envir=.GlobalEnv)
#   if (ANALYSE_EN_COURS == "O") {
   choixRepertoireSortie.f()
#   } else {
      ANALYSE_EN_COURS <- "O"
       assign("ANALYSE_EN_COURS",ANALYSE_EN_COURS,envir=.GlobalEnv)   
#   }       
   Gradient.f()
} else {
   tkmessageBox(message="Vous devez d'abord choisir un workspace",type="ok")
}
} 
#################################################################################
## FONCTION PRINCIPALE
## Constitution du menu d'interface
#################################################################################

tm <- tktoplevel(height=500,width=800)
topMenu <- tkmenu(tm)
tkconfigure(tm,menu=topMenu)
tkwm.title(tm,"Amphore - Analyse des indicateurs bio-écologiques")

fontHeading <- tkfont.create(family="times",size=24,weight="bold",slant="italic")
fontTextLabel <- tkfont.create(family="times",size=12)
fontFixedWidth <- tkfont.create(family="courier",size=12)
tkgrid(tklabel(tm,text="Bienvenue dans l'application",font=fontTextLabel))

#logo du projet Amphore
image1 <- tclVar()
tcl("image","create","photo",image1,file=paste(repertoire_images,"/logo_amphore_couleur.gif",sep=""))
imgAsLabel <- tklabel(tm,image=image1,bg="white")

#sortie de la console
scr <- tkscrollbar(tm, repeatinterval=5, command=function(...)tkyview(txt,...))
txt_principal <- tktext(tm,bg="#FFFFCC",font="courier", height=20,width=50, yscrollcommand=function(...)tkset(scr,...))
tkinsert(txt_principal,"end","")

tkgrid(imgAsLabel,txt_principal,scr)
tkgrid.configure(scr,sticky="ns")

Objectif <- tkmenu(topMenu,tearoff=FALSE)
Peche <- tkmenu(topMenu,tearoff=FALSE)   
Conservation <- tkmenu(topMenu,tearoff=FALSE)  
PecheTemporelle <- tkmenu(topMenu,tearoff=FALSE)  
PecheSpatiale <- tkmenu(topMenu,tearoff=FALSE)   
ConservationTemporelle <- tkmenu(topMenu,tearoff=FALSE)  
ConservationSpatiale <- tkmenu(topMenu,tearoff=FALSE) 
  
##Troisieme niveau de menu

  # Menu deroulant de "spatiale" 
  tkadd(PecheSpatiale,"command",label="Interieur vs Extérieur", command = Interieur_Exterieur_Peche.f)
  tkadd(PecheSpatiale,"command",label="Gradient", command = Gradient_Peche.f)
  tkadd(ConservationSpatiale,"command",label="Interieur vs Extérieur", command = Interieur_Exterieur_Conservation.f)
  tkadd(ConservationSpatiale,"command",label="Gradient", command = Gradient_Conservation.f)  
  
  # Menu deroulant de "PecheTemporelle" 
  tkadd(PecheTemporelle,"command",label="Avant vs Après", command = Avant_Apres_Peche.f)
  tkadd(PecheTemporelle,"command",label="T1 vs T2", command = T1_T2_Peche.f)
  tkadd(PecheTemporelle,"command",label="T1 vs T2 vs T3", command = T1_T2_T3_Peche.f)  
  tkadd(PecheTemporelle,"command",label="Série Temporelle", command = Serie_Temporelle_Peche.f)
  # Menu deroulant de "ConservationTemporelle"   
  tkadd(ConservationTemporelle,"command",label="Avant vs Après", command = Avant_Apres_Conservation.f)
  tkadd(ConservationTemporelle,"command",label="T1 vs T2", command = T1_T2_Conservation.f)
  tkadd(ConservationTemporelle,"command",label="T1 vs T2 vs T3", command = T1_T2_T3_Conservation.f)  
  tkadd(ConservationTemporelle,"command",label="Série Temporelle", command = Serie_Temporelle_Conservation.f)  

##Deuxieme niveau de menu
  tkadd(Peche,"cascade",label="Analyse temporelle", menu=PecheTemporelle)
  tkadd(Peche,"cascade",label="Analyse spatiale", menu=PecheSpatiale)
  tkadd(Conservation,"cascade",label="Analyse temporelle", menu = ConservationTemporelle) 
  tkadd(Conservation,"cascade",label="Analyse spatiale", menu=ConservationSpatiale)
    
 # Menu deroulant de "Objectif"
  tkadd(Objectif,"cascade",label="Pêche", menu = Peche) 
  tkadd(Objectif,"cascade",label="Conservation",menu = Conservation) 
 
# Premier niveau de menu
  tkadd(topMenu,"command",label="Import des donnees", command = import_Donnees.f)
  tkadd(topMenu,"command",label="Cartographie", command = carto.f)
  tkadd(topMenu,"cascade",label="Objectif",menu=Objectif)
  tkadd(topMenu,"command",label="Aide",command=aide.f)
  tkadd(topMenu,"command",label="Quitter", command=function() tkdestroy(tm))
  
tkfocus(tm)

