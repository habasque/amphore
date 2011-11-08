################################################################################
## Nom     : lecture_fichiers.f
## Objet   : lecture des fichiers
## Input   : CibleVulnerable.csv, tailles.csv, traits.csv, captures.csv
## Output  :
################################################################################

lecture_fichiers.f = function() {

#1- Lecture des Scenarios
# Le fichier doit comporter les variables suivantes:
# Approche;Objectif;Scenario;CodeScen;Processus
# Scenario *** IMPORTANT: l'ordre des indicateurs doit demeurer le même ****
  # [peche]: ln(N); Div; Vuln
  # [cons]: ln(N); Lbar; Cible
   Scenario <- read.csv2(paste(repertoire_Scenario,"/scenario_rsufi.csv",sep=""), header = TRUE, sep = ";", dec=".")
   assign("Scenario",Scenario,envir=.GlobalEnv)

#2- Fichiers de donnees
#
# Liste des especes Cibles et/ou vulnerables, et/ou predatrices pour le site etudie
# Le fichier doit comporter les variables suivantes
# Espece;Nom;L1;L2;L3;large
# tous les champs doivent etre presents mais :
# Espece: code R-sufi, essentiel
# Nom: nom d'espece en clair, important
# L1: est cible (0 ou 1), essentiel
# L2: est vulnerable (0 ou 1), essentiel
# L3: ? (0 ou 1), inutile ici
# large: est predateur (0 ou 1), essentiel
   Especes_Cible_Vulnerable <- read.csv2(paste(repertoire_donnees,"/cible_vulnerable.csv",sep=""), header = TRUE, sep = ";", dec=".")
   assign("Especes_Cible_Vulnerable",Especes_Cible_Vulnerable,envir=.GlobalEnv)

   reftaxa <- read.csv2(paste(repertoire_refTax,"/ref_tax_amp.csv",sep=""), header = TRUE, sep = ";", dec=".") # par exemple "Reftax_AMP.csv"
   assign("reftaxa",reftaxa,envir=.GlobalEnv)

# Lecture du fichier des traits
# Le fichier doit comporter les variables suivantes
# Campagne;Annee;Mois;Trait;Strate;SurfaceBalayee;Lat;Long;Station
# tous les champs doivent etre presents mais :
# Campagne: nom de la campagne, important
# Annee: au format aaaa, essentiel
# Mois: au format numerique (1 a 12), essentiel
# Trait:  coup de peche, essentiel (doit etre unique pour chaque ligne dans ce fichier)
# Strate: code de position de la zone (par exemple int/ext code 1 et 2), essentiel
# Surface balayee: surface couverte par l'engin pour chaque trait (en km2), essentiel
# Lat: latitude en degres decimaux (en negatif pour le sud), essentiel
# Long: longitude en degres decimaux (en negatif pour l'ouest), essentiel
# Station: nom ou code de la station de peche, important (rajoute par LTdM)
   trait <- read.csv2(paste(repertoire_donnees,"/traits.csv",sep=""), header = TRUE, sep = ";", dec=".", stringsAsFactors = FALSE)
   if (dim(trait)[2]>1) {
      # selection des champs necessaires
      trait <- subset(trait,select=c(Campagne,Annee,Mois,Trait,Strate,SurfaceBalayee,Latitude,Longitude,Station)) 
      assign("trait",trait,envir=.GlobalEnv)
   } else {
      tkmessageBox(message="ATTENTION, le fichier traits n est pas au bon format, supprimez les apostrophes de debut et fin de ligne.",icon="warning",type="ok")   
   }
# Lecture du fichier des tailles
# Le fichier doit comporter les variables suivantes
# Trait;Espece;Sexe;Maturite;Longueur;Nombre;Poids
# tous les champs doivent etre presents mais :
# Trait: coup de peche, essentiel
# Espece: code R-sufi, essentiel
     # si le nom de l'espece est en clair il faut le changer par le code (cf. ci-dessous)
# Sexe: important
# Maturite: important
# Longueur: classe de taille, essentiel
# Nombre: nombre d'individus de l'espece dans chaque classe de taille, essentiel
# Poids: poids de tous les individus de l'espece dans chaque classe de taille, important
   taille <- read.csv2(paste(repertoire_donnees,"/tailles.csv",sep=""), header = TRUE, sep = ";", dec=".",colClasses= "character")
   if (dim(taille)[2]>1) {   
      taille <- subset(taille,select=c(Trait,Espece,Sexe,Maturite,Longueur,Nombre,Poids)) # selection des champs necessaires
      taille$Longueur <- as.numeric(taille$Longueur)
      taille$Nombre <- as.numeric(taille$Nombre)
      taille$Poids <- as.numeric(taille$Poids)    
      taille$Espece <- as.factor(taille$Espece)  
      assign("taille",taille,envir=.GlobalEnv)
   } else {
      tkmessageBox(message="ATTENTION, le fichier taille n est pas au bon format, supprimez les apostrophes de debut et fin de ligne.",icon="warning",type="ok")   
   }
   
# Lecture du fichier des captures
# Le fichier doit comporter les variables suivantes
# "Trait";"Espece";"Nombre";"Poids";
# tous les champs doivent etre presents mais :
# Trait:  coup de peche, essentiel
# Espece: code R-sufi, essentiel
     # si le nom de l'espece est en clair il faut le changer par le code (cf. ci-dessous)
# Nombre: nombre d'individus de chaque espece capturee, essentiel
# Poids: poids de tous les individus par espece, essentiel
   captures <- read.csv2(paste(repertoire_donnees,"/captures.csv", sep=""), header = TRUE, sep = ";", dec=".",colClasses= "character")
   if (dim(captures)[2]>1) {
      captures$Nombre <- as.numeric(captures$Nombre)
      captures$Poids <- as.numeric(captures$Poids) 
      captures$Espece <- as.factor(captures$Espece)  
      captures <- subset(captures,select=c(Trait,Espece,Nombre,Poids)) # selection des champs necessaires
      assign("captures",captures,envir=.GlobalEnv)
   } else {
      tkmessageBox(message="ATTENTION, le fichier captures n est pas au bon format, supprimez les apostrophes de debut et fin de ligne.",icon="warning",type="ok")   
   }
   
   capt <- merge(captures,trait,by.x="Trait",by.y="Trait",all.x=T,all.y=F)
   size <- merge(taille,trait,by.x="Trait",by.y="Trait",all.x=T,all.y=F)   
     
   assign("capt",capt,envir=.GlobalEnv)
   assign("size",size,envir=.GlobalEnv) 

   # Création des listes d'especes
   Ltot_avantSelection <- names(which(summary(capt$Espece)>2)) #especes observees au moins sur 2 traits
   assign("Ltot_avantSelection", Ltot_avantSelection,envir=.GlobalEnv)   
   LCible <- unique(Especes_Cible_Vulnerable[Especes_Cible_Vulnerable$L2==1,2]) #selection des especes cible
   assign("LCible", LCible,envir=.GlobalEnv)
   LVuln <- unique(Especes_Cible_Vulnerable[Especes_Cible_Vulnerable$L3==1,2]) #selection des especes vulnerable
   assign("LVuln", LVuln,envir=.GlobalEnv)
   LPred <- unique(Especes_Cible_Vulnerable[Especes_Cible_Vulnerable$large==1,2]) # selection des predateurs
   assign("LPred", LPred,envir=.GlobalEnv)

#3- Fichier des probabilites (pour le test binomial)
   probabilites <- read.csv2(paste(repertoire_probabilites,"/probabilites_test_binomial.csv",sep=""), header = TRUE, sep = ";", dec=".")
   assign("probabilites",probabilites,envir=.GlobalEnv)
} # fin lecture_fichiers

################################################################################
## Nom     : verification_fichiers.f
## Objet   : verification des fichiers importes
## Input   :
## Output  :
################################################################################

#--- Vérification du nombre de champs de chaque fichier
verification_nombre_champs.f = function() {
    if (dim(trait)[2] != 9) {
         rm(trait)
         tkmessageBox(message="ATTENTION, votre fichier 'Traits' ne comporte pas le bon nombre de champs! Corrigez-le et recommencez l'importation.",icon="warning",type="ok")
         format_fichiers_OK <- "N"
    }
    if (dim(taille)[2] != 7) {
         rm(taille)
         tkmessageBox(message="ATTENTION, votre fichier 'Tailles' ne comporte pas le bon nombre de champs! Corrigez-le et recommencez l'importation.",icon="warning",type="ok")
         format_fichiers_OK <- "N"
    }
    if (dim(captures)[2] != 4) {
         rm(captures)
         tkmessageBox(message="ATTENTION, votre fichier 'Captures' ne comporte pas le bon nombre de champs! Corrigez-le et recommencez l'importation.",icon="warning",type="ok")
         format_fichiers_OK <- "N"
    }
    assign("format_fichiers_OK",format_fichiers_OK,envir=.GlobalEnv)
}
 
#--- controle de la surface balayee
verification_surface_balayee.f = function() {
   if (length(unique(trait$SurfaceBalayee))>1) {
   #Il faut dans ce cas diviser le nombre de poissons par la surface balayée pour 
   # avoir des densités et de cette manière tous les traits seront comparables: ind/km2 
      capt$Nombre <- (capt$Nombre*0.005) / capt$SurfaceBalayee
      capt$Poids <- (capt$Poids*0.005) / capt$SurfaceBalayee      
      size$Nombre <- (size$Nombre*0.005) / size$SurfaceBalayee  

      capt$SurfaceBalayee <- 0.005
      size$SurfaceBalayee <- 0.005    
      trait$SurfaceBalayee <- 0.005  
      
      if (length(unique(size$Poids)) > 1){          
         size$Poids <- size$Poids / size$SurfaceBalayee
      }
      #il faut reassigner les fichiers modifies dans l'environnement global
      assign("size",size,envir=.GlobalEnv)
      assign("capt",capt,envir=.GlobalEnv)
      assign("trait",trait,envir=.GlobalEnv)                      
      tkmessageBox(message="La surface échantillonnée a été standardisée",icon="info",type="ok")   
   }
}    
    
#--- controle des annees et mois saisis
verification_annees_mois.f = function() {
   #controle des mois saisis 
   if (length((unique(trait$Mois) %in% c(1:12))) == 1) {
      if ((unique(unique(trait$Mois) %in% c(1:12)))==FALSE) {
         tkmessageBox(message="Tous les mois saisis sont incorrects",icon="info",type="ok")
         format_fichiers_OK <- "N"         
      }
   } else {
      if (length(unique(unique(trait$Mois) %in% c(1:12)))>1) {
         tkmessageBox(message="Un des mois saisis est incorrect",icon="info",type="ok")
         format_fichiers_OK <- "N"         
      }
   }
   #controle des annees saisies 
   if (!is.numeric(trait$Annee)) {
      tkmessageBox(message="Annee incorrecte car non numerique",icon="info",type="ok")   
      format_fichiers_OK <- "N"         
   }   
   if (trait$Annee < 1950 || trait$Annee > as.numeric(substr(Sys.Date(),1,4))) {
      tkmessageBox(message="Annee incoherente (non comprise entre 1950 et aujourd'hui)",icon="info",type="ok")   
      format_fichiers_OK <- "N"         
   } 
   assign("format_fichiers_OK",format_fichiers_OK,envir=.GlobalEnv) 
} 
   
#--- controles de la latitude et longitude
verification_coordonnees.f = function() {
   if (!is.numeric(trait$Latitude)) {
      tkmessageBox(message="Latitude incorrecte, le separateur decimal doit etre le point et le champ ne doit pas contenir d'espaces",icon="info",type="ok")   
      format_fichiers_OK <- "N"         
   } else {
       if (trait$Latitude < -60 || trait$Latitude > 60) {
          tkmessageBox(message="Latitude incorrecte, les valeurs doivent être comprises entre -60° et 60°",icon="info",type="ok")      
          format_fichiers_OK <- "N"          
       }     
   }
   if (!is.numeric(trait$Longitude)) {
       tkmessageBox(message="Longitude incorrecte, le separateur decimal doit etre le point et le champ ne doit pas contenir d'espaces",icon="info",type="ok")   
       format_fichiers_OK <- "N"       
   } else {       
       if (trait$Longitude < -180 || trait$Longitude > 180) {
           tkmessageBox(message="Longitude incorrecte, les valeurs doivent être comprises entre -180° et 180°",icon="info",type="ok")      
           format_fichiers_OK <- "N"           
       }           
   }
   assign("format_fichiers_OK",format_fichiers_OK,envir=.GlobalEnv)
}
   
#---presence des codes especes dans le referentiel
presence_code_espece_referentiel.f = function(mat) {
   # verification que l'ensemble des codes se trouve dans le ref_taxa
   nb_code_espece <- length(unique(mat$Espece))
   id_Espece <- match(mat$Espece, reftaxa$ScientificName)
   code_Espece <- reftaxa$CodeEspece[id_Espece]
   nb_libelle_espece_capt <- length(unique(code_Espece[!is.na(code_Espece)]))
   if (nb_code_espece == nb_libelle_espece) {
       mat$Espece <- code_Espece[,drop=TRUE]
   } else {
       tkmessageBox(message="Code espèce absent du référentiel",icon="warning",type="ok")
       format_fichiers_OK <- "N"
   }      
   assign("format_fichiers_OK",format_fichiers_OK,envir=.GlobalEnv)
   return(mat)
} 
#---presence des noms d'especes dans le referentiel
presence_nom_espece_referentiel.f = function(mat) {
   correspondance <- match(mat$Espece, reftaxa$ScientificName)
   non_correspondance <- mat$Espece[is.na(correspondance)]
   if (length(unique(non_correspondance))>0 ) {
       tkmessageBox(message=paste("Libellé(s) espece", toString(non_correspondance),"absent(s) du référentiel"),icon="warning",type="ok")
       format_fichiers_OK <- "N"
   }
   assign("format_fichiers_OK",format_fichiers_OK,envir=.GlobalEnv)     
}   
   
###### ----- fonction principale de verification des fichiers / donnees
verification_fichiers.f = function() {

   format_fichiers_OK <- "O"
   assign("format_fichiers_OK",format_fichiers_OK,envir=.GlobalEnv)
   
   verification_nombre_champs.f() 

#Suppression des enregistrements du fichier 'captures' dont le nombre d'individus est à zero
  if (dim(subset(captures, Nombre == 0))[1] > 0) {
     capt <- subset(capt, Nombre != 0)
     tkmessageBox(message="ATTENTION, des captures dont le nombre d'individus est à 0 ont ete supprimees.",icon="warning",type="ok")
     assign("capt",capt,envir=.GlobalEnv)
  }
#Suppression des enregistrements du fichier 'tailles' dont le nombre d'individus est à zero
  if (dim(subset(taille, Nombre == 0))[1] > 0) {
     size <- subset(size, Nombre != 0)
     tkmessageBox(message="ATTENTION, des tailles dont le nombre d'individus est à 0 ont ete supprimees.",icon="warning",type="ok")
     assign("size",size,envir=.GlobalEnv)
  }

##fichier traits
   #controle de la strate saisie
   if (!is.numeric(trait$Strate)) {
       tkmessageBox(message="Strate incorrecte car non numerique",icon="info",type="ok")   
       format_fichiers_OK <- "N"         
   }   
   if (trait$Strate < 1 || trait$Strate > 3) {
       tkmessageBox(message="Strate incoherente (non comprise entre 1 et 3)",icon="info",type="ok")   
       format_fichiers_OK <- "N"
   }  

   verification_surface_balayee.f()  
   verification_annees_mois.f()
   verification_coordonnees.f()
   
   titre <- paste(SITE_ETUDIE)

   if (format_fichiers_OK=="O") {
##fichier captures
   #controle du nombre d'individus par trait
   capturesParTraitTemp <- tapply(capt$Nombre, list(capt$Trait), sum, na.rm = T)
   capturesParTrait = as.data.frame(matrix(NA,dim(capturesParTraitTemp)[1],2))
   colnames(capturesParTrait) = c("TRAIT","NOMBRE")
   capturesParTrait$TRAIT = rep(dimnames(capturesParTraitTemp)[[1]])
   capturesParTrait$NOMBRE = as.vector(capturesParTraitTemp,"numeric")
   #dev.new()
  # par(cex=0.4,ps=24)
  # hist(capturesParTrait$NOMBRE, n=100, xlab ="Nombre d'individus", main = "Nombre d'individus par trait")
  # savePlot(filename = paste("Nombre_individus_par_trait", titre), "png")

   #controle du poids par trait
#   if (length(unique(capt$Poids)) > 1){
#   poidsParTraitTemp <- tapply(capt$Poids, list(capt$Trait), sum, na.rm = T)
#   poidsParTrait = as.data.frame(matrix(NA,dim(poidsParTraitTemp)[1],2))
#   colnames(poidsParTrait) = c("TRAIT","POIDS")
#   poidsParTrait$TRAIT = rep(dimnames(poidsParTraitTemp)[[1]])
#   poidsParTrait$POIDS = as.vector(poidsParTraitTemp,"numeric")
   #dev.new()
  # par(cex=0.4,ps=24)
  # hist(poidsParTrait$POIDS, n=100, xlab ="Poids", main = "Poids par trait")
  # savePlot(filename = paste("Poids_par_trait", titre), "png")

   #controle des poids totaux par trait pour chaque espece
#   for (i in 1:length(Ltot_avantSelection)) {
#       especeTraitee <- is.element(capt$Espece, Ltot_avantSelection[i])
#       #calcul du poids max par espece
#       q1 <- quantile(capt[especeTraitee,"Poids"], probs=c(0.25),names=FALSE, na.rm=T)
#       q3 <- quantile(capt[especeTraitee,"Poids"], probs=c(0.75),names=FALSE, na.rm=T)
#       poids_max_espece_trait = q3 + 3*(q3-q1)
#       listeTrait <- unique(capt$Trait)
#       #pour chaque trait, verif du poids total
#       for (j in 1:length(listeTrait)) {
#           traitTraite <- subset(capt[especeTraitee,], Trait == listeTrait[j])
#           if (length(traitTraite$Poids[!is.na(traitTraite$Poids)])>0) {
#           if (traitTraite$Poids> poids_max_espece_trait){
#              cat(paste("Erreur possible du poids de l'espèce", Ltot_avantSelection[i], "sur le trait", listeTrait[j],"\n"))
#          }
#          }
#       }
#   }
#}

##fichier tailles
   #controle du format du champ longueur
   if (!is.numeric(taille$Longueur)) {
       tkmessageBox(message="Longueur incorrecte, le separateur decimal doit etre le point",icon="info",type="ok")   
   }
   #controle du format du champ nombre
   if (!is.numeric(taille$Nombre)) {
       tkmessageBox(message="Nombre incorrect, le separateur decimal doit etre le point",icon="info",type="ok")   
   }
   #controle de la distribution des longueurs par espece
   listeEspeces <- sort(unique(taille$Espece))
   nb_groupes <- length(unique(taille$Espece)) / 40
   indice_debut <- 1
   indice_fin <- 40
   for (i in 1:ceiling(nb_groupes)) {
       liste_Especes_Groupe <- listeEspeces[indice_debut:indice_fin]
       especes_groupe <- subset(taille, Espece %in% liste_Especes_Groupe)
   #    dev.new()
   #    par(mar=c(5, 25, 2.5, 0.25), oma = rep(2, 4),cex=0.4, ps=24)
       especes_groupe$Espece <- as.character(especes_groupe$Espece)
   #    boxplot(especes_groupe$Longueur ~ especes_groupe$Espece, data = especes_groupe , las=1, horizontal = T, xlab = "Longueur(cm)", main = "Distribution des longueurs par espece")
       indice_fin <- indice_fin + 40
       indice_debut <- indice_debut + 40
   }

   Ltot_avantSelection_size <- subset(Ltot_avantSelection, Ltot_avantSelection %in% size$Espece)   
   assign("Ltot_avantSelection_size",Ltot_avantSelection_size,envir=.GlobalEnv)   
  
   #test par méthode des quantiles
#   erreurs_longueurs <- ""
#   for (i in 1:length(Ltot_avantSelection_size)) {
#       especeTraitee <- is.element(size$Espece, Ltot_avantSelection_size[i])
#       #calcul du la longueur max par espece
#       q1 <- quantile(size[especeTraitee,"Longueur"], probs=c(0.25),names=FALSE, na.rm=T)
#       q3 <- quantile(size[especeTraitee,"Longueur"], probs=c(0.75),names=FALSE, na.rm=T)
#       longueur_max_espece = q3 + 3*(q3-q1)
#       if (max(size[especeTraitee,"Longueur"])> longueur_max_espece){
#          cat(paste("erreur possible sur une longueur de l'espèce", Ltot_avantSelection_size[i],"\n"))          
#       }
#   }

   #controle de la distribution des poids moyen par espece
   #test si le champ poids est renseigne
 #  if (length(unique(taille$Poids)) > 1){
#      taille$PoidsMoyen <- taille$Poids / taille$Nombre
# #     par(oma = rep(2, 4),cex=0.4,ps=24)
# #     boxplot(taille$PoidsMoyen ~ taille$Espece, data = taille, las=1, horizontal = T, xlab = "Longueur(cm)", main = "Distribution des poids moyen par espece")
# #     savePlot(filename = paste("Distribution_PoidsMoyen_Par_Espece", titre), "png")
#      for (i in 1:length(Ltot_avantSelection_size)) {
#          especeTraitee <- is.element(size$Espece, Ltot_avantSelection_size[i])
#          #calcul du la longueur max par espece
#          q1 <- quantile(size[especeTraitee,"Poids"], probs=c(0.25),names=FALSE, na.rm=T)
#          q3 <- quantile(size[especeTraitee,"Poids"], probs=c(0.75),names=FALSE, na.rm=T)
#          poids_max_espece = q3 + 3*(q3-q1)
#          if (max(size[especeTraitee,"Poids"])> poids_max_espece){
#             print(paste("erreur possible sur un poids de l'espèce", Ltot_avantSelection_size[i]))
#          }
#      }
#   } else {
#      print("Aucun poids renseigné")
#   }

   #  si necessaire on change les codes especes par les libelles dans les fichiers taille et captures
   if (max(nchar(unique(as.character(capt$Espece))))<8) { #si il n'y a pas d'espaces dans le champ espece
      capt <- presence_code_espece_referentiel.f(capt)            
      assign("capt",capt,envir=.GlobalEnv)
   }  else { #sinon on verifie bien l'existence du nom scientifique dans le référentiel
      presence_nom_espece_referentiel.f(capt)
   }
   if (max(nchar(unique(as.character(size$Espece))))<8) { #si il n'y a pas d'espaces dans le champ espece   
      size <- presence_code_espece_referentiel.f(size)
      assign("size",size,envir=.GlobalEnv)      
   }  else { #sinon on verifie bien l'existence du nom scientifique dans le référentiel
      presence_nom_espece_referentiel.f(size)
   }
   presence_nom_espece_referentiel.f(Especes_Cible_Vulnerable)
   
#nb total de traits
   #print(paste("Nombre total de traits : ", length(unique(trait$Trait))))
   
   if (length(unique(trait$Trait)) != length(unique(capt$Trait))) {
      trait_aucune_captures <- subset(trait, ! Trait %in% captures$Trait)
      tkmessageBox(message=paste("Trait(s) sans capture(s)",toString(trait_aucune_captures$Trait)),icon="info",type="ok")      
   }
   
#plan d'echantillonnage : nb traits / année / mois / strate
   planEchantillonnage <- with(trait,table(Annee,Mois,Strate,SurfaceBalayee, exclude = NA))
   dimnames(planEchantillonnage)[[4]] <- paste(dimnames(planEchantillonnage)[[4]],"km²")   
   #print(planEchantillonnage)
   recap = as.data.frame(planEchantillonnage)
   #write.csv(recap,file="planEchantillonnage.csv",row.names=FALSE)

#especes presentes par annee zone: nb traits / espece / annee / strate
   EspecesAnneeZone <- with(capt,table(Espece,Annee,Strate, exclude = NA))
   #print(EspecesAnneeZone)
   #write.csv(EspecesAnneeZone,file="EspecesAnneeZone.csv",row.names=FALSE)

#affichage de la liste des espèces cibles et vulnérables
   #print(paste("Espèce(s) cible(s): ", toString(LCible)))
   #print(paste("Espèce(s) vulnérable(s): ",toString(LVuln)))
     
} # fin de if (format_fichiers_OK=="O") {

assign("format_fichiers_OK",format_fichiers_OK,envir=.GlobalEnv)

} # fin verification_fichiers.f


transformation_ligne_individu.f = function() {
   #---- IMPORTANT : on transforme les fichiers tailles de telle sorte qu'une ligne = un individu
    capt_orig <- capt
#   # par precaution preserver le fichier d'origine 
#   capt.ext <- as.data.frame(lapply(capt, function(x) rep(x, round(capt$Nombre)))) 
#   # Puisque dans ce fichier chaque ligne est un individu, je mets nt=1 partout
#   capt.ext$Nombre <- 1 
#   capt <- capt.ext   
   
   size_orig <- size
   # par precaution preserver le fichier d'origine 
   size.ext <- as.data.frame(lapply(size, function(x) rep(x, round(size$Nombre)))) 
   # Puisque dans ce fichier chaque ligne est un individu, je mets nt=1 partout
   size.ext$Nombre <- 1 
   size <- size.ext
    
#   assign("capt",capt,envir=.GlobalEnv)
   assign("capt_orig",capt_orig,envir=.GlobalEnv)   
   assign("size",size,envir=.GlobalEnv)
   assign("size_orig",size_orig,envir=.GlobalEnv)     
}

