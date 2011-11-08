################################################################################
## Nom     : Serie_Temporelle.f
## Objet   :
## Input   :
## Output  :
################################################################################
 
 
Serie_Temporelle_analyse_exploratoire.f = function() { 
## Bilan du nombre d'observations dans chaque niveau du facteur

table(capt$Annee)
table(capt$Mois)

# Analyse exploratoire des données de captures

#ANNEE

#-- abondance
CapturesAnneeTemp = tapply(capt$Nombre,list(capt$Annee),sum, na.rm = TRUE)
CapturesAnnee = as.data.frame(matrix(NA,dim(CapturesAnneeTemp)[1],2))
colnames(CapturesAnnee) = c("Annee","Nombre")
CapturesAnnee$Nombre = as.vector(CapturesAnneeTemp,"numeric")
CapturesAnnee$Annee = rep(dimnames(CapturesAnneeTemp)[[1]])

x11(width=25,height=15,pointsize=12)
#par(fg = "blue", col.axis = "blue", col.lab = "blue",col.main = "blue", col.sub = "blue", lab = c(20, 5, 7))
plot(CapturesAnnee$Annee,CapturesAnnee$Nombre,type ="l",xlab="Année",ylab="Abondance",main="Evolution des captures totales annuelle")

#-- poids
PoidsAnneeTemp = tapply(capt$Poids,list(capt$Annee),sum, na.rm = TRUE)
PoidsAnnee = as.data.frame(matrix(NA,dim(PoidsAnneeTemp)[1],2))
colnames(PoidsAnnee) = c("Annee","Poids")
PoidsAnnee$Poids = as.vector(PoidsAnneeTemp,"numeric")
PoidsAnnee$Annee = rep(dimnames(PoidsAnneeTemp)[[1]])
PoidsAnnee$Annee <- as.numeric(PoidsAnnee$Annee)

x11(width=25,height=15,pointsize=12)
#par(fg = "blue", col.axis = "blue", col.lab = "blue",col.main = "blue", col.sub = "blue", lab = c(20, 5, 7))
plot(PoidsAnnee$Annee,PoidsAnnee$Poids,type ="l",xlab="Année",ylab="Poids (en kg)",main="Evolution de la biomasse annuelle (en kg) ")

#MOIS

if (length(unique(capt$Mois))>1) {
#-- abondance
NombreAnneeMoisTemp = tapply(log(capt$Nombre),list(capt$Annee, capt$Mois),na.rm = TRUE,mean)
NombreAnneeMois = as.data.frame(matrix(NA,dim(NombreAnneeMoisTemp)[1]*dim(NombreAnneeMoisTemp)[2],3))
colnames(NombreAnneeMois) = c("Annee","Mois","Nombre")
NombreAnneeMois$Nombre = as.vector(NombreAnneeMoisTemp,"numeric")
NombreAnneeMois$Annee = rep(dimnames(NombreAnneeMoisTemp)[[1]],dim(NombreAnneeMoisTemp)[2])
NombreAnneeMois$Mois = rep(dimnames(NombreAnneeMoisTemp)[[2]], each = dim(NombreAnneeMoisTemp)[1],1)
 

x11(width=25,height=15,pointsize=12)
par(fg = "blue", col.axis = "blue", col.lab = "blue",col.main = "blue", col.sub = "blue", lab = c(20, 5, 7))
interaction.plot(x.factor = NombreAnneeMois$Annee, trace.factor = NombreAnneeMois$Mois, response = NombreAnneeMois$Nombre, fun = sum, lwd=2, trace.label = "Mois",
col=cl[seq(550,(550+(4*(length(split(NombreAnneeMois,NombreAnneeMois$Annee))-1))),by=4)], xlab = "Année", ylab = "Poids (en kg)", main= "Evolution saisonnière de l'abondance")
    

#-- poids
PoidsAnneeMoisTemp = tapply(log(capt$Poids),list(capt$Annee, capt$Mois),na.rm = TRUE,mean)
PoidsAnneeMois = as.data.frame(matrix(NA,dim(PoidsAnneeMoisTemp)[1]*dim(PoidsAnneeMoisTemp)[2],3))
colnames(PoidsAnneeMois) = c("Annee","Mois","Poids")
PoidsAnneeMois$Poids = as.vector(PoidsAnneeMoisTemp,"numeric")
PoidsAnneeMois$Annee = rep(dimnames(PoidsAnneeMoisTemp)[[1]],dim(PoidsAnneeMoisTemp)[2])
PoidsAnneeMois$Mois = rep(dimnames(PoidsAnneeMoisTemp)[[2]], each = dim(PoidsAnneeMoisTemp)[1],1)
 

x11(width=25,height=15,pointsize=12)
par(fg = "blue", col.axis = "blue", col.lab = "blue",col.main = "blue", col.sub = "blue", lab = c(20, 5, 7))
interaction.plot(x.factor = PoidsAnneeMois$Annee, trace.factor = PoidsAnneeMois$Mois, response = PoidsAnneeMois$Poids, fun = sum, lwd=2, trace.label = "Mois",
col=cl[seq(550,(550+(4*(length(split(PoidsAnneeMois,PoidsAnneeMois$Annee))-1))),by=4)], xlab = "Année", ylab = "Poids (en kg)", main= "Evolution saisonnière des biomasses")
     
}

#STRATE

if (APPROCHE_CHOISIE == "S") {
#-- abondance
x11(width=25,height=15,pointsize=12)
par(fg = "blue", col.axis = "blue", col.lab = "blue",col.main = "blue", col.sub = "blue", lab = c(20, 5, 7))
interaction.plot(x.factor = capt$Annee, trace.factor = capt$Strate, response = capt$Nombre, fun = sum, lwd=2, trace.label = "Strate",
col=cl[seq(550,(550+(4*(length(split(capt,capt$Annee))-1))),by=4)], xlab = "Année", ylab = "Abondance", main= "Evolution annuelle des captures par statut de protection")
#-- poids
x11(width=25,height=15,pointsize=12)
par(fg = "blue", col.axis = "blue", col.lab = "blue",col.main = "blue", col.sub = "blue", lab = c(20, 5, 7))
interaction.plot(x.factor = capt$Annee, trace.factor = capt$Strate, response = capt$Poids, fun = sum, lwd=2, trace.label = "Strate",
col=cl[seq(550,(550+(4*(length(split(capt,capt$Annee))-1))),by=4)], xlab = "Année", ylab = "Poids (en kg)", main= "Evolution annuelle des biomasses par statut de protection")
}
}

################################################################################
Serie_Temporelle.f = function() {

    #test si l'analyse est faisable (nombre d'années >=5)
    if (length(unique(trait$Annee))>=5){

    #analyse exploratoire 
    #Serie_Temporelle_analyse_exploratoire.f()

    #choix des années
    choixUnGroupeAnnees.f()
    
    #test si le nombre d'annees choisi est suffisant (cad >=5)
    if (length(GROUPE_ANNEES_1)>=5 ) {

    #test la continuité de la série temporelle
    if (length(which(unique(diff(GROUPE_ANNEES_1))>1)==TRUE)==0) {

    # Selection des donnees du groupe d'années selectionnees
    capt <- data.frame(capt[capt$Annee %in% GROUPE_ANNEES_1,])   
    size <- data.frame(size[size$Annee %in% GROUPE_ANNEES_1,])   
    
    #filtre sur le(s) mois communs aux annees etudiees
    croisementMoisAnnees <- data.frame(table(capt$Annee, capt$Mois))
    colnames(croisementMoisAnnees) = c("Annee","Mois","Frequence")
    moisManquants <- croisementMoisAnnees$Mois[croisementMoisAnnees$Frequence==0]
       
    listeMois <- unique(capt$Mois)
    listeMoisCommuns <- subset(listeMois, ! listeMois %in% moisManquants)

    #filtre sur le(s) mois
    choix_Mois.f(listeMoisCommuns)
    capt <- subset(capt, Mois %in% MOIS_ETUDIES)
    size <- subset(size, Mois %in% MOIS_ETUDIES)
    assign("capt",capt,envir=.GlobalEnv)      
    assign("size",size,envir=.GlobalEnv)    
        
    Ltot <- names(which(summary(capt$Espece)>2)) #especes observees au moins sur 2 traits
    assign("Ltot", Ltot,envir=.GlobalEnv)  
    Ltot_size <- subset(Ltot, Ltot %in% size$Espece)   
    assign("Ltot_size",Ltot_size,envir=.GlobalEnv) 
  
    #recherche des especes communes de la serie de donnees
    #--- capt
    croisementEspeceAnnees <- data.frame(table(capt$Espece, capt$Annee))
    colnames(croisementEspeceAnnees) = c("Espece","Annee","Frequence")
    especesManquantes <- unique(croisementEspeceAnnees$Espece[croisementEspeceAnnees$Frequence==0])
    listeEspecesCommunesCapt <- subset(Ltot, ! Ltot %in% especesManquantes)
    assign("listeEspecesCommunesCapt",listeEspecesCommunesCapt,envir=.GlobalEnv)
    #--- size
    croisementEspeceAnnees_size <- data.frame(table(size$Espece, size$Annee))
    colnames(croisementEspeceAnnees_size) = c("Espece","Annee","Frequence")
    especesManquantes_size <- unique(croisementEspeceAnnees_size$Espece[croisementEspeceAnnees_size$Frequence==0])
    listeEspecesCommunesSize <- subset(Ltot_size, ! Ltot_size %in% especesManquantes_size)
    assign("listeEspecesCommunesSize",listeEspecesCommunesSize,envir=.GlobalEnv)
        
    listeEspecesManquantes <- subset(Ltot, ! Ltot %in% listeEspecesCommunesCapt)  
    assign("listeEspecesManquantes",listeEspecesManquantes,envir=.GlobalEnv)    
    
    titre <- paste(SITE_ETUDIE," - (",trim(toString(GROUPE_ANNEES_1[1])),") - (",trim(toString(GROUPE_ANNEES_1[length(GROUPE_ANNEES_1)])),") - Mois_",trim(toString(MOIS_ETUDIES)) , sep="")

    tkdelete(txt_principal, "1.0", "end")  
    tkinsert(txt_principal, "end", "Calcul des indicateurs...") 
    tkconfigure(tm,cursor="watch")  
    
    # Graphiques   
    if (OBJECTIF_ETUDIE == "P") {  
       Indicateurs_Population_Peche_TS.f(capt, size, titre)             
    #   Indicateurs_Communaute_Peche_TS.f(titre)
    } else {        
       Indicateurs_Population_Conservation_TS.f(capt, size, titre) 
    #   Indicateurs_Communaute_Conservation_TS.f(titre)           
    }        
    
    #anova indicateurs population
    Anova_Indicateurs_Population_TS.f(capt, size, titre)
   
    #calcul des scenario (anova indicateurs communaute)
    Anova_Indicateurs_Communaute_TS.f(capt, size, titre)
     
    #Diagnostic
    Diagnostic_Final_Temporel.f(scen, titre)     
  
    nom_Fichier_Sortie <- paste(titre,"_mois_",toString(MOIS_ETUDIES),".odt", sep="")
       
    tkinsert(txt_principal, "end", "Calcul des indicateurs terminé")     
    tkdelete(txt_principal, "1.0", "end")                 
    tkconfigure(tm,cursor="arrow") 
    tkinsert(txt_principal, "end", "Ecriture du rapport en cours")        
            
    if (OBJECTIF_ETUDIE == "P") {
        Radar_Plot_Peche_TS.f(capt, size, titre)
        if (! paste("Resultats_Peche_", nom_Fichier_Sortie, sep="") %in% list.files(repertoire_resultats)) {                        
           odfWeave(paste(repertoire_modele,"/modele_peche_serie_temporelle.odt",sep=""),paste("Resultats_Peche_", titre, ".odt", sep=""))         
           tkdelete(txt_principal, "1.0", "end")            
           tkinsert(txt_principal, "end", "Ecriture terminée")                  
        } else {
           tkmessageBox(message="ATTENTION, ce rapport existe déjà",icon="warning",type="ok")        
        }          
    } else {     
        Radar_Plot_Conservation_TS.f(capt, size, titre)
        if (! paste("Resultats_Conservation_", nom_Fichier_Sortie, sep="") %in% list.files(repertoire_resultats)) {                                
           odfWeave(paste(repertoire_modele,"/modele_conservation_serie_temporelle.odt",sep=""),paste("Resultats_Conservation_", titre, ".odt", sep=""))         
           tkdelete(txt_principal, "1.0", "end")            
           tkinsert(txt_principal, "end", "Ecriture terminée")                  
        } else {
           tkmessageBox(message="ATTENTION, ce rapport existe déjà",icon="warning",type="ok")        
        }            
    }    
    dev.off() 
    } else {
       tkmessageBox(message="Série temporelle non continue",icon="warning",type="ok")        
    }
    } else {
       tkmessageBox(message="Analyse impossible, la série de données sélectionnée doit être supérieure à 5 ans",icon="warning",type="ok")
    }
    } else {
       tkmessageBox(message="Analyse impossible car série de données trop courte",icon="warning",type="ok")
    }

} #fin Serie_Temporelle