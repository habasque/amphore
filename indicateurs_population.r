################################################################################
## Nom     : Indicateurs_Population_Peche.f
## Objet   : Distribution boxplot de l'abondance et des tailles par espece
## Input   :
## Output  :
################################################################################

Indicateurs_Population_Peche.f = function(indiceFacteur, indiceFacteur2, titre, titre1, titre2) {

# log(N)
for (k in 1:length(listeEspecesCommunesCapt)) {
     pdf(paste("ln(N)_Species_",titre,".pdf",sep=""))
     #pour chaque espece
     for (i in 1:length(listeEspecesCommunesCapt)) {
         id=is.element(capt2$Espece, listeEspecesCommunesCapt[i])
         boxplot(log(capt2[id,"Nombre"])~ capt2[id, indiceFacteur], col=c("blue","red"),xlab="facteur",ylab="log(n)",main=paste("Abondance",listeEspecesCommunesCapt[i],"\n",titre)) 
     }
 dev.off()
}

# taille moyenne
for (k in 1:length(listeEspecesCommunesSize)) {
     pdf(paste("Size_Species_",titre,".pdf",sep=""))
     #pour chaque espece
     for (i in 1:length(listeEspecesCommunesSize)) {
         id=is.element(size2$Espece, listeEspecesCommunesSize[i])
         boxplot(log(size2[id,"Longueur"])~ size2[id, indiceFacteur2], col=c("blue","red"),xlab="facteur",ylab="log(size)",main=paste("Taille",listeEspecesCommunesSize[i],"\n",titre))
     }
 dev.off()
}
} # fin Indicateurs_Population_Peche

################################################################################
## Nom     : Indicateurs_Population_Peche_3grp.f
## Objet   :
## Input   :
## Output  :
################################################################################

Indicateurs_Population_Peche_3grp.f = function(indiceFacteur, indiceFacteur2, titre, titre1, titre2, titre3) {

# ln(N)
for (k in 1:length(listeEspecesCommunesCapt)) {
     pdf(paste("ln(N)_Species_",titre,".pdf",sep=""))
     #pour chaque espece
     for (i in 1:length(listeEspecesCommunesCapt)) {
         #nomEspece <- List$Nom[match(listeEspecesCommunesCapt[i],List$Espece)]
         id=is.element(capt3$Espece, listeEspecesCommunesCapt[i])
         boxplot(log(capt3[id,"Nombre"])~ capt3[id, indiceFacteur], col=c("blue","red"),xlab="facteur",ylab="log(n)",main=paste("Abondance",listeEspecesCommunesCapt[i],"\n",titre)) 
     }
 dev.off()
}

# Lbar
for (k in 1:length(listeEspecesCommunesSize)) {
     pdf(paste("Size_Species_",titre,".pdf",sep=""))
     #pour chaque espece
     for (i in 1:length(listeEspecesCommunesSize)) {
         #nomEspece <- List$Nom[match(listeEspecesCommunesSize[i],List$Espece)]
         id=is.element(size3$Espece, listeEspecesCommunesSize[i])
         boxplot(log(size3[id,"Longueur"])~ size3[id, indiceFacteur2], col=c("blue","red"),xlab="facteur",ylab="log(size)",main=paste("Taille",listeEspecesCommunesSize[i],"\n",titre))          
     }
 dev.off()
}
} # fin Indicateurs_Population_Peche_3grp

################################################################################
## Nom     : Indicateurs_Population_Peche_TS.f
## Objet   : Distribution boxplot de l'abondance et des tailles par espece
## Input   :
## Output  :
################################################################################

Indicateurs_Population_Peche_TS.f = function(capt, size, titre) {

# log(N)
for (k in 1:length(listeEspecesCommunesCapt)) {
     pdf(paste("log(N)_Species_",titre,".pdf",sep=""))
     #pour chaque espece commune de la serie de donnees
     for (i in 1:length(listeEspecesCommunesCapt)) {
         id_espece=is.element(capt$Espece, listeEspecesCommunesCapt[i])
         dfr <- data.frame(capt[id_espece,"Nombre"],capt[id_espece,"Annee"])
         plot(log(capt[id_espece,"Nombre"]) ~ capt[id_espece,"Annee"], data = dfr, xlab= "Annee", ylab = "log(Nombre)", 
         main = paste("Abondance",listeEspecesCommunesCapt[i],"\n",titre), col = "blue")
         fm <- lm(log(capt[id_espece,"Nombre"]) ~ capt[id_espece,"Annee"], data = dfr)         
         #abline(fm, col="red")     
         x1 <- min(capt[id_espece,"Annee"])
         x2 <- max(capt[id_espece,"Annee"])         
         y1 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*min(capt[id_espece,"Annee"]))
         y2 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*max(capt[id_espece,"Annee"]))
         lines(c(x1,x2),c(y1,y2), col="red")             
         text(min(capt$Annee),max(log(capt[id_espece,"Nombre"])), paste("Pente :", round(fm$coefficients[[2]][1],3)) , col=2, adj=c(0,1))       
     }
 dev.off()
}

# Longueur moyenne
for (k in 1:length(listeEspecesCommunesSize)) {
     pdf(paste("Size_Species_",titre,".pdf",sep=""))
     #pour chaque espece
     for (i in 1:length(listeEspecesCommunesSize)) {
         id_espece=is.element(size$Espece, listeEspecesCommunesSize[i])
         dfr <- data.frame(size[id_espece,"Longueur"],size[id_espece,"Annee"])
         plot(log(size[id_espece,"Longueur"]) ~ size[id_espece,"Annee"], data = dfr, xlab= "Annee", ylab = "log(Longueur)", 
         main = paste("Taille",listeEspecesCommunesSize[i],"\n",titre), col = "blue")
         fm <- lm(log(size[id_espece,"Longueur"]) ~ size[id_espece,"Annee"], data = dfr)         
         #abline(fm, col="red") 
         x1 <- min(size[id_espece,"Annee"])
         x2 <- max(size[id_espece,"Annee"])         
         y1 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*min(size[id_espece,"Annee"]))
         y2 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*max(size[id_espece,"Annee"]))
         lines(c(x1,x2),c(y1,y2), col="red")                   
         text(min(size$Annee),max(log(size[id_espece,"Longueur"])), paste("Pente :", round(fm$coefficients[[2]][1],3)) , col=2, adj=c(0,1))       
     }
 dev.off()
}
} # fin Indicateurs_Population_Peche_TS

################################################################################
## Nom     : Indicateurs_Population_Conservation.f
## Objet   :   identique à Indicateurs_Population_Peche.f
## Input   :
## Output  :
################################################################################

Indicateurs_Population_Conservation.f = function(indiceFacteur, indiceFacteur2, titre, titre1, titre2) {
    Indicateurs_Population_Peche.f(indiceFacteur, indiceFacteur2, titre, titre1, titre2)
}

################################################################################
## Nom     : Indicateurs_Population_Conservation_TS.f
## Objet   :   identique à Indicateurs_Population_Peche_TS.f
## Input   :
## Output  :
################################################################################

Indicateurs_Population_Conservation_TS.f = function(capt, size, titre) {
    Indicateurs_Population_Peche_TS.f(capt, size, titre)
}

################################################################################
## Nom     : Indicateurs_Population_Conservation_3grp.f
## Objet   :   identique à Indicateurs_Population_Peche_3grp.f
## Input   :
## Output  :
################################################################################

Indicateurs_Population_Conservation_3grp.f = function(indiceFacteur, indiceFacteur2, titre, titre1, titre2, titre3) {
    Indicateurs_Population_Peche_3grp.f(indiceFacteur, indiceFacteur2, titre, titre1, titre2, titre3)
}