################################################################################
## Nom     : Indicateurs_Communaute_Peche.f
## Objet   : Abondance totale, longueur moyenne, abondance et taille moyenne
##           des espèces cible
## Input   :
## Output  :
################################################################################

Indicateurs_Communaute_Peche.f = function(facteur, facteur2, titre, titre1, titre2) {

# Abondance totale log(N)
dev.new()
    boxplot(log(capt2$Nombre)~capt2[,facteur], data=capt2, xlab="Facteur",ylab="Abondance (log)", 
    col=c("blue","red"), main=paste("Abondance totale - log(N)\n",titre)) 
    savePlot(filename = paste("log(N)",titre), "png")

# Longueur moyenne
dev.new()
    boxplot(log(size2$Longueur)~size2[,facteur2], data=size2, xlab="Facteur",ylab="Size (log)", 
    col=c("blue","red"), main=paste("Taille moyenne\n",titre)) 
    savePlot(filename = paste("Taille_moyenne",titre), "png") 

# Biomasse et taille moyenne des especes cibles
dev.new()    
    par(mar=c(10, 10, 10, 5),oma = c(0, 0, 10, 0), mfrow = c(1, 2), cex=0.4, font=2)
    # Abundance
    id_capt_cible=is.element(capt2$Espece, LCible)    
    boxplot(log(capt2[id_capt_cible,"Nombre"])~capt2[id_capt_cible,facteur], data=capt2, xlab="Facteur",ylab="Abondance (log)", 
    col=c("blue","red"), main="Abondance") 
    
    # Size
    id_size_cible=is.element(size2$Espece, LCible)    
    boxplot(log(size2[id_size_cible,"Longueur"])~size2[id_size_cible,facteur2], data=size2, xlab="Facteur",ylab="Taille (log)", 
    col=c("blue","red"), main="Taille moyenne") 
    mtext(paste(titre,"\nEspeces cibles"), side = 3, outer = TRUE)
    savePlot(filename = paste("Abondance_Taille_Moyenne_Especes_Cibles_",titre,sep=""),"png") 

} # fin Indicateurs_Communaute_Peche

################################################################################
## Nom     : Indicateurs_Communaute_Peche_3grp.f
## Objet   :
## Input   :
## Output  :
################################################################################

Indicateurs_Communaute_Peche_3grp.f = function(facteur, facteur2, titre, titre1, titre2, titre3) {

# Abondance totale log(N)
 dev.new()
    boxplot(log(capt3$Nombre)~capt3[,facteur], data=capt3, xlab="Facteur",ylab="Abondance (log)", 
    col=c("blue","green","red"), main=paste("Abondance totale - log(N)\n",titre)) 
    savePlot(filename = paste("log(N)",titre), "png")

# Longueur moyenne
 dev.new()
    boxplot(log(size3$Longueur)~size3[,facteur2], data=size3, xlab="Facteur",ylab="Size (log)", 
    col=c("blue","green","red"), main=paste("Taille moyenne\n",titre)) 
    savePlot(filename = paste("Taille_moyenne",titre), "png") 

# Biomasse et taille moyenne des especes cibles
dev.new()    
    par(mar=c(10, 10, 10, 5),oma = c(0, 0, 10, 0), mfrow = c(1, 2), cex=0.4, font=2)
    # Abondance
    id_capt_cible=is.element(capt3$Espece, LCible)    
    boxplot(log(capt3[id_capt_cible,"Nombre"])~capt3[id_capt_cible,facteur], data=capt3, xlab="Facteur",ylab="Abondance (log)", 
    col=c("blue","green","red"), main=paste("Abondance")) 
    
    # Taille
    id_size_cible=is.element(size3$Espece, LCible)    
    boxplot(log(size3[id_size_cible,"Longueur"])~size3[id_size_cible,facteur2], data=size3, xlab="Facteur",ylab="Taille (log)", 
    col=c("blue","green","red"), main=paste("Taille moyenne")) 
    mtext(paste(titre,"\nEspeces cibles"), side = 3, outer = TRUE, cex = 1.5)
    savePlot(filename = paste("Abondance_Taille_Moyenne_Especes_Cibles_",titre,sep=""), "png") 

}  #fin Indicateurs_Communaute_Peche_3grp.f

################################################################################
## Nom     : Indicateurs_Communaute_Peche_TS.f
## Objet   :
## Input   :
## Output  :
################################################################################

Indicateurs_Communaute_Peche_TS.f = function(titre) {

# Abondance totale log(N)
 dev.new()
    dfr <- data.frame(capt$Nombre,capt$Annee)
    plot(log(capt$Nombre) ~ capt$Annee, data = dfr, xlab= "Annee", ylab = "log(Nombre)", 
    main = paste("Abondance totale - log(N)\n",titre), col = "dark red")
    fm <- lm(log(capt$Nombre) ~ capt$Annee, data = dfr)         
    x1 <- min(capt$Annee)
    x2 <- max(capt$Annee)         
    y1 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*min(capt$Annee))
    y2 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*max(capt$Annee))        
    lines(c(x1,x2),c(y1,y2), col="red")          
    text(min(capt$Annee),max(log(capt$Nombre)), paste("Pente :", round(fm$coefficients[[2]][1],3)) , col=2, adj=c(0,1))       
    savePlot(filename = paste("log(N)",titre), "png")

# Longueur moyenne
 dev.new()
    dfr <- data.frame(size$Longueur,size$Annee)
    plot(log(size$Longueur) ~ size$Annee, data = dfr, xlab= "Annee", ylab = "log(Longueur)", 
    main = paste("Longueur moyenne - log(size)\n",titre), col = "dark red")
    fm <- lm(log(size$Longueur) ~ size$Annee, data = dfr)         
    x1 <- min(size$Annee)
    x2 <- max(size$Annee)         
    y1 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*min(size$Annee))
    y2 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*max(size$Annee))        
    lines(c(x1,x2),c(y1,y2), col="red")         
    text(min(size$Annee),max(log(size$Longueur)), paste("Pente :", round(fm$coefficients[[2]][1],3)) , col=2, adj=c(0,1))       
    savePlot(filename = paste("Taille_moyenne",titre), "png") 

# Biomasse et taille moyenne des especes cibles
dev.new()    
    par(mar=c(10, 10, 10, 5),oma = c(0, 0, 10, 0), mfrow = c(1, 2), cex=0.4, font=2)
    # Abondance
    id_capt_cible=is.element(capt$Espece, LCible)    
    dfr <- data.frame(capt[id_capt_cible,"Nombre"],capt[id_capt_cible,"Annee"])
    plot(log(capt[id_capt_cible,"Nombre"]) ~ capt[id_capt_cible,"Annee"], data = dfr, xlab= "Annee", ylab = "log(Nombre)", main ="Abondance - log(N)")
    fm <- lm(log(capt[id_capt_cible,"Nombre"]) ~ capt[id_capt_cible,"Annee"], data = dfr)         
    x1 <- min(capt[id_capt_cible,"Annee"])
    x2 <- max(capt[id_capt_cible,"Annee"])         
    y1 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*min(capt[id_capt_cible,"Annee"]))
    y2 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*max(capt[id_capt_cible,"Annee"]))        
    lines(c(x1,x2),c(y1,y2), col="red")         
    text(min(capt$Annee),max(log(capt[id_capt_cible,"Nombre"])), paste("Pente :", round(fm$coefficients[[2]][1],3)) , col=2, adj=c(0,1))       
       
    # Taille
    id_size_cible=is.element(size$Espece, LCible)    
    dfr <- data.frame(size[id_size_cible,"Longueur"],size[id_size_cible,"Annee"])
    plot(log(size[id_size_cible,"Longueur"]) ~ size[id_size_cible,"Annee"], data = dfr, xlab= "Annee", ylab = "log(Longueur)", main ="Abondance - log(N)")
    fm <- lm(log(size[id_size_cible,"Longueur"]) ~ size[id_size_cible,"Annee"], data = dfr)         
    x1 <- min(size[id_size_cible,"Annee"])
    x2 <- max(size[id_size_cible,"Annee"])         
    y1 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*min(size[id_size_cible,"Annee"]))
    y2 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*max(size[id_size_cible,"Annee"]))        
    lines(c(x1,x2),c(y1,y2), col="red")         
    text(min(size$Annee),max(log(size[id_size_cible,"Longueur"])), paste("Pente :", round(fm$coefficients[[2]][1],3)) , col=2, adj=c(0,1))       

    mtext(paste(titre,"\nEspeces cibles"), side = 3, outer = TRUE, cex = 1.5)
    savePlot(filename = paste("Abondance_Taille_Moyenne_Especes_Cibles_",titre,sep=""), "png") 

}  #fin Indicateurs_Communaute_Peche_TS.f

################################################################################
## Nom     : Indicateurs_Communaute_Conservation.f
## Objet   : Simpson et abondance des espèces vulnérables
## Input   :
## Output  :
################################################################################

Indicateurs_Communaute_Conservation.f = function(facteur, facteur2, titre,  titre1, titre2) {

# Abondance totale log(N)
 dev.new()
    boxplot(log(capt2$Nombre)~capt2[,facteur], data=capt2, xlab="Facteur",ylab="Abondance (log)", 
    col=c("blue","red"), main=paste("Abondance totale - log(N)\n",titre)) 
    savePlot(filename = paste("log(N)",titre), "png")

  # Diversité Simpson
  dev.new()
     #  returns 1-D
      simpson1 <- data.frame(diversity(capt_Un.mat,'simpson'))
      simpson2 <- data.frame(diversity(capt_Deux.mat,'simpson'))
      colnames(simpson2) <- colnames(simpson1)
      jonction = data.frame(rbind(cbind(simpson1,time=c(rep(1,nrow(simpson1)))),cbind(simpson2,time=c(rep(2,nrow(simpson2)))))) 
      colnames(jonction) <- c("diversity_simpson","times")      
      boxplot(jonction$diversity_simpson~jonction$times, prob=TRUE, xlab="Facteur", ylab="Simpson", col=c("blue","red"), 
      main=paste("Simpson -",titre)) 
      savePlot(filename = paste("Simpson",titre), "png")

  # Biomasse et taille moyenne des especes vulnérables
  dev.new()    
    par(mar=c(10, 10, 10, 5),oma = c(0, 0, 10, 0), mfrow = c(1, 2), cex=0.4, font=2)
    # Abondance
    id_capt_vuln=is.element(capt2$Espece, LVuln)    
    boxplot(log(capt2[id_capt_vuln,"Nombre"])~capt2[id_capt_vuln,facteur], data=capt2, xlab="Facteur",ylab="Abondance (log)", 
    col=c("blue","red"), main=paste("Abondance")) 
    
    # Size
    id_size_vuln=is.element(size2$Espece, LVuln)    
    boxplot(log(size2[id_size_vuln,"Longueur"])~size2[id_size_vuln,facteur2], data=size2, xlab="Facteur",ylab="Taille (log)", 
    col=c("blue","red"), main=paste("Taille moyenne")) 
    mtext(paste(titre,"\nEspeces vulnerables"), side = 3, outer = TRUE, cex = 1.5)
    savePlot(filename = paste("Abondance_Taille_Moyenne_Especes_Vulnerables_",titre,sep=""), "png") 
} # fin Indicateurs_Communaute_Conservation.f

################################################################################
## Nom     : Indicateurs_Communaute_Conservation_3grp.f
## Objet   :
## Input   :
## Output  :
################################################################################

Indicateurs_Communaute_Conservation_3grp.f = function(facteur, facteur2, titre,  titre1, titre2, titre3) {
# Abondance totale log(N)
 dev.new()
    boxplot(log(capt3$Nombre)~capt3[,facteur], data=capt3, xlab="Facteur",ylab="Abondance (log)", 
    col=c("blue","green","red"), main=paste("Abondance totale - log(N)\n",titre)) 
    savePlot(filename = paste("log(N)",titre), "png")

  # Diversité Simpson
  dev.new()
       #  returns 1-D
      simpson1 <- data.frame(diversity(capt_Un.mat,'simpson'))
      simpson2 <- data.frame(diversity(capt_Deux.mat,'simpson'))
      simpson3 <- data.frame(diversity(capt_Trois.mat,'simpson'))
      colnames(simpson2) <- colnames(simpson1)
      colnames(simpson3) <- colnames(simpson1)      
      jonction = data.frame(rbind(cbind(simpson1,time=c(rep(1,nrow(simpson1)))),cbind(simpson2,time=c(rep(2,nrow(simpson2)))),cbind(simpson3,time=c(rep(3,nrow(simpson3)))))) 
      colnames(jonction) <- c("diversity_simpson","times")      
      boxplot(jonction$diversity_simpson~jonction$times, prob=TRUE, xlab="Facteur", ylab="Simpson", 
      col=c("blue","green","red"), main=paste("Simpson -",titre)) 
      savePlot(filename = paste("Simpson",titre), "png")

  # Biomasse et taille moyenne des especes vulnérables
  dev.new()    
    par(mar=c(10, 10, 10, 5),oma = c(0, 0, 10, 0), mfrow = c(1, 2), cex=0.4, font=2)
    # Abundance
    id_capt_vuln=is.element(capt3$Espece, LVuln)    
    boxplot(log(capt3[id_capt_vuln,"Nombre"])~capt3[id_capt_vuln,facteur], data=capt3, xlab="Facteur",ylab="Abondance (log)", 
    col=c("blue","green","red"), main=paste("Abondance")) 
    
    # Size
    id_size_vuln=is.element(size3$Espece, LVuln)    
    boxplot(log(size3[id_size_vuln,"Longueur"])~size3[id_size_vuln,facteur2], data=size3, xlab="Facteur",ylab="Taille (log)", 
    col=c("blue","green","red"), main=paste("Taille moyenne")) 
    mtext(paste(titre,"\nEspeces vulnerables"), side = 3, outer = TRUE, cex = 1.5)
    savePlot(filename = paste("Abondance_Taille_Moyenne_Especes_Vulnerables_",titre,sep=""), "png") 
} # fin Indicateurs_Communaute_Conservation_3grp

################################################################################
## Nom     : Indicateurs_Communaute_Conservation_TS.f
## Objet   :
## Input   :
## Output  :
################################################################################

Indicateurs_Communaute_Conservation_TS.f = function(titre) {

# Abondance totale log(N)
 dev.new()
    dfr <- data.frame(capt$Nombre,capt$Annee)
    plot(log(capt$Nombre) ~ capt$Annee, data = dfr, xlab= "Annee", ylab = "log(Nombre)", 
    main = paste("Abondance totale - log(N)\n",titre), col = "dark red")
    fm <- lm(log(capt$Nombre) ~ capt$Annee, data = dfr)         
    x1 <- min(capt$Annee)
    x2 <- max(capt$Annee)         
    y1 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*min(capt$Annee))
    y2 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*max(capt$Annee))
    lines(c(x1,x2),c(y1,y2), col="red")         
    text(min(capt$Annee),max(log(capt$Nombre)), paste("Pente :", round(fm$coefficients[[2]][1],3)) , col=2, adj=c(0,1))       
    savePlot(filename = paste("log(N)",titre), "png")

# Simpson
 dev.new()
    # Creation de la table de contingence
    listeAnnees <- unique(capt$Annee)
    jonction <- NULL
    for (i in 1:length(unique(capt$Annee))) {
        capt_TS.mat <- contingence.fct(capt[capt$Annee == listeAnnees[i],])
        #  returns 1-D
        simpson <- data.frame(diversity(capt_TS.mat,'simpson'))
        colnames(simpson) <- c("diversity")
        jonction = data.frame(rbind(jonction,cbind(simpson,Annee=c(rep(listeAnnees[i],nrow(simpson))))))          
    }   
    dfr <- data.frame(jonction$diversity,jonction$Annee)
    plot(jonction$diversity ~ jonction$Annee, data = dfr, xlab= "Annee", ylab = "Simpson", main = paste("Simpson\n",titre), col = "dark red")
    fm <- lm(jonction$diversity ~ jonction$Annee, data = dfr)         
    x1 <- min(capt$Annee)
    x2 <- max(capt$Annee)         
    y1 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*min(capt$Annee))
    y2 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*max(capt$Annee))        
    text(min(jonction$Annee),max(jonction$diversity), paste("Pente :", round(fm$coefficients[[2]][1],3)) , col=2, adj=c(0,1))       
    savePlot(filename = paste("Simpson",titre), "png") 

# Biomasse et taille moyenne des especes vulnerables
dev.new()    
    par(mar=c(10, 10, 10, 5),oma = c(0, 0, 10, 0), mfrow = c(1, 2), cex=0.4, font=2)
    # Abondance
    id_capt_vuln=is.element(capt$Espece, LVuln)    
    dfr <- data.frame(capt[id_capt_vuln,"Nombre"],capt[id_capt_vuln,"Annee"])
    plot(log(capt[id_capt_vuln,"Nombre"]) ~ capt[id_capt_vuln,"Annee"], data = dfr, xlab= "Annee", ylab = "log(Nombre)", main ="Abondance - log(N)")
    fm <- lm(log(capt[id_capt_vuln,"Nombre"]) ~ capt[id_capt_vuln,"Annee"], data = dfr)         
    x1 <- min(capt[id_capt_vuln,"Annee"])
    x2 <- max(capt[id_capt_vuln,"Annee"])         
    y1 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*min(capt[id_capt_vuln,"Annee"]))
    y2 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*max(capt[id_capt_vuln,"Annee"]))
    lines(c(x1,x2),c(y1,y2), col="red")        
    text(min(capt$Annee),max(log(capt[id_capt_vuln,"Nombre"])), paste("Pente :", round(fm$coefficients[[2]][1],3)) , col=2, adj=c(0,1))       
       
    # Taille
    id_size_vuln=is.element(size$Espece, LVuln)    
    dfr <- data.frame(size[id_size_vuln,"Longueur"],size[id_size_vuln,"Annee"])
    plot(log(size[id_size_vuln,"Longueur"]) ~ size[id_size_vuln,"Annee"], data = dfr, xlab= "Annee", ylab = "log(Longueur)", main ="Abondance - log(N)")
    fm <- lm(log(size[id_size_vuln,"Longueur"]) ~ size[id_size_vuln,"Annee"], data = dfr)         
    x1 <- min(size[id_size_vuln,"Annee"])
    x2 <- max(size[id_size_vuln,"Annee"])         
    y1 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*min(size[id_size_vuln,"Annee"]))
    y2 <- fm$coefficients[[1]] + (fm$coefficients[[2]]*max(size[id_size_vuln,"Annee"]))        
    lines(c(x1,x2),c(y1,y2), col="red")  
    text(min(size$Annee),max(log(size[id_size_vuln,"Longueur"])), paste("Pente :", round(fm$coefficients[[2]][1],3)) , col=2, adj=c(0,1))       

    mtext(paste(titre,"\nEspeces vulnerables"), side = 3, outer = TRUE, cex = 1.5)
    savePlot(filename = paste("Abondance_Taille_Moyenne_Especes_Vulnerables_",titre,sep=""), "png") 
}  #fin Indicateurs_Communaute_Conservation_TS.f
