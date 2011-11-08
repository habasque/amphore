################################################################################
## Nom     : Anova_Indicateurs_Communaute.f
## Objet   : ANOVA one-way par permutation
## Input   :
## Output  :
################################################################################

Anova_Indicateurs_Communaute.f = function(capt_Un.mat, capt_Deux.mat, capt2, size2, titre, titre1, titre2, facteur, facteur2) {

## Indicateurs Conservation
#dev.new()
 # par(mfrow=c(2,2),oma = c(0, 0, 3, 0),mex = 0.7)
   # Abondance totale log(N)
     id_Tot=is.element(capt2$Espece, listeEspecesCommunesCapt)
     ind.trait <- tapply(capt2[id_Tot,"Nombre"], capt2[id_Tot,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(capt2[id_Tot,facteur], capt2[id_Tot,"Trait"],mean, na.rm= T)
     Abun_res <- anova.1way(log(ind.trait) ~ as.factor(fact.trait), data=capt2, nperm=999)
     assign("Abun_res",Abun_res,envir=.GlobalEnv)          
     AbunScen <- scenario.f(log(ind.trait),fact.trait)
  #   plotmeans(log(ind.trait) ~ as.factor(fact.trait),main="Abondance log(N)", ylab="log(N)",xlab=paste("p-value=",round(Abun_res$anova.table[1,5],5)))

   # Abondance des especes vulnérables
     id_Vuln=is.element(capt2$Espece, LVuln)
     ind.trait <- tapply(capt2[id_Vuln,"Nombre"], capt2[id_Vuln,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(capt2[id_Vuln,facteur], capt2[id_Vuln,"Trait"],mean, na.rm= T)
     #on verifie qu'il y ait + d'un trait et + d'un facteur
     if (length(unique(ind.trait))>1) { 
        if (length(unique(fact.trait))>1) {  
           Vuln_res <- anova.1way(log(ind.trait) ~ as.factor(fact.trait), data=capt2, nperm=999)
           assign("Vuln_res",Vuln_res,envir=.GlobalEnv)          
           VulnScen <- scenario.f(log(ind.trait),fact.trait)
   #        plotmeans(log(ind.trait)~ as.factor(fact.trait),main="Abondance des especes vulnerables",ylab="Abondance (Log)",xlab=paste("p-value=",round(Vuln_res$anova.table[1,5],5)))
        } else {
           print("Un seul facteur pour les especes vulnerables")
           VulnScen <- 0
        }
     } else {
        print("Un seul trait pour les especes vulnerables")
        VulnScen <- 0        
     }

   # Diversité Simpson
     ind.trait <- tapply(capt2[id_Tot,"Nombre"], capt2[id_Tot,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(capt2[id_Tot,facteur], capt2[id_Tot,"Trait"],mean, na.rm= T)
     SimpBef <- cbind(diversity(capt_Un.mat,'simpson'),c(rep(1,nrow(capt_Un.mat))))
     SimpAft <- cbind(diversity(capt_Deux.mat,'simpson'),c(rep(2,nrow(capt_Deux.mat))))
     Simpdata <- data.frame(rbind(SimpBef,SimpAft))
     colnames(Simpdata) <- c("Simp",facteur)
     Simpdata[,2] <- as.factor(Simpdata[,2])
     Simp_res <- anova.1way(Simp ~ Simpdata[,2], data=Simpdata, nperm=999)
     assign("Simp_res",Simp_res,envir=.GlobalEnv) 
     SimpScen <- scenario.f(log(ind.trait),fact.trait)
    # plotmeans(Simpdata$Simp ~ as.factor(Simpdata[,2]),main="Simpson", ylab="Simpson",xlab=paste("p-value=",round(Simp_res$anova.table[1,5],5)))
    # mtext(titre, side = 3, line = 1, outer = TRUE, cex = 1.5)
    # savePlot(filename = paste("Indicateurs_Communaute_Conservation_",titre,sep=""),"png")     

## Indicateurs Pêche
#dev.new()
 # par(mfrow=c(2,2),oma = c(0, 0, 3, 0),mex = 0.7)
   # Abondance totale log(N)
     id_Tot=is.element(capt2$Espece, listeEspecesCommunesCapt)
     ind.trait <- tapply(capt2[id_Tot,"Nombre"], capt2[id_Tot,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(capt2[id_Tot,facteur], capt2[id_Tot,"Trait"],mean, na.rm= T)
     AbunScen <- scenario.f(log(ind.trait),fact.trait)
  #   plotmeans(log(ind.trait) ~ as.factor(fact.trait),main="log(N)", ylab="log(N)",xlab=paste("p-value=",round(Abun_res$anova.table[1,5],5)))

   # Longueur moyenne
     id_Size=is.element(size2$Espece, listeEspecesCommunesSize)
     ind.trait <- tapply(size2[id_Size,"Longueur"], size2[id_Size,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(size2[id_Size,facteur2], size2[id_Size,"Trait"],mean, na.rm= T)
     Size_res <- anova.1way(ind.trait ~ as.factor(fact.trait), data=size2, nperm=999)
     assign("Size_res",Size_res,envir=.GlobalEnv)      
     SizeScen <- scenario.f(log(ind.trait),fact.trait)
   #  plotmeans(ind.trait ~ as.factor(fact.trait) ,main="Longueur moyenne", ylab="Longueur",xlab=paste("p-value=",round(Size_res$anova.table[1,5],5)))

   # Abondance des especes cibles
     id_ln_cible=is.element(capt2$Espece, LCible)
     ind.trait <- tapply(capt2[id_ln_cible,"Nombre"], capt2[id_ln_cible,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(capt2[id_ln_cible,facteur], capt2[id_ln_cible,"Trait"],mean, na.rm= T)
     #on verifie qu'il y ait + d'un trait et + d'un facteur
     if (length(unique(ind.trait))>1) { 
        if (length(unique(fact.trait))>1) {  
           Cible_res <- anova.1way(log(ind.trait) ~ as.factor(fact.trait), data=capt2, nperm=999)
           assign("Cible_res",Cible_res,envir=.GlobalEnv)              
           CibleScen <- scenario.f(log(ind.trait),fact.trait)
     #      plotmeans(log(ind.trait) ~ as.factor(fact.trait),main="Abondance des especes cibles", ylab="Abondance (log)",xlab=paste("p-value=",round(Cible_res$anova.table[1,5],5)))
        } else {
           print("Un seul facteur pour les especes cibles")
           CibleScen <- 0
        }
     } else {
        print("Un seul trait pour les especes cibles")
        CibleScen <- 0
     }                
    # mtext(titre, side = 3, outer = TRUE, cex = 1.5)
    # savePlot(filename = paste("Indicateurs_Communaute_Peche_",titre,sep=""),"png")

   # Scenarii Cons et Peche
     scen <- data.frame(rbind(ScenPeche_T=paste(AbunScen[1],SizeScen[1],CibleScen[1], sep=""),
                ScenCons_T=paste(AbunScen[1],SimpScen[1],VulnScen[1], sep="")))
     colnames(scen) <- c("Scenario")
     print(scen)
     assign("scen",scen,envir=.GlobalEnv)
} # fin de Anova_Indicateurs_Communaute

################################################################################
## Nom     : Anova_Indicateurs_Communaute_3grp.f
## Objet   : ANOVA one-way par permutation
## Input   :
## Output  :
################################################################################

Anova_Indicateurs_Communaute_3grp.f = function(capt_Un.mat, capt_Deux.mat, capt_Trois.mat, capt3, size3, titre, titre1, titre2, titre3, facteur, facteur2) {

## Indicateurs Conservation
#dev.new()
#  par(mfrow=c(2,2),oma = c(0, 0, 3, 0),mex = 0.7)
   # Abondance totale log(N)
     id_Tot=is.element(capt3$Espece, listeEspecesCommunesCapt)
     ind.trait <- tapply(capt3[id_Tot,"Nombre"], capt3[id_Tot,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(capt3[id_Tot,facteur], capt3[id_Tot,"Trait"],mean, na.rm= T)
     Abun_res <- anova.1way(log(ind.trait) ~ as.factor(fact.trait), data=capt3, nperm=999)
     assign("Abun_res",Abun_res,envir=.GlobalEnv)           
     AbunScen <- scenario.f(log(ind.trait),fact.trait)
 #    plotmeans(log(ind.trait) ~ as.factor(fact.trait),main="Abondance log(N)", ylab="log(N)",xlab=paste("p-value=",round(Abun_res$anova.table[1,5],5)))

   # Abondance des especes vulnérables
     id_Vuln=is.element(capt3$Espece, LVuln)
     ind.trait <- tapply(capt3[id_Vuln,"Nombre"], capt3[id_Vuln,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(capt3[id_Vuln,facteur], capt3[id_Vuln,"Trait"],mean, na.rm= T)
     #on verifie qu'il y ait + d'un trait et + d'un facteur
     if (length(unique(ind.trait))>1) { 
        if (length(unique(fact.trait))>1) {  
           Vuln_res <- anova.1way(log(ind.trait) ~ as.factor(fact.trait), data=capt3, nperm=999)
           assign("Vuln_res",Vuln_res,envir=.GlobalEnv)          
           VulnScen <- scenario.f(log(ind.trait),fact.trait)
  #         plotmeans(log(ind.trait)~ as.factor(fact.trait),main="Abondance des especes vulnerables",ylab="Abondance (Log)",xlab=paste("p-value=",round(Vuln_res$anova.table[1,5],5)))
        } else {
           print("Un seul facteur pour les especes vulnerables")
        }
     } else {
        print("Un seul trait pour les especes vulnerables")
     }

   # Diversité Simpson
     ind.trait <- tapply(capt3[id_Tot,"Nombre"], capt3[id_Tot,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(capt3[id_Tot,facteur], capt3[id_Tot,"Trait"],mean, na.rm= T)
     SimpUn <- cbind(diversity(capt_Un.mat,'simpson'),c(rep(1,nrow(capt_Un.mat))))
     SimpDeux <- cbind(diversity(capt_Deux.mat,'simpson'),c(rep(2,nrow(capt_Deux.mat))))
     SimpTrois <- cbind(diversity(capt_Trois.mat,'simpson'),c(rep(3,nrow(capt_Trois.mat))))
     Simpdata <- data.frame(rbind(SimpUn, SimpDeux, SimpTrois))
     colnames(Simpdata) <- c("Simp",facteur)
     Simpdata[,2] <- as.factor(Simpdata[,2])
     Simp_res <- anova.1way(Simp ~ Simpdata[,2], data=Simpdata, nperm=999)
     assign("Simp_res",Simp_res,envir=.GlobalEnv)
     SimpScen <- scenario.f(log(ind.trait),fact.trait)
#     plotmeans(Simpdata$Simp ~ as.factor(Simpdata[,2]),main="Simpson", ylab="Simpson",xlab=paste("p-value=",round(Simp_res$anova.table[1,5],5)))
#     mtext(titre, side = 3, line = 1, outer = TRUE, cex = 1.5)
#     savePlot(filename = paste("Indicateurs_Communaute_Conservation_",titre,sep=""), "png")
#
## Indicateurs Pêche
#dev.new()
 # par(mfrow=c(2,2),oma = c(0, 0, 3, 0),mex = 0.7)
   # Total abundance ln(N)
     id_Tot=is.element(capt3$Espece, listeEspecesCommunesCapt)
     ind.trait <- tapply(capt3[id_Tot,"Nombre"], capt3[id_Tot,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(capt3[id_Tot,facteur], capt3[id_Tot,"Trait"],mean, na.rm= T) 
     AbunScen <- scenario.f(log(ind.trait),fact.trait)
  #   plotmeans(log(ind.trait) ~ as.factor(fact.trait),main="log(N)", ylab="ln(N)",xlab=paste("p-value=",round(Abun_res$anova.table[1,5],5)))

   # Longueur moyenne  
     id_Size=is.element(size3$Espece, listeEspecesCommunesSize)
     ind.trait <- tapply(size3[id_Size,"Longueur"], size3[id_Size,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(size3[id_Size,facteur2], size3[id_Size,"Trait"],mean, na.rm= T)
     Size_res <- anova.1way(ind.trait ~ as.factor(fact.trait), data=size3, nperm=999)
     assign("Size_res",Size_res,envir=.GlobalEnv) 
     SizeScen <- scenario.f(log(ind.trait),fact.trait)
   #  plotmeans(ind.trait ~ as.factor(fact.trait) ,main="Longueur moyenne", ylab="Longueur",xlab=paste("p-value=",round(Size_res$anova.table[1,5],5)))

   # Abondance et taille moyenne des especes cibles
    # Abondance
     id_ln_cible=is.element(capt3$Espece, LCible)
     ind.trait <- tapply(capt3[id_ln_cible,"Nombre"], capt3[id_ln_cible,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(capt3[id_ln_cible,facteur], capt3[id_ln_cible,"Trait"],mean, na.rm= T)
     #on verifie qu'il y ait + d'un trait et + d'un facteur
     if (length(unique(ind.trait))>1) { 
        if (length(unique(fact.trait))>1) {  
           Cible_res <- anova.1way(log(ind.trait) ~ as.factor(fact.trait), data=capt3, nperm=999)
           assign("Cible_res",Cible_res,envir=.GlobalEnv)              
           CibleScen <- scenario.f(log(ind.trait),fact.trait)
    #       plotmeans(log(ind.trait) ~ as.factor(fact.trait),main="Abondance des especes cibles", ylab="Abondance (log)",xlab=paste("p-value=",round(Cible_res$anova.table[1,5],5)))
        } else {
           print("Un seul facteur pour les especes cibles")
        }
     } else {
        print("Un seul trait pour les especes cibles")
     }  
     #mtext(titre, side = 3, outer = TRUE, cex = 1.5)
     #savePlot(filename = paste("Indicateurs_Communaute_Peche_",titre,sep=""), "png")

   # Scenarii Cons et Peche
     scen <- data.frame(rbind(ScenPeche_T=paste(AbunScen[1],SizeScen[1],CibleScen[1], sep=""),
                ScenCons_T=paste(AbunScen[1],SimpScen[1],VulnScen[1], sep="")))
     colnames(scen) <- c("Scenario")
     assign("scen",scen,envir=.GlobalEnv)
} # fin de Anova_Indicateurs_Communaute_3grp

################################################################################
## Nom     : Anova_Indicateurs_Communaute_TS.f
## Objet   : Régression linéaire
## Input   :
## Output  :
################################################################################

Anova_Indicateurs_Communaute_TS.f = function(capt, size, titre) {

## Indicateurs Conservation
#  dev.new()
#  par(mfrow=c(2,2),oma = c(0, 0, 3, 0),mex = 0.7)

   # Abondance totale log(N)
     id_Tot=is.element(capt$Espece, listeEspecesCommunesCapt)
     ind.trait <- tapply(capt[id_Tot,"Nombre"], capt[id_Tot,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(capt[id_Tot,"Annee"], capt[id_Tot,"Trait"],mean, na.rm= T)
     Abun_res <- summary(lm(log(ind.trait) ~ fact.trait))
     assign("Abun_res",Abun_res,envir=.GlobalEnv)                
     AbunScen <- scenario.f(log(ind.trait),fact.trait)
    # plotmeans(log(ind.trait) ~ as.factor(fact.trait),main="Abondance log(N)", ylab="log(N)",xlab=paste("p-value=",round(Abun_res$coefficients[[8]],3)))

   # Abondance des especes vulnérables
     id_Vuln=is.element(capt$Espece, LVuln)
     ind.trait <- tapply(capt[id_Vuln,"Nombre"], capt[id_Vuln,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(capt[id_Vuln,"Annee"], capt[id_Vuln,"Trait"],mean, na.rm= T)
     #on verifie qu'il y ait + d'un trait et + d'un facteur
     if (length(unique(ind.trait))>1) { 
        if (length(unique(fact.trait))>1) {  
           Vuln_res <- summary(lm(log(ind.trait) ~ as.factor(fact.trait)))
           assign("Vuln_res",Vuln_res,envir=.GlobalEnv)          
           VulnScen <- scenario.f(log(ind.trait),fact.trait)
    #       plotmeans(log(ind.trait)~ as.factor(fact.trait),main="Abondance des especes vulnerables",ylab="Abondance (Log)",xlab=paste("p-value=",round(Vuln_res$coefficients[[8]],5)))
        } else {
           print("Un seul facteur pour les especes vulnerables")
        }
     } else {
        print("Un seul trait pour les especes vulnerables")
     }

   # Diversité Simpson    
     listeAnnees <- unique(capt$Annee)
     jonction <- NULL
     for (i in 1:length(unique(capt$Annee))) {
        capt_TS.mat <- contingence.fct(capt[capt$Annee == listeAnnees[i],])
        simpson <- data.frame(diversity(capt_TS.mat,'simpson'))
        colnames(simpson) <- c("diversity")
        jonction = data.frame(rbind(jonction,cbind(simpson,Annee=c(rep(listeAnnees[i],nrow(simpson))))))          
     }
     jonction[,2] <- as.factor(jonction[,2])
     Simp_res <- summary(lm(jonction$diversity ~ jonction[,2]))
     assign("Simp_res",Simp_res,envir=.GlobalEnv)
     ind.trait <- tapply(capt[id_Tot,"Nombre"], capt[id_Tot,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(capt[id_Tot,"Annee"], capt[id_Tot,"Trait"],mean, na.rm= T)
     
     SimpScen <- scenario.f(log(ind.trait),fact.trait)
  #   plotmeans(jonction$diversity ~ as.factor(jonction[,2]),main="Simpson", ylab="Simpson",xlab=paste("p-value=",round(Simp_res$coefficients[[8]],3)))
  #   mtext(titre, side = 3, line = 1, outer = TRUE, cex = 1.5)
  #   savePlot(filename = paste("Indicateurs_Communaute_Conservation_",titre,sep=""), "png")

## Indicateurs Pêche
#dev.new()
 # par(mfrow=c(2,2),oma = c(0, 0, 3, 0),mex = 0.7)
   # Abondance totale log(N)
     id_Tot=is.element(capt$Espece, listeEspecesCommunesCapt)
     ind.trait <- tapply(capt[id_Tot,"Nombre"], capt[id_Tot,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(capt[id_Tot,"Annee"], capt[id_Tot,"Trait"],mean, na.rm= T)
     AbunScen <- scenario.f(log(ind.trait),fact.trait)
     #plotmeans(log(ind.trait) ~ as.factor(fact.trait),main="log(N)", ylab="ln(N)",xlab=paste("p-value=",round(Abun_res$coefficients[[8]],3)))

   # Longueur moyenne  
     id_Size=is.element(size$Espece, listeEspecesCommunesSize)
     ind.trait <- tapply(size[id_Size,"Longueur"], size[id_Size,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(size[id_Size,"Annee"], size[id_Size,"Trait"],mean, na.rm= T)
     Size_res <- summary(lm(log(ind.trait) ~ fact.trait))
     assign("Size_res",Size_res,envir=.GlobalEnv) 
     SizeScen <- scenario.f(log(ind.trait),fact.trait)
     #plotmeans(log(ind.trait) ~ as.factor(fact.trait) ,main="Longueur moyenne", ylab="Longueur",xlab=paste("p-value=",round(Size_res$coefficients[[8]],3)))

   # Abondance and taille moyenne des especes cibles
    # Abondance
     id_ln_cible=is.element(capt$Espece, LCible)
     ind.trait <- tapply(capt[id_ln_cible,"Nombre"], capt[id_ln_cible,"Trait"],sum, na.rm= T)
     fact.trait <- tapply(capt[id_ln_cible,"Annee"], capt[id_ln_cible,"Trait"],mean, na.rm= T)
     #on verifie qu'il y ait + d'un trait et + d'un facteur
     if (length(unique(ind.trait))>1) { 
        if (length(unique(fact.trait))>1) {  
           Cible_res <- summary(lm(log(ind.trait) ~ as.factor(fact.trait)))
           assign("Cible_res",Cible_res,envir=.GlobalEnv)              
           CibleScen <- scenario.f(log(ind.trait),fact.trait)
           #plotmeans(log(ind.trait) ~ as.factor(fact.trait),main="Abondance des especes cibles", ylab="Abondance (log)",xlab=paste("p-value=",round(Cible_res$coefficients[[8]],5)))
        } else {
           print("Un seul facteur pour les especes cibles")
        }
     } else {
        print("Un seul trait pour les especes cibles")
     }  
    # mtext(titre, side = 3, outer = TRUE, cex = 1.5)
    # savePlot(filename = paste("Indicateurs_Communaute_Peche_",titre,sep=""), "png")

   # Scenarii Conservation et Peche
     scen <- data.frame(rbind(ScenPeche_T=paste(AbunScen[1],SizeScen[1],CibleScen[1], sep=""),
                ScenCons_T=paste(AbunScen[1],SimpScen[1],VulnScen[1], sep="")))
     colnames(scen) <- c("Scenario")
     assign("scen",scen,envir=.GlobalEnv)
} # fin de Anova_Indicateurs_Communaute_TS