################################################################################
## Nom     : Radar_Plot_Peche.f
## Objet   : ????
## Input   :
## Output  :
################################################################################

Radar_Plot_Peche.f = function(capt2, size2, titre) {
#    graphics.off()
#    dev.new(record = TRUE)
#    .SavedPlots <- NULL

   # Abondance totale moyenne log(N)
   # calcul du ratio de l'abondance moyenne des captures time = 2 / moyenne des captures time = 1
     id_abondance=is.element(capt2$Espece, listeEspecesCommunesCapt)
     Ratio_logN <- log(mean(capt2[id_abondance & capt2$time==2,"Nombre"]))/log(mean(capt2[id_abondance & capt2$time==1,"Nombre"]))

   # Longueur moyenne  
     id_longueur=is.element(size2$Espece, listeEspecesCommunesSize)
     Ratio_LongueurMoy <- log(mean(size2[id_longueur & size2$time==2,"Longueur"]))/log(mean(size2[id_longueur & size2$time==1,"Longueur"]))

   # Abondance des especes cibles
     id_cible=is.element(capt2$Espece, LCible)
     Ratio_Cible <- log(mean(capt2[id_cible & capt2$time==2,"Nombre"]))/log(mean(capt2[id_cible & capt2$time==1,"Nombre"]))

## Population
  # calcul du ratio de l'abondance totale moyenne des captures par espèce
    Ratio_logN_pop <- NULL
    for (i in 1:length(listeEspecesCommunesCapt)) {
        id=is.element(capt2$Espece, listeEspecesCommunesCapt[i])
        temp_pop <- log(mean(capt2[id & capt2$time==2,"Nombre"]))/log(mean(capt2[id & capt2$time==1,"Nombre"]))
        Ratio_logN_pop <- rbind(Ratio_logN_pop,temp_pop)
    }
    rownames(Ratio_logN_pop) <- listeEspecesCommunesCapt
    Ratio_logN_pop <- data.frame(na.omit(Ratio_logN_pop))
    colnames(Ratio_logN_pop) <- "LogN"

  # calcul du ratio des tailles moyennes par espèce
    Ratio_LongueurMoy_pop <- NULL
    for (i in 1:length(listeEspecesCommunesSize)) {
        id=is.element(size2$Espece, listeEspecesCommunesSize[i])
        temp2_pop <- log(mean(size2[id & size2$time==2,"Longueur"]))/log(mean(size2[id & size2$time==1,"Longueur"]))
        Ratio_LongueurMoy_pop <- rbind(Ratio_LongueurMoy_pop,temp2_pop)
    }
    rownames(Ratio_LongueurMoy_pop) <- listeEspecesCommunesSize
    Ratio_LongueurMoy_pop <- data.frame(na.omit(Ratio_LongueurMoy_pop)) 
    colnames(Ratio_LongueurMoy_pop) <- c("LongueurMoy")

# Radar Plot

    #Transformation des ratios en scores 
#--- population    
    popLogN <- length(which(Ratio_logN_pop$LogN>1))/nrow(Ratio_logN_pop)*100
    popLongueurMoy <- length(which(Ratio_LongueurMoy_pop$LongueurMoy>1))/nrow(Ratio_LongueurMoy_pop)*100
    pop_ratio <-  data.frame(rbind(popLogN,popLongueurMoy))
    colnames(pop_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios
    brk_scale_pop <- c(0,25,50,75,100)
    pop_ratio$ClassLimit <- cut(pop_ratio[,1], breaks=brk_scale_pop, right=TRUE)
    pop_ratio$Significatif <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(0,25]"] <- -1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(25,50]"] <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(50,75]"] <- 2
    pop_ratio$Classification[pop_ratio$ClassLimit=="(75,100]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (length(listeEspecesSignificatives_ln[!is.na(listeEspecesSignificatives_ln)])==0){
        pop_ratio$Significatif[1] <- 0
        pop_ratio$Classification[1] <- 0        
    }
    if (length(listeEspecesSignificatives_Size[!is.na(listeEspecesSignificatives_Size)])==0){
        pop_ratio$Significatif[2] <- 0
        pop_ratio$Classification[2] <- 0         
    }    

#--- communaute    
    peche_ratio <- data.frame(rbind(Ratio_Cible,Ratio_LongueurMoy,Ratio_logN))
    colnames(peche_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios    
    brk_scale_com <- c(0,1,1.25,1.5,1.75)
    peche_ratio$ClassLimit <- cut(peche_ratio[,1], breaks=brk_scale_com, right=TRUE)
    peche_ratio$Significatif <- 1
    peche_ratio$Classification[peche_ratio$IndVal<1] <- -1
    peche_ratio$Classification[peche_ratio$ClassLimit=="(0,1]"] <- 0    
    peche_ratio$Classification[peche_ratio$ClassLimit=="(1,1.25]"] <- 1
    peche_ratio$Classification[peche_ratio$ClassLimit=="(1.25,1.5]"] <- 2
    peche_ratio$Classification[peche_ratio$ClassLimit=="(1.5,1.75]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (Cible_res$anova.table[[5]][1]>0.05){
        peche_ratio$Significatif[1] <- 0
        peche_ratio$Classification[1] <- 0        
    }
    if (Size_res$anova.table[[5]][1]>0.05){
        peche_ratio$Significatif[2] <- 0
        peche_ratio$Classification[2] <- 0        
    }        
    if (Abun_res$anova.table[[5]][1]>0.05){
        peche_ratio$Significatif[3] <- 0
        peche_ratio$Classification[3] <- 0
    }  

    #ordre : Ratio_Cible, Ratio_LongueurMoy, Ratio_logN, popLogN, popLongueurMoy
    peche_ratio <- rbind(peche_ratio,pop_ratio)
    assign("peche_ratio",peche_ratio,envir=.GlobalEnv) 
             
#    par(cex.axis=1)
#    radial.plot(peche_ratio$Classification,labels=c("Abondance\nesp.cibles","Longueur moyenne","Abondance totale","% esp.abondance","% esp.taille"),
#    rp.type="p",label.pos=NULL,radlab=FALSE, start=0,clockwise=FALSE,label.prop=1.3,
#    line.col="red",lty=par("lty"),lwd=2,mar=c(5,5,5,5), point.symbols=4,  radial.lim=c(-1,3),show.grid=T,poly.col="yellow")                
#    mtext("Radarplot objectif peche",side = 3, line=3, cex=1.5)
#   
#    savePlot(filename = paste("Radial_plot_peche",titre), "png")   

}#fin Radar_Plot_Peche

################################################################################
## Nom     : Radar_Plot_Peche_3grp.f
## Objet   : ????
## Input   :
## Output  :
################################################################################

Radar_Plot_Peche_3grp.f = function(capt3, size3, titre) {
#    graphics.off()
#    dev.new(record = TRUE)
#    .SavedPlots <- NULL
    
## Communaute
   # Abondance totale log(N)
   # calcul du ratio de l'abondance moyenne des captures time = 3 / moyenne des captures time = 1
     id_abondance=is.element(capt3$Espece, listeEspecesCommunesCapt)
     Ratio_logN <- log(mean(capt3[id_abondance & capt3$time==3,"Nombre"]))/log(mean(capt3[id_abondance & capt3$time==1,"Nombre"]))

   # Longueur moyenne  
     id_longueur=is.element(size3$Espece, listeEspecesCommunesSize)
     Ratio_LongueurMoy <- log(mean(size3[id_longueur & size3$time==3,"Longueur"]))/log(mean(size3[id_longueur & size3$time==1,"Longueur"]))

   # Abondance des especes cibles
     id_cible=is.element(capt3$Espece, LCible)
     Ratio_Cible <- log(mean(capt3[id_cible & capt3$time==3,"Nombre"]))/log(mean(capt3[id_cible & capt3$time==1,"Nombre"]))

## Population
  # calcul du ratio de l'abondance moyenne des captures par espèce
    Ratio_logN_pop <- NULL
    for (i in 1:length(listeEspecesCommunesCapt)) {  
        id_espece=is.element(capt3$Espece, listeEspecesCommunesCapt[i])
  # T1<T2>T3 ou T1>T2<T3 ou T3=T2=T1 (non significatif) => T3/T1
        if (listeEspecesCommunesCapt[i] %in% EspAbondanceT2eqT1) { 
           temp_pop <- log(mean(capt3[id_espece & capt3$time==3,"Nombre"]))/log(mean(capt3[id_espece & capt3$time==1,"Nombre"])) 
        }
  # T1=T2<T3 ou T1=T2>T3 => T3 / mean(T1,T2)
        if (listeEspecesCommunesCapt[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))]))) {
           temp_pop <- log(mean(capt3[id_espece & capt3$time==3,"Nombre"]))/log((mean(capt3[id_espece & capt3$time==2,"Nombre"]) + mean(capt3[id_espece & capt3$time==1,"Nombre"]))/2)        
        }
  # T1<T2=T3 ou T1>T2=T3 => mean(T3,T2) / T1
        if (listeEspecesCommunesCapt[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0))]))) {
           temp_pop <- log((mean(capt3[id_espece & capt3$time==2,"Nombre"]) + mean(capt3[id_espece & capt3$time==3,"Nombre"]))/2) / log(mean(capt3[id_espece & capt3$time==1,"Nombre"]))
        }  
  # T1>T2>T3 ou T1<T2<T3 => T3/T1
        if (listeEspecesCommunesCapt[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))]))) {  
           temp_pop <- log(mean(capt3[id_espece & capt3$time==3,"Nombre"]))/log(mean(capt3[id_espece & capt3$time==1,"Nombre"]))
        }              
       Ratio_logN_pop <- rbind(Ratio_logN_pop,temp_pop)
    }
    rownames(Ratio_logN_pop) <- listeEspecesCommunesCapt
    Ratio_logN_pop <- data.frame(na.omit(Ratio_logN_pop))
    colnames(Ratio_logN_pop) <- "LogN"

  # calcul du ratio des tailles moyennes par espèce
    Ratio_LongueurMoy_pop <- NULL
    for (i in 1:length(listeEspecesCommunesSize)) {
        id_espece=is.element(size3$Espece, listeEspecesCommunesSize[i])
  # T1<T2>T3 ou T1>T2<T3 ou T3=T2=T1 (non significatif) => T3/T1
        if (listeEspecesCommunesSize[i] %in% EspLongT2eqT1) { 
           temp2_pop <- log(mean(size3[id_espece & size3$time==3,"Nombre"]))/log(mean(size3[id_espece & size3$time==1,"Nombre"])) 
        }
  # T1=T2<T3 ou T1=T2>T3 => T3 / mean(T1,T2)
        if (listeEspecesCommunesSize[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))]))) {
           temp2_pop <- log(mean(size3[id_espece & size3$time==3,"Nombre"]))/log((mean(size3[id_espece & size3$time==2,"Nombre"]) + mean(size3[id_espece & size3$time==1,"Nombre"]))/2)        
        }
  # T1<T2=T3 ou T1>T2=T3 => mean(T3,T2) / T1
        if (listeEspecesCommunesSize[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0))]))) {
           temp2_pop <- log((mean(size3[id_espece & size3$time==2,"Nombre"]) + mean(size3[id_espece & size3$time==3,"Nombre"]))/2) / log(mean(size3[id_espece & size3$time==1,"Nombre"]))
        }  
  # T1>T2>T3 ou T1<T2<T3 => T3/T1
        if (listeEspecesCommunesSize[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))]))) {  
           temp2_pop <- log(mean(size3[id_espece & size3$time==3,"Nombre"]))/log(mean(size3[id_espece & size3$time==1,"Nombre"]))
        }                               
        Ratio_LongueurMoy_pop <- rbind(Ratio_LongueurMoy_pop,temp2_pop)
    }
    rownames(Ratio_LongueurMoy_pop) <- listeEspecesCommunesSize
    Ratio_LongueurMoy_pop <- data.frame(na.omit(Ratio_LongueurMoy_pop)) 
    colnames(Ratio_LongueurMoy_pop) <- c("LongueurMoy")

# Radar Plot

    #Transformation des ratios en scores 
#--- population    
    popLogN <- length(which(Ratio_logN_pop$LogN>1))/nrow(Ratio_logN_pop)*100
    popLongueurMoy <- length(which(Ratio_LongueurMoy_pop$LongueurMoy>1))/nrow(Ratio_LongueurMoy_pop)*100
    pop_ratio <-  data.frame(rbind(popLogN,popLongueurMoy))
    colnames(pop_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios
    brk_scale_pop <- c(0,25,50,75,100)
    pop_ratio$ClassLimit <- cut(pop_ratio[,1], breaks=brk_scale_pop, right=TRUE)
    pop_ratio$Significatif <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(0,25]"] <- -1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(25,50]"] <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(50,75]"] <- 2
    pop_ratio$Classification[pop_ratio$ClassLimit=="(75,100]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (length(listeEspecesSignificatives_ln[!is.na(listeEspecesSignificatives_ln)])==0){
        pop_ratio$Significatif[1] <- 0
        pop_ratio$Classification[1] <- 0        
    }
    if (length(listeEspecesSignificatives_Size[!is.na(listeEspecesSignificatives_Size)])==0){
        pop_ratio$Significatif[2] <- 0
        pop_ratio$Classification[2] <- 0         
    }   

#--- communaute    
    peche_ratio <- data.frame(rbind(Ratio_Cible,Ratio_LongueurMoy,Ratio_logN))
    colnames(peche_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios    
    brk_scale_com <- c(0,1,1.25,1.5,1.75)
    peche_ratio$ClassLimit <- cut(peche_ratio[,1], breaks=brk_scale_com, right=TRUE)
    peche_ratio$Significatif <- 1
    peche_ratio$Classification[peche_ratio$IndVal<1] <- -1
    peche_ratio$Classification[peche_ratio$ClassLimit=="(0,1]"] <- 0    
    peche_ratio$Classification[peche_ratio$ClassLimit=="(1,1.25]"] <- 1
    peche_ratio$Classification[peche_ratio$ClassLimit=="(1.25,1.5]"] <- 2
    peche_ratio$Classification[peche_ratio$ClassLimit=="(1.5,1.75]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (Cible_res$anova.table[[5]][1]>0.05){
        peche_ratio$Significatif[1] <- 0
        peche_ratio$Classification[1] <- 0        
    }
    if (Size_res$anova.table[[5]][1]>0.05){
        peche_ratio$Significatif[2] <- 0
        peche_ratio$Classification[2] <- 0        
    }        
    if (Abun_res$anova.table[[5]][1]>0.05){
        peche_ratio$Significatif[3] <- 0
        peche_ratio$Classification[3] <- 0
    }  

    #ordre : Ratio_Cible, Ratio_LongueurMoy, Ratio_logN, popLogN, popLongueurMoy
    peche_ratio <- rbind(peche_ratio,pop_ratio)
    assign("peche_ratio",peche_ratio,envir=.GlobalEnv) 
             
#    par(cex.axis=1)
#    radial.plot(peche_ratio$Classification,labels=c("Abondance\nesp.cibles","Longueur moyenne","Abondance totale","% esp.abondance","% esp.taille"),
#    rp.type="p",label.pos=NULL,radlab=FALSE, start=0,clockwise=FALSE,label.prop=1.3,
#    line.col="red",lty=par("lty"),lwd=2,mar=c(5,5,5,5), point.symbols=4,  radial.lim=c(-1,3),show.grid=T,poly.col="yellow")                
#    mtext("Radarplot objectif peche",side = 3, line=3, cex=1.5)
#   
#    savePlot(filename = paste("Radial_plot_peche_3grp",titre), "png")   
#
}#fin Radar_Plot_Peche_3grp


################################################################################
## Nom     : Radar_Plot_Conservation.f
## Objet   :
## Input   :
## Output  :
################################################################################

Radar_Plot_Conservation.f = function(capt_Bef.mat, capt_Aft.mat, capt2, size2, titre) {
#    graphics.off()
#    dev.new(record = TRUE)
#    .SavedPlots <- NULL

    # calcul du ratio de l'abondance moyenne des captures time = 2 / moyenne des captures time = 1
    id_abondance=is.element(capt2$Espece, listeEspecesCommunesCapt)
    Ratio_logN <- log(mean(capt2[id_abondance & capt2$time==2,"Nombre"]))/log(mean(capt2[id_abondance & capt2$time==1,"Nombre"]))

    # Diversité Simpson
    SimpBef <- cbind(diversity(capt_Bef.mat[,-c(1,2)],'simpson'),c(rep(1,nrow(capt_Bef.mat))))
    SimpAft <- cbind(diversity(capt_Aft.mat[,-c(1,2)],'simpson'),c(rep(2,nrow(capt_Aft.mat))))
    Simpdata <- data.frame(rbind(SimpBef,SimpAft)) 
    colnames(Simpdata) <- c("Simp","Time")
    Ratio_Simp <-  mean(SimpAft)/mean(SimpBef)

    # Abondance des especes vulnérables
    id_vuln=is.element(capt2$Espece, LVuln)
    Ratio_vuln <- log(mean(capt2[id_vuln & capt2$time==2,"Nombre"]))/log(mean(capt2[id_vuln & capt2$time==1,"Nombre"]))

## Population
  # calcul du ratio de l'abondance moyenne des captures par espèce
    Ratio_logN_pop <- NULL
    for (i in 1:length(listeEspecesCommunesCapt)) {
        id=is.element(capt2$Espece, listeEspecesCommunesCapt[i])
        temp_pop <- log(mean(capt2[id & capt2$time==2,"Nombre"]))/log(mean(capt2[id & capt2$time==1,"Nombre"]))
        Ratio_logN_pop <- matrix(rbind(temp_pop,Ratio_logN_pop), ncol=1)
    }
    rownames(Ratio_logN_pop) <- listeEspecesCommunesCapt
    Ratio_logN_pop <- data.frame(na.omit(Ratio_logN_pop))
    colnames(Ratio_logN_pop) <- "LogN"

  # calcul du ratio des tailles moyennes par espèce
    Ratio_LongueurMoy_pop <- NULL
    for (i in 1:length(listeEspecesCommunesSize)) {
        id=is.element(size2$Espece, listeEspecesCommunesSize[i])
        temp2_pop <- log(mean(size2[id & size2$time==2,"Longueur"]))/log(mean(size2[id & size2$time==1,"Longueur"]))
        Ratio_LongueurMoy_pop <- matrix(rbind(temp2_pop,Ratio_LongueurMoy_pop), ncol=1)
    }
    rownames(Ratio_LongueurMoy_pop) <- listeEspecesCommunesSize
    Ratio_LongueurMoy_pop <- data.frame(na.omit(Ratio_LongueurMoy_pop)) 
    colnames(Ratio_LongueurMoy_pop) <- c("LongueurMoy")

# Radar Plot

    #Transformation des ratios en scores 
        
#--- population 
    popLogN <- length(which(Ratio_logN_pop$LogN>1))/nrow(Ratio_logN_pop)*100
    popLongueurMoy <- length(which(Ratio_LongueurMoy_pop$LongueurMoy>1))/nrow(Ratio_LongueurMoy_pop)*100
    pop_ratio <-  data.frame(rbind(popLogN,popLongueurMoy))
    colnames(pop_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios
    brk_scale_pop <- c(0,25,50,75,100)
    pop_ratio$ClassLimit <- cut(pop_ratio[,1], breaks=brk_scale_pop, right=TRUE)
    pop_ratio$Significatif <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(0,25]"] <- -1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(25,50]"] <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(50,75]"] <- 2
    pop_ratio$Classification[pop_ratio$ClassLimit=="(75,100]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (length(listeEspecesSignificatives_ln[!is.na(listeEspecesSignificatives_ln)])==0){
        pop_ratio$Significatif[1] <- 0
        pop_ratio$Classification[1] <- 0        
    }
    if (length(listeEspecesSignificatives_Size[!is.na(listeEspecesSignificatives_Size)])==0){
        pop_ratio$Significatif[2] <- 0
        pop_ratio$Classification[2] <- 0         
    }     

    cons_ratio <- data.frame(rbind(Ratio_Simp,Ratio_vuln,Ratio_logN)) 
    colnames(cons_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios    
    brk_scale_com <- c(0,1,1.25,1.5,1.75)
    cons_ratio$ClassLimit <- cut(cons_ratio[,1], breaks=brk_scale_com, right=TRUE)
    cons_ratio$Significatif <- 1
    cons_ratio$Classification[cons_ratio$IndVal<1] <- -1
    cons_ratio$Classification[cons_ratio$ClassLimit=="(0,1]"] <- 0    
    cons_ratio$Classification[cons_ratio$ClassLimit=="(1,1.25]"] <- 1
    cons_ratio$Classification[cons_ratio$ClassLimit=="(1.25,1.5]"] <- 2
    cons_ratio$Classification[cons_ratio$ClassLimit=="(1.5,1.75]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (Simp_res$anova.table[[5]][1]>0.05){
        cons_ratio$Significatif[1] <- 0
        cons_ratio$Classification[1] <- 0        
    }
    if (Vuln_res$anova.table[[5]][1]>0.05){
        cons_ratio$Significatif[2] <- 0
        cons_ratio$Classification[2] <- 0        
    }        
    if (Abun_res$anova.table[[5]][1]>0.05){
        cons_ratio$Significatif[3] <- 0
        cons_ratio$Classification[3] <- 0
    }      
    
    #ordre : Ratio_Simp, Ratio_Vuln, Ratio_logN, popLogN, popLongueurMoy
    cons_ratio <- rbind(cons_ratio,pop_ratio)
    assign("cons_ratio",cons_ratio,envir=.GlobalEnv) 
 
#    par(cex.axis=1)
#    radial.plot(cons_ratio$Classification,labels=c("Simpson","Abondance\nesp.vuln","Abondance totale","% esp.abondance","% esp.taille"),
#    rp.type="p",label.pos=NULL,radlab=FALSE, start=0,clockwise=FALSE,label.prop=1.3,
#    line.col="red",lty=par("lty"),lwd=2,mar=c(5,5,5,5), point.symbols=4, radial.lim=c(-1,3),show.grid=T,poly.col="yellow")                
#    mtext("Radarplot objectif conservation",side = 3, line=3, cex=1.5)
#              
} #fin Radar_Plot_Conservation

################################################################################
## Nom     : Radar_Plot_Conservation_3grp.f
## Objet   :
## Input   :
## Output  :
################################################################################

Radar_Plot_Conservation_3grp.f = function(capt_Un.mat, capt_Deux.mat, capt_Trois.mat, capt3, size3, titre) {
#    graphics.off()
#    dev.new(record = TRUE)
#    .SavedPlots <- NULL

    # calcul du ratio de l'abondance moyenne des captures time = 3 / moyenne des captures time = 1
    id_abondance=is.element(capt3$Espece, listeEspecesCommunesCapt)
    Ratio_logN <- log(mean(capt3[id_abondance & capt3$time==3,"Nombre"]))/log(mean(capt3[id_abondance & capt3$time==1,"Nombre"]))

    # Diversité Simpson
    SimpUn <- cbind(diversity(capt_Un.mat[,-c(1,2)],'simpson'),c(rep(1,nrow(capt_Un.mat))))
    SimpTrois <- cbind(diversity(capt_Trois.mat[,-c(1,2)],'simpson'),c(rep(2,nrow(capt_Trois.mat))))
    Simpdata <- data.frame(rbind(SimpUn,SimpTrois)) 
    colnames(Simpdata) <- c("Simp","Time")
    Ratio_Simp <- mean(SimpTrois)/mean(SimpUn)

    # Abondance des especes vulnérables
    id_vuln=is.element(capt3$Espece, LVuln)
    Ratio_vuln <- log(mean(capt3[id_vuln & capt3$time==3,"Nombre"]))/log(mean(capt3[id_vuln & capt3$time==1,"Nombre"]))

## Population
  # calcul du ratio de l'abondance moyenne des captures par espèce
    Ratio_logN_pop <- NULL
    for (i in 1:length(listeEspecesCommunesCapt)) {  
        id_espece=is.element(capt3$Espece, listeEspecesCommunesCapt[i])
  # T1<T2>T3 ou T1>T2<T3 ou T3=T2=T1 (non significatif) => T3/T1
        if (listeEspecesCommunesCapt[i] %in% EspAbondanceT2eqT1) { 
           temp_pop <- log(mean(capt3[id_espece & capt3$time==3,"Nombre"]))/log(mean(capt3[id_espece & capt3$time==1,"Nombre"])) 
        }
  # T1=T2<T3 ou T1=T2>T3 => T3 / mean(T1,T2)
        if (listeEspecesCommunesCapt[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))]))) {
           temp_pop <- log(mean(capt3[id_espece & capt3$time==3,"Nombre"]))/log((mean(capt3[id_espece & capt3$time==2,"Nombre"]) + mean(capt3[id_espece & capt3$time==1,"Nombre"]))/2)        
        }
  # T1<T2=T3 ou T1>T2=T3 => mean(T3,T2) / T1
        if (listeEspecesCommunesCapt[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0))]))) {
           temp_pop <- log((mean(capt3[id_espece & capt3$time==2,"Nombre"]) + mean(capt3[id_espece & capt3$time==3,"Nombre"]))/2) / log(mean(capt3[id_espece & capt3$time==1,"Nombre"]))
        }  
  # T1>T2>T3 ou T1<T2<T3 => T3/T1
        if (listeEspecesCommunesCapt[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))]))) {  
           temp_pop <- log(mean(capt3[id_espece & capt3$time==3,"Nombre"]))/log(mean(capt3[id_espece & capt3$time==1,"Nombre"]))
        }              
       Ratio_logN_pop <- rbind(Ratio_logN_pop,temp_pop)
    }
    rownames(Ratio_logN_pop) <- listeEspecesCommunesCapt
    Ratio_logN_pop <- data.frame(na.omit(Ratio_logN_pop))
    colnames(Ratio_logN_pop) <- "LogN"

  # calcul du ratio des tailles moyennes par espèce
    Ratio_LongueurMoy_pop <- NULL
    for (i in 1:length(listeEspecesCommunesSize)) {
        id_espece=is.element(size3$Espece, listeEspecesCommunesSize[i])
  # T1<T2>T3 ou T1>T2<T3 ou T3=T2=T1 (non significatif) => T3/T1
        if (listeEspecesCommunesSize[i] %in% EspAbondanceT2eqT1) { 
           temp2_pop <- log(mean(size3[id_espece & size3$time==3,"Nombre"]))/log(mean(size3[id_espece & size3$time==1,"Nombre"])) 
        }
  # T1=T2<T3 ou T1=T2>T3 => T3 / mean(T1,T2)
        if (listeEspecesCommunesSize[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))]))) {
           temp2_pop <- log(mean(size3[id_espece & size3$time==3,"Nombre"]))/log((mean(size3[id_espece & size3$time==2,"Nombre"]) + mean(size3[id_espece & size3$time==1,"Nombre"]))/2)        
        }
  # T1<T2=T3 ou T1>T2=T3 => mean(T3,T2) / T1
        if (listeEspecesCommunesSize[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0))]))) {
           temp2_pop <- log((mean(size3[id_espece & size3$time==2,"Nombre"]) + mean(size3[id_espece & size3$time==3,"Nombre"]))/2) / log(mean(size3[id_espece & size3$time==1,"Nombre"]))
        }  
  # T1>T2>T3 ou T1<T2<T3 => T3/T1
        if (listeEspecesCommunesSize[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))]))) {  
           temp2_pop <- log(mean(size3[id_espece & size3$time==3,"Nombre"]))/log(mean(size3[id_espece & size3$time==1,"Nombre"]))
        }                               
        Ratio_LongueurMoy_pop <- rbind(Ratio_LongueurMoy_pop,temp2_pop)
    }
    rownames(Ratio_LongueurMoy_pop) <- listeEspecesCommunesSize
    Ratio_LongueurMoy_pop <- data.frame(na.omit(Ratio_LongueurMoy_pop)) 
    colnames(Ratio_LongueurMoy_pop) <- c("LongueurMoy")

# Radar Plot

    #Transformation des ratios en scores 
        
#--- population 
    popLogN <- length(which(Ratio_logN_pop$LogN>1))/nrow(Ratio_logN_pop)*100
    popLongueurMoy <- length(which(Ratio_LongueurMoy_pop$LongueurMoy>1))/nrow(Ratio_LongueurMoy_pop)*100
    pop_ratio <-  data.frame(rbind(popLogN,popLongueurMoy))
    colnames(pop_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios
    brk_scale_pop <- c(0,25,50,75,100)
    pop_ratio$ClassLimit <- cut(pop_ratio[,1], breaks=brk_scale_pop, right=TRUE)
    pop_ratio$Significatif <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(0,25]"] <- -1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(25,50]"] <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(50,75]"] <- 2
    pop_ratio$Classification[pop_ratio$ClassLimit=="(75,100]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (length(listeEspecesSignificatives_ln[!is.na(listeEspecesSignificatives_ln)])==0){
        pop_ratio$Significatif[1] <- 0
        pop_ratio$Classification[1] <- 0        
    }
    if (length(listeEspecesSignificatives_Size[!is.na(listeEspecesSignificatives_Size)])==0){
        pop_ratio$Significatif[2] <- 0
        pop_ratio$Classification[2] <- 0         
    }     
  
    cons_ratio <- data.frame(rbind(Ratio_Simp,Ratio_vuln,Ratio_logN)) 
    colnames(cons_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios    
    brk_scale_com <- c(0,1,1.25,1.5,1.75)
    cons_ratio$ClassLimit <- cut(cons_ratio[,1], breaks=brk_scale_com, right=TRUE)
    cons_ratio$Significatif <- 1
    cons_ratio$Classification[cons_ratio$IndVal<1] <- -1
    cons_ratio$Classification[cons_ratio$ClassLimit=="(0,1]"] <- 0    
    cons_ratio$Classification[cons_ratio$ClassLimit=="(1,1.25]"] <- 1
    cons_ratio$Classification[cons_ratio$ClassLimit=="(1.25,1.5]"] <- 2
    cons_ratio$Classification[cons_ratio$ClassLimit=="(1.5,1.75]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (Simp_res$anova.table[[5]][1]>0.05){
        cons_ratio$Significatif[1] <- 0
        cons_ratio$Classification[1] <- 0        
    }
    if (Vuln_res$anova.table[[5]][1]>0.05){
        cons_ratio$Significatif[2] <- 0
        cons_ratio$Classification[2] <- 0        
    }        
    if (Abun_res$anova.table[[5]][1]>0.05){
        cons_ratio$Significatif[3] <- 0
        cons_ratio$Classification[3] <- 0
    }      
    
    #ordre : Ratio_Simp, Ratio_Vuln, Ratio_logN, popLogN, popLongueurMoy
    cons_ratio <- rbind(cons_ratio,pop_ratio)
    assign("cons_ratio",cons_ratio,envir=.GlobalEnv) 
 
#    par(cex.axis=1)
#    radial.plot(cons_ratio$Classification,labels=c("Simpson","Abondance\nesp.vuln","Abondance totale","% esp.abondance","% esp.taille"),
#    rp.type="p",label.pos=NULL,radlab=FALSE, start=0,clockwise=FALSE,label.prop=1.3,
#    line.col="red",lty=par("lty"),lwd=2,mar=c(5,5,5,5), point.symbols=4, radial.lim=c(-1,3),show.grid=T,poly.col="yellow")                
#    mtext("Radarplot objectif conservation",side = 3, line=3, cex=1.5)
#              
} #fin Radar_Plot_Conservation_3grp

################################################################################
## Nom     : Radar_Plot_Conservation_Gradient.f
## Objet   :
## Input   :
## Output  :
################################################################################

Radar_Plot_Conservation_Gradient.f = function(capt_Un.mat, capt_Deux.mat, capt_Trois.mat, capt3, size3, titre) {
#    graphics.off()
#    dev.new(record = TRUE)
#    .SavedPlots <- NULL

    # calcul du ratio de l'abondance moyenne des captures Strate = 3 / moyenne des captures Strate = 1
    id_abondance=is.element(capt3$Espece, listeEspecesCommunesCapt)
    Ratio_logN <- log(mean(capt3[id_abondance & capt3$Strate==3,"Nombre"]))/log(mean(capt3[id_abondance & capt3$Strate==1,"Nombre"]))

    # Diversité Simpson
    SimpUn <- cbind(diversity(capt_Un.mat[,-c(1,2)],'simpson'),c(rep(1,nrow(capt_Un.mat))))
    SimpTrois <- cbind(diversity(capt_Trois.mat[,-c(1,2)],'simpson'),c(rep(2,nrow(capt_Trois.mat))))
    Simpdata <- data.frame(rbind(SimpUn,SimpTrois)) 
    colnames(Simpdata) <- c("Simp","Strate")
    Ratio_Simp <- mean(SimpTrois)/mean(SimpUn)

    # Abondance des especes vulnérables
    id_vuln=is.element(capt3$Espece, LVuln)
    Ratio_vuln <- log(mean(capt3[id_vuln & capt3$Strate==3,"Nombre"]))/log(mean(capt3[id_vuln & capt3$Strate==1,"Nombre"]))

## Population
  # calcul du ratio de l'abondance moyenne des captures par espèce
    Ratio_logN_pop <- NULL
    for (i in 1:length(listeEspecesCommunesCapt)) {  
        id_espece=is.element(capt3$Espece, listeEspecesCommunesCapt[i])
  # T1<T2>T3 ou T1>T2<T3 ou T3=T2=T1 (non significatif) => T3/T1
        if (listeEspecesCommunesCapt[i] %in% EspAbondanceT2eqT1) { 
           temp_pop <- log(mean(capt3[id_espece & capt3$Strate==3,"Nombre"]))/log(mean(capt3[id_espece & capt3$Strate==1,"Nombre"])) 
        }
  # T1=T2<T3 ou T1=T2>T3 => T3 / mean(T1,T2)
        if (listeEspecesCommunesCapt[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))]))) {
           temp_pop <- log(mean(capt3[id_espece & capt3$Strate==3,"Nombre"]))/log((mean(capt3[id_espece & capt3$Strate==2,"Nombre"]) + mean(capt3[id_espece & capt3$Strate==1,"Nombre"]))/2)        
        }
  # T1<T2=T3 ou T1>T2=T3 => mean(T3,T2) / T1
        if (listeEspecesCommunesCapt[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0))]))) {
           temp_pop <- log((mean(capt3[id_espece & capt3$Strate==2,"Nombre"]) + mean(capt3[id_espece & capt3$Strate==3,"Nombre"]))/2) / log(mean(capt3[id_espece & capt3$Strate==1,"Nombre"]))
        }  
  # T1>T2>T3 ou T1<T2<T3 => T3/T1
        if (listeEspecesCommunesCapt[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))]))) {  
           temp_pop <- log(mean(capt3[id_espece & capt3$Strate==3,"Nombre"]))/log(mean(capt3[id_espece & capt3$Strate==1,"Nombre"]))
        }              
       Ratio_logN_pop <- rbind(Ratio_logN_pop,temp_pop)
    }
    rownames(Ratio_logN_pop) <- listeEspecesCommunesCapt
    Ratio_logN_pop <- data.frame(na.omit(Ratio_logN_pop))
    colnames(Ratio_logN_pop) <- "LogN"

  # calcul du ratio des tailles moyennes par espèce
    Ratio_LongueurMoy_pop <- NULL
    for (i in 1:length(listeEspecesCommunesSize)) {
        id_espece=is.element(size3$Espece, listeEspecesCommunesSize[i])
  # T1<T2>T3 ou T1>T2<T3 ou T3=T2=T1 (non significatif) => T3/T1
        if (listeEspecesCommunesSize[i] %in% EspAbondanceT2eqT1) { 
           temp2_pop <- log(mean(size3[id_espece & size3$Strate==3,"Nombre"]))/log(mean(size3[id_espece & size3$Strate==1,"Nombre"])) 
        }
  # T1=T2<T3 ou T1=T2>T3 => T3 / mean(T1,T2)
        if (listeEspecesCommunesSize[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))]))) {
           temp2_pop <- log(mean(size3[id_espece & size3$Strate==3,"Nombre"]))/log((mean(size3[id_espece & size3$Strate==2,"Nombre"]) + mean(size3[id_espece & size3$Strate==1,"Nombre"]))/2)        
        }
  # T1<T2=T3 ou T1>T2=T3 => mean(T3,T2) / T1
        if (listeEspecesCommunesSize[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0))]))) {
           temp2_pop <- log((mean(size3[id_espece & size3$Strate==2,"Nombre"]) + mean(size3[id_espece & size3$Strate==3,"Nombre"]))/2) / log(mean(size3[id_espece & size3$Strate==1,"Nombre"]))
        }  
  # T1>T2>T3 ou T1<T2<T3 => T3/T1
        if (listeEspecesCommunesSize[i] %in% union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]),names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))]))) {  
           temp2_pop <- log(mean(size3[id_espece & size3$Strate==3,"Nombre"]))/log(mean(size3[id_espece & size3$Strate==1,"Nombre"]))
        }                               
        Ratio_LongueurMoy_pop <- rbind(Ratio_LongueurMoy_pop,temp2_pop)
    }
    rownames(Ratio_LongueurMoy_pop) <- listeEspecesCommunesSize
    Ratio_LongueurMoy_pop <- data.frame(na.omit(Ratio_LongueurMoy_pop)) 
    colnames(Ratio_LongueurMoy_pop) <- c("LongueurMoy")

# Radar Plot

    #Transformation des ratios en scores 
        
#--- population 
    popLogN <- length(which(Ratio_logN_pop$LogN>1))/nrow(Ratio_logN_pop)*100
    popLongueurMoy <- length(which(Ratio_LongueurMoy_pop$LongueurMoy>1))/nrow(Ratio_LongueurMoy_pop)*100
    pop_ratio <-  data.frame(rbind(popLogN,popLongueurMoy))
    colnames(pop_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios
    brk_scale_pop <- c(0,25,50,75,100)
    pop_ratio$ClassLimit <- cut(pop_ratio[,1], breaks=brk_scale_pop, right=TRUE)
    pop_ratio$Significatif <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(0,25]"] <- -1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(25,50]"] <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(50,75]"] <- 2
    pop_ratio$Classification[pop_ratio$ClassLimit=="(75,100]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (length(listeEspecesSignificatives_ln[!is.na(listeEspecesSignificatives_ln)])==0){
        pop_ratio$Significatif[1] <- 0
        pop_ratio$Classification[1] <- 0        
    }
    if (length(listeEspecesSignificatives_Size[!is.na(listeEspecesSignificatives_Size)])==0){
        pop_ratio$Significatif[2] <- 0
        pop_ratio$Classification[2] <- 0         
    }   
  
    cons_ratio <- data.frame(rbind(Ratio_Simp,Ratio_vuln,Ratio_logN)) 
    colnames(cons_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios    
    brk_scale_com <- c(0,1,1.25,1.5,1.75)
    cons_ratio$ClassLimit <- cut(cons_ratio[,1], breaks=brk_scale_com, right=TRUE)
    cons_ratio$Significatif <- 1
    cons_ratio$Classification[cons_ratio$IndVal<1] <- -1
    cons_ratio$Classification[cons_ratio$ClassLimit=="(0,1]"] <- 0    
    cons_ratio$Classification[cons_ratio$ClassLimit=="(1,1.25]"] <- 1
    cons_ratio$Classification[cons_ratio$ClassLimit=="(1.25,1.5]"] <- 2
    cons_ratio$Classification[cons_ratio$ClassLimit=="(1.5,1.75]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (Simp_res$anova.table[[5]][1]>0.05){
        cons_ratio$Significatif[1] <- 0
        cons_ratio$Classification[1] <- 0        
    }
    if (Vuln_res$anova.table[[5]][1]>0.05){
        cons_ratio$Significatif[2] <- 0
        cons_ratio$Classification[2] <- 0        
    }        
    if (Abun_res$anova.table[[5]][1]>0.05){
        cons_ratio$Significatif[3] <- 0
        cons_ratio$Classification[3] <- 0
    }      
    
    #ordre : Ratio_Simp, Ratio_Vuln, Ratio_logN, popLogN, popLongueurMoy
    cons_ratio <- rbind(cons_ratio,pop_ratio)
    assign("cons_ratio",cons_ratio,envir=.GlobalEnv) 
 
#    par(cex.axis=1)
#    radial.plot(cons_ratio$Classification,labels=c("Simpson","Abondance\nesp.vuln","Abondance totale","% esp.abondance","% esp.taille"),
#    rp.type="p",label.pos=NULL,radlab=FALSE, start=0,clockwise=FALSE,label.prop=1.3,
#    line.col="red",lty=par("lty"),lwd=2,mar=c(5,5,5,5), point.symbols=4, radial.lim=c(-1,3),show.grid=T,poly.col="yellow")                
#    mtext("Radarplot objectif conservation",side = 3, line=3, cex=1.5)
#              
} #fin Radar_Plot_Conservation_Gradient


################################################################################
## Nom     : Radar_Plot_Conservation_TS.f
## Objet   :
## Input   :
## Output  :
################################################################################

Radar_Plot_Conservation_TS.f = function(capt, size, titre) {
    #graphics.off()
    #dev.new(record = TRUE)
    #.SavedPlots <- NULL

    listeAnnees <- unique(capt$Annee)

    captAnneeDebut <- subset(capt, Annee == min(listeAnnees))
    captAnneeFin <- subset(capt, Annee == max(listeAnnees))
    sizeAnneeDebut <- subset(size, Annee == min(listeAnnees))
    sizeAnneeFin <- subset(size, Annee == max(listeAnnees))
    
    # calcul du ratio de l'abondance moyenne des captures annee fin / annee debut
    id_abondance_debut=is.element(captAnneeDebut$Espece, listeEspecesCommunesCapt)
    id_abondance_fin=is.element(captAnneeFin$Espece, listeEspecesCommunesCapt)    
    Ratio_logN <- log(mean(captAnneeFin[id_abondance_fin,"Nombre"]))/log(mean(captAnneeDebut[id_abondance_debut,"Nombre"]))

    # Diversité Simpson
    captAnneeDebut.mat <- contingence.fct(captAnneeDebut)
    captAnneeFin.mat <- contingence.fct(captAnneeFin)        
    SimpDebut <- cbind(diversity(captAnneeDebut.mat[,-c(1,2)],'simpson'),c(rep(1,nrow(captAnneeDebut.mat))))
    SimpFin <- cbind(diversity(captAnneeFin.mat[,-c(1,2)],'simpson'),c(rep(2,nrow(captAnneeFin.mat))))
    Simpdata <- data.frame(rbind(SimpDebut,SimpFin)) 
    colnames(Simpdata) <- c("Simp","Time")        
    Ratio_Simp <- mean(SimpFin)/mean(SimpDebut)

    # Abondance des especes vulnérables
    id_vuln_debut=is.element(captAnneeDebut$Espece, LVuln)
    id_vuln_fin=is.element(captAnneeFin$Espece, LVuln)
    Ratio_vuln <- log(mean(captAnneeFin[id_vuln_fin,"Nombre"]))/log(mean(captAnneeDebut[id_vuln_debut,"Nombre"]))

## Population
    Ratio_logN_pop <- NULL
    for (i in 1:length(listeEspecesCommunesCapt)) {
        id_debut=is.element(captAnneeDebut$Espece, listeEspecesCommunesCapt[i])
        id_fin=is.element(captAnneeFin$Espece, listeEspecesCommunesCapt[i])                
        temp_pop <- log(mean(captAnneeFin[id_fin,"Nombre"]))/log(mean(captAnneeDebut[id_debut,"Nombre"]))
        Ratio_logN_pop <- rbind(Ratio_logN_pop,temp_pop)
    }
    rownames(Ratio_logN_pop) <- listeEspecesCommunesCapt
    Ratio_logN_pop <- data.frame(na.omit(Ratio_logN_pop))
    colnames(Ratio_logN_pop) <- "LogN"

  # calcul du ratio des tailles moyennes par espèce
    Ratio_LongueurMoy_pop <- NULL
    for (i in 1:length(listeEspecesCommunesSize)) {
        id_debut=is.element(sizeAnneeDebut$Espece, listeEspecesCommunesSize[i])
        id_fin=is.element(sizeAnneeFin$Espece, listeEspecesCommunesSize[i])
        temp2_pop <- log(mean(sizeAnneeFin[id_fin,"Longueur"]))/log(mean(sizeAnneeDebut[id_debut,"Longueur"]))
        Ratio_LongueurMoy_pop <- rbind(Ratio_LongueurMoy_pop,temp2_pop)
    }
    rownames(Ratio_LongueurMoy_pop) <- listeEspecesCommunesSize
    Ratio_LongueurMoy_pop <- data.frame(na.omit(Ratio_LongueurMoy_pop)) 
    colnames(Ratio_LongueurMoy_pop) <- c("LongueurMoy")
    
# Radar Plot

    #Transformation des ratios en scores 
        
#--- population 
    popLogN <- length(which(Ratio_logN_pop$LogN>1))/nrow(Ratio_logN_pop)*100
    popLongueurMoy <- length(which(Ratio_LongueurMoy_pop$LongueurMoy>1))/nrow(Ratio_LongueurMoy_pop)*100
    pop_ratio <-  data.frame(rbind(popLogN,popLongueurMoy))
    colnames(pop_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios
    brk_scale_pop <- c(0,25,50,75,100)
    pop_ratio$ClassLimit <- cut(pop_ratio[,1], breaks=brk_scale_pop, right=TRUE)
    pop_ratio$Significatif <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(0,25]"] <- -1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(25,50]"] <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(50,75]"] <- 2
    pop_ratio$Classification[pop_ratio$ClassLimit=="(75,100]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (length(listeEspecesSignificatives_ln[!is.na(listeEspecesSignificatives_ln)])==0){
        pop_ratio$Significatif[1] <- 0
        pop_ratio$Classification[1] <- 0        
    }
    if (length(listeEspecesSignificatives_Size[!is.na(listeEspecesSignificatives_Size)])==0){
        pop_ratio$Significatif[2] <- 0
        pop_ratio$Classification[2] <- 0         
    }    
  
    cons_ratio <- data.frame(rbind(Ratio_Simp,Ratio_vuln,Ratio_logN)) 
    colnames(cons_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios    
    brk_scale_com <- c(0,1,1.25,1.5,1.75)
    cons_ratio$ClassLimit <- cut(cons_ratio[,1], breaks=brk_scale_com, right=TRUE)
    cons_ratio$Significatif <- 1
    cons_ratio$Classification[cons_ratio$IndVal<1] <- -1
    cons_ratio$Classification[cons_ratio$ClassLimit=="(0,1]"] <- 0    
    cons_ratio$Classification[cons_ratio$ClassLimit=="(1,1.25]"] <- 1
    cons_ratio$Classification[cons_ratio$ClassLimit=="(1.25,1.5]"] <- 2
    cons_ratio$Classification[cons_ratio$ClassLimit=="(1.5,1.75]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (Simp_res$coefficients[[8]]>0.05){
        cons_ratio$Significatif[1] <- 0
        cons_ratio$Classification[1] <- 0        
    }
    if (Vuln_res$coefficients[[8]]>0.05){
        cons_ratio$Significatif[2] <- 0
        cons_ratio$Classification[2] <- 0        
    }        
    if (Abun_res$coefficients[[8]]>0.05){
        cons_ratio$Significatif[3] <- 0
        cons_ratio$Classification[3] <- 0
    }      
    
    #ordre : Ratio_Simp, Ratio_Vuln, Ratio_logN, popLogN, popLongueurMoy
    cons_ratio <- rbind(cons_ratio,pop_ratio)
    assign("cons_ratio",cons_ratio,envir=.GlobalEnv) 
 
    par(cex.axis=1)
    radial.plot(cons_ratio$Classification,labels=c("Simpson","Abondance\nesp.vuln","Abondance totale","% esp.abondance","% esp.taille"),
    rp.type="p",label.pos=NULL,radlab=FALSE, start=0,clockwise=FALSE,label.prop=1.3,
    line.col="red",lty=par("lty"),lwd=2,mar=c(5,5,5,5), point.symbols=4, radial.lim=c(-1,3),show.grid=T,poly.col="yellow")                
    mtext("Radarplot objectif conservation",side = 3, line=3, cex=1.5)
              
} #fin Radar_Plot_Conservation_TS

################################################################################
## Nom     : Radar_Plot_Peche_TS.f
## Objet   :
## Input   :
## Output  :
################################################################################

Radar_Plot_Peche_TS.f = function(capt, size, titre) {
#    graphics.off()
#    dev.new(record = TRUE)
#    .SavedPlots <- NULL

    listeAnnees <- unique(capt$Annee)

    captAnneeDebut <- subset(capt, Annee == min(listeAnnees))
    captAnneeFin <- subset(capt, Annee == max(listeAnnees))
    sizeAnneeDebut <- subset(size, Annee == min(listeAnnees))
    sizeAnneeFin <- subset(size, Annee == max(listeAnnees))
    
    # calcul du ratio de l'abondance moyenne des captures annee fin / annee debut
    id_abondance_debut=is.element(captAnneeDebut$Espece, listeEspecesCommunesCapt)
    id_abondance_fin=is.element(captAnneeFin$Espece, listeEspecesCommunesCapt)    
    Ratio_logN <- log(mean(captAnneeFin[id_abondance_fin,"Nombre"]))/log(mean(captAnneeDebut[id_abondance_debut,"Nombre"]))
   
    # Longueur moyenne  
    id_longueur_debut=is.element(sizeAnneeDebut$Espece, listeEspecesCommunesSize)
    id_longueur_fin=is.element(sizeAnneeFin$Espece, listeEspecesCommunesSize)
    Ratio_LongueurMoy <- log(mean(sizeAnneeFin[id_longueur_fin,"Longueur"]))/log(mean(sizeAnneeDebut[id_longueur_debut,"Longueur"]))

    # Abondance des especes cibles
    id_cible_debut=is.element(captAnneeDebut$Espece, LCible)
    id_cible_fin=is.element(captAnneeFin$Espece, LCible)
    Ratio_Cible <- log(mean(captAnneeFin[id_cible_fin,"Nombre"]))/log(mean(captAnneeDebut[id_cible_debut,"Nombre"]))

## Population
    Ratio_logN_pop <- NULL
    for (i in 1:length(listeEspecesCommunesCapt)) {
        id_debut=is.element(captAnneeDebut$Espece, listeEspecesCommunesCapt[i])
        id_fin=is.element(captAnneeFin$Espece, listeEspecesCommunesCapt[i])                
        temp_pop <- log(mean(captAnneeFin[id_fin,"Nombre"]))/log(mean(captAnneeDebut[id_debut,"Nombre"]))
        Ratio_logN_pop <- rbind(Ratio_logN_pop,temp_pop)
    }
    rownames(Ratio_logN_pop) <- listeEspecesCommunesCapt
    Ratio_logN_pop <- data.frame(na.omit(Ratio_logN_pop))
    colnames(Ratio_logN_pop) <- "LogN"

  # calcul du ratio des tailles moyennes par espèce
    Ratio_LongueurMoy_pop <- NULL
    for (i in 1:length(listeEspecesCommunesSize)) {
        id_debut=is.element(sizeAnneeDebut$Espece, listeEspecesCommunesSize[i])
        id_fin=is.element(sizeAnneeFin$Espece, listeEspecesCommunesSize[i])
        temp2_pop <- log(mean(sizeAnneeFin[id_fin,"Longueur"]))/log(mean(sizeAnneeDebut[id_debut,"Longueur"]))
        Ratio_LongueurMoy_pop <- rbind(Ratio_LongueurMoy_pop,temp2_pop)
    }
    rownames(Ratio_LongueurMoy_pop) <- listeEspecesCommunesSize
    Ratio_LongueurMoy_pop <- data.frame(na.omit(Ratio_LongueurMoy_pop)) 
    colnames(Ratio_LongueurMoy_pop) <- c("LongueurMoy")
    
# Radar Plot

    #Transformation des ratios en scores 
        
#--- population 
    popLogN <- length(which(Ratio_logN_pop$LogN>1))/nrow(Ratio_logN_pop)*100
    popLongueurMoy <- length(which(Ratio_LongueurMoy_pop$LongueurMoy>1))/nrow(Ratio_LongueurMoy_pop)*100
    pop_ratio <-  data.frame(rbind(popLogN,popLongueurMoy))
    colnames(pop_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios
    brk_scale_pop <- c(0,25,50,75,100)
    pop_ratio$ClassLimit <- cut(pop_ratio[,1], breaks=brk_scale_pop, right=TRUE)
    pop_ratio$Significatif <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(0,25]"] <- -1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(25,50]"] <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(50,75]"] <- 2
    pop_ratio$Classification[pop_ratio$ClassLimit=="(75,100]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (length(listeEspecesSignificatives_ln[!is.na(listeEspecesSignificatives_ln)])==0){
        pop_ratio$Significatif[1] <- 0
        pop_ratio$Classification[1] <- 0        
    }
    if (length(listeEspecesSignificatives_Size[!is.na(listeEspecesSignificatives_Size)])==0){
        pop_ratio$Significatif[2] <- 0
        pop_ratio$Classification[2] <- 0         
    }    
  
    peche_ratio <- data.frame(rbind(Ratio_Cible,Ratio_LongueurMoy,Ratio_logN))
    colnames(peche_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios    
    brk_scale_com <- c(0,1,1.25,1.5,1.75)
    peche_ratio$ClassLimit <- cut(peche_ratio[,1], breaks=brk_scale_com, right=TRUE)
    peche_ratio$Significatif <- 1
    peche_ratio$Classification[peche_ratio$IndVal<1] <- -1
    peche_ratio$Classification[peche_ratio$ClassLimit=="(0,1]"] <- 0    
    peche_ratio$Classification[peche_ratio$ClassLimit=="(1,1.25]"] <- 1
    peche_ratio$Classification[peche_ratio$ClassLimit=="(1.25,1.5]"] <- 2
    peche_ratio$Classification[peche_ratio$ClassLimit=="(1.5,1.75]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (Cible_res$coefficients[[8]]>0.05){
        peche_ratio$Significatif[1] <- 0
        peche_ratio$Classification[1] <- 0        
    }
    if (Size_res$coefficients[[8]]>0.05){
        peche_ratio$Significatif[2] <- 0
        peche_ratio$Classification[2] <- 0        
    }        
    if (Abun_res$coefficients[[8]]>0.05){
        peche_ratio$Significatif[3] <- 0
        peche_ratio$Classification[3] <- 0
    }      
    
    #ordre : Ratio_Cible, Ratio_LongueurMoy, Ratio_logN, popLogN, popLongueurMoy
    peche_ratio <- rbind(peche_ratio,pop_ratio)
    assign("peche_ratio",peche_ratio,envir=.GlobalEnv) 
 
#    par(cex.axis=1)
#    radial.plot(peche_ratio$Classification,labels=c("Abondance\nesp.cibles","Longueur moyenne","Abondance totale","% esp.abondance","% esp.taille"),
#    rp.type="p",label.pos=NULL,radlab=FALSE, start=0,clockwise=FALSE,label.prop=1.3,
#    line.col="red",lty=par("lty"),lwd=2,mar=c(5,5,5,5), point.symbols=4, radial.lim=c(-1,3),show.grid=T,poly.col="yellow")                
#    mtext("Radarplot objectif peche",side = 3, line=3, cex=1.5)
#              
} #fin Radar_Plot_Peche_TS

################################################################################
## Nom     : Radar_Plot_Peche_Spatial.f
## Objet   : ????
## Input   :
## Output  :
################################################################################

Radar_Plot_Peche_Spatial.f = function(capt2, size2, titre) {
#    graphics.off()
#    dev.new(record = TRUE)
#    .SavedPlots <- NULL

   # Abondance totale moyenne log(N)
   # calcul du ratio de l'abondance moyenne des captures Strate = 2 / moyenne des captures Strate = 1
     id_abondance=is.element(capt2$Espece, listeEspecesCommunesCapt)
     Ratio_logN <- log(mean(capt2[id_abondance & capt2$Strate==2,"Nombre"]))/log(mean(capt2[id_abondance & capt2$Strate==1,"Nombre"]))

   # Longueur moyenne 
     id_longueur=is.element(size2$Espece, listeEspecesCommunesCapt)
     Ratio_LongueurMoy <- log(mean(size2[id_longueur & size2$Strate==2,"Longueur"]))/log(mean(size2[id_longueur & size2$Strate==1,"Longueur"]))

   # Abondance des especes cibles
     id_cible=is.element(capt2$Espece, LCible)
     Ratio_Cible <- log(mean(capt2[id_cible & capt2$Strate==2,"Nombre"]))/log(mean(capt2[id_cible & capt2$Strate==1,"Nombre"]))

## Population
  # calcul du ratio de l'abondance totale moyenne des captures par espèce
    Ratio_logN_pop <- NULL
    for (i in 1:length(listeEspecesCommunesCapt)) {
        id=is.element(capt2$Espece, listeEspecesCommunesCapt[i])
        temp_pop <- log(mean(capt2[id & capt2$Strate==2,"Nombre"]))/log(mean(capt2[id & capt2$Strate==1,"Nombre"]))
        Ratio_logN_pop <- rbind(Ratio_logN_pop,temp_pop)
    }
    rownames(Ratio_logN_pop) <- listeEspecesCommunesCapt
    Ratio_logN_pop <- data.frame(na.omit(Ratio_logN_pop))
    colnames(Ratio_logN_pop) <- "LogN"

  # calcul du ratio des tailles moyennes par espèce
    Ratio_LongueurMoy_pop <- NULL
    for (i in 1:length(listeEspecesCommunesSize)) {
        id=is.element(size2$Espece, listeEspecesCommunesSize[i])
        temp2_pop <- log(mean(size2[id & size2$Strate==2,"Longueur"]))/log(mean(size2[id & size2$Strate==1,"Longueur"]))
        Ratio_LongueurMoy_pop <- rbind(Ratio_LongueurMoy_pop,temp2_pop)
    }
    rownames(Ratio_LongueurMoy_pop) <- listeEspecesCommunesSize
    Ratio_LongueurMoy_pop <- data.frame(na.omit(Ratio_LongueurMoy_pop)) 
    colnames(Ratio_LongueurMoy_pop) <- c("LongueurMoy")

# Radar Plot

    #Transformation des ratios en scores 
#--- population    
    popLogN <- length(which(Ratio_logN_pop$LogN>1))/nrow(Ratio_logN_pop)*100
    popLongueurMoy <- length(which(Ratio_LongueurMoy_pop$LongueurMoy>1))/nrow(Ratio_LongueurMoy_pop)*100
    pop_ratio <-  data.frame(rbind(popLogN,popLongueurMoy))
    colnames(pop_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios
    brk_scale_pop <- c(0,25,50,75,100)
    pop_ratio$ClassLimit <- cut(pop_ratio[,1], breaks=brk_scale_pop, right=TRUE)
    pop_ratio$Significatif <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(0,25]"] <- -1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(25,50]"] <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(50,75]"] <- 2
    pop_ratio$Classification[pop_ratio$ClassLimit=="(75,100]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (length(listeEspecesSignificatives_ln[!is.na(listeEspecesSignificatives_ln)])==0){
        pop_ratio$Significatif[1] <- 0
        pop_ratio$Classification[1] <- 0        
    }
    if (length(listeEspecesSignificatives_Size[!is.na(listeEspecesSignificatives_Size)])==0){
        pop_ratio$Significatif[2] <- 0
        pop_ratio$Classification[2] <- 0         
    }    

#--- communaute    
    peche_ratio <- data.frame(rbind(Ratio_Cible,Ratio_LongueurMoy,Ratio_logN))
    colnames(peche_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios    
    brk_scale_com <- c(0,1,1.25,1.5,1.75)
    peche_ratio$ClassLimit <- cut(peche_ratio[,1], breaks=brk_scale_com, right=TRUE)
    peche_ratio$Significatif <- 1
    peche_ratio$Classification[peche_ratio$IndVal<1] <- -1
    peche_ratio$Classification[peche_ratio$ClassLimit=="(0,1]"] <- 0    
    peche_ratio$Classification[peche_ratio$ClassLimit=="(1,1.25]"] <- 1
    peche_ratio$Classification[peche_ratio$ClassLimit=="(1.25,1.5]"] <- 2
    peche_ratio$Classification[peche_ratio$ClassLimit=="(1.5,1.75]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (Cible_res$anova.table[[5]][1]>0.05){
        peche_ratio$Significatif[1] <- 0
        peche_ratio$Classification[1] <- 0        
    }
    if (Size_res$anova.table[[5]][1]>0.05){
        peche_ratio$Significatif[2] <- 0
        peche_ratio$Classification[2] <- 0        
    }        
    if (Abun_res$anova.table[[5]][1]>0.05){
        peche_ratio$Significatif[3] <- 0
        peche_ratio$Classification[3] <- 0
    }  

    #ordre : Ratio_Cible, Ratio_LongueurMoy, Ratio_logN, popLogN, popLongueurMoy
    peche_ratio <- rbind(peche_ratio,pop_ratio)
    assign("peche_ratio",peche_ratio,envir=.GlobalEnv) 
             
#    par(cex.axis=1)
#    radial.plot(peche_ratio$Classification,labels=c("Abondance\nesp.cibles","Longueur moyenne","Abondance totale","% esp.abondance","% esp.taille"),
#    rp.type="p",label.pos=NULL,radlab=FALSE, start=0,clockwise=FALSE,label.prop=1.3,
#    line.col="red",lty=par("lty"),lwd=2,mar=c(5,5,5,5), point.symbols=4,  radial.lim=c(-1,3),show.grid=T,poly.col="yellow")                
#    mtext("Radarplot objectif peche",side = 3, line=3, cex=1.5)
#   
#    savePlot(filename = paste("Radial_plot_peche_spatial",titre), "png")   
#
}#fin Radar_Plot_Peche_Spatial

################################################################################
## Nom     : Radar_Plot_Conservation_Spatial.f
## Objet   :
## Input   :
## Output  :
################################################################################

Radar_Plot_Conservation_Spatial.f = function(capt_Un.mat, capt_Deux.mat, capt2, size2, titre) {
#    graphics.off()
#    dev.new(record = TRUE)
#    .SavedPlots <- NULL

    # calcul du ratio de l'abondance moyenne des captures Strate = 2 / moyenne des captures Strate = 1
    id_abondance=is.element(capt2$Espece, listeEspecesCommunesCapt)
    Ratio_logN <- log(mean(capt2[id_abondance & capt2$Strate==2,"Nombre"]))/log(mean(capt2[id_abondance & capt2$Strate==1,"Nombre"]))

    # Diversité Simpson
    SimpBef <- cbind(diversity(capt_Un.mat[,-c(1,2)],'simpson'),c(rep(1,nrow(capt_Un.mat))))
    SimpAft <- cbind(diversity(capt_Deux.mat[,-c(1,2)],'simpson'),c(rep(2,nrow(capt_Deux.mat))))
    Simpdata <- data.frame(rbind(SimpBef,SimpAft)) 
    colnames(Simpdata) <- c("Simp","Strate")
    Ratio_Simp <-  mean(SimpAft)/mean(SimpBef)

    # Abondance des especes vulnérables
    id_vuln=is.element(capt2$Espece, LVuln)
    Ratio_vuln <- log(mean(capt2[id_vuln & capt2$Strate==2,"Nombre"]))/log(mean(capt2[id_vuln & capt2$Strate==1,"Nombre"]))

## Population
  # calcul du ratio de l'abondance moyenne des captures par espèce
    Ratio_logN_pop <- NULL
    for (i in 1:length(listeEspecesCommunesCapt)) {
        id=is.element(capt2$Espece, listeEspecesCommunesCapt[i])
        temp_pop <- log(mean(capt2[id & capt2$Strate==2,"Nombre"]))/log(mean(capt2[id & capt2$Strate==1,"Nombre"]))
        Ratio_logN_pop <- matrix(rbind(temp_pop,Ratio_logN_pop), ncol=1)
    }
    rownames(Ratio_logN_pop) <- listeEspecesCommunesCapt
    Ratio_logN_pop <- data.frame(na.omit(Ratio_logN_pop))
    colnames(Ratio_logN_pop) <- "LogN"

  # calcul du ratio des tailles moyennes par espèce
    Ratio_LongueurMoy_pop <- NULL
    for (i in 1:length(listeEspecesCommunesSize)) {
        id=is.element(size2$Espece, listeEspecesCommunesSize[i])
        temp2_pop <- log(mean(size2[id & size2$Strate==2,"Longueur"]))/log(mean(size2[id & size2$Strate==1,"Longueur"]))
        Ratio_LongueurMoy_pop <- matrix(rbind(temp2_pop,Ratio_LongueurMoy_pop), ncol=1)
    }
    rownames(Ratio_LongueurMoy_pop) <- listeEspecesCommunesSize
    Ratio_LongueurMoy_pop <- data.frame(na.omit(Ratio_LongueurMoy_pop)) 
    colnames(Ratio_LongueurMoy_pop) <- c("LongueurMoy")

# Radar Plot

    #Transformation des ratios en scores 
        
#--- population 
    popLogN <- length(which(Ratio_logN_pop$LogN>1))/nrow(Ratio_logN_pop)*100
    popLongueurMoy <- length(which(Ratio_LongueurMoy_pop$LongueurMoy>1))/nrow(Ratio_LongueurMoy_pop)*100
    pop_ratio <-  data.frame(rbind(popLogN,popLongueurMoy))
    colnames(pop_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios
    brk_scale_pop <- c(0,25,50,75,100)
    pop_ratio$ClassLimit <- cut(pop_ratio[,1], breaks=brk_scale_pop, right=TRUE)
    pop_ratio$Significatif <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(0,25]"] <- -1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(25,50]"] <- 1
    pop_ratio$Classification[pop_ratio$ClassLimit=="(50,75]"] <- 2
    pop_ratio$Classification[pop_ratio$ClassLimit=="(75,100]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (length(listeEspecesSignificatives_ln[!is.na(listeEspecesSignificatives_ln)])==0){
        pop_ratio$Significatif[1] <- 0
        pop_ratio$Classification[1] <- 0        
    }
    if (length(listeEspecesSignificatives_Size[!is.na(listeEspecesSignificatives_Size)])==0){
        pop_ratio$Significatif[2] <- 0
        pop_ratio$Classification[2] <- 0         
    }   

    cons_ratio <- data.frame(rbind(Ratio_Simp,Ratio_vuln,Ratio_logN)) 
    colnames(cons_ratio) <- "IndVal"
    #affecte une classe au score calculé à partir des ratios    
    brk_scale_com <- c(0,1,1.25,1.5,1.75)
    cons_ratio$ClassLimit <- cut(cons_ratio[,1], breaks=brk_scale_com, right=TRUE)
    cons_ratio$Significatif <- 1
    cons_ratio$Classification[cons_ratio$IndVal<1] <- -1
    cons_ratio$Classification[cons_ratio$ClassLimit=="(0,1]"] <- 0    
    cons_ratio$Classification[cons_ratio$ClassLimit=="(1,1.25]"] <- 1
    cons_ratio$Classification[cons_ratio$ClassLimit=="(1.25,1.5]"] <- 2
    cons_ratio$Classification[cons_ratio$ClassLimit=="(1.5,1.75]"] <- 3
    #test d'abord si les indicateurs Abundata et Sizedata sont significatifs   
    if (Simp_res$anova.table[[5]][1]>0.05){
        cons_ratio$Significatif[1] <- 0
        cons_ratio$Classification[1] <- 0        
    }
    if (Vuln_res$anova.table[[5]][1]>0.05){
        cons_ratio$Significatif[2] <- 0
        cons_ratio$Classification[2] <- 0        
    }        
    if (Abun_res$anova.table[[5]][1]>0.05){
        cons_ratio$Significatif[3] <- 0
        cons_ratio$Classification[3] <- 0
    }      
    
    #ordre : Ratio_Simp, Ratio_Vuln, Ratio_logN, popLogN, popLongueurMoy
    cons_ratio <- rbind(cons_ratio,pop_ratio)
    assign("cons_ratio",cons_ratio,envir=.GlobalEnv) 
 
#    par(cex.axis=1)
#    radial.plot(cons_ratio$Classification,labels=c("Simpson","Abondance\nesp.vuln","Abondance totale","% esp.abondance","% esp.taille"),
#    rp.type="p",label.pos=NULL,radlab=FALSE, start=0,clockwise=FALSE,label.prop=1.3,
#    line.col="red",lty=par("lty"),lwd=2,mar=c(5,5,5,5), point.symbols=4, radial.lim=c(-1,3),show.grid=T,poly.col="yellow")                
#    mtext("Radarplot objectif conservation",side = 3, line=3, cex=1.5)
#              
} #fin Radar_Plot_Conservation_Spatial