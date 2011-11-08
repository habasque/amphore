################################################################################
## Nom     : anova_indicateurs_population.f
## Objet   :
## Input   :
## Output  :
################################################################################

Anova_Indicateurs_Population.f = function(capt2, size2, titre, facteur, facteur2) {

# log(N)

    Abundata <- vector("list",length(listeEspecesCommunesCapt))
    names(Abundata) <- listeEspecesCommunesCapt
    p.abund <- vector("list",length(listeEspecesCommunesCapt))
    names(p.abund) <- listeEspecesCommunesCapt
    listeEspeces_Nb_ANOVA_impossible <- NA
    nbEspeces_Nb_ANOVA_impossible <- 0       
    for (i in 1:length(listeEspecesCommunesCapt)) {
        id=is.element(capt2$Espece, listeEspecesCommunesCapt[i])
        ind.trait_temp <- tapply(capt2[id,"Nombre"], capt2[id,"Trait"],sum)
        ind.trait = as.data.frame(matrix(NA,dim(ind.trait_temp)[1],2))
        colnames(ind.trait) = c("Trait","Individus")
        ind.trait$Trait = rep(dimnames(ind.trait_temp)[[1]])
        ind.trait$Individus = as.vector(ind.trait_temp,"numeric")
        fact.trait <- tapply(capt2[id,facteur], capt2[id,1],mean)
        #l'anova est utilisée lorsque l'espece traitee apparait dans au moins 3 traits
        if (length(unique(ind.trait$Trait[ind.trait$Individus>0 & !is.na(ind.trait$Individus)]))>2 ){
        #l'anova n'est pas utilisée lorsque pour une espece donnee, un seul individu
        # est observe par trait
        if (length(unique(ind.trait$Individus[!is.na(ind.trait$Individus)]))>1 && length(unique(fact.trait))>1) {
           Abundata[[i]] <- anova.1way(log(ind.trait_temp) ~ as.factor(fact.trait), data=capt2, nperm=99)$anova.table[5]
           p.abund[[i]] <- summary(anova.1way(log(ind.trait_temp) ~ as.factor(fact.trait), data=capt2, nperm=99)$anova.table[5][,1][1])
        } else {
           #print(paste("Anova non effectuee car un seul individu dans un des groupes pour l'espece",listeEspecesCommunesCapt[i]))
           nbEspeces_Nb_ANOVA_impossible <- nbEspeces_Nb_ANOVA_impossible +1
           listeEspeces_Nb_ANOVA_impossible[nbEspeces_Nb_ANOVA_impossible] <- listeEspecesCommunesCapt[i] 
        }
        } else {
           #print(paste("Anova non effectuee car l'espece",listeEspecesCommunesCapt[i],"apparait dans moins de 3 traits"))        
           nbEspeces_Nb_ANOVA_impossible <- nbEspeces_Nb_ANOVA_impossible +1           
           listeEspeces_Nb_ANOVA_impossible[nbEspeces_Nb_ANOVA_impossible] <- listeEspecesCommunesCapt[i]           
        }
    }
    assign("Abundata",Abundata,envir=.GlobalEnv)

    # affichage des especes pour lesquelles il y a des differences significatives pour ln
    listeEspecesSignificatives_ln <- NA
    nbEspecesSignificatives_ln <- 0
    for (i in 1:length(p.abund)) {
        #print(p.abund[i][(p.abund[[i]][1]<0.05)])
        if ( (!is.null(p.abund[[i]][1])) && (p.abund[[i]][1]<0.05)) {
           nbEspecesSignificatives_ln <- nbEspecesSignificatives_ln + 1
           listeEspecesSignificatives_ln[nbEspecesSignificatives_ln] <- names(p.abund[i])
        }
    }
    listeEspecesNonSignificatives_ln <- subset(listeEspecesCommunesCapt, ! listeEspecesCommunesCapt %in% listeEspecesSignificatives_ln)

    # liste des especes pour lesquelles il y a des differences significatives
    MoySignT1T2 <- data.frame(c(0,0),row.names=c("T1","T2"))
    if (!is.na(listeEspecesSignificatives_ln) && length(listeEspecesSignificatives_ln)> 0) {
       #x11(width=18,height=18,pointsize=12)
       #par(mfrow = c(ceiling(length(listeEspecesSignificatives_ln)/2), 2),oma = c(0, 0, 3, 0),mex = 0.7)
       for (i in 1:length(listeEspecesSignificatives_ln)) {
       #    plotmeans(log(capt2[capt2$Espece==listeEspecesSignificatives_ln[i],"Nombre"]) ~ as.factor(capt2[capt2$Espece==listeEspecesSignificatives_ln[i],facteur]),main=paste(listeEspecesSignificatives_ln[i]), xlab="facteur", ylab="log(n)")
           MoySignT1T2[1:2,i] <- tapply(log(capt2[capt2$Espece==listeEspecesSignificatives_ln[i],"Nombre"]),as.factor(capt2[capt2$Espece==listeEspecesSignificatives_ln[i],facteur]),mean)
       }
       #mtext(titre, side = 3, outer = TRUE, cex = 1.5)
       #savePlot(filename = paste("listeEspecesSignificatives_log(n)_",titre,sep=""), "png")       
    }
    
    names(MoySignT1T2) <- listeEspecesSignificatives_ln
    MoySignT1T2
    assign("MoySignT1T2",MoySignT1T2,envir=.GlobalEnv)

    #print(paste("Nombre d'especes significatives pour leurs abondances :", nbEspecesSignificatives_ln))
    assign("listeEspecesSignificatives_ln",listeEspecesSignificatives_ln,envir=.GlobalEnv)
    assign("listeEspecesNonSignificatives_ln",listeEspecesNonSignificatives_ln,envir=.GlobalEnv)
    assign("listeEspeces_Nb_ANOVA_impossible",listeEspeces_Nb_ANOVA_impossible,envir=.GlobalEnv)       

# Taille moyenne
    Sizedata <- vector("list",length(listeEspecesCommunesSize))
    names(Sizedata) <- listeEspecesCommunesSize
    p.size <- vector("list",length(listeEspecesCommunesSize))
    names(p.size) <- listeEspecesCommunesSize
    listeEspeces_Size_ANOVA_impossible <- NA
    nbEspeces_Size_ANOVA_impossible <- 0        
    for (i in 1:length(listeEspecesCommunesSize)) {
        id=is.element(size2$Espece, listeEspecesCommunesSize[i])
        ind.trait_temp <- tapply(size2[id,"Longueur"], size2[id,"Trait"],sum)
        ind.trait = as.data.frame(matrix(NA,dim(ind.trait_temp)[1],2))
        colnames(ind.trait) = c("Trait","Individus")
        ind.trait$Trait = rep(dimnames(ind.trait_temp)[[1]])
        ind.trait$Individus = as.vector(ind.trait_temp,"numeric")
        fact.trait <- tapply(size2[id,facteur2], size2[id,1],mean)
        #l'anova est utilisée lorsque l'espece traitee apparait dans au moins 3 traits
        if (length(unique(ind.trait$Trait[ind.trait$Individus>0 & !is.na(ind.trait$Individus)]))>2 ){
        #l'anova n'est pas utilisée lorsque pour une espece donnee, un seul individu
        # est observe par trait
        if (length(unique(ind.trait$Individus[!is.na(ind.trait$Individus)]))>1 && length(unique(fact.trait))>1) {
           Sizedata[[i]] <- anova.1way(log(ind.trait_temp) ~ as.factor(fact.trait), data=size2, nperm=99)$anova.table[5]
           p.size[[i]] <- summary(anova.1way(log(ind.trait_temp) ~ as.factor(fact.trait), data=size2, nperm=99)$anova.table[5][,1][1])
        } else {
           #print(paste("Anova non effectuee car un seul individu dans un des groupes pour l'espece",listeEspecesCommunesSize[i]))
           nbEspeces_Size_ANOVA_impossible <- nbEspeces_Size_ANOVA_impossible +1
           listeEspeces_Size_ANOVA_impossible[nbEspeces_Size_ANOVA_impossible] <- listeEspecesCommunesSize[i] 
        }
        } else {
           #print(paste("Anova non effectuee car l'espece",listeEspecesCommunesSize[i],"apparait dans moins de 4 traits"))        
           nbEspeces_Size_ANOVA_impossible <- nbEspeces_Size_ANOVA_impossible +1           
           listeEspeces_Size_ANOVA_impossible[nbEspeces_Size_ANOVA_impossible] <- listeEspecesCommunesSize[i]           
        }
    }
    assign("Sizedata",Sizedata,envir=.GlobalEnv)

    # affichage des especes pour lesquelles il y a des differences significatives
    listeEspecesSignificatives_Size <- NA
    nbEspecesTaillesSignificatives_Size <- 0
    for (i in 1:length(p.size)) {
        #print(p.size[i][(p.size[[i]][1]<0.05)])
        if ( (!is.null(p.size[[i]][1])) && (p.size[[i]][1]<0.05)) {
           nbEspecesTaillesSignificatives_Size <- nbEspecesTaillesSignificatives_Size + 1
           listeEspecesSignificatives_Size[nbEspecesTaillesSignificatives_Size] <- names(p.size[i])
        }
    }
    listeEspecesNonSignificatives_Size <- subset(listeEspecesCommunesSize, ! listeEspecesCommunesSize %in% listeEspecesSignificatives_Size)

    SizeSignT1T2 <- data.frame(c(0,0), row.names=c("T1","T2"))
    if (!is.na(listeEspecesSignificatives_Size) && length(listeEspecesSignificatives_Size)> 0) {
        #x11(width=18,height=18,pointsize=12)
        #par(mfrow = c(ceiling(length(listeEspecesSignificatives_Size)/2), 2),mex = 0.7)
        # liste des especes pour lesquelles il y a des differences de tailles significatives
        for (i in 1:length(listeEspecesSignificatives_Size)) {
        #    plotmeans(log(size2[size2$Espece==listeEspecesSignificatives_Size[i],"Longueur"]) ~ as.factor(size2[size2$Espece==listeEspecesSignificatives_Size[i],facteur2]),main=paste(listeEspecesSignificatives_Size[i]), xlab="facteur", ylab="log(taille)")
            SizeSignT1T2[1:2,i] <- tapply(log(size2[size2$Espece==listeEspecesSignificatives_Size[i],"Longueur"]),as.factor(size2[size2$Espece==listeEspecesSignificatives_Size[i],facteur2]),mean)
        }
        #mtext(titre, side = 3, outer = TRUE, cex = 1.5)
        #savePlot(filename = paste("listeEspecesSignificatives_Size_",titre,sep=""), "png")
    }

    names(SizeSignT1T2) <- listeEspecesSignificatives_Size
    SizeSignT1T2
    assign("SizeSignT1T2",SizeSignT1T2,envir=.GlobalEnv)

    #print(paste("Nombre d'especes significatives pour leurs tailles :", nbEspecesTaillesSignificatives_Size))
    assign("listeEspecesSignificatives_Size",listeEspecesSignificatives_Size,envir=.GlobalEnv)
    assign("listeEspecesNonSignificatives_Size",listeEspecesNonSignificatives_Size,envir=.GlobalEnv)
    assign("listeEspeces_Size_ANOVA_impossible",listeEspeces_Size_ANOVA_impossible,envir=.GlobalEnv)   
} # fin Anova_Indicateurs_Population

################################################################################
## Nom     : Anova_Indicateurs_Population_3grp.f
## Objet   :
## Input   :
## Output  :
################################################################################

Anova_Indicateurs_Population_3grp.f = function(capt3, size3, titre, facteur, facteur2) {

# log(N)

    Abundata <- vector("list",length(listeEspecesCommunesCapt))
    names(Abundata) <- listeEspecesCommunesCapt
    p.abund <- vector("list",length(listeEspecesCommunesCapt))
    names(p.abund) <- listeEspecesCommunesCapt
    listeEspeces_Nb_ANOVA_impossible <- NA
    nbEspeces_Nb_ANOVA_impossible <- 0      
    for (i in 1:length(listeEspecesCommunesCapt)) {
        id=is.element(capt3$Espece, listeEspecesCommunesCapt[i])
        ind.trait_temp <- tapply(capt3[id,"Nombre"], capt3[id,"Trait"],sum)
        ind.trait = as.data.frame(matrix(NA,dim(ind.trait_temp)[1],2))
        colnames(ind.trait) = c("Trait","Individus")
        ind.trait$Trait = rep(dimnames(ind.trait_temp)[[1]])
        ind.trait$Individus = as.vector(ind.trait_temp,"numeric")
        fact.trait <- tapply(capt3[id,facteur], capt3[id,1],mean)
        #l'anova est utilisée lorsque l'espece traitee apparait dans au moins 3 traits       
        if (length(unique(ind.trait$Trait[ind.trait$Individus>0 & !is.na(ind.trait$Individus)]))>3 ){
        #l'anova n'est pas utilisée lorsque pour une espece donnee, un seul individu est observe par trait       
        if (length(unique(ind.trait$Individus[!is.na(ind.trait$Individus) & (ind.trait$Individus!=1)]))>1 && length(unique(fact.trait))>1) {
           Abundata[[i]] <- anova.1way(log(ind.trait_temp) ~ as.factor(fact.trait), data=capt3, nperm=99)$anova.table[5]
           p.abund[[i]] <- summary(anova.1way(log(ind.trait_temp) ~ as.factor(fact.trait), data=capt3, nperm=99)$anova.table[5][,1][1])
        } else {
           #print(paste("Anova non effectuee car un seul individu dans un des groupes pour l'espece",listeEspecesCommunesCapt[i]))
           nbEspeces_Nb_ANOVA_impossible <- nbEspeces_Nb_ANOVA_impossible +1
           listeEspeces_Nb_ANOVA_impossible[nbEspeces_Nb_ANOVA_impossible] <- listeEspecesCommunesCapt[i] 
        }
        } else {
           #print(paste("Anova non effectuee car l'espece",listeEspecesCommunesCapt[i],"apparait dans moins de 4 traits"))        
           nbEspeces_Nb_ANOVA_impossible <- nbEspeces_Nb_ANOVA_impossible +1           
           listeEspeces_Nb_ANOVA_impossible[nbEspeces_Nb_ANOVA_impossible] <- listeEspecesCommunesCapt[i]           
        }
    }
    
    assign("Abundata",Abundata,envir=.GlobalEnv)

    # affichage des especes pour lesquelles il y a des differences significatives pour ln
    listeEspecesSignificatives_ln <- NA
    nbEspecesSignificatives_ln <- 0
    for (i in 1:length(p.abund)) {
        #print(p.abund[i][(p.abund[[i]][1]<0.05)])
        if ( (!is.null(p.abund[[i]][1])) && (p.abund[[i]][1]<0.05)) {
           nbEspecesSignificatives_ln <- nbEspecesSignificatives_ln + 1
           listeEspecesSignificatives_ln[nbEspecesSignificatives_ln] <- names(p.abund[i])
        }
    }
    listeEspecesNonSignificatives_ln <- subset(listeEspecesCommunesCapt, ! listeEspecesCommunesCapt %in% listeEspecesSignificatives_ln)

    # liste des especes pour lesquelles il y a des differences significatives
    MoySignT1T2T3 <- data.frame(c(0,0,0),row.names=c("T1","T2","T3"))
    if (!is.na(listeEspecesSignificatives_ln) && length(listeEspecesSignificatives_ln)> 0) {
       #x11(width=18,height=18,pointsize=12)
       #par(mfrow = c(ceiling(length(listeEspecesSignificatives_ln)/2), 2),oma = c(0, 0, 3, 0),mex = 0.7)
       for (i in 1:length(listeEspecesSignificatives_ln)) {
        #   plotmeans(log(capt3[capt3$Espece==listeEspecesSignificatives_ln[i],"Nombre"]) ~ as.factor(capt3[capt3$Espece==listeEspecesSignificatives_ln[i],facteur]),main=paste(listeEspecesSignificatives_ln[i]), xlab="facteur", ylab="log(n)")
           MoySignT1T2T3[1:3,i] <- tapply(log(capt3[capt3$Espece==listeEspecesSignificatives_ln[i],"Nombre"]),as.factor(capt3[capt3$Espece==listeEspecesSignificatives_ln[i],facteur]),mean)
       }
       #mtext(titre, side = 3, outer = TRUE, cex = 1.5)
       #savePlot(filename = paste("listeEspecesSignificatives_log(N)_",titre,sep=""), "png")       
    }

    names(MoySignT1T2T3) <- listeEspecesSignificatives_ln
    MoySignT1T2T3
    assign("MoySignT1T2T3",MoySignT1T2T3,envir=.GlobalEnv)

    #print(paste("Nombre d'especes significatives ln :", nbEspecesSignificatives_ln))
    assign("listeEspecesSignificatives_ln",listeEspecesSignificatives_ln,envir=.GlobalEnv)
    assign("listeEspecesNonSignificatives_ln",listeEspecesNonSignificatives_ln,envir=.GlobalEnv)
    assign("listeEspeces_Nb_ANOVA_impossible",listeEspeces_Nb_ANOVA_impossible,envir=.GlobalEnv)        

# Taille moyenne
    Sizedata <- vector("list",length(listeEspecesCommunesSize))
    names(Sizedata) <- listeEspecesCommunesSize
    p.size <- vector("list",length(listeEspecesCommunesSize))
    names(p.size) <- listeEspecesCommunesSize
    listeEspeces_Size_ANOVA_impossible <- NA
    nbEspeces_Size_ANOVA_impossible <- 0    
    for (i in 1:length(listeEspecesCommunesSize)) {
        id=is.element(size3$Espece, listeEspecesCommunesSize[i])
        ind.trait_temp <- tapply(size3[id,"Longueur"], size3[id,"Trait"],sum)
        ind.trait = as.data.frame(matrix(NA,dim(ind.trait_temp)[1],2))
        colnames(ind.trait) = c("Trait","Individus")
        ind.trait$Trait = rep(dimnames(ind.trait_temp)[[1]])
        ind.trait$Individus = as.vector(ind.trait_temp,"numeric")
        fact.trait <- tapply(size3[id,facteur2], size3[id,1],mean)
        #l'anova est utilisée lorsque l'espece traitee apparait dans au moins 4 traits
        if (length(unique(ind.trait$Trait[ind.trait$Individus>0 & !is.na(ind.trait$Individus)]))>3 ){
        #l'anova n'est pas utilisée lorsque pour une espece donnee, un seul individu est observe par trait
        if (length(unique(ind.trait$Individus[!is.na(ind.trait$Individus) & (ind.trait$Individus!=1)]))>1 && length(unique(fact.trait))>1) {
           Sizedata[[i]] <- anova.1way(log(ind.trait_temp) ~ as.factor(fact.trait), data=size3, nperm=99)$anova.table[5]
           p.size[[i]] <- summary(anova.1way(log(ind.trait_temp) ~ as.factor(fact.trait), data=size3, nperm=99)$anova.table[5][,1][1])
        } else {
           #print(paste("Anova non effectuee car un seul individu dans un des groupes pour l'espece",listeEspecesCommunesSize[i]))
           nbEspeces_Size_ANOVA_impossible <- nbEspeces_Size_ANOVA_impossible +1
           listeEspeces_Size_ANOVA_impossible[nbEspeces_Size_ANOVA_impossible] <- listeEspecesCommunesSize[i] 
        }
        } else {
           #print(paste("Anova non effectuee car l'espece",listeEspecesCommunesSize[i],"apparait dans moins de 4 traits"))        
           nbEspeces_Size_ANOVA_impossible <- nbEspeces_Size_ANOVA_impossible +1           
           listeEspeces_Size_ANOVA_impossible[nbEspeces_Size_ANOVA_impossible] <- listeEspecesCommunesSize[i]           
        }
    }
    
    assign("Sizedata",Sizedata,envir=.GlobalEnv)

    # affichage des especes pour lesquelles il y a des differences significatives
    listeEspecesSignificatives_Size <- NA
    nbEspecesTaillesSignificatives_Size <- 0
    for (i in 1:length(p.size)) {
        #print(p.size[i][(p.size[[i]][1]<0.05)])
        if ( (!is.null(p.size[[i]][1])) && (p.size[[i]][1]<0.05)) {
           nbEspecesTaillesSignificatives_Size <- nbEspecesTaillesSignificatives_Size + 1
           listeEspecesSignificatives_Size[nbEspecesTaillesSignificatives_Size] <- names(p.size[i])
        }
    }
    listeEspecesNonSignificatives_Size <- subset(listeEspecesCommunesSize, ! listeEspecesCommunesSize %in% listeEspecesSignificatives_Size)

    SizeSignT1T2T3 <- data.frame(c(0,0,0), row.names=c("T1","T2","T3"))
    if (!is.na(listeEspecesSignificatives_Size) && length(listeEspecesSignificatives_Size)> 0) {
        #x11(width=18,height=18,pointsize=12)
        #par(mfrow = c(ceiling(length(listeEspecesSignificatives_Size)/2), 2),oma = c(0, 0, 3, 0),mex = 0.7)        
        # liste des especes pour lesquelles il y a des differences de tailles significatives
        for (i in 1:length(listeEspecesSignificatives_Size)) {
        #    plotmeans(log(size3[size3$Espece==listeEspecesSignificatives_Size[i],"Longueur"]) ~ as.factor(size3[size3$Espece==listeEspecesSignificatives_Size[i],facteur2]),main=paste(listeEspecesSignificatives_Size[i]), xlab="facteur", ylab="log(taille)")
            SizeSignT1T2T3[1:3,i] <- tapply(log(size3[size3$Espece==listeEspecesSignificatives_Size[i],"Longueur"]),as.factor(size3[size3$Espece==listeEspecesSignificatives_Size[i],facteur2]),mean)
        }
        #mtext(titre, side = 3, outer = TRUE, cex = 1.5)
        #savePlot(filename = paste("listeEspecesSignificatives_Size_",titre,sep=""), "png")
    }   
    
    names(SizeSignT1T2T3) <- listeEspecesSignificatives_Size
    SizeSignT1T2T3
    assign("SizeSignT1T2T3",SizeSignT1T2T3,envir=.GlobalEnv)

    #print(paste("Nombre d'especes significatives size :", nbEspecesTaillesSignificatives_Size))
    assign("listeEspecesSignificatives_Size",listeEspecesSignificatives_Size,envir=.GlobalEnv)
    assign("listeEspecesNonSignificatives_Size",listeEspecesNonSignificatives_Size,envir=.GlobalEnv)
    assign("listeEspeces_Size_ANOVA_impossible",listeEspeces_Size_ANOVA_impossible,envir=.GlobalEnv)        
} # fin Anova_Indicateurs_Population_3grp

################################################################################
## Nom     : Anova_Indicateurs_Population_TS.f
## Objet   :
## Input   :
## Output  :
################################################################################

Anova_Indicateurs_Population_TS.f = function(capt, size, titre) {

# log(N)
    Abundata <- vector("list",length(listeEspecesCommunesCapt))
    names(Abundata) <- listeEspecesCommunesCapt
    p.abund <- vector("list",length(listeEspecesCommunesCapt))
    names(p.abund) <- listeEspecesCommunesCapt
    listeEspeces_Nb_ANOVA_impossible <- NA
    nbEspeces_Nb_ANOVA_impossible <- 0       
    for (i in 1:length(listeEspecesCommunesCapt)) {
        id=is.element(capt$Espece, listeEspecesCommunesCapt[i])
        ind.trait_temp <- tapply(capt[id,"Nombre"], capt[id,"Trait"],sum)
        ind.trait = as.data.frame(matrix(NA,dim(ind.trait_temp)[1],2))
        colnames(ind.trait) = c("Trait","Individus")
        ind.trait$Trait = rep(dimnames(ind.trait_temp)[[1]])
        ind.trait$Individus = as.vector(ind.trait_temp,"numeric")
        ind.trait$Annee <- trait$Annee[match(ind.trait$Trait,trait$Trait)]
        Abundata[[i]] <- lm(log(ind.trait_temp) ~ ind.trait$Annee)[[1]][[2]]
        p.abund[[i]] <- summary(lm(log(ind.trait_temp) ~ ind.trait$Annee))[[4]][8]  
    }
    assign("Abundata",Abundata,envir=.GlobalEnv)

    # affichage des especes pour lesquelles il y a des differences significatives pour ln
    listeEspecesSignificatives_ln <- NA
    nbEspecesSignificatives_ln <- 0
    for (i in 1:length(p.abund)) {
        #print(p.abund[i][(p.abund[[i]][1]<0.05)])
        if ( (!is.null(p.abund[[i]][1])) && (p.abund[[i]][1]<0.05)) {
           nbEspecesSignificatives_ln <- nbEspecesSignificatives_ln + 1
           listeEspecesSignificatives_ln[nbEspecesSignificatives_ln] <- names(p.abund[i])
        }
    }
    listeEspecesNonSignificatives_ln <- subset(listeEspecesCommunesCapt, ! listeEspecesCommunesCapt %in% listeEspecesSignificatives_ln)

    # liste des especes pour lesquelles il y a des differences significatives
    MoySignT1T2 <- data.frame(c(0,0),row.names=c("T1","T2"))
    if (!is.na(listeEspecesSignificatives_ln) && length(listeEspecesSignificatives_ln)> 0) {
       #x11(width=18,height=18,pointsize=12)
       #par(mfrow = c(ceiling(length(listeEspecesSignificatives_ln)/2), 2),oma = c(0, 0, 3, 0),mex = 0.7)
       for (i in 1:length(listeEspecesSignificatives_ln)) {
       #    plotmeans(log(capt[capt$Espece==listeEspecesSignificatives_ln[i],"Nombre"]) ~ as.factor(capt[capt$Espece==listeEspecesSignificatives_ln[i],facteur]),main=paste(listeEspecesSignificatives_ln[i]), xlab="facteur", ylab="log(n)")
           MoySignT1T2[1:2,i] <- tapply(log(capt[capt$Espece==listeEspecesSignificatives_ln[i],"Nombre"]),as.factor(capt[capt$Espece==listeEspecesSignificatives_ln[i],facteur]),mean)
       }
       #mtext(titre, side = 3, outer = TRUE, cex = 1.5)
       #savePlot(filename = paste("listeEspecesSignificatives_log(n)_",titre,sep=""), "png")       
    }

    names(MoySignT1T2) <- listeEspecesSignificatives_ln
    MoySignT1T2
    assign("MoySignT1T2",MoySignT1T2,envir=.GlobalEnv)

    #print(paste("Nombre d'especes significatives pour leurs abondances :", nbEspecesSignificatives_ln))
    assign("listeEspecesSignificatives_ln",listeEspecesSignificatives_ln,envir=.GlobalEnv)
    assign("listeEspecesNonSignificatives_ln",listeEspecesNonSignificatives_ln,envir=.GlobalEnv)
    assign("listeEspeces_Nb_ANOVA_impossible",listeEspeces_Nb_ANOVA_impossible,envir=.GlobalEnv)       

# Taille moyenne
    Sizedata <- vector("list",length(listeEspecesCommunesSize))
    names(Sizedata) <- listeEspecesCommunesSize
    p.size <- vector("list",length(listeEspecesCommunesSize))
    names(p.size) <- listeEspecesCommunesSize
    listeEspeces_Size_ANOVA_impossible <- NA
    nbEspeces_Size_ANOVA_impossible <- 0        
    for (i in 1:length(listeEspecesCommunesSize)) {
        id=is.element(size$Espece, listeEspecesCommunesSize[i])
        ind.trait_temp <- tapply(size[id,"Longueur"], size[id,"Trait"],sum)
        ind.trait = as.data.frame(matrix(NA,dim(ind.trait_temp)[1],2))
        colnames(ind.trait) = c("Trait","Longueur")
        ind.trait$Trait = rep(dimnames(ind.trait_temp)[[1]])
        ind.trait$Longueur = as.vector(ind.trait_temp,"numeric")
        ind.trait$Annee <- trait$Annee[match(ind.trait$Trait,trait$Trait)]
        Sizedata[[i]] <- lm(log(ind.trait_temp) ~ ind.trait$Annee)[[1]][[2]]
        p.size[[i]] <- summary(lm(log(ind.trait_temp) ~ ind.trait$Annee))[[4]][8]  
    }
    assign("Sizedata",Sizedata,envir=.GlobalEnv)

    # affichage des especes pour lesquelles il y a des differences significatives
    listeEspecesSignificatives_Size <- NA
    nbEspecesTaillesSignificatives_Size <- 0
    for (i in 1:length(p.size)) {
        #print(p.size[i][(p.size[[i]][1]<0.05)])
        if ( (!is.null(p.size[[i]][1])) && (p.size[[i]][1]<0.05)) {
           nbEspecesTaillesSignificatives_Size <- nbEspecesTaillesSignificatives_Size + 1
           listeEspecesSignificatives_Size[nbEspecesTaillesSignificatives_Size] <- names(p.size[i])
        }
    }
    listeEspecesNonSignificatives_Size <- subset(listeEspecesCommunesSize, ! listeEspecesCommunesSize %in% listeEspecesSignificatives_Size)

    SizeSignT1T2 <- data.frame(c(0,0), row.names=c("T1","T2"))
    if (!is.na(listeEspecesSignificatives_Size) && length(listeEspecesSignificatives_Size)> 0) {
        #x11(width=18,height=18,pointsize=12)
        #par(mfrow = c(ceiling(length(listeEspecesSignificatives_Size)/2), 2),oma = c(0, 0, 3, 0),mex = 0.7)
        # liste des especes pour lesquelles il y a des differences de tailles significatives
        for (i in 1:length(listeEspecesSignificatives_Size)) {
        #    plotmeans(log(size[size$Espece==listeEspecesSignificatives_Size[i],"Longueur"]) ~ as.factor(size[size$Espece==listeEspecesSignificatives_Size[i],facteur2]),main=paste(listeEspecesSignificatives_Size[i]), xlab="facteur", ylab="log(taille)")
            SizeSignT1T2[1:2,i] <- tapply(log(size[size$Espece==listeEspecesSignificatives_Size[i],"Longueur"]),as.factor(size2[size2$Espece==listeEspecesSignificatives_Size[i],facteur2]),mean)
        }
        #mtext(titre, side = 3, outer = TRUE, cex = 1.5)
        #savePlot(filename = paste("listeEspecesSignificatives_Size_",titre,sep=""), "png")
    }

    names(SizeSignT1T2) <- listeEspecesSignificatives_Size
    SizeSignT1T2
    assign("SizeSignT1T2",SizeSignT1T2,envir=.GlobalEnv)

    #print(paste("Nombre d'especes significatives pour leurs tailles :", nbEspecesTaillesSignificatives_Size))
    assign("listeEspecesSignificatives_Size",listeEspecesSignificatives_Size,envir=.GlobalEnv)
    assign("listeEspecesNonSignificatives_Size",listeEspecesNonSignificatives_Size,envir=.GlobalEnv)
    assign("listeEspeces_Size_ANOVA_impossible",listeEspeces_Size_ANOVA_impossible,envir=.GlobalEnv)   
} # fin Anova_Indicateurs_Population_TS
