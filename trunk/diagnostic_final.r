################################################################################
## Nom     : Diagnostic_Final_Temporel.f
## Objet   : analyse des diagnostics population et communaute
##           test binomial, diagramme de Venn
## Input   :
## Output  :
################################################################################

Diagnostic_Final_Temporel.f = function(ScenInd, titre) {

# Diagnostic population
if (length(GROUPE_ANNEES_3[!is.na(GROUPE_ANNEES_3)])) { #comparaison T1 vs T2 vs T3
   # Abondances
# T1<T2=T3 ou T1<T2>T3 ou T1=T2<T3 
   EspAbondanceT2gtT1 <- union(union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0))]), names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))])),names(MoySignT1T2T3[which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0)]))
# T1=T2>T3 ou T1>T2>T3 ou T1>T2=T3   
   EspAbondanceT2ltT1 <- union(union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]), names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))])),names(MoySignT1T2T3[which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0)]))
# T1<T2>T3 ou T1>T2<T3 ou T3=T2=T1 (non significatif)
   EspAbondanceT2eqT1 <- union(union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]), names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))])),listeEspecesNonSignificatives_ln)
   
   # Longueurs
# T1<T2=T3 ou T1<T2>T3 ou T1=T2<T3 
   EspLongT2gtT1 <- union(union(names(SizeSignT1T2T3[(which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]>0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]==0))]), names(SizeSignT1T2T3[(which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]>0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]>0))])),names(SizeSignT1T2T3[which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]==0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]>0)]))
# T1=T2>T3 ou T1>T2>T3 ou T1>T2=T3   
   EspLongT2ltT1 <- union(union(names(SizeSignT1T2T3[(which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]==0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]<0))]), names(SizeSignT1T2T3[(which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]<0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]<0))])),names(SizeSignT1T2T3[which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]<0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]==0)]))
# T1<T2>T3 ou T1>T2<T3 ou T3=T2=T1 (non significatif)
   EspLongT2eqT1 <- union(union(names(SizeSignT1T2T3[(which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]>0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]<0))]), names(SizeSignT1T2T3[(which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]<0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]>0))])),listeEspecesNonSignificatives_Size)

} else {
   # Abondances
   EspAbondanceT2gtT1 <- names(MoySignT1T2[which(MoySignT1T2[2,]-MoySignT1T2[1,]>0)]) # T2>T1
   EspAbondanceT2ltT1 <- names(MoySignT1T2[which(MoySignT1T2[2,]-MoySignT1T2[1,]<0)]) # T2<T1
   EspAbondanceT2eqT1 <- listeEspecesNonSignificatives_ln # T2=T1 (non significatif)
   # Longueurs
   EspLongT2gtT1 <- names(SizeSignT1T2[which(SizeSignT1T2[2,]-SizeSignT1T2[1,]>0)]) # T2>T1
   EspLongT2ltT1 <- names(SizeSignT1T2[which(SizeSignT1T2[2,]-SizeSignT1T2[1,]<0)]) # # T2<T1
   EspLongT2eqT1 <- listeEspecesNonSignificatives_Size # T2=T1 (non significatif)
}
   assign("EspAbondanceT2gtT1",EspAbondanceT2gtT1,envir=.GlobalEnv)
   assign("EspAbondanceT2ltT1",EspAbondanceT2ltT1,envir=.GlobalEnv)
   assign("EspAbondanceT2eqT1",EspAbondanceT2eqT1,envir=.GlobalEnv)
   assign("EspLongT2gtT1",EspLongT2gtT1,envir=.GlobalEnv)
   assign("EspLongT2ltT1",EspLongT2ltT1,envir=.GlobalEnv)
   assign("EspLongT2eqT1",EspLongT2eqT1,envir=.GlobalEnv)

   # Graphiques de Venn
   Ae <-c(EspAbondanceT2ltT1,EspAbondanceT2eqT1)
   Ai <-c(EspAbondanceT2gtT1,EspAbondanceT2eqT1)
   Le <-c(EspLongT2ltT1,EspLongT2eqT1)
   Li <-c(EspLongT2gtT1,EspLongT2eqT1)
   #venn(list(Ae,Ai,Le,Li))
   #savePlot(filename = paste("Venn_populations",titre), "png")
   vl <- venn(list(Ae,Ai,Le,Li), show.plot=F)
   vl.df <- as.data.frame(as.matrix(vl)[1:dim(vl)[1],1:dim(vl)[2]])
   # Tableau croise
   MatPop <- matrix(data=vl.df[c(6,14,10,8,16,12,7,15,11),1], nrow=3, ncol=3, dimnames=list(c("Ab2>Ab1","Ab2=Ab1","Ab2<Ab1"), c("Lm2>Lm1","Lm2=Lm1","Lm2<Lm1")))
   assign("MatPop",MatPop,envir=.GlobalEnv)
   write.table(MatPop,paste("Tableau_croise",titre,".txt"))
   # Probabilites, test binomial
   CasPossibles <- sum(MatPop)
   CasFavorables <- sum(MatPop[,1],MatPop[1,2])
   Proba <- sum(probabilites[1,2:4]) + sum(probabilites[2:3,2])
   TestBinomial <- binom.test(CasFavorables,CasPossibles,Proba)  
   assign("CasFavorables",CasFavorables,envir=.GlobalEnv)   
   assign("CasPossibles",CasPossibles,envir=.GlobalEnv)   
   assign("TestBinomial",TestBinomial,envir=.GlobalEnv)   
   dput(TestBinomial,paste("TestBinomial",titre,".txt"), control=NULL)
   
   effet_population <- FALSE
   if (TestBinomial$p.value < 0.05) { 
      effet_population <- TRUE
   } else {
      print(paste("Effet non significatif car p-value du test binomial : ", TestBinomial$p.value))
   }

   #liste des especes qui beneficient d'un effet AMP
   listeEspecesBenefAMP <- union(EspAbondanceT2gtT1,EspLongT2gtT1)
   print(paste("L'AMP semble bénéficier aux espèces suivantes : ", toString(listeEspecesBenefAMP)))
   assign("listeEspecesBenefAMP",listeEspecesBenefAMP,envir=.GlobalEnv)   
   
# Lecture du scenario associe au diagnostic communaute

# ScPech_S ; ScCons_S ;
   ScPech_T <- Scenario[Scenario$Approche=="T" & Scenario$Objectif=="P",]
   ScCons_T <- Scenario[Scenario$Approche=="T" & Scenario$Objectif=="C",]

   effet_communaute <- "S0"
   if (OBJECTIF_ETUDIE == "P") {
       scenar_Peche_T <- NULL
       id_scen <- is.element(ScPech_T$CodeScen,ScenInd[rownames(ScenInd)=="ScenPeche_T",])
       if (summary(id_scen)[3]>=1) {
          scenar_Peche_T <- cbind(ScPech_T[id_scen,4], paste(ScPech_T[id_scen,3]),paste(ScPech_T[id_scen,5], sep=""))
          effet_communaute <- ScPech_T[id_scen,3]          
       }   
       if (summary(id_scen)[3]<1) {
          scenar_Peche_T <- paste("Pas d'effet AMP sur la Peche")
       }
       print(toString(scenar_Peche_T))
       assign("scenar_Peche_T",scenar_Peche_T,envir=.GlobalEnv)
       score_global.f(effet_population,effet_communaute)
   } else {
       scenar_Cons_T <- NULL
       id_scen <- is.element(ScCons_T$CodeScen,ScenInd[rownames(ScenInd)=="ScenCons_T",])
       if (summary(id_scen)[3]>=1) {
          scenar_Cons_T <- cbind(ScCons_T[id_scen,4], paste(ScCons_T[id_scen,3]),paste(ScCons_T[id_scen,5], sep=""))
          effet_communaute <- ScCons_T[id_scen,3]          
       }
       if (summary(id_scen)[3]<1) {
          scenar_Cons_T <- paste("Pas d'effet AMP sur la Conservation")
       }
       print(toString(scenar_Cons_T))
       assign("scenar_Cons_T",scenar_Cons_T,envir=.GlobalEnv)       
       score_global.f(effet_population,effet_communaute)       
   }
   print("Diagnostic terminé")
} # fin Diagnostic_Final_Temporel

################################################################################
## Nom     : Diagnostic_Final_Spatial.f
## Objet   :
## Input   :
## Output  :
################################################################################

Diagnostic_Final_Spatial.f = function(ScenInd, titre) {

   # Abondances
   EspAbondanceIgtE <- names(MoySignT1T2[which(MoySignT1T2[1,]-MoySignT1T2[2,]>0)]) # I>E
   EspAbondanceIltE <- names(MoySignT1T2[which(MoySignT1T2[1,]-MoySignT1T2[2,]<0)]) # I<E
   EspAbondanceIeqE <- listeEspecesNonSignificatives_ln # I=E (non significatif)
   # Longueurs
   EspLongueurIgtE <- names(SizeSignT1T2[which(SizeSignT1T2[1,]-SizeSignT1T2[2,]>0)]) # I>E
   EspLongueurIltE <- names(SizeSignT1T2[which(SizeSignT1T2[1,]-SizeSignT1T2[2,]<0)]) # I<E
   EspLongueurIeqE <- listeEspecesNonSignificatives_Size # I=E (non significatif)

   assign("EspAbondanceIgtE",EspAbondanceIgtE,envir=.GlobalEnv)
   assign("EspAbondanceIltE",EspAbondanceIltE,envir=.GlobalEnv)
   assign("EspAbondanceIeqE",EspAbondanceIeqE,envir=.GlobalEnv)
   assign("EspLongueurIgtE",EspLongueurIgtE,envir=.GlobalEnv)
   assign("EspLongueurIltE",EspLongueurIltE,envir=.GlobalEnv)
   assign("EspLongueurIeqE",EspLongueurIeqE,envir=.GlobalEnv)
      
   # Graphiques de Venn
   Ae <-c(EspAbondanceIltE,EspAbondanceIeqE)
   Ai <-c(EspAbondanceIgtE,EspAbondanceIeqE)
   Le <-c(EspLongueurIltE,EspLongueurIeqE)
   Li <-c(EspLongueurIgtE,EspLongueurIeqE)
   #venn(list(Ae,Ai,Le,Li))
   #savePlot(filename = paste("Venn_populations",titre), "jpeg")
   vl <- venn(list(Ae,Ai,Le,Li), show.plot=F)
   vl.df <- as.data.frame(as.matrix(vl)[1:dim(vl)[1],1:dim(vl)[2]])
   # Tableau croise
   MatPop <- matrix(data=vl.df[c(6,14,10,8,16,12,7,15,11),1], nrow=3, ncol=3, dimnames=list(c("Ab2>Ab1","Ab2=Ab1","Ab2<Ab1"), c("Lm2>Lm1","Lm2=Lm1","Lm2<Lm1")))
   MatPop
   assign("MatPop",MatPop,envir=.GlobalEnv)
   write.table(MatPop,paste("Tableau_croise",titre,".txt"))
   # Probabilites, test binomial
   CasPossibles <- sum(MatPop)
   CasFavorables <- sum(MatPop[,1],MatPop[1,2])
   Proba <- sum(probabilites[1,2:4]) + sum(probabilites[2:3,2])   
   TestBinomial <- binom.test(CasFavorables,CasPossibles,Proba)  
   assign("CasFavorables",CasFavorables,envir=.GlobalEnv)   
   assign("CasPossibles",CasPossibles,envir=.GlobalEnv)   
   assign("TestBinomial",TestBinomial,envir=.GlobalEnv)     
   dput(TestBinomial,paste("TestBinomial",titre,".txt"), control=NULL)
   
   effet_population <- FALSE
   if (TestBinomial$p.value < 0.05) { 
      effet_population <- TRUE
   } else {
      print(paste("Effet non significatif car p-value du test binomial : ", TestBinomial$p.value))
   }

   #liste des especes qui beneficient d'un effet AMP
   listeEspecesBenefAMP <- union(EspAbondanceIgtE,EspLongueurIgtE)
   print(paste("L'AMP semble bénéficier aux espèces suivantes : ", toString(listeEspecesBenefAMP)))
   assign("listeEspecesBenefAMP",listeEspecesBenefAMP,envir=.GlobalEnv)   
   
# Lecture des scenarios associes au diagnostic communaute

# ScPech_S ; ScCons_S ;
   ScPech_S <- Scenario[Scenario$Approche=="S" & Scenario$Objectif=="P",]
   ScCons_S <- Scenario[Scenario$Approche=="S" & Scenario$Objectif=="C",]

   effet_communaute <- "S0"
   if (OBJECTIF_ETUDIE == "P") {
      scenar_Peche_S <- NULL
      id_scen <- is.element(ScPech_S$CodeScen,ScenInd[rownames(ScenInd)=="ScenPeche_S",])
      if (summary(id_scen)[3]>=1) {
         scenar_Peche_S <- cbind(ScPech_S[id_scen,4], paste(ScPech_S[id_scen,3]),paste(ScPech_T[id_scen,5], sep=""))
         effet_communaute <- ScPech_S[id_scen,3]                   
      }
      if (summary(id_scen)[3]<1) {
         scenar_Peche_S <- paste("Pas d'effet AMP sur la Peche")
      }
      print(toString(scenar_Peche_S))
      assign("scenar_Peche_S",scenar_Peche_S,envir=.GlobalEnv)         
      score_global.f(effet_population,effet_communaute)      
   } else {
      scenar_Cons_S <- NULL
      id_scen <- is.element(ScCons_S$CodeScen,ScenInd[rownames(ScenInd)=="ScenCons_S",])
      if (summary(id_scen)[3]>=1) {
         scenar_Cons_S <- cbind(ScCons_S[id_scen,4], paste(ScCons_S[id_scen,3]),paste(ScCons_T[id_scen,5], sep=""))
         effet_communaute <- ScCons_S[id_scen,3]                            
      }         
      if (summary(id_scen)[3]<1) {
         scenar_Cons_S <- paste("Pas d'effet AMP sur la Conservation")
      }
      print(toString(scenar_Cons_S))
      assign("scenar_Cons_S",scenar_Cons_S,envir=.GlobalEnv)   
      score_global.f(effet_population,effet_communaute)            
   }
   print("Diagnostic terminé") 
}# fin Diagnostic_Final_Spatial

################################################################################
## Nom     : Diagnostic_Final_Gradient.f
## Objet   :
## Input   :
## Output  :
################################################################################

Diagnostic_Final_Gradient.f = function(ScenInd, titre) {

   # Abondances
# Z1<Z2=Z3 ou Z1<Z2>Z3 ou Z1=Z2<Z3 
   EspAbondanceIgtE <- union(union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0))]), names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))])),names(MoySignT1T2T3[which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0)]))
# Z1=Z2>Z3 ou Z1>Z2>Z3 ou Z1>Z2=Z3   
   EspAbondanceIltE <- union(union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]==0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]), names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))])),names(MoySignT1T2T3[which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]==0)]))
# Z1<Z2>Z3 ou Z1>Z2<Z3 ou Z3=Z2=Z1 (non significatif)
   EspAbondanceIeqE <- union(union(names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]>0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]<0))]), names(MoySignT1T2T3[(which(MoySignT1T2T3[2,]-MoySignT1T2T3[1,]<0 & MoySignT1T2T3[3,]-MoySignT1T2T3[2,]>0))])),listeEspecesNonSignificatives_ln)
   
   # Longueurs
# Z1<Z2=Z3 ou Z1<Z2>Z3 ou Z1=Z2<Z3 
   EspLongueurIgtE <- union(union(names(SizeSignT1T2T3[(which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]>0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]==0))]), names(SizeSignT1T2T3[(which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]>0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]>0))])),names(SizeSignT1T2T3[which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]==0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]>0)]))
# Z1=Z2>Z3 ou Z1>Z2>Z3 ou Z1>Z2=Z3    
   EspLongueurIltE <- union(union(names(SizeSignT1T2T3[(which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]==0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]<0))]), names(SizeSignT1T2T3[(which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]<0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]<0))])),names(SizeSignT1T2T3[which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]<0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]==0)]))
# Z1<Z2>Z3 ou Z1>Z2<Z3 ou Z3=Z2=Z1 (non significatif)
   EspLongueurIeqE <- union(union(names(SizeSignT1T2T3[(which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]>0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]<0))]), names(SizeSignT1T2T3[(which(SizeSignT1T2T3[2,]-SizeSignT1T2T3[1,]<0 & SizeSignT1T2T3[3,]-SizeSignT1T2T3[2,]>0))])),listeEspecesNonSignificatives_Size)
 
   assign("EspAbondanceIgtE",EspAbondanceIgtE,envir=.GlobalEnv)
   assign("EspAbondanceIltE",EspAbondanceIltE,envir=.GlobalEnv)
   assign("EspAbondanceIeqE",EspAbondanceIeqE,envir=.GlobalEnv)
   assign("EspLongueurIgtE",EspLongueurIgtE,envir=.GlobalEnv)
   assign("EspLongueurIltE",EspLongueurIltE,envir=.GlobalEnv)
   assign("EspLongueurIeqE",EspLongueurIeqE,envir=.GlobalEnv)
      
   # Graphiques de Venn
   Ae <-c(EspAbondanceIltE,EspAbondanceIeqE)
   Ai <-c(EspAbondanceIgtE,EspAbondanceIeqE)
   Le <-c(EspLongueurIltE,EspLongueurIeqE)
   Li <-c(EspLongueurIgtE,EspLongueurIeqE)
   #venn(list(Ae,Ai,Le,Li))
   #savePlot(filename = paste("Venn_populations",titre), "jpeg")
   vl <- venn(list(Ae,Ai,Le,Li), show.plot=F)
   vl.df <- as.data.frame(as.matrix(vl)[1:dim(vl)[1],1:dim(vl)[2]])
   # Tableau croise
   MatPop <- matrix(data=vl.df[c(6,14,10,8,16,12,7,15,11),1], nrow=3, ncol=3, dimnames=list(c("Ab2>Ab1","Ab2=Ab1","Ab2<Ab1"), c("Lm2>Lm1","Lm2=Lm1","Lm2<Lm1")))
   MatPop
   assign("MatPop",MatPop,envir=.GlobalEnv)
   write.table(MatPop,paste("Tableau_croise",titre,".txt"))
   # Probabilites, test binomial
   CasPossibles <- sum(MatPop)
   CasFavorables <- sum(MatPop[,1],MatPop[1,2])
   Proba <- sum(probabilites[1,2:4]) + sum(probabilites[2:3,2])   
   TestBinomial <- binom.test(CasFavorables,CasPossibles,Proba)  
   assign("CasFavorables",CasFavorables,envir=.GlobalEnv)   
   assign("CasPossibles",CasPossibles,envir=.GlobalEnv)   
   assign("TestBinomial",TestBinomial,envir=.GlobalEnv)     
   dput(TestBinomial,paste("TestBinomial",titre,".txt"), control=NULL)
   
   effet_population <- FALSE
   if (TestBinomial$p.value < 0.05) { 
      effet_population <- TRUE
   } else {
      print(paste("Effet non significatif car p-value du test binomial : ", TestBinomial$p.value))
   }

   #liste des especes qui beneficient d'un effet AMP
   listeEspecesBenefAMP <- union(EspAbondanceIgtE,EspLongueurIgtE)
   print(paste("L'AMP semble bénéficier aux espèces suivantes : ", toString(listeEspecesBenefAMP)))
   assign("listeEspecesBenefAMP",listeEspecesBenefAMP,envir=.GlobalEnv)   
   
# Lecture des scenarios associes au diagnostic communaute

# ScPech_S ; ScCons_S ;
   ScPech_S <- Scenario[Scenario$Approche=="S" & Scenario$Objectif=="P",]
   ScCons_S <- Scenario[Scenario$Approche=="S" & Scenario$Objectif=="C",]

   effet_communaute <- "S0"
   if (OBJECTIF_ETUDIE == "P") {
      scenar_Peche_S <- NULL
      id_scen <- is.element(ScPech_S$CodeScen,ScenInd[rownames(ScenInd)=="ScenPeche_S",])
      if (summary(id_scen)[3]>=1) {
         scenar_Peche_S <- cbind(ScPech_S[id_scen,4], paste(ScPech_S[id_scen,3]),paste(ScPech_T[id_scen,5], sep=""))
         effet_communaute <- ScPech_S[id_scen,3]                   
      }
      if (summary(id_scen)[3]<1) {
         scenar_Peche_S <- paste("Pas d'effet AMP sur la Peche")
      }
      print(toString(scenar_Peche_S))
      assign("scenar_Peche_S",scenar_Peche_S,envir=.GlobalEnv)         
      score_global.f(effet_population,effet_communaute)      
   } else {
      scenar_Cons_S <- NULL
      id_scen <- is.element(ScCons_S$CodeScen,ScenInd[rownames(ScenInd)=="ScenCons_S",])
      if (summary(id_scen)[3]>=1) {
         scenar_Cons_S <- cbind(ScCons_S[id_scen,4], paste(ScCons_S[id_scen,3]),paste(ScCons_T[id_scen,5], sep=""))
         effet_communaute <- ScCons_S[id_scen,3]                            
      }         
      if (summary(id_scen)[3]<1) {
         scenar_Cons_S <- paste("Pas d'effet AMP sur la Conservation")
      }
      print(toString(scenar_Cons_S))
      assign("scenar_Cons_S",scenar_Cons_S,envir=.GlobalEnv)   
      score_global.f(effet_population,effet_communaute)            
   }
   print("Diagnostic terminé") 
}# fin Diagnostic_Final_Gradient