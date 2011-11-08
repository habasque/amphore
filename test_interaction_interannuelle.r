################################################################################
## Nom     : test_difference_interannuelle.f
## Objet   : 
## Input   :
## Output  :
################################################################################

#####################################################################
## Test de difference inter-annuelle pour pooler les anneees
##    dans le calcul des indicateurs
#####################################################################

test_difference_interannuelle.f = function(captures) {
  lm_diff_annuelle <- anova(lm(log(captures$Nombre) ~ as.factor(captures$Annee)))  
  texte_interaction <- "Aucune diff�rence annuelle d�tect�e"
  if (lm_diff_annuelle[[5]][1] <0.05) {
      texte_interaction <- "Avertissement. Diff�rences annuelles d�tect�es, analysez separ�ment les ann�es" 
  }
   if (ANALYSE_EN_COURS == "O") {  
     tkinsert(txt_annees,"end",texte_interaction)
   }
  assign("lm_diff_annuelle",lm_diff_annuelle,envir=.GlobalEnv)
}

#####################################################################
## Test de variation mensuelle pour pooler les anneees
##    dans le calcul des indicateurs
#####################################################################

test_difference_mois.f = function(captures) {
  lm_diff_mensuelle<- anova(lm(log(captures$Nombre) ~ as.factor(captures$Mois)))
  texte_interaction_mois <- "Aucune diff�rence mensuelle d�tect�e"
  if (lm_diff_mensuelle[[5]][1] <0.05) {
      texte_interaction_mois <- "Avertissement. Diff�rences mensuelles d�tect�es, vous devriez combiner les mois en fonction des saisons ou les analyser s�par�ment" 
  }
  if (ANALYSE_EN_COURS == "O") {
     tkinsert(txt_mois,"end",texte_interaction_mois)
  }
  assign("lm_diff_mensuelle",lm_diff_mensuelle,envir=.GlobalEnv)  
}


