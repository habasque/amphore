
################################################################################
## Nom     : choix_Annee_Mise_En_Reserve.f
## Objet   : l'utilisateur choisit l'année de mise en réserve de l'AMP
## Input   :
## Output  :
################################################################################

choix_Annee_Mise_En_Reserve.f = function() {
      aa<-tktoplevel()
      tkwm.title(aa,"Annee de mise en reserve")
      #zone de saisie
      Year <- tclVar("    ")
      entry.Year <-tkentry(aa,width="6",textvariable=Year)
      tkgrid(tklabel(aa,text="Saisir l'annee de mise en reserve"))
      tkgrid(entry.Year)
      OnOK <- function(){
      	ANNEE_MISE_EN_RESERVE <- tclvalue(Year)
	      assign("ANNEE_MISE_EN_RESERVE",ANNEE_MISE_EN_RESERVE,envir=.GlobalEnv)
   	    print(ANNEE_MISE_EN_RESERVE)
	      tkdestroy(aa)
      }
      OK.but <-tkbutton(aa,text="   OK   ",command=OnOK)
      tkbind(entry.Year, "<Return>",OnOK)
      tkgrid(OK.but)
      tkwm.deiconify(aa)
      tkfocus(aa)
      tkwait.window(aa)
      tkdestroy(aa)
}
################################################################################
## Nom    : choixUnGroupeAnnees.f()
## Objet  : choix d'un groupe d'années
## Input  :
## Output :
################################################################################

choixUnGroupeAnnees.f = function() {
# selection du premier groupe d'annees
      aa<-tktoplevel()
      tkwm.title(aa,"Selection du premier groupe d'annees")
      scr <- tkscrollbar(aa, repeatinterval=5)
      tl<-tklistbox(aa,height=10,width=30,selectmode="extended",background="white")    
      txt_annees <- tktext(aa,bg="#FFFFCC",height=10,width=50) 
      assign("txt_annees",txt_annees,envir=.GlobalEnv)      
      test_difference_interannuelle.f(capt_orig)      
      tkgrid(tklabel(aa,text="Liste des annees"))
      tkgrid(tl,scr,txt_annees)
      tkgrid.configure(scr,rowspan=4,sticky="nsw")
      anneesCaptures <- unique(capt_orig$Annee)
      anneesTaille <- unique(size_orig$Annee)
      anneesCommunesCaptSize <- subset(anneesCaptures, anneesCaptures %in% anneesTaille)
      assign("anneesCommunesCaptSize",anneesCommunesCaptSize,envir=.GlobalEnv)    
      annees <- sort(anneesCommunesCaptSize)
      a = length(annees)
      for (i in (1:a)){
          tkinsert(tl,"end",annees[i])
      }
      tkselection.set(tl,0)

      OnOK <- function(){
          anneesSelectionnees = as.character(tkcurselection(tl))
          GROUPE_ANNEES_1 <- NA
          for (k in (1:length(anneesSelectionnees))){
              GROUPE_ANNEES_1[k] <- annees[as.numeric(anneesSelectionnees[k])+1]
          }
          assign("GROUPE_ANNEES_1",GROUPE_ANNEES_1,envir=.GlobalEnv)
          ANNEE_DEBUT <- min(GROUPE_ANNEES_1)
          ANNEE_FIN <- max(GROUPE_ANNEES_1)          
          assign("ANNEE_DEBUT",ANNEE_DEBUT,envir=.GlobalEnv)
          assign("ANNEE_FIN",ANNEE_FIN,envir=.GlobalEnv)          
          tkdestroy(aa)
      }
      OK.but <-tkbutton(aa,text="OK",command=OnOK)
      tkgrid(OK.but)
      tkwm.deiconify(aa)
      tkfocus(aa)
      tkwait.window(aa)
      tkdestroy(aa)
}
################################################################################
## Nom    : choixDeuxGroupesAnnees.f()
## Objet  : choix des 2 groupes d'annees a comparer
## Input  :
## Output :
################################################################################

choixDeuxGroupesAnnees.f = function() {
# selection du premier groupe d'annees
      choixUnGroupeAnnees.f()
      #test si le nombre d'annees selectionnes n'est pas egal au nombre d'annees de la serie
      if (length(unique(trait$Annee)) == length(GROUPE_ANNEES_1)) {
         tkmessageBox(message="Vous ne pouvez pas selectionner l'ensemble des annees",icon="warning",type="ok")   
         choixUnGroupeAnnees.f()         
      }

## selection du deuxieme groupe d'annees
      aa<-tktoplevel()
      tkwm.title(aa,"Selection du second groupe d'annees")
      scr <- tkscrollbar(aa, repeatinterval=5, command=function(...)tkyview(tl,...))
      tl<-tklistbox(aa,height=10,width=30,selectmode="extended",background="white")    
      txt_annees <- tktext(aa,bg="#FFFFCC",height=10,width=50) 
      assign("txt_annees",txt_annees,envir=.GlobalEnv)      
      test_difference_interannuelle.f(capt_orig)      
      tkgrid(tklabel(aa,text="Liste des annees"))
      tkgrid(tl,scr,txt_annees)
      tkgrid.configure(scr,rowspan=4,sticky="nsw")
      #on supprime les annees deja selectionnees dans le 1er groupe de la liste des annees
      annees_restantes <- subset(anneesCommunesCaptSize, ! anneesCommunesCaptSize %in% GROUPE_ANNEES_1)
      annees_restantes <- sort(unique(annees_restantes))
      a = length(annees_restantes)
      for (i in (1:a)){
          tkinsert(tl,"end",annees_restantes[i])
      }
      tkselection.set(tl,0)

      OnOK <- function(){
          anneesSelectionnees = as.character(tkcurselection(tl))
          GROUPE_ANNEES_2 <- NA
          for (k in (1:length(anneesSelectionnees))){
              GROUPE_ANNEES_2[k] <- annees_restantes[as.numeric(anneesSelectionnees[k])+1]
          }
          assign("GROUPE_ANNEES_2",GROUPE_ANNEES_2,envir=.GlobalEnv)
          tkdestroy(aa)
      }
      OK.but <-tkbutton(aa,text="OK",command=OnOK)
      tkgrid(OK.but)
      tkwm.deiconify(aa)
      tkfocus(aa)
      tkwait.window(aa)
      tkdestroy(aa)
} # fin choixDeuxGroupesAnnees.f

################################################################################
## Nom    : choixTroisGroupesAnnees.f()
## Objet  : choix des 3 groupes d'annees a comparer
## Input  :
## Output :
################################################################################

choixTroisGroupesAnnees.f = function() {
# selection du premier groupe d'annees
      choixDeuxGroupesAnnees.f()

## selection du troisieme groupe d'annees
      aa<-tktoplevel()
      tkwm.title(aa,"Selection du troisieme groupe d'annees")
      scr <- tkscrollbar(aa, repeatinterval=5, command=function(...)tkyview(tl,...))
      tl<-tklistbox(aa,height=10,width=30,selectmode="extended",yscrollcommand=function(...)tkset(scr,...),background="white")
      #txt <- tktext(aa,bg="#FFFFCC",font="courier",height=10,width=30,yscrollcommand=function(...)tkset(scr,...))      
      tkgrid(tklabel(aa,text="Liste des annees"))
      tkgrid(tl,scr)
      tkgrid.configure(scr,rowspan=4,sticky="nsw")
      #on supprime les annees deja selectionnes dans le 2eme groupe de la liste des annees
      annees_restantes <- subset(anneesCommunesCaptSize, ! anneesCommunesCaptSize %in% c(GROUPE_ANNEES_1,GROUPE_ANNEES_2))
      annees_restantes <- sort(unique(annees_restantes))
      a = length(annees_restantes)
      for (i in (1:a)){
          tkinsert(tl,"end",annees_restantes[i])
      }
      tkselection.set(tl,0)

      OnOK <- function(){
          anneesSelectionnees = as.character(tkcurselection(tl))
          GROUPE_ANNEES_3 <- NA
          for (k in (1:length(anneesSelectionnees))){
              GROUPE_ANNEES_3[k] <- annees_restantes[as.numeric(anneesSelectionnees[k])+1]
          }
          assign("GROUPE_ANNEES_3",GROUPE_ANNEES_3,envir=.GlobalEnv)
          tkdestroy(aa)
      }
      OK.but <-tkbutton(aa,text="OK",command=OnOK)
      tkgrid(OK.but)
      tkwm.deiconify(aa)
      tkfocus(aa)
      tkwait.window(aa)
      tkdestroy(aa)
} # fin choixTroisGroupesAnnees.f

################################################################################
## Nom     : choix_Mois.f
## Objet   : permet à l'utilisateur de choisir les mois (saisons) qu'il souhaite étudier
## Input   :
## Output  :
################################################################################

choix_Mois.f = function(listeMois) {
      aa<-tktoplevel()
      tkwm.title(aa,"Liste des mois du jeu de donnees")
      scr <- tkscrollbar(aa, repeatinterval=5,command=function(...)tkyview(tl,...))
      tl<-tklistbox(aa,height=10,width=30,selectmode="extended",yscrollcommand=function(...)tkset(scr,...),background="white")
      txt_mois <- tktext(aa,bg="#FFFFCC",height=10,width=50,yscrollcommand=function(...)tkset(scr,...))      
      assign("txt_mois",txt_mois,envir=.GlobalEnv)
      test_difference_mois.f(capt_orig)      
      tkgrid(tklabel(aa,text="Liste des mois"))
      tkgrid(tl,scr,txt_mois)
      tkgrid.configure(scr,rowspan=4,sticky="nsw")
      mois <- sort(unique(listeMois))
      m = length(mois)
      for (i in (1:m)){
          tkinsert(tl,"end",mois[i])
      }
      tkselection.set(tl,0)
      OnOK <- function(){
          moisSelectionnes = as.character(tkcurselection(tl))
          MOIS_ETUDIES <- NA
          for (k in (1:length(moisSelectionnes))){
              MOIS_ETUDIES[k] <- mois[as.numeric(moisSelectionnes[k])+1]
          }
          assign("MOIS_ETUDIES",MOIS_ETUDIES,envir=.GlobalEnv)
          tkdestroy(aa)
      }
      OK.but <-tkbutton(aa,text="OK",command=OnOK)
      tkgrid(OK.but)
      tkwm.deiconify(aa)
      tkfocus(aa)
      tkwait.window(aa)
      tkdestroy(aa)
} #fin choix_Mois
