################################################################################
## Nom     : score_global.f
## Objet   : résumer les diagnostics en 5 classes
## Input   :
## Output  :
################################################################################


score_global.f = function(scenarioPopulation, scenarioCommunaute) {
score_final <- ""

if (scenarioPopulation == FALSE & scenarioCommunaute == "S0" ) {
    score_final <- "0"
}
if (scenarioPopulation == TRUE & scenarioCommunaute == "S0" ) {
    score_final <- "1"
}
if (scenarioPopulation == FALSE & scenarioCommunaute %in% c("S7","S8","S9") ) {
    score_final <- "2"
}
if (scenarioPopulation == FALSE & scenarioCommunaute %in% c("S1","S2","S3","S4","S5","S6") ) {
    score_final <- "3"
}
if (scenarioPopulation == TRUE & scenarioCommunaute %in% c("S1","S2","S3","S4","S5","S6","S7","S8","S9") ) {
    score_final <- "4"
}
  assign("score_final",score_final,envir=.GlobalEnv)
  print(paste("Score indicateurs biologiques : ",score_final,sep=""))
} #fin score global

