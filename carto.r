################################################################################
## Nom     : carto.f
## Objet   :
## Input   :
## Output  :
################################################################################

#library(maps)
#library(mapdata)
#library(maptools)
#data(world.cities)
#
## Définition des couleurs de fond de carte.
#CouleurDeLaMer <- rgb(217,235,255,maxColorValue=255)
#CouleurAutresPays <- rgb(235,217,183,maxColorValue=255)
#
#
#northarrow <- function(loc,size,bearing=0,ArrowType=1,cols,cex=1,...) {
#  # checking arguments
#  if(missing(loc)) stop("loc is missing")
#  if(missing(size)) stop("size is missing")
#  # default colors are white and black
#  if(missing(cols)) cols <- rep(c("white","black"),8)
#  # calculating coordinates of polygons
#  radii <- rep(size/c(1,4,2,4),4)
#  x <- radii[(0:15)+1]*cos((0:15)*pi/8+bearing)+loc[1]
#  y <- radii[(0:15)+1]*sin((0:15)*pi/8+bearing)+loc[2]
#
#  # drawing polygons
#  # Rose complète
#  if(ArrowType==1){
#    for (i in 1:15) {
#      x1 <- c(x[i],x[i+1],loc[1])
#      y1 <- c(y[i],y[i+1],loc[2])
#      polygon(x1,y1,col=cols[i])
#    }
#    # drawing the last polygon
#    polygon(c(x[16],x[1],loc[1]),c(y[16],y[1],loc[2]),col=cols[16],...)
#  }
#    
#  # Polygones du nord seuls
#  if(ArrowType==2){
#    for (i in 4:5) {
#        x1 <- c(x[i],x[i+1],loc[1])
#        y1 <- c(y[i],y[i+1],loc[2])
#        polygon(x1,y1,col=cols[i],...)
#      }
#  }
#
#  # Polygones du nord modifiés
#  if(ArrowType==3){
#    i <- 4
#    x1 <- c(x[i],x[i+1],loc[1])-loc[1]/2
#    y1 <- c(y[i],y[i+1],loc[2])
#    NewY <- y1[3]+2*(y1[1] - y1[3])
#    y1[3] <- NewY
#    polygon(x1,y1,col=cols[i],...)
#    i <- 5
#    x1 <- c(x[i],x[i+1],loc[1])-loc[1]/2
#    y1 <- c(y[i],y[i+1],loc[2])
#    y1[3] <- NewY
#    polygon(x1,y1,col=cols[i],...)
#  }
#
#
#  # drawing letters
##  b <- c("O","N","E","S")
#  b <- c("","N","","")
#  pos <- c(4,3,2,1)
##  for (i in 0:3) text((size+par("cxy")[1])*cos(bearing+i*pi/2)+loc[1],
##    (size+par("cxy")[2])*sin(bearing+i*pi/2)+loc[2],b[i+1],
##    cex=cex)
#  if(ArrowType==3) loc[1] <- loc[1]/2
#  for (i in 0:3) text((size)*cos(bearing+i*pi/2)+loc[1],
#    (size)*sin(bearing+i*pi/2)+loc[2],b[i+1],
#    cex=cex,
#    pos=pos[i+1])
#}
#
##choix du pays
#choix_region.f = function() {
#      aa<-tktoplevel()
#      tkwm.title(aa,"Liste des regions du monde")
#      scr <- tkscrollbar(aa, repeatinterval=5,command=function(...)tkyview(tl,...))
#      tl<-tklistbox(aa,height=10,width=30,selectmode="extended",yscrollcommand=function(...)tkset(scr,...),background="white")
#      tkgrid(tklabel(aa,text="Liste des regions"))
#      tkgrid(tl,scr)
#      tkgrid.configure(scr,rowspan=4,sticky="nsw")
#      regions <- sort(unique(world.cities$country.etc))
#      r = length(regions)
#      for (i in (1:r)){
#          tkinsert(tl,"end",regions[i])
#      }
#      tkselection.set(tl,0)
#      OnOK <- function(){
#          regionsSelectionnes = as.character(tkcurselection(tl))
#          REGIONS_ETUDIEES <- NA
#          for (k in (1:length(regionsSelectionnes))){
#              REGIONS_ETUDIEES[k] <- regions[as.numeric(regionsSelectionnes[k])+1]
#          }
#          assign("REGIONS_ETUDIEES",REGIONS_ETUDIEES,envir=.GlobalEnv)
#          tkdestroy(aa)
#      }
#      OK.but <-tkbutton(aa,text="OK",command=OnOK)
#      tkgrid(OK.but)
#      tkwm.deiconify(aa)
#      tkfocus(aa)
#      tkwait.window(aa)
#      tkdestroy(aa)
#}
#
#choix_AMP.f = function() {
#      aa<-tktoplevel()
#      tkwm.title(aa,"Liste des AMP du pays")
#      scr <- tkscrollbar(aa, repeatinterval=5,command=function(...)tkyview(tl,...))
#      tl<-tklistbox(aa,height=10,width=30,selectmode="extended",yscrollcommand=function(...)tkset(scr,...),background="white")
#      tkgrid(tklabel(aa,text="Liste des AMP"))
#      tkgrid(tl,scr)
#      tkgrid.configure(scr,rowspan=4,sticky="nsw")
#      liste_AMP <- as.character(sort(unique(liste_AMP_country$name)))
#      r = length(liste_AMP)
#      for (i in (1:r)){
#          tkinsert(tl,"end",liste_AMP[i])
#      }
#      tkselection.set(tl,0)
#      OnOK <- function(){
#          AMPSelectionnees = as.character(tkcurselection(tl))
#          AMP_ETUDIEES <- NA
#          for (k in (1:length(AMPSelectionnees))){
#              AMP_ETUDIEES[k] <- liste_AMP[as.numeric(AMPSelectionnees[k])+1]
#          }
#          assign("AMP_ETUDIEES",AMP_ETUDIEES,envir=.GlobalEnv)
#          tkdestroy(aa)
#      }
#      OK.but <-tkbutton(aa,text="OK",command=OnOK)
#      tkgrid(OK.but)
#      tkwm.deiconify(aa)
#      tkfocus(aa)
#      tkwait.window(aa)
#      tkdestroy(aa)
#}
## Barre d'échelle
#scalebar <- function(loc
#                    ,length
#                    ,width
#                     ,unit="km"
#                    ,division.cex=.8
#                    ,ScaleFactor=1
#                    ,...) {
#  if(missing(loc)) stop("loc is missing")
#  if(missing(length)) stop("length is missing")
#  x <- c(0,length/c(4,2,4/3,1),length*1.1)+loc[1]
#  y <- c(0,width/(10*3:1))+loc[2]
#  cols <- rep(c("black","white"),2)
#  for (i in 1:4) rect(x[i],y[1],x[i+1],y[2],col=cols[i])
#  for (i in 1:5) segments(x[i],y[2],x[i],y[3])
#  labels <- (x[c(1,3)]-loc[1])
#  labels <- c(paste(round(labels*ScaleFactor)),paste(round((x[5]-loc[1])*ScaleFactor),unit))
#  text(x[c(1,3,5)],y[4],labels=labels,adj=.5,cex=division.cex)
#  text(loc[1]-par("cxy")[1],loc[2],"Sources : Ifremer, SHOM, IGN.",adj=c(1,0),cex=division.cex)
#}



carto.f = function() {
   data <- data.frame(trait$Latitude,trait$Longitude)
   names(data) <- c("lat","lon")
   bb <- qbbox(lat=data$lat, lon=data$lon)
   #map <- GetMap.bbox(bb$lonR, bb$latR, destfile = "cartographie.png", maptype="hybrid")
   map <- GetMap.bbox(bb$lonR, bb$latR, destfile = "cartographie.png", maptype="hybrid")   
png("cartographie.png")
PlotOnStaticMap(map, lat=data$lat, lon=data$lon, destfile = NULL, cex=2, pch=20, col=rgb(1,0,0,0.7),add=F,verbose=FALSE)
dev.off()    
   #PlotOnStaticMap(map, lat=data$lat, lon=data$lon, destfile = "cartographie.png", cex=2, pch=20, col=rgb(1,0,0,0.7))
   #savePlot(filename = "Carte de la region et des positions echantillonnees","png")
}

#cartobis.f = function() {
#
#  #  choix_region.f()
#  #  liste_AMP_country <- subset(shape_mondial_amp@data,country == "SEN")
#  #  assign("liste_AMP_country",liste_AMP_country,envir=.GlobalEnv)
#  
#  #choix_AMP.f()               
#    #carte_AMP <- shape_mondial_amp[shape_mondial_amp@data$name == AMP_ETUDIEES,]
#    #Xlim <- carte_AMP@bbox[1,]
#    #Ylim <- carte_AMP@bbox[2,]    
#    
#    # Découpage de la fenêtre graphique
#    layout(matrix(c(1,1,1,2,1,1,1,3,1,1,1,4), 3, 4, byrow = TRUE))
#
#    # Marges de la carte locale et carte locale
#    par(mar=c(0,0,4,0))
#               
#    Xlim <- c(min(trait$Long)-0.05,max(trait$Long)+0.05)  
#    Ylim <- c(min(trait$Lat)-0.05,max(trait$Lat)+0.05)   
#    ## trait de cote
#    par(mar = c(2, 2, 2, 2) )
#    plot(shape_trait_cote,col=CouleurAutresPays,bg=CouleurDeLaMer,xlim=Xlim,ylim=Ylim,
#    xlab ="Longitude (decimal degrees)", 
#    ylab ="Latitude (decimal degrees)",
#    axes= T)
#    
#    par(new=TRUE)
#    ##ajout des rivieres
#    #map('rivers',xlim=Xlim,ylim=Ylim,add=TRUE,col = "blue")
#    
# 
#    
#    #shape mondial des AMP
#    #plot(shape_mondial_amp[shape_mondial_amp@data$name == AMP_ETUDIEES,], border="blue")
#      
#    ColWithAlpha <- function(col,alpha=191){
#       col <- as.vector(col2rgb(col))
#       out <- rgb(col[1],col[2],col[3],alpha,maxColorValue=255)
#       return(out)
#    }    
#    
#    # Plot du nombre d'échantillons par station
#    ColCercles <- "grey75"    
#    ColCercles <- ColWithAlpha(ColCercles)
#
#    #calcul du nombre de traits par stations
#    nbTraitsStationTemp <- tapply(trait$Trait, trait$Station, function(x) length(unique(x)))
#    nbTraitsStation = as.data.frame(matrix(NA,dim(nbTraitsStationTemp)[1],2))
#    colnames(nbTraitsStation) = c("Station","Effectif")
#    nbTraitsStation$Station = rep(dimnames(nbTraitsStationTemp)[[1]])
#    nbTraitsStation$Effectif = as.vector(nbTraitsStationTemp,"numeric")
#    nbTraitsStation$Long <- trait$Long[match(nbTraitsStation$Station,trait$Station)]
#    nbTraitsStation$Lat <- trait$Lat[match(nbTraitsStation$Station,trait$Station)]
#
#    points(nbTraitsStation$Long,nbTraitsStation$Lat,
#    ,pch=20
#    ,col=ColCercles
#    ,cex=sqrt(nbTraitsStation$Effectif)+2
#    ,xlim=Xlim
#    ,ylim=Ylim)       
#    
#    text(nbTraitsStation[,3]
#    ,nbTraitsStation[,4]
#    ,nbTraitsStation$Effectif)
#
#    #representation des coordonnees des stations
#    par(new=TRUE)
#
#    PointsStations <- unique(trait$Long,trait$Lat)
#    BlackTransparent <- ColWithAlpha("black")
#     
#    for (i in 1:length(unique(trait$Latitude))) {
#        points(trait$Long,trait$Lat, pch=15,cex=0.5,col=BlackTransparent)
#    }
#
#    legend("bottomleft" ,legend=c("Station d'échantillonnage","Nombre de traits")
#        ,pch=c(15,20)
#        ,col=c("black",ColCercles)
#        ,pt.cex=c(1,3)
#        ,cex=1.5
#        ,bty="n")
#           
#
#    # Ajout d'une fléche du nord
#    par(new=TRUE)
#    plot(0,0
#    ,xlim=c(0,1)
#    ,ylim=c(0,1)
#    ,type="n"
#    ,xlab=""
#    ,xaxt="n"
#    ,yaxt="n"
#    ,ylab=""
#    )
#    size <- 0.1
#    loc <- switch("topleft"
#              ,topleft = c(par("cxy")[1]+size/2,1-2*par("cxy")[2]-size/2)
#              ,topright = c(1-par("cxy")[1]-size/2,1-2*par("cxy")[2]-size/2)
#              ,bottomright = c(1-par("cxy")[1]-size/2,par("cxy")[2]+size/2)
#              ,bottomleft = c(par("cxy")[1]+size/2,par("cxy")[2]+size/2)
#              ,StaPos)
#    
#    northarrow(loc,size,ArrowType=3,border=NA)     
#    
#    #ajout de la carte globale
#    par(mfg=c(1,2))
#    par(mar=c(0,0,4,0))
#    map("worldHires", REGIONS_ETUDIEES)
#    rect(min(trait$Long)-0.1,
#         min(trait$Lat)-0.1,
#         max(trait$Long)+0.1,
#         max(trait$Lat)+0.1,border="red")
#    box()     
#    #title("Positions des stations d'échantillonnage et nombre de traits", cex.main = 2, font.main= 4)
#        
#    savePlot(filename = "Carte de la region et des positions echantillonnees","png")
#}
#
#
#
#