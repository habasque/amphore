   # Diversité Shannon
#   windows()
#      hist(diversity(capt_Bef.mat[,-c(1,2)],'shannon'), prob=TRUE, border="gray", xlab="Shannon",
#          main=paste("Shannon -",titre))
#      dens.Bef <- density(diversity(capt_Bef.mat[,-c(1,2)],'shannon'),kernel = "gaussian",adjust=2)
#      lines(dens.Bef, col="blue")
#      dens.Aft <- density(diversity(capt_Aft.mat[,-c(1,2)],'shannon'),kernel = "gaussian",adjust=2)
#      lines(dens.Aft, col="Red")
#      colorlegend(posy=c(0.7,0.75),posx=c(0.2,0.25),col="blue", zlim=c(0,1),zlevels=NULL,main=titre1)
#      colorlegend(posy=c(0.85,0.9),posx=c(0.2,0.25),col="red", zlim=c(0,1),zlevels=NULL,main=titre2)
#      savePlot(filename = paste("Shannon",titre), "png")



   # Diversité Shannon
   windows()
      hist(diversity(capt_Un.mat[,-c(1,2)],'shannon'), prob=TRUE, border="gray", xlab="Shannon",
          main=paste("Shannon -",titre))
      dens.Un <- density(diversity(capt_Un.mat[,-c(1,2)],'shannon'),kernel = "gaussian",adjust=2)
      lines(dens.Un, col="blue")
      dens.Deux <- density(diversity(capt_Deux.mat[,-c(1,2)],'shannon'),kernel = "gaussian",adjust=2)
      lines(dens.Deux, col="Red")
      dens.Trois <- density(diversity(capt_Trois.mat[,-c(1,2)],'shannon'),kernel = "gaussian",adjust=2)
      lines(dens.Trois, col="green")
      colorlegend(posy=c(0.55,0.6),posx=c(0.2,0.25),col="blue", zlim=c(0,1),zlevels=NULL,main=titre1)
      colorlegend(posy=c(0.7,0.75),posx=c(0.2,0.25),col="red", zlim=c(0,1),zlevels=NULL,main=titre2)
      colorlegend(posy=c(0.85,0.9),posx=c(0.2,0.25),col="green", zlim=c(0,1),zlevels=NULL,main=titre3)
      savePlot(filename = paste("Shannon",titre), "pdf")

