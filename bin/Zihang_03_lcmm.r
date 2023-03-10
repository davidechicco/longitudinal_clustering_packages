#---
## Description: This program implement several cluster analysis methods
## to analyze and cluster epileptic.qol data
## Author: Zihang Lu"
## Date: "Fall 2022"

## Individual Longitudinal Profiles for Three Features of Interest
## (1) Anxiety Score
## (2) Depress Score
## (3) Liverpool Adverse Events Profile

# rm(list = ls())
## install.packages("joineRML")

list.of.packages <- c("easypackages", "dplyr", "pacman", "joineRML", "ggplot2","cowplot", "survminer", "survival", "lcmm")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

library("easypackages")
libraries(list.of.packages)


data(epileptic.qol)							# import data from joineRML library	(use ?epileptic.qol to see details)
epileptic.qol$time_month <- epileptic.qol$time/30.25 		# convert days to months
epileptic.qol <- epileptic.qol[order(epileptic.qol$id,epileptic.qol$time_month),]  # Sort by ID and time
   
#-----------------------------------------------------------------------------------------#
# Fitting Latent Class Mixed Effect Model via the lcmm package
#-----------------------------------------------------------------------------------------#
epileptic.qol$anxiety_scale <- as.numeric(scale(epileptic.qol$anxiety))
epileptic.qol$depress_scale <- as.numeric(scale(epileptic.qol$depress))
epileptic.qol$aep_scale <- as.numeric(scale(epileptic.qol$aep))

library(lcmm)
mult1a <- multlcmm(anxiety_scale + depress_scale + aep_scale~ time_month, 
		 	random =~ 1 ,
			subject='id', 
			data = epileptic.qol, 
			randomY = TRUE,ng = 1)

BIC <- NULL
for (kk in 2:8){
fit.multlcmm <- multlcmm(anxiety_scale + depress_scale + aep_scale ~ time_month, 
			mixture = ~ time_month,
		 	random =~ 1 , 
			subject='id', 
			data = epileptic.qol, 
			randomY = TRUE,ng = kk, B =mult1a  )
BIC <- c(BIC,fit.multlcmm$BIC)
}

# print the number of clusters with the smallest BIC
num.clust.multlcmm <- which.min(BIC) + 1; num.clust.multlcmm

ptm <- proc.time()
fit.multlcmm <- multlcmm(anxiety_scale + depress_scale + aep_scale ~ time_month, 
			mixture = ~ time_month,
		 	random =~ 1 , 
			subject='id', 
			data = epileptic.qol, 
			randomY = TRUE,ng = num.clust.multlcmm, B =mult1a  )
run.time.lcmm <- as.numeric((proc.time() - ptm)[3])

# relabel cluster
cluster.re <- (fit.multlcmm$pprob$class==4)*1 + (fit.multlcmm$pprob$class==1)*2 + (fit.multlcmm$pprob$class==2)*3 + (fit.multlcmm$pprob$class==3)*4 

N <- length(unique(epileptic.qol$id))
per <- paste(round(100*table(cluster.re)/N,1),"%",sep="")
cluster.lcmm <- factor(cluster.re, label=paste("Cluster ",1:num.clust.multlcmm," (",per,")",sep=""))

dat.cluster <- data.frame(fit.multlcmm$pprob$id,cluster.lcmm)
colnames(dat.cluster) <- c("id","cluster.multlcmm")
dnew_uq <- epileptic.qol[!duplicated(epileptic.qol$id, fromLast=TRUE),] # Keep last observation per id
dnew_uq <- merge(dnew_uq,dat.cluster,by="id")
dnew <- merge(epileptic.qol,dat.cluster,by="id")
head(dnew)

p1.lcmm <- ggplot(data =dnew, aes(x =time_month, y = anxiety, color=cluster.multlcmm,linetype=cluster.multlcmm,fill=cluster.multlcmm))+
    ggtitle("lcmm") + 
 		#geom_point(size=2,alpha=0.2) +
	 	#geom_line(aes(x = time_month, y = anxiety,group=id,color=cluster.multlcmm),size=1.5,alpha=0.2)+ 
		geom_smooth(aes(x =time_month, y = anxiety, color=cluster.multlcmm,linetype=cluster.multlcmm,fill=cluster.multlcmm), method = "loess", size = 3,se = FALSE,span=2)+
 		theme_bw() + 
		theme(legend.position = "bottom",
			plot.title = element_text(size = 15, face = "bold"),
			axis.text=element_text(size=15),
			axis.title=element_text(size=15),
			axis.text.x = element_text(angle = 0 ),
			strip.text.x = element_text(size = 15, angle = 0),
			strip.text.y = element_text(size = 15,face="bold")) + 
		guides(fill=guide_legend(title=NULL,nrow = 1,byrow=TRUE),color=guide_legend(title=NULL,nrow = 1,byrow=TRUE),
				linetype=guide_legend(title=NULL,nrow = 1,byrow=TRUE)) + 
		xlab("Time (months)") + ylab("anxiety") + 
		ylim(c(min(dnew$anxiety,na.rm=TRUE),max(dnew$anxiety,na.rm=TRUE)))+
	 	scale_color_manual(values=c("green", "black","blue","red","purple","goldenrod3","dimgray","darkorange3"))+
	 	scale_fill_manual(values=c("green", "black","blue","red","purple","goldenrod3","dimgray","darkorange3"))

p2.lcmm <- ggplot(data =dnew, aes(x =time_month, y = depress, color=cluster.multlcmm,linetype=cluster.multlcmm,fill=cluster.multlcmm))+
  ggtitle("lcmm") + 
 		#geom_point(size=2,alpha=0.2) +
		#geom_line(aes(x = time_month, y = depress,group=id,color=cluster.multlcmm),size=1.5,alpha=0.2)+ 
		geom_smooth(aes(x =time_month, y = depress, color=cluster.multlcmm,linetype=cluster.multlcmm,fill=cluster.multlcmm), method = "loess", size = 3,se = FALSE,span=2)+
 		theme_bw() + 
		theme(legend.position = "bottom",
			plot.title = element_text(size = 15, face = "bold"),
			axis.text=element_text(size=15),
			axis.title=element_text(size=15),
			axis.text.x = element_text(angle = 0 ),
			strip.text.x = element_text(size = 15, angle = 0),
			strip.text.y = element_text(size = 15,face="bold")) +
		guides(fill=guide_legend(title=NULL,nrow = 1,byrow=TRUE),color=guide_legend(title=NULL,nrow = 1,byrow=TRUE),
				linetype=guide_legend(title=NULL,nrow = 1,byrow=TRUE)) + 
		xlab("Time (months)") + ylab("depress") +
		 ylim(c(min(dnew$depress,na.rm=TRUE),max(dnew$depress,na.rm=TRUE)))+ 
	 	scale_color_manual(values=c("green", "black","blue","red","purple","goldenrod3","dimgray","darkorange3"))+
	 	scale_fill_manual(values=c("green", "black","blue","red","purple","goldenrod3","dimgray","darkorange3"))

p3.lcmm <- ggplot(data =dnew, aes(x =time_month, y = aep, color=cluster.multlcmm,linetype=cluster.multlcmm,fill=cluster.multlcmm))+
  ggtitle("lcmm") + 
 		#geom_point(size=2,alpha=0.2) +
		#geom_line(aes(x = time_month, y = aep,group=id,color=cluster.multlcmm),size=1.5,alpha=0.2)+ 
		geom_smooth(aes(x =time_month, y = aep, color=cluster.multlcmm,linetype=cluster.multlcmm,fill=cluster.multlcmm), method = "loess", size = 3,se = FALSE,span=2)+
 		theme_bw() + 
		theme(legend.position = "bottom",
			plot.title = element_text(size = 15, face = "bold"),
			axis.text=element_text(size=15),
			axis.title=element_text(size=15),
			axis.text.x = element_text(angle = 0 ),
			strip.text.x = element_text(size = 15, angle = 0),
			strip.text.y = element_text(size = 15,face="bold")) + 
		guides(fill=guide_legend(title=NULL,nrow = 1,byrow=TRUE),color=guide_legend(title=NULL,nrow = 1,byrow=TRUE),
				linetype=guide_legend(title=NULL,nrow = 1,byrow=TRUE)) + 
		xlab("Time (months)") + ylab("aep")  +
		ylim(c(min(dnew$aep,na.rm=TRUE),max(dnew$aep,na.rm=TRUE)))+ 
	 	scale_color_manual(values=c("green", "black","blue","red","purple","goldenrod3","dimgray","darkorange3"))+
	 	scale_fill_manual(values=c("green", "black","blue","red","purple","goldenrod3","dimgray","darkorange3"))

dev.new(width=180, height=90)
plot_grid(p1.lcmm,NULL,p2.lcmm,NULL,p3.lcmm,NULL,labels=c("(A)","", "(B)","","(C)",""), nrow = 1,  
		align = "v", rel_widths = c(1,0.1,1,0.1,1,0.1))
 

#------------------------------------ END OF THE PROGRAM ------------------------------------------------------#
 
