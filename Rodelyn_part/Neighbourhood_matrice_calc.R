rm(list=ls())

#packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(here)

#------------------------------------------------------------------------------------------------------------------------------

#read in data 

#------------------------------------------------------------------------------------------------------------------------------

#taking all data sets from workspace
Full_data <- lapply(list.files(path = here("workspace"), pattern = ".csv", full.names = TRUE),read.csv)
names(Full_data) <- c("ArchData", "all_fruit_data","ShootData") #assigning them names

#taking the data frames and placing them in indiviual data set in the global environment
lapply(names(Full_data), function(x) assign(x, Full_data[[x]], envir = .GlobalEnv))

#Fruit data
all_fruit_data <- all_fruit_data %>% 
	                   mutate(Row = case_when(Quadrant %in% 1:6 ~ 1,
						                      Quadrant %in% 7:12 ~ 2,
						                      Quadrant %in% 13:18 ~ 3,
						                      Quadrant %in% 19:24 ~ 4,
						                      Quadrant %in% 25:30 ~ 5,
						                      Quadrant %in% 31:36 ~ 6),
		                      Col = case_when(Quadrant %in% seq(1,31,by=6) ~ 1,
		   				                      Quadrant %in% seq(2,32,by=6) ~ 2,
		   				                      Quadrant %in% seq(3,33,by=6) ~ 3,
		   				                      Quadrant %in% seq(4,34,by=6) ~ 4,
		   				                      Quadrant %in% seq(5,35,by=6) ~ 5,
		   				                      Quadrant %in% seq(6,36,by=6) ~ 6))
#summarise DM by shoot
ggplot(all_fruit_data, aes(DryMatter,group=VineUUID)) + geom_density()
ggplot(all_fruit_data, aes(DryMatter,group=ParentOriginID)) + geom_density()

#give ID
all_fruit_data <- all_fruit_data %>% 
	mutate(ID =paste(SegmentEndY,SegmentEndX,CaneUUID,ParentOriginID,VineUUID,sep="_"))

#taking the average of each shoot
length(unique(all_fruit_data$ID));length(unique(all_fruit_data$ShootUUID))

ArchData <- ArchData %>% 
	           mutate(Row = case_when(Quadrant %in% 1:6 ~ 1,
						              Quadrant %in% 7:12 ~ 2,
						              Quadrant %in% 13:18 ~ 3,
						              Quadrant %in% 19:24 ~ 4,
						              Quadrant %in% 25:30 ~ 5,
						              Quadrant %in% 31:36 ~ 6),
		              Col = case_when(Quadrant %in% seq(1,31,by=6) ~ 1,
		   				              Quadrant %in% seq(2,32,by=6) ~ 2,
		   				              Quadrant %in% seq(3,33,by=6) ~ 3,
		   				              Quadrant %in% seq(4,34,by=6) ~ 4,
		   				              Quadrant %in% seq(5,35,by=6) ~ 5,
		   				              Quadrant %in% seq(6,36,by=6) ~ 6))

#------------------------------------------------------------------------------------------------------------------------------

#Visualisations

#------------------------------------------------------------------------------------------------------------------------------



ArchData %>%
	filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & !is.na(SegmentEndY) & VineTreatmentNoNumber %in% c("Conventional","Strung") ) %>%
	ggplot() +
	geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend =  SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter),col="lightgrey") +
	all_fruit_data %>% filter(.,!is.na(DryMatter) & VineTreatmentNoNumber %in% c("Conventional","Strung")) %>% 
	group_by(SegmentEndY, SegmentEndX, CaneUUID,VineUUID) %>%
	summarise(DryMatter = mean(DryMatter, na.rm = T),
			  ParentOriginID = ParentOriginID) %>%
    geom_point(data=., aes(SegmentEndY, SegmentEndX,col=ParentOriginID),
			shape = 21, alpha=0.8) +
	geom_line(data=	all_fruit_data %>% filter(.,!is.na(DryMatter) & VineTreatmentNoNumber %in% c("Conventional","Strung")) %>% 
			  	group_by(SegmentEndY, SegmentEndX, CaneUUID), aes(SegmentEndY, SegmentEndX, group = paste(ParentOriginID,CaneUUID,VineUUID)),alpha=0.8) +
	facet_wrap(~VineUUID, scales="free")+
	scale_size_continuous(breaks = pretty_breaks(10)) +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) 

#------------------------------------------------------------------------------------------------------------------------------

#Calculating neighbourhoods

#------------------------------------------------------------------------------------------------------------------------------



NB_mat <- function(Data){
	n = length(Data$SegmentEndY)
	if(n>1){ 
	if(!sum(diff(Data$SegmentEndY))==0) {x <- as.numeric(factor(rank(Data$SegmentEndY)))} else {x <- as.numeric(factor(rank(Data$SegmentEndX)))}
	ParentOriginID <- as.character(unique(Data$ParentOriginID))
	DF <- data.frame(x=x, SegmentEndY = Data$SegmentEndY,SegmentEndX = Data$SegmentEndX, 
					 CaneUUID = Data$CaneUUID,ParentOriginID = Data$ParentOriginID,VineUUID = Data$VineUUID,ID=Data$ID)
	Ymax=max(DF$x)
	mat <- lapply(DF$x, function(x) if(x==1) {c(x+1)}	else {if(x==Ymax) c(x-1) else c(x-1,  x+1)})
	names(mat) <- DF$ID
	
	DF2 <- data.frame(x=rep(DF$x,unlist(lapply(mat, length))),X_neigbour=unlist(mat))
	DF2$from_id <- rep(names(mat),unlist(lapply(mat, length)))
	DF2$to_id <- as.character(DF[match(DF2$X_neigbour,DF$x),]$ID)
	DF2$NB_SegmentEndX <- DF[match(DF2$X_neigbour,DF$x),]$SegmentEndX
	DF2$NB_SegmentEndY <- DF[match(DF2$X_neigbour,DF$x),]$SegmentEndY
	DF2 <- left_join(DF2,DF[match(DF2$X_neigbour,DF$x),])
    DF2[,c("CaneUUID", "ParentOriginID","ID")] <- sapply(DF2[,c("CaneUUID", "ParentOriginID","ID")],as.character)
    DX <- data.frame(DF2[which.min(abs(DF2$SegmentEndY-0)),])
    DX$to_id <- "to_Parent"
    DF2 <- unique(rbind(DF2,DX) %>% as.data.frame())
	} else  {
    	x <- as.numeric(factor(rank(Data$SegmentEndY)))
    	DF <- data.frame(x=x, X_neigbour=NA, from_id =Data$ID, to_id=NA,
    					 SegmentEndY = Data$SegmentEndY,SegmentEndX = Data$SegmentEndX, 
    					 CaneUUID = Data$CaneUUID,ParentOriginID = Data$ParentOriginID,VineUUID = Data$VineUUID,ID=Data$ID,
    					 NB_SegmentEndX=NA, NB_SegmentEndY=NA)
    
    	DF2 <- rbind(DF,DF)
    	DF2[2,"to_id"] <- "to_Parent"
    	DF2[,c("CaneUUID", "ParentOriginID","ID")] <- sapply(DF2[,c("CaneUUID", "ParentOriginID","ID")],as.character)
    }
	
	return(DF2)
}

Data <- DF_subset
Data_toanalyse <- all_fruit_data %>% filter(.,!is.na(ParentOriginID) & VineTreatmentNoNumber %in% c("Conventional","Strung"))
Data_toanalyse <- Data_toanalyse %>% group_by(ID) %>% slice(1) %>% as.data.frame()
i=1
NB_mat_df <- data.frame()
for(i in 1:length(unique(Data_toanalyse$CaneUUID))){
	DF_subset <- Data_toanalyse[Data_toanalyse$CaneUUID %in% unique(Data_toanalyse$CaneUUID)[i], ] %>% as.data.frame()
	DF_mat <- NB_mat(DF_subset)
	DF_mat$y <- i
	NB_mat_df <- unique(rbind(NB_mat_df, DF_mat) %>% as.data.frame())
}

NB_mat_df2 <- rbind(NB_mat_df,NB_mat_df %>% group_by(VineUUID) %>% 
	      mutate(y = mean(y),
		   x=-10) %>% filter(.,to_id %in% "to_Parent") %>%as.data.frame())

diff(sort(unique((NB_mat_df2 %>% filter(VineUUID==1))$y)) )  

ggplot(NB_mat_df2, aes(x=x, y=y, group=paste(ParentOriginID,CaneUUID)) ) +
	geom_point(aes(fill=factor(y)),alpha=0.5,shape=21) +  
	geom_line(alpha=0.5) +
	facet_wrap(~VineUUID, scales="free_y") + 
	theme_classic() +
	theme(legend.position="none", 
		  axis.text.x = element_blank(),axis.ticks.x =  element_blank(), 
		  axis.text.y = element_blank(),axis.ticks.y =  element_blank()) + xlab("") + ylab("") 

NB_mat_df2 <- NB_mat_df2 %>% group_by(VineUUID) %>%
	mutate(y2 = as.numeric(factor(rank(y)))) %>% group_by(y2) %>%
	mutate(y3 = min(y2)-1)


NB_mat_df2 %>% group_by(VineUUID) %>% head(100)

ggplot(NB_mat_df2, aes(x=x, y=rank(y), group=paste(ParentOriginID,CaneUUID)) ) +
	geom_point(aes(fill=factor(y)),alpha=0.5,shape=21) +  
	geom_line(alpha=0.5) +
	facet_wrap(~VineUUID, scales="free_y") + 
	theme_classic() +
	theme(legend.position="none", 
		  axis.text.x = element_blank(),axis.ticks.x =  element_blank(), 
		  axis.text.y = element_blank(),axis.ticks.y =  element_blank()) + xlab("") + ylab("") 


ggplot(NB_mat_df2, aes(x=x, y=y3))  +
	geom_tile(col="gray") +  
	facet_wrap(~VineUUID, scales="free_y") + 
	theme_classic() +
	theme(legend.position="none", 
		  axis.text.x = element_blank(),axis.ticks.x =  element_blank()) + xlab("") + ylab("") + xlim(c(0,20))


ggplot(NB_mat_df2 %>% filter(VineUUID==1), aes(x=x, y=y)) + geom_tile() +scale_x_log10() + scale_y_sqrt() + 
	#facet_wrap(~VineUUID, scales="free_y") +
		theme(legend.position="none") + theme_classic() 

NB_plot_DF <-NB_mat_df %>% select(from_id,to_id,ParentOriginID,VineUUID)

for(i in 1:length(unique(Data_toanalyse$VineUUID))){
	DF_subset <- Data_toanalyse[Data_toanalyse$VineUUID %in% unique(Data_toanalyse$VineUUID)[i], ] 
	DF_subset <- DF_subset %>% group_by(ParentOriginID) %>%slice(which.min(abs(SegmentEndY - 0))) %>%as.data.frame()
	DF_subset$x <- as.numeric(factor(rank(DF_subset$SegmentEndY)))
	
	mat <- lapply(DF_subset$x, function(x) if(x==1) {c(x+1)}	else {if(x==Ymax) c(x-1) else c(x-1,  x+1)})
	names(mat) <- DF_subset$ParentOriginID
	
	DF2 <- data.frame(from_id=rep(DF_subset$ParentOriginID,unlist(lapply(mat, length))),X_neigbour=unlist(mat))
	DF2$ParentOriginID <- DF2$from_id 
	DF2$to_id <- DF_subset[match(DF2$X_neigbour,DF_subset$x),]$ParentOriginID
	DF2$VineUUID <- unique(Data_toanalyse$VineUUID)[i]
	NB_plot_DF <- unique(rbind(NB_plot_DF, DF2[,c("from_id","to_id","VineUUID","ParentOriginID")]) %>% as.data.frame())
}

ggplot(NB_plot_DF, aes(from_id, to_id,fill=ParentOriginID)) + geom_raster() +facet_grid(~VineUUID)

NB_mat_df2 <- unique(NB_mat_df)

ArchData %>%
	filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & 
		   	!is.na(SegmentEndY) & VineTreatmentNoNumber %in% c("Conventional","Strung") & VineUUID==1 ) %>%
	ggplot() +
	geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend =  SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter),col="lightgrey") +
	all_fruit_data %>% filter(.,!is.na(DryMatter) & VineTreatmentNoNumber %in% c("Conventional","Strung") & VineUUID==1) %>% 
	geom_point(data=	., aes(SegmentEndY, SegmentEndX),alpha=0.8,shape=21)+
	NB_mat_df2 %>% filter(.,VineUUID==1 & ID %in% unique(ID)[101]) %>%
	geom_point(data=., aes(SegmentEndY, SegmentEndX),
			   shape = 19, alpha=0.8,col=3) +
	NB_mat_df2 %>% filter(.,VineUUID==1& ID %in% unique(ID)[101]) %>%
	geom_point(data=., aes(NB_SegmentEndY, NB_SegmentEndX),
			   shape = 19, alpha=0.8,col=2) +
	scale_size_continuous(breaks = pretty_breaks(10)) +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) 


unique((NB_mat_df %>% filter(.,VineUUID==1) %>% 
	filter(SegmentEndY <0 & SegmentEndY >-500 &SegmentEndX <0 & SegmentEndX >-500  ))$CaneUUID)
	NB_mat_df %>% filter(CaneUUID %in% "1-H6")

unique((NB_mat_df %>% filter(CaneUUID %in% "1-H6"))$ID)
ArchData %>%
	filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & 
		   	!is.na(SegmentEndY) & VineTreatmentNoNumber %in% c("Conventional","Strung") & VineUUID==1 ) %>%
	ggplot() +
	geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend =  SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter),col="lightgrey") +
	#all_fruit_data %>% filter(.,!is.na(DryMatter) & VineTreatmentNoNumber %in% c("Conventional","Strung") & VineUUID==1) %>% 
	#geom_point(data=., aes(SegmentEndY, SegmentEndX),alpha=0.8,shape=21)+
	NB_mat_df2 %>% filter(ID %in% "-320_-410_1-H6_1-O9_1") %>%
	geom_point(data=., aes(SegmentEndY, SegmentEndX),
			   shape = 19, alpha=0.8,col=3) +
	NB_mat_df2 %>% filter(.,ID %in% "-320_-410_1-H6_1-O9_1") %>% 
	geom_point(data=., aes(NB_SegmentEndY, NB_SegmentEndX),
			   shape = 19, alpha=0.8,col=2) +
	NB_mat_df2 %>% filter(.,ID %in% "-320_-410_1-H6_1-O9_1") %>% 
	geom_line(data=., aes(NB_SegmentEndY, NB_SegmentEndX),
			   shape = 19, alpha=0.8,col=3) +
	scale_size_continuous(breaks = pretty_breaks(10)) +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) + xlim(c(-500,0)) + ylim(c(-250,-500))

unique(as.character((NB_mat_df %>% filter(CaneUUID %in% "1-H6") %>% as.data.frame())$ID))
	
unique(NB_mat_df %>% filter(.,ID %in% "-320_-410_1-H6_1-O9_1"))	
ArchData %>%
	filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & 
		   	!is.na(SegmentEndY) & VineTreatmentNoNumber %in% c("Conventional","Strung") & VineUUID==1 ) %>%
	ggplot() +
	geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend =  SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter),col="lightgrey") +
	all_fruit_data %>% filter(.,!is.na(DryMatter) & VineTreatmentNoNumber %in% c("Conventional","Strung") & VineUUID==1) %>% 
	geom_point(data=., aes(SegmentEndY, SegmentEndX,col=CaneUUID),alpha=0.8,shape=21)+
	all_fruit_data %>% filter(.,!is.na(DryMatter) & VineTreatmentNoNumber %in% c("Conventional","Strung") & VineUUID==1) %>% 
	geom_line(data=., aes(SegmentEndY, SegmentEndX, group=CaneUUID, col=ParentOriginID),alpha=0.8)+
	scale_size_continuous(breaks = pretty_breaks(10)) +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) 

ggplot(NB_mat_df %>% filter(VineUUID==1)B_mat_df %>% filter(VineUUID==1), aes(from=from_id, to=to_id))+
	geom_net()

s2 <- NB_mat_df %>% head(5)	   
s1 <- as.data.frame(table(s2[,c("from_id","to_id")]))
str(s1)				
graph_from_adjacency_matrix(as.matrix(table(s2[,c("from_id","to_id")]), nrow=2))
