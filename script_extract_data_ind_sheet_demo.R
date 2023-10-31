library(ggplot2)
library("ggpubr")
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(data.table)
library(grid)
library(dplyr)
library(gridExtra)

setwd("I:/ARTICLE/data_avaibility/demo_script")
list_cond = c("ala3","cys2R") 
titre_cond=c("Alanine [10-3M]","Cysteine [10-2M]")
num_cond=c(1,2)

fish_type="CF"
list_fish_num = setdiff(64:69,c(65,67))

tinjection=30

#j: ID.Number of fish
#k: odour
#s: starting time (min)
#e: Ending time (min)

#raw_txt_files = out put data file from "Real Fish Tracker"

##f_coorxc to calculate x coordinates used for X-axis side view plot

f_coorxc<- function(j,k){
  
  fish_num=j
  cond=k
  filecoor=paste("raw_txt_files/","/",fish_type,"_",fish_num,"_",list_cond[cond],".txt", sep="")
  
  fish_data= read.table(filecoor,header = TRUE, sep = " ",skip=12)
  fish_coort=(fish_data[,1])/60 # minutes
  fish_coorx=fish_data[,2]
  
  box_coor=readLines(filecoor,2)
  box_coor= c(box_coor[2])
  box_coor1= as.numeric(unlist(strsplit(box_coor, " "))) # coordinates of  "tracking area"
  box_coorx=c(box_coor1[1],box_coor1[2],box_coor1[3],box_coor1[4]) # coordinates of  fish tracking area
  
  coor_ori=box_coorx[1]+((box_coorx[3]-box_coorx[1])/2)#middle of the side'box is recoded as zero
  
  #recoding of coordinates in new reference from -1 (water on the right) to 1 (odour side on the left) with 0 the middle of the side of the box
  
  fish_cor_coorx=fish_coorx-coor_ori
  box_cor_coorx=box_coorx-coor_ori
  
  max_x=max(fish_cor_coorx)
  min_x=min(fish_cor_coorx)
  
  for (i in 1:length(fish_cor_coorx)){
    if (fish_cor_coorx[i]<0){fish_cor_coorx[i]=fish_cor_coorx[i]/(min_x)}
    else fish_cor_coorx[i]=fish_cor_coorx[i]/-max_x}
  
  #reverse x when injection on the right side 
  
  if(cond==2){fish_cor_coorx=-fish_cor_coorx}
 
  
  return(fish_cor_coorx)
}

##f_coorx to calculate x coordinates used for 2D and 3D plot

f_coorx<- function(j,k){
  
  fish_num=j
  cond=k
  filecoor=paste("raw_txt_files/","/",fish_type,"_",fish_num,"_",list_cond[cond],".txt", sep="")
  
  fish_data= read.table(filecoor,header = TRUE, sep = " ",skip=12)
  fish_coort=(fish_data[,1])/60 # minutes
  fish_coorx=fish_data[,2]
  
  box_coor=readLines(filecoor,2)
  box_coor= c(box_coor[2])
  box_coor1= as.numeric(unlist(strsplit(box_coor, " "))) # coordinates of  "tracking area"
  box_coorx=c(box_coor1[1],box_coor1[2],box_coor1[3],box_coor1[4]) # coordinates of  fish tracking area
  
  coor_ori=box_coorx[1]+((box_coorx[3]-box_coorx[1])/2)#middle of the side'box is recoded as zero
  
  #recoding of coordinates in new reference from -1 (water on the right) to 1 (odour side on the left) with 0 the middle of the side of the box
  
  fish_cor_coorx=fish_coorx-coor_ori
  box_cor_coorx=box_coorx-coor_ori

  max_x=max(fish_cor_coorx)
  min_x=min(fish_cor_coorx)
  
    for (i in 1:length(fish_cor_coorx)){
    if (fish_cor_coorx[i]<0){fish_cor_coorx[i]=fish_cor_coorx[i]/(-(min_x))}
    else fish_cor_coorx[i]=fish_cor_coorx[i]/max_x}
  
  return(fish_cor_coorx)
  
}

##f_coory to calculate x coordinates used for 2D and 3D plot

f_coory<- function(j,k){
  
  fish_num=j
  cond=k
  filecoor=paste("raw_txt_files/","/",fish_type,"_",fish_num,"_",list_cond[cond],".txt", sep="")
  
  fish_data= read.table(filecoor,header = TRUE, sep = " ",skip=12)
  fish_coort=(fish_data[,1])/60 # minutes
  fish_coory=fish_data[,3]
  
  box_coor=readLines(filecoor,2)
  box_coor= c(box_coor[2])
  box_coor1= as.numeric(unlist(strsplit(box_coor, " "))) # coordinates of  "tracking area"
  box_coory=c(box_coor1[1],box_coor1[2],box_coor1[3],box_coor1[4])# coordinates of  fish tracking area
  
  coor_ori=box_coory[2]+((box_coory[4]-box_coory[2])/2) #middle of the side'box is recoded as zero
  
  #recoding of coordinates in new reference from -1 to 1  with 0 the middle of the side of the box
  
  fish_cor_coory=fish_coory-coor_ori
  box_cor_coory=box_coory-coor_ori
  
  max_y=max(fish_cor_coory)
  min_y=min(fish_cor_coory)
  
  for (i in 1:length(fish_cor_coory)){
    if (fish_cor_coory[i]<0){fish_cor_coory[i]=fish_cor_coory[i]/(-(min_y))}
    else fish_cor_coory[i]=fish_cor_coory[i]/max_y}
  
  fish_cor_coory=fish_cor_coory*0.74# rectangular box proportions

   return(-fish_cor_coory)#reverse coordinates Y because 0 is on the top left in "Real Fish Tracker"
}

f_coort<- function(j,k){
  fish_num=j
  cond=k
  filecoor=paste("raw_txt_files/","/",fish_type,"_",fish_num,"_",list_cond[cond],".txt", sep="")
  fish_data= read.table(filecoor,header = TRUE, sep = " ",skip=12)
  fish_coort=(fish_data[,1])/60# minutes
  return(fish_coort)
}


f_speed<- function(j,k){
  
  fish_num=j
  cond=k
  filecoor=paste("raw_txt_files/","/",fish_type,"_",fish_num,"_",list_cond[cond],".txt", sep="")
  
  fish_data= read.table(filecoor,header = TRUE, sep = " ",skip=12)
  fish_coort=(fish_data[,1])/60
  fish_speed=seq(1,(nrow(fish_data)))
  
  
  for (i in 1:(nrow(fish_data))) {
    fish_speed[i] = c((sqrt((fish_data[i+1,7] - fish_data[i,7]) ^ 2) + ((fish_data[i+1,8] - fish_data[i,8]) ^ 2))/(1/30))}
 
  
  return(fish_speed)
}


#######individual file of recoded data

extract_data <- function(){
  
  for (j in list_fish_num){
    for (k in num_cond){
      
      write.table(data.frame("time"=f_coort(j,k),"xc"=f_coorxc(j,k),"x"=f_coorx(j,k),"y"=f_coory(j,k),"speed"=f_speed(j,k)),
                  paste("Extract_data/",fish_type,"/extract_data_",fish_type,"_",j,"_",list_cond[k],".txt", sep=""), append = FALSE, sep = " ", dec = ".",
                  row.names = FALSE, quote=FALSE )
    }
    print(j)}
}

extract_data()

####X-axis side view plot with "XC" ### 2D and 3D with "X" and "Y"

setwd("I:/ARTICLE/data_avaibility/demo_script")

f_plot_coordx<- function(j,k){
  
  fish_num=j
  cond=k
  
  filecoor=paste("Extract_data/",fish_type,"/extract_data_",fish_type,"_",j,"_",list_cond[k],".txt", sep="")
  fish_data= read.table(filecoor,header = TRUE, sep = " ")
  s=0
  e=60
  s=(s*30*60)
  e=(e*30*60)
  fish_coort=fish_data[s:e,1]
  
  fish_coorx=fish_data[s:e,2]
  
  
  fish_coort_f <- fish_coort[seq(1, (length(fish_coort)), 4)]
  fish_coorx_f <- fish_coorx[seq(1, (length(fish_coorx)), 4)]
  
  
  #color1 water side
  #color2 odor side : darkorange/alanine; darkolivegreen/cysteine
  
  if(cond==1) {
    color_1="aquamarine"
      color_2="darkorange"
  }
  
  if(cond==2) {
    color_1="aquamarine"
    color_2="darkolivegreen"
  }
  
  
  library(ggplot2)
  
  
  ggpl_cond=ggplot(data.frame(fish_coort_f,fish_coorx_f),aes(x=fish_coort_f, y=fish_coorx_f)) + 
    geom_rect(aes(xmin = tinjection, xmax = tinjection, ymin = -1, ymax = 1), color="red", size = 1.25)+
    geom_rect(aes(xmin = 30, xmax = 60, ymin = -1, ymax = -0.333), alpha =0.005,fill=color_1)+
    geom_rect(aes(xmin = 30, xmax = 60, ymin = 1, ymax = 0.333), alpha =0.005,fill=color_2)+
    geom_rect(aes(xmin = -1, xmax = 60, ymin = -0.33,ymax= -0.33), linetype = "dashed", color="grey",alpha =0.05, size = 1) +
    geom_rect(aes(xmin = -1, xmax = 60, ymin = 0.33, ymax= 0.33), linetype = "dashed", color="grey",alpha =0.05, size = 1) +
    geom_line()+
    xlab("Time (min)")+
    ylab("Position (x-axis)")+
    ggtitle(label = paste(fish_type," n°",fish_num," ",titre_cond[cond], sep=""))+
    theme(panel.border = element_rect(color = "grey",size=1.25, fill="NA"),
          plot.title = element_text(face="bold", colour="black", size=10,hjust=0.5))+
    scale_x_continuous(expand=c(0,0),breaks = c(0,10,25,30,37,52,60))+
    theme(text=element_text(family="Arial"))
  return(plot(ggpl_cond))
}


f_plot1_2D<- function(j,k,s,e){
  
  fish_num=j
  cond=k
  s=10
  e=25
  
  filecoor=paste("Extract_data/",fish_type,"/extract_data_",fish_type,"_",j,"_",list_cond[k],".txt", sep="")
  Top_data= read.table(filecoor,header = TRUE, sep = " ")
  
  s=(s*30*60)
  e=(e*30*60)
  
  fish_coort1=Top_data[s:e,1]
  fish_coort1 <- fish_coort1[seq(1, (length(fish_coort1)), 4)]
  fish_coorx1=Top_data[s:e,3]
  fish_coorx1 <- fish_coorx1[seq(1, (length(fish_coorx1)), 4)]
  fish_coory1=Top_data[s:e,4]
  fish_coory1 <- fish_coory1[seq(1, (length(fish_coory1)), 4)]
  
  
  library(ggplot2)
  
  
  ggpl_plot_2D=ggplot(data.frame(fish_coorx1,fish_coory1),aes(x=fish_coorx1, y=fish_coory1)) + 
    geom_path(size = 0.3, alpha = 0.8, color = "black")+
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank())+
    #ggtitle(label = paste("10 min - 25 min"))+
    theme(plot.title = element_text( face="bold", colour="black", size=18))+
    geom_rect(aes(xmin = -1.02, xmax = 1.02, ymin = -0.76, ymax = 0.76),alpha =0.05,color="grey", size = 1.25,fill="NA")+
    annotate(geom="text", x=0, y=-0.85, label="x", color="grey50",size=4,face="bold")+
    annotate(geom="text", x=-1.15, y=0, label="y", color="grey50",size=4,face="bold")+
    theme(text=element_text(family="Arial"))
  
  
  return(plot(ggpl_plot_2D))
}

f_plot2_2D<- function(j,k,s,e){
  
  fish_num=j
  cond=k
  s=37
  e=52
  
  filecoor=paste("Extract_data/",fish_type,"/extract_data_",fish_type,"_",j,"_",list_cond[k],".txt", sep="")
  Top_data= read.table(filecoor,header = TRUE, sep = " ")
  
  s=(s*30*60)
  e=(e*30*60)
  
  fish_coort1=Top_data[s:e,1]
  fish_coort1 <- fish_coort1[seq(1, (length(fish_coort1)), 4)]
  fish_coorx1=Top_data[s:e,3]
  fish_coorx1 <- fish_coorx1[seq(1, (length(fish_coorx1)), 4)]
  fish_coory1=Top_data[s:e,4]
  fish_coory1 <- fish_coory1[seq(1, (length(fish_coory1)), 4)]
  
  #color1  tube on right side
  #color2  tube on left side
  
  if(cond==1) {
    color_1="aquamarine"
    color_2="darkorange"
  }
  
  if(cond==2) {
    color_1="darkolivegreen"
      color_2="aquamarine"
  }
  
  library(ggplot2)
  
  
  ggpl_plot_2D=ggplot(data.frame(fish_coorx1,fish_coory1),aes(x=fish_coorx1, y=fish_coory1)) + 
    geom_path(size = 0.3, alpha = 0.8, color = "black")+
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank())+
    #ggtitle(label = paste("37 min - 52 min"))+
    geom_point(aes(x=0.85, y=0), data = NULL, shape = 19, alpha=0.005,color=color_1,size =12)+
    geom_point(aes(x=-0.85, y=0), data = NULL, shape = 19, alpha=0.005,color=color_2,size =12)+
    theme(plot.title = element_text( face="bold", colour="black", size=18))+
    geom_rect(aes(xmin = -1.02, xmax = 1.02, ymin = -0.76, ymax = 0.76),alpha =0.05,color="grey", size = 1.25,fill="NA")+
    annotate(geom="text", x=0, y=-0.85, label="x", color="grey50",size=4,face="bold")+
    annotate(geom="text", x=-1.15, y=0, label="y", color="grey50",size=4,face="bold")+
    theme(text=element_text(family="Arial"))
  
  return(plot(ggpl_plot_2D))
}



library(gg3D)

f_plot1_3D<- function(j,k,s,e){
  
  
  fish_num=j
  cond=k
  
  filecoor=paste("Extract_data/",fish_type,"/extract_data_",fish_type,"_",j,"_",list_cond[k],".txt", sep="")
  Top_data= read.table(filecoor,header = TRUE, sep = " ")
  
  s=10
  e=25
  s=(s*30*60)
  e=(e*30*60)
  
  fish_coort1=Top_data[s:e,1]
  
  fish_coorx1=Top_data[s:e,3]
  
  fish_coory1=Top_data[s:e,4]
  
  fish_coort1 <- fish_coort1[seq(1, (length(fish_coort1)), 4)]
  fish_coorx1 <- fish_coorx1[seq(1, (length(fish_coorx1)), 4)]
  fish_coory1 <- fish_coory1[seq(1, (length(fish_coory1)), 4)]
  
  Minutes=(fish_coort1)
  xf=(fish_coorx1)
  yf=(fish_coory1)
  
  theta=-240
  phi=0
  
  plot3D=ggplot(data.frame(xf,yf,Minutes),aes(x=xf, y=yf, z=Minutes,color=Minutes)) +
    axes_3D(theta=theta, phi=phi,size=2,colour="grey")+
    stat_3D(theta=theta, phi=phi, geom="path", size=1) +
    labs_3D(theta=theta, phi=phi, 
            labs=c(" ", " ", " "), 
            angle=c(30,-12,90),
            hjust=c(-0.2,0.8,0.1), 
            vjust=c(-0.8,-0.8,3),
            size=6, 
            face="bold") +
    scale_color_gradientn(colors=plot3D::jet2.col())+
    ggtitle(label = paste("control"))+
    theme_classic2() +
    theme(legend.position = "right",
          legend.key.size = unit(1, "cm"),
          legend.key.width = unit(0.3, "cm"),
          legend.box.spacing = unit(0.5,"cm"),
          legend.title = element_text(size=12,face="bold"),
          legend.text = element_blank())+
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size=18, hjust=0.5))+
    theme(text=element_text(family="Arial"))
  
  return(plot(plot3D))
}

f_plot2_3D<- function(j,k,s,e){
  
  
  fish_num=j
  cond=k
  
  filecoor=paste("Extract_data/",fish_type,"/extract_data_",fish_type,"_",j,"_",list_cond[k],".txt", sep="")
  Top_data= read.table(filecoor,header = TRUE, sep = " ")
  
  s=37
  e=52
  s=(s*30*60)
  e=(e*30*60)
  
  fish_coort1=Top_data[s:e,1]
  
  fish_coorx1=Top_data[s:e,3]
  
  fish_coory1=Top_data[s:e,4]
  
  fish_coort1 <- fish_coort1[seq(1, (length(fish_coort1)), 4)]
  fish_coorx1 <- fish_coorx1[seq(1, (length(fish_coorx1)), 4)]
  fish_coory1 <- fish_coory1[seq(1, (length(fish_coory1)), 4)]
  
  Minutes=(fish_coort1)
  xf=(fish_coorx1)
  yf=(fish_coory1)
  
  theta=-240
  phi=0
  
  plot3D=ggplot(data.frame(xf,yf,Minutes),aes(x=xf, y=yf, z=Minutes,color=Minutes)) +
    axes_3D(theta=theta, phi=phi,size=2,colour="grey")+
    stat_3D(theta=theta, phi=phi, geom="path", size=1) +
    labs_3D(theta=theta, phi=phi, 
            labs=c(" ", " ", " "), 
            angle=c(30,-12,90),
            hjust=c(-0.2,0.8,0.1), 
            vjust=c(-0.8,-0.8,3),
            size=6, 
            face="bold") +
    scale_color_gradientn(colors=plot3D::jet2.col())+
    ggtitle(label = paste("odor"))+
    scale_x_continuous( breaks = seq(1,11,1))+
    theme_classic2() +
    theme(legend.position = "right",
          legend.key.size = unit(1, "cm"),
          legend.key.width = unit(0.3, "cm"),
          legend.box.spacing = unit(0.5,"cm"),
          legend.title = element_text(size=12,face="bold"),
          legend.text = element_blank())+
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size=18, hjust=0.5))+
    theme(text=element_text(family="Arial"))
  
  return(plot(plot3D))
}


Fiche_individuelle_page1<- function(){

  for (j in list_fish_num){
    for (k in num_cond){

    figure1=grid.arrange(
      plot(f_plot_coordx(j,k)),
      plot(f_plot1_2D(j,k)),plot(f_plot2_2D(j,k)),plot(f_plot1_3D(j,k)),plot(f_plot2_3D(j,k)),
      nrow=2,
      widths = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
      layout_matrix = rbind(c(1,1,1,1,1,1,1,1,1,1,1,1,4,4,4,5,5,5),
                            c(1,1,1,1,1,1,1,1,1,1,1,1,4,4,4,5,5,5),
                            c(NA,NA,2,2,2,2,NA,3,3,3,3,NA,4,4,4,5,5,5),
                            c(NA,NA,2,2,2,2,NA,3,3,3,3,NA,4,4,4,5,5,5),
                            c(NA,NA,2,2,2,2,NA,3,3,3,3,NA,4,4,4,5,5,5)))
    
    
    annotate_figure(figure1,top = text_grob(paste(fish_type,"_Demo",sep=""), size = 10,family = "Arial"))
    ggsave(paste("graph_fish/",fish_type,"/",fish_type,"_",j,"_",list_cond[k],"_","Eth_2D_3D.png", sep=""), width = 16.54 , height = 4.6 )
    print(j)}}
}


Fiche_individuelle_page1()
