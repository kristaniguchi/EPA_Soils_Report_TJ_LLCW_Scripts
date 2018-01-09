#R Script to generate ternary diagrams in EPA Soils Report by K. Taniguchi (kristaniguchi@gmail.com)
  #Figures 4.17 to 4.21
    #ternary plot of the observed soils (LLCW and US) on the SSURGO-derived soil map, with US and MX sediment trap texture
      #Sandy Conglomerate: CbB, CfB, LfC, Lf
      #Conglomerate: surface has low clay content: Oh


install.packages("vcd") #install on very first run only
library(vcd)

#Directory
dir = "C:/Users/krist/Downloads/ternarydiagram/Update_Ternary_Plots_20171027/"
  list.files(dir) #list files in directory

##################################################################################################################################
#################Ternary Diagram with LLCW, TJE trap, MXTrap, and SSURGO  ########################################################

#file names for each source of soil texture data
fname.survey.LLCW = "napoleon_texture_LLCW_mapping_2015_in_report3_KT.csv" #napo's soil samples collected in LLCW
fname.US.traps.AMEC = "soil_texture_US_sedtraps_AMEC_KT.csv" #texture in the Tijuana Estuary (TJE) traps US
fname.SSURGO = "SSURGO_texture_for_analagous_soils2_KT.csv" #Napo removed the SURGO outlier soils that don't fit our data
fname.survey.US.TJE.SC = "soil_texture_20170731_US_TJE.SC_KT.csv" #KT's soil samples collected in US at TJE and Spring Canyon (SC)

#read in all 4 csv files
texture.llcw0 = read.csv(paste0(dir,fname.survey.LLCW),stringsAsFactors = FALSE) #LLCW soil texture
  texture.llcw = texture.llcw0[,c(3:length(texture.llcw0[1,]))] #LLCW soil texture subset
texture.US.traps = read.csv(paste0(dir,fname.US.traps.AMEC),stringsAsFactors = FALSE) #tje trap survey
texture.SSURGO = read.csv(paste0(dir,fname.SSURGO),stringsAsFactors = FALSE) #SSURGO texture
texture.US.TJE.SC = read.csv(paste0(dir,fname.survey.US.TJE.SC),stringsAsFactors = FALSE) #US soil texture TJE and SC

#combine texture data from llcw with US trap 
texture.llcw.UStraps = rbind(texture.llcw,texture.US.traps) #combine LLCW survey data and TJE trap data so can standardize based on total fines (has cobble included, want sand, silt, clay to add to 100)

#Calc % sand, silt, clay for LLCW and US trap data (based on total sand silt clay), original includes cobble%, only want to summarize based on finer than cobble categories
index.toplot = grep("SURF|SUB|MXSB|USTRAP",texture.llcw.UStraps$Geology ) #index of all surface, subsurface, trap soil samples, this excludes channel samples and AGNPS proposed texture

texture.llcw.UStraps$Total = rowSums(texture.llcw.UStraps[,4:8]) #sum of all texture from cobble to clay for each row
texture.llcw.UStraps$Tfines = rowSums(texture.llcw.UStraps[,6:8]) #sum of texture sand, silt, clay for each row
texture.llcw.UStraps$Sand.norm = texture.llcw.UStraps$Sand/texture.llcw.UStraps$Tfines #percent sand (based on Tfines as total) normalized
texture.llcw.UStraps$Silt.norm = texture.llcw.UStraps$Silt/texture.llcw.UStraps$Tfines #percent silt (based on Tfines as total) normalized
texture.llcw.UStraps$Clay.norm = texture.llcw.UStraps$Clay/texture.llcw.UStraps$Tfines #percent clay (based on Tfines as total) normalized
texture.llcw.UStraps1 = texture.llcw.UStraps[index.toplot,] #all the rows except for PROP ones in AGNPS model
texture.llcw.UStraps1.simple = data.frame(Geology_type = texture.llcw.UStraps1$Geology_type, Geology =texture.llcw.UStraps1$Geology ,Sand=texture.llcw.UStraps1$Sand.norm,Silt=texture.llcw.UStraps1$Silt.norm,Clay=texture.llcw.UStraps1$Clay.norm) #sand, silt, clay for each
  texture.llcw.UStraps.means = aggregate(texture.llcw.UStraps1.simple,by=list(as.character(texture.llcw.UStraps1.simple$Geology)), FUN="mean" ) #mean sand, silt, clay for each geology group

# Set Geology symbols for plotting
unique.Geology = as.character(unique(texture.llcw.UStraps1.simple$Geology)) #unique Geology categories
#index of each Geol category
CfB.MX.SUB.llcw.index = grep("CfB.MX.SUB.LLCW",texture.llcw.UStraps1.simple$Geology )
CfB.MX.SURF.llcw.index = grep("CfB.MX.SURF.LLCW",texture.llcw.UStraps1.simple$Geology )
Lf.SUB.llcw.index = grep("Lf.SUB.LLCW",texture.llcw.UStraps1.simple$Geology )
Lf.SURF.llcw.index = grep("Lf.SURF.LLCW",texture.llcw.UStraps1.simple$Geology )
MXSB.index = grep("MXSB",texture.llcw.UStraps1.simple$Geology )
USTRAP.index = grep("USTRAP",texture.llcw.UStraps1.simple$Geology )
#setting symbols
pvec = c(1,16,2,17,7,9) #point symbols
pchvec.all = rep(pvec[1],times=length(texture.llcw.UStraps1.simple$Geology ))  #set plotting symbol type for all soil samples, first all to pch=1, CfB.MX.SUB.LLCW is 1 (open circle)
pchvec.all[CfB.MX.SURF.llcw.index] = pvec[2]
pchvec.all[Lf.SUB.llcw.index] = pvec[3]
pchvec.all[Lf.SURF.llcw.index] = pvec[4]
pchvec.all[MXSB.index] = pvec[5]
pchvec.all[USTRAP.index] = pvec[6]
#set color of symbols
colcode = c(1:5)
colvec.all = rep("black",times=length(texture.llcw.UStraps1.simple$Geology)) #black is all llcw, set US traps as "grey"
colvec.all[MXSB.index] = "grey" #sediment traps are grey
colvec.all[USTRAP.index] = "grey" #sediment traps are grey


#SSURGO data
texture.SSURGO.simple = data.frame(Geology_type = texture.SSURGO$Geology_type, Geology =texture.SSURGO$Geology ,Sand=texture.SSURGO$Sand,Silt=texture.SSURGO$Silt,Clay=texture.SSURGO$Clay)
#combine llcw, UStraps, MXtraps, with SSURGO (simplified tables)
texture.llcw.UStraps1.simple.SSURGO = rbind(texture.llcw.UStraps1.simple,texture.SSURGO.simple)

#Symbology for SSURGO
unique.SSURGO = as.character(unique(texture.SSURGO.simple$Geology))
#setting symbols and colors for SSURGO, all red (took out OhCsub5 and Lfsub5, so won't need diff colors)
pch.SSURGO = c(18,5,16,1,17,2) #symbol values for each Geology
colvec.SSURGO = rep("red", time=length(texture.SSURGO.simple[,1])) #all red

#Combined symbology for llcw,mxsed,ustraps, and SSURGO
colvec.final = c(colvec.all,colvec.SSURGO) #color for SURGO will be based on layer depth, may need to assign color based on the depth number
pchvec.final = c(pchvec.all,pch.SSURGO) #symbol for SURGO will be 
#symbology for legend
pvec.legend = c(pvec, pch.SSURGO)
col.legend = c(rep("black",length(pvec)-2),"grey","grey", colvec.SSURGO) #llcw black, sed traps grey, ssurgo red

#ternary plot
ternaryplot(data.frame(texture.llcw.UStraps1.simple.SSURGO$Sand,texture.llcw.UStraps1.simple.SSURGO$Silt,texture.llcw.UStraps1.simple.SSURGO$Clay),pch=pchvec.final,col=colvec.final,main="",dimnames=c("Sand","Silt","Clay"))
grid_legend("topright", inset=c(0.01,0),pch=pvec.legend,col=col.legend,labels=c("CfB.MX.SUB.LLCW","CfB.MX.SURF.LLCW","Lf.SUB.LLCW","Lf.SURF.LLCW","MXSB","USTRAP", unique.SSURGO))


##################################################################################################################################
########FIGURE 4.17: Ternary Plot with ALL the data including US soil samples (TJE/SC)
####Add in the US soil samples (texture.US.TJE.SC) to texture.llcw.UStraps1.simple.SSURGO df#######

#simplified US soil samples (TJESC) dataframe
texture.US.TJE.SC.simple = data.frame(Geology_type = texture.US.TJE.SC$Geology_type, Geology =texture.US.TJE.SC$Geology ,Sand=texture.US.TJE.SC$Sand/100,Silt=texture.US.TJE.SC$Silt/100,Clay=texture.US.TJE.SC$Clay/100) #orig sand,silt,clay in %, divided by 100 to get decimal

#symbology for US TJE SC Geology (use same symbology, but in blue--> Lf has 3 sub layers, need to use diff shades of blue)
unique.TJESC.texture = sort(as.character(unique(texture.US.TJE.SC.simple$Geology)))
#index of each Geol category
CbB.SUB.US.index = grep("CbB.SUB.US",texture.US.TJE.SC.simple$Geology )
CbB.SURF.US.index = grep("CbB.SURF.US",texture.US.TJE.SC.simple$Geology )
CfB.SUB.US.index = grep("CfB.SUB.US",texture.US.TJE.SC.simple$Geology )
CfB.SURF.US.index = grep("CfB.SURF.US",texture.US.TJE.SC.simple$Geology )
Lf.SUB.US.index = grep("Lf.SUB.US",texture.US.TJE.SC.simple$Geology )
Lf.SUB2.US.index = grep("Lf.SUB2.US",texture.US.TJE.SC.simple$Geology )
Lf.SUB3.US.index = grep("Lf.SUB3.US",texture.US.TJE.SC.simple$Geology )
Lf.SURF.US.index = grep("Lf.SURF.US",texture.US.TJE.SC.simple$Geology )
  #OhC.SURF.US.index = grep("OhC.SURF.US",texture.US.TJE.SC.simple$Geology ) #took out OhC sample because no OhC mapped in LLCW anymore! CfB.MX instead
#setting symbols
pvec.US = c(5,18,1,16,2,2,2,17) #point symbols for all the unique Geology classes
pchvec.all.US = rep(pvec.US[1],times=length(texture.US.TJE.SC.simple$Geology ))  # set plotting symbol type for all soil samples, first all to pch=1
pchvec.all.US[CbB.SURF.US.index] = pvec.US[2]
pchvec.all.US[CfB.SUB.US.index] = pvec.US[3]
pchvec.all.US[CfB.SURF.US.index] = pvec.US[4]
pchvec.all.US[Lf.SUB.US.index] = pvec.US[5]
pchvec.all.US[Lf.SUB2.US.index] = pvec.US[6]
pchvec.all.US[Lf.SUB3.US.index] = pvec.US[7]
pchvec.all.US[Lf.SURF.US.index] = pvec.US[8]
#set color of symbols, all are blue, but Lf sub2 and Lf sub3 need to be different shade of blue to distingushi from sub1
colvec.all.US = rep("blue",times=length(texture.US.TJE.SC.simple$Geology)) #black is subsurface, grey is surface, blue is sed basins
colvec.all.US[Lf.SUB2.US.index] = "cyan" #bright blue
colvec.all.US[Lf.SUB3.US.index] = "blueviolet" #dark blue

#Combine USTJESC texture with all data!
texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL = data.frame(rbind(texture.llcw.UStraps1.simple.SSURGO, texture.US.TJE.SC.simple))

#combine symbology and colors for whole dataframe (all data)
colvec.all.US.MX.final = c(colvec.final,colvec.all.US)
pchvec.all.US.MX.final = c(pchvec.final,pchvec.all.US)
#legend symbology and colors (keeping US samples on a diff legend since already a lot in first legend)
  #for symbology use pvec.US which corresponds to the unique.TJESC.texture
colvec.US = rep("blue",times=length(unique.TJESC.texture)) #US all blue except for Lf which has multiple sub surfaces
  Lf.SUB2.US.index.legend = grep("Lf.SUB2.US",unique.TJESC.texture) #index of unique LfSUB2
  Lf.SUB3.US.index.legend = grep("Lf.SUB3.US",unique.TJESC.texture) #index of unique LfSUB3
colvec.US[Lf.SUB2.US.index.legend] = "cyan" #bright blue
colvec.US[Lf.SUB3.US.index.legend] = "blueviolet" #dark blue

#FIGURE 4.17
#ternary plot with ALL samples including US and MX, SSURGO, US and MX Sed traps
ternaryplot(data.frame(texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL$Sand,texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL$Silt,texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL$Clay),pch=pchvec.all.US.MX.final,col=colvec.all.US.MX.final,main="",dimnames=c("Sand","Silt","Clay"))
grid_legend("topright",inset=c(0.01,0),pch=pvec.legend,col=col.legend,labels=c("CfB.MX.SUB.LLCW","CfB.MX.SURF.LLCW","Lf.SUB.LLCW","Lf.SURF.LLCW","MXSB","USTRAP", unique.SSURGO))
grid_legend("topleft",pch=pvec.US,col=colvec.US,labels= unique.TJESC.texture)

##################################################################################################################################
##############################Ternary diagrams based on Geology Type###################################################################

################################### CfB.MX Texture Diagrams ##################################################################################
###Figure 4.18
#was previously called OhC, now called CfB.MX
#subset the dataframe by Geology_type, take only the CfB.MX samples and put into diagram
#added in average CfB.MX SURF and SUB from LLCW soil samples, add in LF SSURGO into plot!

#subset df --> CfB.MX
#index of all the CfB.MX samples to subset df, symbology and colors
CfB.MX.index.ALL = grep("CfB.MX", texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL$Geology_type)
  cFB.SSURGO.index = grep("^CfB$", texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL$Geology_type) #index SSURGO CfB exact match ^CfB$
CfB.MX.subset.ALL = rbind(texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL[CfB.MX.index.ALL,], texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL[cFB.SSURGO.index,])

#symbology and colors --> for CfB.MX subset
  colvec.CfB.MX1 = colvec.all.US.MX.final[CfB.MX.index.ALL] #for CfB.MX
colvec.CfB.MX = c(colvec.CfB.MX1, "red", "red") #for CfB.MX and SSURGO CfB at end
  pchvec.CfB.MX1 = pchvec.all.US.MX.final[CfB.MX.index.ALL] 
pchvec.CfB.MX = c(pchvec.CfB.MX1, 1,16) #for CfB.MX and SSURGO CfB at end
  
#legend symbology and colors for CfB.MX subset
unique.CfB.MX.Geology = sort(as.character(unique(CfB.MX.subset.ALL$Geology))) #unique CfB.MX Geology sorted
pchvec.CfB.MX.legend = c(1,16,1,16) #open circle SUB, filled circle SURF
colvec.CfB.MX.legend = c("black","black","red","red") #LLCW black, SSURGO red 

#ternary diagram CfB.MX (without average and LF)
ternaryplot(data.frame(CfB.MX.subset.ALL$Sand,CfB.MX.subset.ALL$Silt,CfB.MX.subset.ALL$Clay),pch=pchvec.CfB.MX,col=colvec.CfB.MX,main="CfB.MX Ternary Diagram",dimnames=c("Sand","Silt","Clay"))
grid_legend("topright",inset=c(0.01,0),pch=pchvec.CfB.MX.legend,col=colvec.CfB.MX.legend,labels=unique.CfB.MX.Geology)
  #CfB.MX sub4 and sub5 are the same texture! (not sure why there was 2 listed?)

#####Average CfB.MX added to Ternary:

#subsets of CfB.MX SUB and SURF LLCW to get averages for sand/silt/clay 
CfB.MX.SUB = CfB.MX.subset.ALL[as.character(CfB.MX.subset.ALL$Geology) == "CfB.MX.SUB.LLCW",]
CfB.MX.SURF = CfB.MX.subset.ALL[as.character(CfB.MX.subset.ALL$Geology) == "CfB.MX.SURF.LLCW",]
#Averages for SUB and SURF
CfB.MX.SUB.AV =  c("CfB.MX", "CfB.MX.SUB.AV", mean(CfB.MX.SUB$Sand), mean(CfB.MX.SUB$Silt), mean(CfB.MX.SUB$Clay)) #create row for CfB.MX.SUB.AV
CfB.MX.SURF.AV =  c("CfB.MX", "CfB.MX.SURF.AV", mean(CfB.MX.SURF$Sand), mean(CfB.MX.SURF$Silt), mean(CfB.MX.SURF$Clay)) #create row for CfB.MX.SURF.AV
CfB.MX.Average.sub.surf = data.frame(rbind(CfB.MX.SUB.AV,CfB.MX.SURF.AV)) #create df of av surf and sub combined
#set sand silt clay columns as numeric
CfB.MX.Average.sub.surf[,3] = as.numeric(as.character( CfB.MX.Average.sub.surf[,3])) #set sand as numeric
CfB.MX.Average.sub.surf[,4] = as.numeric(as.character( CfB.MX.Average.sub.surf[,4])) #set silt as numeric
CfB.MX.Average.sub.surf[,5] = as.numeric(as.character( CfB.MX.Average.sub.surf[,5])) #set clay as numeric
names(CfB.MX.Average.sub.surf) <- names(CfB.MX.subset.ALL)
#Cbind to entire CfB.MX df with CfB SURF SSURGO
CfB.MX.subset.ALL.av = rbind(CfB.MX.subset.ALL, CfB.MX.Average.sub.surf) #all with average!

#symbology and colors --> for CfB.MX subset with average and CfB surf ssurgo
colvec.CfB.MX.av = c(colvec.CfB.MX, "green", "green") #add av CfB.MX sub and surf LLCW both black
pchvec.CfB.MX.av = c(pchvec.CfB.MX, 1, 16) #add symbols for CfB.MX sub (1) and surf (16) average , CfB surf filled diamond (18)
#cex.CfB.MX.av = c(rep(1, length=length(CfB.MX.subset.ALL$Geology)),1.5,1.5) #average has larger symbols
#legend symbology and colors for CfB.MX subset
unique.CfB.MX.Geology.av = sort(as.character(unique(CfB.MX.subset.ALL.av$Geology))) #unique CfB.MX Geology sorted
pchvec.CfB.MX.legend.av = c(1,1,16,16,1,16) #CfB surf filled diamond, open circle SUB, filled circle SURF
colvec.CfB.MX.legend.av = c("green","black","green","black","red","red") #LLCW black, SSURGO red (all but sub5 burlywood3), US blue

#FIgure 4.18
#ternary diagram CfB.MX (with Average as green symbols)
ternaryplot(data.frame(CfB.MX.subset.ALL.av$Sand,CfB.MX.subset.ALL.av$Silt,CfB.MX.subset.ALL.av$Clay), pch=pchvec.CfB.MX.av,col=colvec.CfB.MX.av,main="CfB.MX Ternary Diagram",dimnames=c("Sand","Silt","Clay"))
grid_legend("topright",inset=c(0.01,0),pch=pchvec.CfB.MX.legend.av,col=colvec.CfB.MX.legend.av,labels=unique.CfB.MX.Geology.av)
#CfB.MX sub4 and sub5 are the same texture! (not sure why there was 2 listed?)


################################### CfB.US Texture Diagrams ################################################
###Figure 4.19
#subset the dataframe by Geology_type, take only the CfB samples and put into diagram (no LLCW CfB)

#subset df --> CfB
#index of all the CfB samples to subset df, symbology and colors
CfB.index.ALL.US = grep("CfB.US", texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL$Geology_type)
  cFB.SSURGO.index = grep("^CfB$", texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL$Geology_type) #index SSURGO CfB exact match ^CfB$
CfB.US.subset.ALL = rbind(texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL[CfB.index.ALL.US,], texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL[cFB.SSURGO.index,])

#symbology and colors --> for CfB subset
  colvec.CfB1 = colvec.all.US.MX.final[CfB.index.ALL.US]
colvec.CfB.US = c(colvec.CfB1,"red","red")
  pchvec.CfB1 = pchvec.all.US.MX.final[CfB.index.ALL.US]
pchvec.CfB.US = c(pchvec.CfB1,16,1)
#legend symbology and colors for CfB subset
unique.CfB.Geology = sort(as.character(unique(CfB.US.subset.ALL$Geology))) #unique CfB Geology sorted
pchvec.CfB.legend = c(1,1,16,16) 
colvec.CfB.legend = c("red","blue","red","blue") #LLCW black, SSURGO red (all but sub5 burlywood3), US blue

#ternary diagram CfB
ternaryplot(data.frame(CfB.US.subset.ALL$Sand,CfB.US.subset.ALL$Silt,CfB.US.subset.ALL$Clay),pch=pchvec.CfB.US,col=colvec.CfB.US,main="CfB Ternary Diagram",dimnames=c("Sand","Silt","Clay"))
grid_legend("topright",inset=c(0.01,0),pch=pchvec.CfB.legend,col=colvec.CfB.legend,labels=unique.CfB.Geology)

####Add in averaged LLCW survey data for sub and surf! 

#subsets of CfB SUB and SURF LLCW to get averages for sand/silt/clay 
CfB.SUB = CfB.US.subset.ALL[as.character(CfB.US.subset.ALL$Geology) == "CfB.SUB.US",]
CfB.SURF = CfB.US.subset.ALL[as.character(CfB.US.subset.ALL$Geology) == "CfB.SURF.US",]
#Averages for SUB and SURF
CfB.SUB.AV =  c("CfB", "CfB.SUB.AV", mean(CfB.SUB$Sand), mean(CfB.SUB$Silt), mean(CfB.SUB$Clay)) #create row for CfB.SUB.AV
CfB.SURF.AV =  c("CfB", "CfB.SURF.AV", mean(CfB.SURF$Sand), mean(CfB.SURF$Silt), mean(CfB.SURF$Clay)) #create row for CfB.SURF.AV
CfB.Average.sub.surf = data.frame(rbind(CfB.SUB.AV,CfB.SURF.AV)) #create df of av surf and sub combined
#set sand silt clay columns as numeric
CfB.Average.sub.surf[,3] = as.numeric(as.character( CfB.Average.sub.surf[,3])) #set sand as numeric
CfB.Average.sub.surf[,4] = as.numeric(as.character( CfB.Average.sub.surf[,4])) #set silt as numeric
CfB.Average.sub.surf[,5] = as.numeric(as.character( CfB.Average.sub.surf[,5])) #set clay as numeric
names(CfB.Average.sub.surf) <- names(CfB.SUB)
#add in CbB.SUB ssurgo because looks like it matches averaged data best
CbB.index.ALL = grep("CbB", texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL$Geology_type)
CbB.subset.ALL = texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL[CbB.index.ALL,]
CbB.subset.ALL[CbB.subset.ALL$Geology=="CbB.SUB.SSURGO",]
#Cbind av to entire CfB df, also with CbB 
CfB.US.subset.ALL.CbBSSURGO.av = rbind(CfB.US.subset.ALL, CfB.Average.sub.surf, CbB.subset.ALL[CbB.subset.ALL$Geology=="CbB.SUB.SSURGO",]) #all with average!

#symbology and colors --> for CfB subset with Average and CbB
colvec.CfB.av.CbB = c(colvec.CfB.US, "green", "green", "red") #with 2 CfB averages green, CbB SSURGO
pchvec.CfB.av.CbB = c(pchvec.CfB.US, 1,16,5) #with CfB Sub av 5, CfB surf av 18, CbB 
#legend symbology and colors for CfB subset with average (CbB legend will be separate with CfB SURF SSURGO)
unique.CfB.av.Geology = sort(c(as.character(unique(CfB.US.subset.ALL$Geology)), as.character(CfB.Average.sub.surf$Geology),"CbB.SUB.SSURGO")) #unique CfB Geology sorted with CfB averages
pchvec.CfB.av.legend = c(5,1,1,1,16,16,16) 
colvec.CfB.av.legend = c("red","green","red","blue","green","red","blue") #LLCW black, SSURGO red, US blue (except sub2 blueviolet, sub3 cyan)

###Figure 4.19
#ternary diagram CfB and Av with CbB SSURGO
ternaryplot(data.frame(CfB.US.subset.ALL.CbBSSURGO.av$Sand,CfB.US.subset.ALL.CbBSSURGO.av$Silt,CfB.US.subset.ALL.CbBSSURGO.av$Clay),pch=pchvec.CfB.av.CbB,col=colvec.CfB.av.CbB,main="CfB Ternary Diagram with CbB SSURGO",dimnames=c("Sand","Silt","Clay"))
grid_legend("topright",inset=c(0.01,0),pch=pchvec.CfB.av.legend,col=colvec.CfB.av.legend,labels=unique.CfB.av.Geology)


################################### CbB Texture Diagrams #########################################################################
#Figure 4.20
#subset the dataframe by Geology_type, take only the CbB samples and put into diagram (no LLCW CbB)

#subset df --> CbB
#index of all the CbB samples to subset df, symbology and colors
CbB.index.ALL = grep("CbB", texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL$Geology_type)
CbB.subset.ALL = texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL[CbB.index.ALL,]

#symbology and colors --> for CbB subset
colvec.CbB = colvec.all.US.MX.final[CbB.index.ALL]
pchvec.CbB = pchvec.all.US.MX.final[CbB.index.ALL]
#legend symbology and colors for CbB subset
unique.CbB.Geology = sort(as.character(unique(CbB.subset.ALL$Geology))) #unique CbB Geology sorted
pchvec.CbB.legend = c(5,5,18,18) 
colvec.CbB.legend = c("red","blue","red","blue") #LLCW black, SSURGO red (all but sub5 burlywood3), US blue

#ternary diagram CbB
ternaryplot(data.frame(CbB.subset.ALL$Sand,CbB.subset.ALL$Silt,CbB.subset.ALL$Clay),pch=pchvec.CbB,col=colvec.CbB,main="CbB Ternary Diagram",dimnames=c("Sand","Silt","Clay"))
grid_legend("topright",inset=c(0.01,0),pch=pchvec.CbB.legend,col=colvec.CbB.legend,labels=unique.CbB.Geology)


####Add in averaged LLCW survey data for sub and surf! 

#subsets of CbB SUB and SURF LLCW to get averages for sand/silt/clay 
CbB.SUB = CbB.subset.ALL[as.character(CbB.subset.ALL$Geology) == "CbB.SUB.US",]
CbB.SURF = CbB.subset.ALL[as.character(CbB.subset.ALL$Geology) == "CbB.SURF.US",]
#Averages for SUB and SURF
CbB.SUB.AV =  c("CbB", "CbB.SUB.AV", mean(CbB.SUB$Sand), mean(CbB.SUB$Silt), mean(CbB.SUB$Clay)) #create row for CbB.SUB.AV
CbB.SURF.AV =  c("CbB", "CbB.SURF.AV", mean(CbB.SURF$Sand), mean(CbB.SURF$Silt), mean(CbB.SURF$Clay)) #create row for CbB.SURF.AV
CbB.Average.sub.surf = data.frame(rbind(CbB.SUB.AV,CbB.SURF.AV)) #create df of av surf and sub combined
#set sand silt clay columns as numeric
CbB.Average.sub.surf[,3] = as.numeric(as.character( CbB.Average.sub.surf[,3])) #set sand as numeric
CbB.Average.sub.surf[,4] = as.numeric(as.character( CbB.Average.sub.surf[,4])) #set silt as numeric
CbB.Average.sub.surf[,5] = as.numeric(as.character( CbB.Average.sub.surf[,5])) #set clay as numeric
names(CbB.Average.sub.surf) <- names(CbB.SUB)
#Cbind av to entire CbB df, also with Lf 
CbB.subset.ALL.av = rbind(CbB.subset.ALL, CbB.Average.sub.surf) #all with average!

#symbology and colors --> for CbB subset with Average and Lf
colvec.CbB.av.Lf = c(colvec.CbB, "green", "green") #with 2 CbB averages green
pchvec.CbB.av.Lf = c(pchvec.CbB, 5,18) 
#legend symbology and colors for CbB subset with average 
unique.CbB.av.Geology = sort(c(as.character(unique(CbB.subset.ALL$Geology)), as.character(CbB.Average.sub.surf$Geology))) #unique CbB Geology sorted with CbB averages
pchvec.CbB.av.legend = c(5,5,5,18,18,18) 
colvec.CbB.av.legend = c("green","red","blue","green","red","blue") #LLCW black, SSURGO red, US blue 

#Figure 4.20
#ternary diagram CbB and Av with Lf SSURGO
ternaryplot(data.frame(CbB.subset.ALL.av$Sand,CbB.subset.ALL.av$Silt,CbB.subset.ALL.av$Clay),pch=pchvec.CbB.av.Lf,col=colvec.CbB.av.Lf,main="CbB Ternary Diagram",dimnames=c("Sand","Silt","Clay"))
grid_legend("topright",inset=c(0.01,0),pch=pchvec.CbB.av.legend,col=colvec.CbB.av.legend,labels=unique.CbB.av.Geology)


################################### Lf Texture Diagrams ##############################################################################
###Figure 4.21
#Previously labeled as Hr, now Lf
#subset the dataframe by Geology_type, take only the Lf samples and put into diagram

#subset df --> Lf
#index of all the Lf samples to subset df, symbology and colors
Lf.index.ALL = grep("Lf", texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL$Geology_type)
Lf.subset.ALL = texture.llcw.UStraps1.simple.SSURGO.TJESC.ALL[Lf.index.ALL,]

#symbology and colors --> for Lf subset
colvec.Lf = colvec.all.US.MX.final[Lf.index.ALL]
pchvec.Lf = pchvec.all.US.MX.final[Lf.index.ALL]
#legend symbology and colors for Lf subset
unique.Lf.Geology = sort(as.character(unique(Lf.subset.ALL$Geology))) #unique Lf Geology sorted
pchvec.Lf.legend = c(2,2,2,2,2,17,17,17) #open triangles SUB, filled triangles SURF
colvec.Lf.legend = c("black","blue","blueviolet","cyan","red","black","red","blue") #LLCW black, SSURGO red, US blue (except sub2 blueviolet, sub3 cyan)

#ternary diagram Lf
ternaryplot(data.frame(Lf.subset.ALL$Sand,Lf.subset.ALL$Silt,Lf.subset.ALL$Clay),pch=pchvec.Lf,col=colvec.Lf,main="Lf Ternary Diagram",dimnames=c("Sand","Silt","Clay"))
grid_legend("topright",pch=pchvec.Lf.legend,col=colvec.Lf.legend,labels=unique.Lf.Geology)


####Add in averaged LLCW survey data for sub and surf! and CfB surf ssurgo

#subsets of Lf SUB and SURF LLCW to get averages for sand/silt/clay 
Lf.SUB = Lf.subset.ALL[as.character(Lf.subset.ALL$Geology) == "Lf.SUB.LLCW",]
Lf.SURF = Lf.subset.ALL[as.character(Lf.subset.ALL$Geology) == "Lf.SURF.LLCW",]
#Averages for SUB and SURF
Lf.SUB.AV =  c("Lf", "Lf.SUB.AV", mean(Lf.SUB$Sand), mean(Lf.SUB$Silt), mean(Lf.SUB$Clay)) #create row for Lf.SUB.AV
Lf.SURF.AV =  c("Lf", "Lf.SURF.AV", mean(Lf.SURF$Sand), mean(Lf.SURF$Silt), mean(Lf.SURF$Clay)) #create row for Lf.SURF.AV
Lf.Average.sub.surf = data.frame(rbind(Lf.SUB.AV,Lf.SURF.AV)) #create df of av surf and sub combined
#set sand silt clay columns as numeric
Lf.Average.sub.surf[,3] = as.numeric(as.character( Lf.Average.sub.surf[,3])) #set sand as numeric
Lf.Average.sub.surf[,4] = as.numeric(as.character( Lf.Average.sub.surf[,4])) #set silt as numeric
Lf.Average.sub.surf[,5] = as.numeric(as.character( Lf.Average.sub.surf[,5])) #set clay as numeric
names(Lf.Average.sub.surf) <- names(Lf.subset.ALL)
#Cbind to entire Lf df, also with CfB SURF.SSURGO
Lf.subset.ALL.av = rbind(Lf.subset.ALL, Lf.Average.sub.surf) #all with average!


#symbology and colors --> for Lf subset with Average and Lf
colvec.Lf.av.Lf = c(colvec.Lf, "green", "green") #with 2 Lf averages green
pchvec.Lf.av.Lf = c(pchvec.Lf, 2, 17) #with Lf Sub av 2, Lf surf av 17
#legend symbology and colors for Lf subset with average (LF legend will be separate with CfB SURF SSURGO)
unique.Lf.av.Geology = sort(c(as.character(unique(Lf.subset.ALL$Geology)), as.character(Lf.Average.sub.surf$Geology))) #unique Lf Geology sorted with Lf averages
pchvec.Lf.av.legend = c(2,2,2,2,2,2,17,17,17,17) #open triangles SUB, filled triangles SURF
colvec.Lf.av.legend = c("green","black","blue","blueviolet","cyan","red","green","black","red","blue") #LLCW black, SSURGO red, US blue (except sub2 blueviolet, sub3 cyan)

#Figure 4.21
#ternary diagram Lf with Lf SSURGO
ternaryplot(data.frame(Lf.subset.ALL.av$Sand,Lf.subset.ALL.av$Silt,Lf.subset.ALL.av$Clay),pch=pchvec.Lf.av.Lf,col=colvec.Lf.av.Lf,main="Lf Ternary Diagram",dimnames=c("Sand","Silt","Clay"))
grid_legend("topright",inset=c(0.01,0),pch=pchvec.Lf.av.legend,col=colvec.Lf.av.legend,labels=unique.Lf.av.Geology)




##################################################################################################################################
###############Final Summary table of the average values from LLCW soil samples for each type##########

average.summary = data.frame(rbind(CfB.MX.Average.sub.surf,Lf.Average.sub.surf,CbB.Average.sub.surf,CfB.Average.sub.surf))
#Round the values
average.summary[,3] = round(average.summary[,3],2)
average.summary[,4] = round(average.summary[,4],2)
average.summary[,5] = round(average.summary[,5],2)


write.csv(average.summary, file="C:/Users/Kris/Downloads/ternarydiagram/Update_Ternary_Plots_20171027/LLCW.soil.type.average.csv")


##################################################################################################################################






