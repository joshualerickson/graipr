#Load required libraries
library(foreign) 
library(gdalUtils)
library(rgdal)
library(raster)
library(rgeos)
library(sf)
library(lwgeom)
library(ggplot2)
library(latexpdf)
library(tinytex)
library(tictoc)

tic("Total Run Time")
#ProjectPath<-"Q:/Transfer_Folder/1A_For_NN/fixed/2019"
#ProjectPath<-"Q:/Transfer_Folder/1A_For_NN/fixed/2013"
#ProjectPath<-"H:/Test"
#ProjectPath<-"C:/Work/Clean"
#ProjectPath<-"F:/PreCold"
#ProjectPath<-"C:/Work/Umpqua2020/20200716/ReRun"
#ProjectPath<-"C:/Work/MT_GRAIP/fixed/2013"
#ProjectPath<-"C:/Work/MT_GRAIP/fixed/2019"
#ProjectPath<-"C:/Work/Umpqua2020/FullRun"
#ProjectPath<-"C:/Work/1a_MTphotocheck/MT_strmcheck_shapes/2019"
#ProjectPath<-"C:/Work/Malhuer_Denine_Graip"
ProjectPath<-"G:/PostFire/Boise_Post"
ProjectPath<-"G:/PostFire/MT_Post"
ProjectPath<-"E:/Work/ExportTest"
ProjectPath<-"C:/Work/Umpqua2021/Steelhead_combined"
ProjectPath<-"C:/Work/Umpqua2021/Final_GRAIP_Data_20210830/LittleRiver"
#ProjectPath<-"C:/Work/Umpqua2021/Final_GRAIP_Data_20210830/Steamboat"
ShapesPath<-paste(ProjectPath,"/Shapefiles",sep="")
GridsPath<-paste(ProjectPath,"/Grids",sep="")

setwd(GridsPath)
z<-raster("DEM.tif")
datacrs<-crs(z,asText=TRUE)
setwd(ProjectPath)
ProjectFileList<-list.files(ProjectPath,pattern="_LIST_RAW.csv")


RDFileList<-read.csv("RD_LIST_RAW.csv")
BsRateList<-RDFileList$BaseRate
DPFileList1<-read.csv("DP_LIST_RAW.csv")
DPFileList<-paste(DPFileList1$FileName,".dbf",sep="")
DPTypeList<-DPFileList1$DrainType
DPTypeIDList<-DPFileList1$DrainTypeID
DPList<-paste(DPFileList1$FileName,".shp",sep="")
RDList<-paste(RDFileList$FileName,".shp",sep="")
RDs<-as.character(RDFileList$FileName)
DPs<-as.character(DPFileList1$FileName)

#Set CRS for shapefiles to match DEM

# for(i in 1:length(DPs)){
#   infilename<-DPs[i]
#   outfilename<-DPList[i]
#   temp<-readOGR(dsn = ShapesPath, layer =infilename)#Using Spatial dataframe
#   if(is.na(crs(temp))){
#     crs(temp)<-datacrs
#   }else{
#     spTransform(temp,datacrs)
#   }
#   writeOGR(temp,ShapesPath,infilename,driver = "ESRI Shapefile",overwrite_layer = TRUE)#Using Spatial dataframe
# }
# for(i in 1:length(RDs)){
#   infilename<-RDs[i]
#   outfilename<-RDList[i]
#   temp<-readOGR(dsn = ShapesPath, layer =infilename)#Using Spatial dataframe
#   if(is.na(crs(temp))){
#     crs(temp)<-datacrs
#   }else{
#     spTransform(temp,datacrs)
#   }
#   writeOGR(temp,ShapesPath,infilename,driver = "ESRI Shapefile",overwrite_layer = TRUE)#Using Spatial dataframe
# }

DPIDs<-0


for (i in 1:length(DPFileList)){
  dptype<-DPTypeList[i]
  dptypeID<-DPTypeIDList[i]
  infilename<-paste(ShapesPath,"/",DPFileList[i],sep="")
  dptable<-read.dbf(infilename)
  #Create GDPID
  yr<-substr(dptable$CDATE,3,4)
  numrec<-length(yr)
  mo<-substr(dptable$CDATE,6,7)
  dy<-substr(dptable$CDATE,9,10)
  hr1<-substr(dptable$CTIME,1,2)
  hr2<-rep(NA,length(hr1))
  min<-substr(dptable$CTIME,4,5)
  sec<-substr(dptable$CTIME,7,8)
  ampm<-substr(dptable$CTIME,9,10)
  ve<-as.character(dptable$VEHICLE)
  sec2<-as.numeric(sec)
  minrnd<-rep(NA,length(sec))
  for (ii in 1:length(hr1)){
    if(ampm[ii]=="am"){
      hr2[ii]<-hr1[ii]
    }else{
      if(ampm[ii]=="pm" & hr1[ii]=="12"){
        hr2[ii]<-hr1[ii]
      }else{
        temp1<-as.numeric(hr1[ii])
        temp2<-temp1+12
        hr2[ii]<-as.character(temp2)
      }
    }
  }
  for (ii in 1:length(sec)){
    if(sec2[ii]<30){
      minrnd[ii]<-as.character(min[ii])
    }else{
      temp1<-as.numeric(min[ii])
      temp2<-as.character(temp1+1)
      temp3<-nchar(temp2)
      if(temp3==2){
        minrnd[ii]<-temp2
      }else{
        temp4<-paste("0",temp2,sep="")
        minrnd[ii]<-temp4
      }
    }
  }
  dptable$GDPID<-paste(yr,mo,dy,hr2,minrnd,ve,sep="")
  dptable$DRAINTYPE<-rep(dptype,numrec)
  dptable$DRAINTYPEI<-rep(dptypeID,numrec)
  dptable$File<-rep(DPFileList1$FileName[i],numrec)
  
  if(dptypeID==6){
    dptable$STREAM_CON<-rep("Yes",numrec)
    dptable$DISCHRG_TO<-rep("Stream",numrec)
    dptable$SBI<-rep(NA,numrec)
    dptable$SBI_Flag<-rep(0,numrec)
    dptable$ChanW<-rep(NA,numrec)
    dptable$SkewAngleScore<-rep(1,numrec)
    dptable$PCRatio<-rep(NA,numrec)
    dptable$PCScore<-rep(NA,numrec)
    dptable$PipDim<-rep(NA,numrec)
    dptable$PipDimNum<-rep(NA,numrec)
    dptable$FVatRisk<-rep(NA,numrec)
    
    #Cycle through stream crossing records
    for (iii in 1:numrec){
      
      #Skew Angle Score
      if(is.na(dptable$CHAN_ANGL[iii])==TRUE){
        dptable$SkewAngleScore[iii]<-NA
        dptable$SBI_Flag[iii]<-1
      }else{
        if(dptable$CHAN_ANGL[iii]=="< 25 degrees"){
          dptable$SkewAngleScore[iii]<-0
        }
        
        if(dptable$CHAN_ANGL[iii]=="25-45 degrees"){
          dptable$SkewAngleScore[iii]<-0
        }
      }
      
      #Pipe diameter
      if(is.na(dptable$PIPE_DIA[iii])==TRUE) {
        dptable$SBI_Flag[iii]<-1
        dptable$PIPE_DIA[iii]<-NA
      }else{
        if(dptable$PIPE_DIA[iii]=="N/A"){
          dptable$SBI_Flag[iii]<-1
          dptable$PIPE_DIA[iii]<-NA
          dptable$Dp_m[iii]<-NA
        }else{
          pipnchar<-nchar(as.character(dptable$PIPE_DIA[iii]))
          if(pipnchar==4){
            dptable$PipDim[iii]<-substr(dptable$PIPE_DIA[iii],1,pipnchar-2)
            dptable$Dp_m[iii]<-as.numeric(dptable$PipDim[iii])*2.54/100
          }else{
            if(pipnchar==5){
              dptable$PipDim[iii]<-substr(dptable$PIPE_DIA[iii],3,pipnchar-1)
              dptable$Dp_m[iii]<-as.numeric(dptable$PipDim[iii])*2.54/100
            }else{
              if(pipnchar==3){
                dptable$PipDim[iii]<-substr(dptable$PIPE_DIA[iii],1,pipnchar-1)
                dptable$Dp_m[iii]<-as.numeric(dptable$PipDim[iii])*2.54/100
              }
            }
          }
        }
      }
      
      #Channel Width to inches
      if(is.na(dptable$CHAN_WDTH[iii])==TRUE){
        dptable$CHAN_WDTH[iii]<-"-99"
      }
      if(dptable$CHAN_WDTH[iii]=="-99"){
        dptable$SBI_Flag[iii]<-1
        dptable$Wc_m[iii]<-NA
      }else{
        dptable$ChanW[iii]<-as.numeric(dptable$CHAN_WDTH[iii])*12
        dptable$Wc_m[iii]<-dptable$ChanW[iii]*2.54/100
      }
      
      #Pipe-Channel Ratio
      dptable$PipDimNum[iii]<-as.numeric(dptable$PipDim[iii])
      dptable$PCRatio[iii]<-dptable$PipDimNum[iii]/dptable$ChanW[iii]
      
      #Pipe Length to meters
      if(dptable$PIPE_LEN[iii]=="-99"){
        dptable$Lp_m[iii]<-NA
      }else{
        dptable$Lp_m[iii]<-as.numeric(dptable$PIPE_LEN[iii])*12*2.54/100
      }
      
      #Fill Depth to meters
      if(dptable$FILL_DEPTH[iii]=="-99"){
        dptable$FD_m[iii]<-NA
      }else{
        dptable$FD_m[iii]<-as.numeric(dptable$FILL_DEPTH[iii])*12*2.54/100
      }
      
      #Pipe Grade to radians
      if(dptable$PIPE_GRADE[iii]=="-99"){
        dptable$PG_rad[iii]<-NA
      }else{
        dptable$PG_rad[iii]<-atan(abs(as.numeric(dptable$PIPE_GRADE[iii]))/100)
      }
      
      h2<-(4.9*(dptable$Lp_m[iii]*cos(dptable$PG_rad[iii])-4.9))/(4*dptable$FD_m[iii])
      Au<-4.9*4.9*(dptable$FD_m[iii]*cos(dptable$PG_rad[iii])-4.9)/(8*dptable$FD_m[iii])
      Al<-dptable$FD_m[iii]*(4.9+dptable$Lp_m[iii])/2
      Vc<-Al*1.2*dptable$Wc_m[iii]
      Vt<-1/3*(Au+Al)*((dptable$FD_m[iii]+h2)/3)
      Vu<-Au/9*h2+Au/3*dptable$FD_m[iii]
      Vf<-Vt-Vu
      dptable$FVatRisk[iii]<-Vf+Vc
      
      #if SBI_Flag is 1, fill others as NA, else do following
      if(dptable$SBI_Flag[iii]==1){
        dptable$PCScore[iii]<-NA
      }else{
        if(dptable$PCRatio[iii]>=1){
          dptable$PCScore[iii]<-1
        }else{
          if(dptable$PCRatio[iii]<0.5){
            dptable$PCScore[iii]<-3
          }else{
            dptable$PCScore[iii]<-2
          }
        }
        if(dptable$PCRatio[iii]<0){
          dptable$SBI_Flag[iii]<-1
          dptable$PCScore[iii]<-NA
        }
      }
      
      
      if(dptable$SBI_Flag[iii]==1){
        dptable$SBI[iii]<-NA
      }else{
        dptable$SBI[iii]<-dptable$PCScore[iii]+dptable$SkewAngleScore[iii]
      }
    }
    #PC>=1, score=1; 0.5<=Pipcan<1, score =2; PC<.5,  score=3
    
  }
  
  #Special Handling for Excavated Stream Crossings
  if(dptypeID==9){
    dptable$STREAM_CON<-rep("Yes",numrec)
    dptable$DISCHRG_TO<-rep("Stream",numrec)
    dptable$FILL_EROS<-rep(0,numrec)
  }
  
  if(dptypeID==4){
    dptable$FILL_EROS<-rep(0,numrec)
  }
  
  #Special Handling for Sumps
  if(dptypeID==7){
    dptable$STREAM_CON<-rep("No",numrec)
    dptable$DISCHRG_TO<-rep("Infiltrates",numrec)
    dptable$FILL_EROS<-rep(0,numrec)
  }
  
  #Special Handling for Diffuse Drains
  if(dptypeID==2){
    dptable$CONDIT<-rep("No Problem",numrec)
    dptable$ORPHAN<-rep("No",numrec)
  }
  
  #Generic Drainpoint ID
  DPIDe<-DPIDs+numrec
  dptable$DPID<-seq(DPIDs,DPIDe-1,1)
  DPIDs<-DPIDe
  
  #Create SC
  dptable$SC<-rep(NA,length(yr))
  for(iii in 1:length(dptable$STREAM_CON)){
    if(is.na(dptable$STREAM_CON[iii])==TRUE){
      dptable$SC[iii]<-1
    }else{
      if(dptable$STREAM_CON[iii]=="No"){
        dptable$SC[iii]<-0
      }else{
        dptable$SC[iii]<-1
      }
    }  
  }
  
  for(i in 1:numrec){
    if(is.na(dptable$FILL_EROS[i])==TRUE){
      dptable$FILL_EROS[i]<-0
    }
  }
  
  for(i in 1:numrec){
    if(dptable$FILL_EROS[i]=="Yes"){
      dptable$FILL_EROS[i]<-5
    }else{
      if(dptable$FILL_EROS[i]=="No"){
        dptable$FILL_EROS[i]<-0
      }
    }
  }
  
  dpattout<-data.frame(dptable)
  
  write.dbf(dpattout,infilename)
  
}
#End Drainpoint Attribute Table Changes###########End Drainpoint Attribute Table Changes##########
# toc()
# tic("Error Checking")
#Create Drainpoints.shp############Create Drainpoints.shp############Create Drainpoints.shp###########

# DPList<-c("BBASE_DI.shp","DIFF_DRA.shp","DTCH_REL.shp","EXCAV_ST.shp","LEAD_OFF.shp","MISC_DP.shp",
#           "NON_ENGI.shp","STRM_CRO.shp","SUMP.shp","SWAL_XIN.shp","WATER_BA.shp")
DPList<-paste(DPFileList1$FileName,".shp",sep="")
DrainpointsFieldList<-"File,DPID,GDPID,DRAINTYPE,DRAINTYPEI,SC,CONDIT,ORPHAN,DISCHRG_TO,FILL_EROS"
numdptypes<-length(DPList)

ogr2ogr(paste(ShapesPath,"/",DPList[1],sep=""),paste(ProjectPath,"/","Drainpoints.shp",sep=""),f="ESRI Shapefile",select=DrainpointsFieldList,overwrite=TRUE)#Using Spatial dataframe

for( i in 2:numdptypes){
  ogr2ogr(paste(ShapesPath,"/",DPList[i],sep=""),paste(ProjectPath,"/","Drainpoints.shp",sep=""),f="ESRI Shapefile",append=TRUE,select=DrainpointsFieldList)#Using Spatial dataframe
}

Drainpointsshp<-readOGR(dsn = ProjectPath, layer ="Drainpoints")#Using Spatial dataframe
if(is.na(crs(Drainpointsshp))){
  crs(Drainpointsshp)<-datacrs
}else{
  spTransform(Drainpointsshp,datacrs)
}
writeOGR(Drainpointsshp,ProjectPath,"Drainpoints",driver = "ESRI Shapefile",overwrite_layer = TRUE)#Using Spatial dataframe

Drainpointsshp<-st_read(dsn=ProjectPath,layer="Drainpoints")

#colnames(Drainpointsshp)<-c("DPID","GDPID","DrainType","SC","Condition","Orphan","LinkCascade",
# "DischargeTo")
#st_write(Drainpointsshp,ProjectPath,"Drainpoints")
numdp<-length(Drainpointsshp$DPID)
Drainpointsshp$OrpFlg<-rep(NA,numdp)
Drainpointsshp$OrpFlg1<-rep(NA,numdp)
Drainpointsshp$OrpFlg2<-rep(NA,numdp)
Drainpointsshp$DupFlag<-rep(NA,numdp)
#Drainpointsshp$RdFP1<-rep(NA,numdp)
#Drainpointsshp$RdFP2<-rep(NA,numdp)
#Drainpoints Shapefile for Orphan Analysis Created####Drainpoints Shapefile for Orphan Analysis Created###

#Create Roadlines.shp####################Create Roadlines.shp####################Create Roadlines.shp###
#Setup for multiple road shapefiles

RIDs<-0
#Read As SF st_read()
if(length(RDFileList$FileName)==1){
  infilename<-paste(ShapesPath,"/",RDFileList$FileName[1],".shp",sep="")
  infilename2<-as.character(RDFileList$FileName[1])
  rdtemp<-st_read(dsn=ShapesPath,layer=infilename2)
  rdtemp$BsRate<-BsRateList[1]
  rdtemp$File<-RDFileList$FileName[1]
  nrec<-length(rdtemp$CTIME1)
  RIDe<-RIDs+nrec
  rdtemp$RID<-seq(RIDs,RIDe-1,1)
  st_write(rdtemp,infilename,delete_layer=TRUE)
  ogr2ogr(infilename,"Roadlines.shp")
}else{
  infilename<-paste(ShapesPath,"/",RDFileList$FileName[1],".shp",sep="")
  infilename2<-as.character(RDFileList$FileName[1])
  rdtemp<-st_read(dsn=ShapesPath,layer=infilename2)
  rdtemp$BsRate<-BsRateList[1]
  rdtemp$File<-RDFileList$FileName[1]
  nrec<-length(rdtemp$CTIME1)
  RIDe<-RIDs+nrec
  rdtemp$RID<-seq(RIDs,RIDe-1,1)
  RIDs<-RIDe
  st_write(rdtemp,infilename,delete_layer=TRUE)
  ogr2ogr(infilename,"Roadlines.shp")
  for(i in 2:length(RDFileList$FileName)){
    infilename<-paste(ShapesPath,"/",RDFileList$FileName[i],".shp",sep="")
    infilename2<-as.character(RDFileList$FileName[i])
    rdtemp<-st_read(dsn=ShapesPath,layer=infilename2)
    rdtemp$BsRate<-BsRateList[i]
    rdtemp$File<-RDFileList$FileName[i]
    nrec<-length(rdtemp$CTIME1)
    RIDe<-RIDs+nrec
    rdtemp$RID<-seq(RIDs,RIDe-1,1)
    RIDs<-RIDe
    st_write(rdtemp,infilename,delete_layer=TRUE)
    ogr2ogr(infilename,"Roadlines.shp",append=TRUE)
  }
}

ROAD<-readOGR(dsn = ProjectPath, layer ="Roadlines")#Using Spatial dataframe
if(is.na(crs(ROAD))){
  crs(ROAD)<-datacrs
}else{
  spTransform(ROAD,datacrs)
}
writeOGR(ROAD,ProjectPath,"Roadlines",driver = "ESRI Shapefile",overwrite_layer = TRUE)#Using Spatial dataframe

#Roadlines
ROAD<-st_read(dsn=ProjectPath,layer="Roadlines")

ROAD$Len_m<-st_length(ROAD)
#Roadline flowpath GRAIPDrainIDs
yrrd<-substr(ROAD$CDATE,3,4)
mord<-substr(ROAD$CDATE,6,7)
dyrd<-substr(ROAD$CDATE,9,10)
verd<-as.character(ROAD$VEHICLE)
fp1ctime<-as.character(ROAD$CTIME1)
fp2ctime<-as.character(ROAD$CTIME2)
ROAD$FP1DID<-paste(yrrd,mord,dyrd,fp1ctime,verd,sep="")
ROAD$FP2DID<-paste(yrrd,mord,dyrd,fp2ctime,verd,sep="")

#Create fields for GRAIPDrainIDs
numrecrd<-length(fp1ctime)
ROAD$FP1VF<-rep(NA,numrecrd)
ROAD$FP2VF<-rep(NA,numrecrd)
ROAD$SfcFact<-rep(NA,numrecrd)
#ROAD$FP1DID<-rep(NA,numrecrd)
ROAD$FP1Orph<-rep(NA,numrecrd)
ROAD$FP1SC<-rep(NA,numrecrd)
#ROAD$FP2DID<-rep(NA,numrecrd)
ROAD$FP2Orph<-rep(NA,numrecrd)
ROAD$FP2SC<-rep(NA,numrecrd)

#Test for Orphan Road Segments by Flowpath
for (i in 1:numrecrd){
  if (ROAD$FP1DID[i] %in% Drainpointsshp$GDPID){
    dptemp<-which(Drainpointsshp$GDPID == ROAD$FP1DID[i])
    ROAD$FP1SC[i]<-Drainpointsshp$SC[dptemp]
    ROAD$FP1Orph[i]<-"NOT Orphan"
  }else{
    ROAD$FP1Orph[i]<-"FP1 Orphan"
  }
}
for (i in 1:numrecrd){
  if (ROAD$FP2DID[i] %in% Drainpointsshp$GDPID){
    dptemp<-which(Drainpointsshp$GDPID == ROAD$FP2DID[i])
    ROAD$FP2SC[i]<-Drainpointsshp$SC[dptemp]
    ROAD$FP2Orph[i]<-"NOT Orphan"
  }else{
    ROAD$FP2Orph[i]<-"FP2 Orphan"
  }
}

#Create Orphan Road Segments list
fp1Orph<-which(ROAD$FP1Orph=="FP1 Orphan")
fp2Orph<-which(ROAD$FP2Orph=="FP2 Orphan")
FP1OrphTable<-data.frame(ROAD$File[fp1Orph],ROAD$RID[fp1Orph],ROAD$FP1Orph[fp1Orph])
colnames(FP1OrphTable)<-c("File","RID","Orphan")
FP2OrphTable<-data.frame(ROAD$File[fp2Orph],ROAD$RID[fp2Orph],ROAD$FP2Orph[fp2Orph])
colnames(FP2OrphTable)<-c("File","RID","Orphan")
RoadOrphTable<-rbind(FP1OrphTable,FP2OrphTable)
RoadOrphTableSorted<-RoadOrphTable[order(RoadOrphTable$RID),]
if(length(RoadOrphTable$RID)==0){
  NoOrphRD<-"No Orphan Road Segments"
  write.csv(NoOrphRD,"NoOrphanRoadSegments.csv")
}else{
  write.csv(RoadOrphTableSorted,"SortedOrphanRoadSegments.csv")
}
#Test for Orphan Drainpoints
for(i in 1:numdp){
  fp1<-which(ROAD$FP1DID==Drainpointsshp$GDPID[i])
  fp2<-which(ROAD$FP2DID==Drainpointsshp$GDPID[i])
  if(length(fp1)==0){
    #Drainpointsshp$RdFP1[i]<-NA
    Drainpointsshp$OrpFlg1[i]<-"Orphan"
  }else{
    Drainpointsshp$OrpFlg1[i]<-"Not Orphan"
    ROAD$FP1SC[fp1]<-Drainpointsshp$SC[i]
    #Drainpoints$RdFP1[i]<-paste(ROAD$RID[fp1],sep=",") 
  }
  if(length(fp2)==0){
    #Drainpointsshp$RdFP2[i]<-NA
    Drainpointsshp$OrpFlg2[i]<-"Orphan"
  }else{
    Drainpointsshp$OrpFlg2[i]<-"Not Orphan"
    ROAD$FP2SC[fp2]<-Drainpointsshp$SC[i]
    #Drainpoints$RdFP2[i]<-paste(ROAD$RID[fp2],sep=",") 
  }
  if(Drainpointsshp$OrpFlg1[i]=="Orphan" & Drainpointsshp$OrpFlg2[i]=="Orphan"){
    Drainpointsshp$OrpFlg[i]<-"Orphan"
  }else{
    Drainpointsshp$OrpFlg[i]<-"Not Orphan"
  }
  
}

#Create Orphan Drainpoint List
OrphDPList<-which(Drainpointsshp$OrpFlg=="Orphan")
if(length(OrphDPList)==0){
  NoOrph<-"No orhan drainpoints found"
  write.csv(NoOrph,"NoOrphanDrainpoints.csv")
}else{
  OrphDPTable<-data.frame(Drainpointsshp$File[OrphDPList],Drainpointsshp$DPID[OrphDPList],Drainpointsshp$GDPID[OrphDPList],
                          Drainpointsshp$DRAINTYPE[OrphDPList],Drainpointsshp$OrpFlg[OrphDPList],
                          Drainpointsshp$ORPHAN[OrphDPList])
  colnames(OrphDPTable)<-c("File","DPID","GDPID","DrainpointType","Orphan","Orphan Check")
  write.csv(OrphDPTable,"OrphanDrainpoints.csv")
}

#Test for Not Orphan Drainpoints
FalseParentList<-which(Drainpointsshp$ORPHAN=="Yes"& Drainpointsshp$OrpFlg=="Not Orphan")
if(length(FalseParentList)==0){
  NFP<-"No false parents found"
  write.csv(NFP,"NoFalseParents.csv")
}else{
  FalseParentTable<-data.frame(Drainpointsshp$File[FalseParentList],Drainpointsshp$DPID[FalseParentList],Drainpointsshp$GDPID[FalseParentList],
                               Drainpointsshp$DRAINTYPE[FalseParentList],Drainpointsshp$OrpFlg[FalseParentList],
                               Drainpointsshp$ORPHAN[FalseParentList])
  colnames(FalseParentTable)<-c("File","DPID","GDPID","DrainpointType","Orphan","Orphan Check")
  write.csv(FalseParentTable,"FalseParents.csv")
}
#Test for Duplicate Drainpoints
for(i in 1:numdp){
  DupList1<-which(Drainpointsshp$GDPID==Drainpointsshp$GDPID[i])
  if(length(DupList1)>1){
    Drainpointsshp$DupFlag[DupList1]<-"Duplicate GDPID"
  }
}
DupList2<-which(Drainpointsshp$DupFlag=="Duplicate GDPID")
if(length(DupList2)==0){
  NoDup<-"There are no duplicate drainpoints found in this data set"
  write.csv(NoDup,"NoDuplicateDrains.csv")
}else{
  DupTable<-data.frame(Drainpointsshp$File[DupList2],Drainpointsshp$DPID[DupList2],Drainpointsshp$GDPID[DupList2],Drainpointsshp$DRAINTYPE[DupList2],
                       Drainpointsshp$DupFlag[DupList2])
  colnames(DupTable)<-c("File","DPID","GDPID","DrainpointType","Duplicate")
  DupTable2<-DupTable[order(DupTable$GDPID),]
  write.csv(DupTable2,"DuplicateDrains.csv")
}
#Write roadlines and drainpoint shapefiles: Field Names Change when shapefiles written
st_write(Drainpointsshp,paste(ProjectPath,"/Drainpoints.shp",sep=""),delete_layer=TRUE)
st_write(ROAD,paste(ProjectPath,"/Roadlines.shp",sep=""),delete_layer=TRUE)
#End Orphan Error Checking###############End Orphan Error Checking##############


# tic("TauDEM run")
setwd(GridsPath)

Eres<-(z@extent@xmax-z@extent@xmin)/z@ncols
#Nres(z@extent@ymax-z@extent@ymin)/z@nrows
if(Eres<=5){
  writeRaster(z,paste(GridsPath,"/DEMhr.tif",sep=""),"GTiff",datatype="FLT4S",overwrite=TRUE)
  gdalwarp("DEMhr.tif","DEM.tif",tr=c(10,10),r="cubic",overwrite=TRUE)
  z=raster("DEM.tif")
}else{
  gdalwarp("DEM.tif","DEMhr.tif",tr=c(5,5),r="cubic",overwrite=TRUE)
}
#Pit Removal
system("mpiexec -n 8 pitremove -z DEM.tif -fel DEMfel.tif")


#D8 Flow Directions
system("mpiexec -n 8 D8Flowdir -p DEMp.tif -sd8 DEMsd8.tif -fel DEMfel.tif")


#D8 Contributing Area
system("mpiexec -n 8 AreaD8 -p DEMp.tif -ad8 DEMad8.tif -nc")


#Grid Network
system("mpiexec -n 8 Gridnet -p DEMp.tif -gord DEMgord.tif -plen DEMplen.tif -tlen DEMtlen.tif")


#Dinf Flow Directions
system("mpiexec -n 8 DinfFlowdir -ang DEMang.tif -slp DEMslp.tif -fel DEMfel.tif")


#Dinf Contributing Area
system("mpiexec -n 8 AreaDinf -ang DEMang.tif -sca DEMsca.tif -nc")


#Use Peuker Douglas Stream Delineation, maybe set threshold based on DEM resoluton?
system("mpiexec -n 8 PeukerDouglas -fel DEMfel.tif -ss DEMss.tif -par 0.4 0.1 0.05")

system("mpiexec -n 8 AreaD8 -p DEMp.tif -ad8 DEMssa.tif -wg DEMss.tif -nc")

system("mpiexec -n 8 Threshold -ssa DEMssa.tif -src DEMsrc.tif -thresh 25")


#Initial Stream Reach and Watershed
system("mpiexec -n 8 Streamnet -fel DEMfel.tif -p DEMp.tif -ad8 DEMad8.tif -src DEMsrc.tif -ord DEMord.tif -tree DEMtree.txt -coord DEMcoord.txt -net DEMnet.shp -w DEMw.tif")


#Distance to Stream
system("mpiexec -n 8 D8HDistToStrm -p DEMp.tif -src DEMsrc.tif -dist DEMdist.tif")


#Slope Averaged Down
system("mpiexec -n 8 SlopeAveDown -p DEMp.tif -fel DEMfel.tif -slpd DEMslpd.tif")

#Set DEMnet CRS
temp<-readOGR(dsn = GridsPath, layer ="DEMnet")#Using Spatial dataframe
if(is.na(crs(temp))){
  crs(temp)<-datacrs
}else{
  spTransform(temp,datacrs)
}
writeOGR(temp,GridsPath,"DEMnet",driver = "ESRI Shapefile",overwrite_layer = TRUE)#Using Spatial dataframe

#Make Road/Stream Intersection Points
ROAD <- readOGR(dsn = ProjectPath, layer ="Roadlines")#Using Spatial dataframe
STRM <- readOGR(dsn=GridsPath,layer="DEMnet")#Using Spatial dataframe
RSI2<-gIntersection(ROAD,STRM)
RSI_Coords<-coordinates(RSI2)
Easting<-RSI_Coords[,1]
Northing<-RSI_Coords[,2]
ID<-seq(0,length(Northing)-1,1)
RSI_ATT<-data.frame(ID,Easting,Northing)
colnames(RSI_ATT)<-c("ID","Easting","Northing")


#Get stream reach outlets DS
STRM2<-st_read(dsn=GridsPath,layer="DEMnet")
#st_Transform(STRM2,datacrs)
DS<-st_sf(st_startpoint(STRM2))
st_write(DS,"DS.shp",delete_layer=TRUE)
DS2<-readOGR(dsn=GridsPath,layer="DS")
DS_Coords<-coordinates(DS2)
DS_Easting<-DS_Coords[,1]
DS_Northing<-DS_Coords[,2]
DS_ID<-seq(length(Northing),length(Northing)+length(DS_Easting)-1,1)
DS_ATT<-data.frame(DS_ID,DS_Easting,DS_Northing)
colnames(DS_ATT)<-c("ID","Easting","Northing")


#Make Outlets.shp
Outlets_ATT<-rbind(RSI_ATT,DS_ATT)
colnames(Outlets_ATT)<-c("ID","Easting","Northing")
Outlets<-SpatialPointsDataFrame(Outlets_ATT[,2:3],Outlets_ATT,proj4string = ROAD@proj4string)
writeOGR(Outlets,GridsPath,"Outlets",driver = "ESRI Shapefile",overwrite_layer = TRUE)#Using Spatial dataframe


#Final Stream Reach and Watershed
system("mpiexec -n 8 Streamnet -fel DEMfel.tif -p DEMp.tif -ad8 DEMad8.tif -src DEMsrc.tif -ord DEMord.tif -tree DEMtree.txt -coord DEMcoord.txt -net DEMnet.shp -w DEMw.tif -o Outlets.shp")


z=raster("DEM.tif")
zslope<-terrain(z,opt="slope",unit="radians")
zaspect<-terrain(z,opt="aspect",unit="radians")
zhs<-hillShade(zslope,zaspect,normalize=TRUE,angle=45)
crs(zhs)<-datacrs
writeRaster(zhs,filename = "DEMhs.tif",format="GTiff",overwrite=TRUE)

Streams<-st_read(dsn=GridsPath,layer="DEMnet")
#st_Transform(Streams,datacrs)
setwd(ProjectPath)
ROAD<-st_read(dsn=ProjectPath,layer="Roadlines")


#toc()

#tic("GRAIP Model Run")
#ROAD<-st_read(dsn=ProjectPath,layer="Roadlines")
#st_Transform(ROAD,datacrs)
Drainpointsshp<-st_read(dsn=ProjectPath,layer="Drainpoints")
#st_Transform(Drainpointsshp,datacrs)

StartPoints<-st_sf(st_startpoint(ROAD))
EndPoints<-st_sf(st_endpoint(ROAD))
#Read Raster
DEM<-raster(paste(GridsPath,"/DEMhr.tif",sep=""))
#Extract Endpoint Values - Does this work on simple features?
ROAD$StElev<-extract(DEM,StartPoints)
ROAD$EndElev<-extract(DEM,EndPoints)
#rm(DEM)

#Populate Surface Factors
# numrecrd<-length(fp1ctime)
for (i in 1:numrecrd){
  if (ROAD$SURF_TY[i]=="Native"){
    ROAD$SfcFact[i]<-5
  }
}
for (i in 1:numrecrd){
  if (ROAD$SURF_TY[i]=="Crushed rock"){
    ROAD$SfcFact[i]<-1
  }
}
for (i in 1:numrecrd){
  if (ROAD$SURF_TY[i]=="Crushed Rock"){
    ROAD$SfcFact[i]<-1
  }
}
for (i in 1:numrecrd){
  if (ROAD$SURF_TY[i]=="Paved"){
    ROAD$SfcFact[i]<-0.2
  }
}

#Populate Flowpath Vegetation Factors
for (i in 1:numrecrd){
  if (ROAD$FLWPTH_VG1[i]=="< 25%"){
    ROAD$FP1VF[i]<-1
  }
}
for (i in 1:numrecrd){
  if (ROAD$FLWPTH_VG1[i]==">25%"){
    ROAD$FP1VF[i]<-0.14
  }
}
for (i in 1:numrecrd){
  if (ROAD$FLWPTH_VG2[i]=="< 25%"){
    ROAD$FP2VF[i]<-1
  }
}
for (i in 1:numrecrd){
  if (ROAD$FLWPTH_VG2[i]==">25%"){
    ROAD$FP2VF[i]<-0.14
  }
}
#Sediment Production Calculations
ROAD$Range<-abs(ROAD$EndElev-ROAD$StElev)
ROAD$SP_FP1<-0.5*ROAD$BsRate*ROAD$SfcFact*ROAD$FP1VF*ROAD$Range
ROAD$SP_FP2<-0.5*ROAD$BsRate*ROAD$SfcFact*ROAD$FP2VF*ROAD$Range


#Recontoured road sediment production
#Only applies to sections with Surface condition as Recontoured AND Diff/Diff
#Buffer only recontoured sections, pull mean slp value for each and use to calculate surface area sedprod
slpgradient<-seq(0,0.8,0.1)
slpwidth_m<-c(4.9,5.2,6.3,8.3,12.0,19.8,41.0,62.4,130.9)
slpwidthtable<-data.frame(slpgradient,slpwidth_m)
slpgrad<-seq(0,1.5,0.05)
slpBR<-c(0.00,0.00,0.02,0.46,0.90,1.34,1.78,2.23,2.67,3.11,3.55,3.99,4.43,4.88,5.32,5.76,6.20,6.64,7.08,7.53,7.97,8.41,8.85,9.29,9.73,10.18,10.62,11.06,11.50,11.94,12.38)

Recon<-which(ROAD$SURF_COND=="Recontoured"&ROAD$FLOW_PATH1=="Diffuse"&ROAD$FLOW_PATH2=="Diffuse")
if(length(Recon)!=0){
  slp<-raster(paste(GridsPath,"/DEMslp.tif",sep=""))
  for(i in 1:length(Recon)){
    segid<-Recon[i]
    segsurf<-as.character(ROAD$SURF_TYPE[segid])
   if(segsurf=="Native"){
      ROAD$SfcFact[segid]<-1
    }else{
      if(segsurf=="Paved"){
        ROAD$SfcFact[segid]<-0.04
      }else{
        ROAD$SfcFact[segid]<-0.2
      }
    }
    segbuffer<-st_buffer(ROAD[segid,],15,endCapStyle="FLAT")
    segslp<-extract(slp,segbuffer,fun=mean)
    #Calculate width
    if(segslp>0.8){
      segwdth<-150
    }else{
      segslp2<-segslp[1,1]
      ly<-slpwidth_m[max(which(slpgradient<segslp2))]
      uy<-slpwidth_m[min(which(slpgradient>=segslp2))]
      lx<-slpgradient[max(which(slpgradient<segslp2))]
      ux<-slpgradient[min(which(slpgradient>=segslp2))]
      segwdth<-(uy-ly)/0.1*(segslp2-lx)+ly
    }
    #Calculate Base rate
    if(segslp>0.8){
      segBR<-15*(ROAD$BsRate[segid]/79)
    }else{
      segslp2<-segslp[1,1]
      ly2<-slpBR[max(which(slpBR<segslp2))]
      uy2<-slpBR[min(which(slpBR>=segslp2))]
      lx2<-slpgrad[max(which(slpgrad<segslp2))]
      ux2<-slpgrad[min(which(slpgrad>=segslp2))]
      segBR<-(ROAD$BsRate[segid]/79)*(uy2-ly2)/0.1*(segslp2-lx2)+ly2
    }
    ROAD$SP_FP1[segid]<-0.5*segBR*segwdth*ROAD$Len_m[segid]*ROAD$SfcFact[segid]*ROAD$FP1VF[segid]
    ROAD$SP_FP2[segid]<-0.5*segBR*segwdth*ROAD$Len_m[segid]*ROAD$SfcFact[segid]*ROAD$FP2VF[segid]
  }
}

ROAD$TS_Prod<-ROAD$SP_FP1+ROAD$SP_FP2
ROAD$SD_FP1<-rep(NA,numrecrd)
ROAD$SD_FP2<-rep(NA,numrecrd)
#Road Sediment Delivery (Drainpoint Processing Part 1 must be run prior to this step)
for(i in 1:numrecrd){
  fp1<-which(ROAD$FP1DID[i]==Drainpointsshp$GDPID)
  fp2<-which(ROAD$FP2DID[i]==Drainpointsshp$GDPID)
  # fp1<-which(Drainpointsshp$GDPID[i]==ROAD$FP1DID)
  # fp2<-which(Drainpointsshp$GDPID[i]==ROAD$FP2DID)
  if(length(fp1)!=0){
    ROAD$FP1SC[i]<-Drainpointsshp$SC[fp1]
    #Drainpoints$RdFP1[i]<-paste(ROAD$RID[fp1],sep=",") 
  }
  if(length(fp2)!=0){
    ROAD$FP2SC[i]<-Drainpointsshp$SC[fp2]
    #Drainpoints$RdFP1[i]<-paste(ROAD$RID[fp1],sep=",") 
  }
  if(ROAD$FP1Orph[i]=="FP1 Orphan"){
    ROAD$SD_FP1[i]<-0
  }else{
    ROAD$SD_FP1[i]<-ROAD$SP_FP1[i]*ROAD$FP1SC[i]
  }
  if(ROAD$FP2Orph[i]=="FP2 Orphan"){
    ROAD$SD_FP2[i]<-0
  }else{
    ROAD$SD_FP2[i]<-ROAD$SP_FP2[i]*ROAD$FP2SC[i]
  }
} 

ROAD$TS_Del<-ROAD$SD_FP1+ROAD$SD_FP2
st_write(ROAD,paste(ProjectPath,"/Roadlines.shp",sep=""),delete_layer=TRUE)

#Drainpoint Processing Part 2

numdp<-length(Drainpointsshp$DPID)
Drainpointsshp$SedProd<-rep(NA,numdp)
Drainpointsshp$SP_FP1<-rep(NA,numdp)
Drainpointsshp$SP_FP2<-rep(NA,numdp)
Drainpointsshp$E_Len<-rep(NA,numdp)
Drainpointsshp$E_Len1<-rep(NA,numdp)
Drainpointsshp$E_Len2<-rep(NA,numdp)
Drainpointsshp$U_S_Prod<-rep(NA,numdp)
Drainpointsshp$SedDel<-rep(NA,numdp)
Drainpointsshp$U_S_Del<-rep(NA,numdp)
Drainpointsshp$FP1Gullycft<-rep(0,numdp)
Drainpointsshp$FP2Gullycft<-rep(0,numdp)
Drainpointsshp$FPGullycft<-rep(0,numdp)


for(i in 1:numdp){
  fp1<-which(Drainpointsshp$GDPID[i]==ROAD$FP1DID)
  fp2<-which(Drainpointsshp$GDPID[i]==ROAD$FP2DID)
  
  if(length(fp1)==0&length(fp2)==0){#If drainpoint is an orphan
    Drainpointsshp$SP_FP1[i]<-0
    Drainpointsshp$SP_FP2[i]<-0
    Drainpointsshp$SedProd[i]<-0
    Drainpointsshp$E_Len1[i]<-0
    Drainpointsshp$E_Len2[i]<-0
    Drainpointsshp$E_Len[i]<-0
    Drainpointsshp$U_S_Prod[i]<-0
    Drainpointsshp$SedDel[i]<-0
    Drainpointsshp$U_S_Del[i]<-0
  }else{
    Drainpointsshp$SP_FP1[i]<-sum(ROAD$SP_FP1[fp1])
    Drainpointsshp$SP_FP2[i]<-sum(ROAD$SP_FP2[fp2])
    Drainpointsshp$SedProd[i]<-Drainpointsshp$SP_FP1[i]+Drainpointsshp$SP_FP2[i]
    Drainpointsshp$E_Len1[i]<-sum(ROAD$Len_m[fp1])*0.5
    Drainpointsshp$E_Len2[i]<-sum(ROAD$Len_m[fp2])*0.5
    Drainpointsshp$E_Len[i]<-Drainpointsshp$E_Len1[i]+Drainpointsshp$E_Len2[i]
    Drainpointsshp$U_S_Prod[i]<-Drainpointsshp$SedProd[i]/Drainpointsshp$E_Len[i]
    Drainpointsshp$SedDel[i]<-Drainpointsshp$SedProd[i]*Drainpointsshp$SC[i]
    Drainpointsshp$U_S_Del[i]<-Drainpointsshp$SedDel[i]/Drainpointsshp$E_Len[i]
    Drainpointsshp$FP1Gullycft[i]<-sum(as.numeric(ROAD$FP1_GULLY[fp1]))
    Drainpointsshp$FP2Gullycft[i]<-sum(as.numeric(ROAD$FP2_GULLY[fp2]))
    Drainpointsshp$FPGullycft[i]<-Drainpointsshp$FP1Gullycft[i]+Drainpointsshp$FP2Gullycft[i]
  }
}

#################################################################################################################################

#ESI Calculation
aveslp<-raster(paste(GridsPath,"/DEMslpd.tif",sep=""))
Drainpointsshp$Slope<-extract(aveslp,Drainpointsshp)
Drainpointsshp$ESI<-Drainpointsshp$E_Len*Drainpointsshp$Slope^2

#D8 Distance to Stream
strmdist<-raster(paste(GridsPath,"/DEMdist.tif",sep=""))
Drainpointsshp$STRMDST<-extract(strmdist,Drainpointsshp)

#rm(c(aveslp,strmdist))
DPshpFieldNames<-colnames(Drainpointsshp)
st_write(Drainpointsshp,paste(ProjectPath,"/Drainpoints.shp",sep=""),delete_layer=TRUE)#Abbreviates field names

#Drainpoints$SedDel to raster
z<-raster(paste(GridsPath,"/DEM.tif",sep=""))
cellarea_m2<-(z@extent@xmax-z@extent@xmin)/z@ncols*(z@extent@ymax-z@extent@ymin)/z@nrows
Drains<-readOGR(dsn = ProjectPath, layer ="Drainpoints")#Using Spatial dataframe
colnames(Drains@data)<-DPshpFieldNames[c(1:14,16:30)]
writeOGR(Drains,ProjectPath,"Drainpoints",driver = "ESRI Shapefile",overwrite_layer = TRUE)#Using Spatial dataframe
swt<-rasterize(Drains,z,Drains$SedDel,fun=sum,background=0)
writeRaster(swt,paste(GridsPath,"/DEMswt.tif",sep=""),"GTiff",datatype="FLT4S",overwrite=TRUE)
setwd(GridsPath)
system("mpiexec -n 8 AreaD8 -p DEMp.tif -ad8 DEMsed.tif -wg DEMswt.tif -nc")
demnet<-st_read(dsn=GridsPath,layer="DEMnet")
#st_Transform(demnet,datacrs)
demnetpoints<-st_sf(st_cast(st_line_sample(demnet,sample = 0.5),"POINT"))
ad8<-raster(paste(GridsPath,"/DEMad8.tif",sep=""))
sedkg<-raster(paste(GridsPath,"/DEMsed.tif",sep=""))
demnet$ContAm2<-cellarea_m2*extract(ad8,demnetpoints)
demnet$ConAKm2<-demnet$ContAm2/1000000
demnet$SedKg<-extract(sedkg,demnetpoints)
demnet$SedAcMg<-demnet$SedKg/1000
demnet$SpecSed<-demnet$SedAcMg/demnet$ConAKm2
#toc()

#tic("Mass Wasting Analysis")
#Drains<-readOGR(dsn = ProjectPath, layer ="Drainpoints")
Drains$FE_kg<-1500/(3.28^3)*as.numeric(Drains$FILL_EROS)
Drains$FE_kg_del<-Drains$FE_kg*Drains$SC
feswt<-rasterize(Drains,z,Drains$FE_kg_del,fun=sum,background=0)
writeRaster(feswt,paste(GridsPath,"/DEMfeswt.tif",sep=""),"GTiff",datatype="FLT4S",overwrite=TRUE)
setwd(GridsPath)
system("mpiexec -n 8 AreaD8 -p DEMp.tif -ad8 DEMfesed.tif -wg DEMfeswt.tif -nc")
fekg<-raster(paste(GridsPath,"/DEMfesed.tif",sep=""))
demnet$FE_kg<-extract(fekg,demnetpoints)
demnet$FE_Mg<-demnet$FE_kg/1000
demnet$SpecSedFE<-demnet$FE_Mg/demnet$ConAKm2
setwd(ProjectPath)
#st_write(demnet,paste(ProjectPath,"/Streams.shp",sep=""),delete_layer=TRUE)

Drains$FPG_kg<-1500/(3.28^3)*Drains$FPGullycft
Drains$FPG_kg_del<-Drains$FPG_kg*Drains$SC
feswt<-rasterize(Drains,z,Drains$FPG_kg_del,fun=sum,background=0)
writeRaster(feswt,paste(GridsPath,"/DEMfpgswt.tif",sep=""),"GTiff",datatype="FLT4S",overwrite=TRUE)
setwd(GridsPath)
system("mpiexec -n 8 AreaD8 -p DEMp.tif -ad8 DEMfpgsed.tif -wg DEMfpgswt.tif -nc")
fpgkg<-raster(paste(GridsPath,"/DEMfpgsed.tif",sep=""))
demnet$FPG_kg<-extract(fpgkg,demnetpoints)
demnet$FPG_Mg<-demnet$FPG_kg/1000
demnet$SpecSedFPG<-demnet$FPG_Mg/demnet$ConAKm2
setwd(ProjectPath)
st_write(demnet,paste(ProjectPath,"/Streams.shp",sep=""),delete_layer=TRUE)
writeOGR(Drains,ProjectPath,"Drainpoints",driver = "ESRI Shapefile",overwrite_layer = TRUE)#Using Spatial dataframe

#Gullies
if("GY_LIST_RAW.csv" %in% ProjectFileList){
  GYFileList<-read.csv("GY_LIST_RAW.csv")
  GYs<-as.character(GYFileList$FileName)
  GYList<-paste(GYFileList$FileName,".shp",sep="")
  
  #Set CRS for shapefiles to match DEM
  for(i in 1:length(GYs)){
    infilename<-GYs[i]
    outfilename<-GYList[i]
    temp<-readOGR(dsn = ShapesPath, layer =infilename)#Using Spatial dataframe
    if(is.na(crs(temp))){
      crs(temp)<-datacrs
    }else{
      spTransform(temp,datacrs)
    }
    # if("STREAM_CON" %in% colnames(temp)){
    #   print(paste("STREAM_CON exists in",outfilename))
    # }else{
    #   temp$STREAM_CON<-rep("Unknown",length(temp$CDATE))
    # }
    writeOGR(temp,ShapesPath,infilename,driver = "ESRI Shapefile",overwrite_layer = TRUE)#Using Spatial dataframe
  }
  
  GYFieldList<-"ACTIVITY,LOCATION,LENGTH,WIDTH,DEPTH,STREAM_CON,CDATE,VEHICLE,COMMENT,CTIME1,CTIME2"
  numGY<-length(GYList)
  ogr2ogr(paste(ShapesPath,"/",GYList[1],sep=""),paste(ProjectPath,"/","Gullies.shp",sep=""),f="ESRI Shapefile",select=GYFieldList,overwrite=TRUE)
  for( i in 2:numGY){
    ogr2ogr(paste(ShapesPath,"/",GYList[i],sep=""),paste(ProjectPath,"/","Gullies.shp",sep=""),f="ESRI Shapefile",append=TRUE,select=GYFieldList)
  }
  
  Gullies<-st_read(dsn=ProjectPath,layer="Gullies")
  #st_Transform(Gullies,datacrs)
  Gullies$Vol_m3<-0.5*as.numeric(Gullies$LENGTH)*as.numeric(Gullies$WIDTH)*as.numeric(Gullies$DEPTH)/(3.28^3)
  Gullies$Mass_kg<-1500*Gullies$Vol_m3
  
  #Create GDPID numbers
  yrrd<-substr(Gullies$CDATE,3,4)
  mord<-substr(Gullies$CDATE,6,7)
  dyrd<-substr(Gullies$CDATE,9,10)
  verd<-as.character(Gullies$VEHICLE)
  fp1ctime<-as.character(Gullies$CTIME1)
  fp2ctime<-as.character(Gullies$CTIME2)
  Gullies$GDPID1<-paste(yrrd,mord,dyrd,fp1ctime,verd,sep="")
  Gullies$GDPID2<-paste(yrrd,mord,dyrd,fp2ctime,verd,sep="")
  Gullies$GDPID1SC<-rep(0,length(Gullies$CDATE))
  Gullies$GDPID2SC<-rep(0,length(Gullies$CDATE))
  Gullies$OSC<-rep(0,length(Gullies$CDATE))
  Gullies$SC<-rep(0,length(Gullies$CDATE))
  
  #Check drainpoint stream connection
  for(i in 1:length(Gullies$CDATE)){
    dp1<-which(Gullies$GDPID1[i]==Drainpointsshp$GDPID)
    dp2<-which(Gullies$GDPID2[i]==Drainpointsshp$GDPID)
    if(length(dp1)!=0){
      Gullies$GDPID1SC[i]<-Drainpointsshp$SC[dp1]
    }
    if(length(dp2)!=0){
      Gullies$GDPID2SC[i]<-Drainpointsshp$SC[dp2]
    }
    if("STREAM_CON" %in% colnames(Gullies)){
      if(Gullies$STREAM_CON[i]=="Yes"){
        Gullies$OSC[i]<-1
      }  
    }
    sc_check<-sum(Gullies$GDPID1SC[i],Gullies$GDPID2SC[i],Gullies$OSC[i])
    if(sc_check>0){
      Gullies$SC[i]<-1
    }
  } 
  
  Gullies$SedDel<-Gullies$SC*Gullies$Mass_kg
  GYswt<-rasterize(Drains,z,Gullies$SedDel,fun=sum,background=0)
  writeRaster(GYswt,paste(GridsPath,"/DEMGYswt.tif",sep=""),"GTiff",datatype="FLT4S",overwrite=TRUE)
  setwd(GridsPath)
  system("mpiexec -n 8 AreaD8 -p DEMp.tif -ad8 DEMGYsed.tif -wg DEMGYswt.tif -nc")
  setwd(ProjectPath)
  GYkg<-raster(paste(GridsPath,"/DEMGYsed.tif",sep=""))
  demnet$GY_kg<-extract(GYkg,demnetpoints)
  demnet$GY_Mg<-demnet$GY_kg/1000
  demnet$SpecSedGY<-demnet$GY_Mg/demnet$ConAKm2
  st_write(demnet,paste(ProjectPath,"/Streams.shp",sep=""),delete_layer=TRUE)
  st_write(Gullies,paste(ProjectPath,"/Gullies.shp",sep=""),delete_layer=TRUE)
}else{
  
}

#Landslides
if("LS_LIST_RAW.csv" %in% ProjectFileList){
  LSFileList<-read.csv("LS_LIST_RAW.csv")
  LSs<-as.character(LSFileList$FileName)
  LSList<-paste(LSFileList$FileName,".shp",sep="")
  
  #Set CRS for shapefiles to match DEM
  for(i in 1:length(LSs)){
    infilename<-LSs[i]
    outfilename<-LSList[i]
    temp<-readOGR(dsn = ShapesPath, layer =infilename)#Using Spatial dataframe
    if(is.na(crs(temp))){
      crs(temp)<-datacrs
    }else{
      spTransform(temp,datacrs)
    }
    # if("STREAM_CON" %in% colnames(temp)){
    #   print(paste("STREAM_CON exists in",outfilename))
    # }else{
    #   temp$STREAM_CON<-rep("Unknown",length(temp$CDATE))
    # }
    writeOGR(temp,ShapesPath,infilename,driver = "ESRI Shapefile",overwrite_layer = TRUE)#Using Spatial dataframe
  }
  
  LSFieldList<-"LOCATION,LENGTH,WIDTH,DEPTH,STREAM_CON,CDATE,VEHICLE,COMMENT,CTIME1,CTIME2"
  numLS<-length(LSList)
  ogr2ogr(paste(ShapesPath,"/",LSList[1],sep=""),paste(ProjectPath,"/","Landslides.shp",sep=""),f="ESRI Shapefile",select=LSFieldList,overwrite=TRUE)
  for( i in 2:numdptypes){
    ogr2ogr(paste(ShapesPath,"/",LSList[i],sep=""),paste(ProjectPath,"/","Landslides.shp",sep=""),f="ESRI Shapefile",append=TRUE,select=LSFieldList)
  }
  
  Landslides<-st_read(dsn=ProjectPath,layer="Landslides")
  #st_Transform(Landslides,datacrs)
  Landslides$Vol_m3<-(2/3*pi*as.numeric(Landslides$LENGTH)/2*as.numeric(Landslides$WIDTH)/2*as.numeric(Landslides$DEPTH)/2)/(3.28^3)
  Landslides$Mass_kg<-1500*Landslides$Vol_m3
  
  #Create GDPID numbers
  yrrd<-substr(Landslides$CDATE,3,4)
  mord<-substr(Landslides$CDATE,6,7)
  dyrd<-substr(Landslides$CDATE,9,10)
  verd<-as.character(Landslides$VEHICLE)
  fp1ctime<-as.character(Landslides$CTIME1)
  fp2ctime<-as.character(Landslides$CTIME2)
  Landslides$GDPID1<-paste(yrrd,mord,dyrd,fp1ctime,verd,sep="")
  Landslides$GDPID2<-paste(yrrd,mord,dyrd,fp2ctime,verd,sep="")
  Landslides$GDPID1SC<-rep(0,length(Landslides$CDATE))
  Landslides$GDPID2SC<-rep(0,length(Landslides$CDATE))
  Landslides$OSC<-rep(0,length(Landslides$CDATE))
  Landslides$SC<-rep(0,length(Landslides$CDATE))
  
  #Check drainpoint stream connection
  for(i in 1:length(Landslides$CDATE)){
    dp1<-which(Landslides$GDPID1[i]==Drainpointsshp$GDPID)
    dp2<-which(Landslides$GDPID2[i]==Drainpointsshp$GDPID)
    if(length(dp1)!=0){
      Landslides$GDPID1SC[i]<-Drainpointsshp$SC[dp1]
    }
    if(length(dp2)!=0){
      Landslides$GDPID2SC[i]<-Drainpointsshp$SC[dp2]
    }
    if(!is.na(Landslides$STREAM_CON[i])==TRUE & Landslides$STREAM_CON[i]=="Yes"){
      Landslides$OSC[i]<-1
    }
    sc_check<-sum(Landslides$GDPID1SC[i],Landslides$GDPID2SC[i],Landslides$OSC[i])
    if(sc_check>0){
      Landslides$SC[i]<-1
    }
  } 
  
  Landslides$SedDel<-Landslides$SC*Landslides$Mass_kg
  LSswt<-rasterize(Drains,z,Landslides$SedDel,fun=sum,background=0)
  writeRaster(LSswt,paste(GridsPath,"/DEMLSswt.tif",sep=""),"GTiff",datatype="FLT4S",overwrite=TRUE)
  setwd(GridsPath)
  system("mpiexec -n 8 AreaD8 -p DEMp.tif -ad8 DEMLSsed.tif -wg DEMLSswt.tif -nc")
  setwd(ProjectPath)
  LSkg<-raster(paste(GridsPath,"/DEMLSsed.tif",sep=""))
  demnet$LS_kg<-extract(LSkg,demnetpoints)
  demnet$LS_Mg<-demnet$LS_kg/1000
  demnet$SpecSedLS<-demnet$LS_Mg/demnet$ConAKm2
  st_write(demnet,paste(ProjectPath,"/Streams.shp",sep=""),delete_layer=TRUE)
  st_write(Landslides,paste(ProjectPath,"/Landslides.shp",sep=""),delete_layer=TRUE)
}else{
  
}
#toc()

#tic("Data Analysis")
#Read data for maps and analysis
Roadlines<-st_read(dsn=ProjectPath,layer="Roadlines")
#st_Transform(Roadlines,datacrs)
st_write(Roadlines,paste(ProjectPath,"/Roadlines.shp",sep=""),delete_layer=TRUE)
Drainpoints<-st_read(dsn=ProjectPath,layer="Drainpoints")
#st_Transform(Drainpoints,datacrs)
st_write(Drainpoints,paste(ProjectPath,"/Drainpoints.shp",sep=""),delete_layer=TRUE)
Streams<-st_read(dsn=ProjectPath,layer="Streams")
#st_Transform(Streams,datacrs)
st_write(Streams,paste(ProjectPath,"/Streams.shp",sep=""),delete_layer=TRUE)
if("GY_LIST_RAW.csv" %in% ProjectFileList){
Gullies<-st_read(dsn=ProjectPath,layer="Gullies")
#st_Transform(Gullies,datacrs)
st_write(Gullies,paste(ProjectPath,"/Gullies.shp",sep=""),delete_layer=TRUE)
}
if("LS_LIST_RAW.csv" %in% ProjectFileList){
Landslides<-st_read(dsn=ProjectPath,layer="Landslides")
#st_Transform(Landslides,datacrs)
st_write(Landslides,paste(ProjectPath,"/Landslides.shp",sep=""),delete_layer=TRUE)
}


#Results tables
ResultsList<-c("Number of Road Segments","Number of Drainpoints","Sediment Production (Mg) - Roads","Sediment Production (Mg) - Drainpoints","Sediment Delivery (Mg) - Roads","Sediment Delivery (Mg) - Drainpoints","Road Length (km)","Effective Length (km)")
NumRd<-length(Roadlines$RID)
NumDP<-length(Drainpoints$GDPID)
AllSedProdRD_Mg<-sum(Roadlines$TS_Prod)/1000
AllSedProdDP_Mg<-sum(Drainpoints$SedProd)/1000
AllSedDelRD_Mg<-sum(Roadlines$TS_Del)/1000
AllSedDelDP_Mg<-sum(Drainpoints$SedDel)/1000
TotRDLen_km<-sum(Roadlines$Len_m)/1000
Tot_ELen_km<-sum(Drainpoints$E_Len)/1000
ResultsValues<-c(NumRd,NumDP,AllSedProdRD_Mg,AllSedProdDP_Mg,AllSedDelRD_Mg,AllSedDelDP_Mg,TotRDLen_km,Tot_ELen_km)
ResultsTable<-data.frame(ResultsList,ResultsValues)
colnames(ResultsTable)<-c("Attribute","Value")
write.csv(ResultsTable,"Results_Table.csv")
print(ResultsTable)

#Pareto curves
SDclass<-seq(0,110,10)
#Drainpoints<-data.frame(Drainpoints$GDPID,Drainpoints$SedDel,Drainpoints$E_Len)
#colnames(Drainpoints)<-c("GRAIP Drain ID","SedDel_kg","ELen_m")
Drainpoints$SedDel_Mg<-as.numeric(Drainpoints$SedDel)/1000
Drainpoints$ELen_km<-as.numeric(Drainpoints$E_Len)/1000
Drainpoints_Sorted<-Drainpoints[order(Drainpoints$SedDel_Mg,decreasing = TRUE),]
Drainpoints_Sorted$PercSedDel<-Drainpoints_Sorted$SedDel_Mg/AllSedDelDP_Mg*100
Drainpoints_Sorted$PercELen<-Drainpoints_Sorted$ELen_km/Tot_ELen_km*100
Drainpoints_Sorted$CumPercSedDel<-rep(NA,length(Drainpoints_Sorted$PercSedDel))
Drainpoints_Sorted$CumPercELen<-rep(NA,length(Drainpoints_Sorted$PercSedDel))
Drainpoints_Sorted$SDClasses<-rep(NA,length(Drainpoints_Sorted$PercSedDel))
Drainpoints_Sorted$SDClassColor<-rep(NA,length(Drainpoints_Sorted$PercSedDel))
Drainpoints_Sorted$SDClassSize<-rep(NA,length(Drainpoints_Sorted$PercSedDel))
Drainpoints_Sorted$CumPercSedDel[1]<-Drainpoints_Sorted$PercSedDel[1]
Drainpoints_Sorted$CumPercELen[1]<-Drainpoints_Sorted$PercELen[1]
ClassColors<-c("darkred","red","orangered","orange","yellow1","yellowgreen","green","lightseagreen","deepskyblue","blue","midnightblue")
ClassSize<-seq(5.5,0.5,-0.5)
for(i in 2:length(Drainpoints_Sorted$PercSedDel)){
  Drainpoints_Sorted$CumPercSedDel[i]<-Drainpoints_Sorted$PercSedDel[i]+Drainpoints_Sorted$CumPercSedDel[i-1]
  Drainpoints_Sorted$CumPercELen[i]<-Drainpoints_Sorted$PercELen[i]+Drainpoints_Sorted$CumPercELen[i-1]
}
for(i in 2:length(SDclass)){
  ClassTemp<-which(Drainpoints_Sorted$CumPercSedDel>SDclass[i-1]&Drainpoints_Sorted$CumPercSedDel<=SDclass[i])
  Drainpoints_Sorted$SDClasses[ClassTemp]<-i-1
  Drainpoints_Sorted$SDClassColor[ClassTemp]<-ClassColors[i-1]
  Drainpoints_Sorted$SDClassSize[ClassTemp]<-ClassSize[i-1]
}
write.csv(Drainpoints_Sorted,"Pareto_Data.csv")
xrange<-c(0,100)
yrange<-c(0,100)
jpeg("ParetoCurve.jpeg",width=3000,height=2000,quality=100,res=300,pointsize=15) #Creates jpeg image file
par(mar=c(4,6,4,1))
plot(xrange,yrange,type="n",main="",xlab="",ylab="",xaxt="n",yaxt="n") 
axis(1,labels=TRUE,tick=TRUE,tck=0.01,lwd=2,cex.axis=1,col="black",xaxp=c(0,100,10))
axis(2,labels=TRUE,tick=TRUE,tck=0.01,lwd=2,cex.axis=1,col="black",las=2,yaxp=c(0,100,10))
lines(Drainpoints_Sorted$CumPercSedDel~Drainpoints_Sorted$CumPercELen,col="gray50",lwd=0.5)
points(Drainpoints_Sorted$CumPercSedDel~Drainpoints_Sorted$CumPercELen,pch=20,col=Drainpoints_Sorted$SDClassColor,cex=Drainpoints_Sorted$SDClassSize)
mtext("Percent Sediment Delivered",2,cex=1.5, line=3.5)
mtext("Percent Road Length",1,cex=1.5, line=2.5)

abline(v=seq(0,100,10),col="black")
abline(h=seq(0,100,10),col="black")
abline(v=0,col="black",lwd=2)
abline(h=0,col="black",lwd=2)
box(which="plot",lwd=2)
box(which="outer",lwd=5)
dev.off() #Turns off and saves jpeg

par(mar=c(4,6,4,1))
plot(xrange,yrange,type="n",main="",xlab="",ylab="",xaxt="n",yaxt="n") 
axis(1,labels=TRUE,tick=TRUE,tck=0.01,lwd=2,cex.axis=1,col="black",xaxp=c(0,100,10))
axis(2,labels=TRUE,tick=TRUE,tck=0.01,lwd=2,cex.axis=1,col="black",las=2,yaxp=c(0,100,10))
lines(Drainpoints_Sorted$CumPercSedDel~Drainpoints_Sorted$CumPercELen,col="gray50",lwd=0.5)
points(Drainpoints_Sorted$CumPercSedDel~Drainpoints_Sorted$CumPercELen,pch=20,col=Drainpoints_Sorted$SDClassColor,cex=Drainpoints_Sorted$SDClassSize)
mtext("Percent Sediment Delivered",2,cex=1.5, line=3.5)
mtext("Percent Road Length",1,cex=1.5, line=2.5)

abline(v=seq(0,100,10),col="black")
abline(h=seq(0,100,10),col="black")
abline(v=0,col="black",lwd=2)
abline(h=0,col="black",lwd=2)
box(which="plot",lwd=2)
box(which="outer",lwd=5)

#Critical ESI Plot, Adapted from Original code by Richard Cissel

library(locfit)
DischargeTo<-Drainpoints$DISCHRG
ESI<-Drainpoints$ESI
DischargeCode<-rep(0,length(DischargeTo))
ESI_Dat<-data.frame(DischargeTo,DischargeCode,ESI)
Gullies<-which(ESI_Dat$DischargeTo=="Gully")
ESI_Dat$DischargeCode[Gullies]<-1
model <- locfit(DischargeCode~ESI,data=ESI_Dat,maxk=1000000) # run the local fit
yrange<-c(0,1)
xrange<-c(0,round(max(ESI_Dat$ESI)+5,digits=-1))
ESI_Dat$DischargePlot<-1*ESI_Dat$DischargeCode
write.csv(ESI_Dat,"ESI_Data.csv")

jpeg("ESI_plot.jpeg",width=3000,height=2000,quality=100,res=300,pointsize=15) #Creates jpeg image file in the Calib folder
par(mar=c(4,6,4,1))
plot(xrange,yrange,type="n",main="",xlab="",ylab="",xaxt="n",yaxt="n") #Creates plot of stream connection probabilities based on variables
axis(1,labels=TRUE,tick=TRUE,tck=0.01,lwd=2,cex.axis=1,col="black",xaxp=c(0,max(xrange),max(xrange)*0.1))
axis(2,labels=TRUE,tick=TRUE,tck=0.01,lwd=2,cex.axis=1,col="black",las=2,yaxp=c(0,1,20))
lines(model,col="steelblue",lwd=3)
points(DischargePlot~ESI,data=ESI_Dat, col="red",cex=0.5)

mtext("Gully Probability",2,cex=1.5, line=3.5)
mtext("Erosion Sensitivity Index",1,cex=1.5, line=2.5)
abline(v=0,col="black")
abline(h=0,col="black")
box(which="plot",lwd=2)
box(which="outer",lwd=5)
dev.off() #Turns off and saves jpeg

par(mar=c(4,6,4,1))
plot(xrange,yrange,type="n",main="",xlab="",ylab="",xaxt="n",yaxt="n") #Creates plot of stream connection probabilities based on variables
axis(1,labels=TRUE,tick=TRUE,tck=0.01,lwd=2,cex.axis=1,col="black",xaxp=c(0,max(xrange),max(xrange)*0.1))
axis(2,labels=TRUE,tick=TRUE,tck=0.01,lwd=2,cex.axis=1,col="black",las=2,yaxp=c(0,1,20))
lines(model,col="steelblue",lwd=3)
points(DischargePlot~ESI,data=ESI_Dat, col="red",cex=0.5)

mtext("Gully Probability",2,cex=1.5, line=3.5)
mtext("Erosion Sensitivity Index",1,cex=1.5, line=2.5)
abline(v=0,col="black")
abline(h=0,col="black")
box(which="plot",lwd=2)
box(which="outer",lwd=5)
#toc()

toc()

