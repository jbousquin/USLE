###############PURPOSE#################
#This script generates a distribution of R (rainfall factor) values for a dataset of monthly rainfall and R densities
#These distributions are then joined back to spatially explicit locations where USLE can be calculated
#Using multiple R values maintains pre-existing spatial variation while
#applying a distribution to the precipitation values used to calculate the R value
#also maintains variation due to differences in rainfall
#######################################
############PACKAGES###################
#install rgdal to work with RUSLE .shp file directly
install.packages("rgdal")
library(rgdal)
#######################################
##########READ IN FILES################
#start clock for timing process
ptm <- proc.time()
#Set the working directory for file locations
setwd("E:/GIS_Results")
#.shp R datasets were downloaded from http://fargo.nserl.purdue.edu/RUSLE2_ftp/Climate_data/
R_table<-readOGR("RUSLE2", "pptbycounty12")
#Read in other layers from GIs (spatial statistics-extraction-sample in ArcGIS)
GIS_sample <- read.csv("sample5.txt", sep=",")
#######################################
##########IN FILE SETUP################
#If using a dataset that is different from the original, column names can be altered
#by specifying the column index that corresponds to the desired attribute
#the script will then update the column name to match the original dataset allowing the script to run without
#replacing headings names or numbers anywhere else in the script
#If run on the original dataset, columns are renamed what they were already named

#The mean precipitation and R density for each month will be used from the R_table
#FIPS_GRDCD [1] was used as the unique identifier to relate the R_table and GIS_SAMPLE (R_ID)
R_ID_table<-R_table$FIPS_GRDCD

#Four values for each raster grid cell are needed from GIS_sample:
#(1) The R_ID, column [12], to join this data to the R values
colnames(GIS_sample)[12]<-"R_ID"
#PREP NOTES:
#The shapefile had gaps for waterways that were eliminated in GIS prior to sampling it as a raster
#If buffering or snapping is incorrect it may result in zero values in the attribute table, NA in R
#If exported as integer it can be coerced in DATA PREP OPTIONS section

#(2) The soil kf value [5]
colnames(GIS_sample)[5]<- "kf_soil"

#(3) The land cover value for nlcd values [9] and where coffee replacements were made [6]
colnames(GIS_sample)[9]<-"cover_nlcd"
colnames(GIS_sample)[6]<-"cover_coffee"
#PREP NOTES:
#NLCD landuse IDs will be replaced with CCap c values in DATA PREP OPTIONS section

#(4) The length slope value [7]
colnames(GIS_sample)[7]<-"lenSlope"
#PREP NOTES:
#These values were calculated from percent or degree slope in GIS using raster calculator

#In this analysis annual precipitation values were available to compare with the same from RUSLE2
colnames(GIS_sample)[8]<-"annual_rain"
#In this analysis watershed identifiers were used to subset the multiple watersheds analyzed at once
#If only one watershed is being used still specify a column with constant values
colnames(GIS_sample)[13]<-"WSHED_NAME"
#######################################
#########USER SET PARAMS###############
#set param n as the number of samples
n <- 3
#Set param rainfall Coefficient of Variation (CoV) to be applied to the dataset for variation over time
CoV  <- 0.2
#set param rg as number of R groups (R_ID) from the file
rg <- nrow(R_table)
#set average rainfall for watershed, single average method:
rain <- 1897
#If there are multiple watersheds the rain values will be updated during post-processing
#C Values
sunC<-0.129
shadeC<-0.048
#placeholder value for coffee locations, this allows it to be exported with any value
holdC<-300
#raster grid cell size (m)
cell<-30
#######################################
##########DATA PREP OPTIONS############
#Prepare GIS_Sample if it wasn't previously
#Since R_ID was exported as integer it needed to be reformatted 
GIS_sample$R_ID<-as.numeric(gsub(",","", GIS_sample$R_ID))

#To convert nlcd 2001 IDs to CCap values
#95 Emergent Herbaceous Wetlands & 90 Woody Wetlands
GIS_sample$cover_nlcd[GIS_sample$cover_nlcd == "95"] <- "0.080"
GIS_sample$cover_nlcd[GIS_sample$cover_nlcd == "90"] <- "0.080"
#82 Cultivated Crops 
GIS_sample$cover_nlcd[GIS_sample$cover_nlcd == "82"] <- "0.200"
#81 Pasture Land & 71 Grassland
GIS_sample$cover_nlcd[GIS_sample$cover_nlcd == "81"] <- "0.125"
GIS_sample$cover_nlcd[GIS_sample$cover_nlcd == "71"] <- "0.125"
#52 Scrub/Shrub
GIS_sample$cover_nlcd[GIS_sample$cover_nlcd == "52"] <- "0.050"
#42 Evergreen Forest
GIS_sample$cover_nlcd[GIS_sample$cover_nlcd == "42"] <- "0.015"
#31 Barren Land
GIS_sample$cover_nlcd[GIS_sample$cover_nlcd == "31"] <- "0.220"
#24 Developed High Intensity & 23 Medium Intensity & 22 Low Intensity & Developed Open Space
GIS_sample$cover_nlcd[GIS_sample$cover_nlcd == "24"] <- "0.210"
GIS_sample$cover_nlcd[GIS_sample$cover_nlcd == "23"] <- "0.210"
GIS_sample$cover_nlcd[GIS_sample$cover_nlcd == "22"] <- "0.210"
GIS_sample$cover_nlcd[GIS_sample$cover_nlcd == "21"] <- "0.210"
#11 Open Water
GIS_sample$cover_nlcd[GIS_sample$cover_nlcd == "11"] <- "0.005"
#Coerce back to numeric
GIS_sample$cover_nlcd<-as.numeric(GIS_sample$cover_coffee)
#Add potential coffee scenarios
GIS_sample$cover_coffee[GIS_sample$cover_coffee == "95"] <- "0.080"
GIS_sample$cover_coffee[GIS_sample$cover_coffee == "90"] <- "0.080"
GIS_sample$cover_coffee[GIS_sample$cover_coffee == "82"] <- "0.200"
GIS_sample$cover_coffee[GIS_sample$cover_coffee == "81"] <- "0.125"
GIS_sample$cover_coffee[GIS_sample$cover_coffee == "71"] <- "0.125"
GIS_sample$cover_coffee[GIS_sample$cover_coffee == "52"] <- "0.050"
GIS_sample$cover_coffee[GIS_sample$cover_coffee == "42"] <- "0.015"
GIS_sample$cover_coffee[GIS_sample$cover_coffee == "31"] <- "0.220"
GIS_sample$cover_coffee[GIS_sample$cover_coffee == "24"] <- "0.210"
GIS_sample$cover_coffee[GIS_sample$cover_coffee == "23"] <- "0.210"
GIS_sample$cover_coffee[GIS_sample$cover_coffee == "22"] <- "0.210"
GIS_sample$cover_coffee[GIS_sample$cover_coffee == "21"] <- "0.210"
GIS_sample$cover_coffee[GIS_sample$cover_coffee == "11"] <- "0.005"
# holdC was used as a placeholder where the coffee replacement was made
#add a column for 100% sun
GIS_sample$lc_All_sun<-GIS_sample$cover_nlcd
GIS_sample$lc_All_sun[GIS_sample$lc_All_sun == holdC] <- sunC
GIS_sample$lc_All_sun<- as.numeric(GIS_sample$lc_All_sun)
#add a column for 50% sun 50% shade
GIS_sample$lc_some_sun<-GIS_sample$cover_nlcd
GIS_sample$lc_some_sun[GIS_sample$lc_some_sun == holdC] <- ((sunC+shadeC)/2)
GIS_sample$lc_some_sun<-as.numeric(GIS_sample$lc_some_sun)
#add a column for 100% shade
GIS_sample$lc_All_shade<-GIS_sample$cover_nlcd
GIS_sample$lc_All_shade[GIS_sample$lc_All_shade == holdC] <- shadeC
GIS_sample$lc_All_shade<-as.numeric(GIS_sample$lc_All_shade)

#Multiply LS * K * C for each of the 4 scenarios
GIS_sample$LS_K_C_nlcd<-GIS_sample$lenSlope * GIS_sample$kf_soil * GIS_sample$cover_nlcd
GIS_sample$LS_K_C_sun<-GIS_sample$lenSlope * GIS_sample$kf_soil * GIS_sample$lc_All_sun
GIS_sample$LS_K_C_some<-GIS_sample$lenSlope * GIS_sample$kf_soil * GIS_sample$lc_some_sun
GIS_sample$LS_K_C_shade<-GIS_sample$lenSlope * GIS_sample$kf_soil * GIS_sample$lc_All_shade

#######################################
##############PROCEDURE################
#Designate dataset vectors for 1st and 2nd moment, to apply distribution to
mean_jan <- R_table$PPT_JAN
std_jan <- mean_jan*CoV
mean_feb <- R_table$PPT_FEB
std_feb <- mean_feb*CoV
mean_mar <- R_table$PPT_MAR
std_mar <- mean_mar*CoV
mean_apr <- R_table$PPT_APR
std_apr <- mean_apr*CoV
mean_may <- R_table$PPT_MAY
std_may <- mean_may*CoV
mean_jun <- R_table$PPT_JUN
std_jun <- mean_jun*CoV
mean_jul <- R_table$PPT_JUL
std_jul <- mean_jul*CoV
mean_aug <- R_table$PPT_AUG
std_aug <- mean_aug*CoV
mean_sep <- R_table$PPT_SEP
std_sep <- mean_sep*CoV
mean_oct <- R_table$PPT_OCT
std_oct <- mean_oct*CoV
mean_nov <- R_table$PPT_NOV
std_nov <- mean_nov*CoV
mean_dec <- R_table$PPT_DEC
std_dec <- mean_dec*CoV

#The following process will be looped n times, with each entry becoming a row in table x
#Loop takes each sample to completion and appends it to a file, this makes it difficult to 
#troubleshoots and extends run times, but allows for larger datasets to be analyzed
z <- 1:n
x <- matrix(nrow=0, ncol=11)
for(i in seq(along=z)){
  #Produce random probability
  rprob <- runif(1, 0, 1)
  #Get n random samples from the normal distribution around the moments
  #The new value is sampled by the random probability for each original mean, rg X n  
  sample_jan <- qnorm(rep(rprob, each=rg), mean_jan, std_jan)
  sample_feb <- qnorm(rep(rprob, each=rg), mean_feb, std_feb)
  sample_mar <- qnorm(rep(rprob, each=rg), mean_mar, std_mar)
  sample_apr <- qnorm(rep(rprob, each=rg), mean_apr, std_apr)
  sample_may <- qnorm(rep(rprob, each=rg), mean_may, std_may)
  sample_jun <- qnorm(rep(rprob, each=rg), mean_jun, std_jun)
  sample_jul <- qnorm(rep(rprob, each=rg), mean_jul, std_jul)
  sample_aug <- qnorm(rep(rprob, each=rg), mean_aug, std_aug)
  sample_sep <- qnorm(rep(rprob, each=rg), mean_sep, std_sep)
  sample_oct <- qnorm(rep(rprob, each=rg), mean_oct, std_oct)
  sample_nov <- qnorm(rep(rprob, each=rg), mean_nov, std_nov)
  sample_dec <- qnorm(rep(rprob, each=rg), mean_dec, std_dec)
  
  #Multiply the monthly ppt values by the corresponding erosivity density value
  r_jan<- sample_jan * R_table$ED_JAN
  r_feb<- sample_feb * R_table$ED_FEB
  r_mar<- sample_mar * R_table$ED_MAR
  r_apr<- sample_apr * R_table$ED_APR 
  r_may<- sample_may * R_table$ED_MAY
  r_jun<- sample_jun * R_table$ED_JUN
  r_jul<- sample_jul * R_table$ED_JUL
  r_aug<- sample_aug * R_table$ED_AUG 
  r_sep<- sample_sep * R_table$ED_SEP
  r_oct<- sample_oct * R_table$ED_OCT
  r_nov<- sample_nov * R_table$ED_NOV 
  r_dec<- sample_dec * R_table$ED_DEC
  
  #Add the monthly R values together to get annual R values
  r_annual <- (r_jan + r_feb + r_mar + r_apr + r_may + r_jun + r_jul + r_aug + r_sep + r_oct + r_nov+ r_dec)
  
  #Turn the list of possible values into a matrix with rg rows
  r_matrix  <- matrix(r_annual, nrow=rg, ncol=1)  
  
  #Make sure the matrix has the R groups IDs too
  r_matrix <- as.data.frame(cbind(r_matrix, R_ID_table))
  
  #This completes phase 1 where R distributions are formulated
  #phase 2 will join these R values to other spatially explicit values based on R_ID
  #Results are split by watershed in post-processing
  nlcd1 <- merge(GIS_sample, r_matrix, by.x= "R_ID", by.y = "R_ID_table", all.x=TRUE)
  nlcd1 <- cbind(nlcd1, nlcd1$V1*nlcd1$LS_K_C_nlcd)
  nlcd1 <- cbind(nlcd1, nlcd1$V1*nlcd1$LS_K_C_sun)
  nlcd1 <- cbind(nlcd1, nlcd1$V1*nlcd1$LS_K_C_some)
  nlcd1 <- cbind(nlcd1, nlcd1$V1*nlcd1$LS_K_C_shade)
  
  x1 <- cbind(nlcd1$V1*nlcd1$LS_K_C_nlcd, nlcd1$V1*nlcd1$LS_K_C_sun, nlcd1$V1*nlcd1$LS_K_C_some, nlcd1$V1*nlcd1$LS_K_C_shade, nlcd1$WSHED_NAME)
  
  #Data were aggregated to the watershed before totaling 
  lst.Name<-unique(nlcd1$WSHED_NAME)
  #loop through each group, getting totals for each
  x2<-matrix(nrow=0, ncol=9)
  for (name in lst.Name){
    x_area<-(nlcd1[nlcd1$WSHED_NAME == name,])
    #Determine sediment delivery ratio
    numCells<-nrow(x_area)
    Wshed_area_km<-(numCells*cell*cell)/1000000
    SDR<-0.41*((Wshed_area_km)^(-0.3))
    x_area<-data.frame(as.list(colSums(x_area[,22:25])))
    x_area<-cbind(x_area, x_area*SDR)
    x_area<-cbind(x_area, name)
    #Apply sediment delivery ratio
    x2<-rbind(x2, x_area)
  }  
  ptp <- qnorm(rprob, rain, rain*CoV)
  x2 <- cbind(x2, rprob, ptp)  
  x <- rbind(x,x2)
}
###########WRITE OUTPUT################
#Rename columns

x$X<-NULL
colnames(x)[1]<-"nlcd_original_Eroded"
colnames(x)[2]<-"All_Sun_Eroded"
colnames(x)[3]<-"Partial_Conversion_Eroded"
colnames(x)[4]<-"All_Shade_Eroded"
colnames(x)[5]<-"nlcd_original_Delivered"
colnames(x)[6]<-"All_Sun_Delivered"
colnames(x)[7]<-"Partial_Conversion_Delivered"
colnames(x)[8]<-"All_Shade_Delivered"


write.csv(x, "R_output.csv")
proc.time() - ptm
#######################################
##########POST PROCESSING##############
#Read back in if not already assigned
Results <- read.csv("R_output.csv", sep=",")

#if it has been opened in excel, factors have to be coerced to numeric
#Results$rprob<-as.numeric(as.character(Results$rprob))

#####POST PROCESSING FUNCTIONS#####
#createwatershed specific files
WatershedSpecificResults<-function(str){
  Results1<-Results[Results$"name" == str,]
  Results_rain<-(mean(GIS_sample$annual_rain[GIS_sample$WSHED_NAME== str]))*25.4
  Results1$ptp<-qnorm(Results1$rprob, Results_rain, Results_rain*CoV)
  return(Results1)
}
#create scenario files for export to netica
ScenarioResults<-function(str){
  Sun_scenario<-cbind(str$All_Sun_Delivered, str$ptp, "Sun")
  Some_scenario<-cbind(str$Partial_Conversion_Delivered, str$ptp, "Some")
  Shade_scenario<-cbind(str$All_Shade_Delivered, str$ptp, "Shade")
  Scenarios<-rbind(Sun_scenario, Some_scenario, Shade_scenario)
  return(Scenarios)
}
#####END FUNCTIONS#####
lst.Name<-unique(Results$name)

#Lucchetti Example: write.csv(WatershedSpecificResults("Lucchetti"), "Lucchetti_output.csv")
for (name in lst.Name){
  write.csv(WatershedSpecificResults(paste(name,sep="")), paste(name, "_output.csv", sep=""))
}

#Lucchetti Example: write.csv(ScenarioResults(WatershedSpecificResults("Lucchetti")), "Lucchetti_Scenarios_output.csv")
for (name in lst.Name){
  write.csv(ScenarioResults(WatershedSpecificResults(paste(name,sep=""))), paste(name, "_Scenarios_output.csv", sep=""))
}

