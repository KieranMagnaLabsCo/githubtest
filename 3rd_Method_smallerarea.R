##### Third method final code #####
##### set working directory and install packages
setwd("/Users/kieranmckeag/Desktop/Black Bear Research")
library(stats)
library(bestglm)
library(ggplot2)
library(tidyverse)


# create data frame of all random points bears
Random_Detection_Points_df <- data.frame(read.csv("/Users/kieranmckeag/Desktop/June7_stats.csv"))
                                         
# remove unwanted columns from data frame and add a detection column to hold 0's for binomial response
Random_Detection_Points_df$Detection <- 0 # now this data is ready for linear regression
colnames(Random_Detection_Points_df) <- c("OID_","elevclippe", "roadseucdi",  "watereucdi", "HumDenProj", "nlcdproj", "StandAge", "Detection")
Random_Detection_Points_df <- subset(Random_Detection_Points_df, select = c("OID_", "nlcdproj", "roadseucdi", "HumDenProj", "watereucdi", "elevclippe", "StandAge", "Detection"))
Random_Detection_Points_df$OID_ <- 1:nrow(Random_Detection_Points_df)
#Actual detections
MRB_Occupancy_Data <- read.csv("/Users/kieranmckeag/Desktop/Black Bear Research/MRBUSedWithStandAge.csv")
MRB_Grouped <- MRB_Occupancy_Data %>% group_by(bear) %>% arrange(bear)
count(MRB_Grouped, "bear")
count <- count(MRB_Grouped, "bear")


# bear 0 is actually a combined detection history of bears 32784, 32826, and 32828
# see how they are actually broken up within the data frame to get individual detection histories of each
# use tidyverse and the 'CollarID' column in the data frame
a <- MRB_Occupancy_Data %>% group_by(CollarID) %>% arrange(CollarID)
count(a, "CollarID")
countID <- count(a, "CollarID")

# seperate detection histories of each bear 
Bear32784_Detections <- MRB_Grouped[1:1074,1:34]
Bear32784_Detections$bear <- c(32784)
Bear32826_Detections <- MRB_Grouped[1075:5541,1:34]
Bear32826_Detections$bear <- c(32826)
Bear32828_Detections <- MRB_Grouped[5542:5700,1:34]
Bear32828_Detections$bear <- c(32828)
Bear487_Detections <- MRB_Grouped[5701:11361, 1:34]
Bear488_Detections <- MRB_Grouped[11362:17595 ,1:34]
Bear490_Detections <- MRB_Grouped[17596:22709, 1:34]
Bear491_Detections <- MRB_Grouped[22710:29973, 1:34]
Bear492_Detections <- MRB_Grouped[29974:32321, 1:34]
Bear493_Detections <- MRB_Grouped[32322:37446, 1:34]
Bear494_Detections <- MRB_Grouped[37447:44650, 1:34]
Bear495_Detections <- MRB_Grouped[44651:47547, 1:34]
Bear496_Detections <- MRB_Grouped[47548:49092, 1:34]
Bear497_Detections <- MRB_Grouped[49093:49242, 1:34]
Bear498_Detections <- MRB_Grouped[49243:57276, 1:34]
Bear500_Detections <- MRB_Grouped[57277:58074, 1:34]
Bear501_Detections <- MRB_Grouped[58075:64584, 1:34]
Bear503_Detections <- MRB_Grouped[64584:70387, 1:34]
Bear504_Detections <- MRB_Grouped[70388:72799, 1:34]
Bear505_Detections <- MRB_Grouped[72800:75512, 1:34]
Bear506_Detections <- MRB_Grouped[75513:77467, 1:34]
Bear710267_Detections <- MRB_Grouped[77468:78431, 1:34]
Bear710268_Detections <- MRB_Grouped[78432:81513, 1:34]
Bear710270_Detections <- MRB_Grouped[81514:82260, 1:34]
Bear710273_Detections <- MRB_Grouped[82261:83416, 1:34]
Bear710280_Detections <- MRB_Grouped[83417:85986, 1:34]
Bear710281_Detections <- MRB_Grouped[85987:89530, 1:34]
Bear712874_Detections <- MRB_Grouped[89531:90742, 1:34]
Bear712875_Detections <- MRB_Grouped[90743:92195, 1:34]
Bear712878_Detections <- MRB_Grouped[92196:92842, 1:34]

MRB_Grouped <- data.frame(rbind(Bear487_Detections, Bear488_Detections, Bear490_Detections,
                                Bear491_Detections, Bear492_Detections, Bear493_Detections,
                                Bear494_Detections, Bear495_Detections, Bear496_Detections,
                                Bear497_Detections, Bear498_Detections, Bear500_Detections,
                                Bear501_Detections, Bear503_Detections, Bear504_Detections,
                                Bear505_Detections, Bear506_Detections, Bear32784_Detections,
                                Bear32826_Detections, Bear32828_Detections, Bear710267_Detections,
                                Bear710268_Detections, Bear710270_Detections, Bear710273_Detections,
                                Bear710280_Detections, Bear710281_Detections, Bear712874_Detections,
                                Bear712875_Detections, Bear712878_Detections))
# do same for actual detections as above
Actual_Detection_Points_df <- data.frame(MRB_Grouped)
Actual_Detection_Points_df <- subset(Actual_Detection_Points_df, select = -c(GMT_Date, GMT_Time, Latitude, Longitude_, Longitude,
                                                                             Latitude_1, GPS_Latitu, GPS_Longit, GPS_UTM_Zo,
                                                                             GPS_UTM_No, GPS_UTM_Ea, GPS_Horizo, CollarID, LMT_Date, LMT_Time,
                                                                             Latitude__, DOP, Easting, Northing, EastCor, waterrecls, roadsrecls,
                                                                             slope, aspect, stewrcls, HDrastKm))

Actual_Detection_Points_df$Detection <- 1 # now this data is also ready for linear regression

Actual_Detection_Points_df <- Actual_Detection_Points_df[, c(1,5,4,7,3,6,8,9)]
colnames(Actual_Detection_Points_df) <- c("OID_", "nlcdproj","roadseucdi","HumDenProj", "watereucdi","elevclippe", "StandAge","Detection")
Actual_Detection_Points_df
write.csv(Actual_Detection_Points_df, file = "MRB_Bear_Detection_Data_finished.csv")

# Now, combine the two dataframes into a massive one to run the regression
MELR_Detection_df <- rbind(Random_Detection_Points_df, Actual_Detection_Points_df)
MELR_Detection_df <- MELR_Detection_df[sample(1:nrow(MELR_Detection_df)),] # randomize the data
MELR_Detection_df$StandAge[is.na(MELR_Detection_df$StandAge)] <- 0 # get rid of a few unwanted NA's
MELR_Detection_df <- na.omit(MELR_Detection_df) # get rid of a few unwanted NA's

# need NLCD classes to match
for (i in 1:nrow(MELR_Detection_df)) {
  if (MELR_Detection_df$nlcdproj[i] == 42){
    MELR_Detection_df$nlcdproj[i] <- "Evergreen Forest"
  }else if (MELR_Detection_df$nlcdproj[i] == 11){ 
    MELR_Detection_df$nlcdproj[i] <- "Open Water"
  }else if (MELR_Detection_df$nlcdproj[i] == 21){
    MELR_Detection_df$nlcdproj[i] <- "Developed"
  }else if (MELR_Detection_df$nlcdproj[i] == 22){
    MELR_Detection_df$nlcdproj[i] <- "Developed"
  }else if (MELR_Detection_df$nlcdproj[i] == 23){
    MELR_Detection_df$nlcdproj[i] <- "Developed"
  }else if (MELR_Detection_df$nlcdproj[i] == 24){
    MELR_Detection_df$nlcdproj[i] <- "Developed"
  }else if (MELR_Detection_df$nlcdproj[i] == 31){
    MELR_Detection_df$nlcdproj[i] <- "Barren Land"
  }else if (MELR_Detection_df$nlcdproj[i] == 41){
    MELR_Detection_df$nlcdproj[i] <- "Deciduous Forest"
  }else if (MELR_Detection_df$nlcdproj[i] == 43){
    MELR_Detection_df$nlcdproj[i] <- "Mixed Forest"
  }else if (MELR_Detection_df$nlcdproj[i] == 52){
    MELR_Detection_df$nlcdproj[i] <- "Shrub/Scrub"
  }else if (MELR_Detection_df$nlcdproj[i] == 71){
    MELR_Detection_df$nlcdproj[i] <- "Shrub/Scrub" #Herbaceous combined 4/3
  }else if (MELR_Detection_df$nlcdproj[i] == 81){
    MELR_Detection_df$nlcdproj[i] <- "Agriculture"
  }else if (MELR_Detection_df$nlcdproj[i] == 82){
    MELR_Detection_df$nlcdproj[i] <- "Agriculture"
  }else if (MELR_Detection_df$nlcdproj[i] == 90){
    MELR_Detection_df$nlcdproj[i] <- "Woody Wetlands"
  }else if (MELR_Detection_df$nlcdproj[i] == 95){ 
    MELR_Detection_df$nlcdproj[i] <- "Woody Wetlands"
  } else if (MELR_Detection_df$nlcdproj[i] == "42"){
    MELR_Detection_df$nlcdproj[i] <- "Evergreen Forest"
  }else if (MELR_Detection_df$nlcdproj[i] == "11"){ 
    MELR_Detection_df$nlcdproj[i] <- "Open Water"
  }else if (MELR_Detection_df$nlcdproj[i] == "21"){
    MELR_Detection_df$nlcdproj[i] <- "Developed"
  }else if (MELR_Detection_df$nlcdproj[i] == "22"){
    MELR_Detection_df$nlcdproj[i] <- "Developed"
  }else if (MELR_Detection_df$nlcdproj[i] == "23"){
    MELR_Detection_df$nlcdproj[i] <- "Developed"
  }else if (MELR_Detection_df$nlcdproj[i] == "24"){
    MELR_Detection_df$nlcdproj[i] <- "Developed"
  }else if (MELR_Detection_df$nlcdproj[i] == "31"){
    MELR_Detection_df$nlcdproj[i] <- "Barren Land"
  }else if (MELR_Detection_df$nlcdproj[i] == "41"){
    MELR_Detection_df$nlcdproj[i] <- "Deciduous Forest"
  }else if (MELR_Detection_df$nlcdproj[i] == "43"){
    MELR_Detection_df$nlcdproj[i] <- "Mixed Forest"
  }else if (MELR_Detection_df$nlcdproj[i] == "52"){
    MELR_Detection_df$nlcdproj[i] <- "Shrub/Scrub"
  }else if (MELR_Detection_df$nlcdproj[i] == "71"){
    MELR_Detection_df$nlcdproj[i] <- "Shrub/Scrub" #Herbaceous combined 4/3
  }else if (MELR_Detection_df$nlcdproj[i] == "81"){
    MELR_Detection_df$nlcdproj[i] <- "Agriculture"
  }else if (MELR_Detection_df$nlcdproj[i] == "82"){
    MELR_Detection_df$nlcdproj[i] <- "Agriculture"
  }else if (MELR_Detection_df$nlcdproj[i] == "90"){
    MELR_Detection_df$nlcdproj[i] <- "Woody Wetlands"
  }else if (MELR_Detection_df$nlcdproj[i] == "95"){ 
    MELR_Detection_df$nlcdproj[i] <- "Woody Wetlands" #EHW combined 4/3
  }
}


## add columns of 1's and 0's for nlcd classes
# add columns:
Ag_array <- array(NA, dim = c(nrow(MELR_Detection_df),1)) #agriculture 1's and 0's
for (i in 1:nrow(Ag_array)) {
  if (MELR_Detection_df$nlcdproj[i] == "Agriculture")
    Ag_array[i] <-  1
  else if (MELR_Detection_df$nlcdproj[i] != "Agriculture")
    Ag_array[i] <-  0
}
MELR_Detection_df <- MELR_Detection_df %>% add_column(Agriculture = Ag_array) # adding ag column to MELR data frame
BL_array <- array(NA, dim = c(nrow(MELR_Detection_df),1)) # barren land 1's and 0's
for (i in 1:nrow(BL_array)) {
  if (MELR_Detection_df$nlcdproj[i] == "Barren Land")
    BL_array[i] <-  1
  else if (MELR_Detection_df$nlcdproj[i] != "Barren Land")
    BL_array[i] <-  0
}
MELR_Detection_df <- MELR_Detection_df %>% add_column(Barren_Land = BL_array) # adding barren land column to MELR data frame
Dev_array <- array(NA, dim = c(nrow(MELR_Detection_df),1)) # developed 1's and 0's
for (i in 1:nrow(Dev_array)) {
  if (MELR_Detection_df$nlcdproj[i] == "Developed")
    Dev_array[i] <-  1
  else if (MELR_Detection_df$nlcdproj[i] != "Developed")
    Dev_array[i] <-  0
}
MELR_Detection_df <- MELR_Detection_df %>% add_column(Developed = Dev_array) # adding developed column to MELR data frame
DF_array <- array(NA, dim = c(nrow(MELR_Detection_df),1)) # deciduous forest 1's and 0's
for (i in 1:nrow(DF_array)) {
  if (MELR_Detection_df$nlcdproj[i] == "Deciduous Forest")
    DF_array[i] <-  1
  else if (MELR_Detection_df$nlcdproj[i] != "Deciduous Forest")
    DF_array[i] <-  0
}
MELR_Detection_df <- MELR_Detection_df %>% add_column(Deciduous_Forest = DF_array) # adding deciduous forest column to MELR data frame
EF_array <- array(NA, dim = c(nrow(MELR_Detection_df),1)) # evergreen forest 1's and 0's
for (i in 1:nrow(EF_array)) {
  if (MELR_Detection_df$nlcdproj[i] == "Evergreen Forest")
    EF_array[i] <-  1
  else if (MELR_Detection_df$nlcdproj[i] != "Evergreen Forest")
    EF_array[i] <-  0
}
MELR_Detection_df <- MELR_Detection_df %>% add_column(Evergreen_Forest = EF_array) # adding evergreen forest column to MELR data frame
MF_array <- array(NA, dim = c(nrow(MELR_Detection_df),1)) # mixed forest 1's and 0's
for (i in 1:nrow(MF_array)) {
  if (MELR_Detection_df$nlcdproj[i] == "Mixed Forest")
    MF_array[i] <-  1
  else if (MELR_Detection_df$nlcdproj[i] != "Mixed Forest")
    MF_array[i] <-  0
}
MELR_Detection_df <- MELR_Detection_df %>% add_column(Mixed_Forest = MF_array) # adding mixed forest column to MELR data frame
WW_array <- array(NA, dim = c(nrow(MELR_Detection_df),1)) # woody wetlands 1's and 0's
for (i in 1:nrow(WW_array)) {
  if (MELR_Detection_df$nlcdproj[i] == "Woody Wetlands")
    WW_array[i] <-  1
  else if (MELR_Detection_df$nlcdproj[i] != "Woody Wetlands")
    WW_array[i] <-  0
}
MELR_Detection_df <- MELR_Detection_df %>% add_column(Woody_Wetlands = WW_array) # adding woody wetlands column to MELR data frame
OW_array <- array(NA, dim = c(nrow(MELR_Detection_df),1)) # open water 1's and 0's
for (i in 1:nrow(OW_array)) {
  if (MELR_Detection_df$nlcdproj[i] == "Open Water")
    OW_array[i] <-  1
  else if (MELR_Detection_df$nlcdproj[i] != "Open Water")
    OW_array[i] <-  0
}
MELR_Detection_df <- MELR_Detection_df %>% add_column(Open_Water = OW_array) # adding open water column to MELR data frame
SS_array <- array(NA, dim = c(nrow(MELR_Detection_df),1)) # shrub/scrub 1's and 0's
for (i in 1:nrow(SS_array)) {
  if (MELR_Detection_df$nlcdproj[i] == "Shrub/Scrub")
    SS_array[i] <-  1
  else if (MELR_Detection_df$nlcdproj[i] != "Shrub/Scrub")
    SS_array[i] <-  0
}
MELR_Detection_df <- MELR_Detection_df %>% add_column(Shrub_Scrub = SS_array) # adding shrub/scrub column to MELR data frame

# Scale continuous covariates to have mean = 0
MELR_Detection_df[,"watereucdiscaled"] <- scale(MELR_Detection_df[,"watereucdi"])
MELR_Detection_df[,"roadseucdiscaled"] <- scale(MELR_Detection_df[,"roadseucdi"])
MELR_Detection_df[,"elevclippescaled"] <- scale(MELR_Detection_df[,"elevclippe"])
MELR_Detection_df[,"HumDenProjscaled"] <- scale(MELR_Detection_df[,"HumDenProj"])
MELR_Detection_df[,"StandAgescaled"] <- scale(MELR_Detection_df[,"StandAge"])


MELR_Detection_df <- subset(MELR_Detection_df, select = c(elevclippescaled, HumDenProjscaled, roadseucdiscaled, watereucdiscaled, StandAgescaled, Agriculture,
                                                          Barren_Land, Developed, Deciduous_Forest, Evergreen_Forest, Mixed_Forest,
                                                          Woody_Wetlands, Open_Water, Shrub_Scrub, Detection))
#check for correlation among covariates
cor(MELR_Detection_df, method = "pearson")



# save as a csv
#write.csv(MELR_Detection_df, file = "GLM_Data.csv")

# RUN BESTGLM
#check for correlation among covariates

correlation_matrix <- cor(MELR_Detection_df_2, method = "pearson")
correlation_matrix <- data.frame(correlation_matrix)
colnames(correlation_matrix) <- c("elevclippescaled", "roadseucdiscaled",
"watereucdiscaled", "StandAgescaled",  "Agriculture",  "Barren_Land", "Developed",
"Deciduous_Forest", "Evergreen_Forest", "Mixed_Forest", "Woody_Wetlands",   "Open_Water",
"Shrub_Scrub",   "Detection")
rownames(correlation_matrix) <- c("elevclippescaled", "roadseucdiscaled",
                                  "watereucdiscaled", "StandAgescaled",  "Agriculture",  "Barren_Land", "Developed",
                                  "Deciduous_Forest", "Evergreen_Forest", "Mixed_Forest", "Woody_Wetlands",   "Open_Water",
                                  "Shrub_Scrub",   "Detection")
write.table(correlation_matrix, file = "Correlation_Table.xls")

bestglm <- bestglm(MELR_Detection_df_2, family = binomial, IC = "AIC", TopModels = 15, method = "exhaustive")
bestglm
bestglm$BestModels

# verify bestglm glm results
glm_check <- glm(Detection ~ elevclippescaled + roadseucdiscaled +
                   watereucdiscaled + StandAgescaled + Agriculture + Barren_Land + Developed +
                   Deciduous_Forest + Evergreen_Forest + Mixed_Forest + Open_Water +
                   Woody_Wetlands + Shrub_Scrub, family = binomial, data = MELR_Detection_df_2)

#calculate confidence intervals
#intercept
interceptCI <- cbind(CIlower = mean(0.25538535) - 1.96 * 5 / 10, CIupper = mean(0.25538535) + 1.96 * 5 / 10)
#roads
roadsCI <- cbind(CIlower = mean(-0.02325426) - 1.96 * 5 / 10, CIupper = mean(-0.02325426) + 1.96 * 5 / 10)
#elevation
elevationCI <- cbind(CIlower = mean(-0.37426) - 1.96 * 5 / 10, CIupper = mean(-0.37426) + 1.96 * 5 / 10)
#water
waterCI <- cbind(CIlower = mean(0.80241369) - 1.96 * 5 / 10, CIupper = mean(0.80241369) + 1.96 * 5 / 10)
#standage
SACI <- cbind(CIlower = mean(-0.92173048) - 1.96 * 5 / 10, CIupper = mean(-0.92173048) + 1.96 * 5 / 10)
#Agriculture
ACI <- cbind(CIlower = mean(-0.69087) - 1.96 * 5 / 10, CIupper = mean(-0.69087) + 1.96 * 5 / 10)
#BarrenLand
BLCI <- cbind(CIlower = mean(-2.04280) - 1.96 * 5 / 10, CIupper = mean(-2.04280) + 1.96 * 5 / 10)
# Developed
DCI <- cbind(CIlower = mean(-2.39575) - 1.96 * 5 / 10, CIupper = mean(-2.39575) + 1.96 * 5 / 10)
#Evergreen Forest
EFCI <- cbind(CIlower = mean(0.39176) - 1.96 * 5 / 10, CIupper = mean(0.39176) + 1.96 * 5 / 10)
#Woody Wetlands
WWCI <- cbind(CIlower = mean(0.68915) - 1.96 * 5 / 10, CIupper = mean(0.68915) + 1.96 * 5 / 10)
#Open Water
OWCI <- cbind(CIlower = mean(-1.80848) - 1.96 * 5 / 10, CIupper = mean(-1.80848) + 1.96 * 5 / 10)
#Shrub/Scrub
SSCI <- cbind(CIlower = mean(-0.45099529) - 1.96 * 5 / 10, CIupper = mean(-0.45099529) + 1.96 * 5 / 10)
#Mixed Forest
MFCI <- cbind(CIlower = mean(0.44591) - 1.96 * 5 / 10, CIupper = mean(0.44591) + 1.96 * 5 / 10)
#Deciduous Forest
DFCI <- cbind(CIlower = mean(0.54380) - 1.96 * 5 / 10, CIupper = mean(0.54380) + 1.96 * 5 / 10)

# create habitat suitability map using coefficients
##### calculate Y values and tidy data #####
# add coefficients to random detection columns:
# need NLCD classes to match
# create data frame of all random points bears
Random_Detection_Points_df <- data.frame(read.csv("/Users/kieranmckeag/Desktop/June7_stats.csv"))
# remove unwanted columns from data frame and add a detection column to hold 0's for binomial response
Random_Detection_Points_df$Detection <- 0 # now this data is ready for linear regression
colnames(Random_Detection_Points_df) <- c("OID_","elevclippe", "roadseucdi",  "watereucdi", "HumDenProj", "nlcdproj", "StandAge", "Detection")
Random_Detection_Points_df <- subset(Random_Detection_Points_df, select = c("OID_", "nlcdproj", "roadseucdi", "HumDenProj", "watereucdi", "elevclippe", "StandAge", "Detection"))
Random_Detection_Points_df$OID_ <- 1:nrow(Random_Detection_Points_df)
Random_Detection_Points_df$StandAge[is.na(Random_Detection_Points_df$StandAge)] <- 0
Random_Detection_Points_df$nlcdproj[is.na(Random_Detection_Points_df$nlcdproj)] <- 0
Random_Detection_Points_df$elevclippe[is.na(Random_Detection_Points_df$elevclippe)] <- 35.89214 # using averages for covariate for NA's
Random_Detection_Points_df$roadseucdi[is.na(Random_Detection_Points_df$roadseucdi)] <- 2949.004
Random_Detection_Points_df$watereucdi[is.na(Random_Detection_Points_df$watereucdi)] <- 529.9417
for (i in 1:nrow(Random_Detection_Points_df)) {
  if (Random_Detection_Points_df$nlcdproj[i] == 42){
    Random_Detection_Points_df$nlcdproj[i] <- "Evergreen Forest"
  }else if (Random_Detection_Points_df$nlcdproj[i] == 11){ 
    Random_Detection_Points_df$nlcdproj[i] <- "Open Water"
  }else if (Random_Detection_Points_df$nlcdproj[i] == 21){
    Random_Detection_Points_df$nlcdproj[i] <- "Developed"
  }else if (Random_Detection_Points_df$nlcdproj[i] == 22){
    Random_Detection_Points_df$nlcdproj[i] <- "Developed"
  }else if (Random_Detection_Points_df$nlcdproj[i] == 23){
    Random_Detection_Points_df$nlcdproj[i] <- "Developed"
  }else if (Random_Detection_Points_df$nlcdproj[i] == 24){
    Random_Detection_Points_df$nlcdproj[i] <- "Developed"
  }else if (Random_Detection_Points_df$nlcdproj[i] == 31){
    Random_Detection_Points_df$nlcdproj[i] <- "Barren Land"
  }else if (Random_Detection_Points_df$nlcdproj[i] == 41){
    Random_Detection_Points_df$nlcdproj[i] <- "Deciduous Forest"
  }else if (Random_Detection_Points_df$nlcdproj[i] == 43){
    Random_Detection_Points_df$nlcdproj[i] <- "Mixed Forest"
  }else if (Random_Detection_Points_df$nlcdproj[i] == 52){
    Random_Detection_Points_df$nlcdproj[i] <- "Shrub/Scrub"
  }else if (Random_Detection_Points_df$nlcdproj[i] == 71){
    Random_Detection_Points_df$nlcdproj[i] <- "Shrub/Scrub" #Herbaceous combined 4/3
  }else if (Random_Detection_Points_df$nlcdproj[i] == 81){
    Random_Detection_Points_df$nlcdproj[i] <- "Agriculture"
  }else if (Random_Detection_Points_df$nlcdproj[i] == 82){
    Random_Detection_Points_df$nlcdproj[i] <- "Agriculture"
  }else if (Random_Detection_Points_df$nlcdproj[i] == 90){
    Random_Detection_Points_df$nlcdproj[i] <- "Woody Wetlands"
  }else if (Random_Detection_Points_df$nlcdproj[i] == 95){ 
    Random_Detection_Points_df$nlcdproj[i] <- "Woody Wetlands"
  }else if (Random_Detection_Points_df$nlcdproj[i] == 0){ 
    Random_Detection_Points_df$nlcdproj[i] <- "No Class"
  }
}
sum(is.na(Random_Detection_Points_df$nlcdproj))

no_class_array <- array(NA, dim = nrow(Random_Detection_Points_df))
for (i in 1:nrow(no_class_array)) {
  if (Random_Detection_Points_df$nlcdproj[i] == "No Class")
    no_class_array[i] <-  0
  else if (Random_Detection_Points_df$nlcdproj[i] != "No Class")
    no_class_array[i] <-  0
}
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(No_Class = no_class_array) # adding ag column to habitat suitability data frame


Ag_array_habsuit <- array(NA, dim = nrow(Random_Detection_Points_df)) #agriculture 1's and 0's
for (i in 1:nrow(Ag_array_habsuit)) {
  if (Random_Detection_Points_df$nlcdproj[i] == "Agriculture")
    Ag_array_habsuit[i] <-  -0.69087
  else if (Random_Detection_Points_df$nlcdproj[i] != "Agriculture")
    Ag_array_habsuit[i] <-  0
}
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Agriculture = Ag_array_habsuit) # adding ag column to habitat suitability data frame


BL_array <- array(NA, dim = nrow(Random_Detection_Points_df)) # barren land 1's and 0's
for (i in 1:nrow(BL_array)) {
  if (Random_Detection_Points_df$nlcdproj[i] == "Barren Land")
    BL_array[i] <-  -2.04280
  else if (Random_Detection_Points_df$nlcdproj[i] != "Barren Land")
    BL_array[i] <-  0
}
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Barren_Land = BL_array) # adding barren land column to habitat suitability data frame

Dev_array <- array(NA, dim = nrow(Random_Detection_Points_df)) # developed 1's and 0's
for (i in 1:nrow(Dev_array)) {
  if (Random_Detection_Points_df$nlcdproj[i] == "Developed")
    Dev_array[i] <-  -2.39575
  else if (Random_Detection_Points_df$nlcdproj[i] != "Developed")
    Dev_array[i] <-  0
}
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Developed = Dev_array) # adding developed column to MELR data frame

DF_array <- array(NA, dim = nrow(Random_Detection_Points_df)) # deciduous forest 1's and 0's
for (i in 1:nrow(DF_array)) {
  if (Random_Detection_Points_df$nlcdproj[i] == "Deciduous Forest")
    DF_array[i] <- 0.54380
  else if (Random_Detection_Points_df$nlcdproj[i] != "Deciduous Forest")
    DF_array[i] <-  0
}
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Deciduous_Forest = DF_array) # adding deciduous forest column to MELR data frame

EF_array <- array(NA, dim = nrow(Random_Detection_Points_df)) # evergreen forest 1's and 0's
for (i in 1:nrow(EF_array)) {
  if (Random_Detection_Points_df$nlcdproj[i] == "Evergreen Forest")
    EF_array[i] <-  0.39176
  else if (Random_Detection_Points_df$nlcdproj[i] != "Evergreen Forest")
    EF_array[i] <-  0
}
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Evergreen_Forest = EF_array) # adding evergreen forest column to MELR data frame

MF_array <- array(NA, dim = nrow(Random_Detection_Points_df)) # mixed forest 1's and 0's
for (i in 1:nrow(MF_array)) {
  if (Random_Detection_Points_df$nlcdproj[i] == "Mixed Forest")
    MF_array[i] <-  0.44591
  else if (Random_Detection_Points_df$nlcdproj[i] != "Mixed Forest")
    MF_array[i] <-  0
}
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Mixed_Forest = MF_array) # adding mixed forest column to MELR data frame

WW_array <- array(NA, dim = nrow(Random_Detection_Points_df)) # woody wetlands 1's and 0's
for (i in 1:nrow(WW_array)) {
  if (Random_Detection_Points_df$nlcdproj[i] == "Woody Wetlands")
    WW_array[i] <-  0.68915
  else if (Random_Detection_Points_df$nlcdproj[i] != "Woody Wetlands")
    WW_array[i] <-  0
}
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Woody_Wetlands = WW_array) # adding woody wetlands column to MELR data frame

OW_array <- array(NA, dim = nrow(Random_Detection_Points_df)) # open water 1's and 0's
for (i in 1:nrow(OW_array)) {
  if (Random_Detection_Points_df$nlcdproj[i] == "Open Water")
    OW_array[i] <-  -1.80848
  else if (Random_Detection_Points_df$nlcdproj[i] != "Open Water")
    OW_array[i] <-  0
}
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Open_Water = OW_array) # adding open water column to MELR data frame

SS_array <- array(NA, dim = nrow(Random_Detection_Points_df)) # shrub/scrub 1's and 0's
for (i in 1:nrow(SS_array)) {
  if (Random_Detection_Points_df$nlcdproj[i] == "Shrub/Scrub")
    SS_array[i] <-  -0.45099529
  else if (Random_Detection_Points_df$nlcdproj[i] != "Shrub/Scrub")
    SS_array[i] <-  0
}
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Shrub_Scrub = SS_array) # adding shrub/scrub column to MELR data frame

# Scale continuous covariates to have mean = 0
Random_Detection_Points_df[,"watereucdiscaled"] <- scale(Random_Detection_Points_df[,"watereucdi"])
Random_Detection_Points_df[,"roadseucdiscaled"] <- scale(Random_Detection_Points_df[,"roadseucdi"])
Random_Detection_Points_df[,"elevclippescaled"] <- scale(Random_Detection_Points_df[,"elevclippe"])
Random_Detection_Points_df[,"StandAgescaled"] <- scale(Random_Detection_Points_df[,"StandAge"])

DW_array <- array(NA, dim = nrow(Random_Detection_Points_df)) # distance to water array
for (i in 1:nrow(DW_array)) {
  Random_Detection_Points_df$DW_array[i] <- 0.80241369 * Random_Detection_Points_df$watereucdiscaled[i]
}
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(distwater = DW_array) # adding distance to water column to habsuitpoints data frame

DR_array <- array(NA, dim = nrow(Random_Detection_Points_df)) # distance to roads array
for (i in 1:nrow(DR_array)) {
  Random_Detection_Points_df$DR_array[i] <- -0.02325426 * Random_Detection_Points_df$roadseucdiscaled[i]
}
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(distroads = DR_array) # adding distance to roads column to habsuitpoints data frame

# make sure column matches! You spent three hours on errors because you forgot an underscore in the csv!
SA_array <- array(NA, dim = nrow(Random_Detection_Points_df)) # stand age array
for (i in 1:nrow(SA_array)) {
  Random_Detection_Points_df$SA_array[i] <- -0.92173048 * Random_Detection_Points_df$StandAgescaled[i]
}
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Stand_Age = SA_array) # adding stand age column to habsuitpoints data frame

E_array <- array(NA, dim = nrow(Random_Detection_Points_df)) # elevation array
for (i in 1:nrow(E_array)) {
  Random_Detection_Points_df$E_array[i] <- 0.37443694 * Random_Detection_Points_df$elevclippescaled[i]
}
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(elevation = E_array) # adding elevation column to habsuitpoints data frame

# add intercept to all rows
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Intercept = 0.25538535)

# may not need this habsuitpoints <- habsuitpoints %>% add_column(Y = NA) # add y value column

# tidy data to calculate y value
Random_Detection_Points_df <- subset(Random_Detection_Points_df, select = -c(distwater, distroads, Stand_Age, elevation))

# calculate Y values for all
#bestglm$BestModels
#array <- array(NA, dim = nrow(Random_Detection_Points_df))
#for (i in 1:nrow(Random_Detection_Points_df)) {
 # array[i] <- Random_Detection_Points_df$Agriculture[i] + Random_Detection_Points_df$Barren_Land[i] + Random_Detection_Points_df$Developed[i] + 
  #  Random_Detection_Points_df$Deciduous_Forest[i] + Random_Detection_Points_df$Evergreen_Forest[i] + Random_Detection_Points_df$Mixed_Forest[i] +
   # Random_Detection_Points_df$Woody_Wetlands[i] + Random_Detection_Points_df$Open_Water[i] + Random_Detection_Points_df$Shrub_Scrub[i] + 
    #Random_Detection_Points_df$DW_array[i] + Random_Detection_Points_df$DR_array[i] + Random_Detection_Points_df$SA_array[i] + 
    #Random_Detection_Points_df$E_array[i] + Random_Detection_Points_df$Intercept[i]
#}
#Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Y = array)
#Random_Detection_Points_df # Y = Y values

# Transform y-values to 0-1.0 using logit transformation
#Random_Detection_Points_df$Ytran <- (1/(1 + exp(-Random_Detection_Points_df$Y)))

# calculate Y values for top model
bestglm$BestModels # no deciduous forest or mixed forest
array <- array(NA, dim = nrow(Random_Detection_Points_df))
for (i in 1:nrow(Random_Detection_Points_df)) {
  array[i] <- Random_Detection_Points_df$Agriculture[i] + Random_Detection_Points_df$Barren_Land[i] + Random_Detection_Points_df$Developed[i] + 
    Random_Detection_Points_df$Evergreen_Forest[i] + 
    Random_Detection_Points_df$Woody_Wetlands[i] + Random_Detection_Points_df$Open_Water[i] + Random_Detection_Points_df$Shrub_Scrub[i] + 
    Random_Detection_Points_df$DW_array[i] + Random_Detection_Points_df$DR_array[i] + Random_Detection_Points_df$SA_array[i] + 
    Random_Detection_Points_df$E_array[i] + Random_Detection_Points_df$Intercept[i]
}
Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Ytop = array)
Random_Detection_Points_df # Y = Y values

# Transform y-values to 0-1.0 using logit transformation
Random_Detection_Points_df$Ytoptran <- (1/(1 + exp(-Random_Detection_Points_df$Ytop)))

# calculate Y values 2nd model
bestglm$BestModels # no barren land open water
array <- array(NA, dim = nrow(Random_Detection_Points_df))
for (i in 1:nrow(Random_Detection_Points_df)) {
  array[i] <- Random_Detection_Points_df$Agriculture[i] +  Random_Detection_Points_df$Developed[i] + 
    Random_Detection_Points_df$Deciduous_Forest[i] + Random_Detection_Points_df$Evergreen_Forest[i] + Random_Detection_Points_df$Mixed_Forest[i] +
    Random_Detection_Points_df$Woody_Wetlands[i] +  Random_Detection_Points_df$Shrub_Scrub[i] + 
    Random_Detection_Points_df$DW_array[i] + Random_Detection_Points_df$DR_array[i] + Random_Detection_Points_df$SA_array[i] + 
    Random_Detection_Points_df$E_array[i] + Random_Detection_Points_df$Intercept[i]
}

Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Y2nd = array)
Random_Detection_Points_df # Y = Y values

# Transform y-values to 0-1.0 using logit transformation
Random_Detection_Points_df$Y2ndtran <- (1/(1 + exp(-Random_Detection_Points_df$Y2nd)))

# calculate Y values for 3rd model
bestglm$BestModels # no deciduous forest, woody wetlands
array <- array(NA, dim = nrow(Random_Detection_Points_df))
for (i in 1:nrow(Random_Detection_Points_df)) {
  array[i] <- Random_Detection_Points_df$Agriculture[i] + Random_Detection_Points_df$Barren_Land[i] + Random_Detection_Points_df$Developed[i] + 
    Random_Detection_Points_df$Evergreen_Forest[i] + Random_Detection_Points_df$Mixed_Forest[i] +
    Random_Detection_Points_df$Open_Water[i] + Random_Detection_Points_df$Shrub_Scrub[i] + 
    Random_Detection_Points_df$DW_array[i] + Random_Detection_Points_df$DR_array[i] + Random_Detection_Points_df$SA_array[i] + 
    Random_Detection_Points_df$E_array[i] + Random_Detection_Points_df$Intercept[i]
}

Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Y3rd = array)
Random_Detection_Points_df # Y = Y values

# Transform y-values to 0-1.0 using logit transformation
Random_Detection_Points_df$Y3rdtran <- (1/(1 + exp(-Random_Detection_Points_df$Y3rd)))

# calculate Y values for 4th model
bestglm$BestModels # no deciduous forest, evergreen forest
array <- array(NA, dim = nrow(Random_Detection_Points_df))
for (i in 1:nrow(Random_Detection_Points_df)) {
  array[i] <- Random_Detection_Points_df$Agriculture[i] + Random_Detection_Points_df$Barren_Land[i] + Random_Detection_Points_df$Developed[i] + 
    Random_Detection_Points_df$Mixed_Forest[i] +
    Random_Detection_Points_df$Woody_Wetlands[i] + Random_Detection_Points_df$Open_Water[i] + Random_Detection_Points_df$Shrub_Scrub[i] + 
    Random_Detection_Points_df$DW_array[i] + Random_Detection_Points_df$DR_array[i] + Random_Detection_Points_df$SA_array[i] + 
    Random_Detection_Points_df$E_array[i] + Random_Detection_Points_df$Intercept[i]
}

Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Y4th = array)
Random_Detection_Points_df # Y = Y values

# Transform y-values to 0-1.0 using logit transformation
Random_Detection_Points_df$Y4thtran <- (1/(1 + exp(-Random_Detection_Points_df$Y4th)))

# calculate Y values for 5th model
bestglm$BestModels # no agriculture
array <- array(NA, dim = nrow(Random_Detection_Points_df))
for (i in 1:nrow(Random_Detection_Points_df)) {
  array[i] <- Random_Detection_Points_df$Barren_Land[i] + Random_Detection_Points_df$Developed[i] + 
    Random_Detection_Points_df$Deciduous_Forest[i] + Random_Detection_Points_df$Evergreen_Forest[i] + Random_Detection_Points_df$Mixed_Forest[i] +
    Random_Detection_Points_df$Woody_Wetlands[i] + Random_Detection_Points_df$Open_Water[i] + Random_Detection_Points_df$Shrub_Scrub[i] + 
    Random_Detection_Points_df$DW_array[i] + Random_Detection_Points_df$DR_array[i] + Random_Detection_Points_df$SA_array[i] + 
    Random_Detection_Points_df$E_array[i] + Random_Detection_Points_df$Intercept[i]
}

Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Y5th = array)
Random_Detection_Points_df # Y = Y values

# Transform y-values to 0-1.0 using logit transformation
Random_Detection_Points_df$Y5thtran <- (1/(1 + exp(-Random_Detection_Points_df$Y5th)))

# calculate Y values for 6th model
bestglm$BestModels # no barren land, 
array <- array(NA, dim = nrow(Random_Detection_Points_df))
for (i in 1:nrow(Random_Detection_Points_df)) {
  array[i] <- Random_Detection_Points_df$Agriculture[i] + Random_Detection_Points_df$Developed[i] + 
    Random_Detection_Points_df$Deciduous_Forest[i] + Random_Detection_Points_df$Evergreen_Forest[i] + Random_Detection_Points_df$Mixed_Forest[i] +
    Random_Detection_Points_df$Woody_Wetlands[i] + Random_Detection_Points_df$Open_Water[i] + Random_Detection_Points_df$Shrub_Scrub[i] + 
    Random_Detection_Points_df$DW_array[i] + Random_Detection_Points_df$DR_array[i] + Random_Detection_Points_df$SA_array[i] + 
    Random_Detection_Points_df$E_array[i] + Random_Detection_Points_df$Intercept[i]
}

Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Y6th = array)
Random_Detection_Points_df # Y = Y values

# Transform y-values to 0-1.0 using logit transformation
Random_Detection_Points_df$Y6thtran <- (1/(1 + exp(-Random_Detection_Points_df$Y6th)))

# calculate Y values for 7th model
bestglm$BestModels # no developed,
array <- array(NA, dim = nrow(Random_Detection_Points_df))
for (i in 1:nrow(Random_Detection_Points_df)) {
  array[i] <- Random_Detection_Points_df$Agriculture[i] + Random_Detection_Points_df$Barren_Land[i] +  
    Random_Detection_Points_df$Deciduous_Forest[i] + Random_Detection_Points_df$Evergreen_Forest[i] + Random_Detection_Points_df$Mixed_Forest[i] +
    Random_Detection_Points_df$Woody_Wetlands[i] + Random_Detection_Points_df$Open_Water[i] + Random_Detection_Points_df$Shrub_Scrub[i] + 
    Random_Detection_Points_df$DW_array[i] + Random_Detection_Points_df$DR_array[i] + Random_Detection_Points_df$SA_array[i] + 
    Random_Detection_Points_df$E_array[i] + Random_Detection_Points_df$Intercept[i]
}

Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Y7th = array)
Random_Detection_Points_df # Y = Y values

# Transform y-values to 0-1.0 using logit transformation
Random_Detection_Points_df$Y7thtran <- (1/(1 + exp(-Random_Detection_Points_df$Y7th)))

# calculate Y values for 8th model
bestglm$BestModels # no deciduous
array <- array(NA, dim = nrow(Random_Detection_Points_df))
for (i in 1:nrow(Random_Detection_Points_df)) {
  array[i] <- Random_Detection_Points_df$Agriculture[i] + Random_Detection_Points_df$Barren_Land[i] + Random_Detection_Points_df$Developed[i] + 
    Random_Detection_Points_df$Evergreen_Forest[i] + Random_Detection_Points_df$Mixed_Forest[i] +
    Random_Detection_Points_df$Woody_Wetlands[i] + Random_Detection_Points_df$Open_Water[i] + Random_Detection_Points_df$Shrub_Scrub[i] + 
    Random_Detection_Points_df$DW_array[i] + Random_Detection_Points_df$DR_array[i] + Random_Detection_Points_df$SA_array[i] + 
    Random_Detection_Points_df$E_array[i] + Random_Detection_Points_df$Intercept[i]
}

Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Y8th = array)
Random_Detection_Points_df # Y = Y values

# Transform y-values to 0-1.0 using logit transformation
Random_Detection_Points_df$Y8thtran <- (1/(1 + exp(-Random_Detection_Points_df$Y8th)))

# calculate Y values for 9th model
bestglm$BestModels # no evergreen forest
array <- array(NA, dim = nrow(Random_Detection_Points_df))
for (i in 1:nrow(Random_Detection_Points_df)) {
  array[i] <- Random_Detection_Points_df$Agriculture[i] + Random_Detection_Points_df$Barren_Land[i] + Random_Detection_Points_df$Developed[i] + 
    Random_Detection_Points_df$Deciduous_Forest[i] + Random_Detection_Points_df$Mixed_Forest[i] +
    Random_Detection_Points_df$Woody_Wetlands[i] + Random_Detection_Points_df$Open_Water[i] + Random_Detection_Points_df$Shrub_Scrub[i] + 
    Random_Detection_Points_df$DW_array[i] + Random_Detection_Points_df$DR_array[i] + Random_Detection_Points_df$SA_array[i] + 
    Random_Detection_Points_df$E_array[i] + Random_Detection_Points_df$Intercept[i]
}

Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Y9th = array)
Random_Detection_Points_df # Y = Y values

# Transform y-values to 0-1.0 using logit transformation
Random_Detection_Points_df$Y9thtran <- (1/(1 + exp(-Random_Detection_Points_df$Y9th)))

# calculate Y values for 10th model
bestglm$BestModels # no mixed forest
array <- array(NA, dim = nrow(Random_Detection_Points_df))
for (i in 1:nrow(Random_Detection_Points_df)) {
  array[i] <- Random_Detection_Points_df$Agriculture[i] + Random_Detection_Points_df$Barren_Land[i] + Random_Detection_Points_df$Developed[i] + 
    Random_Detection_Points_df$Deciduous_Forest[i] + Random_Detection_Points_df$Evergreen_Forest[i] + 
    Random_Detection_Points_df$Woody_Wetlands[i] + Random_Detection_Points_df$Open_Water[i] + Random_Detection_Points_df$Shrub_Scrub[i] + 
    Random_Detection_Points_df$DW_array[i] + Random_Detection_Points_df$DR_array[i] + Random_Detection_Points_df$SA_array[i] + 
    Random_Detection_Points_df$E_array[i] + Random_Detection_Points_df$Intercept[i]
}

Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Y10th = array)
Random_Detection_Points_df # Y = Y values

# Transform y-values to 0-1.0 using logit transformation
Random_Detection_Points_df$Y10thtran <- (1/(1 + exp(-Random_Detection_Points_df$Y10th)))

# calculate Y values for 11th model
bestglm$BestModels # no woody wetlands
array <- array(NA, dim = nrow(Random_Detection_Points_df))
for (i in 1:nrow(Random_Detection_Points_df)) {
  array[i] <- Random_Detection_Points_df$Agriculture[i] + Random_Detection_Points_df$Barren_Land[i] + Random_Detection_Points_df$Developed[i] + 
    Random_Detection_Points_df$Deciduous_Forest[i] + Random_Detection_Points_df$Evergreen_Forest[i] + 
    Random_Detection_Points_df$Open_Water[i] + Random_Detection_Points_df$Shrub_Scrub[i] + 
    Random_Detection_Points_df$DW_array[i] + Random_Detection_Points_df$DR_array[i] + Random_Detection_Points_df$SA_array[i] + 
    Random_Detection_Points_df$E_array[i] + Random_Detection_Points_df$Intercept[i]
}

Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Y11th = array)
Random_Detection_Points_df # Y = Y values

# Transform y-values to 0-1.0 using logit transformation
Random_Detection_Points_df$Y11thtran <- (1/(1 + exp(-Random_Detection_Points_df$Y11th)))

# calculate Y values for 12th model
bestglm$BestModels # no open water
array <- array(NA, dim = nrow(Random_Detection_Points_df))
for (i in 1:nrow(Random_Detection_Points_df)) {
  array[i] <- Random_Detection_Points_df$Agriculture[i] + Random_Detection_Points_df$Barren_Land[i] + Random_Detection_Points_df$Developed[i] + 
    Random_Detection_Points_df$Deciduous_Forest[i] + Random_Detection_Points_df$Evergreen_Forest[i] + 
    Random_Detection_Points_df$Woody_Wetlands[i] + Random_Detection_Points_df$Shrub_Scrub[i] + 
    Random_Detection_Points_df$DW_array[i] + Random_Detection_Points_df$DR_array[i] + Random_Detection_Points_df$SA_array[i] + 
    Random_Detection_Points_df$E_array[i] + Random_Detection_Points_df$Intercept[i]
}

Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Y12th = array)
Random_Detection_Points_df # Y = Y values

# Transform y-values to 0-1.0 using logit transformation
Random_Detection_Points_df$Y12thtran <- (1/(1 + exp(-Random_Detection_Points_df$Y12th)))

# calculate Y values for 13th model
bestglm$BestModels # no shrub scrub
array <- array(NA, dim = nrow(Random_Detection_Points_df))
for (i in 1:nrow(Random_Detection_Points_df)) {
  array[i] <- Random_Detection_Points_df$Agriculture[i] + Random_Detection_Points_df$Barren_Land[i] + Random_Detection_Points_df$Developed[i] + 
    Random_Detection_Points_df$Deciduous_Forest[i] + Random_Detection_Points_df$Evergreen_Forest[i] + 
    Random_Detection_Points_df$Woody_Wetlands[i] + Random_Detection_Points_df$Open_Water[i] +  
    Random_Detection_Points_df$DW_array[i] + Random_Detection_Points_df$DR_array[i] + Random_Detection_Points_df$SA_array[i] + 
    Random_Detection_Points_df$E_array[i] + Random_Detection_Points_df$Intercept[i]
}

Random_Detection_Points_df <- Random_Detection_Points_df %>% add_column(Y13th = array)
Random_Detection_Points_df # Y = Y values

# Transform y-values to 0-1.0 ussing logit transformation
Random_Detection_Points_df$Y13thtran <- (1/(1 + exp(-Random_Detection_Points_df$Y13th)))


# create csv with all the transformed values to put into GIS
Yval <- subset(Random_Detection_Points_df, select = c(Ytop, Ytoptran, Y2nd, Y2ndtran, Y3rd, Y3rdtran, Y4th, Y4thtran, Y5th, Y5thtran, 
                                         Y6th, Y6thtran, Y7th, Y7thtran, Y8th,
                                         Y8thtran, Y9th, Y9thtran, Y10th, Y10thtran, 
                                         Y11th, Y11thtran, Y12th, Y12thtran, Y13th, Y13thtran))
write.csv(Yval, file = "TransformedYValues_June11.csv") # send to GIS to make habitat suitability maps

write.csv(Random_Detection_Points_df, file = "RandomPointsDataFINAL.csv")


##### Plots #####
# maybe consider some Jitter here?

# NLCD vs. Y-Values
ggplot(Random_Detection_Points_df, aes(x = nlcdproj, y = Ytoptran)) + geom_point() + scale_x_discrete(labels = c("Agriculture" = "Ag", 
                                                                                                                "Barren Land" = "BL", 
                                                                                                                "Deciduous Forest" = "DF",
                                                                                                                "Developed" = "Dev", 
                                                                                                                "Evergreen Forest" = "EF", 
                                                                                                                "Mixed Forest" = "MF",
                                                                                                                "No Class" = "NC", 
                                                                                                                "Open Water" = "OW", 
                                                                                                                "Shrub/Scrub" = "SS", 
                                                                                                                "Woody Wetlands" = "WW"))
ggsave(filename = "nlcdvsy.pdf")
# elevation
ggplot(Random_Detection_Points_df, aes(x = elevclippe, y = Ytoptran)) + geom_point()
ggsave(filename = "elevationvsy.pdf")
# distance to roads
ggplot(Random_Detection_Points_df, aes(x = roadseucdi, y = Ytoptran)) + geom_point()
ggsave(filename = "roadsvsy.pdf")
# distance to water
ggplot(Random_Detection_Points_df, aes(x = watereucdi, y = Ytoptran)) + geom_point()
ggsave(filename = "watervsy.pdf")
# Stand Age
ggplot(Random_Detection_Points_df, aes(x = StandAge, y = Ytoptran)) + geom_point()
ggsave(filename = "StandAgevsy.pdf")
# Intercept doesn't really do much here lol
ggplot(Random_Detection_Points_df, aes(x = Ytoptran, y = Intercept)) + geom_point()
ggsave(filename = "Interceptvsy.pdf")

##### Simulate Viability loss with greater carrying capacity #####
## Creating Data ##
## Create distributions of bears in MRB, OS, EG, and AP
# Mobile River Basin (MRB)
MRB_Obs_Hetero_Gamma <- rgamma(86, 23.592, rate = 69.388) # MRB Observed Hetero w/gamma distribution #

## Create table of simulated data of fake MRB bears ##
Fake_MRB_Bears <- cbind(MRB_Obs_Hetero_Gamma)
colnames(Fake_MRB_Bears) <- c("Observed_Heterozygosity")

# Mortality:
x <- c(1, 0.99, 0.98, 0.97, 0.96) ## values from Clark et al. paper
# above line creates values for mortality values

# Scenario One: 1 male bear home range acquired (65km^2, ~9 more bears)
# Create arrays to hold values
MRB_change_Hetero_HA_iterations <- array(NA, dim = c(1,100))
HA_Ni_Array <-array(NA, dim = c(1,100))

for (Iteration in 1:100) {
  for (PopNum in 1:100) {
    MortalityValues <- sample(x, 20, replace = TRUE)
    
    # calculate change in population size
    Gen1 <- (95) * MortalityValues[[1]]
    Gen2 <- (Gen1) * MortalityValues[[2]]
    Gen3 <- (Gen2) * MortalityValues[[3]]
    Gen4 <- (Gen3) * MortalityValues[[4]]
    Gen5 <- (Gen4) * MortalityValues[[5]]
    Gen6 <- (Gen5) * MortalityValues[[6]]
    Gen7 <- (Gen6) * MortalityValues[[7]]
    Gen8 <- (Gen7) * MortalityValues[[8]]
    Gen9 <- (Gen8) * MortalityValues[[9]]
    Gen10 <- (Gen9) * MortalityValues[[10]]
    Gen11 <- (Gen10) * MortalityValues[[11]]
    Gen12 <- (Gen11) * MortalityValues[[12]]
    Gen13 <- (Gen12) * MortalityValues[[13]]
    Gen14 <- (Gen13) * MortalityValues[[14]]
    Gen15 <- (Gen14) * MortalityValues[[15]]
    Gen16 <- (Gen15) * MortalityValues[[16]]
    Gen17 <- (Gen16) * MortalityValues[[17]]
    Gen18 <- (Gen17) * MortalityValues[[18]]
    Gen19 <- (Gen18) * MortalityValues[[19]]
    Gen20 <- (Gen19) * MortalityValues[[20]]
    Gen20
    HA_Ni_Array[,PopNum] <- Gen20
    
    # Create array of all generation's population number
    HA_GenPop <- array(c(95, Gen1,Gen2,Gen3,Gen4,Gen5,Gen6,Gen7,Gen8,Gen9,Gen10,Gen11,Gen12,Gen13,Gen14,Gen15,Gen16,Gen17,Gen18,Gen19,Gen20))
    
    # Create Ne Vector
    Sequence_of_Ne <- array(c(0.355,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)) # creating within loop because it will change each iteration
    for (i in 2:21) { # decreasing to 0.355 because population grew from 86
      if (HA_GenPop[i] > HA_GenPop[i - 1]) {
        Sequence_of_Ne[i] <-  Sequence_of_Ne[i - 1] - 0.005
      } else if (HA_GenPop[i] < HA_GenPop[i - 1]){
        Sequence_of_Ne[i] <- Sequence_of_Ne[i - 1] + 0.005
      } else if (HA_GenPop[i] == HA_GenPop[i - 1]){
        Sequence_of_Ne[i] <- Sequence_of_Ne[i - 1]
      }
    }
    Sequence_of_Ne
  }
  
  ## Calculate expected heterozygosity through twenty generations if no methods are conducted
  HA_HeteroArray <- array(c(mean(MRB_Obs_Hetero_Gamma),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  for (j in 2:21) {
    for (i in length(MortalityValues)) {
      for (Ne in 2:21) { # below line is the equation for calcualting heterozygosity
        HA_HeteroArray[[j]] <- (1 - 1/(2 * ((95 * MortalityValues[i] * Sequence_of_Ne[Ne])))) * (HA_HeteroArray[j-1]) 
      }
    }## changes in MRB's heterozygosity with varrying Ne with a timescale of 20 generations
    # Omitted the exponent because the equation is already indexed 20 times with mortality values
    MRB_change_Hetero_HA <- HA_HeteroArray[2:21]
    MRB_change_Hetero_HA <- array(MRB_change_Hetero_HA, dim = c(1,20))
  }
  MRB_change_Hetero_HA_iterations[,Iteration] <- MRB_change_Hetero_HA[1,20]
}
MRB_change_Hetero_HA_iterations

# calculate means for no methods heterozygosity changes
mean(MRB_change_Hetero_HA_iterations)
Gen20

# Scenario Two: 2 male bear home range acquired (130km^2, ~18 more bears)
# Create arrays to hold values
MRB_change_Hetero_HA2_iterations <- array(NA, dim = c(1,100))
HA2_Ni_Array <-array(NA, dim = c(1,100))

for (Iteration in 1:100) {
  for (PopNum in 1:100) {
    MortalityValues <- sample(x, 20, replace = TRUE)
    
    # calculate change in population size
    Gen1 <- (104) * MortalityValues[[1]]
    Gen2 <- (Gen1) * MortalityValues[[2]]
    Gen3 <- (Gen2) * MortalityValues[[3]]
    Gen4 <- (Gen3) * MortalityValues[[4]]
    Gen5 <- (Gen4) * MortalityValues[[5]]
    Gen6 <- (Gen5) * MortalityValues[[6]]
    Gen7 <- (Gen6) * MortalityValues[[7]]
    Gen8 <- (Gen7) * MortalityValues[[8]]
    Gen9 <- (Gen8) * MortalityValues[[9]]
    Gen10 <- (Gen9) * MortalityValues[[10]]
    Gen11 <- (Gen10) * MortalityValues[[11]]
    Gen12 <- (Gen11) * MortalityValues[[12]]
    Gen13 <- (Gen12) * MortalityValues[[13]]
    Gen14 <- (Gen13) * MortalityValues[[14]]
    Gen15 <- (Gen14) * MortalityValues[[15]]
    Gen16 <- (Gen15) * MortalityValues[[16]]
    Gen17 <- (Gen16) * MortalityValues[[17]]
    Gen18 <- (Gen17) * MortalityValues[[18]]
    Gen19 <- (Gen18) * MortalityValues[[19]]
    Gen20 <- (Gen19) * MortalityValues[[20]]
    Gen20
    HA2_Ni_Array[,PopNum] <- Gen20
    
    # Create array of all generation's population number
    HA2_GenPop <- array(c(104, Gen1,Gen2,Gen3,Gen4,Gen5,Gen6,Gen7,Gen8,Gen9,Gen10,Gen11,Gen12,Gen13,Gen14,Gen15,Gen16,Gen17,Gen18,Gen19,Gen20))
    
    # Create Ne Vector
    Sequence_of_Ne <- array(c(0.355,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)) # creating within loop because it will change each iteration
    for (i in 2:21) { # decreasing to 0.355 because population grew from 86
      if (HA2_GenPop[i] > HA2_GenPop[i - 1]) {
        Sequence_of_Ne[i] <-  Sequence_of_Ne[i - 1] - 0.005
      } else if (HA2_GenPop[i] < HA2_GenPop[i - 1]){
        Sequence_of_Ne[i] <- Sequence_of_Ne[i - 1] + 0.005
      } else if (HA2_GenPop[i] == HA2_GenPop[i - 1]){
        Sequence_of_Ne[i] <- Sequence_of_Ne[i - 1]
      }
    }
    Sequence_of_Ne
  }
  
  ## Calculate expected heterozygosity through twenty generations if no methods are conducted
  HA2_HeteroArray <- array(c(mean(MRB_Obs_Hetero_Gamma),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  for (j in 2:21) {
    for (i in length(MortalityValues)) {
      for (Ne in 2:21) { # below line is the equation for calcualting heterozygosity
        HA2_HeteroArray[[j]] <- (1 - 1/(2 * ((104 * MortalityValues[i] * Sequence_of_Ne[Ne])))) * (HA2_HeteroArray[j-1]) 
      }
    }## changes in MRB's heterozygosity with varrying Ne with a timescale of 20 generations
    # Omitted the exponent because the equation is already indexed 20 times with mortality values
    MRB_change_Hetero_HA2 <- HA2_HeteroArray[2:21]
    MRB_change_Hetero_HA2 <- array(MRB_change_Hetero_HA2, dim = c(1,20))
  }
  MRB_change_Hetero_HA2_iterations[,Iteration] <- MRB_change_Hetero_HA2[1,20]
}
MRB_change_Hetero_HA2_iterations

# calculate means for no methods heterozygosity changes
mean(MRB_change_Hetero_HA2_iterations)
Gen20

# Scenario Three: 3 male bear home range acquired (195km^2, ~27 more bears)
# Create arrays to hold values
MRB_change_Hetero_HA3_iterations <- array(NA, dim = c(1,100))
HA3_Ni_Array <-array(NA, dim = c(1,100))

for (Iteration in 1:100) {
  for (PopNum in 1:100) {
    MortalityValues <- sample(x, 20, replace = TRUE)
    
    # calculate change in population size
    Gen1 <- (113) * MortalityValues[[1]]
    Gen2 <- (Gen1) * MortalityValues[[2]]
    Gen3 <- (Gen2) * MortalityValues[[3]]
    Gen4 <- (Gen3) * MortalityValues[[4]]
    Gen5 <- (Gen4) * MortalityValues[[5]]
    Gen6 <- (Gen5) * MortalityValues[[6]]
    Gen7 <- (Gen6) * MortalityValues[[7]]
    Gen8 <- (Gen7) * MortalityValues[[8]]
    Gen9 <- (Gen8) * MortalityValues[[9]]
    Gen10 <- (Gen9) * MortalityValues[[10]]
    Gen11 <- (Gen10) * MortalityValues[[11]]
    Gen12 <- (Gen11) * MortalityValues[[12]]
    Gen13 <- (Gen12) * MortalityValues[[13]]
    Gen14 <- (Gen13) * MortalityValues[[14]]
    Gen15 <- (Gen14) * MortalityValues[[15]]
    Gen16 <- (Gen15) * MortalityValues[[16]]
    Gen17 <- (Gen16) * MortalityValues[[17]]
    Gen18 <- (Gen17) * MortalityValues[[18]]
    Gen19 <- (Gen18) * MortalityValues[[19]]
    Gen20 <- (Gen19) * MortalityValues[[20]]
    Gen20
    HA3_Ni_Array[,PopNum] <- Gen20
    
    # Create array of all generation's population number
    HA3_GenPop <- array(c(113, Gen1,Gen2,Gen3,Gen4,Gen5,Gen6,Gen7,Gen8,Gen9,Gen10,Gen11,Gen12,Gen13,Gen14,Gen15,Gen16,Gen17,Gen18,Gen19,Gen20))
    
    # Create Ne Vector
    Sequence_of_Ne <- array(c(0.355,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)) # creating within loop because it will change each iteration
    for (i in 2:21) { # decreasing to 0.355 because population grew from 86
      if (HA3_GenPop[i] > HA3_GenPop[i - 1]) {
        Sequence_of_Ne[i] <-  Sequence_of_Ne[i - 1] - 0.005
      } else if (HA3_GenPop[i] < HA3_GenPop[i - 1]){
        Sequence_of_Ne[i] <- Sequence_of_Ne[i - 1] + 0.005
      } else if (HA3_GenPop[i] == HA3_GenPop[i - 1]){
        Sequence_of_Ne[i] <- Sequence_of_Ne[i - 1]
      }
    }
    Sequence_of_Ne
  }
  
  ## Calculate expected heterozygosity through twenty generations if no methods are conducted
  HA3_HeteroArray <- array(c(mean(MRB_Obs_Hetero_Gamma),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  for (j in 2:21) {
    for (i in length(MortalityValues)) {
      for (Ne in 2:21) { # below line is the equation for calcualting heterozygosity
        HA3_HeteroArray[[j]] <- (1 - 1/(2 * ((113 * MortalityValues[i] * Sequence_of_Ne[Ne])))) * (HA3_HeteroArray[j-1]) 
      }
    }## changes in MRB's heterozygosity with varrying Ne with a timescale of 20 generations
    # Omitted the exponent because the equation is already indexed 20 times with mortality values
    MRB_change_Hetero_HA3 <- HA3_HeteroArray[2:21]
    MRB_change_Hetero_HA3 <- array(MRB_change_Hetero_HA3, dim = c(1,20))
  }
  MRB_change_Hetero_HA3_iterations[,Iteration] <- MRB_change_Hetero_HA3[1,20]
}
MRB_change_Hetero_HA3_iterations

# calculate means for no methods heterozygosity changes
mean(MRB_change_Hetero_HA3_iterations)
Gen20

##### No Methods Conducted #####
#  MRB Expected Heterozygosity if no methods are conducted
# Create arrays to hold values
MRB_change_Hetero_nm_iterations <- array(NA, dim = c(1,100))
NM_Ni_Array <-array(NA, dim = c(1,100))

for (Iteration in 1:100) {
  for (PopNum in 1:100) {
    MortalityValues <- sample(x, 20, replace = TRUE)
    
    # calculate change in population size
    Gen1 <- (86) * MortalityValues[[1]]
    Gen2 <- (Gen1) * MortalityValues[[2]]
    Gen3 <- (Gen2) * MortalityValues[[3]]
    Gen4 <- (Gen3) * MortalityValues[[4]]
    Gen5 <- (Gen4) * MortalityValues[[5]]
    Gen6 <- (Gen5) * MortalityValues[[6]]
    Gen7 <- (Gen6) * MortalityValues[[7]]
    Gen8 <- (Gen7) * MortalityValues[[8]]
    Gen9 <- (Gen8) * MortalityValues[[9]]
    Gen10 <- (Gen9) * MortalityValues[[10]]
    Gen11 <- (Gen10) * MortalityValues[[11]]
    Gen12 <- (Gen11) * MortalityValues[[12]]
    Gen13 <- (Gen12) * MortalityValues[[13]]
    Gen14 <- (Gen13) * MortalityValues[[14]]
    Gen15 <- (Gen14) * MortalityValues[[15]]
    Gen16 <- (Gen15) * MortalityValues[[16]]
    Gen17 <- (Gen16) * MortalityValues[[17]]
    Gen18 <- (Gen17) * MortalityValues[[18]]
    Gen19 <- (Gen18) * MortalityValues[[19]]
    Gen20 <- (Gen19) * MortalityValues[[20]]
    Gen20
    NM_Ni_Array[,PopNum] <- Gen20
    
    # Create array of all generation's population number
    NM_GenPop <- array(c(86, Gen1,Gen2,Gen3,Gen4,Gen5,Gen6,Gen7,Gen8,Gen9,Gen10,Gen11,Gen12,Gen13,Gen14,Gen15,Gen16,Gen17,Gen18,Gen19,Gen20))
    
    # Create Ne Vector
    Sequence_of_Ne <- array(c(0.36,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)) # creating within loop because it will change each iteration
    for (i in 2:21) {
      if (NM_GenPop[i] > NM_GenPop[i - 1]) {
        Sequence_of_Ne[i] <-  Sequence_of_Ne[i - 1] - 0.005
      } else if (NM_GenPop[i] < NM_GenPop[i - 1]){
        Sequence_of_Ne[i] <- Sequence_of_Ne[i - 1] + 0.005
      } else if (NM_GenPop[i] == NM_GenPop[i - 1]){
        Sequence_of_Ne[i] <- Sequence_of_Ne[i - 1]
      }
    }
    Sequence_of_Ne
  }
  
  ## Calculate expected heterozygosity through twenty generations if no methods are conducted
  NoMethod_HeteroArray <- array(c(mean(MRB_Obs_Hetero_Gamma),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  for (j in 2:21) {
    for (i in length(MortalityValues)) {
      for (Ne in 2:21) { # below line is the equation for calcualting heterozygosity
        NoMethod_HeteroArray[[j]] <- (1 - 1/(2 * ((86 * MortalityValues[i] * Sequence_of_Ne[Ne])))) * (NoMethod_HeteroArray[j-1]) 
      }
    }## changes in MRB's heterozygosity with varrying Ne with a timescale of 20 generations
    # Omitted the exponent because the equation is already indexed 20 times with mortality values
    MRB_change_Hetero_nm <- NoMethod_HeteroArray[2:21]
    MRB_change_Hetero_nm <- array(MRB_change_Hetero_nm, dim = c(1,20))
  }
  MRB_change_Hetero_nm_iterations[,Iteration] <- MRB_change_Hetero_nm[1,20]
}
MRB_change_Hetero_nm_iterations

# calculate means for no methods heterozygosity changes
mean(MRB_change_Hetero_nm_iterations)


##### Results #####
#final abundance
HApop <- mean(HA_Ni_Array)
HA2pop <- mean(HA2_Ni_Array)
HA3pop <- mean(HA3_Ni_Array)
no_methods <- .250

#increase vs. no methods
mean(MRB_change_Hetero_HA_iterations) - .250
mean(MRB_change_Hetero_HA2_iterations) - .250
mean(MRB_change_Hetero_HA3_iterations) - .250

#range of iterations
HA_range <-  range(HA_Ni_Array)
HA2_range <-  range(HA2_Ni_Array)
HA3_range <-  range(HA3_Ni_Array)

##difference vs. no methods
#heterozygosity
mean(MRB_change_Hetero_HA_iterations) - mean(MRB_change_Hetero_nm_iterations)
mean(MRB_change_Hetero_HA2_iterations) - mean(MRB_change_Hetero_nm_iterations)
mean(MRB_change_Hetero_HA3_iterations) - mean(MRB_change_Hetero_nm_iterations)
#abundance
HApop - mean(NM_Ni_Array)
HA2pop - mean(NM_Ni_Array)
HA3pop - mean(NM_Ni_Array)

#abundance data frame
acquisition_abundance_df <- data.frame(c(mean(NM_Ni_Array),
                              HApop,
                              HA2pop,
                              HA3pop), 
                              c(min(NM_Ni_Array), min(HA_Ni_Array), min(HA2_Ni_Array), min(HA3_Ni_Array)),
                              c(max(NM_Ni_Array), max(HA_Ni_Array), max(HA2_Ni_Array), max(HA3_Ni_Array)))
colnames(acquisition_abundance_df) <- c("Final Abundance",
                                        "Lower Bound",
                                        "Upper Bound")
rownames(acquisition_abundance_df) <- c("No Methods",
                                        "Scenario One",
                                        "Scenario Two",
                                        "Scenario Three")
is.num <- sapply(acquisition_abundance_df,is.numeric)
acquisition_abundance_df[is.num] <- lapply(acquisition_abundance_df[is.num], round, 1)

level_order <- c("No Methods", "Scenario One", "Scenario Two", "Scenario Three")
HA_abundance_plot <- ggplot(acquisition_abundance_df, 
                            mapping = aes(y = acquisition_abundance_df$`Final Abundance`, 
                            x = factor(rownames(acquisition_abundance_df), level = level_order),
                            color = rownames(acquisition_abundance_df))) + 
                            geom_point(size = 2.5) +
                            geom_text(mapping = aes(label = `Final Abundance`), 
                             hjust = 1, vjust = -1) +
                            geom_errorbar(aes(ymin = `Lower Bound`, ymax = `Upper Bound`), 
                            width = .2, size = 0.5) + xlab("Method") + theme(legend.title = element_blank()) + 
                            labs(title = "Habiat Acquisition Population Change vs. No Methods") + ylab("Population Size")
  
ggsave("3rd Method Population vs. no methods plot.pdf")

#heterozygosity data frame
acquisition_heterozygosity_df <- data.frame(c(mean(MRB_change_Hetero_nm_iterations),
                                         mean(MRB_change_Hetero_HA_iterations),
                                         mean(MRB_change_Hetero_HA2_iterations),
                                         mean(MRB_change_Hetero_HA3_iterations)), 
                                       c(min(MRB_change_Hetero_nm_iterations), min(MRB_change_Hetero_HA_iterations), min(MRB_change_Hetero_HA2_iterations), min(MRB_change_Hetero_HA3_iterations)),
                                       c(max(MRB_change_Hetero_nm_iterations), max(MRB_change_Hetero_HA_iterations), max(MRB_change_Hetero_HA2_iterations), max(MRB_change_Hetero_HA3_iterations)))
colnames(acquisition_heterozygosity_df) <- c("Final Heterozygosity",
                                        "Lower Bound",
                                        "Upper Bound")
rownames(acquisition_heterozygosity_df) <- c("No Methods",
                                        "Scenario One",
                                        "Scenario Two",
                                        "Scenario Three")
is.num <- sapply(acquisition_heterozygosity_df,is.numeric)
acquisition_heterozygosity_df[is.num] <- lapply(acquisition_heterozygosity_df[is.num], round, 3)

level_order <- c("No Methods", "Scenario One", "Scenario Two", "Scenario Three")
HA_heterozygosity_plot <- ggplot(acquisition_heterozygosity_df, 
                            mapping = aes(y = acquisition_heterozygosity_df$`Final Heterozygosity`, 
                                          x = factor(rownames(acquisition_heterozygosity_df), level = level_order),
                                          color = rownames(acquisition_heterozygosity_df))) + 
  geom_point(size = 2.5) +
  geom_text(mapping = aes(label = `Final Heterozygosity`), 
            hjust = 1, vjust = -1) +
  geom_errorbar(aes(ymin = `Lower Bound`, ymax = `Upper Bound`), 
                width = .2, size = 0.5) + xlab("Method") + theme(legend.title = element_blank()) + 
  labs(title = "Habiat Acquisition Heterozygosity Change vs. No Methods") + ylab("Final Heterozygosity")

ggsave("3rd Method Heterozygosity vs. no methods plot.pdf")
