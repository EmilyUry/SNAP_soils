#' ---
#' title: "Three axes of salinization"
#' author: "Emily Ury"
#' date: "May 7, 2020"
#' output: github_document
#' ---
#'
#' Data is from the *soils mastersheet* from **2019**
#' 
#' Info about instant ocean salt mixture is found in 
#' http://cmetton.free.fr/Divers/Aqua/35509243.pdf
#' 

library(plyr)


setwd("C:/Users/eau6/Dropbox (Duke Bio_Ea)/My data/SNAP_compilation/SNAP_Carbon_Story")

data <- read.csv("2019_SNAP_master.csv", header = TRUE)
names(data) <- c("Date","Site", "Treatment", "Core", "Depth", "Cond", "BD", 
                 "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
                 "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
                 "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4")

element <- c("Cl", "SO4", "Na", "K", "Mg", "Ca")
IO <- c(521, 23, 462, 9.4, 52, 9.4) ## mmol/kg IO  ## reported concentration of each ion in the above citation
#### note that these are per kg of "sea water mixture" made by adding 35 g of IO to DI up to 1kg 

MW <- c(35.45, 96.06, 23, 39.1, 24.3, 40.1) #gram/mol
EqF <- c(1,2,1,1,2,2)
IOg <- IO*MW/1000 ### grams of each ion in 35 grams of IO
IOgg <- IO*MW/1000/35 ## grams of each ion in 1 gram of IO
Eq <- c(35.45, 48, 23, 39, 12, 20) ## grams/Eq
IOeq <- IOgg/Eq*1000  ## mEq of each ion in 1 gram of IO



#' How much salt have we added to the plots?
#' We have done 19 salt additions to date (however only 16 of these were prior to 
#' the 2019 soil sampling effort.
#' 
#' Each addition delivers 30 pounds of salt to a treatment plot (200 m2)

16*30/200   # additions * pounds / area

#' So far we have added 2.4 pounds of salt per square meter
#' Or 1.08862 kg (2.4/2.20462)

cdata <- ddply(data, c( "Depth"), summarise,  ## also include "Site" and "Treatment" with "Depth"
               N    = length(BD),
               mean = mean(BD),
               sd   = sd(BD),
               se   = sd / sqrt(N))
cdata

#' So, the mean bulk density for the (0-5) cm depth is 1.1697 g/cm3
#' And, in the top 5cm of soil, there is 1m2 x 0.05m = 0.05 m3 of soil,
#' or 50,000 cm3 of soil (100 x 100 x 5).
#' 

1088.62/50000/1.1697   ## grams of salt/ area bulk density = grams salt added per gram soil

#' 0.0186 grams of salt added to each gram of soil. 
#' OR 18.614 mg of salt per gram of soil


ion.mEq <- IOeq * 0.0186 ## amount of each ion added to each gram of soil is the
                         ## amount of each ion in each gram of IO * grams of IO added
                         ## to each gram of soil 
ion.uEq <- ion.mEq *1000
#' Cl = 0.277  mEq/g
#' SO4 = 0.0245
#' Na = 0.245
#' K = 0.00501
#' Mg = 0.0560
#' Ca = 0.0100


ions <- (data[,c(14:19)])/Eq/100*1000 ### mg/L --> mEq/L  /100 --> uEq/g 
colnames(ions)<-paste(colnames(ions), "_uEq_g", sep="")
data<-cbind(data,ions)


data$Cl.rec <- data$Cl_uEq_g/ion.uEq[1]*100
data$SO4.rec <- data$SO4_uEq_g/ion.uEq[2]*100
data$Na.rec <- data$Na_uEq_g/ion.uEq[3]*100
data$K.rec <- data$K_uEq_g/ion.uEq[4]*100
data$Mg.rec <- data$Mg_uEq_g/ion.uEq[5]*100
data$Ca.rec <- data$Ca_uEq_g/ion.uEq[6]*100

cdata <- ddply(data, c("Site", "Treatment", "Depth"), summarise,  ## also include "Site" and "Treatment" with "Depth"
               N    = length(SO4.rec),
               mean = mean(Mg.rec),
               sd   = sd(Mg.rec),
               se   = sd / sqrt(N))
cdata
