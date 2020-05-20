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
ion.uEq

#' Cl = 277  mEq/g
#' SO4 = 24.4
#' Na = 245.5
#' K = 5
#' Mg = 56
#' Ca = 10
sum(ion.uEq[3:6])
#' TIC = 316.5

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

### formula for calculating CEC https://ohioline.osu.edu/factsheet/anr-81
### see also: https://www.extension.purdue.edu/extmedia/ay/ay-238.html
data$CEC <- 12*(7-data$pH) + (data$Na_uEq_g + data$Ca_uEq_g + data$K_uEq_g + data$Mg_uEq_g)/10
data$TCC <- (data$Na_uEq_g + data$Ca_uEq_g + data$K_uEq_g + data$Mg_uEq_g)
plot(data$TCC, data$CEC)




#####################
## money Figure
#####################



x <- data[which(data$Treatment == "Salt" | data$Treatment == "Control"),]
##x <- x[which(x$Depth == "(0-5)"),]

#xp <- c(1,1,1,1,1,2,2,2,2,2,4,4,4,4,4,5,5,5,5,5,7,7,7,7,7,8,8,8,8,8)
xp <- c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,
        7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8)
x$xp <- xp
x$Depth <- as.factor(x$Depth)
x$Treatment <- as.factor(x$Treatment)

col3 <- c("#d6604d", "black", "#9970AB","black", "#4393C3") ## medium
col3l <- c("#f4a582", "black", "#C2A5CF", "black", "#92C5DE") ## light
col3d <- c("#b2182b", "black", "#762A83","black", "#2166AC") ## dark


par(mfrow = c(1,3), mar = c(12,4,12,1), cex = 0.9)

plot(x$xp, x$Cl_uEq_g, xaxt = "n", xlim = c(0.5, 8.5), pch = c(21,24)[x$Treatment], cex = 1,
     col = col3d[x$Site], bg = ifelse(x$Depth == "(5-10)", col3l[x$Site], col3d[x$Site]),
     ylab = "[Cl-] (uEq/g)", xlab = " ", 
     main = "Chloride \nAmount added = 138 uEq/g")
axis(1, at = c(1,2,4,5,7,8), labels = c("Salt", "Control", "Salt", "Control", "Salt", "Control"), las = 2)
axis(3, at = c(1.5, 4.5, 7.5), labels = c("Site 1", "Site 3", "Site 5"), tick = FALSE, font = 2, cex = 2)
abline(v = 3)
abline(v = 6)


plot(x$xp, x$SO4_uEq_g, xaxt = "n", xlim = c(0.5, 8.5), pch = c(21,24)[x$Treatment], cex = 1,
     col = col3d[x$Site], bg = ifelse(x$Depth == "(5-10)", col3l[x$Site], col3d[x$Site]),
     ylab = "[SO4] (uEq/g)", xlab = " ", 
     main = "Sulfate\nAmount added = 12.2 uEq/g")

mtext("Ions recovered in the top 10 cm", 3, 8, font = 3, cex = 1.1)

axis(1, at = c(1,2,4,5,7,8), labels = c("Salt", "Control", "Salt", "Control", "Salt", "Control"), las = 2)
axis(3, at = c(1.5, 4.5, 7.5), labels = c("Site 1", "Site 3", "Site 5"), tick = FALSE, font = 2, cex = 2)
abline(v = 3)
abline(v = 6)

plot(x$xp, x$TCC, xaxt = "n", xlim = c(0.5, 8.5), pch = c(21,24)[x$Treatment], cex = 1,
     col = col3d[x$Site], bg = ifelse(x$Depth == "(5-10)", col3l[x$Site], col3d[x$Site]),
     ylab = "[Na + K + Ca + Mg] (uEq/g)", xlab = " ", 
     main = "Base Cations\nAmount added = 158 uEq/g")
axis(1, at = c(1,2,4,5,7,8), labels = c("Salt", "Control", "Salt", "Control", "Salt", "Control"), las = 2)
axis(3, at = c(1.5, 4.5, 7.5), labels = c("Site 1", "Site 3", "Site 5"), tick = FALSE, font = 2, cex = 2)
abline(v = 3)
abline(v = 6)

