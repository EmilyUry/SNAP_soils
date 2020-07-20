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
#' TCC = 316.5

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
MgIgnore <- data$Mg_uEq_g
MgIgnore[is.na(MgIgnore)]  <- 0
data$TCC <- (data$Na_uEq_g + data$Ca_uEq_g + data$K_uEq_g + MgIgnore)
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


par(mfrow = c(1,3), mar = c(4,4,4,1), cex = 0.9)

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









#####################
## money Figure V2
#####################



x <- data[which(data$Treatment == "Salt" | data$Treatment == "Control"),]
#x <- x[which(x$Depth == "(0-5)"),]
x$Depth <- as.factor(x$Depth)
x$Treatment <- as.factor(x$Treatment)

xp <- c(1,1,1,1,1,2,2,2,2,2,
        4,4,4,4,4,5,5,5,5,5,
        8,8,8,8,8, 9,9,9,9,9,
        11,11,11,11,11,12,12,12,12,12,
        15,15,15,15,15,16,16,16,16,16,
        18,18,18,18,18,19,19, 19, 19, 19)

par(mfrow = c(1,3), mar = c(12,4,12,1), cex = 0.9)
plot(xp, x$Cl_uEq_g, xaxt = "n", xlim = c(0, 20), pch = c("o","+")[x$Treatment], cex = 1,
     col = c("gray30", "gray70")[x$Depth],
     ylab = "[Cl-] (uEq/g)", xlab = " ", 
     main = "Chloride \nAmount added = 138 uEq/g")
axis(1, at = c(1.5,4.5,8.5,11.5,15.5, 18.5), labels = c("Salt", "Control", "Salt", "Control", "Salt", "Control"), las = 2)
axis(3, at = c(3, 10, 17), labels = c("Site 1", "Site 3", "Site 5"), tick = FALSE, font = 2, cex = 2)
abline(v = 6.5)
abline(v = 13.5)


plot(xp, x$SO4_uEq_g, xaxt = "n", xlim = c(0, 20), pch = c("o","+")[x$Treatment], cex = 1,
     col = c("gray30", "gray70")[x$Depth],
     ylab = "[SO4--] (uEq/g)", xlab = " ", 
     main = "Sulfate \nAmount added = 12.2 uEq/g")
axis(1, at = c(1.5,4.5,8.5,11.5,15.5, 18.5), labels = c("Salt", "Control", "Salt", "Control", "Salt", "Control"), las = 2)
axis(3, at = c(3, 10, 17), labels = c("Site 1", "Site 3", "Site 5"), tick = FALSE, font = 2, cex = 2)
abline(v = 6.5)
abline(v = 13.5)
legend("bottomleft", inset = c(0.3,-0.7), c("(0-5) cm", "(5-10) cm"), pch = "+", col = c("gray30", "gray70"),
       title = "Depth", xpd = T)

mtext("Ions recovered in the top 10 cm", 3, 8, font = 3, cex = 1.1)


plot(xp, x$TCC, xaxt = "n", xlim = c(0, 20), pch = c("o","+")[x$Treatment], cex = 1,
     col = c("gray30", "gray70")[x$Depth],
     ylab = "[Na+, K+, Ca2+, Mg2+] (uEq/g)", xlab = " ", 
     main = "Base Cations \nAmount added = 158 uEq/g")
axis(1, at = c(1.5,4.5,8.5,11.5,15.5, 18.5), labels = c("Salt", "Control", "Salt", "Control", "Salt", "Control"), las = 2)
axis(3, at = c(3, 10, 17), labels = c("Site 1", "Site 3", "Site 5"), tick = FALSE, font = 2, cex = 2)
abline(v = 6.5)
abline(v = 13.5)








#####################
## money Figure 6 plots
#####################

x <- data[which(data$Treatment == "Salt" | data$Treatment == "Control"),]
#x <- x[which(x$Depth == "(0-5)"),]
x$Depth <- as.factor(x$Depth)
x$Treatment <- as.factor(x$Treatment)

xp <- c(1,1,1,1,1,2,2,2,2,2,
        4,4,4,4,4,5,5,5,5,5,
        8,8,8,8,8, 9,9,9,9,9,
        11,11,11,11,11,12,12,12,12,12,
        15,15,15,15,15,16,16,16,16,16,
        18,18,18,18,18,19,19, 19, 19, 19)

par(mfrow = c(2,3), mar = c(4,5,4,1), cex = 0.9)
plot(xp, x$Cl_uEq_g, xaxt = "n", xlim = c(0, 20), pch = c("o","+")[x$Treatment], cex = 1,
     col = c("gray30", "gray70")[x$Depth],
     ylab = "[Cl-] (uEq/g)", xlab = " ", 
     main = "Chloride")
axis(1, at = c(1.5,4.5,8.5,11.5,15.5, 18.5), labels = c("Salt", "Control", "Salt", "Control", "Salt", "Control"), las = 2)
axis(3, at = c(3, 10, 17), labels = c("Site 1", "Site 3", "Site 5"), line = -1, tick = FALSE, font = 2, cex = 2)
abline(v = 6.5)
abline(v = 13.5)

plot(xp, x$SO4_uEq_g, xaxt = "n", xlim = c(0, 20), pch = c("o","+")[x$Treatment], cex = 1,
     col = c("gray30", "gray70")[x$Depth],
     ylab = "[SO4--] (uEq/g)", xlab = " ", 
     main = "Sulfate")
axis(1, at = c(1.5,4.5,8.5,11.5,15.5, 18.5), labels = c("Salt", "Control", "Salt", "Control", "Salt", "Control"), las = 2)
axis(3, at = c(3, 10, 17), labels = c("Site 1", "Site 3", "Site 5"), line = -1, tick = FALSE, font = 2, cex = 2)
abline(v = 6.5)
abline(v = 13.5)

plot(xp, x$TCC, xaxt = "n", xlim = c(0, 20), pch = c("o","+")[x$Treatment], cex = 1,
     col = c("gray30", "gray70")[x$Depth],
     ylab = "[Na+, K+, Ca2+, Mg2+] (uEq/g)", xlab = " ", 
     main = "Base Cations")
axis(1, at = c(1.5,4.5,8.5,11.5,15.5, 18.5), labels = c("Salt", "Control", "Salt", "Control", "Salt", "Control"), las = 2)
axis(3, at = c(3, 10, 17), labels = c("Site 1", "Site 3", "Site 5"), line = -1, tick = FALSE, font = 2, cex = 2)
abline(v = 6.5)
abline(v = 13.5)


plot(xp, x$K, xaxt = "n", xlim = c(0, 20), pch = c("o","+")[x$Treatment], cex = 1,
     col = c("gray30", "gray70")[x$Depth],
     ylab = "pH", xlab = " ", 
     main = "pH")
axis(1, at = c(1.5,4.5,8.5,11.5,15.5, 18.5), labels = c("Salt", "Control", "Salt", "Control", "Salt", "Control"), las = 2)
axis(3, at = c(3, 10, 17), labels = c("Site 1", "Site 3", "Site 5"), line = -1, tick = FALSE, font = 2, cex = 2)
abline(v = 6.5)
abline(v = 13.5)

plot(xp, x$LOI, xaxt = "n", xlim = c(0, 20), pch = c("o","+")[x$Treatment], cex = 1,
     col = c("gray30", "gray70")[x$Depth],
     ylab = "LOI (%)", xlab = " ", 
     main = "LOI")
axis(1, at = c(1.5,4.5,8.5,11.5,15.5, 18.5), labels = c("Salt", "Control", "Salt", "Control", "Salt", "Control"), las = 2)
axis(3, at = c(3, 10, 17), labels = c("Site 1", "Site 3", "Site 5"), line = -1, tick = FALSE, font = 2, cex = 2)
abline(v = 6.5)
abline(v = 13.5)

plot(xp, x$Ca_uEq_g, xaxt = "n", xlim = c(0, 20), pch = c("o","+")[x$Treatment], cex = 1,
     col = c("gray30", "gray70")[x$Depth],
     ylab = "[Ca2+] (uEq/g)", xlab = " ", 
     main = "Calcium")
axis(1, at = c(1.5,4.5,8.5,11.5,15.5, 18.5), labels = c("Salt", "Control", "Salt", "Control", "Salt", "Control"), las = 2)
axis(3, at = c(3, 10, 17), labels = c("Site 1", "Site 3", "Site 5"), line = -1, tick = FALSE, font = 2, cex = 2)
abline(v = 6.5)
abline(v = 13.5)





###### 3D plot

library("scatterplot3d")
x0 <- x[which(x$Depth == "(0-5)"),]
x0$Site <- as.factor(x0$Site)
col3 <- c("#d6604d","#9970AB" ,  "#4393C3")

par(mfrow = c(1,1), mar = c(8,8,4,10))

scatterplot3d(x0$Cl_uEq_g, x0$SO4_uEq_g, x0$TCC, pch = c(21,16)[x0$Treatment], color = col3[x0$Site], angle =40)
legend("bottomright", legend = c("Site 1", "Site 3", "Site 5"),
       col =  c("#d6604d", "#9970AB", "#4393C3"), pch = 16, 
       inset = c(-0.0, -0.2), xpd = TRUE, horiz = TRUE)
legend("bottomright", legend = c("Salt", "Control"),
       col =  c("#d6604d", "#d6604d"), pt.bg = c("#d6604d", "#FFFFFF"), pch = 21, 
       inset = c(-0.0, -0.1), xpd = TRUE, horiz = TRUE)



par(mfrow = c(1,3), mar = c(12,4,12,2) )
plot(x0$Cl.rec, x0$SO4.rec, pch = c(21,16)[x0$Treatment], col = col3[x0$Site])
plot(x0$Cl.rec, x0$Na.rec, pch = c(21,16)[x0$Treatment], col = col3[x0$Site])


plot(x0$Cl_uEq_g, x0$SO4_uEq_g, pch = c(21,16)[x0$Treatment], col = col3[x0$Site])
plot(x0$Cl.rec, x0$Na_uEq_g, pch = c(21,16)[x0$Treatment], col = col3[x0$Site])


plot(log(x0$Cl_uEq_g), log(x0$SO4_uEq_g), pch = c(21,16)[x0$Treatment], col = col3[x0$Site])
plot(log(x0$Cl_uEq_g), log(x0$Na_uEq_g), pch = c(21,16)[x0$Treatment], col = col3[x0$Site])
plot(log(x0$SO4_uEq_g), log(x0$Na_uEq_g), pch = c(21,16)[x0$Treatment], col = col3[x0$Site])

x0$TCC <- (x0[,35] + x0[,36] + x0[,37] + x0[,38])
x0$TCC.rec <- x0$TCC/ 316.5 *100
par(mfrow = c(1,3), mar = c(12,4,12,2) )
plot(x0$pH, log(x0$Cl.rec), pch = c(21,16)[x0$Treatment], col = col3[x0$Site])
plot(x0$pH, log(x0$SO4.rec), pch = c(21,16)[x0$Treatment], col = col3[x0$Site])
plot(x0$pH, log(x0$TCC.rec), pch = c(21,16)[x0$Treatment], col = col3[x0$Site])





#########################################
## "Effect size" and Recovery rates

x <- data[which(data$Treatment == "Salt" | data$Treatment == "Control"),]
xS <- x[which(x$Treatment == "Salt" ),]
xC <- x[which(x$Treatment == "Control" ),]


# 
# sum.cl <- ddply(x, c("Site", "Treatment", "Depth"), summarise,
#                  mean.cl = mean(Cl_uEq_g),
#                  sd.cl   = sd(Cl_uEq_g))
# sum.SO4 <- ddply(x, c("Site", "Treatment", "Depth"), summarise,
#                 mean.so4 = mean(SO4_uEq_g),
#                 sd.so4   = sd(SO4_uEq_g))
# sum.Na <- ddply(x, c("Site", "Treatment", "Depth"), summarise,
#                 mean.Na = mean(Na_uEq_g),
#                 sd.Na   = sd(Na_uEq_g))
# sum.K <- ddply(x, c("Site", "Treatment", "Depth"), summarise,
#                 mean.k = mean(K_uEq_g),
#                 sd.k = sd(K_uEq_g))
# sum.Mg <- ddply(x, c("Site", "Treatment", "Depth"), summarise,
#                 mean.mg = mean(Mg_uEq_g),
#                 sd.mg = sd(Mg_uEq_g))
# sum.Ca <- ddply(x, c("Site", "Treatment", "Depth"), summarise,
#                 mean.ca = mean(Ca_uEq_g),
#                 sd.ca   = sd(Ca_uEq_g))
# df <- cbind(sum.cl, sum.SO4[,4:5], sum.Na[,4:5], sum.K[,4:5], sum.Mg[,4:5], sum.Ca[,4:5])
# df[is.na(df)] <- 0
# df




sum.clS <- ddply(xS, c("Site", "Treatment", "Depth"), summarise,
                 mean.cl = mean(Cl_uEq_g),
                 sd.cl   = sd(Cl_uEq_g))
sum.clC <- ddply(xC, c("Site", "Treatment", "Depth"), summarise,
                 mean.cl = mean(Cl_uEq_g),
                 sd.cl   = sd(Cl_uEq_g))
sum.clC
ES.cl <- sum.clS$mean.cl - sum.clC$mean.cl
err.cl <- sqrt((sum.clS$sd.cl)^2 + (sum.clC$sd.cl)^2)

sumS <- ddply(xS, c("Site", "Treatment", "Depth"), summarise,
                 mean = mean(SO4_uEq_g),
                 sd   = sd(SO4_uEq_g))
sumC <- ddply(xC, c("Site", "Treatment", "Depth"), summarise,
                 mean = mean(SO4_uEq_g),
                 sd   = sd(SO4_uEq_g))
ES.SO4 <- sumS$mean - sumC$mean
err.SO4 <- sqrt((sumS$sd)^2 + (sumC$sd)^2)


sumS <- ddply(xS, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(Na_uEq_g),
              sd   = sd(Na_uEq_g))
sumC <- ddply(xC, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(Na_uEq_g),
              sd   = sd(Na_uEq_g))
ES.Na <- sumS$mean - sumC$mean
err.Na <- sqrt((sumS$sd)^2 + (sumC$sd)^2)

sumS <- ddply(xS, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(K_uEq_g),
              sd   = sd(K_uEq_g))
sumC <- ddply(xC, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(K_uEq_g),
              sd   = sd(K_uEq_g))
ES.K <- sumS$mean - sumC$mean
err.K <- sqrt((sumS$sd)^2 + (sumC$sd)^2)

sumS <- ddply(xS, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(Mg_uEq_g),
              sd   = sd(Mg_uEq_g))
sumC <- ddply(xC, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(Mg_uEq_g),
              sd   = sd(Mg_uEq_g))
sumC[is.na(sumC)] <- 0
ES.Mg <- sumS$mean - sumC$mean
err.Mg <- sqrt((sumS$sd)^2 + (sumC$sd)^2)

sumS <- ddply(xS, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(Ca_uEq_g),
              sd   = sd(Ca_uEq_g))
sumC <- ddply(xC, c("Site", "Treatment", "Depth"), summarise,
              mean = mean(Ca_uEq_g),
              sd   = sd(Ca_uEq_g))
ES.Ca <- sumS$mean - sumC$mean
err.Ca <- sqrt((sumS$sd)^2 + (sumC$sd)^2)

df <- cbind(sum.clS[,1:3], ES.cl, err.cl, ES.SO4, err.SO4, ES.Na, err.Na, ES.K, err.K, ES.Mg, err.Mg, ES.Ca, err.Ca )

xxp <- c(1,2,4,5,7,8)

i <- 4 ## chloride

seq <- seq(from = 4,14, by = 2)
lab <- c(" ", " ", " ", "Chloride", " ", "Sulfate", " ", " Sodium", " ", "Potassium", 
        " ", "Magnesium", " ", "Calcium", " ")


##### HERE is the EFFECT Size of each ION addition (Salt - Control)


par(mfrow = c(2,3), mar = c(5,5,3,1), cex = 0.9)

for(i in seq){

plot(xxp, df[,i], xaxt = "n", xlim = c(0,9), ylim = c(min(df[,i]-df[,i+1]), max(df[,i]+df[,i+1])),
     pch = 21, cex = 1, bg = c("black", "white"),
     ylab = "Effect Size (uEq/g)", xlab = " ", 
     main = lab[i])
axis(1, at = c(1,2,4,5,7,8), labels = c("(0-5) cm", "(5-10) cm", "(0-5) cm", "(5-10) cm", "(0-5) cm", "(5-10) cm"), las = 2)
axis(3, at = c(1.5, 4.5, 7.5), labels = c("Site 1", "Site 3", "Site 5"), line = -1, tick = FALSE, font = 2, cex = 2)
abline(v = 3, col = "lightblue")
abline(v = 6, col = "lightblue")
arrows(xxp, df[,i]+df[,i+1]/2.2, xxp, df[,i]-df[,i+1]/2.2, angle = 90, length = 0.01, code = 3)}


##### Here is the percent RECOVERY

rec <- c(0,0,0,277,0, 24.4, 0, 246, 0, 5, 0, 56, 0, 10, 0)

par(mfrow = c(2,3), mar = c(5,5,3,1), cex = 0.9)

for(i in seq){
  
  plot(xxp, df[,i]/rec[i]*100, xaxt = "n", xlim = c(0,9), ylim = c(-15, 25), #ylim = c(min(df[,i]-df[,i+1])/rec[i]*100, max(df[,i]+df[,i+1])/rec[i]*100),
       pch = 21, cex = 1, bg = c("black", "white"),
       ylab = "% Recovery", xlab = " ", 
       main = lab[i])
  axis(1, at = c(1,2,4,5,7,8), labels = c("(0-5) cm", "(5-10) cm", "(0-5) cm", "(5-10) cm", "(0-5) cm", "(5-10) cm"), las = 2)
  axis(3, at = c(1.5, 4.5, 7.5), labels = c("Site 1", "Site 3", "Site 5"), line = -1, tick = FALSE, font = 2, cex = 2)
  abline(v = 3, col = "gray80")
  abline(v = 6, col = "gray80")
  abline(h = 0, lty = 2, col = "red")
  arrows(xxp, (df[,i]+df[,i+1]/2.2)/rec[i]*100, xxp, (df[,i]-df[,i+1]/2.2)/rec[i]*100, angle = 90, length = 0.01, code = 3)}




###### Drivers of percent recovery 

dr <- data[, c(2:5, 39:44, 7:12, 30)]
dr <- dr[which(dr$Treatment == "Salt"),]
dr$Depth <- as.factor(dr$Depth)
labs <- c(" ", " ", " ", " ", "Chloride", "Sulfate", "Sodium", "Potassium", "Magnesium", "Calcium")
col3 <- c("#d6604d", "black", "#9970AB","black", "#4393C3") ## medium

par(mfrow = c(2,3), mar = c(5,5,3,1), cex = 0.9)
#### Bulk Density

for (i in c(1:6)){
  plot(dr[,12], dr[,i+4], ylim = c(-15, 25), #ylim = c(min(df[,i]-df[,i+1])/rec[i]*100, max(df[,i]+df[,i+1])/rec[i]*100),
       pch = 21, cex = 1, col = col3[dr$Site], bg = ifelse(dr$Depth == "(5-10)", "white", col3[dr$Site]),
       ylab = labs[i+4], xlab = "Soil moisture (%)", 
       main = labs[i+4])}

for (i in c(1:6)){
  plot(dr[,13], dr[,i+4], ylim = c(-15, 25), #ylim = c(min(df[,i]-df[,i+1])/rec[i]*100, max(df[,i]+df[,i+1])/rec[i]*100),
       pch = 21, cex = 1, col = col3[dr$Site], bg = ifelse(dr$Depth == "(5-10)", "white", col3[dr$Site]),
       ylab = labs[i+4], xlab = "LOI (%)", 
       main = labs[i+4])}

for (i in c(1:6)){
  plot(dr[,14], dr[,i+4], ylim = c(-15, 25), #ylim = c(min(df[,i]-df[,i+1])/rec[i]*100, max(df[,i]+df[,i+1])/rec[i]*100),
       pch = 21, cex = 1, col = col3[dr$Site], bg = ifelse(dr$Depth == "(5-10)", "white", col3[dr$Site]),
       ylab = labs[i+4], xlab = "pH", 
       main = labs[i+4])}




