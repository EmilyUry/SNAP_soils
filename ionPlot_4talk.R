

#### ion tracer plot for talk
setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_compilation/SNAP_Carbon_Story")

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







x <- data[which(data$Treatment == "Salt" | data$Treatment == "Control"),]
xS <- x[which(x$Treatment == "Salt" ),]
xC <- x[which(x$Treatment == "Control" ),]



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

xxp <- c(1.5,4.5,7.5




seq <- seq(from = 4,14, by = 2)
lab <- c(" ", " ", " ", "Chloride", " ", "Sulfate", " ", " Sodium", " ", "Potassium", 
         " ", "Magnesium", " ", "Calcium", " ")



##### Here is the percent RECOVERY

rec <- c(0,0,0,277,0, 24.4, 0, 246, 0, 5, 0, 56, 0, 10, 0)

par(mfrow = c(2,3), mar = c(5,5,3,1), cex = 0.9)

for(i in seq){
  
  plot(xxp, df[,i]/rec[i]*100, xaxt = "n", xlim = c(0,9), ylim = c(-15, 25), #ylim = c(min(df[,i]-df[,i+1])/rec[i]*100, max(df[,i]+df[,i+1])/rec[i]*100),
       pch = 21, cex = 1, bg = c("black", "white"),
       ylab = "% Recovery", xlab = " ", 
       main = lab[i])
  axis(1, at = c(1.5,4.5,7.5), labels = c("(0-5) cm", "(5-10) cm", "(0-5) cm", "(5-10) cm", "(0-5) cm", "(5-10) cm"), las = 2)
  axis(3, at = c(1.5, 4.5, 7.5), labels = c("Site 1", "Site 3", "Site 5"), line = -1, tick = FALSE, font = 2, cex = 2)
  abline(v = 3, col = "gray80")
  abline(v = 6, col = "gray80")
  abline(h = 0, lty = 2, col = "red")
  arrows(xxp, (df[,i]+df[,i+1]/2.2)/rec[i]*100, xxp, (df[,i]-df[,i+1]/2.2)/rec[i]*100, angle = 90, length = 0.01, code = 3)}




seq <- c(4,6,10,8,12,14)

dfa <- df[which(df$Depth == "(0-5)"),]
xxp <- c(1.5,4.5,7.5)

par(mfrow = c(2,3), mar = c(2,5,3,1), cex = 0.9)

for(i in seq){
  
  plot(xxp, dfa[,i]/rec[i]*100, xaxt = "n", xlim = c(0,9), ylim = c(-11, 20), 
       pch = 15, cex = 1.5, col = c("#d6604d", "#9970AB",  "#4393C3"), 
       ylab = "% Recovery", xlab = " ", 
       main = lab[i])
  axis(3, at = c(1.5), labels = c("Dry"), line = -1, tick = FALSE, font = 2, cex = 2, 
       col.axis = c("red"))
  axis(3, at = c(4.5), labels = c( "Intermediate"), line = -1, tick = FALSE, font = 2, cex = 2, 
       col.axis = c("purple"))
  axis(3, at = c( 7.5), labels = c( "Wet"), line = -1, tick = FALSE, font = 2, cex = 2, 
       col.axis = c("blue"))
  abline(v = 3, col = "gray80")
  abline(v = 6, col = "gray80")
  abline(h = 0, lty = 2, col = "black")
  arrows(xxp, (dfa[,i]+dfa[,i+1]/2.2)/rec[i]*100, xxp, (dfa[,i]-dfa[,i+1]/2.2)/rec[i]*100, angle = 90, length = 0.01, code = 3)}

