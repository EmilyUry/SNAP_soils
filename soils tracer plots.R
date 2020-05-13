

#' ---
#' title: "Tracer plots for anions/cations in soil cores"
#' author: "Emily Ury"
#' date: "May 12, 2020"
#' output: github_document
#' ---
#'


#' Created this script to look at the balance of anions and cations
#' in the experimental plot soils, relative to the ratios being added
#' from the instant ocean salt mixture.
#'   
#' Data is from the *soils mastersheet* from **2019**
#' 
#' Info about instant ocean salt mixture is found in 
#' http://cmetton.free.fr/Divers/Aqua/35509243.pdf
#' 



## file location
setwd("C:/Users/eau6/Dropbox (Duke Bio_Ea)/My data/SNAP_compilation/SNAP_Carbon_Story")

col3 <- c("#d6604d", "black", "#9970AB","black", "#4393C3") ## medium
col4 <- c("#FDE725FF", "#55C667FF", "#33638DFF", "#481567FF") ## viridis
col4a <- c("#FDE72599", "#55C66799", "#33638D99", "#48156799") ## viridis transparent


data <- read.csv("2019_SNAP_master.csv", header = TRUE)

#' Ratio information
#' SW = sea water
#' IO = instant ocean
#' MW = molecular weight
#' 
#' Anions and cations are in mmol/kg
#' Posphate is in umol/kg
#' No3 and NH4 are in mmol/kg

element <- c("Cl", "SO4", "Na", "K", "Mg", "Ca", "P:PO4", "N:NO3", "N:NH4")
SW <- c(550, 28, 470, 10.2, 53, 10.3, 0.2, 0.2, 0.2)
IO <- c(521, 23, 462, 9.4, 52, 9.4, 0.05, 1, 10.2)
##MW <- c(35.45, 96.06, 22.99, 39.1, 24.31, 40.08, 30.97, 14)

ratioSW <- SW/550
ratioIO <- IO/521

rd <- data
names(rd) <- c("Date","Site", "Treatment", "Core", "Depth", "Cond.", "B.D.", 
                 "S.M.", "LOI", "pH", "Roots", "DOC", "TDN", 
                 "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
                 "Cmin.s", "Cmin.c", "SIR.s", "SIR.c", "Br", "Phenol", "NO3", "PO4")
rd$Treatment <- as.factor(rd$Treatment)
rd$Site <- as.factor(rd$Site)
rd$Cl_SO4 <- rd$Cl/rd$SO4




##### 
## Sulfate to chloride ###
#####

par(mfrow = c(1,1), mar = c(6,16,4,8), xpd=F)
plot(rd$Cl, rd$SO4, pch = c(21,22,24)[rd$Site], col = col4[rd$Treatment],
     bg = ifelse(rd$Depth == "(5-10)", "white", col4a[rd$Treatment]), 
     main = "Soil core chloride:sulfate", xlab = "Chloride", ylab = "Sulfate")
abline(0, ratioSW[2], lty = 1)
abline(0, ratioIO[2], lwd = 2, lty =3)
text(70, 4, "sea water", srt = 17)
text(65, 2.6, "Instant Ocean", srt = 15)

legend("topright", inset=c(-0.3,-0.0), c("0-5 cm", "5-10 cm"), 
       pch = c(21, 21), pt.cex=1, cex = 1,
       col = c("gray20"), pt.bg = c("gray70", "white"),
       ncol = 1, title = "Depth", xpd = T)
legend("topright", inset=c(-0.3,0.3), c("Control", "Nutrient", "Salt", "SxN"), 
       pch = 21, pt.cex = 1, cex = 1,
       col = col4, pt.bg = col4a, 
       title = "Treatment", ncol = 1, xpd = T)
legend("topright", inset=c(-0.3,0.7), c("Site 1   ", "Site 3", "Site 5"), 
       pch = c(21,22,24), pt.cex = 1, cex = 1,
       col = "gray20", pt.bg = "white", 
       title = "Site", ncol = 1, xpd = T)


## Facet by site

Site1 <- rd[which(rd$Site == "1"),]
Site3 <- rd[which(rd$Site == "3"),]
Site5 <- rd[which(rd$Site == "5"),]
par(mfrow = c(1,4), mar = c(12,4,8,1), xpd=FALSE)
plot(rd$Cl, rd$SO4, pch = c(16), main = "All cores",
     xlab = "Chloride", ylab = "Sulfate")
abline(0, ratioIO[2], lwd = 2, lty =3)
plot(Site1$Cl, Site1$SO4, pch = c(21), xlab = "Chloride", ylab = "Sulfate",
     col = col4[rd$Treatment], main = "Site 1",
     bg = ifelse(rd$Depth == "(5-10)", "white", col4a[rd$Treatment]))
abline(0, ratioIO[2], lwd = 2, lty =3)
legend("bottomleft", inset=c(-0.0,-0.4), c("0-5 cm", "5-10 cm"), 
       pch = c(21, 21), pt.cex=1, cex = 1,
       col = c("gray20"), pt.bg = c("gray70", "white"),
       ncol = 1, title = "Depth", xpd = T)
legend("bottomright", inset=c(-0.0,-0.4), c("Site 1   ", "Site 3", "Site 5"), 
       pch = c(21,22,24), pt.cex = 1, cex = 1,
       col = "gray20", pt.bg = "white", 
       title = "Site", ncol = 1, xpd = T)
plot(Site3$Cl, Site3$SO4, pch = c(22), xlab = "Chloride", ylab = "Sulfate",
     col = col4[rd$Treatment], main = "Site 3",
     bg = ifelse(rd$Depth == "(5-10)", "white", col4a[rd$Treatment]))
abline(0, ratioIO[2], lwd = 2, lty =3)
legend("bottomleft", inset=c(-0.0,-0.4), c("Control", "Nutrient", "Salt", "SxN"), 
       pch = 21, pt.cex = 1, cex = 1,
       col = col4, pt.bg = col4a, 
       title = "Treatment", ncol = 2, xpd = T)
plot(Site5$Cl, Site5$SO4, pch = c(24), xlab = "Chloride", ylab = "Sulfate",
     col = col4[rd$Treatment], main = "Site 5",
     bg = ifelse(rd$Depth == "(5-10)", "white", col4a[rd$Treatment]))
abline(0, ratioIO[2], lwd = 2, lty =3)


####
#### Ca to Cl
###
par(mfrow = c(1,1), mar = c(6,16,4,8), xpd=F)
plot(rd$Cl, rd$Ca, pch = c(21,22,24)[rd$Site], col = col4[rd$Treatment],
     bg = ifelse(rd$Depth == "(5-10)", "white", col4a[rd$Treatment]), 
     main = "Soil core chloride:calcium", xlab = "Chloride", ylab = "Calcium")
abline(0, ratioSW[6], lty = 1)
abline(0, ratioIO[6], lwd = 2, lty =3)
text(70, 1.5, "sea water", srt = 15)
text(65, 1.1, "Instant Ocean", srt = 15)

legend("topright", inset=c(-0.3,-0.0), c("0-5 cm", "5-10 cm"), 
       pch = c(21, 21), pt.cex=1, cex = 1,
       col = c("gray20"), pt.bg = c("gray70", "white"),
       ncol = 1, title = "Depth", xpd = T)
legend("topright", inset=c(-0.3,0.3), c("Control", "Nutrient", "Salt", "SxN"), 
       pch = 21, pt.cex = 1, cex = 1,
       col = col4, pt.bg = col4a, 
       title = "Treatment", ncol = 1, xpd = T)
legend("topright", inset=c(-0.3,0.7), c("Site 1   ", "Site 3", "Site 5"), 
       pch = c(21,22,24), pt.cex = 1, cex = 1,
       col = "gray20", pt.bg = "white", 
       title = "Site", ncol = 1, xpd = T)



####
#### Cl to Mg
###
par(mfrow = c(1,1), mar = c(6,16,5,8), xpd=F)
plot(rd$Cl, rd$Mg, pch = c(21,22,24)[rd$Site], col = col4[rd$Treatment],
     bg = ifelse(rd$Depth == "(5-10)", "white", col4a[rd$Treatment]), 
     main = "Soil core chloride:magnesium", xlab = "Chloride", ylab = "Magnesium")
abline(0, ratioSW[5], lty = 1)
abline(0, ratioIO[5], lwd = 2, lty =3)
legend("topright", inset=c(-0.3,-0.0), c("0-5 cm", "5-10 cm"), 
       pch = c(21, 21), pt.cex=1, cex = 1,
       col = c("gray20"), pt.bg = c("gray70", "white"),
       ncol = 1, title = "Depth", xpd = T)
legend("topright", inset=c(-0.3,0.3), c("Control", "Nutrient", "Salt", "SxN"), 
       pch = 21, pt.cex = 1, cex = 1,
       col = col4, pt.bg = col4a, 
       title = "Treatment", ncol = 1, xpd = T)
legend("topright", inset=c(-0.3,0.7), c("Site 1   ", "Site 3", "Site 5"), 
       pch = c(21,22,24), pt.cex = 1, cex = 1,
       col = "gray20", pt.bg = "white", 
       title = "Site", ncol = 1, xpd = T)
legend("topright", inset = c(-0.30, -0.2), c("Instant Ocean", "Sea water"), lty = c(3, 1),
       lwd = c(2,1), xpd = TRUE)




####
#### Cl to Na
###
par(mfrow = c(1,1), mar = c(6,16,5,8), xpd=F)
plot(rd$Cl, rd$Na, pch = c(21,22,24)[rd$Site], col = col4[rd$Treatment],
     bg = ifelse(rd$Depth == "(5-10)", "white", col4a[rd$Treatment]), 
     main = "Soil core chloride:sodium", xlab = "Chloride", ylab = "Sodium")
abline(0, ratioSW[3], lty = 1)
abline(0, ratioIO[3], lwd = 2, lty =3)
legend("topright", inset=c(-0.3,-0.0), c("0-5 cm", "5-10 cm"), 
       pch = c(21, 21), pt.cex=1, cex = 1,
       col = c("gray20"), pt.bg = c("gray70", "white"),
       ncol = 1, title = "Depth", xpd = T)
legend("topright", inset=c(-0.3,0.3), c("Control", "Nutrient", "Salt", "SxN"), 
       pch = 21, pt.cex = 1, cex = 1,
       col = col4, pt.bg = col4a, 
       title = "Treatment", ncol = 1, xpd = T)
legend("topright", inset=c(-0.3,0.7), c("Site 1   ", "Site 3", "Site 5"), 
       pch = c(21,22,24), pt.cex = 1, cex = 1,
       col = "gray20", pt.bg = "white", 
       title = "Site", ncol = 1, xpd = T)
legend("topright", inset = c(-0.30, -0.2), c("Instant Ocean", "Sea water"), lty = c(3, 1),
       lwd = c(2,1), xpd = TRUE)



####
#### Cl to K
###
par(mfrow = c(1,1), mar = c(6,16,5,8), xpd=F)
plot(rd$Cl, rd$K, pch = c(21,22,24)[rd$Site], col = col4[rd$Treatment],
     bg = ifelse(rd$Depth == "(5-10)", "white", col4a[rd$Treatment]), 
     main = "Soil core chloride:potassium", xlab = "Chloride", ylab = "Potassium")
abline(0, ratioSW[4], lty = 1)
abline(0, ratioIO[4], lwd = 2, lty =3)
legend("topright", inset=c(-0.3,-0.0), c("0-5 cm", "5-10 cm"), 
       pch = c(21, 21), pt.cex=1, cex = 1,
       col = c("gray20"), pt.bg = c("gray70", "white"),
       ncol = 1, title = "Depth", xpd = T)
legend("topright", inset=c(-0.3,0.3), c("Control", "Nutrient", "Salt", "SxN"), 
       pch = 21, pt.cex = 1, cex = 1,
       col = col4, pt.bg = col4a, 
       title = "Treatment", ncol = 1, xpd = T)
legend("topright", inset=c(-0.3,0.7), c("Site 1   ", "Site 3", "Site 5"), 
       pch = c(21,22,24), pt.cex = 1, cex = 1,
       col = "gray20", pt.bg = "white", 
       title = "Site", ncol = 1, xpd = T)
legend("topright", inset = c(-0.30, -0.2), c("Instant Ocean", "Sea water"), lty = c(3, 1),
       lwd = c(2,1), xpd = TRUE)






####
#### Cl to NH4
###
par(mfrow = c(1,1), mar = c(6,16,5,8), xpd=F)
plot(rd$Cl, rd$NH4, pch = c(21,22,24)[rd$Site], col = col4[rd$Treatment],
     bg = ifelse(rd$Depth == "(5-10)", "white", col4a[rd$Treatment]), 
     main = "Soil core chloride:Ammonium", xlab = "Chloride", ylab = "Ammonium")
abline(0, ratioSW[9], lty = 1)
abline(0, ratioIO[9], lwd = 2, lty =3)
legend("topright", inset=c(-0.3,-0.0), c("0-5 cm", "5-10 cm"), 
       pch = c(21, 21), pt.cex=1, cex = 1,
       col = c("gray20"), pt.bg = c("gray70", "white"),
       ncol = 1, title = "Depth", xpd = T)
legend("topright", inset=c(-0.3,0.3), c("Control", "Nutrient", "Salt", "SxN"), 
       pch = 21, pt.cex = 1, cex = 1,
       col = col4, pt.bg = col4a, 
       title = "Treatment", ncol = 1, xpd = T)
legend("topright", inset=c(-0.3,0.7), c("Site 1   ", "Site 3", "Site 5"), 
       pch = c(21,22,24), pt.cex = 1, cex = 1,
       col = "gray20", pt.bg = "white", 
       title = "Site", ncol = 1, xpd = T)
legend("topright", inset = c(-0.30, -0.2), c("Instant Ocean", "Sea water"), lty = c(3, 1),
       lwd = c(2,1), xpd = TRUE)



####
#### Cl to NO3
###
par(mfrow = c(1,1), mar = c(6,16,5,8), xpd=F)
plot(rd$Cl, rd$NO3, pch = c(21,22,24)[rd$Site], col = col4[rd$Treatment],
     bg = ifelse(rd$Depth == "(5-10)", "white", col4a[rd$Treatment]),
     main = "Soil core chloride:nitrate", xlab = "Chloride", ylab = "Nitrate")
abline(0, ratioSW[8], lty = 1)
abline(0, ratioIO[8], lwd = 2, lty =3)
legend("topright", inset=c(-0.3,-0.0), c("0-5 cm", "5-10 cm"), 
       pch = c(21, 21), pt.cex=1, cex = 1,
       col = c("gray20"), pt.bg = c("gray70", "white"),
       ncol = 1, title = "Depth", xpd = T)
legend("topright", inset=c(-0.3,0.3), c("Control", "Nutrient", "Salt", "SxN"), 
       pch = 21, pt.cex = 1, cex = 1,
       col = col4, pt.bg = col4a, 
       title = "Treatment", ncol = 1, xpd = T)
legend("topright", inset=c(-0.3,0.7), c("Site 1   ", "Site 3", "Site 5"), 
       pch = c(21,22,24), pt.cex = 1, cex = 1,
       col = "gray20", pt.bg = "white", 
       title = "Site", ncol = 1, xpd = T)
legend("topright", inset = c(-0.30, -0.2), c("Instant Ocean", "Sea water"), lty = c(3, 1),
       lwd = c(2,1), xpd = TRUE)




####
#### Cl to PO4
###
par(mfrow = c(1,1), mar = c(6,16,5,8), xpd=F)
plot(rd$Cl, rd$PO4, pch = c(21,22,24)[rd$Site], col = col4[rd$Treatment],
     bg = ifelse(rd$Depth == "(5-10)", "white", col4a[rd$Treatment]), ylim = c(0,50),
     main = "Soil core chloride:phosphate", xlab = "Chloride", ylab = "Phosphate")
abline(0, ratioSW[8], lty = 1)
abline(0, ratioIO[8], lwd = 2, lty =3)
legend("topright", inset=c(-0.3,-0.0), c("0-5 cm", "5-10 cm"), 
       pch = c(21, 21), pt.cex=1, cex = 1,
       col = c("gray20"), pt.bg = c("gray70", "white"),
       ncol = 1, title = "Depth", xpd = T)
legend("topright", inset=c(-0.3,0.3), c("Control", "Nutrient", "Salt", "SxN"), 
       pch = 21, pt.cex = 1, cex = 1,
       col = col4, pt.bg = col4a, 
       title = "Treatment", ncol = 1, xpd = T)
legend("topright", inset=c(-0.3,0.7), c("Site 1   ", "Site 3", "Site 5"), 
       pch = c(21,22,24), pt.cex = 1, cex = 1,
       col = "gray20", pt.bg = "white", 
       title = "Site", ncol = 1, xpd = T)
legend("topright", inset = c(-0.30, -0.2), c("Instant Ocean", "Sea water"), lty = c(3, 1),
       lwd = c(2,1), xpd = TRUE)


