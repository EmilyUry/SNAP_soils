#' ---
#' title: "PCAs of all soil core measurements"
#' author: "Emily Ury"
#' date: "May 7, 2020"
#' output: github_document
#' ---
#'


#' Created this script to look at general the soil properties of
#' SNAP soil cores,  how they are distributed across the three plots, 
#' how the treatments are affecting these properties, etc...
#'  
#' Data is from the *soils mastersheet* from **2019**


library(ecodist)


setwd("C:/Users/eau6/Dropbox (Duke Bio_Ea)/My data/SNAP_compilation/SNAP_Carbon_Story")
data <- read.csv("2019_SNAP_master.csv", header = TRUE)
data$PO4_P_µg_l[is.na(data$PO4_P_µg_l)] <- 2   ### set the below detection
data$Mg_mg_L[is.na(data$Mg_mg_L)] <- 0.001  ## set the below detection
data$Treatment <- as.factor(data$Treatment)

col3 <- c("#d6604d", "black", "#9970AB","black", "#4393C3") ## medium
col4 <- c("#5AAE61", "#d6604d", "#9970AB", "#4393C3")
col4 <- c("#FDE725FF", "#55C667FF", "#33638DFF", "#481567FF")  # viridis
col4a <- c("#FDE72599", "#55C66799", "#33638D99", "#48156799")


cid <- data[,2:5]  ## stratifying information (core ID) 
schar <- data[,6:32]
names(schar) <- c("Cond.", "B.D.", "S.M.", "LOI", "pH", "Roots", "DOC", "TDN", 
                 "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
                 "Cmin.s", "Cmin.c", "SIR.s", "SIR.c", "Br", "Phenol", "NO3", "PO4")
chem <- data[, c(10,14,15,16,17,18,19,22,31,32)] ## soil characteristics (pH, Cl, SO4, Na, K, Mg, Ca, NH4, NO3, PO4)
phys <- data[, c(7, 8, 9)]
resp <- data[, c(9, 12, 25, 26, 27, 28, 30)]



## do a PCA for chemical characterisitcs of all soil cores
pca <-prcomp(chem, center = TRUE, scale = TRUE)
print(pca)

summary(pca) #100% of PCA variance, but not the actual NMS axis variance
pca.scores<-pca$x

df <- cbind(cid, pca.scores[,1:2])

df <- cbind(df, resp)


### facet plot the PCA

par(mfrow = c(1,4), mar = c(12,4,8,1))

plot(df$PC1, df$PC2, pch = 16, cex = 0.9, main = "PCA all soil cores", 
     xlab = "PC1", ylab = "PC2")

df1 <- df[which(df$Site == "1"),]
df3 <- df[which(df$Site == "3"),]
df5 <- df[which(df$Site == "5"),]


par(mfrow = c(1,4), mar = c(12,4,8,1), xpd=TRUE)
plot(df$PC1, df$PC2, pch = 16, cex = 0.9, main = "PCA all soil cores", 
     xlab = "PC1", ylab = "PC2", 
     xlim = c(-8, 3), ylim = c(-3,5))
plot(df1$PC1, df1$PC2, pch = 22, cex = 1.5, main = "Site 1 (dry - pine/oak)", 
     xlab = "PC1", ylab = "", 
     xlim = c(-8, 3), ylim = c(-3,5),
     col = col4[df1$Treatment], 
     bg = ifelse(df1$Depth == "(5-10)", "white", col4a[df1$Treatment]))

legend("bottomright", inset=c(0.0,-0.4), c("0-5 cm", "5-10 cm"), 
       pch = c(16, 1), pt.cex=1.5, cex = 1.1,
       col = c("gray50"), ncol = 2, title = "Depth")

plot(df3$PC1, df3$PC2, pch = 21, cex = 1.5, main = "Site 3 (oak)", 
     xlab = "PC1", ylab = "",
     xlim = c(-8, 3), ylim = c(-3,5),
     col = col4[df3$Treatment],
     bg = ifelse(df3$Depth == "(5-10)", "white", col4a[df3$Treatment]))

legend("bottomleft", inset=c(0,-0.4), c("Control", "Nutrient", "Salt", "SxN"), 
       pch = 21, pt.cex = 1.5, cex = 1.2,
       col = col4, pt.bg = col4a, 
       title = "Treatment", ncol = 2)

plot(df5$PC1, df5$PC2, pch = 24, cex = 1.5, main = "Site 5 (wet - cypress/mixed)", 
     xlab = "PC1", ylab = "", 
     xlim = c(-8, 3), ylim = c(-3,5),
     col = col4[df5$Treatment],
     bg = ifelse(df5$Depth == "(5-10)", "white", col4a[df5$Treatment]))









