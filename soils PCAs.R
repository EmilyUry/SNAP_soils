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

names(data) <- c("Date","Site", "Treatment", "Core", "Depth", "Cond.", "B.D.", 
                 "S.M.", "LOI", "pH", "Roots", "DOC", "TDN", 
                  "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
                  "Cmin.s", "Cmin.c", "SIR.s", "SIR.c", "Br", "Phenol", "NO3", "PO4")

col3 <- c("#d6604d", "black", "#9970AB","black", "#4393C3") ## medium
col4 <- c("#5AAE61", "#d6604d", "#9970AB", "#4393C3")
col4 <- c("#FDE725FF", "#55C667FF", "#33638DFF", "#481567FF")  # viridis
col4a <- c("#FDE72599", "#55C66799", "#33638D99", "#48156799")


cid <- data[,2:5]  ## stratifying information (core ID) 
schar <- data[,6:32]

chem <- data[, c(10,14,15,16,17,18,19,22,31,32)] ## soil vars (pH, Cl, SO4, Na, K, Mg, Ca, NH4, NO3, PO4)
phys <- data[, c(7, 8, 9)]
resp <- data[, c(9, 12, 25, 26, 27, 28, 30)]

par(mfrow = c(1,1), mar = c(4,8,4,5))

#' Create a PCA for chemical characterisitcs of all soil cores
#' 
pca <-prcomp(chem, center = TRUE, scale = TRUE)
print(pca)
plot(pca)

#' How many components to keep? 
#' Test the Kaiser criterion  (Kaiser 1960)

plot(pca,type="line",cex.lab=1.5, cex.main=1.5)
abline(h=1,lty=3, col="red") ## keep only the first three principle components (var > 1)


## explore some correlations
cor(chem$Cl, pca$x[,1])
plot(chem$Cl, pca$x[,1], xlab = "Chloride", ylab = "PC1", frame = F)
plot(resp$Cmin.s, pca$x[,1], xlab = "Cmin", ylab = "PC1", frame = F)  ## no correlation

summary(pca) #100% of PCA variance, but not the actual NMS axis variance
pca.scores<-pca$x
pca.loading <- pca$rotation
df <- cbind(cid, pca.scores[,1:3])
df <- cbind(df, resp)

#' Plot the pca

plot(df$PC1, df$PC2, pch = 16, cex = 0.9, main = "PCA all soil cores", 
     xlab = "PC1", ylab = "PC2")
arrows(0,0, pca.loading[,1]*7, pca.loading[,2]*7, length = 0.1, lwd = 1.5, col = "red")
text(pca.loading[,1]*7.5, pca.loading[,2]*7.3, row.names(pca.loading), cex = 0.8, col = "red")

#' Facet out the PCA, a plot for each experimental site

df1 <- df[which(df$Site == "1"),]
df3 <- df[which(df$Site == "3"),]
df5 <- df[which(df$Site == "5"),]


par(mfrow = c(1,4), mar = c(12,3,8,1), xpd=TRUE)
plot(df$PC1, df$PC2, pch = 16, cex = 0.9,
     xlab = "PC1", ylab = "PC2", 
     xlim = c(-8, 3), ylim = c(-3,5))
arrows(0,0, pca.loading[,1]*7.5, pca.loading[,2]*7.5, length = 0.1, lwd = 1.5, col = "red")
text(pca.loading[,1]*8, pca.loading[,2]*8, row.names(pca.loading), cex = 1, col = "red")
title(main = "PCA all soil cores", line = 1)

plot(df1$PC1, df1$PC2, pch = 22, cex = 1.5, 
     xlab = "PC1", ylab = "", 
     xlim = c(-8, 3), ylim = c(-3,5),
     col = col4[df1$Treatment], 
     bg = ifelse(df1$Depth == "(5-10)", "white", col4a[df1$Treatment]))
title(main = "Site 1 (dry - pine/oak)", line = 1)

legend("bottomright", inset=c(0.0,-0.4), c("0-5 cm", "5-10 cm"), 
       pch = c(21, 21), pt.cex=1.5, cex = 1.2,
       col = c("gray20"), pt.bg = c("gray70", "white"),
       ncol = 1, title = "Depth")

plot(df3$PC1, df3$PC2, pch = 21, cex = 1.5, 
     xlab = "PC1", ylab = "",
     xlim = c(-8, 3), ylim = c(-3,5),
     col = col4[df3$Treatment],
     bg = ifelse(df3$Depth == "(5-10)", "white", col4a[df3$Treatment]))
title(main = "Site 3 (oak)", line = 1)

legend("bottomleft", inset=c(0,-0.4), c("Control", "Nutrient", "Salt", "SxN"), 
       pch = 21, pt.cex = 1.5, cex = 1.2,
       col = col4, pt.bg = col4a, 
       title = "Treatment", ncol = 2)

plot(df5$PC1, df5$PC2, pch = 24, cex = 1.5, 
     xlab = "PC1", ylab = "", 
     xlim = c(-8, 3), ylim = c(-3,5),
     col = col4[df5$Treatment],
     bg = ifelse(df5$Depth == "(5-10)", "white", col4a[df5$Treatment]))
title(main = "Site 5 (wet - cypress/mixed)", line = 1)




#### some basic extra visualization using the package

library(factoextra)
fviz_eig(pca)
fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


#### get the info out of the pca
# Eigenvalues
eig.val <- get_eigenvalue(pca)
eig.val

# Results for Variables
res.var <- get_pca_var(pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(pca)
#res.ind$coord          # Coordinates
#res.ind$contrib        # Contributions to the PCs
#res.ind$cos2           # Quality of representation 