
#' ---
#' title: "soils + salt in lab follow up"
#' author: "Emily Ury"
#' date: "June 16, 2020"
#' output: github_document
#' ---
#'


setwd("C:/Users/eau6/Dropbox (Duke Bio_Ea)/My data/SNAP_compilation/SNAP_Carbon_Story")

x <-  read.csv("2019_SIR_salt_w_eplainers.csv")


x$ID<-paste(x$Site,x$Plot,x$Rep,sep="")


Response <- x$µg_C_CO2_hr_gC
Response <- x$µg_C_CO2_hr_gds

x$Treatment <-replace(x$Treatment, c(1:30), "a_zero")
x$Treatment <-replace(x$Treatment, c(31:60), "b_two")
x$Treatment <- replace(x$Treatment, c(61:90), "c_six")


par(mar = c(7,5,2,2))
boxplot(Response ~ Treatment*Plot*Site, data = x, las = 2, 
        col = c("#f4a582","#f4a582","#f4a582", "#d6604d", "#d6604d","#d6604d",
                "#C2A5CF", "#C2A5CF", "#C2A5CF", "#9970AB","#9970AB","#9970AB",
                "#92C5DE","#92C5DE","#92C5DE",  "#4393C3",  "#4393C3",  "#4393C3" ))


model1 <- lmer(Response ~ Treatment + (1|Site) + (1|Plot) + (1|ID) , data = x, REML = F)
null <- lmer(Response ~  1 + (1|Site) + (1|Plot) + (1|ID), data = x, REML = F)
anova(null, model1) 

summary(model1)







## When using the untransformed variables
model2 <- lmer((Response) ~ (Cl) + (SO4) + (pH) + (1|Site) + (1|Plot) + (1|ID), data = x, REML = F)
summary(model2)
null2 <- lmer((Response) ~ 1 + (1|Site) + (1|Plot) + (1|ID), data = x, REML = F)
anova(null2, model2)






## when using the log transformed variables
model3 <- lmer((Response) ~ logCl + logSO4 + pH + (1|Site) + (1|Plot) + (1|ID), data = x, REML = F)
summary(model3)
null3 <- lmer((Response) ~ 1 + (1|Site) + (1|Plot) + (1|ID), data = x, REML = F)

anova(null3, model3)






## try dropping out the variables
model4a <- lmer(scale(Response) ~ scale(logCl) + scale(logSO4) + (1|Site)+ (1|Plot) + (1|ID), data = x, REML = F)
model4b <- lmer(scale(Response) ~ scale(logCl) + scale(pH) + (1|Site)+ (1|Plot) + (1|ID), data = x, REML = F)
model4c <- lmer(scale(Response) ~ scale(logSO4) + scale(pH) + (1|Site)+ (1|Plot) + (1|ID), data = x, REML = F)
model4d <- lmer(scale(Response) ~ scale(logCl) + (1|Site)+ (1|Plot) + (1|ID), data = x, REML = F)
model4e <- lmer(scale(Response) ~ scale(pH) + (1|Site)+ (1|Plot) + (1|ID), data = x, REML = F)
model4f <- lmer(scale(Response) ~ scale(logSO4) + (1|Site)+ (1|Plot) + (1|ID), data = x, REML = F)
summary(model4e)
null4 <- lmer(scale(Response) ~ 1 + (1|Site) + (1|Plot) + (1|ID), data = x, REML = F)
model4 <- lmer(scale(Response) ~ scale(logCl) + scale(logSO4) + scale(pH) + (1|Site) + (1|Plot) + (1|ID), data = x, REML = F)

anova(null4, model4)  # full

anova(null4, model4a)  # drop pH
anova(null4, model4b)  # drop SO4
anova(null4, model4c)  # drop Cl
anova(null4, model4d)  # Cl alone
anova(null4, model4e)  # ph alone
anova(null4, model4f)  # SO4 alone

anova(model4, model4e)






#### model 5 -- random slopes
## when using the log transformed variables
model5<- lmer((Response) ~  Treatment + (1+Treatment|Site) + (1|Plot) + (1|ID), data = x, REML = F)
summary(model5)
null5 <- lmer((Response) ~ 1 +  (1+Treatment|Site) + (1|Plot) + (1|ID), data = x, REML = F)

anova(null5, model5)
