setwd("~/Documents/EMORY_PHD/year 5/Chapter 2/data")

#read all data in 
Bari_diam_data <- read.csv("Bari_diam.csv")
Ndiol_Maure_data <- read.csv("Ndiol_Maure.csv")
Ndiol_Maure_data <- read.csv("Ndiol_Maure.csv")[,-c(16, 19)] # cut out "3rd Urea application" and "Weeds" columns
Ndelle_Boye_data <- read.csv("Ndelle_boye.csv")
Ndelle_Boye_data <- subset(Ndelle_Boye_data,Farmer != "Amar BOYE" | Plot != 3) #take out this data
Ndelle_Boye_data <- subset(Ndelle_Boye_data,Farmer != "Abdou BOYE" | !(Plot %in%  c(1,5))) #take out this data
Mboltogne_data <- read.csv("Mboltonge.csv")
Mboltogne_data <- subset(Mboltonge_data,Farmer != "Ngadie GAYE" | !(Plot %in%  c(2,3))) #take out this data



head(Bari_diam_data)
head(Ndelle_Boye_data)
head(Mboltogne_data)
head(Ndiol_Maure_data)

library("tidyverse")
library("glmmTMB")
library("ggplot2")
#install.packages("ggbreak")
library(ggbreak) 
library(patchwork)

### Example subsetting when farmers worked together
Controls = subset(Bari_diam_data, Treatment == "Control")
Azolla = subset(Bari_diam_data, Treatment == "Azolla")
head(Controls)
tail(Controls)

plot(Controls$Plot, Azolla$Plot)

Rel_Change_Az = 100*(Azolla$Yield.at.14...Kg.ha. - Controls$Yield.at.14...Kg.ha.)/Controls$Yield.at.14...Kg.ha.

Controls[,"Percent_Change_Az"] = Rel_Change_Az
head(Controls)
hist(Controls$Percent_Change_Az)

mean(Controls$Percent_Change_Az) # percent change
sd(Controls$Percent_Change_Az)/sqrt(dim(Controls)[1]) #Standard error of the percent change

#### Code for Bari diam, where the plots were physically separated
#m_Az = glmmTMB(Percent_Change_Az ~ 1, data=subset(Controls, Farmer == "Oumar DIOUF"))
#summary(m_Az)

#m_Az = glmmTMB(Percent_Change_Az ~ 1, data=subset(Controls, Farmer != "Oumar DIOUF"))
#summary(m_Az)






### Example subsetting when farmers worked independently or plots were separate
##Bari Diam
ControlsF1 = subset(Bari_diam_data, Treatment == "Control" & Farmer == "Oumar DIOUF")
AzollaF1 = subset(Bari_diam_data, Treatment == "Azolla" & Farmer == "Oumar DIOUF")
head(ControlsF1)
tail(ControlsF1)

plot(ControlsF1$Plot, AzollaF1$Plot)

Rel_Change_Az = 100*(AzollaF1$Yield.at.14...Kg.ha. - ControlsF1$Yield.at.14...Kg.ha.)/ControlsF1$Yield.at.14...Kg.ha.

ControlsF1[,"Percent_Change_Az"] = Rel_Change_Az
head(ControlsF1)
hist(ControlsF1$Percent_Change_Az)

mean(ControlsF1$Percent_Change_Az) # percent change
sd(ControlsF1$Percent_Change_Az)/sqrt(dim(ControlsF1)[1]) #Standard error of the percent change

Village_means = data.frame("Site" = "Bari Diam 1", "Percent_change_Az" = mean(ControlsF1$Percent_Change_Az), "Percent_change_AZ_SE" = sd(ControlsF1$Percent_Change_Az)/sqrt(dim(ControlsF1)[1]))

ControlsF2 = subset(Bari_diam_data, Treatment == "Control" & Farmer != "Oumar DIOUF")
AzollaF2 = subset(Bari_diam_data, Treatment == "Azolla" & Farmer != "Oumar DIOUF")
head(ControlsF2)
tail(ControlsF2)

plot(ControlsF2$Plot, AzollaF2$Plot)

Rel_Change_Az = 100*(AzollaF2$Yield.at.14...Kg.ha. - ControlsF2$Yield.at.14...Kg.ha.)/ControlsF2$Yield.at.14...Kg.ha.

ControlsF2[,"Percent_Change_Az"] = Rel_Change_Az
head(ControlsF2)
hist(ControlsF2$Percent_Change_Az)

mean(ControlsF2$Percent_Change_Az) # percent change
sd(ControlsF2$Percent_Change_Az)/sqrt(dim(ControlsF2)[1]) #Standard error of the percent change

Village_means = rbind(Village_means, data.frame("Site" = "Bari Diam 2","Percent_change_Az" = mean(ControlsF2$Percent_Change_Az), "Percent_change_AZ_SE" = sd(ControlsF2$Percent_Change_Az)/sqrt(dim(ControlsF2)[1]) ))

ggplot(data= Village_means, aes(x=Site, y = Percent_change_Az)) + geom_point() + 
  geom_linerange(aes(ymin = Percent_change_Az - Percent_change_AZ_SE, ymax = Percent_change_Az + Percent_change_AZ_SE))


###Ndelle Boye
Controls_NB = subset(Ndelle_Boye_data, Treatment == "Control")
Azolla_NB = subset(Ndelle_Boye_data, Treatment == "Azolla")
head(Controls_NB)
tail(Controls_NB)

plot(Controls_NB$Plot, Azolla_NB$Plot)

Rel_Change_Az_NB = 100*(Azolla_NB$Yield.at.14...Kg.ha. - Controls_NB$Yield.at.14...Kg.ha.)/Controls_NB$Yield.at.14...Kg.ha.

Controls_NB[,"Percent_Change_Az"] = Rel_Change_Az_NB
head(Controls_NB)
hist(Controls_NB$Percent_Change_Az)

mean(Controls_NB$Percent_Change_Az) # percent change
sd(Controls_NB$Percent_Change_Az)/sqrt(dim(Controls_NB)[1]) #Standard error of the percent change

Village_means = rbind(Village_means, data.frame("Site" = "Ndelle Boye","Percent_change_Az" = mean(Controls_NB$Percent_Change_Az), "Percent_change_AZ_SE" = sd(Controls_NB$Percent_Change_Az)/sqrt(dim(Controls_NB)[1]) ))

##PLOT
ggplot(data= Village_means, aes(x=Site, y = Percent_change_Az)) + geom_point() + 
  geom_linerange(aes(ymin = Percent_change_Az - Percent_change_AZ_SE, ymax = Percent_change_Az + Percent_change_AZ_SE))



##Ndiol Maure
Controls_NM = subset(Ndiol_Maure_data, Treatment == "Control")
Azolla_NM = subset(Ndiol_Maure_data, Treatment == "Azolla")
head(Controls_NM)
tail(Controls_NM)

plot(Controls_NM$Plot, Azolla_NM$Plot)

Rel_Change_Az_NM = 100*(Azolla_NM$Yield.at.14...Kg.ha. - Controls_NM$Yield.at.14...Kg.ha.)/Controls_NM$Yield.at.14...Kg.ha.

Controls_NM[,"Percent_Change_Az"] = Rel_Change_Az_NM
head(Controls_NM)
hist(Controls_NM$Percent_Change_Az)

mean(Controls_NM$Percent_Change_Az) # percent change
sd(Controls_NM$Percent_Change_Az)/sqrt(dim(Controls_NM)[1]) #Standard error of the percent change

Village_means = rbind(Village_means, data.frame("Site" = "Ndiole Maure","Percent_change_Az" = mean(Controls_NM$Percent_Change_Az), "Percent_change_AZ_SE" = sd(Controls_NM$Percent_Change_Az)/sqrt(dim(Controls_NM)[1]) ))

##PLOT
ggplot(data= Village_means, aes(x=Site, y = Percent_change_Az)) + geom_point() + 
  geom_linerange(aes(ymin = Percent_change_Az - Percent_change_AZ_SE, ymax = Percent_change_Az + Percent_change_AZ_SE))





##Mboltonge
Controls_M = subset(Mboltogne_data, Treatment == "Control")
Azolla_M = subset(Mboltogne_data, Treatment == "Azolla")
head(Controls_M)
tail(Controls_M)

plot(Controls_M$Plot, Azolla_M$Plot)

Rel_Change_Az_M = 100*(Azolla_M$Yield.at.14...Kg.ha. - Controls_M$Yield.at.14...Kg.ha.)/Controls_M$Yield.at.14...Kg.ha.

Controls_M[,"Percent_Change_Az"] = Rel_Change_Az_M
head(Controls_M)
hist(Controls_M$Percent_Change_Az)

mean(Controls_M$Percent_Change_Az) # percent change
sd(Controls_M$Percent_Change_Az)/sqrt(dim(Controls_M)[1]) #Standard error of the percent change

Village_means = rbind(Village_means, data.frame("Site" = "Mboltogne","Percent_change_Az" = mean(Controls_M$Percent_Change_Az), "Percent_change_AZ_SE" = sd(Controls_M$Percent_Change_Az)/sqrt(dim(Controls_M)[1]) ))

##PLOT with all 5 sites
ggplot(data= Village_means, aes(x=Site, y = Percent_change_Az)) + geom_point() +
  geom_linerange(aes(ymin = Percent_change_Az - Percent_change_AZ_SE, ymax = Percent_change_Az + Percent_change_AZ_SE)) +
  labs(x="Village Sites", y = "Rice Grain Yield Percent Change (%)") + theme_classic() 



# OVERRALL Village_means
overall_mean_AZ <- mean(Village_means$Percent_change_Az)



#Village_means = rbind(Village_means, data.frame("Site" = "Overall","Percent_change_Az" = mean(Village_means$Percent_Change_Az)))
#Village_means = rbind(Village_means, "Site")

## Plot overall on current graph
ggplot(data= Village_means, aes(x=Site, y = Percent_change_Az)) + geom_point() +
  geom_linerange(aes(ymin = Percent_change_Az - Percent_change_AZ_SE, ymax = Percent_change_Az + Percent_change_AZ_SE)) +
  labs(title = "Azolla additions", x="Village Sites", y = "Rice Grain Yield Percent Change (%)") + theme_classic() +
  geom_point(aes(x="Overall",y=overall_mean_AZ),colour="red")

#Save plot
ggsave("yieldchange_AzollaAdditions.pdf")
ggsave("yieldchange_AzollaAdditions.png")

##OVERALL AZ_SE means
#overall_mean_SE <- mean(Village_means$Percent_change_AZ_SE)

#Village_means = rbind(Village_means, data.frame("Site" = "Overall","Percent_change_Az" = mean(Village_means$Percent_change_Az), "Percent_change_AZ_SE" = Village_means$Percent_change_AZ_SE))


#################### Modelling for Azolla vs Control ##################

#### Code for Bari diam, where the plots were physically separated
m_Az = glmmTMB(Percent_Change_Az ~ 1, data=subset(Controls, Farmer == "Oumar DIOUF"))
summary(m_Az)

m_Az = glmmTMB(Percent_Change_Az ~ 1, data=subset(Controls, Farmer != "Oumar DIOUF"))
summary(m_Az)




#### Example code for other villages, where plots were adjacent and farmers worked together
m_Az = glmmTMB(Percent_Change_Az ~ 1, data=Controls_M)
summary(m_Az)

m_Az = glmmTMB(Percent_Change_Az ~ 1, data=Controls_NM)
summary(m_Az)

m_Az = glmmTMB(Percent_Change_Az ~ 1, data=Controls_NB)
summary(m_Az)


####### Overall village mean and SE's
ControlsF1[,"Village"] = "Bari Diam 1"
ControlsF2[,"Village"] = "Bari Diam 2"
Controls_M
Controls_NM
Controls_NB

Village_percentchange = rbind(ControlsF1,ControlsF2,Controls_M,Controls_NM,Controls_NB)
head(Village_percentchange)

moverall_Az = glmmTMB(Percent_Change_Az ~ 1 + (1|Village), data=Village_percentchange)
summary(moverall_Az)

#Village_means = rbind(Village_means, data.frame("Site" = "Overall","Percent_change_Az" = mean(Village_means$Percent_Change_Az)))
Village_means = data.frame(rbind(Village_means, c(Site = "Overall",Percent_change_Az = coef(summary(moverall_Az))$cond[1,1],Percent_change_AZ_SE = coef(summary(moverall_Az))$cond[1,2])))

Village_means[,2] = as.numeric(Village_means[,2])
Village_means[,3] = as.numeric(Village_means[,3])

## Plot overall on current graph
p1 <- ggplot(data= data.frame(Village_means), aes(x = Site, y = Percent_change_Az)) + geom_point() +
  geom_linerange(aes(ymin = Percent_change_Az - Percent_change_AZ_SE, ymax = Percent_change_Az + Percent_change_AZ_SE)) +
 scale_y_continuous(limits = c(-20,40)) + 
  labs( tag= "A", y = "Rice Grain Yield Percent Change (%)\n with Addition", x = "") + theme_classic() + geom_hline(yintercept = 0, linetype = 2)+
  theme(axis.text.x = element_text(face="bold")) + 
  theme(axis.text.y = element_text(face="bold")) + 
  geom_vline(xintercept = 5.5, color = "blue", alpha=0.2, size=5 ) 


###### Combining both figures
#install.packages("cowplot")
library(cowplot)

#read all data in 
Bari_diam_data <- read.csv("Bari_diam.csv")
Ndiol_Maure_data <- read.csv("Ndiol_Maure.csv")
Ndiol_Maure_data <- read.csv("Ndiol_Maure.csv")[,-c(16, 19)] # cut out "3rd Urea application" and "Weeds" columns
Ndelle_Boye_data <- read.csv("Ndelle_boye.csv")
Ndelle_Boye_data <- subset(Ndelle_Boye_data,Farmer != "Amar BOYE" | Plot != 3) #take out this data
Ndelle_Boye_data <- subset(Ndelle_Boye_data,Farmer != "Abdou BOYE" | !(Plot %in%  c(1,5))) #take out this data
Mboltogne_data <- read.csv("Mboltogne.csv")
Mboltogne_data <- subset(Mboltogne_data,Farmer != "Ngadie GAYE" | !(Plot %in%  c(2,3))) #take out this data



head(Bari_diam_data)
head(Ndelle_Boye_data)
head(Mboltogne_data)
head(Ndiol_Maure_data)

library("tidyverse")
library("glmmTMB")

### Example subsetting when farmers worked together
Controls = subset(Bari_diam_data, Treatment == "Control")
Azolla = subset(Bari_diam_data, Treatment == "Azolla-Urea")
head(Controls)
tail(Controls)

plot(Controls$Plot, Azolla$Plot)

Rel_Change_Az = 100*(Azolla$Yield.at.14...Kg.ha. - Controls$Yield.at.14...Kg.ha.)/Controls$Yield.at.14...Kg.ha.

Controls[,"Percent_Change_Az"] = Rel_Change_Az
head(Controls)
hist(Controls$Percent_Change_Az)

mean(Controls$Percent_Change_Az) # percent change
sd(Controls$Percent_Change_Az)/sqrt(dim(Controls)[1]) #Standard error of the percent change

#### Code for Bari diam, where the plots were physically separated
#m_Az = glmmTMB(Percent_Change_Az ~ 1, data=subset(Controls, Farmer == "Oumar DIOUF"))
#summary(m_Az)

#m_Az = glmmTMB(Percent_Change_Az ~ 1, data=subset(Controls, Farmer != "Oumar DIOUF"))
#summary(m_Az)






### Example subsetting when farmers worked independently or plots were separate
##Bari Diam
ControlsF1 = subset(Bari_diam_data, Treatment == "Control" & Farmer == "Oumar DIOUF")
AzollaF1 = subset(Bari_diam_data, Treatment == "Azolla-Urea" & Farmer == "Oumar DIOUF")
head(ControlsF1)
tail(ControlsF1)

plot(ControlsF1$Plot, AzollaF1$Plot)

Rel_Change_Az = 100*(AzollaF1$Yield.at.14...Kg.ha. - ControlsF1$Yield.at.14...Kg.ha.)/ControlsF1$Yield.at.14...Kg.ha.

ControlsF1[,"Percent_Change_Az"] = Rel_Change_Az
head(ControlsF1)
hist(ControlsF1$Percent_Change_Az)

mean(ControlsF1$Percent_Change_Az) # percent change
sd(ControlsF1$Percent_Change_Az)/sqrt(dim(ControlsF1)[1]) #Standard error of the percent change

Village_means = data.frame("Site" = "Bari Diam 1", "Percent_change_Az" = mean(ControlsF1$Percent_Change_Az), "Percent_change_AZ_SE" = sd(ControlsF1$Percent_Change_Az)/sqrt(dim(ControlsF1)[1]))

ControlsF2 = subset(Bari_diam_data, Treatment == "Control" & Farmer != "Oumar DIOUF")
AzollaF2 = subset(Bari_diam_data, Treatment == "Azolla-Urea" & Farmer != "Oumar DIOUF")
head(ControlsF2)
tail(ControlsF2)

plot(ControlsF2$Plot, AzollaF2$Plot)

Rel_Change_Az = 100*(AzollaF2$Yield.at.14...Kg.ha. - ControlsF2$Yield.at.14...Kg.ha.)/ControlsF2$Yield.at.14...Kg.ha.

ControlsF2[,"Percent_Change_Az"] = Rel_Change_Az
head(ControlsF2)
hist(ControlsF2$Percent_Change_Az)

mean(ControlsF2$Percent_Change_Az) # percent change
sd(ControlsF2$Percent_Change_Az)/sqrt(dim(ControlsF2)[1]) #Standard error of the percent change

Village_means = rbind(Village_means, data.frame("Site" = "Bari Diam 2","Percent_change_Az" = mean(ControlsF2$Percent_Change_Az), "Percent_change_AZ_SE" = sd(ControlsF2$Percent_Change_Az)/sqrt(dim(ControlsF2)[1]) ))

ggplot(data= Village_means, aes(x=Site, y = Percent_change_Az)) + geom_point() + 
  geom_linerange(aes(ymin = Percent_change_Az - Percent_change_AZ_SE, ymax = Percent_change_Az + Percent_change_AZ_SE))


###Ndelle Boye
Controls_NB = subset(Ndelle_Boye_data, Treatment == "Control")
Azolla_NB = subset(Ndelle_Boye_data, Treatment == "Azolla-Urea")
head(Controls_NB)
tail(Controls_NB)

plot(Controls_NB$Plot, Azolla_NB$Plot)

Rel_Change_Az_NB = 100*(Azolla_NB$Yield.at.14...Kg.ha. - Controls_NB$Yield.at.14...Kg.ha.)/Controls_NB$Yield.at.14...Kg.ha.

Controls_NB[,"Percent_Change_Az"] = Rel_Change_Az_NB
head(Controls_NB)
hist(Controls_NB$Percent_Change_Az)

mean(Controls_NB$Percent_Change_Az) # percent change
sd(Controls_NB$Percent_Change_Az)/sqrt(dim(Controls_NB)[1]) #Standard error of the percent change

Village_means = rbind(Village_means, data.frame("Site" = "Ndelle Boye","Percent_change_Az" = mean(Controls_NB$Percent_Change_Az), "Percent_change_AZ_SE" = sd(Controls_NB$Percent_Change_Az)/sqrt(dim(Controls_NB)[1]) ))

##PLOT
ggplot(data= Village_means, aes(x=Site, y = Percent_change_Az)) + geom_point() + 
  geom_linerange(aes(ymin = Percent_change_Az - Percent_change_AZ_SE, ymax = Percent_change_Az + Percent_change_AZ_SE))



##Ndiol Maure
Controls_NM = subset(Ndiol_Maure_data, Treatment == "Control")
Azolla_NM = subset(Ndiol_Maure_data, Treatment == "Azolla-Urea")
head(Controls_NM)
tail(Controls_NM)

plot(Controls_NM$Plot, Azolla_NM$Plot)

Rel_Change_Az_NM = 100*(Azolla_NM$Yield.at.14...Kg.ha. - Controls_NM$Yield.at.14...Kg.ha.)/Controls_NM$Yield.at.14...Kg.ha.

Controls_NM[,"Percent_Change_Az"] = Rel_Change_Az_NM
head(Controls_NM)
hist(Controls_NM$Percent_Change_Az)

mean(Controls_NM$Percent_Change_Az) # percent change
sd(Controls_NM$Percent_Change_Az)/sqrt(dim(Controls_NM)[1]) #Standard error of the percent change

Village_means = rbind(Village_means, data.frame("Site" = "Ndiole Maure","Percent_change_Az" = mean(Controls_NM$Percent_Change_Az), "Percent_change_AZ_SE" = sd(Controls_NM$Percent_Change_Az)/sqrt(dim(Controls_NM)[1]) ))

##PLOT
ggplot(data= Village_means, aes(x=Site, y = Percent_change_Az)) + geom_point() + 
  geom_linerange(aes(ymin = Percent_change_Az - Percent_change_AZ_SE, ymax = Percent_change_Az + Percent_change_AZ_SE))




##Mboltonge
Controls_M = subset(Mboltogne_data, Treatment == "Control")
Azolla_M = subset(Mboltogne_data, Treatment == "Azolla-Urea")
head(Controls_M)
tail(Controls_M)

plot(Controls_M$Plot, Azolla_M$Plot)

Rel_Change_Az_M = 100*(Azolla_M$Yield.at.14...Kg.ha. - Controls_M$Yield.at.14...Kg.ha.)/Controls_M$Yield.at.14...Kg.ha.

Controls_M[,"Percent_Change_Az"] = Rel_Change_Az_M
head(Controls_M)
hist(Controls_M$Percent_Change_Az)

mean(Controls_M$Percent_Change_Az) # percent change
sd(Controls_M$Percent_Change_Az)/sqrt(dim(Controls_M)[1]) #Standard error of the percent change

Village_means = rbind(Village_means, data.frame("Site" = "Mboltogne","Percent_change_Az" = mean(Controls_M$Percent_Change_Az), "Percent_change_AZ_SE" = sd(Controls_M$Percent_Change_Az)/sqrt(dim(Controls_M)[1]) ))

# OVERRALL Village_meansO
overall_mean_AZ <- mean(Village_means$Percent_change_Az)


##PLOT
ggplot(data= Village_means, aes(x=Site, y = Percent_change_Az)) + geom_point() + theme_classic () +
  geom_linerange(aes(ymin = Percent_change_Az - Percent_change_AZ_SE, ymax = Percent_change_Az + Percent_change_AZ_SE))+
  labs(title = "Azolla Substitutions", x="Village Sites", y = "Rice Grain Yield Percent Change (%)") +
  geom_point(aes(x="Overall",y=overall_mean_AZ),colour="red")

#Save plot
ggsave("yieldchange_AzollaSubstitutions.pdf")
ggsave("yieldchange_AzollaSubstitutions.png")

#################### Modelling for Azolla vs Control ##################

#### Code for Bari diam, where the plots were physically separated
m_Az = glmmTMB(Percent_Change_Az ~ 1, data=subset(Controls, Farmer == "Oumar DIOUF"))
summary(m_Az)

m_Az = glmmTMB(Percent_Change_Az ~ 1, data=subset(Controls, Farmer != "Oumar DIOUF"))
summary(m_Az)




#### Example code for other villages, where plots were adjacent and farmers worked together
m_Az = glmmTMB(Percent_Change_Az ~ 1, data=Controls_M)
summary(m_Az)

m_Az = glmmTMB(Percent_Change_Az ~ 1, data=Controls_NM)
summary(m_Az)

m_Az = glmmTMB(Percent_Change_Az ~ 1, data=Controls_NB)
summary(m_Az)


####### Overall village mean and SE's
####### Overall village mean and SE's
ControlsF1[,"Village"] = "Bari Diam 1"
ControlsF2[,"Village"] = "Bari Diam 2"
Controls_M
Controls_NM
Controls_NB

Village_percentchange = rbind(ControlsF1,ControlsF2,Controls_M,Controls_NM,Controls_NB)
head(Village_percentchange)

moverall_Az = glmmTMB(Percent_Change_Az ~ 1 + (1|Village), data=Village_percentchange)
summary(moverall_Az)

#Village_means = rbind(Village_means, data.frame("Site" = "Overall","Percent_change_Az" = mean(Village_means$Percent_Change_Az)))
Village_means = data.frame(rbind(Village_means, c(Site = "Overall",Percent_change_Az = coef(summary(moverall_Az))$cond[1,1],Percent_change_AZ_SE = coef(summary(moverall_Az))$cond[1,2])))

Village_means[,2] = as.numeric(Village_means[,2])
Village_means[,3] = as.numeric(Village_means[,3])

## Plot overall on current graph
p2 <- ggplot(data= data.frame(Village_means), aes(x=Site, y = Percent_change_Az)) + geom_point() +
  geom_linerange(aes(ymin = Percent_change_Az - Percent_change_AZ_SE, ymax = Percent_change_Az + Percent_change_AZ_SE)) +
  scale_y_continuous(limits = c(-20,40)) +
  labs(tag = "B", x="Village Sites", y = "Rice Grain Yield Percent Change (%)\n with Substitution") + theme_classic() + geom_hline(yintercept = 0, linetype = 2)+
  theme(axis.text.x = element_text(face="bold")) + 
  theme(axis.text.y = element_text(face="bold")) + 
  geom_vline(xintercept = 5.5, color = "blue", alpha=0.2, size=5 ) 

plot_grid(p1, p2, nrow = 2)

#Save plot
ggsave("yieldchange_AzollaAdditions_Sub.pdf")
ggsave("yieldchange_AzollaAdditions_Sub.png")
#ggsave("yieldchange_AzollaAdditions_Sub.TIFF", res = 300)
ggsave("yieldchange_AzollaAdditions_Sub.TIFF", width = 7, height = 7, device='tiff', dpi=300)

dev.off()
