setwd("~/Documents/EMORY_PHD/year 5/Chapter 2/data")


#### MODEL COORELATION###
#read all data in 
Bari_diam_data <- read.csv("Bari_diam2.csv")
Ndiol_Maure_data <- read.csv("Ndiol_Maure.csv")
Ndiol_Maure_data <- read.csv("Ndiol_Maure.csv")[,-c(16, 19)] # cut out "3rd Urea application" and "Weeds" columns
Ndelle_Boye_data <- read.csv("Ndelle_boye.csv")
Ndelle_Boye_data <- subset(Ndelle_Boye_data,Farmer != "Amar BOYE" | Plot != 3) #take out this data
Ndelle_Boye_data <- subset(Ndelle_Boye_data,Farmer != "Abdou BOYE" | !(Plot %in%  c(1,5))) #take out this data
Mboltonge_data <- read.csv("Mboltonge.csv")
Mboltonge_data <- subset(Mboltonge_data,Farmer != "Ngadie GAYE" | !(Plot %in%  c(2,3))) #take out this data


all_village_sheets = rbind(Ndiol_Maure_data, Bari_diam_data, Ndelle_Boye_data, Mboltonge_data)
head(all_village_sheets)

cor_data <- subset(all_village_sheets, select = c("Village", "Farmer", "Plot","Subplot" ,"Treatment", "Number.of.panicle.plant","X100.Grain.Weight..g.","Yield.at.14...Kg.ha."))
head(cor_data)

#data from all villages with 10 plant samples
masterdata10 <- read.csv("masterdata11_v2.csv")
library(tidyverse)
plot_data = masterdata10 %>%
              group_by(Farmer, Plot, Subplot) %>%
                summarise(Tiller.Number = mean(Tiller.Number), Plant.Length..cm. = mean(Plant.Length..cm.), 
                          Panicle.Length..cm. = mean(Panicle.Length..cm.), Panicle.Dry.Weight..g. = mean(Panicle.Dry.Weight..g.))
head(masterdata10)
colnames(masterdata10)
#install.packages("dplyr")
library(dplyr)


#Take two data sheets and join some columns together. all data is in!
cor_data <- left_join(x= cor_data, y = plot_data, by = c("Farmer", "Plot", "Subplot"))

#rename col
colnames(cor_data)= c("Village", "Farmer", "Plot","Subplot", "Treatment","Number_panicle","hundred_grain","Yield_14","Tiller_number",
                      "Plant_length", "Panicle_length","Panicle_weight")

#looking at the Full model with all variables
mFull = lm (Yield_14 ~ Village + Number_panicle + hundred_grain + Tiller_number + Plant_length + Panicle_length + Panicle_weight, data = cor_data, na.action = "na.fail")
summary(mFull)

#install.packages("MuMIn")
library(MuMIn)

#automatic model selection
mod_sel <- dredge(mFull, trace = TRUE)
mod_sel

#average all possible models
mod_avg <- model.avg(mod_sel, fit = TRUE)
summary(mod_avg) #winning model is variables 1,3,5,6,7. Use full average.

#predicted value and SE of yield

#changed to NM because its closest to the mean of all villages
pred.data <- data.frame("hundred_grain" = seq(1.25,3.25,0.1), "Village"= "Ndiole Maure",
                        "Panicle_length"= 0, "Plant_length" = 0, "Tiller_number" = 0, "Panicle_weight" = 0, "Number_panicle" = 0 )

#fit + SE, fit, Fit - SE, Regression line  + 1SE of prediction
confid_line <-predict(mod_avg, newdata = pred.data, se.fit = T)


#mod_avg$coefficients # we want the want the one that says FULL. FULL = variables not in the model equals 0 
# hundred grain, number of panicle, and tiller name are all good estimates for the model since the numbers are similar b/w full and subset

##Use full average. hund grain weight, tiller number, and villages. Less tillers = more yield. heavier grains explains the yield not number of grains
#mod_avg <- model.avg(mod_sel)
#summary(mod_avg)

#Get R square for winning model. Variables, 1,3,5,6,7
m_Win <-lm (Yield_14 ~ Village + hundred_grain + Tiller_number + Plant_length + Panicle_length, data = cor_data, na.action = "na.fail")
summary(m_Win) # multiple r square = .5251


#yield vs hundred grain weight for each village. 

library(ggplot2)
library(ggthemes)
library(RColorBrewer)

#plot = ggplot(cor_data, aes(x=hundred_grain, y=Yield_14, color=Village)) + 
 # theme_classic(base_size=20)+
 # geom_point() +
#  geom_abline(intercept=-2707.401,slope = 5657.782,color = brewer.pal(8, "Dark2")[1])+ #bari diam 1
 # geom_abline(intercept= -2707.401 + 1674.752 ,slope = 5657.782 , color = brewer.pal(8, "Dark2")[2])+ #bari Diam 2
  #geom_abline(intercept= -2707.401 + -19.129 ,slope = 5657.782 , color = brewer.pal(8, "Dark2")[3])+  #Mboltoge
  #geom_abline(intercept= -2707.401 + -2954.131 ,slope = 5657.782 , color = brewer.pal(8, "Dark2")[4])+ # Ndelle Boye
  #geom_abline(intercept= -2707.401 + -1428.614 ,slope = 5657.782 , color = brewer.pal(8, "Dark2")[5])+ #Ndiol Maure
  # xlim(0,3)+
#  scale_colour_brewer(palette = "Dark2")+
#  ggtitle("Relationship Between Hundred Grain Weight \nand Rice Grain Yield")+
#  xlab("Hundred Grain Weight (g)") +
#  ylab("Yield (kg/ha)")+
#  theme(plot.title = element_text(hjust = 0.5))
#plot

#Save plot
#ggsave("~/Documents/EMORY_PHD/year 5/Chapter 2/data/fig2".jpeg", device = "jpeg", width=6, height=6,plot_for_fig2, dpi=300)
#ggsave("yield_hund_grain.pdf", width = 10, height = 6, dpi = 300)
#ggsave("yield_hund_grain.png", width = 10, height = 6, dpi = 300)

## Real plot that i will use for MS. 
plot = ggplot(cor_data, aes(x=hundred_grain, y=Yield_14, color = Village)) + 
  labs(color = "Village Sites") + 
  theme_classic(base_size=20)+
  geom_point() +
  geom_abline(intercept=-2707.401-1428.614,slope = 5657.782)+ #bari diam 1
  # xlim(0,3)+
  geom_ribbon(data= data.frame(confid_line), inherit.aes= FALSE, aes(x=seq(1.25,3.25,0.1), ymin=fit-se.fit, ymax=fit+se.fit), alpha=.3) + 
  scale_colour_brewer(palette = "Set1") +
  xlab("Hundred Grain Weight (g)") +
  ylab("Rice Yield (kg/ha)")+
  theme(plot.title = element_text(hjust = 0.5))
plot

ggsave("yield_hund_grain.pdf", width = 10, height = 6, dpi = 300)
ggsave("yieldchange_AzollaAdditions_Sub.TIFF", width = 10, height = 6, device='tiff', dpi=300)



#Notes
#Bari diam is the average of the treatments but not the data
#Ndiol Maure is the average of the data which is why we changed it to NM
#rel between 100 grain yeidl with intercept set to the study wide average of NM
#if you zoon in you will see the middle concave in but doesnt have to be visually obvious. 





