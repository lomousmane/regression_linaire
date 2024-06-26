library(dplyr)
library(tidyverse)
data<-oats
View(data)
names(data)=c("block","variete","nitrogene","yield")
str(data)
summary(data)
# analyse univarié
table(data$variete)
# visualisation du rendement
ggplot(data,aes(x=yield),color="blue")+geom_histogram(bins=10)

# analyse bivariee
p=ggplot(data,aes(x=variete,y=yield,fill=variete))+
geom_boxplot()+theme_bw()
p
ggplotly(p)

p_1=ggplot(data,aes(x=block,y=yield,fill=variete))+
    geom_boxplot()+theme_bw()
p_1
ggplotly(p)
# relation nirogen et variété

p_2=ggplot(data,aes(x=nitrogene,y=yield,fill=variete))+
    geom_boxplot()+theme_bw()
p_2
ggplotly(p)

# analyse de la regression linéaire entre le rendement et 
# variables explicatives (variete,nitogéne)
modele=lm(yield~nitrogene+variete+block,data=data)
modele
summary(modele)
# analyse de la variance
anova(modele)



