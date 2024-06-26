# regression logistique avec les fleurs d'iris
# Installation des packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(datasets)
# charger le dataset
data("iris")
# structure des données
iris
str(iris)
# résumé des donnés
summary(iris)
# analyse univarié 
boxplot(iris$Sepal.Length,col='blue',ylab="Petal.length",
        main="diagramme en boxplot")
grid(col='green')

# Analyse bivariée
# observer la correlation entre les valeurs numériques
cor_matrix <- cor(iris[, 1:4])

# Print la correlation
print(cor_matrix)

# Plot the correlation matrix  avec corrplot
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, # Text label color and rotation
         addCoef.col = "black", # Add correlation coefficients
         cl.pos = "b", # Position of the color legend
         number.cex = 0.7) # Size of the correlation coefficients
# diagramme en boxplot entre espéces et sepal width
ggplot(iris,aes(x=Species,y=Sepal.Width,color=Species ))+
    geom_boxplot()+
    theme_bw()+
    facet_wrap(~Species)+
    labs(title = "boxplot sepal.width")
# diagramme en boxplot entre espéces et sepal Pétal
ggplot(iris,aes(x=Sepal.Width ,y=Sepal.Length),fill=Species)+
    geom_point(alpha=0.5,size=4)+
    theme_classic()+
    geom_smooth(method=lm,col="red")+
    facet_wrap(~Species)
ggplot(iris,aes(x=Petal.Width ,y=Petal.Length),colors=TRUE)+
    geom_point(alpha=0.5,size=4)+
    theme_classic()+
    geom_smooth(method=lm,col="red")+
    facet_wrap(~Species)
# data manipulation
data<-iris
data
data %>%
    select(Petal.Width,Petal.Length) 
data %>% 
    filter(Species=='virginica')
data %>% 
    group_by(Species) %>% 
    summarize(average=mean(Sepal.Length,na.rm = TRUE),
              sd=sd(Sepal.Width,na.rm = TRUE),count=n())
# Naives bayes algorith machine learning
options(warnings=-50)
# Machine learning
library(e1071)
library(PreProcessing)
library(nnet)
# Partitionner les données
df<-iris
set.seed(123) # fixer la graine aléatoire
ind<-sample(2,nrow(df),replace=T, prob=c(0.70,0.30))
dim(ind)
train<-df[ind==1,]  # donnée entrainement
test<-df[ind==2,]    # donnée test
# dimension des données
dim(train)
dim(test)
mod<-naiveBayes(Species~.,data=train)
mod
pred<-predict(mod,test)
pred[1:10]
tab<-table(Predict=pred,Actual=test$Species)
tab
accuracy<-sum(diag(tab))/sum(tab)*100
accuracy
specificity<-100-accuracy
specificity
# regression logistique avec les fleurs d'iris
glm=multinom(Species~.,data=train)
# résumé du modéle
summary(glm)
# prediction du modéle
pred<-predict(glm,test,type="class")
pred[1:10]  # prédiction des 10 premiers fleurs
# Confusion Matrix
tab<-table(Predict=pred,Actual=test$Species)
tab
# calcul de l'accuracy
accuracy<-sum(diag(tab))/sum(tab)*100
accuracy     # le modéle fait une prédiction correcte à 90%
# spécificité
specificity<-100-accuracy
specificity


