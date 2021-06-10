#***************************************************************
#*Binome : 
#*         - AZZEDINE LAKHDAR
#*         - YOUSSEF QAISSOUMI
#*Projet : PLATEFORME ET TECHNOLOGIE DE L'IOT
#*
#* année : 2020/2021
#***************************************************************
#install.packages("caTools")
#install.packages("Metrics")
# on importe notre dataset :
mydata = read.csv("Salary_data.csv")
#on affiche les 10 premiers lignes de notre data 
View(head(mydata,10))
#on plot notre data pour mieux comprendre a propos d'elle :
#salaire en fonction d'année passé
x <- mydata$YearsExperience
y <- mydata$Salary
library(ggplot2)
ggplot(mydata, aes(x = YearsExperience, y = Salary)) + geom_point()
#calcul de la corelation entre nos deux variables
cor(mydata$YearsExperience, mydata$Salary, method = c("pearson", "kendall", "spearman"))
# on divise notre dataset en deux parties une pour le training et autre pour tester
# 80% pour trainer notre modele et 20% pour tester
library(caTools)
set.seed(123)
sample = sample.split(mydata,SplitRatio=0.8)
Train_data = mydata[sample==TRUE, ]
Test_data = mydata[sample==FALSE, ]
#construire notre modele
a <- Train_data$YearsExperience
b <- Train_data$Salary
modele = lm(b~a, data = Train_data)
# information a propos de notre modele
s <- summary(modele)
co <- s$coefficients
co[ , 1]
# tracer la ligne de notre modele 
abline(modele)
# ploter notre modele 
ggplot(mydata, aes(x = YearsExperience, y = Salary)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")
# Show attributes of linModel
attributes(modele)
#exemple de prediction pour une valeur
predict(modele, data.frame(a = 3.0))
24800.510/9536.873

#calculation de rmse:
vector2 = predict(modele, data.frame(a = Test_data$YearsExperience))
library(Metrics)
rmse(vector2,Test_data$Salary)
