install.packages("caTools")
# on importe notre dataset :
mydata = read.csv("Salary_data.csv")
#on affiche les 10 premiers lignes de notre data 
View(head(mydata,10))
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
modele = lm(Train_data$Salary~Train_data$YearsExperience, data = Train_data)
# Show attributes of linModel
attributes(modele)
#exemple de prediction
predict(modele, data.frame(YearsExperience = 1.1))
