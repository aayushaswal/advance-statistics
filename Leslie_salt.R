#importing  the file

library(readxl)
LeslieSalt=read_excel("LeslieSalt.xlsx")

#exploring the  data
str(LeslieSalt)
attach(LeslieSalt)
names(LeslieSalt)
LeslieSalt

##changing the data from numerical to factor
LeslieSalt$County=factor(LeslieSalt$County,levels = c(0,1),labels=c("San Mateo","Santa Clara"))
LeslieSalt$Flood=factor(LeslieSalt$Flood,levels=c(0,1),labels = c("otherwise","tidal action"))

str(LeslieSalt)

summary(LeslieSalt)

# regression model  
model=lm(Price~.,data = LeslieSalt)
summary(model)
anova(model)

#from the above output we find that the P- value is very less than .05
#so we reject the null hypothsis that all and accept the alternate hypothesis.
#R-squared = .747 which mean 74.7% of the total variation is explained by this model.
#we also observe that the distance is the least signifance.
#So, we make a model without distance and check its output.

model1=lm(Price~.-Distance,data = LeslieSalt)
summary(model1)
anova(model1)

#from the model2, R-squared value is almost the same.
#also model summary explain that data and size are least significant
#so another model without size and date individually and simultaneously

model2=lm(Price~.-Distance-Size,data = LeslieSalt)
summary(model2)
anova(model2)

model3=lm(Price~.-Distance-Date,data = LeslieSalt)
summary(model3)
anova(model3)

model4=lm(Price~.-Distance-Date-Size,data = LeslieSalt)
summary(model4)
anova(model4)

#Model capablities:-
predicted=predict(model1)
actual=LeslieSalt$Price
backtrack=data.frame(actual,predicted)
backtrack
plot(actual,col="red")
lines(actual,col="red")
plot(predicted,col="blue")
lines(predicted,col="blue")
lines(actual,col="red")

#from the above three model, the R squared value is decresing.
#so we take model1 which predict the most of the variation.

#conclusion:-
#ANSWER-1:-
#data having continious variable,nominal variable and catagorical variable.
# Price is the dependent variable. 
#Country,size,elevation,sewer,date,flood are the independent variable.

#ANSWER-2:-
#Country and Flood has been changed from integer to factor.

#ANSWER-3:-
#regression equation is:-
#Yhat= 2.521e+01 +(-1.005e+01*County)+(-5.425e-03*Size)+(4.995e-01*Elevation)+(-1.054e-03*Sewer)+(7.842e-02*Date)+(-1.219e+01*Flood)
#from the above output we find that the P- value is very less than .05
#so we reject the null hypothsis that all and accept the alternate hypothesis.
#R-squared = .7437 which mean 74.37% of the total variation is explained by this model.
#Adjusted R-squared =0.6797 which mean the actual contribution of the model is 67.97% 