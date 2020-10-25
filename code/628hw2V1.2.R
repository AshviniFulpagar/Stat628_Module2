rm(list=ls())
BodyFat <- read.csv('BodyFat.csv')

#data observation
#  here is no variable "sex", but from the html file we know that sex will influence the bodyfat deeply?
head(BodyFat)
summary(BodyFat)
cor(BodyFat)


#data clean
BodyFat_new <- BodyFat
BodyFat_new[,c(1,3)] <- NULL
hist(BodyFat_new$BODYFAT,main = c("BodyFat frequency"),breaks = 30)
BodyFat_new <- BodyFat_new[-(BodyFat_new[,5]<1|BodyFat_new[,1]>40),]
#Normally speaking,only very few people has bodyfat smaller than 5%

hist(BodyFat_new$BODYFAT,main = c("BodyFat frequency"),breaks = 30)
plot(BodyFat$HEIGHT,BodyFat$BODYFAT,main = "Is height corrlated to bodyfat",xlab='height',ylab = 'bodyfat')
plot(BodyFat$WEIGHT,BodyFat$BODYFAT,main = "Is weight corrlated to bodyfat",xlab='height',ylab = 'bodyfat')
plot(BodyFat$HEIGHT,BodyFat$WEIGHT,main = "However height is highly correlated to weight")
#HEIGHT and WEIGHT have interacitons,though height seems not so correlated to bofyfat, we sholdn't delete it 
#directly


#model fit 
model_full <- lm(BodyFat_new$BODYFAT~.*.,data = BodyFat_new)#2th interaction 
model_all <- lm(BodyFat_new$BODYFAT~.,data = BodyFat_new)#without interaction
model_zero <- lm(BodyFat_new$BODYFAT~1)
summary(model_full)
#Because we have know that height and weight have interaction, so we can consider 2fi's interaction in our mdoel

model_stepwise <- step(model_full,direction = c('both'))#stepwise to select a mdoel 
model_stepwise2 <- step(model_all,direction = c('both'))#stepwise to select a model without interaction

#now we can take the model without interaction first
#model diagnostic
plot(model_stepwise$fitted.values, model_stepwise$residuals,main = c("residual plot"))
standardized_redidual <- rstandard(model_stepwise2)
studentized_redidual <- rstudent(model_stepwise2)


#indentify outliers of x
x.matrix <- as.matrix(cbind(rep(1,dim(BodyFat_new)[1]),BodyFat_new[,c("AGE","WEIGHT","NECK","ABDOMEN","HIP","THIGH","FOREARM","WRIST")]))
colnames(x.matrix) <- NULL
H.matrix <- x.matrix%*%solve(t(x.matrix)%*%x.matrix)%*%t(x.matrix)
n <- dim(x.matrix)[1]
p <- dim(x.matrix)[2]-1
#H matrix

hii <- H.matrix[c(1:dim(H.matrix)[1]),c(1:dim(H.matrix)[1])]
plot(hii)

which(hii>2*(dim(x.matrix)[2]-1)/dim(x.matrix)[2])
#no x outliers

which(studentized_redidual>qt(1-0.05/(2*n),df=n-p-1,lower.tail = TRUE))#Bonferroni correction
#no Y outliers, equal variance should hold

plot(model_stepwise2)
# check normality ,indenpendence and equal variance

plot(cooks.distance(model_stepwise2))
# check if there is influentials

plot(dffits(model_stepwise2))
# check if there is influentials

cook1 <- cooks.distance(model_stepwise2)
diffbeta1 <- dffits(model_stepwise2)
which(abs(cook1)==max(abs(cook1)))
#38,39 is influential by cook's distance

which(abs(diffbeta1)==max(abs(diffbeta1)))
#also,38,39 us influential by diffits

#let's check them
BodyFat_new[c(38,39),]
rank(BodyFat_new$BODYFAT,)[c(38,39)]
rank(BodyFat_new$WEIGHT,)[c(38,39)]
rank(BodyFat_new$ADIPOSITY,)[c(38,39)]
rank(BodyFat_new$ABDOMEN,)[c(38,39)]
#4 variables of this have very high rank, it's considerable that they are influentials

anova(model_stepwise2)
# if we can through some variables,hip is not significant from anova table

cat('The F-value of variable HIP is',qf(6.9/15.7,1,242,lower.tail = TRUE))
#check if this variable which is not significant in model can be dropped.


#then we can consider the model with 2fi's interaction?(need or not)




##select important variables



summary(model_all)
anova_table <-anova(model_all)#anova table
order_ssr <- order(anova_table$`Sum Sq`[-length(anova_table$`Sum Sq`)],decreasing = TRUE)#sort it
anova_table$`Sum Sq`[order_ssr]#check the largest ssr to find which variables are ralatively important
ranked_name <- rownames(anova_table)[order_ssr]

#to find a appropriate range of important variables to select the model w need;
really_important_varaible <-ranked_name[1:4] 
important_variable <- ranked_name[5:9]
other_variable <- ranked_name[10:14]


really_important_model <- lm(BodyFat_new$BODYFAT~.,data=BodyFat_new[,colnames(BodyFat_new)%in%really_important_varaible])
importantmodel <- lm(BodyFat_new$BODYFAT~.,data=BodyFat_new[,colnames(BodyFat_new)%in%c(really_important_varaible,important_variable)])
  

model_selection <- function(given_information){
  if (sum(really_important_varaible%in%names(given_information))==3&&sum(important_variable%in%names(given_information))<6){
               
    used_data <- given_information[names(given_information)%in%really_important_varaible]
    prediction <- predict(really_important_model,used_data)   
  
  } else if (sum(c(really_important_varaible,important_variable)%in%names(given_information))==9&&sum(other_variable%in%names(given_information))<5){
    
    used_data <- given_information[names(given_information)%in%c(really_important_varaible,important_variable)]
    prediction <- predict(important_model,used_data)
    
  } else if (sum(c(really_important_varaible,important_variable)%in%names(given_information))==9&&sum(other_variable%in%names(given_information))==5){
    
    prediction <- predict(model_all,used_data)
    
    } else{
      cat("Can't predict because the information is not enough!")
    }
  return(prediction)
    
}#if here we can do he stepwise to do a btter prediction

scaled_data <- scale(BodyFat_new)
data_riv <- data.frame(scaled_data[,colnames(BodyFat_new)%in%c(really_important_varaible)])
data_iv <- data.frame(scaled_data[,colnames(BodyFat_new)%in%c(really_important_varaible,important_variable)])

library(MASS)

model_riv_ridge <- lm.ridge(scale(BodyFat_new$BODYFAT)~.-1,data_riv,lambda=seq(0,5,0.1))

plot(model_riv_ridge)
model_riv_ridge_last <- lm.ridge(scale(BodyFat_new$BODYFAT)~.-1,data_riv,lambda=min(model_riv_ridge$GCV))
prediction_riv <- as.matrix(data_riv)%*%as.matrix(model_riv_ridge_last$coef)
plot(scale(BodyFat_new$BODYFAT),type='l',col='red')
lines(prediction_riv,type='l',col='blue')
# prediction_riv <- as.matrix(BodyFat_new[,colnames(BodyFat_new)%in%c(really_important_varaible)])%*%as.matrix(model_riv_ridge_last$coef)
# plot(BodyFat_new$BODYFAT,type='l',col='red')
# lines(prediction_riv,type='l',col='blue')



model_iv_ridge <- lm.ridge(scale(BodyFat_new$BODYFAT)~.-1,data_iv,lambda=seq(0,5,0.1))

plot(model_iv_ridge)
model_iv_ridge_last <- lm.ridge(scale(BodyFat_new$BODYFAT)~.-1,data_iv,lambda=min(model_iv_ridge$GCV))
prediction_iv <- as.matrix(data_iv)%*%as.matrix(model_iv_ridge_last$coef)
plot(scale(BodyFat_new$BODYFAT),type='l',col='red')
lines(prediction_iv,type='l',col='blue')


model_ridge <- lm.ridge(scale(BodyFat_new$BODYFAT)~.-1,data.frame(scaled_data),lambda=seq(0,5,0.1))

plot(model_ridge)
model_ridge_last <- lm.ridge(scale(BodyFat_new$BODYFAT)~.-1,data.frame(scaled_data),lambda=min(model_ridge$GCV))
prediction <- as.matrix(scale(BodyFat_new[,-1]))%*%as.matrix(model_ridge_last$coef)
plot(scale(BodyFat_new$BODYFAT),type='l',col='red')
lines(prediction,type='l',col='blue')

