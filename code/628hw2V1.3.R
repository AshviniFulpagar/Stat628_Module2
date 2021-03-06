# data
rm(list=ls())
BodyFat = read.csv('C:/Users/25098/Desktop/628/Module2/BodyFat.csv')
library(MASS)
library(car)
library(corrplot)
library(ggplot2)
#data observation
head(BodyFat)
summary(BodyFat)

#data cleaning
BodyFat_new <- BodyFat
BodyFat_new[,c("IDNO","DENSITY")] <- NULL

BMI = 703*BodyFat_new$WEIGHT/(BodyFat_new$HEIGHT)^2
dif = BMI-BodyFat$ADIPOSITY
dif[which(dif>0.1)]
View(BodyFat_new[which(dif>0.1),]) # the 42th data point has strange Height
# Use weight and adiposity to estimate the height
BodyFat_new$HEIGHT[42] = round(sqrt(703*BodyFat_new$WEIGHT[42]/BodyFat$ADIPOSITY[42]),2)

hist(BodyFat_new$BODYFAT,main = c("BodyFat frequency"),breaks = 30) #>40
BodyFat_new <- BodyFat_new[-which(BodyFat_new[,1]>40),]
hist(BodyFat_new$AGE,main = c("Age frequency")) 
hist(BodyFat_new$WEIGHT,main = c("Weight frequency")) # >350
BodyFat_new <- BodyFat_new[-which(BodyFat_new$WEIGHT>350),]
hist(BodyFat_new$HEIGHT,main = c("Height frequency")) 
hist(BodyFat_new$ADIPOSITY,main = c("Adiposity frequency")) 
hist(BodyFat_new$NECK,main = c("Neck frequency")) 
hist(BodyFat_new$CHEST,main = c("Chest frequency")) # ?
hist(BodyFat_new$ABDOMEN,main = c("Abdomen frequency")) #?
hist(BodyFat_new$HIP,main = c("Hip frequency")) 
hist(BodyFat_new$THIGH,main = c("Thigh frequency")) 
hist(BodyFat_new$KNEE,main = c("Knee frequency")) 
hist(BodyFat_new$ANKLE,main = c("Ankle frequency")) # ?
hist(BodyFat_new$BICEPS,main = c("Biceps frequency")) 
hist(BodyFat_new$FOREARM,main = c("Forearm frequency")) 
hist(BodyFat_new$WRIST,main = c("Wrist frequency")) 

corrplot(cor(BodyFat_new))
model_all <- lm(BodyFat_new$BODYFAT~.,data = BodyFat_new)#without interaction
plot(vif(model_all),main='VIF plot',ylab = 'VIF')
abline(h=10,col='red')
text(x=c(1:length(vif(model_all))),y=vif(model_all),labels=names(vif(model_all)),col='blue',cex = 0.7,pos = 1)

# BodyFat_new <- BodyFat_new[,c(1,3,4,5)]
# # BodyFat_new$ADIPOSITY<- NULL
# # BodyFat_new$WEIGHT<- NULL
# # #BodyFat_new$HIP<- NULL
corrplot(cor(BodyFat_new))


plot(BodyFat_new$HEIGHT,BodyFat_new$BODYFAT,main = "Is height corrlated to bodyfat",xlab='height',ylab = 'bodyfat')
plot(BodyFat_new$WEIGHT,BodyFat_new$BODYFAT,main = "Is weight corrlated to bodyfat",xlab='height',ylab = 'bodyfat')
plot(BodyFat_new$HEIGHT,BodyFat_new$WEIGHT,main = "However height is highly correlated to weight")
#HEIGHT and WEIGHT have interacitons,though height seems not so correlated to bofyfat, we sholdn't delete it 
#directly




#model fit 
model_full <- lm(BodyFat_new$BODYFAT~.*.,data = BodyFat_new)#2th interaction 
model_all <- lm(BodyFat_new$BODYFAT~.,data = BodyFat_new)#without interaction
model_zero <- lm(BodyFat_new$BODYFAT~1)
summary(model_full)
#Because we have know that height and weight have interaction, so we can consider 2fi's interaction in our mdoel

plot(model_stepwise$fitted.values, model_stepwise$residuals,main = c("residual plot"))
standardized_redidual <- rstandard(model_all)
studentized_redidual <- rstudent(model_all)


#indentify outliers of x
x.matrix <- as.matrix(cbind(rep(1,dim(BodyFat_new)[1]),BodyFat_new[,-1]))
colnames(x.matrix) <- NULL
H.matrix <- x.matrix%*%solve(t(x.matrix)%*%x.matrix)%*%t(x.matrix)
n <- dim(x.matrix)[1]
p <- dim(x.matrix)[2]
#H matrix

hii <- H.matrix[c(1:dim(H.matrix)[1]),c(1:dim(H.matrix)[1])]

which(hii>2*(dim(x.matrix)[2]-1)/dim(x.matrix)[2])
#no x outliers

which(studentized_redidual>qt(1-0.05/(2*n),df=n-p-1,lower.tail = TRUE))#Bonferroni correction
#no Y outliers, equal variance should hold
par(mfrow = c(2,2))
plot(model_all)
# check normality ,indenpendence and equal variance


par(mfrow = c(1,1))
plot(cooks.distance(model_all),ylim = c(-1,1),main="Cook's distance")
abline(h=qf(0.5,p,n-p),col='red')
# check if there is influentials using cook's distance


plot(abs(dffits(model_all)),main="dffits",ylim = c(0,1.5))
abline(h=1,col='red')
# check if there is influentials using diffits

#fit a new model withot interactions
model_all <- lm(BodyFat_new$BODYFAT~.,data = BodyFat_new)
summary(model_all)
anova_table <-anova(model_all)#anova table
ssr<-anova_table$`Sum Sq`[-length(anova_table$`Sum Sq`)]
order_ssr <- order(anova_table$`Sum Sq`[-length(anova_table$`Sum Sq`)],decreasing = TRUE)#sort it
anova_table$`Sum Sq`[order_ssr]#check the largest ssr to find which variables are ralatively important
ranked_name <- rownames(anova_table)[order_ssr]

#to find a appropriate range of important variables to select the model we need;
v_name <- rownames(anova_table)[-length(anova_table$`Sum Sq`)]
really_important_variable <-v_name[ssr>1000] 
important_variable <- v_name[which(ssr>100&ssr<=1000)] 


# 10 fold cross-validation

fold_id = sample(10,nrow(BodyFat_new),replace=TRUE)

MSE_stepwise_full = c()
MSE_stepwise_all = c()
MSE_anova_iv = c()
MSE_anova_riv = c()
MSE_anova_all = c()
MSE_ridge_riv = c()
MSE_ridge_iv = c()
MSE_ridge = c()

for (i in 1:10){
  test = BodyFat_new[fold_id==i,]
  train = BodyFat_new[-which(fold_id==i),]
  # model
  # 1. stepwise
  model_full = lm(BODYFAT~.*.,data = train)#2th interaction 
  model_all = lm(BODYFAT~.,data = train)#without interaction
  model_stepwise = step(model_full,direction = c('both'))#stepwise to select a mdoel 
  model_stepwise2 = step(model_all,direction = c('both'))#stepwise to select a model without interaction
  # predict
  pre_stepwise = predict(model_stepwise,test)
  pre_stepwise2 = predict(model_stepwise2,test)
  # MSE for the ith fold
  MSE_stepwise_full[i] =  sum((test$BODYFAT-pre_stepwise)^2)/length(pre_stepwise)
  MSE_stepwise_all[i] =  sum((test$BODYFAT-pre_stepwise2)^2)/length(pre_stepwise2)
  
  # 2. ANOVA
 
  really_important_model = lm(BodyFat_new$BODYFAT~.,data=BodyFat_new[,colnames(BodyFat_new)%in%really_important_variable])
  important_model = lm(BodyFat_new$BODYFAT~.,data=BodyFat_new[,colnames(BodyFat_new)%in%c(really_important_variable,important_variable)])
  
  # test = test[,colnames(test) %in% c("BODYFAT",colnames(important_model[["model"]]))]
  pre_anova_iv = predict(important_model,test)
  pre_anova_riv = predict(really_important_model,test)
  pre_anova_all = predict(model_all,test)
  MSE_anova_iv[i] = sum((test$BODYFAT-pre_anova_iv)^2)/length(pre_anova_iv)
  MSE_anova_riv[i] = sum((test$BODYFAT-pre_anova_riv)^2)/length(pre_anova_riv)
  MSE_anova_all[i] = sum((test$BODYFAT-pre_anova_all)^2)/length(pre_anova_all)
  
  # 3. ridge regression
  lambda_ori <- seq(0,5,0.1)
  train_riv = data.frame(scale(train[,colnames(train)%in%c(really_important_variable)]))
  scale_test = test
  for (j in 1:ncol(test)){
    scale_test[,j] = (test[,j]-mean(train[,j]))/sd(train[,j])
  }
  test_riv = data.frame(scale_test[,colnames(test)%in%c(really_important_variable)])
  train_iv = data.frame(scale(train[,colnames(train)%in%c(really_important_variable,important_variable)]))
  test_iv = data.frame(scale_test[,colnames(test)%in%c(really_important_variable,important_variable)])
  
  # riv
  model_riv_ridge = lm.ridge(scale(train$BODYFAT)~.-1,train_riv,lambda=lambda_ori)
  model_riv_ridge_last = lm.ridge(scale(train$BODYFAT)~.-1,train_riv,lambda=lambda_ori[model_riv_ridge$GCV==min(model_riv_ridge$GCV)])
  pre_ridge_riv = as.matrix(test_riv)%*%as.matrix(model_riv_ridge_last$coef)
  MSE_ridge_riv[i] = sum((test$BODYFAT-(pre_ridge_riv*sd(train$BODYFAT)+mean(train$BODYFAT)))^2)/length(pre_ridge_riv)
  
  # iv
  model_iv_ridge = lm.ridge(scale(train$BODYFAT)~.-1,train_iv,lambda=lambda_ori)
  model_iv_ridge_last = lm.ridge(scale(train$BODYFAT)~.-1,train_iv,lambda=lambda_ori[model_iv_ridge$GCV==min(model_iv_ridge$GCV)])
  pre_ridge_iv = as.matrix(test_iv)%*%as.matrix(model_iv_ridge_last$coef)
  MSE_ridge_iv[i] = sum((test$BODYFAT-(pre_ridge_iv*sd(train$BODYFAT)+mean(train$BODYFAT)))^2)/length(pre_ridge_iv)
  
  # all
  model_ridge = lm.ridge(scale(train$BODYFAT)~.-1,data.frame(scale(train)),lambda=lambda_ori)
  model_ridge_last = lm.ridge(scale(train$BODYFAT)~.-1,data.frame(scale(train)),lambda=lambda_ori[model_ridge$GCV==min(model_ridge$GCV)])
  pre_ridge = as.matrix(scale_test[,-1])%*%as.matrix(model_ridge_last$coef)
  MSE_ridge[i] = sum((test$BODYFAT-(pre_ridge*sd(train$BODYFAT)+mean(train$BODYFAT)))^2)/length(pre_ridge)

}

cv_result = data.frame(stepwise_full = MSE_stepwise_full ,
                       stepwise_all = MSE_stepwise_all ,
                       ANOVA_iv = MSE_anova_iv ,
                       ANOVA_riv = MSE_anova_riv ,
                       ANOVA_all = MSE_anova_all ,
                       Ridge_riv = MSE_ridge_riv ,
                       Ridge_iv = MSE_ridge_iv ,
                       Ridge = MSE_ridge )
plot(x=seq(1,dim(cv_result)[2],1),y=apply(cv_result,2, mean),type='l',xlim=c(0,dim(cv_result)[2]+1),ylim = c(10,40),ylab = )
points(x=seq(1,dim(cv_result)[2],1),y=apply(cv_result,2, mean),type = 'o',col='red')
text(x=seq(1,dim(cv_result)[2],1),y=apply(cv_result,2, mean),labels = colnames(cv_result),cex = 0.7,pos=1,col='blue')
apply(cv_result,2, mean)

data = data.frame(x = rep(1:10,4), value = c(MSE_stepwise_full ,
                                             MSE_stepwise_all ,
                                             # MSE_anova_iv ,
                                             MSE_anova_riv ,
                                             # MSE_anova_all ,
                                             MSE_ridge_riv 
                                             #MSE_ridge_iv ,
                                             # MSE_ridge 
), 
k=rep(c("stepwise_full",
        "stepwise_all",
        # "ANOVA_iv",
        "ANOVA_riv",
        # "ANOVA_all",
        "Ridge_riv"
        # "Ridge_iv",
        # "Ridge"
),
each=10))

ggplot(data, aes(x=x, y=value, col=k)) + 
  geom_line(aes(linetype=k),size=1.2)+
  labs(title="MSE of 10-fold cross-validation", x="fold", y="MSE")+
  scale_linetype_manual(values=c(1,2,1,2))



data = data.frame(x = rep(1:10,4), value = c(MSE_stepwise_full ,
                                             MSE_stepwise_all ,
                                             MSE_anova_iv ,
                                             #MSE_anova_riv ,
                                             #MSE_anova_all ,
                                             #MSE_ridge_riv, 
                                             MSE_ridge_iv 
                                             #MSE_ridge 
), 
k=rep(c("stepwise_full",
        "stepwise_all",
        "ANOVA_iv",
        #"ANOVA_riv",
        #"ANOVA_all",
        #"Ridge_riv"
        "Ridge_iv"
        #"Ridge"
),
each=10))

ggplot(data, aes(x=x, y=value, col=k)) + 
  geom_line(aes(linetype=k),size=1.2)+
  labs(title="MSE of 10-fold cross-validation", x="fold", y="MSE")+
  scale_linetype_manual(values=c(1,2,1,2))

data = data.frame(x = rep(1:10,2), value = c(MSE_stepwise_full ,
  MSE_stepwise_all ,
  #MSE_anova_iv ,
  #MSE_anova_riv ,
  MSE_anova_all ,
  #MSE_ridge_riv 
  #MSE_ridge_iv ,
  MSE_ridge 
), 
k=rep(c("stepwise_full",
  "stepwise_all",
  # "ANOVA_iv",
  #"ANOVA_riv",
  "ANOVA_all",
  #"Ridge_riv"
  # "Ridge_iv",
  "Ridge"
),
each=10))
ggplot(data, aes(x=x, y=value, col=k)) + 
  geom_line(aes(linetype=k),size=1.2)+
  labs(title="MSE of 10-fold cross-validation", x="fold", y="MSE")+
  scale_linetype_manual(values=c(1,2,1,2))

#go back to the whole data
model_all <- lm(BodyFat_new$BODYFAT~.,data = BodyFat_new)
summary(model_all)
anova_table <-anova(model_all)#anova table
ssr<-anova_table$`Sum Sq`[-length(anova_table$`Sum Sq`)]
order_ssr <- order(anova_table$`Sum Sq`[-length(anova_table$`Sum Sq`)],decreasing = TRUE)#sort it

#to find a appropriate range of important variables to select the model we need;
v_name <- rownames(anova_table)[-length(anova_table$`Sum Sq`)]
really_important_variable <-v_name[ssr>1000] 
important_variable <- v_name[which(ssr>100&ssr<=1000)] 

really_important_model <- lm(BodyFat_new$BODYFAT~.,data=BodyFat_new[,colnames(BodyFat_new)%in%really_important_variable])
important_model <- lm(BodyFat_new$BODYFAT~.,data=BodyFat_new[,colnames(BodyFat_new)%in%c(really_important_variable,important_variable)])
scaled_data <- scale(BodyFat_new)
model_ridge <- lm.ridge(scale(BodyFat_new$BODYFAT)~.-1,data.frame(scaled_data),lambda=seq(0,5,0.1))
model_ridge_last <- lm.ridge(scale(BodyFat_new$BODYFAT)~.-1,data.frame(scaled_data),lambda=min(model_ridge$GCV))
#model generate





model_selection <- function(given_information){
  if (sum(really_important_variable%in%names(given_information))==length(really_important_variable)&sum(important_variable%in%names(given_information))<length(important_variable)){
    
    used_data <- given_information[names(given_information)%in%really_important_variable]
    prediction <- predict(really_important_model,used_data)  
    return(prediction)
    
  } else if (sum(c(really_important_variable,important_variable)%in%names(given_information))==length(c(really_important_variable,important_variable))){
    
    used_data <- given_information[names(given_information)%in%c(really_important_variable,important_variable)]
    prediction <- predict(important_model,used_data)
    return(prediction)
    
  } else{
    cat("Can't predict because the information is not enough!")
    return(NA)
  }
  
}
#f-test
n=dim(BodyFat_new)[1]
p<-length(really_important_model$coefficients)
rimi<-summary(really_important_model)
rima<-anova(really_important_model)
pf(rima$`F value`[-5],1,n-p,lower.tail = FALSE)
n=dim(BodyFat_new)[1]
p<-length(important_model$coefficients)
summary(important_model)
imi<-summary(important_model)
ima<-anova(important_model)
pf(ima$`F value`[-9],1,n-p,lower.tail = FALSE)

#model diagnostic
#really important model 
#indentify outliers of x
x.matrix <- as.matrix(cbind(rep(1,dim(BodyFat_new)[1]),BodyFat_new[,colnames(BodyFat_new)%in%really_important_variable]))
colnames(x.matrix) <- NULL
H.matrix <- x.matrix%*%solve(t(x.matrix)%*%x.matrix)%*%t(x.matrix)
n <- dim(x.matrix)[1]
p <- dim(x.matrix)[2]
#H matrix

hii <- H.matrix[c(1:dim(H.matrix)[1]),c(1:dim(H.matrix)[1])]

which(hii>2*(dim(x.matrix)[2]-1)/dim(x.matrix)[2])
#no x outliers
studentized_redidual <- rstudent(really_important_model)
which(studentized_redidual>qt(1-0.05/(2*n),df=n-p-1,lower.tail = TRUE))#Bonferroni correction
#no Y outliers, equal variance should hold
par(mfrow = c(2,3))
plot(really_important_model)
# check normality ,indenpendence and equal variance

plot(cooks.distance(really_important_model),ylim = c(-1,1),main="Cook's distance")
abline(h=qf(0.5,p,n-p-1),col='red')
# check if there is influentials using cook's distance


plot(abs(dffits(really_important_model)),main="absolute values of dffits",ylim = c(0,1.5))
abline(h=1,col='red')
# check if there is influentials using diffits


x.matrix <- as.matrix(cbind(rep(1,dim(BodyFat_new)[1]),BodyFat_new[,colnames(BodyFat_new)%in%important_variable]))
colnames(x.matrix) <- NULL
H.matrix <- x.matrix%*%solve(t(x.matrix)%*%x.matrix)%*%t(x.matrix)
n <- dim(x.matrix)[1]
p <- dim(x.matrix)[2]
#H matrix

hii <- H.matrix[c(1:dim(H.matrix)[1]),c(1:dim(H.matrix)[1])]

which(hii>2*(dim(x.matrix)[2]-1)/dim(x.matrix)[2])
#no x outliers
studentized_redidual <- rstudent(important_model)
which(studentized_redidual>qt(1-0.05/(2*n),df=n-p-1,lower.tail = TRUE))#Bonferroni correction
#no Y outliers, equal variance should hold
par(mfrow = c(2,3))
plot(important_model)
# check normality ,indenpendence and equal variance

plot(cooks.distance(important_model),ylim = c(-1,1),main="Cook's distance")
abline(h=qf(0.5,p,n-p-1),col='red')
# check if there is influentials using cook's distance


plot(abs(dffits(important_model)),main="absolute values of dffits",ylim = c(0,1.5))
abline(h=1,col='red')
# check if there is influentials using diffits