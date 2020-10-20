rm(list=ls())
BodyFat <- read.csv('BodyFat.csv')

#data observation
#  here is no variable "sex", but from the html file we know that sex will influence the bodyfat deeply?
head(BodyFat)
summary(BodyFat)
cor(BodyFat)
hist(BodyFat_new$BODYFAT,main = c("BodyFat frequency"),breaks = 30)
plot(BodyFat$HEIGHT,BodyFat$BODYFAT,main = "Is height corrlated to bodyfat",xlab='height',ylab = 'bodyfat')
plot(BodyFat$WEIGHT,BodyFat$BODYFAT,main = "Is weight corrlated to bodyfat",xlab='height',ylab = 'bodyfat')
plot(BodyFat$HEIGHT,BodyFat$WEIGHT,main = "However height is highly correlated to weight")
#HEIGHT and WEIGHT have interacitons,though height seems not so correlated to bofyfat, we sholdn't delete it 
#directly

#data clean
BodyFat_new <- BodyFat
BodyFat_new[,c(1,3)] <- NULL
hist(BodyFat_new$BODYFAT,main = c("BodyFat frequency"),breaks = 30)
BodyFat_new <- BodyFat_new[-(BodyFat_new[,5]<1|BodyFat_new[,1]>40),]
#Normally speaking,only very few people has bodyfat smaller than 5%


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

