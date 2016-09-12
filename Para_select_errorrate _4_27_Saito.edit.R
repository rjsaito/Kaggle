dim(Train.comp)
n<-dim(Train.comp)[1]
#further divide the data, select 1/10 for training
set.seed(24)
traindx<-sample(c(1:n),n*1/10)
Train.comp_train<-Train.comp[traindx,]
Train.comp_test<-Train.comp[-traindx,]
n1=dim(Train.comp_test)[1]
tunindx<-sample(c(1:n1),n1*1/2)
Train.comp_tun=Train.comp_test[tunindx,]
Train.comp_test=Train.comp_test[-tunindx,]
###################fitting linear model###################
lindat<-data.frame(cbind(Train.comp_train[,2],Train.comp_train[,4:ncol(Train.comp)]))
colnames(lindat)[1]='V1' #may need to change the first colname to V1
linear<-glm(V1~.,data=lindat,family=binomial)
#predict the test data by using tunnning data to decide threshold
linnew<-as.data.frame(Train.comp_tun[,4:ncol(Train.comp)])
litepre<-predict(linear,newdata =linnew,type = c("response"))
#plot ROC curve
library(ROCR)
liprediction<-prediction(litepre,Train.comp_tun[,2])
liperformance <- performance( liprediction, "tpr", "fpr" )
plot(liperformance)
#calculating AUC
liauc<-performance(liprediction, "auc")@y.values[[1]]    ###area below the line  0.5526044
#select the cut off point
liperformance@x.name
liperformance@y.name
liperformance@alpha.name
licut<-data.frame(cut=performance( liprediction, "tpr", "fpr")@alpha.values[[1]],
                  fpr=performance( liprediction, "tpr", "fpr")@x.values[[1]],
                  tpr=performance( liprediction, "tpr", "fpr")@y.values[[1]])
licut$dis<-(licut$tpr-1)^2+licut$fpr^2 #calculate the distance of the point to (0,1)
licutpoint<-licut$cut[which(licut$dis==min(licut$dis))]   ## get the cutting point 1
###### use the third data to calculate the error rate
linnew1<-as.data.frame(Train.comp_test[,4:ncol(Train.comp)])
litepre1<-predict(linear,newdata =linnew1,type = c("response"))
litepre1[which(litepre1>=licutpoint)]=1
litepre1[which(litepre1<licutpoint)]=0
elinear=sum(litepre1!=Train.comp_test[,2])/nrow(Train.comp_test)   ## error rate is 0.4894936

###################fitting ridge model###################
library(glmnet)
ridge<-glmnet(as.matrix(Train.comp_train[,4:ncol(Train.comp)]),as.matrix(Train.comp_train[,2]),
              family=c("binomial"),alpha=0,standardize = FALSE)
ridge$lambda
nrlam<-length(ridge$lambda)
rpre<-predict(ridge,newx=as.matrix(Train.comp_train[,4:ncol(Train.comp)]),type = c("response"))
dim(rpre)   ### for each lambda, fit a model. Then use thoes 100 models to predict.


#########calculating AIC and BIC of different lambda########
rresult<-data.frame(lambda=rep(0,nrlam),RSS=rep(0,nrlam),AIC=rep(0,nrlam),BIC=rep(0,nrlam))
for (i in c(1:nrlam))
{
  rresult$lambda[i]<-ridge$lambda[i]
  rresult$RSS[i]<-sum((rpre[,i]-Train.comp_train[,2])^2)
  rresult$AIC[i]<-n*log(rresult$RSS[i]/n)+2*ridge$df[i]
  rresult$BIC[i]<-n*log(rresult$RSS[i]/n)+ridge$df[i]*log(n)
}
plot(ridge$lambda,rresult$AIC,main = "Lambda*AIC plot for ridge regression")
plot(ridge$lambda,rresult$BIC,main = "Lambda*BIC plot for ridge regression")

#select the lambda that yelds the smallest AIC or BIC
rlamAIC<-rresult$lambda[which(rresult$AIC==min(rresult$AIC))]
rlamBIC<-rresult$lambda[which(rresult$BIC==min(rresult$BIC))]

#use the best lambda(under AIC) to predict the test data
bridge<-glmnet(as.matrix(Train.comp_train[,4:ncol(Train.comp)]),as.matrix(Train.comp_train[,2]),
               family=c("binomial"),alpha=0,lambda=rlamAIC,standardize = FALSE)
rtepre<-predict(bridge,newx=as.matrix(Train.comp_tun[,4:ncol(Train.comp)]),type = c("response"))
#plot ROC curve
library(ROCR)
rprediction<-prediction(rtepre,Train.comp_tun[,2])
rperformance <- performance( rprediction, "tpr", "fpr" )
plot(rperformance)
#calculating AUC
rauc<-performance( rprediction, "auc")@y.values[[1]]    #0.7219335

#select the cut off point
rperformance@x.name
rperformance@y.name
rperformance@alpha.name
rcut<-data.frame(cut=performance( rprediction, "tpr", "fpr")@alpha.values[[1]],
                 fpr=performance( rprediction, "tpr", "fpr")@x.values[[1]],
                 tpr=performance( rprediction, "tpr", "fpr")@y.values[[1]])
rcut$dis<-(rcut$tpr-1)^2+rcut$fpr^2 #calculate the distance of the point to (0,1)
rcutpoint<-rcut$cut[which(rcut$dis==min(rcut$dis))]    ## get the cutting point 0.7281093
#####calculate the error rate
rtepre1<-predict(bridge,newx=as.matrix(Train.comp_test[,4:ncol(Train.comp)]),type = c("response"))
rtepre1[which(rtepre1>=rcutpoint)]=1
rtepre1[which(rtepre1<rcutpoint)]=0
eridge=sum(rtepre1!=Train.comp_test[,2])/nrow(Train.comp_test)   ## error rate is 0.3415104

###########fitting lasso model, standardize predictors#############
library(glmnet)
lasso<-glmnet(as.matrix(Train.comp_train[,4:ncol(Train.comp)]),as.matrix(Train.comp_train[,2]),
              family=c("binomial"),alpha=1,standardize = TRUE,nlambda = 100)
lasso$lambda
nllam<-length(lasso$lambda)
lpre<-predict(lasso,newx=as.matrix(Train.comp_train[,4:ncol(Train.comp)]),type=c("response"))
dim(lpre)

#############calculating AIC and BIC of different lambda##############
lresult<-data.frame(lambda=rep(0,nllam),RSS=rep(0,nllam),AIC=rep(0,nllam),BIC=rep(0,nllam))
for (i in c(1:nllam))
{
  lresult$lambda[i]<-lasso$lambda[i]
  lresult$RSS[i]<-sum((lpre[,i]-Train.comp_train[,2])^2)
  lresult$AIC[i]<-n*log(lresult$RSS[i]/n)+2*lasso$df[i]
  lresult$BIC[i]<-n*log(lresult$RSS[i]/n)+lasso$df[i]*log(n)
}
plot(lasso$lambda,lresult$AIC,main = "Lambda*AIC plot for lasso regression")
plot(lasso$lambda,lresult$BIC,main = "Lambda*BIC plot for lasso regression")

#select the lambda that yelds the smallest AIC or BIC
llamAIC<-lresult$lambda[which(lresult$AIC==min(lresult$AIC))]    ### is not the smallest one
llamBIC<-lresult$lambda[which(lresult$BIC==min(lresult$BIC))]

#use the best lambda to predict the test data
blasso<-glmnet(as.matrix(Train.comp_train[,4:ncol(Train.comp)]),as.matrix(Train.comp_train[,2]),
               family=c("binomial"),alpha=1,standardize = TRUE,lambda=llamAIC)
ltepre<-predict(blasso,newx=as.matrix(Train.comp_tun[,4:ncol(Train.comp)]),type = c("response"))
#plot ROC curve
library(ROCR)
lprediction<-prediction(ltepre,Train.comp_tun[,2])
lperformance <- performance( lprediction, "tpr", "fpr" )
plot(lperformance)
#calculating AUC
lauc<-performance(lprediction, "auc")@y.values[[1]]   ## auc for lasso is 0.7142276
#select the cut off point
lperformance@x.name
lperformance@y.name
lperformance@alpha.name
lcut<-data.frame(cut=performance( lprediction, "tpr", "fpr")@alpha.values[[1]],
                 fpr=performance( lprediction, "tpr", "fpr")@x.values[[1]],
                 tpr=performance( lprediction, "tpr", "fpr")@y.values[[1]])
lcut$dis<-(lcut$tpr-1)^2+lcut$fpr^2 #calculate the distance of the point to (0,1)
lcutpoint<-lcut$cut[which(lcut$dis==min(lcut$dis))]     ## get the cutting point 0.7531652
#########calculate the error rate
ltepre1<-predict(blasso,newx=as.matrix(Train.comp_test[,4:ncol(Train.comp)]),type = c("response"))
ltepre1[which(ltepre1>=lcutpoint)]=1
ltepre1[which(ltepre1<lcutpoint)]=0
elasso=sum(ltepre1!=Train.comp_test[,2])/nrow(Train.comp_test)   ## error rate is 0.353251

###########fitting elastic net model, alpha start from 0.1 to 0.9##############
library(glmnet)
for (i in c(1:9))
{
  k=i/10
  assign(paste("elastic",i,sep=""),glmnet(as.matrix(Train.comp_train[,4:ncol(Train.comp)]),as.matrix(Train.comp_train[,2]),
                                          family=c("binomial"),alpha=k,standardize = TRUE))
  Temp<-eval(as.name(paste("elastic",i,sep="")))
  nelam<-length(Temp$lambda)
  epre<-predict(Temp,newx=as.matrix(Train.comp_train[,4:ncol(Train.comp)]),type="response")
  dim(epre)
  
  ###############calculating AIC and BIC of different lambda##############
  assign(paste("eresult",i,sep=""),data.frame(lambda=rep(0,nelam),
                                              RSS=rep(0,nelam),AIC=rep(0,nelam),BIC=rep(0,nelam)))
  for (j in c(1:nelam))
  {
    eval(parse(text=paste("eresult",i,"$lambda[",j,"]","<-Temp$lambda[",j,"]",sep = "")))
    eval(parse(text=paste("eresult",i,"$RSS[",j,"]",
                          "<-sum((epre[,",j,"]-Train.comp_train[,2])^2)",sep = "")))
    eval(parse(text=paste("eresult",i,"$AIC[",j,"]",
                          "<-n*log(eresult",i,"$RSS[",j,"]/n)+2*Temp$df[",j,"]",sep = "")))
    eval(parse(text=paste("eresult",i,"$BIC[",j,"]",
                          "<-n*log(eresult",i,"$RSS[",j,"]/n)+Temp$df[",j,"]*log(n)",sep = "")))
  }
}

#select the lambda that yelds the smallest AIC or BIC in each alpha
elam<-data.frame(elamAIC=rep(0,9),eAIC=rep(0,9),elamBIC=rep(0,9),eBIC=rep(0,9))
for (i  in 1:9)
{
  elam$eAIC[i]<-min(eval(parse(text=paste("eresult",i,"$AIC",sep=""))))
  elam$eBIC[i]<-min(eval(parse(text=paste("eresult",i,"$BIC",sep=""))))
  elam$elamAIC[i]<-eval(parse(text=paste(
    "eresult",i,"$lambda[which(eresult",i,"$AIC==min(eresult",i,"$AIC))]",sep = "")))
  elam$elamBIC[i]<-eval(parse(text=paste(
    "eresult",i,"$lambda[which(eresult",i,"$BIC==min(eresult",i,"$BIC))]",sep="")))
}

elamAIC<-elam$elamAIC[which(elam$eAIC==min(elam$eAIC))]
ealpAIC<-which(elam$eAIC==min(elam$eAIC))/10
elamBIC<-elam$elamBIC[which(elam$eBIC==min(elam$eBIC))]
ealpBIC<-which(elam$eBIC==min(elam$eBIC))/10

#use the best lambda and alpha to predict the test data
belastic<-glmnet(as.matrix(Train.comp_train[,4:ncol(Train.comp)]),as.matrix(Train.comp_train[,2]),
                 family=c("binomial"),alpha=ealpAIC,standardize = TRUE,lambda=elamAIC)
etepre<-predict(belastic,newx=as.matrix(Train.comp_tun[,4:ncol(Train.comp)]),type = c("response"))
#plot ROC curve
library(ROCR)
eprediction<-prediction(etepre,Train.comp_tun[,2])
eperformance <- performance( eprediction, "tpr", "fpr" )
plot(eperformance)
#calculating AUC
eauc<-performance(eprediction, "auc")@y.values[[1]]     ###0.7139342
#select the cut off point
eperformance@x.name
eperformance@y.name
eperformance@alpha.name
ecut<-data.frame(cut=performance( eprediction, "tpr", "fpr")@alpha.values[[1]],
                 fpr=performance( eprediction, "tpr", "fpr")@x.values[[1]],
                 tpr=performance( eprediction, "tpr", "fpr")@y.values[[1]])
ecut$dis<-(ecut$tpr-1)^2+ecut$fpr^2 #calculate the squared distance of the point to (0,1)
ecutpoint<-ecut$cut[which(ecut$dis==min(ecut$dis))]     ### cutting point 0.7505447
etepre1<-predict(belastic,newx=as.matrix(Train.comp_test[,4:ncol(Train.comp)]),type = c("response"))
etepre1[which(etepre1>=ecutpoint)]=1
etepre1[which(etepre1<ecutpoint)]=0
enet=sum(etepre1!=Train.comp_test[,2])/nrow(Train.comp_test)   ## error rate is 0.3506463

###############fitting SCAD model, standardize predictors#############
library(ncvreg)
scad <- ncvreg(Train.comp_train[,4:ncol(Train.comp)],Train.comp_train[,2],family="binomial",penalty="SCAD")
#########calculating AIC and BIC of different lambda########
plot(scad$lambda,AIC(scad))
plot(scad$lambda,BIC(scad))
#select the lambda that yelds the smallest AIC or BIC
rlamAIC<-scad$lambda[which(AIC(scad)==min(AIC(scad)))]
rlamBIC<-scad$lambda[which(BIC(scad)==min(BIC(scad)))]
#use the best lambda to predict the test data
bscad<-ncvreg(Train.comp_train[,4:ncol(Train.comp)],Train.comp_train[,2],family="binomial",penalty="SCAD",lambda=rlamAIC)
stepre<-predict(bscad,as.matrix(Train.comp_tun[,4:ncol(Train.comp)]),type = c("response"))
#plot ROC curve
library(ROCR)
sprediction<-prediction(stepre,Train.comp_tun[,2])
sperformance <- performance( sprediction, "tpr", "fpr" )
plot(sperformance)
#calculating AUC
sauc<-performance(sprediction, "auc")@y.values[[1]]       ##0.7226478
#select the cut off point
sperformance@x.name
sperformance@y.name
sperformance@alpha.name
scut<-data.frame(cut=performance( sprediction, "tpr", "fpr")@alpha.values[[1]],
                 fpr=performance( sprediction, "tpr", "fpr")@x.values[[1]],
                 tpr=performance( sprediction, "tpr", "fpr")@y.values[[1]])
scut$dis<-(scut$tpr-1)^2+scut$fpr^2 #calculate the distance of the point to (0,1)
scutpoint<-scut$cut[which(scut$dis==min(scut$dis))]     ####   0.7413722
####calculate the error rate
stepre1<-predict(bscad,as.matrix(Train.comp_test[,4:ncol(Train.comp)]),type = c("response"))
stepre1[which(stepre1>=scutpoint)]=1
stepre1[which(stepre1<scutpoint)]=0
escad=sum(stepre1!=Train.comp_test[,2])/nrow(Train.comp_test)     ###  0.3445038


###########################result summary##############################
data.frame(name=c("Logistic","Lasso","Ridge","Elastic net","SCAD"),
           AUC=c(liauc,lauc,rauc,eauc,sauc),
           CUT=c(licutpoint,lcutpoint,rcutpoint,ecutpoint,scutpoint),
           ErrorRate=c(elinear,elasso,eridge,enet,escad))


varimp = read.csv(paste(path, "imp.500runs.0.1.csv", sep=""))
blasso.0coef = which(blasso$beta[,1] == 0)
blasso.non0coef = which(blasso$beta[,1] != 0)
imp.0coef = merge(varimp, cbind(blasso.0coef), by.x = "X",
	by.y = "row.names")
imp.non0coef = merge(varimp, cbind(blasso.non0coef), by.x = "X",
	by.y = "row.names")

bscad.0coef = which(bscad$beta[,1] == 0)
bscad.non0coef = which(bscad$beta[,1] != 0)
imp.0coef.scad = merge(varimp, cbind(bscad.0coef), by.x = "X",
	by.y = "row.names")
imp.non0coef.scad = merge(varimp, cbind(bscad.non0coef), by.x = "X",
	by.y = "row.names")

with(varimp, hist(meanImp))
par(mfrow=c(2,2))
with(imp.0coef, hist(meanImp, xlim = range(varimp$meanImp)))
with(imp.non0coef, hist(meanImp, xlim = range(varimp$meanImp)))
with(imp.0coef.scad, hist(meanImp, xlim = range(varimp$meanImp)))
with(imp.non0coef.scad, hist(meanImp, xlim = range(varimp$meanImp)))



ranges = cbind(apply(Train.comp, 2, function(x) diff(range(x))))
coefs = merge(cbind(blasso$beta[,1]), cbind(bscad$beta[,1]), by="row.names",
	suffixes = c(".lasso",".scad")) %>%
  merge(cbind(bridge$beta[,1]), by.x = "Row.names", by.y="row.names",
	suffixes = c("",".ridge")) %>%
  merge(ranges, by.x = "Row.names", by.y = "row.names") %>%
  merge(varimp, by.x = "Row.names", by.y = "X")

with(coefs, plot(meanImp, V1.lasso))
with(coefs, points(meanImp, V1.scad, add=T, col="red"))

require(ggplot2)

ggplot(coefs, aes(meanImp)) +                    # basic graphical object
  geom_point(aes(y=log(abs(V1.lasso/V1.y))), colour="red") +  # first layer
  geom_point(aes(y=log(abs(V1.scad/V1.y))), colour="green")



