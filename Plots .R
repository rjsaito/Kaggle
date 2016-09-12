pacman::p_load(corrgram,Amelia,tabplot, mi,reshape)

path = ("C:/Users/Xiaoye/Google Drive/16Spring/PUBH7475/Group Project/")
setwd("C:/Users/Xiaoye/Google Drive/16Spring/PUBH7475/Group Project/")
training = read.csv(paste(path,"train.csv",sep=""),stringsAsFactors=F)
train2=training
data.frame.train=data.frame(training)
str(data.frame.train)
##
for (f in names(train1)) {
  if (class(train1[[f]])=="character") {
    levels <- unique(train1[[f]])
    train1[[f]] <- as.integer(factor(train1[[f]], levels=levels))
  }
}
#############
train2$v3<-as.factor(train2$v3)
train2$v22<-as.factor(train2$v22)
train2$v30<-as.factor(train2$v30)
train2$v31<-as.factor(train2$v31)
train2$v52<-as.factor(train2$v52)
train2$v56<-as.factor(train2$v56)
train2$v91<-as.factor(train2$v91)
train2$v107<-as.factor(train2$v107)
train2$v112<-as.factor(train2$v112)
train2$v113<-as.factor(train2$v113)
train2$v125<-as.factor(train2$v125)
levels(train2$v3)[1] <-NA #to remove the "" level and replace by NA
levels(train2$v22)[1] <- NA
levels(train2$v30)[1] <- NA
levels(train2$v31)[1] <- NA
levels(train2$v52)[1] <- NA
levels(train2$v56)[1] <- NA
levels(train2$v91)[1] <- NA
levels(train2$v107)[1] <- NA
levels(train2$v112)[1] <- NA
levels(train2$v113)[1] <- NA
levels(train2$v125)[1] <- NA

dim(na.omit(train2)) #17756 observations have no missing values

train2$row.miss = rowSums(is.na(train2))   #count of missing values in each row

Ecdf(train2$row.miss)  #diff b/w none and some missing, and b/w <100 and >100
cumsum(table(train2$row.miss))

train2$miss.any = as.numeric(train2$row.miss > 0)   #none vs some missing
train2miss.100plus = as.numeric(train2$row.miss >= 100) #<100 vs >= 100 missing

dim(Train)
################
Count <- apply(train2, 2, function(x){sum(is.na(x))})
Percentage <- apply(train2, 2, function(x){sum(is.na(x))/length(x)})
missingsvariablesold <- cbind(Count, Percentage)
m<-data.frame(missingsvariablesold)
mnew<-m[order(m$Count),]
mnew$variable<-row.names(mnew)
write_csv(mnew,"Variables_Missing_Percentage_whole.csv")
##########
target0 <- train2[train2$target==0,]
target1 <- train2[train2$target==1,]
dim(na.omit(target0))
dim(na.omit(target1))
Count_0 <- apply(target0, 2, function(x){sum(is.na(x))})
Percentage_0 <- apply(target0, 2, function(x){sum(is.na(x))/length(x)})
Count_1 <- apply(target1, 2, function(x){sum(is.na(x))})
Percentage_1 <- apply(target1, 2, function(x){sum(is.na(x))/length(x)})

missingvariables_compare <- cbind(Count_0, Percentage_0, Count_1, Percentage_1)
m_compare<-data.frame(missingvariables_compare)
m_compare$variable<-rownames(m_compare)
write_csv(m_compare,"Variables_Missing_Percentage_compare.csv")


#####################
library(readr)
train=training

for (f in names(train)) {
  if (class(train[[f]])=="character") {
    levels <- unique(train[[f]])
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
  }
}



#make a table of missing values
library(mice)
missers<-md.pattern(train[,-c(1:2)])
head(missers)
write_csv(as.data.frame(missers),"NAsTables.csv")

#make plots of missing values
library(VIM)

png(filename = "missing table", type="cairo",pointsize = 12,res=300)
plot1<-aggr(train2[,-c(1:2)],col=c("blue","orange"),numbers=TRUE,combined=TRUE,varheight= TRUE, border=NA, sortVars=TRUE, sortCombs=FALSE, labels=names(train[-c(1:2)]), cex.axis=.7)
dev.off()

png(filename = "Missing Values.png", type="cairo",
    units="in",
    width=12,
    height=6.5,
    pointsize=10,
    res=300)
plot2<-aggr(train[,-c(1:2)],col=c("blue","orange"),
            numbers=TRUE,combined=TRUE,varheight= FALSE, border="black", 
            sortVars=TRUE, sortCombs=FALSE, labels=names(train[-c(1:2)]), cex.axis=.7)
dev.off()




miceplot1 <- aggr(train[, -c(1:2)], col=c("dodgerblue","orange"),
                  numbers=TRUE, combined=TRUE, varheight=FALSE, border="gray50",
                  sortVars=TRUE, sortCombs=FALSE, ylabs=c("Missing Data Pattern"),
                  labels=names(train[-c(1:2)]), cex.axis=.7)


png(filename="Missing as a whole2.png",
    type="cairo",
    units="in",
    width=12,
    height=6.5,
    pointsize=10,
    res=300)

miceplot2 <- aggr(train2[, -c(1:2)], col=c("blue","orange"),
                  numbers=TRUE, combined=TRUE, varheight=FALSE, border=NA,
                  sortVars=TRUE, sortCombs=FALSE, ylabs=c("Missing Data Pattern w/ Height Adjustment"),
                  labels=names(train2[-c(1:2)]), cex.axis=.7)
dev.off()


#######################
dim(training)
str(training)
table(training$target) #Target=1 means those claims could be accelearted
summary(training)

n<-nrow(training)
set.seed(21)
newind<-sample(seq(1,n),n*1/100)
newdata<-training[newind,]
traindata<-newdata[,-c(1:2)]

#Corr Matrix Plot
library(corrgram)
corrgram(traindata, order=F, upper.panel=NULL, lower.panel=panel.shade, cex.labels=2)
#Visualize missing values
library(Amelia)
missmap(traindata)


#Seperate data into Continous variables and categorical variables  
#categorical variables
catind <- c(paste("v", c(3, 22, 24, 30, 31, 47, 52, 56, 66, 71, 74, 75, 79, 91, 107, 110, 112, 113, 125), sep=""))
summary(training[catind])
catdata<-data.frame(training[catind])
as.factor(catdata[,2]) #18211 Levels
catdata

apply(catdata, 2, function(x){length(unique(x))} )
table(training$v91, training$v107)


#continous variables
contdata<-training[,!(names(training) %in% catind)]
summary(contdata)

hist(train[cont.var.names][[i]], breaks=100, main="Histogram", xlab=colnames(train[cont.var.names][i]))
hist(v1~target1,data=contdata)

cont.var.names <- c(paste("v", c(1, 2, 4:21, 23, 25:29, 32:46, 48:51, 53:55, 57:65, 67:70, 72, 73, 76:78, 80:90, 92:106, 108, 109, 111, 114:124, 126:131), sep=""))
cont.var.names [[3-1]][1]

#target0 <- train[which(train$target==0),]
#target1 <- train[which(train$target==1),]

i=3
for (i in 3:length(names(contdata))) {
  # par(mfrow=c(1,2))
  #hist(target0[cont.var.names][[i]], breaks=100, main="Histogram target 0", xlab=colnames(target0[cont.var.names][i]))
  #hist(target1[cont.var.names][[i]], breaks=100, main="Histogram target 1", xlab=colnames(target1[cont.var.names][i]))
  
  
}



histdata<-train[,c(2,3)]
histdata$target<-as.factor(histdata$target)
a=paste("v",1,sep="")

ggplot(histdata, aes(x=a, fill=target)) +
  geom_histogram(binwidth=.5, alpha=.5, position="dodge")
histdata1<-contdata[,c(2,)]
histdata1$target<-as.factor(histdata1$target)



ggplot(histdata1, aes(x=v1, fill=target)) +
  geom_histogram(binwidth=.5, alpha=.5, position="dodge")
names(contdata[2])








########
library(reshape)
library(ggplot2)
ggmap(traindata)
library(tabplot)

newTrain=training[,c(-1,-2)]
dim(newTrain)

set.seed(2)
n<-dim(newTrain)[1]
nind<-sample(seq(1,n),n*1/10)
tableplotdata<-newTrain[nind,]
m=ncol(tableplotdata)
i=1
for (i in 1:m) {
  n=i+9
  if ( n < m)
  {
    
       tableplot(tableplotdata[,i:n])
    
  }
  i=n
  
}


# tr=data.table(Train)
# tr[,ID :=NULL]
# tr[, target := NULL]
# tr[1:6,]


###
# library(VIM)
# 
# jpeg(filename="MissingDataPattern.jpg",
#     type="cairo",
#     units="in",
#     width=12,
#     height=6.5,
#     pointsize=10,
#     res=300)
# ggmap(traindata)
# miceplot1 <- aggr(training[, -c(1:2)], col=c("dodgerblue","dimgray"),
#                   numbers=TRUE, combined=TRUE, varheight=FALSE, border="gray50",
#                   sortVars=TRUE, sortCombs=FALSE, ylabs=c("Missing Data Pattern"),
#                   labels=names(train[-c(1:2)]), cex.axis=.7)
# dev.off()
# 
# 


library(corrplot)
corrplot(as.matrix(cormatrix),order="hclust", addrect=5, diag=F)
