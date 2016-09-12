#load packages: caret, stringr, Hmisc (use pacman to load packages in one line)
pacman::p_load(caret,stringr,Hmisc,Boruta)

#load data
path = ("C:/Users/Riki/Dropbox/Kaggle/Paribas/")
training = read.csv(paste(path,"train.csv",sep=""),stringsAsFactors=F)
#testing = read.csv(paste(path,"test.csv",sep=""),stringsAsFactors=F)

dim(training)
#table(Train$row.miss)

#1) count missing and create variable
Train = training
Train$row.miss = rowSums(is.na(training))   #count of missing values in each row

dim(Train)
hist(Train$row.miss)

Ecdf(Train$row.miss, xlab = "# Missing Variables")  #diff b/w none and some missing, and b/w <100 and >100
row.miss.cs = data.frame(count = cumsum(table(Train$row.miss)))

Train$miss.any = as.numeric(Train$row.miss > 0)   #none vs some missing
Train$miss.100plus = as.numeric(Train$row.miss >= 100) #<100 vs >= 100 missing

dim(Train)

#table(Train$miss.any, Train$miss.100plus)
#char.vars = which(sapply(Train, class) == "character")
#table(sapply(char.vars, function(x) max(nchar(Train[,x]))))
#sum(sapply(char.vars, function(x) length(unique(Train[,x]))))


#2) remove v22
Train = subset(Train, select = -c(v22))

#3) replace missing with means for numeric variables (not median?)
num.vars = which(sapply(Train, class) == "numeric")
for(i in num.vars) Train[,i] = ifelse(is.na(Train[,i]), mean(Train[,i], na.rm=TRUE), Train[,i])

#4) split multi-character strings into variables
splitvar <- function(df){
  for(i in seq_len(ncol(df))){
    if( !class(df[,i]) %in% c("numeric","integer") ){
      x = df[i]
      colname = names(x)
      len = max(nchar(x[,1]))
      str_wide = str_pad(x[,1],side="right",width=len)
      newvar = do.call(rbind, strsplit(str_wide, ""))
	colnames(newvar) = if(len > 1) paste(colname, seq_len(len), sep=".") else colname
    } else newvar = df[i]
    if(i == 1) newdf = newvar else newdf = data.frame(newdf, newvar, stringsAsFactors=F)
  }
  return(newdf)
}

Train.split = splitvar(Train)
dim(Train.split)

#5) split character variables into dummy variables
Train.mat = model.matrix(~ . - ID - target, data=Train.split) 
Train.comp = cbind(Train.split[,1:2],Train.mat)
dim(Train.comp)

#6) (Optional) Remove variables that are "near zero" or mostly non-unique 
# uniqueCut is the threshold for percent unique, i.e. uniqueCut = 1 => percentage of unique values <= 1%
nzv.05 = nearZeroVar(Train.mat, uniqueCut = 1, saveMetrics= TRUE)  #determines which columns are "near zero"
nzv.01 = nearZeroVar(Train.mat, freqCut = 99/1, uniqueCut = 1, saveMetrics= TRUE)  #determines which columns are "near zero"

#nzv.01$freqRatio = round(nzv.01$freqRatio, 6)
#sum(nzv.01$nzv)
#nrow(nzv.01)
#nzv10 = nzv.01[1:10,]
#nzv10[,2] = round(nzv10[,2],6)
#nzv10

#subset data
vars = c("ID","target","(Intercept)",subset(row.names(nzv.01), nzv.01$nzv==F))
Train.comp = Train.comp[,vars]
dim(Train.comp)
# keep 281 vars out of 363

#write.csv(cbind(vars), paste(path, "varlist.csv", sep=""))



