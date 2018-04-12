mr <- read.table(file.path("agaricus-lepiota.data"),sep=",",header=FALSE)
mr.df<- as.data.frame(mr)

names(mr)<-c("class", "cshape", "csurface", "ccolor", "bruises", "odor", "gattach", "gspace", "gsize", "gcolor", "sshape", "sroot", "ssabove", "ssbelow", "scabove", "scbelow", "vtype", "vcolor", "rnumber", "rtype", "spcolor", "popnum", "habitat")
names(mr)

pairs(class ~ cshape + csurface + ccolor, data=mr)
plot(class ~ sshape + sroot, data=mr)

train.df <- sample(nrow(mr), 0.7*nrow(mr))
mrtrain.df <- mr[train.df,]
mrtest.df <- mr[-train.df,]
table(mrtrain.df$class)
table(mrtest.df$class)

mrf<-as.data.frame(mr, stringAsFactors=FALSE
mrf[,c(1,23)]<-sapply(mrf[,c(1,23)],as.character)
mrf$class[mrf$class=='e'] <- 0
mrf

install.packages("e1071")

library("e1071")

library(lattice)

subdata <- mr[mr$class != 'p',]
subdata$class <- factor(subdata$class)
mr.svm <- svm(class ~ cshape + sroot , kernel = "linear", data = subdata)

mr.svm <- svm(class ~ cshape + sroot , data = mr)
mr.svm <- svm(class ~ cshape + sroot , kernel = "linear" , data = mr)

subdata <- data.frame(mr$class, mr$cshape, mr$sroot)
subdata.svm <- svm(mr.class ~ mr.cshape + mr.sroot , kernel = "linear", data = subdata)

subdata <- data.frame(mr$class, mr$cshape, mr$sroot)

cshape.index <- c("b", "c", "x", "f", "k", "s")
cshape.values <- c(1, 3, 5, 7, 9, 11)
subdata$cshape.values <- cshape.values[match(subdata$mr.cshape, cshape.index)]

sroot.index <- c("b", "c", "u", "e", "z", "r", "?")
sroot.values <- c(1, 3, 5, 7, 9, 11, 0)
subdata$sroot.values <- sroot.values[match(subdata$mr.sroot, sroot.index)]

subdata.svm <- svm(mr.class ~ cshape.values + sroot.values, kernel = "linear", data = subdata)
plot(subdata.svm, subdata, cshape.values ~ sroot.values)
summary(subdata.svm)

compareTable <- table (subdata$mr.class, predict(subdata.svm))
mean(subdata$mr.class != predict(subdata.svm))
mean(subdata$mr.class == predict(subdata.svm))

acq_dataset
for(i in acq)
{
	acq_dataset<-i
}

dataset <- acontent(acq)


for(i in dataset)
{
	acq_dataset<-i
}

reuters_td <- tidy(acq)
reuters_td


for(i in acq)
{
	d = data.frame(text=unlist(sapply(acq[n], `[`, "content")), stringsAsFactors=F)
}



data("crude")
myTdm <- as.matrix(TermDocumentMatrix(crude))
FreqMat <- data.frame(ST = rownames(myTdm), Freq = rowSums(myTdm), row.names = NULL)


revs <- tm_map(acq, content_transformer(tolower))
revs <- tm_map(revs, removeWords, stopwords("english"))
revs <- tm_map(revs, removePunctuation)
revs <- tm_map(revs, removeNumbers)
revs <- tm_map(revs, stripWhitespace)

myTdm <- as.matrix(TermDocumentMatrix(revs))
FreqMat <- data.frame(ST = rownames(myTdm), Freq = rowSums(myTdm), row.names = NULL)

plot(FreqMat, log="x")


FreqMat <- spc(Vm=FreqMat$Freq, m=FreqMat$ST)






