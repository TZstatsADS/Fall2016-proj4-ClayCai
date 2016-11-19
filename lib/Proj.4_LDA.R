library(topicmodels)
library(randomForest)
library(rhdf5)

wd.path="/Users/caikezhi/Desktop/Project4_data/data"
num=191
Make_feature=function()
{
  arr=array(dim=num)
  arr[1]=mean(my.data$bars_confidence)
  arr[2:6]=quantile(my.data$bars_confidence)
  arr[7]=var(my.data$bars_confidence)
  arr[8]=mean(my.data$beats_confidence)
  arr[9:13]=quantile(my.data$beats_confidence)
  arr[14]=var(my.data$beats_confidence)
  arr[15]=mean(my.data$segments_confidence)
  arr[16:20]=quantile(my.data$segments_confidence)
  arr[21]=var(my.data$segments_confidence)
  arr[22]=mean(my.data$segments_loudness_max)
  arr[23:27]=quantile(my.data$segments_loudness_max)
  arr[28]=var(my.data$segments_loudness_max)
  arr[29]=mean(my.data$segments_pitches)
  for(i in 1:12) arr[(30+i*5-5):(30+i*5-1)]=quantile(my.data$segments_pitches[i,])
  if(length(my.data$bars_confidence)==0) arr[90]=10 else
  arr[90]=length(my.data$beats_confidence)/length(my.data$bars_confidence)
  arr[91]=my.data$segments_start[length(my.data$segments_start)]
  arr[92]=length(my.data$segments_pitches)
  cai=my.data$segments_loudness_max-my.data$segments_loudness_start # difference in loudness from start to peak
  arr[93]=mean(cai)
  arr[94:98]=quantile(cai)
  arr[99]=var(cai)
  arr[100]=mean(my.data$sections_confidence)
  arr[101]=median(my.data$sections_confidence)
  arr[102]=var(my.data$sections_confidence)
  arr[103]=mean(my.data$segments_timbre)
  arr[104]=median(my.data$segments_timbre)
  arr[105:116]=rowMeans(my.data$segments_timbre)
  arr[116:127]=rowMeans(my.data$segments_pitches)
  for(i in 1:12) arr[(128+i*5-5):(128+i*5-1)]=quantile(my.data$segments_timbre[i,])
  arr[188]=length(my.data$bars_confidence)
  arr[189]=length(my.data$beats_confidence)
  arr[190]=length(my.data$sections_confidence)
  arr[191]=length(my.data$segments_confidence)
  arr
}

load("/Users/caikezhi/Desktop/lyr.RData")
dic=colnames(lyr)
lyr=lyr[-c(1,2,3,6:30)]
num.songs=dim(lyr)[1]
dd=matrix(nrow=2350,ncol=num)
song.names=array(dim=2350)
n=1
a=Sys.time()

for(i in 1:2)
  for(j in 1:26)
  {
    print(j)
    if (!((i==2)&(j>9)))
    {
      if((i==2)&(j==9)) kk=10 else kk=26
      for(k in 1:kk)
      {
        l1=LETTERS[i]
        l2=LETTERS[j]
        l3=LETTERS[k]
        wd=sprintf("%s/%s/%s/%s",wd.path,l1,l2,l3)
        setwd(wd)
        filenames=list.files()
        if (length(filenames)>0){
          for(l in 1:length(filenames))
          {
            filename=filenames[l]
            my.data=h5read(filename,"analysis")
            cai=Make_feature()
            dd[n,]=cai
            song.names[n]=filename
            n=n+1
          }
        }
      }
    }
  }  
b=Sys.time()
print(b-a)
dd.backup=dd
#for(i in 1:num) dd[,i]=(dd[,i]-min(dd[,i],na.rm=TRUE))/diff(range(dd[,i],na.rm=TRUE))

set.seed(1)
lda.result=LDA(lyr,15)

feature=cbind(topics(lda.result),dd)
feature.backup=feature
feature=as.data.frame(feature)
feature=na.roughfix(feature)
a=Sys.time()
rf.result=randomForest(V1~.,data=feature,n.trees=10000)
b=Sys.time()
print(b-a)
################################
setwd("/Users/caikezhi/Desktop/TestSongFile100")
test.filename=list.files(path="/Users/caikezhi/Desktop/TestSongFile100",pattern=".h5")
test.feature=matrix(nrow=100,ncol=num)
for (i in 1:length(test.filename))
{
  filename=test.filename[i]
  my.data=h5read(filename,"analysis")
  cai=Make_feature()
  test.feature[i,]=cai
}
test.feature=cbind(array(0,dim=100),test.feature)
result=randomForest:::predict.randomForest(rf.result,test.feature,type="response")
result=round(result)

aa=lda.result
bb=t(terms(lda.result,5000))
cc=matrix(nrow=15,ncol=4973)
for(i in 1:15)
  for(j in 1:4973)
  {
    cc[i,which(colnames(lyr)==bb[i,j])]=j
  }

output=matrix(nrow=100,ncol=4973)

for(i in 1:100) output[i,]=cc[result[i],]
output.backup=output
blank=matrix(0,nrow=100,ncol=25)
output=cbind(0,0,output[,1:2],blank,output[,3:4973])
colnames(output)=dic[2:length(dic)]
write.csv(output,"result.csv")
#############################################

