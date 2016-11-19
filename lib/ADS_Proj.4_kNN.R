#source("http://bioconductor.org/biocLite.R")
#biocLite("rhdf5")
library(rhdf5)

wd.path="/Users/caikezhi/Desktop/Project4_data/data"
num=49
Make_feature=function()
{
  arr=array(dim=num)
  arr[1]=mean(my.data$bars_confidence)
  arr[2]=median(my.data$bars_confidence)
  arr[3]=var(my.data$bars_confidence)
  arr[4]=mean(my.data$beats_confidence)
  arr[5]=median(my.data$beats_confidence)
  arr[6]=var(my.data$beats_confidence)
  arr[7]=mean(my.data$segments_confidence)
  arr[8]=median(my.data$segments_confidence)
  arr[9]=var(my.data$segments_confidence)
  arr[10]=mean(my.data$segments_loudness_max)
  arr[11]=median(my.data$segments_loudness_max)
  arr[12]=var(my.data$segments_loudness_max)
  arr[13]=mean(my.data$segments_pitches)
  arr[14]=median(my.data$segments_pitches)
  if(length(my.data$bars_confidence)==0) arr[15]=10 else
  arr[15]=length(my.data$beats_confidence)/length(my.data$bars_confidence)
  arr[16]=my.data$segments_start[length(my.data$segments_start)]
  arr[17]=length(my.data$segments_pitches)
  cai=my.data$segments_loudness_max-my.data$segments_loudness_start # difference in loudness from start to peak
  arr[18]=mean(cai)
  arr[19]=median(cai)
  arr[20]=var(cai)
  arr[21]=mean(my.data$sections_confidence)
  arr[22]=median(my.data$sections_confidence)
  arr[23]=var(my.data$sections_confidence)
  arr[24]=mean(my.data$segments_timbre)
  arr[25]=median(my.data$segments_timbre)
  arr[26:37]=rowMeans(my.data$segments_timbre)
  arr[38:49]=rowMeans(my.data$segments_pitches)
  arr
}

num.songs=dim(lyr)[1]
dd=matrix(nrow=2350,ncol=num)
song.names=array(dim=2350)
n=1
a=Sys.time()

for(i in 1:2)
  for(j in 1:26)
  {
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
for(i in 1:num) dd[,i]=(dd[,i]-min(dd[,i],na.rm=TRUE))/diff(range(dd[,i],na.rm=TRUE))

#####################################################################
#setwd("/Users/caikezhi/Desktop")
#lyr=load("/Users/caikezhi/Desktop/lyr.RData")
lyr=lyr[-c(1,2,3,6:30)]
set.seed(1)
sample.songs=sample(1:2350,235)
track.back=c(1:2350)
track.back=track.back[-sample.songs]
dic=lyr[-sample.songs,]

Make_similarity_matrix=function(dic)
{
  num.train=dim(dic)[1]
  word.ranks=matrix(nrow=num.songs,ncol=100)
  for (i in 1:num.train)
  {
    word.ranks[i,]=order(dic[i,2:dim(dic)[2]],decreasing = TRUE)[1:100]
    print(i)
  }
  simi=matrix(0,nrow=num.train,ncol=num.train)
  for(i in 1:(num.train-1))
    for( j in (i+1):num.train)
    simi[i,j]=length(intersect(word.ranks[i,],word.ranks[j,]))/100
return(simi)
}
simi=Make_similarity_matrix(dic)
###################################################################
threshold=0.6
pairs=which(simi>threshold,arr.ind=T)
diff=matrix(nrow=num,ncol=dim(pairs)[1])
for(i in 1:dim(pairs)[1])
{
  diff[,i]=abs(as.numeric(dd[track.back[pairs[i,1]],])-as.numeric(dd[track.back[pairs[i,2]],]))
  print(i)
}
vari=array(dim=num)
for(i in 1:num) 
{
  vari[i]=var(diff[i,],na.rm=TRUE)
}  
###################################################################
num.features=15
num.neighbors=1
t1=Sys.time()
cai=matrix(0,nrow=15,ncol=20)

Calc_distance=function(x)
{
  temp=0
  for(i in 1:num.features)
  if(!(is.na(a[i]))&!(is.na(x[i])))
    {
    temp=temp+(x[i]-a[i])*(x[i]-a[i])
  }  
  sqrt(temp)
}

features=dd[,order(vari)[1:num.features]]

result=matrix(0,nrow=length(sample.songs),ncol=100)
for (i in 1:length(sample.songs))
{
  print(i)
  a=features[sample.songs[i],]
  distances=apply(features[-sample.songs,],MARGIN=1,Calc_distance)
  neighbors=track.back[order(distances)[1:num.neighbors]]
  recommendation=array(0,dim=dim(dic)[2])
  recommendation=colSums(lyr[neighbors,])
  recommendation=order(recommendation,decreasing=TRUE)[1:100]
  result[i,]=colnames(lyr)[recommendation]
}

score=array(0,dim=length(sample.songs))
compare.to.random=score
random=array(dim=100)
for (i in 1:length(sample.songs))
{
  print(i)
  da.an=colnames(lyr)[order(lyr[sample.songs[i],],decreasing=TRUE)[1:100]]
  score[i]=length(intersect(result[i,],da.an))
  random=order(lyr[round(runif(1,2,2350)),],decreasing=TRUE)[1:100]
  compare.to.random[i]=length(intersect(colnames(lyr)[random],da.an))
}
t2=Sys.time()
print(t2-t1)

hist(score,col="skyblue",main="Results from Model",xlab="Score",ylab="Frequency",labels=TRUE)
hist(compare.to.random,col="pink",main="Results from Random Guess",xlab="Score",ylab="Frequency",labels=TRUE)
