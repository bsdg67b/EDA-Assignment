

rm(list=ls())

x=c(17,16,20,24,22,15,21,18)
#1-1
x[x>20]
#1-2
y=rep(0,8)
for (i in 1:length(x))
{
  if(x[i]>20)
  {
    y[i]=100
  }else{
    y[i]=x[i]
  }
}

#2-1
x=diag(4:8)-1
#2-2
y=x[,-5]
#2-3
#2-4
y1=matrix(0,nrow(y),ncol(y))
for(i in 1:nrow(y))
{
  for(j in 1:ncol(y))
  {
    if (y[i,j]==-1)
    {
      y1[i,j]=0
    }else{
      y1[i,j]=y[i,j]
    }
  }
}

rm(list=ls())

#3-1
rdata=read.table("C:/tmp/rowdata.txt", header=TRUE, sep=" ", stringsAsFactors = FALSE)
#3-2
is.na(rdata)
#3-3
notna=c(0)
for(i in 1:nrow(rdata))
{
  if(is.na(rdata[i,2])==FALSE&is.na(rdata[i,3])==FALSE)
  {
    notna[i]=i
  }else{
    notna[i]=c(0)
  }
}

#3-3간단
notna=c(1:nrow(rdata))
a=which(is.na(rdata[,2]))
b=which(is.na(rdata[,3]))
union(a,b)
notna=notna[-union(a,b)]
#3-4간단
rdata[notna,]




#3-4
for(i in 1:nrow(rdata))
{
 if(is.na(rdata[i,2]))
 {
   rdata=rdata[-i,]
 }
}  
for(j in 1:nrow(rdata))
{
  if(is.na(rdata[j,3]))
  {
    rdata=rdata[-j,]
  }
}
rdata1=rdata


rdata = read.table("C:/Users/uos/Downloads/CancerRates.dat", header=F, sep=" ", stringsAsFactors=FALSE)
rdata

#4-1
temp=list(c(TRUE, FALSE), diag(1,2), seq(0,1,length=100), 1, 2, 3, 4)
#4-2
temp1=temp[-2]
#4-3
temp1[3]
#4-4
length(temp1)

#5-1
a1=-1:2
a2=1:2
a1+a2

#5-2
a1=-(1:2)
a2=1:2
a1+a2

#5-3
a1=matrix(0,2,2)
a2=c(3,4)
a1+a2

#5-4
a1=matrix(1:4,2,2)
a1[a1>2]=0

#5-5
a1=1:5
a1[-1]-a1[length(a1)]


#다시1
a=c(1, 3, rep(0,100))
for(i in 1:length(a))
{
  a[i+2]=0.9*a[i+1]-0.1*a[i]+1
}
a[20]

#다시2
for(j in 1:length(a))
{
  if(a[j]>4) break
} 
j

#다시3
A <- matrix( runif(100), 50, 5)
v=rep(0,nrow(A))
for(i in 1:nrow(A))
{
  v[i]=sum(A[i,])
}

#다시4
tmp = rep(0, 10)
a <- 10:1
idx = 1
for ( j in a)
{
  if (j<5)
  {
    tmp[idx] <- a[j]
    idx <- idx + 1
  }
}
rm(list=ls())
#다시5
x=matrix(0,1000,5)
for(i in 1:nrow(x))
{
  x[i,]=sample(1:10,5)
}
sid=as.vector(x)
#다시6
m.mat=matrix(0,10,5)
for(j in 1:10)
{
  m.mat[j,]=mean(which(sid==j))
}
#다시또6
idist=matrix(0,1000,10)
for(i in 1:1000)
{
  for(j in 1:10)
  {
    idist[i,j]=sum(x[i,]*m.mat[j,])/(sum(x[i,]*x[i,])*sum(m.mat[j,]*m.mat[j,]))^(1/2)
  }
}
idist
#다시7
ivec=rep(0,1000)
for(i in 1:length(ivec))
{
  ivec[i]=which(match(idist[i,],min(idist[i,]))==1)
}
ivec
#다시8-1
set.seed(1)
a = list()
for (i in 1:1000)
{ 
  x= rpois(1,4)+2
  x = min(x,10)
  a[[i]] = sample(1:10, x)
}
k=rep(0,10)
for(i in 2:10)
{
  for(j in 1:1000)
  {
    if(i==length(a[[j]]))
    {
      k[i]=k[i]+1
    }
  }
}
k

#다시8-2
score=rep(0,10)
for(t in 1:1000)
{
  if(length(a[[t]])>=2&length(a[[t]])<=3)
  {
    score[a[[t]][1]]=score[a[[t]][1]]+1
  }else if(length(a[[t]])>=4&length(a[[t]])<=6)
  {
    score[a[[t]][1]]=score[a[[t]][1]]+2
    score[a[[t]][2]]=score[a[[t]][2]]+1
  }else{
    score[a[[t]][1]]=score[a[[t]][1]]+3
    score[a[[t]][2]]=score[a[[t]][2]]+2
    score[a[[t]][3]]=score[a[[t]][3]]+1
  }
}
score


rm(list=ls())
#다시9
set.seed(1)
m1 = 10
m2 = 5
num = 0
#다시9-1
for(i in 1:4)
{
  a=rbinom(1, 1,1/2)
  if(a==1)
  {
    m2=m2-1
    m1=m1+1
  }else{
    m1=m1-1
    m2=m2+1
  }
}
m1
#다시9-2
while(TRUE)
{
  a=rbinom(1, 1,1/2)
  if(a==1)
  {
    m2=m2-1
    m1=m1+1
  }else{
    m1=m1-1
    m2=m2+1
  }
  num=num+1
  if(m1==0|m2==0) break
}
#다시9-3
awin=rep(0,200)
for(k in 1:200)
{
  set.seed(k)
  m1=10
  m2=5
  num=0
  while(TRUE)
  {
    a=rbinom(1, 1,1/2)
    if(a==1)
    {
      m2=m2-1
      m1=m1+1
    }else{
      m1=m1-1
      m2=m2+1
    }
    num=num+1
    if(m1==0|m2==0) break 
  }
  if(m2==0)
  {
    awin[k]=1
  }
}
sum(awin)

#다시10
#m2=10
awin=rep(0,200)
for(k in 1:200)
{
  set.seed(k)
  m1=10
  m2=10
  num=0
  while(TRUE)
  {
    a=rbinom(1, 1,1/2)
    if(a==1)
    {
      m2=m2-1
      m1=m1+1
    }else{
      m1=m1-1
      m2=m2+1
    }
    num=num+1
    if(m1==0|m2==0) break 
  }
  if(m2==0)
  {
    awin[k]=1
  }
}
sum(awin)
#m2=15
awin=rep(0,200)
for(k in 1:200)
{
  set.seed(k)
  m1=10
  m2=15
  num=0
  while(TRUE)
  {
    a=rbinom(1, 1,1/2)
    if(a==1)
    {
      m2=m2-1
      m1=m1+1
    }else{
      m1=m1-1
      m2=m2+1
    }
    num=num+1
    if(m1==0|m2==0) break 
  }
  if(m2==0)
  {
    awin[k]=1
  }
}
sum(awin)
#m2=20
awin=rep(0,200)
for(k in 1:200)
{
  set.seed(k)
  m1=10
  m2=20
  num=0
  while(TRUE)
  {
    a=rbinom(1, 1,1/2)
    if(a==1)
    {
      m2=m2-1
      m1=m1+1
    }else{
      m1=m1-1
      m2=m2+1
    }
    num=num+1
    if(m1==0|m2==0) break 
  }
  if(m2==0)
  {
    awin[k]=1
  }
}
sum(awin)
#m2=25
awin=rep(0,200)
for(k in 1:200)
{
  set.seed(k)
  m1=10
  m2=25
  num=0
  while(TRUE)
  {
    a=rbinom(1, 1,1/2)
    if(a==1)
    {
      m2=m2-1
      m1=m1+1
    }else{
      m1=m1-1
      m2=m2+1
    }
    num=num+1
    if(m1==0|m2==0) break 
  }
  if(m2==0)
  {
    awin[k]=1
  }
}
sum(awin)