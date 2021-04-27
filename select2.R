library(MASS)
library(MPV)
library(boot)
library(leaps)

read=read.table(file="allee.csv",sep=",",header=T)
xx=read[1:354,]
index=xx[,2]
date=xx[,1]
datef=as.Date(format(date),"%Y%m%d")
xname=xx[,3:366]
x=as.matrix(cbind(xname))

out=lm(index~x)
summary(out)
plot(out$fit,out$res)
#################################################################
# ---- forward search
out1=regsubsets(x,index,method='forward',nvmax=50)
out11=summary(out1)
names(out11)
#out11$outmat
#out11$rsq
#out11$adjr2
plot(out11$adjr2,main='Adj R^2 vs # of stocks',xlab="# of stocks",xlim=c(0,50),ylim=c(0.9,1),ylab="Adjusted R^2",type="l")
#out11$cp

##################################################################
# ---- backward search
out2=regsubsets(x,index,method='backward',nvmax=50)
out22=summary(out2)
#out22$outmat
#out22$rsq
#out22$adjr2
lines(out22$adjr2,type="l",col="red")

##################################################################
# ---- stepwise search
out3=regsubsets(x,index,method='seqrep',nvmax=50)
out33=summary(out3)
#out33$rsq
out33$adjr2
mat=out33$outmat
lines(out33$adjr2[c(1:42,44:46,48:50)],type="l",col="blue")
legend(30,0.95,legend=c("Forward","Backward","Stepwise"),lty=c(1,1,1),col=c("black","red","blue"))
###################################################################
######10 stocks
nn=10
i=(2:366)[out33$which[nn,]==TRUE]
i=i-1
name=names(out33$which[nn,i])
name=name[2:(1+nn)]
m=mat.or.vec(nn,1)
for(j in 1:nn)
{
m[j]=(1:364)[names(xname)==name[j]]
}

x10=as.matrix(x[,m])
out10=lm(index~x10)
summary(out10)
out10$coef

#################################################
#######plots
par(mfrow=c(1,2))
plot(index,out10$fit,cex=.5,ylab="Fitted Value",main="Fitted Value against Y")
lines(index,index)

plot(datef,out10$res/out10$fit*100,type="p",xlab="Time",ylab="Residuals/Fitted(%)")
title("Residuals Plot against Time")
hist(out10$res/out10$fit*100)

par(mfrow=c(2,1))
aa=index[1]/out10$fit[1]
plot(datef,index,type="l",col="red",xlab="Time",ylab="Index")
title("Tracking Portfolio vs Index (2000-2006)")
lines(datef,out10$fit,type="l",col="blue")
legend(datef[300],1100,legend=c("Fitted value","Index"),lty=c(1,1),col=c("red","blue"),cex=0.65)

lines(out10$fit*aa,type="l",col="blue")

###########part
plot(datef[1:50],index[1:50],type="l",col="red",xlab="Time",ylab="Index")
title("Tracking Portfolio vs Index (2000)")
lines(datef[1:50],out10$fit[1:50],type="l",col="blue")

###################################################################
#####Back-testing
xxback=read[355:604,]
indexback=xxback[,2]
dateback=xxback[,1]
datefback=as.Date(format(dateback),"%Y%m%d")
xnameback=xxback[,3:366]
#xback=as.matrix(cbind(xnameback))

m2=mat.or.vec(nn,1)
port=mat.or.vec(250,1)
for(j in 1:nn)
{
m2[j]=(1:366)[names(xnameback)==name[j]]
port=port+ xnameback[,m2[j]]*out10$coef[j+1]
}
port= port+out10$coef[1]

par(mfrow=c(1,2))
plot(datefback,indexback,type="l",col="red",xlab="Time",ylab="Index")
title("Tracking Portfolio vs Index (2007-2011)")
lines(datefback,port,type="l",col="blue")
legend(datefback[135],870,legend=c("Index","Tracking Portfolio"),lty=c(1,1),col=c("red","blue"),cex=0.8)

plot(datefback,(port-indexback)/indexback*100,type="l",xlab="Time",ylab="Deviation(%)",main="Deviation Plot")  #tracking error plot

###################################################################
######Rebalancing 
#Time Frame
dev=(port-indexback)/indexback*100

te=mat.or.vec(250,1)
for(i in 13:250)
{
	te[i]=mean(abs(dev[(i-12):i]))
}
plot(te,type="l")
#tracking error

level=5.5
t1=(1:250)[te>=level]
t1=t1[1]

#record std of rebalanced
err=mat.or.vec(250,1)
err[1:t1]=dev[1:t1]

plot(datefback,indexback,type="l",xlab="Time",ylab="Index")
title("Rebalanced Portfolio (Level=5.5%,N=250) vs Index (2007-2011)")
lines(datefback[1:t1],port[1:t1],type="l",col="blue")
##########################################################
###################Loop###################################
tot=t1
while(tot<250)
{	
	new1=select(read,tot)  ###defined below
	newbeg=tot+1
	print(datefback[newbeg])
	dev=(new1-indexback[newbeg:250])/indexback[newbeg:250]*100
	
	te_start=newbeg+12
	te=mat.or.vec(250,1)
	for(i in te_start:250)
	{
		te[i]=mean(abs(dev[(i-te_start+1):(i-te_start+13)]))
	}
	
	t2=250	
	if(max(te)>=level)
	{
		t2=(1:250)[te>=level];
		t2=t2[1];
	}
	
	err[tot+1:t2]=dev[tot+1:t2-tot]
	#####plot
	#aa=indexback[t1+1]/new1[1]
	#newport=new1*aa
	newport=new1
	newend=t2-1
	days=t2-tot-1
	c=runif(3)
	col=rgb(c[1],c[2],c[3])
	lines(datefback[newbeg:newend],newport[1:days],type="l",col=col)
	abline(v=datefback[newbeg],lty=2)
	
	tot=t2
}
print(mean(abs(err[1:250])))
##############Compare deviation plot
#original and rebalanced
#par(mfrow=c(1,2))
plot(datefback,abs((port-indexback)/indexback*100),type="l",xlab="Time",ylab="Absolute Deviation(%)",ylim=c(0,15),main="Absolute Deviation Plot") 
lines(datefback[1:250],abs(err[1:250]),type="l",xlim=c(0,250),ylim=c(0,15),lty=2,col="purple")
legend(datefback[1],13,legend=c("Original","Rebalanced"),lty=c(1,2),col=c("black","purple"))
#########################################function
select=function(read,t1){
beg=104+t1
end=354+t1
reb_x=read[beg:end,3:366]
reb_index=read[beg:end,2]
outre=regsubsets(reb_x,reb_index,method='seqrep',nvmax=50)
outreb=summary(outre)
#plot(outreb$adjr2,type="l",col="blue")

nn=10
i=(2:366)[outreb$which[nn,]==TRUE]
i=i-1
name=names(outreb$which[nn,i])
name=name[2:(1+nn)]
m=mat.or.vec(nn,1)

for(j in 1:nn)
{
m[j]=(1:366)[names(read)==name[j]]
}

xnew=as.matrix(read[beg:end,m])
outreb=lm(reb_index~xnew)

print(summary(outreb))
print(outreb$coef)

newbeg=end+1
port2=mat.or.vec(250-t1,1)
for(j in 1:10)
{
port2=port2+read[newbeg:604,m[j]]*outreb$coef[j+1]
}
port2= port2+outreb$coef[1]

return(port2)
}
#########################################################
####rebalance original
x10all=read[,m+2]
dev=(port-indexback)/indexback*100
err2=mat.or.vec(250,1)
err2[1:t1]=dev[1:t1]

plot(datefback,indexback,type="l",xlab="Time",ylab="Index")
title("Weight-Rebalanced Portfolio (Level=5.5%,N=250) vs Index (2007-2011)")
lines(datefback[1:t1],port[1:t1],type="l",col="blue")

tot=t1
while(tot<250)
{	
	beg=104+tot
	end=354+tot
	reb_x=as.matrix(read[beg:end,m+2])
	reb_index=read[beg:end,2]
	
	outreb=lm(reb_index~reb_x)
	print(summary(outreb))
	print(outreb$coef)
	
	newbeg=tot+1
	allbeg=newbeg+354
	port2=mat.or.vec(250-tot,1)
	for(j in 1:10)
	{
	port2=port2+x10all[allbeg:604,j]*outreb$coef[j+1]
	}
	port2= port2+outreb$coef[1]

	print(datefback[newbeg])
	
	dev=(port2-indexback[newbeg:250])/indexback[newbeg:250]*100
	
	te_start=newbeg+12
	te=mat.or.vec(250,1)
	for(i in te_start:250)
	{
		te[i]=mean(abs(dev[(i-te_start+1):(i-te_start+13)]))
	}
	
	t2=250	
	if(max(te)>=level)
	{
		t2=(1:250)[te>=level];
		t2=t2[1];
	}
	
	err[tot+1:t2]=dev[tot+1:t2-tot]

	newport=port2
	newend=t2-1
	days=t2-tot-1
	c=runif(3)
	col=rgb(c[1],c[2],c[3])
	lines(datefback[newbeg:newend],newport[1:days],type="l",col=col)
	abline(v=datefback[newbeg],lty=2)
	tot=t2
}
print(mean(abs(err[1:250])))
##############Compare deviation plot
#original and rebalanced
#par(mfrow=c(1,2))
plot(datefback,abs((port-indexback)/indexback*100),type="l",xlab="Time",ylab="Absolute Deviation(%)",ylim=c(0,15),main="Absolute Deviation Plot") 
lines(datefback[1:250],abs(err[1:250]),type="l",xlim=c(0,250),ylim=c(0,15),lty=2,col="purple")
legend(datefback[1],13,legend=c("Original","Rebalanced"),lty=c(1,2),col=c("black","purple"))
##############################################################
