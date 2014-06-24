setwd("/Users/Niladri/Documents/Research/Large p, small n/large-p-paper/revision/data")

library(ggplot2)
library(plyr)
library(reshape)

###=================================================================================
# Sum of absolute difference of mean for two groups with p dimensions for noise data
###=================================================================================

sum_p<-function(p){
val<-ldply(1:10000,function(k){
x1<-matrix(rnorm(15*p),ncol=p)
x2<-matrix(rnorm(15*p),ncol=p)
data.frame(ab.val=sum(abs(apply(x1,2,mean)-apply(x2,2,mean))))	
})
return(val$ab.val)
}

##Plotting the data
#sum_p(1)
ggplot() + geom_density(aes(x=sum_p(1)),binwidth=0.1) + geom_density(aes(x=sum_p(2)),binwidth=0.1)  + geom_density(aes(x=sum_p(3)),binwidth=0.1)  + geom_density(aes(x=sum_p(4)),binwidth=0.1)  + geom_density(aes(x=sum_p(5)),binwidth=0.1)

##Calculating the mean and the standard deviation

ldply(1:10,function(p){
	data.frame(p=p,m.sum=mean(sum_p(p)), sd.sum=sd(sum_p(p)))
	})
	
##Calculating the sum for p=1:25

sum_30<-ldply(1:35,function(p){
	data.frame(p=p,sum=sum_p(p))
})	

qplot(p,sum,data=sum_30,geom="boxplot",group=p,alpha=I(0.1),fill=I("red"),col=I("red")) + coord_flip() + geom_hline(y=6,col="blue",alpha=0.4) + scale_x_continuous("dimension") + scale_y_continuous("sum of absolute difference of means")
	
###=================================================================================
# Product of absolute difference of mean for two groups for 1 dimension by p for noise data
###=================================================================================	

prod_p<-function(p){
val<-ldply(1:1000,function(k){
x1<-matrix(rnorm(15*1),ncol=1)
x2<-matrix(rnorm(15*1),ncol=1)
data.frame(ab.val=p*abs(mean(x1)-mean(x2)))	
})
return(val$ab.val)
}

#prod_p(1)
##Plotting the data

ggplot() + geom_density(aes(x=prod_p(1)),binwidth=0.1) + geom_density(aes(x=prod_p(2)),binwidth=0.1)  + geom_density(aes(x=prod_p(3)),binwidth=0.1)  + geom_density(aes(x=prod_p(4)),binwidth=0.1)  + geom_density(aes(x=prod_p(5)),binwidth=0.1)

##Calculating the mean and the standard deviation

ldply(1:10,function(p){
	data.frame(p=p,m.prod=mean(prod_p(p)), sd.prod=sd(prod_p(p)))
	})
	
###==================================================================
##Real Separation
###==================================================================

sum_p_real<-function(p){
val<-ldply(1:10000,function(k){
x1<-matrix(rnorm(15*p),ncol=p)
x1[,p] <- x1[,p] - 3
x2<-matrix(rnorm(15*p),ncol=p)
x2[,p] <- x2[,p] + 3
data.frame(ab.val=sum(abs(apply(x1,2,mean)-apply(x2,2,mean))))	
})
return(val$ab.val)
}	
#sum_p_real(10)	

###
dat<-NULL
dimen<-c(20,40,60,80,100)
for(i in dimen){
	d<-data.frame(sum_p_real(i),sum_p(i),dimen=i)
	cat(i,"\n")
	dat<-rbind(dat,d)
}

colnames(dat)<-c("real","noise","dimen")
d.m<-melt(dat,id="dimen")

levels(d.m$variable) <- c("Yes", "No")
d.m$dimen <- as.factor(d.m$dimen)
levels(d.m$dimen) <- c("p = 20", "p = 40", "p = 60", "p = 80", "p = 100")

dim_m<-ddply(subset(d.m, variable == "Yes"),.(dimen),summarise,q=quantile(value,0.05))


dens <- ddply(d.m, .(dimen, variable), summarize, x = density(value)$x, y = density(value)$y)

dens.q <- merge(subset(dens, variable == "No"), dim_m, by = "dimen")

dens.s <- subset(dens.q, x > q)

#d.m <- read.csv("diff-real-noise.csv")

qplot(value,data=d.m,geom="density",group=variable,fill=variable,col=variable,alpha=I(0.2), facets=dimen~.) + scale_colour_brewer("Separation", type = "qual", palette = "Dark2") + scale_fill_brewer("Separation", type = "qual", palette = "Dark2") + geom_vline(data=dim_m,aes(xintercept=q),facets=dimen~.,alpha=I(0.9),col="#7570B3") + scale_x_continuous("Sum of Absolute Difference of Means",limits=c(0,45)) + geom_ribbon(data = dens.s, aes(x=x, ymin = 0, ymax = y), col = I("#7570B3"), fill=I("#7570B3"), alpha = I(0.8))

ggsave("sum-noise-real-2-rev.pdf", height = 7, width = 7)

write.csv(d.m, "diff-real-noise.csv", row.names = FALSE)

ddply(d.m,.(dimen, variable), summarize, me = mean(value), var = var(value))


#ggsave("sum-noise-real-1.pdf", height = 7, width = 7)

###====================================================================================

###Bootstrap Confidence Interval

boot.strap <- function(data){
	dat <- ldply(1:10000, function(k){ sample(data, length(data), replace = TRUE)})
	p <- sum(data == "TRUE")/length(data)
	prop.correct <- apply(dat, 1, function(x){ sum(x == "TRUE")/length(x)}) 
	return(c(lb = as.numeric(quantile(prop.correct, 0.025)), p = p, ub = as.numeric(quantile(prop.correct, 0.975))))
}

data <- rep(c("TRUE", "FALSE"), c(30, 50))

ptm <- proc.time()
boot.strap(data)
proc.time() - ptm


###===================================================================================

prop_p<-function(k){
dimen<-c(20,40,60,80,100)
dat1<-ldply(1:length(dimen), function(p){
	data.frame(p=dimen[p],prop=sum(sum_p(dimen[p])>quantile(sum_p_real(dimen[p]),k))/1000)
})
return(dat1)
}


###=======================================================================================
#Function to calculate the dimension for which the noise distribution is larger than the real 
#by particular percentage
###=======================================================================================

cal_p<-function(prop){
	p = 10
	pr<-0
	while(pr <=  prop){
	pr<-sum(sum_p(p)>quantile(sum_p_real(p),0.05))/1000
	p <- p + 1
	}
	return(p)	
}


Dat_prop<-NULL
for(i in 1:100){
prop<-c(0.0000001,0.01,0.05,0.1,0.2)
#prop<-c(0.0000001,0.01)
dat_prop<-ldply(1:length(prop),function(k){
	data.frame(proportion=prop[k],dimension=cal_p(prop[k]))
})
Dat_prop<-rbind(Dat_prop,dat_prop)
cat(i,"\n")
}

dat <- read.csv(file.choose())

qplot(dimension, data = dat, geom = "histogram", binwidth = 2, facets=proportion ~ .)

ddply(dat, .(proportion), summarise, Min = min(dimension), Q1 = quantile(dimension, 0.25), Median = quantile(dimension, 0.5), Q3 = quantile(dimension, 0.75), Max = max(dimension))

#write.csv(Dat_prop,"proportion-data.csv")

#qplot(factor(dimension),data=Dat_prop)
#quantile(Dat_prop$dimension,c(0.05,0.95))

###=========================================================================================

dat2<-ldply(1:100,function(k){
	data.frame(prop_p(0.05))
})

qplot(factor(p),prop, data=dat2, geom=c("jitter","boxplot"), alpha=I(0.6))