setwd("/Users/Niladri/Documents/Research/Large p, small n/large-p-paper/submission")

library(tourr)
library(ggplot2)

generate_plot_1d<-function(n=30,p, noise=0, m=20){
	x<-matrix(rnorm(p*n),ncol=p)
	if(noise==0){
x[1:15,p]<-x[1:15,p]-3
x[16:30,p]<-x[16:30,p]+3
}
colnames(x)<-paste("X",1:(p),sep="")
x<-scale(x)
x<-data.frame(x, cl=factor(c(rep(1,n/2),rep(2,n/2))))
d=1

optima <- save_history(x[,-(p+1)], guided_tour(index_f=pda_pp(cl=x[,(p+1)], lambda=0.2), d=1, max.tries=500), max_bases=1000, rescale=F)
nbases<-dim(optima)[3]

optima.global<-unclass(optima)[,,nbases]
projdata<-data.frame(x=as.matrix(x[,-(p+1)])%*%optima.global, cl=x[,(p+1)],nbases=rep(nbases,n))
projdata.true<-projdata[with(projdata, order(cl)), ] 
if(mean(projdata.true$x[1:n/2])>0)
projdata.true$cl<-c(rep(2,n/2),rep(1,n/2))
return(projdata.true)
#ggsave("1d_example3.png",width=7, height=2.3, dpi=75)
}

dat <- generate_plot_1d(30, 20, noise = 0)

y<-rep(1.5,n = 30)

dat1 <- data.frame(y, dat)
#write.table(lineup.data,"1d_data_example3.txt")
qplot(x ,y, data = dat1, colour=factor(cl),geom="jitter", size = I(3)) + scale_colour_discrete(name="Group") + scale_y_continuous("",breaks=c(0,1.5,3),limits=c(0,3))  + scale_x_continuous("PD1", breaks=c(-1.5,0.0,1.5))

#write.table(dat1, "/Users/Niladri/Documents/Research/Large p, small n/large-p-paper/submission/data/test-statistics-1.txt", row.names = F )

dat1.test <- read.table("test-statistics-1.txt", header = T) 

qplot(x ,y, data = dat1.test, colour=factor(cl),geom="jitter", size = I(3)) + scale_y_continuous("",breaks=c(0,1.5,3),limits=c(0,3))  + scale_x_continuous("PD1", breaks=c(-1.5,0.0,1.5)) + scale_colour_brewer(name = "Group", type = "qual", palette = "Dark2")


ggsave("test_statistic_1-rev.pdf",width=4.17, height=3.17, dpi=75)

generate_plot_2d<-function(n=30,p, noise=1, m=20){
	x<-matrix(rnorm(p*n),ncol=p)
	if(noise==0){
x[1:10,(p-1)]<-x[1:10,(p-1)]+3
x[11:20,(p-1)]<-x[11:20,(p-1)]-3
x[21:30,p]<-x[21:30,p]+sqrt(27)
}
colnames(x)<-paste("X",1:(p),sep="")
x<-scale(x)
x<-data.frame(x, cl=factor(c(rep(1,n/3),rep(2,n/3),rep(3,n/3))))
d=2

optima <- save_history(x[,-(p+1)], tour_path=guided_tour(index_f=pda_pp(cl=x[,(p+1)], lambda=0.2), max.tries=500), max_bases=1000, rescale=F)
nbases<-dim(optima)[3]
optima.global<-unclass(optima)[,,nbases]

projdata.true<-data.frame(as.matrix(x[,-(p+1)])%*%optima.global, cl=x[,(p+1)], nbases=rep(nbases,n))

return(projdata.true)
}

projdata.true <- generate_plot_2d(n = 30, p = 100, noise = 1, m = 20)

qplot(X1, X2, data=projdata.true, colour=cl, size = I(3)) + scale_colour_discrete(name="Group") + scale_y_continuous("PD2") + scale_x_continuous("PD1")

#write.table(projdata.true, "/Users/Niladri/Documents/Research/Large p, small n/large-p-paper/submission/data/test-statistics-2.txt", row.names = F )

dat2.test <- read.table("test-statistics-2.txt", header = T) 

qplot(X1 ,X2, data = dat2.test, colour=factor(cl), size = I(3)) + scale_y_continuous("PD2") + scale_x_continuous("PD1") + scale_colour_brewer(name = "Group", type = "qual", palette = "Dark2")

ggsave("test_statistic_2-rev.pdf",width=4.17, height=3.17, dpi=75)

library(MASS)

generate_plot_lda<-function(n, p){
#random <- read.csv("Dataset1.csv", header=T)
	x<-matrix(rnorm(p*n),ncol=p)
	colnames(x)<-paste("X",1:(p),sep="")
   x<-scale(x)
   cl=c(rep(1,n/2),rep(2,n/2))
   d=1
   
x.lda <- lda(x,cl)
x.ld <- predict(x.lda,dimen=1)$x
true<-data.frame(x.ld, cl)
if(mean(true$LD1[1:n/2])>0)
true$cl<-c(rep(2,n/2),rep(1,n/2))
return(true)
}

true <- generate_plot_lda(n = 30, p = 28)


true <- data.frame(true, y = rep(1.5,30))

qplot(LD1,y, data = true, colour=factor(cl),geom="jitter", size = I(3)) + theme(legend.position="none") + scale_y_continuous("",breaks=c(0,1.5,3),limits=c(0,3)) + scale_x_continuous("LD1")

ggsave("noise-28.pdf", height = 3.17, width = 3.17)

write.table(true, "dat-noise-28.txt", row.names = F)

x <- matrix(rnorm(p*n), ncol = p)
colnames(x) <- paste("X", 1:p, sep = "")
x <- scale(x)
x <- data.frame(x, cl = factor(c(rep(1,n/2), rep(2, n/2))))


numnoise <- 98
x<-matrix(rnorm(30*(numnoise+2)),ncol=(numnoise+2))
#x[1:10,(numnoise+1)]<-x[1:10,(numnoise+1)]+3
#x[11:20,(numnoise+1)]<-x[11:20,(numnoise+1)]-3
#x[21:30,(numnoise+2)]<-x[21:30,(numnoise+2)]+sqrt(27)
colnames(x)<-paste("X",1:(numnoise+2),sep="")
x<-scale(x)
x<-data.frame(x, cl=factor(c(rep(1,15),rep(2,15))))

noise.lda<-lda(x[,1:100],x[,101])
noise.ld<-predict(noise.lda,dimen=2)$x
true<-data.frame(noise.ld,grp=x$cl, y = rep(1.5,30))
qplot(LD1,y, data = true, colour=factor(grp),geom="jitter", size = I(3)) + scale_colour_discrete(name="Group") + scale_y_continuous("",breaks=c(0,1.5,3),limits=c(0,3))  + scale_x_continuous("LD1", breaks=c(-1.5,0.0,1.5))






numnoise<-24
x<-matrix(rnorm(30*(numnoise+2)),ncol=(numnoise+2))
#x[1:10,(numnoise+1)]<-x[1:10,(numnoise+1)]+3
#x[11:20,(numnoise+1)]<-x[11:20,(numnoise+1)]-3
#x[21:30,(numnoise+2)]<-x[21:30,(numnoise+2)]+sqrt(27)
colnames(x)<-paste("X",1:(numnoise+2),sep="")
x<-scale(x)
x<-data.frame(x, cl=factor(c(rep(1,8),rep(2,12), rep(3,6), rep(4,4))))

noise.lda<-lda(x[,1:26],x[,27])
noise.ld<-predict(noise.lda,dimen=2)$x
true<-data.frame(noise.ld,grp=x$cl)

 noise.sim <- ldply(1:19, function(k) {
	cl<- true$grp[sample(1:length(true$grp),replace=FALSE)]

	lres <- lda(x[,-41], grouping=factor(cl))
	data.frame(predict(lres)$x[,1:2], grp=cl,.n=k)
})

lineup.noise<-lineup(true=true, samples=noise.sim)
qplot(LD1, LD2, data=lineup.noise, facets=~.sample, colour=grp) 
