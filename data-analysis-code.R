setwd("/Users/Niladri/Documents/Research/Large p, small n/turk data analysis")

####===================================================================
# Data Analysis
###====================================================================

###Loading package

library(ggplot2)
library(lubridate)
library(productplots)

###Reading the data

raw_turk7<-read.csv("raw_data_turk7.csv")

meas <- read.csv("measures.csv")

###Cleaning the data

raw_turk7$diff <- as.POSIXct(raw_turk7$start_time) - min(as.POSIXct(raw_turk7$start_time))

raw_turk7 <- subset(raw_turk7, diff >= (as.POSIXct("2012-06-17 09:50:41") - min(as.POSIXct(start_time))))

dat <- ddply(raw_turk7, .(id,difficulty), summarise, tot.attempt = length(response), s = sum(response))

got.test.id <-  dat$id[dat$difficulty == 0]

dat1 <- subset(dat, id %in% got.test.id)

id.diff0.s0 <- unique(dat1$id)[dat1$s[dat1$difficulty == 0] != 0]

dat2 <- subset(dat1, id %in% id.diff0.s0)

turk7_dup <- subset(raw_turk7, id %in% dat2$id)

turk7_dup$sort_pic_id <- ddply(turk7_dup, .(id), summarise, sort_pic_id = pic_id[order(start_time)])$sort_pic_id

turk7_dup$dif <- c(30, diff(turk7_dup$sort_pic_id))

turk7 <- subset(turk7_dup, dif != 0 )

###Changing the sample size of the real data to 50

turk7$sample_size[turk7$difficulty == 5] <-50

### Adding a variable called param_id

turk7$param_id <- paste(turk7$sample_size,"_",turk7$param_value, sep="")

### Breaking the parameter ID into respective columns

m <- as.data.frame(matrix(unlist(strsplit(turk7$param_id, "\\_")), ncol = 4, byrow = T))[,2:4]

names(m) <- c("dimension", "noise", "projection")

turk7 <- data.frame(turk7, m )

split.pic_name <- strsplit(as.character(turk7$pic_name),"\\_")

rep <- ldply( 1:length(split.pic_name), function(k){
 	if (split.pic_name[[k]][2] == "large") s = strsplit( split.pic_name[[k]][10],"\\.")[[1]][1]
 	else s <- strsplit(split.pic_name[[k]][7],"\\.")[[1]][1]
 	data.frame(k=k, S = s) 	
 })

turk7$replication <- rep$S

turk7$dimension <- factor(turk7$dimension, levels = c("10" , "20", "40", "60", "80", "100"))

id_perf <- ddply(turk7,.(id),summarise,suc=sum(response), tot.attempt = length(response), suc.rate = sum(response)/length(response) )

id_10 <- id_perf$id[id_perf$tot.attempt > 8]

turk7 <- subset(turk7, id %in% id_10)

### turk 7 is the cleaned data which should be used for data analysis. Before going any further each line above should run. turk 7 excludes the response provided by Mahbub, Niladri, Dr. Cook and Dr. Hofmann.

###Duplicate Plots

#id_dup_plot <- ddply(turk7, .(id), summarise, dup_id = length(unique(pic_id)) != length(pic_id))

#turk7_dup_id <- subset(turk7, id %in% id_dup_plot$id[id_dup_plot$dup_id == TRUE])

#dat_dup_pic <- data.frame(id=turk7_dup_id$id,response=turk7_dup_id$response,pic_id = turk7_dup_id$pic_id[order(turk7_dup_id$start_time)])

#dat_dup_pic$dif <- c(30, diff(dat_dup_pic$pic_id))

###====================================================================================================
# Data Analysis 
###====================================================================================================

setwd("/Users/Niladri/Documents/Research/Large p, small n/large-p-paper")

### Summary statistics

id_perf <- ddply(turk7,.(id),summarise,suc=sum(response), tot.attempt = length(response), suc.rate = sum(response)/length(response) )

diff_perf <- ddply(turk7,.(difficulty),summarise,suc=sum(response), tot.attempt = length(response), suc.rate = sum(response)/length(response) )

pic_perf <- ddply(subset(turk7, dimension !=10 & sample_size != 50),.(pic_name, dimension, noise, projection, replication),summarise,suc=sum(response), tot.attempt = length(response), suc.rate = sum(response)/length(response) )

pic_perf$dimension <- as.numeric(as.character(pic_perf$dimension))

pic_perf$dim_rep <- paste(pic_perf$dimension,"_",pic_perf$replication, sep = "")

pic_perf$dim_rep <- as.factor(pic_perf$dim_rep)

levels(pic_perf$dim_rep) <- levels(pic_perf$dim_rep)[c(4,5,6,7,8,9,10,11,12,13,14,15,1,2,3)]

levels(pic_perf$noise) <- c("Real Separation","Noise Data")
levels(pic_perf$projection) <- c("1D Projection","2D Projection")

qplot(dimension, suc.rate, data = pic_perf, size = I(3)) + facet_grid(noise ~ projection) +  ylab("Proportion of successful evaluation") + xlab("Dimension")

### Proportion of Correct Response by each parameter ID

suc.rate <- ddply(turk7, .(sample_size,noise,dimension,projection), summarise, suc.rate = sum(response)/length(response), tot.attempt = length(response))

suc.rate.sub <- subset(suc.rate, dimension != 10 & sample_size != 50)

###Bootstrap Confidence Interval

boot.strap <- function(data){
	dat <- ldply(1:10000, function(k){ sample(data, length(data), replace = TRUE)})
	p <- sum(data == "TRUE")/length(data)
	prop.correct <- apply(dat, 1, function(x){ sum(x == "TRUE")/length(x)}) 
	return(c(lb.025 = as.numeric(quantile(prop.correct, 0.025)), p = p, ub.975 = as.numeric(quantile(prop.correct, 0.975))))
}

boot.strap.ci <- ddply(subset(turk7,sample_size == 30 & dimension != 10), .(noise,dimension,projection), summarise, ci = boot.strap(response))

levels(boot.strap.ci$noise) <- c("Real Separation", "Noise Data")


levels(boot.strap.ci$projection) <- c("1D Projection", "2D Projection")

boot.strap.ci$dimension <- as.numeric(as.character(boot.strap.ci$dimension))

boot.strap.ci$meas <- rep(c("lb", "p", "ub"), 20)


### Adjusted Wald Intervals

adj.Wald <- function(data, alpha=0.05, x=2) {
	n <- length(data)
	y <- sum(data)
	n1 <- n+2*x
	p <- (y+x)/n1
	phat <- y/n
	li <- qnorm(1-alpha/2)*sqrt(p*(1-p)/n)
	return(list(lo=max(0,phat-li), p=phat, up=min(1,phat+li)))
}

adj.Wald.ci <- ddply(subset(turk7,sample_size == 30 & dimension != 10), .(noise,dimension,projection), summarise, p = adj.Wald(response)$p, lower = adj.Wald(response)$lo, upper = adj.Wald(response)$up )

levels(adj.Wald.ci$noise) <- c("Real Separation", "Noise Data")


levels(adj.Wald.ci$projection) <- c("1D Projection", "2D Projection")

adj.Wald.ci$dimension <- as.numeric(as.character(adj.Wald.ci$dimension))

#adj.Wald.ci$meas <- rep(c("lb", "p", "ub"), 20)


m <- ggplot(adj.Wald.ci, aes(dimension, p, ymin = lower, ymax = upper)) + facet_grid(noise ~ projection)

m + geom_pointrange() + ylim(c(-0.05, 1))

m + geom_errorbar(width = 5) + ylim(c(0, 1))


m + geom_crossbar(width = 5, colour = I("red")) + ylim(c(-0.05, 1))


#ggsave("suc-rate-ci.pdf", height = 7, width = 7)

###===========================================================================================
### GLM model layered on the observed success rates
###=======================================================================================

dat <- subset(turk7, dimension != 10 & sample_size != 50)

dat$res <- 0
1 -> dat$res[dat$response == "TRUE"]

dat$dimension <- as.numeric(as.character(dat$dimension))


fit.power <- glm(response ~ dimension+ noise + projection
             , family=binomial,data=dat)
res <- summary(fit.power)
#str(res)
res$coef

dimension <- rep(seq(20,100, by=1),each=4)
noise <- factor(rep(rep(c(0,1),each=2),length(dimension)))
projection <- factor(rep(rep(c(1,2),2),length(dimension)))
newdat <- data.frame(dimension,noise,projection)
power <- predict(fit.power, newdata = newdat, type="response", se.fit = TRUE)


pow.dat <- data.frame(dimensions = dimension
                    , empirical=power$fit
                    , noise = noise, projection = projection)
pow.dat.m <- melt(pow.dat, id=c("dimensions","noise","projection"))
head(pow.dat.m)
colnames(pow.dat.m) <- c("dimensions","noise","projection","Test","prob")

levels(pow.dat.m$noise) <- c("Real Separation", "Noise Data")

levels(pow.dat.m$projection) <- c("1D Projection", "2D Projection")

qplot(dimension, prob, geom="line", data=pow.dat.m) + facet_grid(noise ~ projection) + xlab("Dimension") + ylab("Probability")

###Plot showing the GLM model on the observed success rates

#ggplot() + geom_point(data = adj.Wald.ci, aes(x = dimension, y = p), col = I("red"), size = I(3)) + geom_errorbar(data = adj.Wald.ci, aes(x = dimension, y = p, ymin = lower, ymax = upper), col = I("red"), width = 5) +  geom_line(data=pow.dat.m, aes(x = dimension, y = prob), colour = I("blue"), size = I(1.2), alpha = I(0.6)) + facet_grid(noise ~ projection) + xlab("Dimension") + ylab("Proportion of Correct Response") + ylim(c(-0.05, 1))

ggplot() + geom_point(data = pic_perf, aes(x = dimension, y = suc.rate),  size = I(3), alpha = I(0.6))  +  geom_line(data=pow.dat.m, aes(x = dimension, y = prob), size = I(1.2), alpha = I(0.7)) + facet_grid(noise ~ projection) + xlab("Dimension") + ylab("Proportion of Correct Response") 

ggsave("suc-rate-rep-glm.pdf", height = 7, width = 7)


###===========================================================================================
### GLM to see the effect of noise, projection and dimension on the response
###=======================================================================================


dat <- subset(turk7, dimension != 10 & sample_size != 50)

levels(dat$noise) <- c("Real Separation", "Noise Data")
levels(dat$projection) <- c("1D Projection", "2D Projection")

d <- ddply(dat, .(dimension, noise, projection, response), summarize, l = length(response))

d$response <- as.numeric(d$response)

d$dimension <- as.numeric(as.character(d$dimension))

ggplot() + geom_point(data = d, aes(x = dimension, y = response, size = l), alpha = I(0.7)) +  geom_line(data=pow.dat.m, aes(x = dimension, y = prob), colour = I("blue"), size = I(1.2), alpha = I(0.6)) + facet_grid(noise ~ projection) + xlab("Dimension") + ylab("Proportion of Correct Response") 

ggsave("glm-model.pdf", height = 7, width = 7)


###=============================================================
### Subjectwise probability of success by GLM
### ============================================================

library(lme4)

dat$dimension <- as.numeric(as.character(dat$dimension))

fit.mixed <- lmer(response ~ dimension + factor(noise) + factor(projection)
              + (1|id)
              , family="binomial"
              , data=dat)
res <- summary(fit.mixed)
B <- res@coefs[,1]

dimension <- rep(seq(20,100, by=1),each=4)
noise <- rep(rep(c(0,1),each=2),length(dimension))
projection <- rep(rep(c(1,2),2),length(dimension))


delta <- as.numeric(res@REmat[4])

power=NULL
gnd <- NULL
tau <- res@ranef    # estimates for existing subject
M <- length(tau)
for (i in 1:M){
	  X <- cbind(rep(1,length(dimension)),dimension,noise,projection)
  xb <- X %*% B + tau[i]
  power <- cbind(power,exp(xb)/(1+exp(xb))) 
}
colnames(power) <- 1:M


pow.dat1 <- data.frame(dimension, power, noise, projection)
pow.dat.m1 <- melt(pow.dat1, id=c("dimension", "noise", "projection"))

pow.dat.m1$noise <- as.factor(pow.dat.m1$noise)

pow.dat.m1$projection <- as.factor(pow.dat.m1$projection)

levels(pow.dat.m1$noise) <- c("Real Separation", "Noise Data")

levels(pow.dat.m1$projection) <- c("1D Projection", "2D Projection")

ggplot() + geom_line(aes(x = dimension,y = value,group=variable),data=pow.dat.m1, alpha = I(0.1)) + facet_grid(noise ~ projection) + geom_line(aes(x = dimension, y = prob), data=pow.dat.m, colour = I("blue"), size = I(1)) + geom_point(data = pic_perf, aes(x = dimension, y = suc.rate), size = I(3), alpha = I(0.7)) + xlab("Dimension") + ylab("Proportion of Correct Response") + ylim(c(0,1))
 
#ggsave("subjectwise-glm.pdf", height = 7, width = 7)

            



 
###==================================================================================
### Time taken to respond for different levels of dimension for noise and real data
###==================================================================================

levels(turk7$noise) <- c("real separation", "noise data")
levels(turk7$projection) <- c("1D projection", "2D projection")

qplot(dimension, log(time_taken), data = subset(turk7, dimension !=10 & sample_size !=50), geom = "boxplot", fill = noise, colour = noise, facets = projection ~. , alpha = I(0.3), size = I(0.8)) + scale_x_discrete("Dimension", limits = c(20, 40, 60, 80, 100)) + scale_y_continuous("log time taken to respond") + scale_fill_discrete(name = "Data") + scale_color_discrete(name = "Data")

ggsave("time-taken-log.pdf", height = 7, width = 7)

### Mean time vs dimension

turk7.d <- ddply(subset(turk7, dimension !=10 & sample_size !=50), .(noise, dimension, projection), summarize, m = median(time_taken))

qplot(dimension, m, data = turk7.d, geom = c("point"), colour = noise,  size = I(3), facets = projection ~ .) + geom_line(aes(group = noise), size = I(1.2)) + scale_x_discrete("Dimension", limits = c(20, 40, 60, 80, 100)) + scale_y_continuous(name = "Mean time taken to respond ") 


###===================================================================================
### Reason of Choice
###===================================================================================

# 1 = Gap is Biggest
# 2 = Centers are far apart
# 3 = Groups are least spread
# 4 = Groups are in corners
# 5 = Other

qplot(factor(choice_reason), geom = "bar", data = subset(turk7, dimension != 10 & sample_size != 50), facets = projection ~ noise)


turk7.choice <- ddply(subset(turk7, dimension != 10 & sample_size != 50), .(dimension, noise, projection, choice_reason), summarize, l = length(choice_reason))

turk7.choice$choice_reason <- as.factor(turk7.choice$choice_reason)

levels(turk7.choice$choice_reason) <- c("Biggest Gap(1)", "Centers Apart(2)", "Least Spread(3)", "Groups in Corners(4)", "Others(5)", "12", "13", "14", "15", "23", "24", "25", "35", "45", "123", "124", "134", "1234")

qplot(dimension, factor(choice_reason), data = subset(turk7.choice, l > 2 ), fill = l, geom = "tile", facets = noise ~ projection, alpha = I(0.7)) + scale_y_discrete("Reasons of Choice") + scale_x_discrete("Dimension", limits = c(20, 40, 60, 80, 100)) + scale_fill_gradient(name = "Number of people",low="green", high="red")

ggplot() + geom_bar(data = subset(turk7.choice, l > 2), aes(x = factor(choice_reason), weights = l, fill = noise), position = "dodge") + facet_grid(dimension ~ projection) + coord_flip() + scale_x_discrete("Reasons of Choice") + scale_fill_discrete(name = "Data")

ggsave("choice-reason-bar.pdf", height = 8, width = 6)

turk7.size <- ddply(subset(turk7, dimension != 10 & sample_size != 50), .(noise, projection, choice_reason), summarize, l = length(choice_reason), s = sum(response)/length(response))

turk7.size$choice_reason <- as.factor(turk7.size$choice_reason)

levels(turk7.size$choice_reason) <- c("Biggest Gap(1)", "Centers Apart(2)", "Least Spread(3)", "Groups in Corners(4)", "Others(5)", "12", "13", "14", "15", "23", "24", "25", "35", "45", "123", "124", "134", "1234")

qplot(factor(choice_reason), weight = l, data = turk7.size,  facets = noise ~ projection) + coord_flip()

qplot(factor(choice_reason), s, size = l, data = turk7.size, facets = noise ~ projection) + coord_flip()


turk7.res <- ddply(subset(turk7, dimension != 10 & sample_size != 50), .(choice_reason), summarize, l = length(choice_reason), s = sum(response)/length(response))

qplot(factor(choice_reason), s, data = turk7.res, size = l) + coord_flip()


###=========================================================================================
### Difficulty of the plots
###=========================================================================================

dat <- merge(meas, turk7, by.x = "file.name", by.y = "pic_name")

dat1 <- subset(dat, dimension != 10 & sample_size != 50 )


dat2 <- ddply(dat1, .(file.name, dimension, noise, projection), summarise, wb.loc = mean(wb.ratio[.sample == plot_location]), wb.min = min(wb.ratio[.sample != plot_location]), lam.loc = mean(wlambda[.sample == plot_location]), lam.min = min(wlambda[.sample != plot_location]))


qplot(wb.loc, wb.min, data = dat2, color = noise) + geom_abline(slope = 1)

dat2$diff.wb <- 100*(- dat2$wb.loc + dat2$wb.min)/dat2$wb.loc

dat2$diff.lam <- 100*(- dat2$lam.loc + dat2$lam.min)/dat2$lam.loc

turk7.pic.res <- ddply(subset(turk7, dimension != 10 & sample_size != 50 ), .(pic_name), summarise, tot.attempt = length(response), s = sum(response)/length(response))

dat3 <- data.frame(dat2, tot.attempt = turk7.pic.res$tot.attempt, suc.rate = turk7.pic.res$s)

levels(dat3$noise) <- c("real separation", "noise data")

qplot(diff.wb, suc.rate, data = dat3, colour = factor(dimension), size = I(3.5), shape = factor(noise) ) + geom_vline(xintercept = 0) + ylab("Probability of successful evaluation") + xlab("Difference") + scale_colour_discrete("Dimension") + scale_shape_discrete("Data")

ggsave("suc-diff-wbratio.pdf", height = 5.5, width = 7)

qplot(diff.wb + 60, suc.rate, data = dat3, size = I(3.5), log = "x", ylim=c(0,1) ) + geom_vline(xintercept = 60) + geom_smooth(method = "lm", se = FALSE) + ylab("Probability of successful evaluation") + xlab("Difference") + scale_colour_discrete("Dimension") + scale_shape_discrete("Data") 

qplot(diff.lam, suc.rate, data = dat3, colour = factor(dimension), size = I(3.5), shape = factor(noise), xlim = c(-150, 2000), facets = . ~ projection) + geom_vline(xintercept = 0)





###=====================================================================================
### Confidence Level
###=====================================================================================


turk7.cl <- ddply(subset(turk7, dimension !=10 & sample_size !=50), .(dimension, noise, projection, conf_level), summarize, s = sum(response)/length(response))

qplot(conf_level, s, data = turk7.cl, size = I(3), alpha = I(0.4), geom = "point", facets = noise ~ projection) + geom_smooth(method = "lm")

qplot(conf_level, s, data = turk7.cl, size = I(3), alpha = I(0.4), geom = "point", colour = factor(dimension), facets = noise ~ projection) + geom_smooth(method = "lm", se = FALSE)

## + geom_line(aes(group = dimension))

turk7.time <- ddply(subset(turk7, dimension !=10 & sample_size !=50), .(dimension, noise, projection, conf_level), summarize, m = mean(time_taken))

#turk7.time$conf_level <- as.factor(turk7.time$conf_level)

#levels(turk7.time$conf_level) <- c("Most(1)", "2", "3", "4", "Least(5)")

qplot( m, factor(conf_level), data = turk7.time, alpha = I(0.4), size = I(3), geom = "point", facets = noise ~ projection) + geom_smooth(method = "lm") 

+ ylab("Mean time taken to respond") + scale_x_continuous("Confidence Level", breaks = c(1,2,3,4, 5), labels = c("Most","2","3","4","Least"))

qplot(conf_level, m, data = turk7.time, size = I(3), alpha = I(0.4), geom = "point", colour = factor(dimension), facets = noise ~ projection, xlab = "Confidence Level", ylab = "Mean time taken to respond") + geom_smooth(method = "lm", se = FALSE) + scale_colour_discrete(name = "Dimension")


qplot(factor(conf_level), time_taken, data = subset(turk7, dimension !=10 & sample_size !=50 & time_taken < 400), geom = "boxplot", facets = noise ~ projection, colour = I("red")) + coord_flip() 


###Proportional Odds Logistic Regression Model

dat <- merge(meas, turk7, by.x = "file.name", by.y = "pic_name")

dat1 <- subset(dat, dimension != 10 & sample_size != 50 )


dat2 <- ddply(dat1, .(file.name, dimension, noise, projection), summarise, wb.loc = mean(wb.ratio[.sample == plot_location]), wb.min = min(wb.ratio[.sample != plot_location]))


dat2$diff.wb <- 100*(- dat2$wb.loc + dat2$wb.min)/dat2$wb.loc

dat.conf <- merge(turk7, dat2, by.y = "file.name", by.x = "pic_name")

dat.conf.small <- subset(dat.conf, select = c("id", "time_taken", "conf_level", "diff.wb"))

dat.conf.small$diff.cat <- "Difficult"
dat.conf.small$diff.cat[dat.conf.small$diff.wb > 0] <- "Easy"
#dat.conf.small$diff.cat[dat.conf.small$diff.wb > 25 ] <- "Easy"

dat.conf.small$diff.cat <- as.factor(dat.conf.small$diff.cat)


head(dat.conf.small)

library(MASS)

fit.polr <- polr(factor(conf_level) ~ time_taken*diff.cat , data = na.omit(dat.conf.small))

summary(fit.polr)

X <- data.frame(expand.grid(
	time_taken=seq(5,953, by = 40), diff.cat = c("Difficult","Easy")
))
X <- data.frame(X,predict(fit.polr, newdata=X, type="p"))
library(reshape2)
X.melt <- melt(X, id.vars=c("time_taken","diff.cat"))


levels(X.melt$variable) <- c("Most(1)", "2", "3", "4", "Least(5)")

qplot(time_taken, value, geom=c("point", "line"),colour = variable, shape = diff.cat, data=X.melt)+ylim(c(0,1))+ylab("Estimated Probabilities") + xlab("Time Taken to Respond") + scale_colour_discrete("Confidence Level") + scale_shape_discrete("Difficulty Level")



###================================================================================
### Subject Wise Probabilty 
###================================================================================

library(lme4)
fit.mixed <- lmer(response ~ gender+dimension+ noise + projection
              + (1|id)
              , family="binomial"
              , data=dat)
res <- summary(fit.mixed)
B <- res@coefs[,1]

dimension <- rep(seq(20,100, by=1),each=4)
noise <- factor(rep(rep(c(0,1),each=2),length(dimension)))
projection <- factor(rep(rep(c(1,2),2),length(dimension)))

#dimension <- seq(20,100, by=1)
#noise <- factor(rep(0,length(dimension)))
#projection <- factor(rep(1,length(dimension)))



#beta <- seq(0.01,16, by=.2)
#head(res@frame)
#sample.size <- rep(0,length(beta))
#sigma.val <- rep(1,length(beta))
#conf_level <- rep(1,length(beta))


delta <- as.numeric(res@REmat[4])

power=NULL
gnd <- NULL
tau <- res@ranef    # estimates for existing subject
M <- length(tau)
for (i in 1:M){
  #tau <- rnorm(mean=0,sd=delta, n=1) # estimates for new subject
  gender.val <- rep(as.numeric(dat$gender[dat$id==i][1])-1,length(dimension))
  X <- cbind(rep(1,length(dimension)),gender.val,dimension,noise,projection)
  #dim(X)
  xb <- X %*% B + tau[i]
  power <- cbind(power,exp(xb)/(1+exp(xb)))
  gnd <- c(gnd,gender.val[1])
}
colnames(power) <- 1:M


pow.dat <- data.frame(dimension, power, noise, projection)
dim(pow.dat)
pow.dat.m <- melt(pow.dat, id=c("dimension", "noise", "projection"))
pow.dat.m$gender <- factor(c(gnd[rep(1:M, each=81*16)]), labels=c("Male","Female"))
head(pow.dat.m)
tail(pow.dat.m)

levels(pow.dat.m$noise) <- c("real separation", "noise data")
levels(pow.dat.m$projection) <- c("1D projection", "2D projection")

p <- qplot(dimension,value,group=variable,geom="line", colour = factor(gender), data=subset(pow.dat.m, !is.na(gender)), alpha = I(0.5)) + facet_grid(noise ~ projection)
p + xlab("Dimension") + ylab("Probability of making the correct response") + scale_colour_discrete("Gender")


ggsave("glm-subjectwise.pdf", height = 7, width = 7)

###===================================================================
### Scatterplot of Frequency vs measures
###===================================================================
 


dat <- merge(meas, turk7, by.x = "file.name", by.y = "pic_name")

###Projection = 1

dat1 <- subset(dat, dimension != 10 & sample_size != 50 )

dat1$dimension <- factor(dat1$dimension, levels = c( "20", "40", "60", "80", "100"))

#ls(dat)

#dat$dim_rep <- paste(dat$dimension,"_", dat$replication, sep = "")
#dat$noise_proj <- paste(dat$noise,"_",dat$projection,sep="")

dat1$dim_proj <- paste(dat1$dimension,"_", dat1$projection, sep = "")
dat1$noise_rep <- paste(dat1$noise,"_",dat1$replication,sep="")

dat1.count <- ddply(dat1, .(dim_proj,noise_rep,.sample, plot_location) , summarise, s = sum(.sample == response_no)/length(.sample == response_no), wlamb = mean(wlambda), wbratio = mean(wb.ratio) , plot_loc = mean(.sample == plot_location)) 


qplot(wlamb, s, data = dat1.count, geom = "point", col = plot_loc)  + geom_linerange(aes(x = wlamb, ymin = 0, ymax = s )) + opts(legend.position="none") + scale_colour_continuous(high = "red", low = "black") + facet_grid(noise_rep ~ dim_proj, scales = "free_x")

dat1.count$dim_proj <- factor(dat1.count$dim_proj, levels <- c("20_1", "20_2", "40_1", "40_2", "60_1", "60_2", "80_1", "80_2", "100_1", "100_2"))
dat1.count$noise_rep <- factor(dat1.count$noise_rep)
levels(dat1.count$dim_proj) <- c( "dim:20 \n proj:1", "dim:20 \n proj:2", "dim:40 \n proj:1", "dim:40 \n proj:2", "dim:60 \n proj:1", "dim:60 \n proj:2", "dim:80 \n proj:1", "dim:80 \n proj:2", "dim:100 \n proj:1", "dim:100 \n proj:2")
levels(dat1.count$noise_rep) <- c("data:real \n rep:1", "data:real \n rep:2", "data:real \n rep:3", "data:noise \n rep:1", "data:noise \n rep:2","data:noise \n rep:3" )
qplot(wbratio, s, data = dat1.count, geom = "point", col = plot_loc)  + geom_linerange(aes(x = wbratio, ymin = 0, ymax = s )) + scale_x_continuous("WB Ratio", labels =NULL, breaks = NULL ) + scale_y_continuous("Relative Frequency of picks", limits = c(0,1), breaks = c(0, 0.5, 1)) + opts(legend.position="none", axis.ticks = theme_blank(), axis.text.x = theme_blank()) + scale_colour_continuous(high = "red", low = "black") + facet_grid(noise_rep ~ dim_proj, scales = "free_x")


ggsave("wbratio.pdf", height = 7, width = 9)

### Plotting the Proportion of Correct Response for Paper Wasp dataset

wasp.turk7 <- subset(turk7, sample_size == 50)
wasp.turk7.suc <- ddply(wasp.turk7, .(noise), summarize, tot.attempt = length(response), suc = sum(response)/length(response))


qplot(factor(noise), suc , data = wasp.turk7.suc, position = "dodge", geom = "bar",  ylim=c(0,1), xlab = "Noise Data", ylab = "Proportion of Correct Response", main = "Paper Wasp Data") 

adj.Wald.ci <- ddply(wasp.turk7, .(noise), summarise, p = adj.Wald(response)$p, lower = adj.Wald(response)$lo, upper = adj.Wald(response)$up )

ggplot() + geom_point(data = adj.Wald.ci, aes(x = noise, y = p), col = I("red"), size = I(3)) + geom_errorbar(data = adj.Wald.ci, aes(x = noise, y = p, ymin = lower, ymax = upper), col = I("red"), width = 0.15) + ylim(c(-0.02,1))

#wasp.turk7$response <- as.factor(wasp.turk7$response)

levels(wasp.turk7.suc$noise) <-  c("Wasp Data", "Permuted Data")
levels(adj.Wald.ci$noise) <- c("Wasp Data", "Permuted Data")

ggplot() + geom_bar(data = wasp.turk7.suc, aes(noise, suc)) + geom_point(data = adj.Wald.ci, aes(x = noise, y = p), col = I("red"), size = I(3)) + geom_errorbar(data = adj.Wald.ci, aes(x = noise, y = p, ymin = lower, ymax = upper), col = I("red"), width = 0.15) + scale_x_discrete("") + scale_y_continuous("Probability of Successful Evaluation", limits = c(-0.02,1))

ggsave("wasp-result.pdf", height = 5, width = 4)

####====================================================================================================
####====================================================================================================
####====================================================================================================
####====================================================================================================
####====================================================================================================
####====================================================================================================

###===========================================================================================
### GLM model with the interaction terms layered on the observed success rates
###=======================================================================================

dat <- subset(turk7, dimension != 10 & sample_size != 50)

dat$res <- 0
1 -> dat$res[dat$response == "TRUE"]

dat$dimension <- as.numeric(as.character(dat$dimension))


fit.int <- glm(response ~ dimension + noise + projection + dimension*noise
             , family=binomial,data=dat)
res <- summary(fit.int)
#str(res)
round(res$coef,5)

dimension <- rep(seq(20,100, by=1),each=4)
noise <- factor(rep(rep(c(0,1),each=2),length(dimension)))
projection <- factor(rep(rep(c(1,2),2),length(dimension)))
newdat <- data.frame(dimension,noise,projection,interaction = factor(dimension*as.numeric(as.character(noise))))
power.int <- predict(fit.int, newdata = newdat, type="response", se.fit = TRUE)


pow.dat.int <- data.frame(dimensions = dimension
                    , empirical=power.int$fit
                    , noise = noise, projection = projection)
pow.dat.int.m <- melt(pow.dat.int, id=c("dimensions","noise","projection"))
#head(pow.dat.m)
colnames(pow.dat.int.m) <- c("dimensions","noise","projection","Test","prob")

levels(pow.dat.int.m$noise) <- c("Real Separation", "Noise Data")

levels(pow.dat.int.m$projection) <- c("1D Projection", "2D Projection")

qplot(dimension, prob, geom="line", data=pow.dat.int.m) + facet_grid(noise ~ projection) + xlab("Dimension") + ylab("Probability")

###Plot showing the GLM model on the observed success rates

ggplot() + geom_point(data = adj.Wald.ci, aes(x = dimension, y = p), col = I("red"), size = I(3)) + geom_errorbar(data = adj.Wald.ci, aes(x = dimension, y = p, ymin = lower, ymax = upper), col = I("red"), width = 5) +  geom_line(data=pow.dat.int.m, aes(x = dimension, y = prob), colour = I("blue"), size = I(1.2), alpha = I(0.6)) + facet_grid(noise ~ projection) + xlab("Dimension") + ylab("Proportion of Correct Response") + ylim(c(-0.05, 1))

ggsave("suc-rate-glm-interaction.pdf", height = 7, width = 7)

###Adding a squared term of the dimension

fit.square <- glm(response ~ poly(dimension,2) + noise + projection + poly(dimension,2)*noise
             , family=binomial,data=dat)
res <- summary(fit.square)
#str(res)
round(res$coef,5)

dimension <- rep(seq(20,100, by=1),each=4)
noise <- factor(rep(rep(c(0,1),each=2),length(dimension)))
projection <- factor(rep(rep(c(1,2),2),length(dimension)))
dimension.sq = dimension^2
newdat <- data.frame(dimension,dimension.sq,noise,projection, interaction = factor(dimension*as.numeric(as.character(noise))), interaction.sq = factor(dimension.sq*as.numeric(as.character(noise))))
power.square <- predict(fit.square, newdata = newdat, type="response", se.fit = TRUE)

pow.dat.sq <- data.frame(dimensions = dimension
                    , empirical=power.square$fit
                    , noise = noise, projection = projection)
pow.dat.sq.m <- melt(pow.dat.sq, id=c("dimensions","noise","projection"))
#head(pow.dat.m)
colnames(pow.dat.sq.m) <- c("dimensions","noise","projection","Test","prob")

levels(pow.dat.sq.m$noise) <- c("Real Separation", "Noise Data")

levels(pow.dat.sq.m$projection) <- c("1D Projection", "2D Projection")

ggplot() + geom_point(data = adj.Wald.ci, aes(x = dimension, y = p), col = I("red"), size = I(3)) + geom_errorbar(data = adj.Wald.ci, aes(x = dimension, y = p, ymin = lower, ymax = upper), col = I("red"), width = 5) +  geom_line(data=pow.dat.sq.m, aes(x = dimension, y = prob), colour = I("blue"), size = I(1.2), alpha = I(0.6)) + facet_grid(noise ~ projection) + xlab("Dimension") + ylab("Proportion of Correct Response") + ylim(c(-0.05, 1))

ggsave("suc-rate-glm-square.pdf", height = 7, width = 7)


####===========================================================================================
####Power Calculation (Not needed for this project)
####===========================================================================================


suc.rate.sub$Power <- suc.rate.sub$suc.rate

for(i in 1:dim(suc.rate.sub)[1]){
suc.rate.sub$Power[i][suc.rate.sub$noise[i] == 1] <- 1 - suc.rate.sub$suc.rate[i]
}

suc.rate.sub$std.err <- sqrt(suc.rate.sub$Power*(1 - suc.rate.sub$Power)/suc.rate.sub$tot.attempt)

for(i in 1:dim(suc.rate.sub)[1]){
suc.rate.sub$Upper_limit[i] <- min(suc.rate.sub$Power[i] + qnorm(0.975)*suc.rate.sub$std.err[i], 1)
}

suc.rate.sub$Lower_limit <- suc.rate.sub$Power - qnorm(0.975)*suc.rate.sub$std.err

levels(suc.rate.sub$noise) <- c("real separation", "noise data")

suc.rate.m <- melt(suc.rate.sub, id = c("sample_size", "noise", "dimension","projection", "suc.rate", "tot.attempt", "std.err"))


###Only one dimensional projection

qplot(factor(dimension), value, data = subset(suc.rate.m, projection == 1), geom = "line", col = I("red"), group = factor(variable), linetype = factor(variable), facets = ~ noise) + scale_x_discrete("dimension") + scale_y_continuous("visual power", limits = c(0,1)) + scale_linetype_discrete("test") 

#ggsave("power-1d.pdf", height = 4, width = 9)

###Both one and two dimensional projections

qplot(factor(dimension), value, data = suc.rate.m, geom = "line", col = I("red"), group = factor(variable), linetype = factor(variable), facets =projection ~ noise) + scale_x_discrete("dimension") + scale_y_continuous("visual power", limits = c(0,1)) + scale_linetype_discrete("test") 

####===========================================================================================
####===========================================================================================


### Plotting the Proportion of Correct Response by each parameter ID

#qplot(param_id, suc.rate, data = suc.rate, geom = "bar")


###Splitting the parameter ID variable to obtain the different parameter values

#m <- as.data.frame(matrix(unlist(strsplit(suc.rate$param_id, "\\_")), ncol = 4, byrow = T))

#names(m) <- c("sample_size", "dimension", "noise", "projection")

### Adding the parameter values to the data frame

#suc.rate.param <- data.frame( suc.rate, m )

### Ordering the levels of the dimesion

#suc.rate.param$dimension <- factor(suc.rate.param$dimension, levels = c("10" , "20", "40", "60", "80", "100")) 


### Plotting the Proportion of Correct Response by each dimension for data with real separation

qplot(factor(dimension), suc.rate, data = subset(suc.rate, dimension!=10 & noise == 0 & sample_size == 30), position = "dodge", geom = "bar", fill = factor(projection), ylim=c(0,1), xlab = "Dimension", ylab = "Proportion of Correct Response", main = "Data with Real Separation") + scale_fill_discrete(name = "Projections") 

### Plotting the Proportion of Correct Response by each dimension for noise data

qplot(factor(dimension), suc.rate, data = subset(suc.rate, dimension!=10 & noise == 1 & sample_size == 30), position = "dodge", geom = "bar", fill = factor(projection), ylim=c(0,1), xlab = "Dimension", ylab = "Proportion of Correct Response", main = "Noise Data") + scale_fill_discrete(name = "Projections")  

### Plotting the Proportion of Correct Response by dimension for 1D projections

qplot(factor(dimension), suc.rate, data = subset(suc.rate, dimension!=10 & projection == 1 & sample_size == 30), position = "dodge", geom = "bar", fill = factor(noise), ylim=c(0,1)) 

### Plotting the Proportion of Correct Response by dimension for 2D projections

qplot(factor(dimension), suc.rate, data = subset(suc.rate, dimension!=10 & projection == 2 & sample_size == 30), position = "dodge", geom = "bar", fill = factor(noise), ylim=c(0,1)) 






###==================================================================================
# Looking at the responses conditioned on Gender
###==================================================================================

success.gen <- ddply(turk7, .(param_id, gender), summarise, suc.rate = sum(response)/length(response), tot.attempt = length(response))

m <- as.data.frame(matrix(unlist(strsplit(success.gen$param_id, "\\_")), ncol = 4, byrow = T))

names(m) <- c("sample_size", "dimension", "noise", "projection")

success.gender <- data.frame( success.gen, m )

success.gender$dimension <- factor(success.gender$dimension, levels = c("10" , "20", "40", "60", "80", "100")) 

qplot(factor(dimension), suc.rate, data = subset(success.gender, !is.na(gender) & dimension!=10 & sample_size == 30), geom = c("blank"), position="dodge", facets=projection ~ noise, xlab = "Dimension", ylab = "Proportion of Correct Response") + scale_fill_discrete(name = "Gender")  + geom_line(aes(group = factor(gender), col = factor(gender)))


###==================================================================================
# Looking at the responses conditioned on Age
###==================================================================================

success.age <- ddply(turk7, .(param_id, age), summarise, suc.rate = sum(response)/length(response), tot.attempt = length(response))

m <- as.data.frame(matrix(unlist(strsplit(success.age$param_id, "\\_")), ncol = 4, byrow = T))

names(m) <- c("sample_size", "dimension", "noise", "projection")

success.age <- data.frame( success.age, m )

success.age$dimension <- factor(success.age$dimension, levels = c("10" , "20", "40", "60", "80", "100")) 

qplot(factor(dimension), suc.rate, data = subset(success.age, !is.na(age) & dimension!=10 & sample_size == 30), geom = "blank"  , col = factor(age), facets=projection ~ noise, xlab = "Dimension", ylab = "Proportion of Correct Response") + scale_fill_discrete(name = "Age")  + geom_line(aes(group=factor(age)))

###==================================================================================
# Looking at the responses conditioned on Academic Study
###==================================================================================

success.aca <- ddply(turk7, .(param_id, academic_study), summarise, suc.rate = sum(response)/length(response), tot.attempt = length(response))

m <- as.data.frame(matrix(unlist(strsplit(success.aca$param_id, "\\_")), ncol = 4, byrow = T))

names(m) <- c("sample_size", "dimension", "noise", "projection")

success.academic <- data.frame( success.aca, m )

success.academic$dimension <- factor(success.academic$dimension, levels = c("10" , "20", "40", "60", "80", "100")) 

qplot(factor(dimension), suc.rate, data = subset(success.academic, !is.na(academic_study) & dimension!=10 & sample_size == 30), geom = "blank"  , col = factor(academic_study), facets=projection ~ noise, xlab = "Dimension", ylab = "Proportion of Correct Response") + scale_colour_discrete(name = "Academic Study")  + geom_line(aes(group=factor(academic_study)))

###==================================================================================
# Looking at the responses conditioned on Time
###==================================================================================


qplot(time_taken, geom="histogram", binwidth = 20, data = subset(turk7, dimension !=10), facets = . ~ noise, fill = factor(response), xlim = c(0, 250), alpha = I(0.5), col = factor(response))

qplot(dimension,time_taken, geom = "boxplot", data = subset(turk7, dimension !=10 & sample_size == 30), fill = noise, alpha = I(0.3), col = noise, ylim = c(0, 400), size = I(0.8) ) + scale_x_discrete(limits = c(20, 40, 60, 80, 100))

###=====================================================================================
# Response by each picture
###=====================================================================================

turk7.pic <- ddply(turk7, .(pic_name), summarise, tot.appear = length(response), suc.rate = sum(response)/length(response))

m1 <- matrix(unlist(strsplit(as.character(turk7.pic$pic_name[1:70]), "\\_")), ncol = 10, byrow = T)
m2 <- matrix(unlist(strsplit(as.character(turk7.pic$pic_name[71:76]), "\\_")), ncol = 7, byrow = T)
m11 <- matrix(unlist(strsplit(as.character(m1[,10]), "\\.")), ncol = 2, byrow = T)
m21 <- matrix(unlist(strsplit(as.character(m2[,7]), "\\.")), ncol = 2, byrow = T)
m <- rbind(data.frame(V1 = m1[,6], V2 = m1[,7], V3 = m1[,8], V4 = m1[,9], V5 = m11[,1]),data.frame(V1 = as.character(50), V2 = m2[,4], V3 = m2[,5], V4 = m2[,6], V5 = m21[,1]))
names(m) <- c("sample_size", "dimension", "noise", "projection", "replication")

turk7.pic_name <- data.frame(turk7.pic, m)

## Success Rate on the noise data

qplot(factor(pic_name), suc.rate, geom = "bar", data = subset(turk7.pic_name, noise == 1 & suc.rate != 0))  + opts(axis.text.x=theme_text(angle=75, hjust = 0.45))

###==========================================================================================
# Matching the measures calculated 
###==========================================================================================

###Noise Data

turk7.pic <- ddply(turk7, .(dimension, noise, projection, replication, pic_name), summarize, tot.attempt = length(response), suc.rate = sum(response)/length(response))

turk7.noise.suc <- subset(turk7.pic, noise == 1 & suc.rate != 0)
noise.suc <- subset(turk7, pic_name %in% turk7.noise.suc$pic_name & response == TRUE)

meas.noise <- merge(meas, noise.suc, by.x = "file.name", by.y = "pic_name")

meas.noise$mat <- meas.noise$.sample == meas.noise$response_no

qplot(factor(file.name), wlambda, data = meas.noise, col = factor(mat))  + opts(axis.text.x=theme_text(angle=75, hjust = 0.45))

qplot(.sample, log(wlambda), data = meas.noise, col = factor(mat), size = I(3.5), alpha = I(0.6)) + facet_wrap(~ pic_id)

qplot(.sample, log(wb.ratio), data = meas.noise, col = factor(mat), size = I(3.5), alpha = I(0.6)) + facet_wrap(~ pic_id)

####Real Separation

turk7.real.suc <- subset(turk7.pic_name, noise == 0 & suc.rate < 0.4)

real.suc <- subset(turk7, pic_name %in% turk7.real.suc$pic_name & response == FALSE)

meas.real <- merge(meas, real.suc, by.x = "file.name", by.y = "pic_name")

meas.real$mat <- 1
meas.real$mat[meas.real$.sample == meas.real$response_no] <- 2
meas.real$mat[meas.real$.sample == meas.real$plot_location] <- 3


qplot(factor(file.name), log(wlambda), data = meas.real, col = factor(mat), size = I(3.5), alpha = I(0.6))  + opts(axis.text.x=theme_text(angle=75, hjust = 0.45))

qplot(.sample, log(wlambda), data = meas.real, col = factor(mat), size = I(3.5), alpha = I(0.1)) + facet_wrap(~ pic_id)

qplot(.sample, log(wb.ratio), data = meas.real, col = factor(mat), size = I(3.5), alpha = I(0.1)) + facet_wrap(~ pic_id)



###===================================================================================================
###Time taken vs other variables
###====================================================================================================


qplot(factor(pic_id), time_taken, data = subset(turk7, dimension != 10 & noise == 0), col = factor(response), size = I(4), alpha = I(0.5)) + coord_flip()

qplot(factor(pic_id), time_taken, data = subset(turk7, dimension != 10 & noise == 1), col = factor(response), size = I(4), alpha = I(0.5)) + coord_flip()

qplot(factor(pic_id), time_taken, data = subset(turk7, dimension != 10 & response == FALSE), col = factor(noise), size = I(4), alpha = I(0.5)) + coord_flip()

qplot(factor(pic_id), time_taken, data = subset(turk7, dimension != 10 & response == TRUE), col = factor(noise), size = I(4), alpha = I(0.5)) + coord_flip()

qplot(factor(pic_id), time_taken, data = subset(turk7, dimension != 10), col = factor(noise), size = I(4), alpha = I(0.5)) + coord_flip()

####========================================================================================================















###=================================================================================
# Matching the lineup with the lineup data
###=================================================================================

dat<-read.table(file.choose(),header=T)
head(dat)
y<-rep(1.5,30)
qplot(x,y,data=dat, geom="jitter",col=factor(cl), ylim=c(0,3)) + facet_wrap(~.sample)

qplot(x,y, data=dat, colour=factor(cl),geom="jitter") + facet_wrap(~ .sample) + scale_colour_discrete(name="Group") + scale_y_continuous("",breaks=c(0,1.5,3),limits=c(0,3))  + scale_x_continuous("", breaks=c(-1.5,0.0,1.5)) + opts(legend.position="none")

ggsave("clone_30_10_0_1_1.png" , width=7.17, height=7.17, dpi=75)

dat2<-read.table(file.choose(),header=T)

head(dat2)

qplot(LD1, LD2, data=dat2, col = grp) + facet_wrap(~.sample)