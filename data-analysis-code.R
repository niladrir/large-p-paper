setwd("/Users/Niladri/Documents/Research/Large p, small n/turk data analysis")

####===================================================================
# Data Analysis
###====================================================================

###Loading package

library(ggplot2)
library(lubridate)

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




### turk 7 is the cleaned data which should be used for data analysis. Before going any further each line above should run. turk 7 excludes the response provided by Mahbub, Niladri, Dr. Cook and Dr. Hofmann.

###Duplicate Plots

#id_dup_plot <- ddply(turk7, .(id), summarise, dup_id = length(unique(pic_id)) != length(pic_id))

#turk7_dup_id <- subset(turk7, id %in% id_dup_plot$id[id_dup_plot$dup_id == TRUE])

#dat_dup_pic <- data.frame(id=turk7_dup_id$id,response=turk7_dup_id$response,pic_id = turk7_dup_id$pic_id[order(turk7_dup_id$start_time)])

#dat_dup_pic$dif <- c(30, diff(dat_dup_pic$pic_id))

###====================================================================================================
# Data Analysis 
###====================================================================================================

### Summary statistics

id_perf <- ddply(turk7,.(id),summarise,suc=sum(response), tot.attempt = length(response), suc.rate = sum(response)/length(response) )

diff_perf <- ddply(turk7,.(difficulty),summarise,suc=sum(response), tot.attempt = length(response), suc.rate = sum(response)/length(response) )

### Proportion of Correct Response by each parameter ID

suc.rate <- ddply(turk7, .(sample_size,noise,dimension,projection), summarise, suc.rate = sum(response)/length(response), tot.attempt = length(response))

suc.rate.sub <- subset(suc.rate, dimension != 10 & sample_size != 50)

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

ggsave("power-1d.pdf", height = 4, width = 9)

###Both one and two dimensional projections

qplot(factor(dimension), value, data = suc.rate.m, geom = "line", col = I("red"), group = factor(variable), linetype = factor(variable), facets =projection ~ noise) + scale_x_discrete("dimension") + scale_y_continuous("visual power", limits = c(0,1)) + scale_linetype_discrete("test") 

### Plotting the Proportion of Correct Response by each parameter ID

qplot(param_id, suc.rate, data = suc.rate, geom = "bar")


###Splitting the parameter ID variable to obtain the different parameter values

m <- as.data.frame(matrix(unlist(strsplit(suc.rate$param_id, "\\_")), ncol = 4, byrow = T))

names(m) <- c("sample_size", "dimension", "noise", "projection")

### Adding the parameter values to the data frame

suc.rate.param <- data.frame( suc.rate, m )

### Ordering the levels of the dimesion

suc.rate.param$dimension <- factor(suc.rate.param$dimension, levels = c("10" , "20", "40", "60", "80", "100")) 


### Plotting the Proportion of Correct Response by each dimension for data with real separation

qplot(factor(dimension), suc.rate, data = subset(suc.rate.param, dimension!=10 & noise == 0 & sample_size == 30), position = "dodge", geom = "bar", fill = factor(projection), ylim=c(0,1), xlab = "Dimension", ylab = "Proportion of Correct Response", main = "Data with Real Separation") + scale_fill_discrete(name = "Projections") 

### Plotting the Proportion of Correct Response by each dimension for noise data

qplot(factor(dimension), suc.rate, data = subset(suc.rate.param, dimension!=10 & noise == 1 & sample_size == 30), position = "dodge", geom = "bar", fill = factor(projection), ylim=c(0,1), xlab = "Dimension", ylab = "Proportion of Correct Response", main = "Noise Data") + scale_fill_discrete(name = "Projections")  

### Plotting the Proportion of Correct Response by dimension for 1D projections

qplot(factor(dimension), suc.rate, data = subset(suc.rate.param, dimension!=10 & projection == 1 & sample_size == 30), position = "dodge", geom = "bar", fill = factor(noise), ylim=c(0,1)) 

### Plotting the Proportion of Correct Response by dimension for 2D projections

qplot(factor(dimension), suc.rate, data = subset(suc.rate.param, dimension!=10 & projection == 2 & sample_size == 30), position = "dodge", geom = "bar", fill = factor(noise), ylim=c(0,1)) 


### Plotting the Proportion of Correct Response for Paper Wasp dataset

qplot(factor(noise), suc.rate, data = subset(suc.rate.param, sample_size == 50), position = "dodge", geom = "bar",  ylim=c(0,1), xlab = "Noise Data", ylab = "Proportion of Correct Response", main = "Paper Wasp Data") 


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


qplot(time_taken, geom="histogram", binwidth = 20, data = subset(turk7, dimension !=10), facets = noise ~ dimension, fill = factor(projection))

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

turk7.noise.suc <- subset(turk7.pic_name, noise == 1 & suc.rate != 0)
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

### Scatterplot of Frequency vs measures 


dat <- merge(meas, turk7, by.x = "file.name", by.y = "pic_name")

###Projection = 1

dat1 <- subset(dat, dimension != 10 & sample_size != 50 & projection == 1 )

dat1$dimension <- factor(dat1$dimension, levels = c( "20", "40", "60", "80", "100"))

#ls(dat)

#dat$dim_rep <- paste(dat$dimension,"_", dat$replication, sep = "")
#dat$noise_proj <- paste(dat$noise,"_",dat$projection,sep="")

dat1$dim_proj <- paste(dat1$dimension,"_", dat1$projection, sep = "")
dat1$noise_rep <- paste(dat1$noise,"_",dat1$replication,sep="")

dat1.count <- ddply(dat1, .(dim_proj,noise_rep,.sample, plot_location) , summarise, freq = sum(.sample == response_no), wlamb = mean(wlambda), wbratio = mean(wb.ratio) , plot_loc = mean(.sample == plot_location)) 


qplot(wlamb, freq, data = dat1.count, facets = noise_rep ~ dim_proj, geom = "point", col = plot_loc)  + geom_linerange(aes(x = wlamb, ymin = 0, ymax = freq )) + opts(legend.position="none") + scale_colour_continuous(high = "red", low = "black")


qplot(wbratio, freq, data = dat1.count, facets = noise_rep ~ dim_proj, geom = "point", col = plot_loc)  + geom_linerange(aes(x = wbratio, ymin = 0, ymax = freq )) + scale_x_continuous("Wb Ratio", breaks=c(0.1,0.3,0.5,0.7), limits = c(0,0.8)) + opts(legend.position="none") + scale_colour_continuous(high = "red", low = "black")


###Projection = 2

dat2 <- subset(dat, dimension != 10 & sample_size != 50 & projection == 2 )

dat2$dimension <- factor(dat2$dimension, levels = c( "20", "40", "60", "80", "100"))


dat2$dim_proj <- paste(dat2$dimension,"_", dat2$projection, sep = "")
dat2$noise_rep <- paste(dat2$noise,"_",dat2$replication,sep="")

dat2.count <- ddply(dat2, .(dim_proj,noise_rep,.sample, plot_location) , summarise, freq = sum(.sample == response_no), wlamb = mean(wlambda), wbratio = mean(wb.ratio) , plot_loc = mean(.sample == plot_location) )

qplot(log(wlamb), freq, data = dat2.count, facets = noise_rep ~ dim_proj, geom = "point", col = plot_loc)  + geom_linerange(aes(x = log(wlamb), ymin = 0, ymax = freq )) + opts(legend.position="none") + scale_colour_continuous(high = "red", low = "black")


qplot(log(wbratio), freq, data = dat2.count, facets = noise_rep ~ dim_proj, geom = "point", col = plot_loc) + geom_linerange(aes(x = log(wbratio), ymin = 0, ymax = freq )) + opts(legend.position="none") + scale_colour_continuous(high = "red", low = "black")
















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