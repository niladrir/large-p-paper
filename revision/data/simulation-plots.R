setwd("/Users/Niladri/Documents/Research/Large p, small n/large-p-paper/submission/data")

# Load libraries
library(ggplot2)
library(nullabor)

# Generate data
#conc.data<-data.frame(label=c(rep("site A",15),rep("site B", 5)), conc=c(rexp(15)*50, rexp(5,)*50+60))

# Plots
#qplot(label, conc, data=conc.data, size=I(4), alpha=I(0.5), ylab="Conc (mg/kg)", xlab="Site", colour=label, ylim=c(0,max(conc.data$conc)))

# Save this data
#write.csv(conc.data, "conc.csv",row.names=F, quote=F)

###Example Lineup

conc.data <- read.csv("conc.csv")

lineup.dat <- lineup(null_permute("label"), conc.data, pos = 16)

write.csv(lineup.dat, "lineup-conc.csv",row.names=F, quote=F)

qplot(label, conc, data= lineup.dat,  colour=label, size=I(3), alpha=I(0.6), ylab="Conc (mg/kg)", xlab="Site") + facet_wrap(~.sample)

ggsave("lineup-dot.pdf", width = 6.17, height = 5.17)

### 1D projection Lineup

dat1 <- read.table("dat_large_p_small_n_30_20_0_1_1.txt", header = T)

qplot(x ,1.5, data = dat1, colour=factor(cl),geom="jitter", size = I(2)) + facet_wrap(~ .sample) + scale_colour_discrete(name="Group") + scale_y_continuous("",breaks=c(0,1.5,3),limits=c(0,3))  + scale_x_continuous("PD1", breaks=c(-1.5,0.0,1.5))

ggsave("lineup-20-real-1-17.pdf", height = 5.17, width = 6.17)

### 2D projection Lineup

dat2 <- read.table("dat_large_p_small_n_30_100_1_2_1.txt", header = T)

qplot(X1 ,X2, data = dat2, colour=factor(cl), size = I(2)) + facet_wrap(~ .sample) + scale_colour_discrete(name="Group")  + scale_y_continuous("PD2") + scale_x_continuous("PD1")

ggsave("lineup-100-noise-2-20.pdf", height = 5.17, width = 6.17)

### Wasp Data Lineup

wasp.dat <- read.table("dat_wasp_30_40_0_2_1.txt", header = T)

qplot(LD1 ,LD2, data = wasp.dat, colour=factor(grp), size = I(2)) + facet_wrap(~ .sample) + scale_colour_discrete(name="Group")  + scale_y_continuous("LD2") + scale_x_continuous("LD1")

ggsave("lineup-wasp-8.pdf", height = 5.17, width = 6.17)

### Increasing dimension and fixed sample size

noise_2 <- read.table("dat-noise-2.txt", header = T)

qplot(LD1,y, data = noise_2, colour=factor(cl),geom="jitter", size = I(3)) + theme(legend.position="none") + scale_y_continuous("",breaks=c(0,1.5,3),limits=c(0,3)) + scale_x_continuous("LD1")

ggsave("noise-2.pdf", height = 3.17, width = 3.17)

noise_8 <- read.table("dat-noise-8.txt", header = T)

qplot(LD1,y, data = noise_8, colour=factor(cl),geom="jitter", size = I(3)) + theme(legend.position="none") + scale_y_continuous("",breaks=c(0,1.5,3),limits=c(0,3)) + scale_x_continuous("LD1")

ggsave("noise-8.pdf", height = 3.17, width = 3.17)

noise_15 <- read.table("dat-noise-15.txt", header = T)

qplot(LD1,y, data = noise_8, colour=factor(cl),geom="jitter", size = I(3)) + theme(legend.position="none") + scale_y_continuous("",breaks=c(0,1.5,3),limits=c(0,3)) + scale_x_continuous("LD1")

ggsave("noise-15.pdf", height = 3.17, width = 3.17)

noise_22 <- read.table("dat-noise-22.txt", header = T)

qplot(LD1,y, data = noise_22, colour=factor(cl),geom="jitter", size = I(3)) + theme(legend.position="none") + scale_y_continuous("",breaks=c(0,1.5,3),limits=c(0,3)) + scale_x_continuous("LD1")

ggsave("noise-22.pdf", height = 3.17, width = 3.17)

noise_28 <- read.table("dat-noise-28.txt", header = T)

qplot(LD1,y, data = noise_28, colour=factor(cl),geom="jitter", size = I(3)) + theme(legend.position="none") + scale_y_continuous("",breaks=c(0,1.5,3),limits=c(0,3)) + scale_x_continuous("LD1")

ggsave("noise-28.pdf", height = 3.17, width = 3.17)