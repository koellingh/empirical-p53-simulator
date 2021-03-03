require(ggplot2)
require(scales)

#These are attractive and color-blind friendly colors to use
colors <- c("#673F03", "#7D3002", "#891901", "#A7000F", "#B50142", "#CD0778", "#D506AD", "#E401E7", "#AB08FF","#7B1DFF", "#5731FD","#5E8EFF", "#4755FF" ,"#6FC4FE", "#86E9FE", "#96FFF7", "#B2FCE3", "#BBFFDB", "#D4FFDD", "#EFFDF0")

#Set your working directory
setwd("~/empirical-p53-simulator/trial1")

#You can have whatever variables you want like this
pop_cap <- 10000

#Read in your data
initial_data <- read.csv("SP1malig0.0benig0.0p530.0.dat", h=T)

#You can add extra columns with calculations with cbind
#initial_data <-cbind(initial_data, hist_0_prop=initial_data$hist_0/initial_data$count, Treatment=as.factor(initial_data$treatment))

#You can get just a portion of your data with subset
#early <- subset(initial_data, update<20)
#just_middle <- subset(initial_data, treatment==0.5)

#If you have NaN values that you would like to be 0, this is how
zeroed <- initial_data
zeroed[is.na(zeroed)] <- 0


#This will make a line plot over time
ggplot(data=early, aes(x=update, y=mean_p53, group=treatment, colour=treatment)) + ylab("Mean Cooperation Value") + xlab("Updates") + stat_summary(aes(color=treatment, fill=treatment),fun.data="mean_cl_boot", geom=c("smooth"), se=TRUE) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(fill=FALSE) +scale_colour_manual(values=colors) + scale_fill_manual(values=colors) +xlim(0,20)

update_1 <- subset(initial_data, update==1)

# This will make a point plot
ggplot(data=data_mut_wild_stuff, aes(x=update, y=mean_p53)) +ylab("mean_p53") + xlab("update") + geom_point() + ggtitle("malig0.25 benig0.25 p530.05")
ggplot(data=initial_data, aes(x=count, y=mean_p53)) +ylab("mean_p53") + xlab("number of organisms") + geom_point()

#Box plot
ggplot(data=update_1, aes(x=Treatment, y=coop)) + geom_boxplot()

# Non-parametric test to see if two sets of data are probably different
wilcox.test(subset(update_1, treatment==1.0)$coop, subset(update_1, treatment=0.5)$coop)

data1 <- read.csv()

seed <- c(1 , 2,  3 , 4 , 5 , 6)
mut_malig <- c(0.0, 0.25, 0.5, 0.75, 0.9)
mut_benig <- c(0.0, 0.25, 0.5, 0.75, 0.9)
p_53 <- c(0.0, 0.05, 0.06, 0.08, 0.13, 0.15, 0.2)


files <- list.files(pattern = "malig0.25benig0.25p530.05.dat")


data_mut_wild_stuff = rbind(read.csv(files[1]),read.csv(files[2]),read.csv(files[3]),read.csv(files[4]),read.csv(files[5]),read.csv(files[6]))
ggplot(data=data_mut_wild_stuff, aes(x=update, y=mean_p53)) +ylab("mean_p53") + xlab("update") + geom_point() + ggtitle("malig0.25 benig0.25 p530.05")

