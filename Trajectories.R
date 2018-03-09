
require (userfriendlyscience)
require(ggplot2)
require(tidyr)
require(traj)


dat <- getData()

names(dat)

dat1 <- dat[,c("LongID","Gender")]
dat1$lonelinessW5 <- (dat$LSDQtotaal_W5 )
dat1$lonelinessW7 <- dat$LACAtotaal_W7 
dat1$lonelinessW9<- dat$LACAtotaal_W9

dat1$lonelinessWxx <- dat1$lonelinessW9 + (rnorm(129,-0.2,.1))     # create fourth time point (random)

examine(dat1[,6])

## Step 1 Plot the data

## Reshape data from wide fromat to long format

dat2 <- gather(dat1, key= wave, value=loneliness, lonelinessW5:lonelinessWxx, factor_key=TRUE)
dat2$waven <- as.numeric(dat2$wave)
p <- ggplot(dat=dat2) + aes(x=waven, y=loneliness) + geom_point() + stat_summary(fun.y=mean_cl_normal,geom="line",lwd=1,aes(group=1),conf.int=0.95, fill="lightblue")
p

# construct the correct data frames for the analyses 
# there are 4 time points necessary!!!

head(example.data$data)
head(example.data$time)

head(datlist$data)
head(datlist$time)

str(example.data)
str(datlist)

datw <- data_wide[,c(1,12,13,14)]
datw <- as.data.frame(a <- as.matrix(datw))
colnames(datw) <- c("ID","T1","T2", "T3")

datw$T0 <- datw$T3 + (rnorm(129,0,.1))  # create fourth time point (random)

dattime <- as.data.frame(cbind(datw[,1], matrix(rep(c(1:3),129), nrow=129, byrow = TRUE)))
colnames(dattime) <- c("ID","t1","t2", "t3")
dattime$t0 <- 4

datlist <- list(data=datw,time=dattime)

s1 = step1measures(datlist$data, datlist$time, ID = TRUE)

s1$measurments

s2 = step2factors(s1)

print(s2$princ.fac$loadings)
print(s2$princ.fac$communality)
print(s2$princ.fac$values)
print(s2$princ.fac$fit)

s3 = step3clusters(s2, nclusters = 1)

s3$clust.distr

plot(s3)
plotMeanTraj(s3)
plotMedTraj(s3)
plotBoxplotTraj(s3)
plotCombTraj(s3)
s3
