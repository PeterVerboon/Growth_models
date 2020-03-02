
require (userfriendlyscience)
require(ggplot2)
require(tidyr)
require(traj)
require(lme4)
require(lmerTest)


dat <- getData()

names(dat)

dat1 <- dat[,c("LongID","Gender")]
dat1$W5 <- (dat$LSDQtotaal_W5 )
dat1$W7 <- dat$LACAtotaal_W7 
dat1$W9 <- dat$LACAtotaal_W9

dat1$Wxx <- dat1$W9 + (rnorm(129,-0.2,.1))     # create fourth time point (random)

examine(dat1[,6])

## Step 1 Plot the data

## Reshape data from wide fromat to long format

dat2 <- gather(dat1, key= wave, value=loneliness, W5:Wxx, factor_key=TRUE)
dat2$waven <- as.numeric(dat2$wave)


## make plot of data points

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

p <- ggplot(dat=dat2, aes(x=wave, y=loneliness)) 
p + geom_point()
p + geom_boxplot()
p + geom_boxplot(aes(color = Gender))


p2 <- ggplot(dat=dat2, aes(x=waven, y=loneliness)) 
p2 + geom_point() + geom_smooth(method="lm") 

dat3 <- summarySE(dat2,measurevar="loneliness",groupvars=c("waven","Gender"), na.rm=TRUE )
dat4 <- summarySE(dat2,measurevar="loneliness",groupvars=c("wave","Gender"), na.rm=TRUE )

p3 <- ggplot(dat=dat3, aes(x=waven, y=loneliness, color=Gender)) 
p4 <- ggplot(dat=dat4, aes(x=wave, y=loneliness, color=Gender)) 

# Use 95% confidence interval 
p3 + geom_errorbar(aes(ymin=loneliness-ci, ymax=loneliness+ci), width=.1, position=pd) + 
  geom_line(position=pd) + geom_point(position=pd)
 
p4 + geom_errorbar(aes(ymin=loneliness-ci, ymax=loneliness+ci), width=.1, position=pd) + 
  geom_line(aes(group=Gender), position=pd) + geom_point(position=pd)


# lines per person

p <- ggplot(dat=dat2, aes(x=wave, y=loneliness)) 
p + geom_line(aes(group=LongID, color=Gender))


# Step 2 Fitting trajectories
# First construct the correct data frames for the analyses in Traj packages 
# There are minimal 4 time points necessary!!!


datw <- dat1[,c(1,3,4,5,6)]
#datw <- as.data.frame(a <- as.matrix(datw))
colnames(datw) <- c("ID","T1","T2","T3","T4")


dattime <- as.data.frame(cbind(datw[,1], matrix(rep(c(1:4),129), nrow=129, byrow = TRUE)))
colnames(dattime) <- c("ID","t1","t2", "t3","t4")

datlist <- list(data=datw,time=dattime)

s1 = step1measures(datlist$data, datlist$time, ID = TRUE)

s1$measurments

s2 = step2factors(s1)

print(s2$princ.fac$loadings)
print(s2$princ.fac$communality)
print(s2$princ.fac$values)
print(s2$princ.fac$fit)

s3 = step3clusters(s2, nclusters = 3)

s3$clust.distr

plot(s3)
plotMeanTraj(s3)
plotMedTraj(s3)
plotBoxplotTraj(s3)
plotCombTraj(s3)
s3

# Step 3 Fitting single traject with random variation


model <- lmer(loneliness ~ 1 + (1|wave), dat=dat2)
model <- lmer(loneliness ~ waven*Gender + (waven|LongID), dat=dat2)

summary(model)

