
require(userfriendlyscience)
require(flexmix)
require(poLCA)
require(mclust)


m1 = flexmix(yn ~ x + I(x^2), data = datw, k = 2)


data("NPreg", package = "flexmix")


dat1 <- na.omit(dat)
dat1 <- na.omit(data_wide)

m1 = flexmix( SCL_W10_anxiety ~ Zw5 + Zw7 + Zw9*Gender , data = dat1, k = 2)

summary(m1)

parameters(m1, component = 1)

parameters(m1, component = 2)

plot(m1)

rm1 <- refit(m1)
summary(rm1)

table(dat1$Gender, m1@cluster)

dat1 <- na.omit(data_long)
m4 = flexmix(formula = SCL_W10_anxiety ~ measurement*Gender | time, data = dat1, k = 2)
summary(m1)

parameters(m4, component = 1)
parameters(m4, component = 2)
rm4 <- refit(m4); summary(rm4)
plot(m4)



#######################

M0 <- poLCA(f,values,data=dat1,nclass=1) 


f <- cbind(SCL_W10_anxiety, SCL_W10_depressie,SCL_W10_overig) ~ 1

M0 <- poLCA(f,data=dat1,nclass=2) 

##########################

dat1 <- na.omit(data_wide[,c(2:11)])
m1 <- Mclust(data=dat1[,-1], G=3)

summary(m1)
class <- as.numeric(m1$classification)
m1$classification

clPairs(dat1[,c(2,3,4)], class)

BIC = mclustBIC(dat1[,-1])
plot(BIC)

mod1 = Mclust(dat1[,-1], x = BIC, G=3)

summary(mod1, parameters = TRUE)

plot(mod1, what = "classification")

table(class, mod1$classification)

par(mfrow = c(2,2))
plot(mod1, what = "uncertainty", dimens = c(2,1), main = "")
plot(mod1, what = "uncertainty", dimens = c(3,1), main = "")
plot(mod1, what = "uncertainty", dimens = c(2,3), main = "")
par(mfrow = c(1,1))

class[c(1,2,3,4)] <- 3
mod2 = MclustDA(dat1[,-1], class, modelType = "EDDA")
summary(mod2)

plot(mod2, what = "scatterplot")

plot(mod2, what = "classification")

mod3 = MclustDA(dat1[,-1], class)
summary(mod3)
plot(mod3, what = "scatterplot")

unlist(cvMclustDA(mod3, nfold = 10)[2:3])
unlist(cvMclustDA(mod2, nfold = 10)[2:3])

mod4 = densityMclust(dat1[,-1])
summary(mod4)
plot(mod4, what = "BIC")
plot(mod4, what = "density", data = dat1[,2], breaks = 15)
plot(mod4, what = "diagnostic", type = "cdf")


mod1dr = MclustDR(mod1, lambda = 1)
summary(mod1dr)