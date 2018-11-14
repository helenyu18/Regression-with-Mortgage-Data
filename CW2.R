# Wei Yu
# CID: 00823764


# As we know, part I has randomly split the data into training and testing set.
# In order to reduce the error in final improvements, I used the same data set 
# as part I, so the process of transforming and splitting data are all in part I. 
# I have pasted them here, as a reference.


load("/Users/helenyu/Desktop/M3S16/S16CW1/US mortgage sample 4.RData")

# Add some necessary columns for specific variables
D2$first.time.homebuyer_Y <- as.numeric(D2$first.time.homebuyer=="Y")
D2$occupancy.status_I <- as.numeric(D2$occupancy.status=="I")
D2$occupancy.status_S <- as.numeric(D2$occupancy.status=="S")
D2$channel_B <- 1-as.numeric(D2$channel=="B")
D2$channel_C <- 1-as.numeric(D2$channel=="C")
D2$channel_T <- 1-as.numeric(D2$channel=="T")
D2$loan.purpose_C <- as.numeric(D2$loan.purpose=="C")
D2$loan.purpose_P <- as.numeric(D2$loan.purpose=="P")
D2$def_in_24_months_F <- 1-as.numeric(D2$def_in_24_months)

# Apply WOE function to three specific variables
woe <- function(x,y) {
  n1 <- sum(y)
  n0 <- sum(1-y)
  nx0n1 <- tapply(1-y,x,sum)*n1
  nx1n0 <- tapply(y,x,sum)  *n0
  nx0n1[which(nx0n1==0)]<-n1
  nx1n0[which(nx1n0==0)]<-n0
  wtab <- log(nx0n1)-log(nx1n0)
  w<-rep(0,length(y))
  ni<-names(wtab)
  for (ix in 1:length(ni)) {
    w[which(x==ni[ix])]<-wtab[ix]
  }
  w 
}

servicer_woe <- woe(D2$servicer, D2$def_in_24_months_F)
property.state_woe <- woe(D2$property.state, D2$def_in_24_months_F)
property.type_woe <- woe(D2$property.type, D2$def_in_24_months_F)

D2$preOIR <- log(D2$"OIR")
D2$prop <- woe(D2$property.state, D2$def_in_24_months_F)
D2$proptype <- woe(D2$property.type, D2$def_in_24_months_F)
D2$serv <- woe(D2$servicer, D2$def_in_24_months_F)

# Split data
predata <- sample(nrow(D2),nrow(D2)*3/4,replace=FALSE)
training_D2 <- D2[predata,]
testing_D2 <- D2[-predata,]


# Delete some NA's data in the original data set
D2 <- D2[!is.na(D2$property.state),]
D2 <- D2[!is.na(D2$score),]
training_D2 <- training_D2[!is.na(training_D2$property.state),]
training_D2 <- training_D2[!is.na(training_D2$score),]
testing_D2 <- testing_D2[!is.na(testing_D2$property.state),]
testing_D2 <- testing_D2[!is.na(testing_D2$score),]

# Install MASS package and then use stepwise selection
library(MASS)
fit <- lm(def_in_24_months_F ~ score + first.time.homebuyer + insurance + 
            number.units + occupancy.status + DTI   + log(OIR) +LTV +   
            loan.purpose + orig.loan.term + prop +proptype + serv, 
          data=training_D2)
step <- stepAIC(fit, direction="backward")
step$anova

# Install rpart package and build a regression tree
library(rpart)
training_D2$def_in_24_months_F <- 1-as.numeric(training_D2$def_in_24_months)
fit <- rpart(def_in_24_months_F ~ score + first.time.homebuyer + insurance + 
               number.units + occupancy.status + DTI   + log(OIR) +LTV +   
               loan.purpose + orig.loan.term + prop +proptype + serv, 
             method="anova", data=training_D2)

printcp(fit)
plotcp(fit) 
summary(fit) 
pfit<- prune(fit, cp=0.01160389) # from cptable   
plot(pfit, uniform=TRUE, 
     main="Regression Tree for Data")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "~/Desktop/M3S16/S16CW1/ptree2.ps", 
     title = "Regression Tree for Data")

# Segmentation process regarding to certain criteria
# Training set segmentation
OIR1 <- training_D2[which(training_D2$preOIR>=1.871),]
OIR2 <- training_D2[which(training_D2$preOIR<1.871),]
score1 <- OIR1[which(OIR1$score<733.5),]
score2 <- OIR1[which(OIR1$score>=733.5),]
score3 <- OIR2[which(OIR2$score<696.5),]
score4 <- OIR2[which(OIR2$score>=696.5),]
prop1 <- score1[which(score1$prop>=-0.106),]
prop2 <- score1[which(score1$prop< -0.106),]

# Testing set segmentation
OIRt1 <- testing_D2[which(testing_D2$preOIR>=1.871),]
OIRt2 <- testing_D2[which(testing_D2$preOIR<1.871),]
scoret1 <- OIRt1[which(OIRt1$score<733.5),]
scoret2 <- OIRt1[which(OIRt1$score>=733.5),]
scoret3 <- OIRt2[which(OIRt2$score<696.5),]
scoret4 <- OIRt2[which(OIRt2$score>=696.5),]
propt1 <- scoret1[which(scoret1$prop>=-0.106),]
propt2 <- scoret1[which(scoret1$prop< -0.106),]

# Merge results
testing2_D2 <- rbind(propt1, propt2, scoret2, scoret3, scoret4)

glm11.out <- with(prop1, glm(def_in_24_months_F ~ score + first.time.homebuyer + 
                               insurance + number.units + occupancy.status + DTI + 
                               LTV + log(OIR) + loan.purpose + orig.loan.term + 
                               prop + proptype + serv,
                             family = binomial("logit")))
glm12.out <- with(prop2, glm(def_in_24_months_F ~ score + first.time.homebuyer + 
                               insurance + number.units + occupancy.status + DTI + 
                               LTV + log(OIR) + loan.purpose + orig.loan.term + 
                               prop + proptype + serv,
                             family = binomial("logit")))
glm13.out <- with(score2, glm(def_in_24_months_F ~ score + first.time.homebuyer + 
                                insurance + number.units + occupancy.status + DTI + 
                                LTV + log(OIR) + loan.purpose + orig.loan.term + 
                                prop + proptype + serv,
                              family = binomial("logit")))
glm14.out <- with(score3, glm(def_in_24_months_F ~ score + first.time.homebuyer + 
                                insurance + number.units + occupancy.status + DTI + 
                                LTV + log(OIR) + loan.purpose + orig.loan.term + 
                                prop + proptype + serv,
                              family = binomial("logit")))
glm15.out <- with(score4, glm(def_in_24_months_F ~ score + first.time.homebuyer + 
                                insurance + number.units + occupancy.status + DTI + 
                                LTV + log(OIR) + loan.purpose + orig.loan.term + 
                                prop + proptype + serv,
                              family = binomial("logit")))
glm.out <- c(glm11.out, glm12.out, glm13.out, glm14.out, glm15.out)

yp1 <- predict(glm11.out, propt1, type="response")
yp2 <- predict(glm12.out, propt2, type="response")
yp3 <- predict(glm13.out, scoret2, type="response")
yp4 <- predict(glm14.out, scoret3, type="response")
yp5 <- predict(glm15.out, scoret4, type="response")
yp <- c(yp1, yp2, yp3, yp4, yp5)

# Build ROC Curve and deduce AUC
roc <- function(y, s)
{
  yav <- rep(tapply(y, s, mean), table(s))
  rocx <- cumsum(yav)
  rocy <- cumsum(1 - yav)
  area <- sum(yav * (rocy - 0.5 * (1 - yav)))
  x1 <- c(0, rocx)/sum(y)
  y1 <- c(0, rocy)/sum(1 - y)
  auc <- area/(sum(y) * sum(1 - y))
  print(auc)
  plot(x1,y1,"l")
}

roc(testing2_D2$def_in_24_months_F, yp)

# End of code
