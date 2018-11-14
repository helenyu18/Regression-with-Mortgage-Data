# Wei Yu
# CID: 00823764



D2$first.time.homebuyer_Y <- as.numeric(D2$first.time.homebuyer=="Y")
D2$occupancy.status_I <- as.numeric(D2$occupancy.status=="I")
D2$occupancy.status_S <- as.numeric(D2$occupancy.status=="S")
D2$channel_B <- 1-as.numeric(D2$channel=="B")
D2$channel_C <- 1-as.numeric(D2$channel=="C")
D2$channel_T <- 1-as.numeric(D2$channel=="T")
D2$loan.purpose_C <- as.numeric(D2$loan.purpose=="C")
D2$loan.purpose_P <- as.numeric(D2$loan.purpose=="P")
D2$def_in_24_months_F <- 1-as.numeric(D2$def_in_24_months)

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

predata <- sample(nrow(D2),nrow(D2)*3/4,replace=FALSE)
training_D2 <- D2[predata,]
testing_D2 <- D2[-predata,]

glm1.out <- 
  with(training_D2, 
       glm(def_in_24_months_F ~ 
             score + first.time.homebuyer + insurance + 
             number.units + occupancy.status + DTI + LTV + log(OIR) +   
             loan.purpose + orig.loan.term + prop + proptype + serv,
           family = binomial("logit")
       )
  )

1-pchisq(2*((-glm1.out$deviance)-(-glm1.out$null)), 4)
yp <- predict(glm1.out, testing_D2, type="response")
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
roc(testing_D2$def_in_24_months_F, yp)

