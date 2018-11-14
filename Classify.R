OIR1 <- training_D2[which(training_D2$preOIR>=1.871),]
OIR2 <- training_D2[which(training_D2$preOIR<1.871),]
score1 <- OIR1[which(OIR1$score<729.5),]
score2 <- OIR1[which(OIR1$score>=729.5),]
score3 <- OIR2[which(OIR2$score<695.5),]
score4 <- OIR2[which(OIR2$score>=695.5),]
prop1 <- score1[which(score1$prop>=-0.1046),]
prop2 <- score1[which(score1$prop< -0.1046),]

OIRt1 <- testing_D2[which(testing_D2$preOIR>=1.871),]
OIRt2 <- testing_D2[which(testing_D2$preOIR<1.871),]
scoret1 <- OIRt1[which(OIRt1$score<729.5),]
scoret2 <- OIRt1[which(OIRt1$score>=729.5),]
scoret3 <- OIRt2[which(OIRt2$score<695.5),]
scoret4 <- OIRt2[which(OIRt2$score>=695.5),]
propt1 <- scoret1[which(scoret1$prop>=-0.1046),]
propt2 <- scoret1[which(scoret1$prop< -0.1046),]

testing2_D2 <- rbind(propt1, propt2, scoret2, scoret3, scoret4)
  #merge(scoret2, scoret3) 
  #c(propt1$def_in_24_months_F, propt2$def_in_24_months_F, scoret2$def_in_24_months_F, scoret3$def_in_24_months_F, scoret4$def_in_24_months_F)

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



