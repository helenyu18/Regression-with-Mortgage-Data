library(rpart)
training_D2$def_in_24_months_F <- 1-as.numeric(training_D2$def_in_24_months)
fit <- rpart(def_in_24_months_F ~ score + first.time.homebuyer + insurance + 
               number.units + occupancy.status + DTI + LTV + log(OIR) +   
               loan.purpose + orig.loan.term + property.state_woe +
               property.type_woe + servicer_woe,
             family = binomial("logit"), 
             method="anova", data=D2)
printcp(fit)
plotcp(fit) 
summary(fit) 
plot(fit, uniform=TRUE, 
     main="Regression Tree for def_in_24_months_F ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
