# Use stepwise selection
library(MASS)

fit <- lm(def_in_24_months_F ~ score + first.time.homebuyer + insurance + 
            number.units + occupancy.status + DTI   + log(OIR) +LTV +   
            loan.purpose + orig.loan.term + prop +proptype + serv, 
            data=training_D2)
step <- stepAIC(fit, direction="forward")
step$anova



# Build a regression tree
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
