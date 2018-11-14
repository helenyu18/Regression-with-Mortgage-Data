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

