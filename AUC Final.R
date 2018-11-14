glm.out <- c(glm11.out, glm12.out, glm13.out, glm14.out, glm15.out)
yp1 <- predict(glm11.out, propt1, type="response")
yp2 <- predict(glm12.out, propt2, type="response")
yp3 <- predict(glm13.out, scoret2, type="response")
yp4 <- predict(glm14.out, scoret3, type="response")
yp5 <- predict(glm15.out, scoret4, type="response")
yp <- c(yp1, yp2, yp3, yp4, yp5)


#yp <- predict(glm.out, testing2_D2, type="response")
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
