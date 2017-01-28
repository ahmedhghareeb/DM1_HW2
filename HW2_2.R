# Simulation of Linear Regression varying n and noise (e)

set.seed(1984)

n=c(25, 100, 200, 500, 5000)

sigmasq= c(0.1, 0.5, 1)

n <- 200

sigmasq <- 1

x1 <- rnorm(n=n, mean = 2, sd = 0.4)

x2 <- rnorm(n=n, mean = -1, sd = 0.1)

x3 <- x1 * x2

e <- rnorm(n=n, mean = 0, sd = sigmasq)

eyx <- 5 + 1.2*x1 + 3*x2 + e

model.df <- as.data.frame(cbind(eyx,x1,x2,x3))

# set null and full model for stepwise regression
nullmodel <- lm(eyx ~ 1, data = model.df)
fullmodel <- lm(eyx ~ ., data = model.df)

# stepwise regression
model.stepwise = step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), 
                      direction = "both")



lm.out = #fit linear regression
  betaMatrix[j,]= lm.out$coefficients
mysummary = summary(lm.out)
listMSE[j] = mysummary$sigma^2 #get MSE per iteration