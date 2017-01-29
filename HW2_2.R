# Simulation of Linear Regression varying n and noise (e)

# sample size
n=c(25, 100, 200, 500, 5000)

# noise
sigma= c(0.1, 0.5, 1)

# initialize matrices 
MSE.matrix <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5)
colnames(MSE.matrix) <- c("sig = .1","sig = .5","sig = 1")
rownames(MSE.matrix) <- c("n = 25", "n = 100", "n = 200", "n = 500", "n = 5000")

rsq.matrix <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5)
colnames(rsq.matrix) <- c("sig = .1","sig = .5","sig = 1")
rownames(rsq.matrix) <- c("n = 25", "n = 100", "n = 200", "n = 500", "n = 5000")

adj.rsq.matrix <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 5)
colnames(adj.rsq.matrix) <- c("sig = .1","sig = .5","sig = 1")
rownames(adj.rsq.matrix) <- c("n = 25", "n = 100", "n = 200", "n = 500", "n = 5000")

for (i in 1:length(n)){
  
  
  for (j in 1:length(sigma)){
    set.seed(1984)
    # generate predictors
    x1 <- rnorm(n=n[i], mean = 2, sd = 0.4)
    x2 <- rnorm(n=n[i], mean = -1, sd = 0.1)
    x3 <- x1 * x2
    
    # generate error
    e <- rnorm(n=n[i], mean = 0, sd = sigma[j])
    
    eyx <- 5 + 1.2*x1 + 3*x2 + e
  
    # create data from for model
    model.df <- as.data.frame(cbind(eyx,x1,x2,x3))
    
    # set null and full model for stepwise regression
    nullmodel <- lm(eyx ~ 1, data = model.df)
    fullmodel <- lm(eyx ~ ., data = model.df)
    
    # stepwise regression
    print(paste("**************** n = ", n[i]," sig = ",sigma[j], " **********************", sep = ""))
    
    model.stepwise = step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), 
                          direction = "both")
    summary.model <- summary(model.stepwise)

  
    MSE.matrix[i,j] <- round(summary.model$sigma^2, 4)
    rsq.matrix[i,j] <- round(summary.model$r.squared, 4)
    adj.rsq.matrix[i,j] <- round(summary.model$adj.r.squared, 4)

  }
}

# print matrices
print(MSE.matrix)
print(rsq.matrix)
print(adj.rsq.matrix)

# user defined function line.plot from plot_func.R - creates line graphs with ggplot2

line.plot(adj.rsq.matrix,"MSE")
line.plot(adj.rsq.matrix,"R-Squared")
line.plot(adj.rsq.matrix,"Adjusted R-Squared")



