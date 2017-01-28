######################################### Problem 1.1###################################################


##### Load Boston Data #####


data("Boston")

#need as.factor for chas?

##### Partition Train/Test #####


index <- sample(1:nrow(Boston),nrow(Boston)*.7)

train <- Boston[index,]

test <- Boston[-index,]


##### Summary Stats #####


str(Boston)
summary(Boston) #no NA values



##### EDA #####

#Scatterplot Matrix and Density Plots

ggpairs(Boston) # some variable have clear correlation, some predictors 
                # have clear relationship with medv, there are outliers present


#prep for correlation matrix

num <- sapply(train, is.numeric)

df_cor <- train[,num]

zv <- apply(df_cor, 2, function(x) length(unique(x)) <= 2)

sum(zv)
zv

df_cor <- df_cor[, !zv]

corr <- cor(df_cor,use = "pairwise.complete.obs")

highCorr <- findCorrelation(corr, 0.70)

length(highCorr) # 5 variables with >.7 correlation

colnames(corr[, highCorr,drop = FALSE]) 

ggcorr(df_cor, method = c("complete", "pearson"), nbreaks = 10) #could also use pairwise, which is the default

# loop of boxplot based on example here: https://www.r-bloggers.com/ggplot2-graphics-in-a-loop/
# need to fix axis titles and scale issues from outliers
boxplot.loop <- function(x, na.rm = TRUE, ...) {
  numer <- sapply(x, is.numeric)
  nm <- names(x[,numer])
  for (i in seq_along(nm)) {
   plots <- ggplot(data = x,aes(x = factor(0), y = x[,nm[i]])) + geom_boxplot()
   ggsave(plots,filename=paste("boxplot",nm[i],".png",sep="_"))
  }
}

boxplot.loop(train) 


#ggplot(data = train, aes(x = factor(0), y= crim)) + geom_boxplot() + coord_flip()

##### Model with no variable manipulation #####

linreg.model <- lm(medv ~ ., data = train)

summary(linreg.model)

#significant variables
  # crim         
  # zn          
  # chas          
  # nox       
  # rm            
  # dis          
  # rad         
  # tax          
  # ptratio      
  # black        
  # lstat        

# could standardize variable if inference from coefficients is desired,
# would have to address multicollinearity also

  # for (i in 1:(ncol(Boston_train) - 1)) {
  # Boston_train[, i] = scale(Boston_train[, i])
  # }

# ways to add interaction effects
  #lm(medv ~ crim + zn + crim:zn, data = Boston_train)
  #lm(medv ~ crim * zn, data = Boston_train)

# Model Fitness

# MSE
model_summary <- summary(linreg.model)
(model_summary$sigma)^2

# R-squared 

model_summary$r.squared

# Adjusted R-squared  (penalizes model complexity)
model_summary$adj.r.squared

# AIC and BIC of the model, these are information criteria. Smaller values indicate better fit.

AIC(linreg.model)

BIC(linreg.model)

# Out of sample prediction

# pi is a vector that contains predicted values for test set.

pi <- predict(object = linreg.model, newdata = test)


# Mean Squared Error (MSE): average of the squared differences between the predicted and actual values

mean((pi - test$medv)^2)


# Mean Absolute Error (MAE) (less popular)
mean(abs(pi - test$medv))


##### variable selection #####




###################################### Problem 1.2 ################################################
