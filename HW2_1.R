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

boxplot.loop <- function(x, na.rm = TRUE, ...) {
  numer <- sapply(x, is.numeric)
  nm <- names(x[,numer])
  for (i in seq_along(nm)) {
    plots <-ggplot(data = x,aes(x = factor(0), y = nm[i])) + geom_boxplot()
    ggsave(plots,filename=paste("boxplot",nm[i],".png",sep="_"))
  }
}

boxplot.loop(train) 


ggplot(data = train, aes(x = factor(0), y= nox)) + geom_boxplot() + coord_flip()


###################################### Problem 1.2 ################################################
