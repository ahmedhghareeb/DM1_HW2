##### function takes x = matrix from HW2_2.R, name = name of metric being observed in graph for title and axis lablel #####

line.plot <- function(x, name) {
  
  df <- as.data.frame(x)
  df<-melt(df)
  df$n <- c(25, 100, 200, 500, 5000)
  df$n <- as.factor(df$n)
  df$sigma <- str_sub(df$variable, start= -2)
  df$sigma <- as.numeric(df$sigma)
  
  
  sig.plot <-ggplot(df, aes(x=sigma, y= value, group = n, colour=n))+geom_line()+ggtitle(paste(name,"vs. noise"))+ labs(y= name)
  ggsave(sig.plot,filename=paste(name,"noise",".png",sep="_"))
  
  df$sigma <- as.factor(df$sigma)
  
  n.plot <- ggplot(df, aes(x=n, y= value , group = sigma, colour=sigma))+geom_line()+ggtitle(paste(name,"vs. n"))+ labs(y= name)
  ggsave(n.plot,filename=paste(name,"n",".png",sep="_"))
}


