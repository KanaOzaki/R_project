install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggsci")
install.packages("reshape2")
library(ggplot2)
library(dplyr)
library(ggsci)
library(reshape2)

### sample data------------
x    <- c(1,2,3,4,5)
y1 <- c(158,162,177,173,166)
y2 <- c(155,155,172,157,164)

data <- data.frame(x=x, y1=y1, y2=y2)

### initialize ggplot()-----------
ggplot()+theme_set(theme_bw(base_size = 14))

### function : plot_Line---------
plot_Line <- function(x, y1, y2){
  # x : x axis
  # y1 : 
  data_1 <- data.frame(x=x, data = "y1", y=y1)
  data_2 <- data.frame(x=x, data = "y2", y=y2)
  
  data <- rbind(data_1, data_2)
  
  g <- ggplot(data, aes(x = x, y = y, color = data))
  g <- g + geom_line()
  g <- g + geom_point(aes(colour=data),size=1,alpha=0.5)
  #g <- g + scale_color_manual(values = c("#202020", "#808080"))
  g <- g + scale_color_grey(start = 0.2, end = 0.8)
  g
}

plot_Line(data$x, data$y1, data$y2)

### function : plot_Bar---------
plot_Bar <- function(x, y1, y2){
  
  data_1 <- data.frame(x=x, data = "y1", y=y1)
  data_2 <- data.frame(x=x, data = "y2", y=y2)
  
  data <- rbind(data_1, data_2)
  
  g <- ggplot(data, aes(x = x, y = y, fill = data))
  g <- g + geom_bar(stat = "identity", position = "dodge")
  #g <- g + scale_fill_manual(values = c("#202020", "#808080"))
  g <- g + scale_fill_grey(start = 0.2, end = 0.6)
  g
}

plot_Bar(data$x, data$y1, data$y2)
