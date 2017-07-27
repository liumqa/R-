# R-
R study
d <- diamonds[sample(nrow(diamonds), 1000), ]
?diamonds

plot_ly(d, x = ~carat, y = ~price, color = ~carat,size = ~carat, text = ~paste("Clarity: ", clarity))
table(d$cut,d$clarity)
library(dplyr)
count(d$cut,d$clarity)
count(d$cut)
