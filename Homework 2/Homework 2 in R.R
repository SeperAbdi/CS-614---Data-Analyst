countrycharsA_1 <- read.csv("countrycharsA-1.csv")
countrycharsB_1 <- read.csv("countrycharsB-1.csv")
gdb_1 <- read.csv("gdp-1.csv")
#1
str(countrycharsB_1)
#append ccA to ccB to make one large data frame
combineddataset <- rbind(countrycharsA_1, countrycharsB_1)

str(combineddataset)
-------------------------------
#2
combineddataset2 <- cbind(combineddataset, gdb_1) 
str(combineddataset2)
View(combineddataset2)
-------------------------------------
#3 Sorting by gdp
srot_by_gdp <- (combineddataset2[order(combineddataset2$gdp, decreasing = FALSE), ]) 
identify_rows <-(combineddataset2 $ gdp >= 20000)
gdp_over_20k <- combineddataset2[identify_rows, ]
gdp_over_20k
--------------------------------------------
#4 inner right  and left merge  
left_merg = merge(x=combineddataset,y=combineddataset2,by = "continent",all.x=TRUE) # run it in the command then it will work 
left_merg

#install.packages("dplyr")
#library(dplyr)

inner_right= combineddataset %>% inner_join(combineddataset2,by="lifeExp")
inner_right
--------------------------------------------  
#5 - 1 quintile()
gdp.q <- quantile(inner_right$gdp)
gdp.q
#5-2 
  
findInterval(x, vec, rightmost.closed = FALSE, all.inside = FALSE,left.open = FALSE)

data1 <- data.frame(combineddataset, lower = findInterval(combineddataset2, vec = NULL))
data1

#5-3 
mean_1 <- mean(combineddataset2$ gdp, na.rm = TRUE)
mean_1
sd_1 <-sd(combineddataset2$ gdp, na.rm = TRUE)
sd_1

  