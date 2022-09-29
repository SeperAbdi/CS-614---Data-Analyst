library(magrittr)
library(finalfit)
library (caret)
library(ggplot2)
library(dplyr)
library(naniar)
library(tidyverse) 
library(mice)

load("HW3.rdata")
summary(df)
glimpse(df)

#df$Adult = as.factor(df$Adult)
#df$Smile = as.factor(df$Smile)
#df = df %>% select(Gender, Image_Quality, Race, Smile, Area)
md.pattern(df)

library(VIM)
aggr_plot = aggr(df, col = c ("navyblue","red"), numbers = TRUE,
                 sortVars = TRUE, lables = names(df), 
                 cex.axis = .7, gap = 3, 
                ylab = c ("Histogram of missing data", "pattern"))

mean(df$Area, na.rm = TRUE)

df%>%
  select(Gender,Image_Quality, Race, Smile, Area)%>%
  filter(!complete.cases(.))


#mice imputation

df_imp <- mice(df, m = 3, method = "rf")
summary(df_imp)
  
df_imp$imp$Area
fini_df_imp <- complete(df_imp,2)
sapply(fini_df_imp, function(x)sum (is.na(x)))  
  

pairs(fini_df_imp)


# 5. Create figures similar to Fig 5 in the Nguyen article


train.index <- createDataPartition(fini_df_imp[,"Area"],p=0.05,list=FALSE)
fini_df_imp.trn <- fini_df_imp[train.index,]
fini_df_imp.tst <- fini_df_imp[-train.index,]

ctrl  <- trainControl(method  = "cv",number  = 10) # summaryFunction = multiClassSummary

fit.cv <- train(Area ~ ., data = fini_df_imp.trn, method = "knn",
  trControl = ctrl, 
  preProcess = c("center","scale"), 
  #tuneGrid =data.frame(k = 10))
  tuneLength = 50, matric ="kappa")

pred <- predict(fit.cv,fini_df_imp.tst)
#pred = as.factor(pred)

confusionMatrix(pred,as.factor(fini_df_imp.tst$Area))
print(fit.cv)
plot(fit.cv)

table(fini_df_imp.tst[,"Adult"],pred )
