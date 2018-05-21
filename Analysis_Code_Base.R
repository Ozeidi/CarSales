#### SET ENVIRONMENT ####
library(tidyverse)
library(broom)
options(scipen=999)
setwd('C:\\Users\\Paulo-ASUS\\Documents\\Units\\SEMESTER 3\\FIT5125\\miniproject')

#### DATA CLEANING ####
df <- read.csv('dbcars.txt', sep = '\t')
df <- df %>% 
  # exclude missing values for the regressors
  filter(
    !is.na(Make), Make != '',
    !is.na(Model), Model != '',
    !is.na(BodyType), BodyType != '',
    !is.na(Trans), Trans != '',
    !is.na(AC), AC != '',
    !is.na(Fuel), Fuel != ''
  ) %>% 
  # exclude groups with < 10 obs
  group_by(Make, Model, BodyType, Trans, AC, Fuel) %>% 
  filter(n() > 10) %>% 
  ungroup()


#### REGRESSION (PART 1) ####
predictors <- c('Make', 'Model', 'BodyType', 'Trans', 'AC', 'Fuel', 'Km', 'State')
response <- 'Price'

data <- df %>% select(c(predictors, response))
data <- na.omit(data)
data <- data[-1,] # to make divisible by 10

formula <- as.formula(paste0(response, '~', paste0(predictors, collapse = '+')))

idx <- sample(1:nrow(data), nrow(data), replace = FALSE)
part.size <- nrow(data)/10 #
models <- list()
errors <- matrix(1:10, ncol=1)

for (i in 1:10) {
  print(paste0("Fold ", i))
  test.idx <- idx[seq((i-1)*part.size + 1,i*part.size)]
  test <- data[test.idx,]
  train <- data[-test.idx,]
  
  models[[i]] <- lm(formula, data=train)     # full model
  
  errors[i, 1] <- mean((test$Price - predict.lm(models[[i]], test))^2)
}

# plot test errors
errors.gg.1 <- ggplot(as.data.frame(cbind(1:10, errors)), aes(y=V2, x=V1))+
  geom_point() +
  scale_x_continuous('Fold Number') +
  scale_y_continuous('Test Error') +
  ggtitle('Model Selection') +
  theme_minimal()

# model selection
best.model <- which.min(errors[,1])
model.1 <- models[[best.model]]
sum.1 <- tidy(summary(model.1))

# filter relevant features
details.1 <- sum %>% filter(p.value < 0.05) %>% arrange(desc(abs(estimate))) %>% filter(!grepl('Model|Make', term))



###### REGRESSION (PART 2)
model <- readRDS('best_mode_1.rds')
# outlier detection
out.idx <- tidy(model$residuals) %>% filter(abs(x) > 60000) %>% select(names) %>% as.vector()

# tracing back to training set
test.out.idx <- idx[seq((best.model-1)*part.size + 1,best.model*part.size)]
train.out <- data[-test.out.idx,]
train.out <- train.out[out.idx$names,]
pred <- predict.lm(model, train.out)

# compare predictions with actual values
train.out <- cbind(train.out, pred) %>% mutate(pred_ratio = pred/Price)
train.out %>% write.csv('outliers.csv')

# tracing back to original dataset
col.names <- colnames(train.out)[2:10]
data.2 <- data %>% 
  left_join(train.out, by=col.names) %>% 
  filter(is.na(pred))

# running the models again
idx <- sample(1:nrow(data.2), nrow(data.2), replace = FALSE)
part.size <- nrow(data.2)/10
models.2 <- list()
errors.2 <- matrix(1:10, ncol=1)

for (i in 1:10) {
  print(paste0("Fold ", i))
  test.idx <- idx[seq((i-1)*part.size + 1,i*part.size)]
  test <- data.2[test.idx,]
  train <- data.2[-test.idx,]
  
  models.2[[i]] <- lm(formula, data=train)
  
  errors.2[i, 1] <- mean((test$Price - predict.lm(models.2[[i]], test))^2)
}

# plot new test error
errors.gg.2 <- ggplot(as.data.frame(cbind(1:10, errors.2)), aes(y=V2, x=V1))+
  geom_point() +
  scale_x_continuous('Fold Number') +
  scale_y_continuous('Test Error') +
  ggtitle('Model Selection - Phase 2') +
  theme_minimal()

# new model selection
best.model.2 <- which.min(errors.2[,1])
model.2 <- models.2[[best.model.2]]
sum.2 <- tidy(summary(model.2))

# filter relevant features
details.2 <- sum.2 %>% filter(p.value < 0.05) %>% arrange(desc(abs(estimate))) %>% filter(!grepl('Model|Make', term))
hist(model.2$residuals, xlab='Residual', main='', breaks = 50)

######################################## T-TEST#########################
df<-read.csv('Dataset/DatasetV2.0_Cleansed.txt',sep = '\t')
df$DateListed <- as.POSIXct(strptime(df$DateListed,"%d/%m/%Y"))
ts_count <- df %>% group_by(DateListed)%>%
  summarise( count_per_date=n())%>%
  filter(year(DateListed) %in% c(2016,2017))
ts_count <- xts(ts_count$count_per_date, order.by = ts_count$DateListed) 
ts_count
autoplot(ts_count)

#Extract month of the year as column
df$Month <- month(df$DateListed,label = T, abbr = T)
df %>% group_by(Month)%>%
  summarise(Median_Price=median(Price), count_per_month=n()) %>% ungroup()-> df1 

par(mfrow=c(2,1))
barplot(df1$count_per_month,names.arg = df1$Month, ylab='Count of Listings ',
        cex.lab=1.2,cex.axis=1.2,cex.names = 1.2)
barplot(df1$Median_Price,names.arg = df1$Month, ylab='Median Price',
        cex.lab=1.2,cex.axis=1.2,cex.names = 1.2)
df1 <- df1 %>% mutate(Q1= Month %in% c('Jan','Feb','Mar'))
# Test if the supply of used cars in Q1 is significantly higher then 
# other times in the year
t.test(df1[df1$Q1==T,]$count_per_month,
       df1[df1$Q1==F,]$count_per_month, paired=FALSE,var.equal = T)
# Test if the median prices in Q1 are significantly lower than 
# other times in the year
t.test(df1[df1$Q1==T,]$Median_Price,
       df1[df1$Q1==F,]$Median_Price, paired=FALSE,var.equal = T)

