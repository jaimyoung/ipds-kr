# 12. 빅데이터 회귀분석 II. 자전거 렌탈수요(bike sharing) 자료분석

library(dplyr)
library(ggplot2)
library(MASS)
library(glmnet)
library(randomForest)
library(gbm)
library(rpart)
library(boot)
library(data.table)
library(ROCR)
library(gridExtra)


# curl https://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip > Bike-Sharing-Dataset.zip
# unzip Bike-Sharing-Dataset.zip

bikesharing <- tbl_df(read.csv("Bike-Sharing-Dataset/hour.csv", strip.white = TRUE))
glimpse(bikesharing)

pairs(bikesharing %>% sample_n(1000))


library(lubridate)
bikesharing <- bikesharing %>%
  mutate(day_of_month = day(ymd(dteday)),
         season = factor(season),
         yr = factor(yr),
         mnth = factor(mnth),
         hr = factor(hr),
         holiday = factor(holiday),
         weekday = factor(weekday),
         workingday = factor(workingday),
         weathersit = factor(weathersit),
         logcnt = log(cnt + 1)) %>%
  select(-instant, -cnt, -casual, -registered)



glimpse(bikesharing)

# 12.4. 훈련셋, 검증셋, 테스트셋의 구분
training <- bikesharing %>%
  filter(day_of_month <= 18) %>%
  select(-dteday, -day_of_month)
validation <- bikesharing %>%
  filter(between(day_of_month, 19, 24)) %>%
  select(-dteday, -day_of_month)
test <- bikesharing %>%
  filter(day_of_month >= 25) %>%
  select(-dteday, -day_of_month)


bksh_lm_full <- lm(logcnt ~ ., data=training)
summary(bksh_lm_full)

y_obs <- validation$logcnt
yhat_lm <- predict(bksh_lm_full, newdata=validation)
rmse(y_obs, yhat_lm)


# 12.6. glmnet 라쏘 모형 적합

xx <- model.matrix(logcnt ~ .-1, bikesharing %>% select(-dteday, -day_of_month))
x <- xx[training_idx, ]
y <- training$logcnt
dim(x)
bksh_cvfit <- cv.glmnet(x, y, alpha=1.0)
plot(bksh_cvfit)


coef(bksh_cvfit, 'lambda.1se')
coef(bksh_cvfit, 'lambda.min')


y_obs <- validation$logcnt
yhat_glmnet <- predict(bksh_cvfit, s="lambda.1se", newx=xx[validate_idx,])
yhat_glmnet <- yhat_glmnet[,1] # change to a vectro from [n*1] matrix
rmse(y_obs, yhat_glmnet)


# 12.7. 나무모형 적합
bksh_tr <- rpart(logcnt ~ ., data = training)
bksh_tr

opar <- par(mfrow = c(1,1), xpd = NA)
# otherwise on some devices the text is clipped
plot(bksh_tr)
text(bksh_tr, use.n = TRUE)
par(opar)


yhat_tr <- predict(bksh_tr, validation)
rmse(y_obs, yhat_tr)


# 12.8. 랜덤 포레스트 적합
set.seed(1607)
bksh_rf <- randomForest(logcnt ~ ., training)
bksh_rf
opar <- par(mfrow=c(1,2))
plot(bksh_rf)
varImpPlot(bksh_rf)
par(opar)

yhat_rf <- predict(bksh_rf, newdata = validation)
rmse(y_obs, yhat_rf)


# 12.9. 부스팅 적합

set.seed(1607)
bksh_gbm <- gbm(logcnt ~ ., data=training,
             n.trees=50000, cv.folds=3, verbose=TRUE)
(best_iter <- gbm.perf(bksh_gbm, method="cv"))


yhat_gbm <- predict(bksh_gbm, n.trees=best_iter, newdata=validation)
rmse(y_obs, yhat_gbm)


# 12.10. 모형선택과 테스트셋 오차 계산
data.frame(lm = rmse(y_obs, yhat_lm),
           glmnet = rmse(y_obs, yhat_glmnet),
           tree = rmse(y_obs, yhat_tr),
           rf = rmse(y_obs, yhat_rf),
           gbm = rmse(y_obs, yhat_gbm)) %>%
  reshape2::melt(value.name = 'rmse', variable.name = 'method')



rmse(test$logcnt, predict(bksh_rf, newdata = test))


# 12.11.2. 시각화와 모형적합의 상호관계
bikesharing %>%
  ggplot(aes(hr, logcnt, group=hr)) + geom_boxplot() +
  facet_wrap(~ weekday)
