# 11. 빅데이터 회귀분석 I. 산불(forest fire)자료분석

rmse <- function(yi, yhat_i){
  sqrt(mean(yi - yhat_i)^2)
}


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

 
# curl https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv > forestfires.csv
# curl https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.names > forestfires.names

forestfires <- tbl_df(read.csv("forestfires.csv", strip.white = TRUE))
glimpse(forestfires)

summary(forestfires)

# 11.3.1. y 변수의 로그변환
p1 <- training %>% ggplot(aes(area)) + geom_density()
p2 <- training %>% ggplot(aes(log(area + 1))) + geom_density()
grid.arrange(p1, p2, ncol=2)

forestfires <- forestfires %>% 
  mutate(logarea = log(area + 1)) %>%
  select(-area)

pairs(forestfires,
      lower.panel=function(x,y){ points(x,y); abline(0, 1, col='red')},
      upper.panel = panel.cor)


# 11.4. 트래인셋과 테스트셋의 구분
set.seed(1606)
n <- nrow(forestfires)
idx <- 1:n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .20)
test_idx <- setdiff(idx, validate_idx)
training <- forestfires[training_idx,]
validation <- forestfires[validate_idx,]
test <- forestfires[test_idx,]


# 11.5. 선형회귀모형 (linera regression model)
ff_lm_full <- lm(logarea ~ ., data=training)
summary(ff_lm_full)

predict(ff_lm_full, newdata = forestfires[1:5,])

# 11.5.1. 선형회귀모형에서 변수선택
ff_lm_full_2 <- lm(logarea ~ .^2, data=training)
summary(ff_lm_full_2)

length(coef(ff_lm_full_2))

library(MASS)
ff_step <- stepAIC(ff_lm_full,
                   scope = list(upper = ~ .^2, lower = ~1))

ff_step

anova(ff_step)

summary(ff_step)

length(coef(ff_step))


# 11.5.2. 모형평가
y_obs <- validation$logarea
yhat_lm <- predict(ff_lm_full, newdata=validation)
yhat_lm_2 <- predict(ff_lm_full_2, newdata=validation)
yhat_step <- predict(ff_step, newdata=validation)
rmse(y_obs, yhat_lm)
rmse(y_obs, yhat_lm_2)
rmse(y_obs, yhat_step)


# 11.6. 라쏘 모형 적합
xx <- model.matrix(logarea ~ .-1, forestfires)
x <- xx[training_idx, ]
y <- training$logarea
glimpse(x)

ff_cvfit <- cv.glmnet(x, y)
plot(ff_cvfit)
 

coef(wage_cvfit, s = c("lambda.1se"))
coef(wage_cvfit, s = c("lambda.min"))


predict.cv.glmnet(ff_cvfit, s="lambda.min", newx = x[1:5,])

y_obs <- validation$logarea
yhat_glmnet <- predict(ff_cvfit, s="lambda.min", newx=xx[validate_idx,])
yhat_glmnet <- yhat_glmnet[,1] # change to a vectro from [n*1] matrix
rmse(y_obs, yhat_glmnet)

# 11.7. 나무모형
ff_tr <- rpart(logarea ~ ., data = training)
ff_tr

printcp(ff_tr)
summary(ff_tr)

opar <- par(mfrow = c(1,1), xpd = NA)
# otherwise on some devices the text is clipped
plot(ff_tr)
text(ff_tr, use.n = TRUE)
par(opar)


yhat_tr <- predict(ff_tr, validation)
rmse(y_obs, yhat_tr)


# 11.8. 랜덤포레스트
set.seed(1607)
ff_rf <- randomForest(logarea ~ ., training)
ff_rf

plot(ff_rf)
varImpPlot(ff_rf)

yhat_rf <- predict(ff_rf, newdata=validation)
rmse(y_obs, yhat_rf)


# 11.9. 부스팅
set.seed(1607)
ff_gbm <- gbm(logarea ~ ., data=training,
          n.trees=100, cv.folds=3, verbose = TRUE)

(best_iter = gbm.perf(ff_gbm, method="cv"))



yhat_gbm <- predict(ff_gbm, n.trees=best_iter, newdata=validation)
rmse(y_obs, yhat_gbm)


# 11.10. 최종 모형선택과  테스트셋 오차계산
data.frame(lm = rmse(y_obs, yhat_lm),
           glmnet = rmse(y_obs, yhat_glmnet),
           rf = rmse(y_obs, yhat_rf),
           gbm = rmse(y_obs, yhat_gbm)) %>%
  reshape2::melt(value.name = 'rmse', variable.name = 'method')

rmse(test$logarea, predict(ff_rf, newdata = test))


# 11.10.1. 회귀분석의 오차의 시각화
boxplot(list(lm = y_obs-yhat_lm, 
             glmnet = y_obs-yhat_glmnet, 
             rf = y_obs-yhat_rf, 
             gbm = y_obs-yhat_gbm), ylab="Error in Validation Set")
abline(h=0, lty=2, col='blue')


pairs(data.frame(y_obs=y_obs,
                 yhat_lm=yhat_lm,
                 yhat_glmnet=c(yhat_glmnet),
                 yhat_rf=yhat_rf,
                 yhat_gbm=yhat_gbm),
      lower.panel=function(x,y){ points(x,y); abline(0, 1, col='red')},
      upper.panel = panel.cor)

