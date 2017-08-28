# 빅데이터 회귀분석. 부동산 가격 예측
#
if (!file.exists("housing.data")){
  system('curl http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data > housing.data')
  system('curl http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names > housing.names')
}

rmse <- function(yi, yhat_i){
  sqrt(mean((yi - yhat_i)^2))
}

mae <- function(yi, yhat_i){
  mean(abs(yi - yhat_i))
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
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

data <- tbl_df(read.table("housing.data", strip.white = TRUE))
names(data) <- c('crim', 'zn', 'indus', 'chas', 'nox', 'rm', 'age',
                 'dis', 'rad', 'tax', 'ptratio', 'b', 'lstat', 'medv')
glimpse(data)

summary(data)

pairs(data %>% sample_n(min(1000, nrow(data))))

png("../../plots/13-1.png", 5.5*1.2, 4*1.2, units='in', pointsize=9, res=600)
pairs(data %>% sample_n(min(1000, nrow(data))),
      lower.panel=function(x,y){ points(x,y); abline(0, 1, col='red')},
      upper.panel = panel.cor)
dev.off()


# 트래인셋과 테스트셋의 구분
set.seed(1606)
n <- nrow(data)
idx <- 1:n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .20)
test_idx <- setdiff(idx, validate_idx)
training <- data[training_idx,]
validation <- data[validate_idx,]
test <- data[test_idx,]


# 선형회귀모형 (linear regression model)
data_lm_full <- lm(medv ~ ., data=training)
summary(data_lm_full)

predict(data_lm_full, newdata = data[1:5,])

# 선형회귀모형에서 변수선택
data_lm_full_2 <- lm(medv ~ .^2, data=training)
summary(data_lm_full_2)

length(coef(data_lm_full_2))

library(MASS)
data_step <- stepAIC(data_lm_full,
                   scope = list(upper = ~ .^2, lower = ~1))

data_step
anova(data_step)
summary(data_step)
length(coef(data_step))


# 모형평가
y_obs <- validation$medv
yhat_lm <- predict(data_lm_full, newdata=validation)
yhat_lm_2 <- predict(data_lm_full_2, newdata=validation)
yhat_step <- predict(data_step, newdata=validation)
rmse(y_obs, yhat_lm)
rmse(y_obs, yhat_lm_2)
rmse(y_obs, yhat_step)


# 라쏘 모형 적합
xx <- model.matrix(medv ~ .^2-1, data)
x <- xx[training_idx, ]
y <- training$medv
glimpse(x)

data_cvfit <- cv.glmnet(x, y)
plot(data_cvfit)


png("../../plots/13-2.png", 5.5, 4, units='in', pointsize=9, res=600)
plot(data_cvfit)
dev.off()

coef(data_cvfit, s = c("lambda.1se"))
coef(data_cvfit, s = c("lambda.min"))

(tmp <- coef(data_cvfit, s = c("lambda.1se")))
tmp <- tmp[,1]
length(tmp[abs(tmp)>0])
(tmp <- coef(data_cvfit, s = c("lambda.min")))
length(tmp[abs(tmp)>0])

predict.cv.glmnet(data_cvfit, s="lambda.min", newx = x[1:5,])

y_obs <- validation$medv
yhat_glmnet <- predict(data_cvfit, s="lambda.min", newx=xx[validate_idx,])
yhat_glmnet <- yhat_glmnet[,1] # change to a vector from [n*1] matrix
rmse(y_obs, yhat_glmnet)

# 나무모형
data_tr <- rpart(medv ~ ., data = training)
data_tr

printcp(data_tr)
summary(data_tr)

png("../../plots/13-3.png", 5.5, 4, units='in', pointsize=9, res=600)
opar <- par(mfrow = c(1,1), xpd = NA)
plot(data_tr)
text(data_tr, use.n = TRUE)
par(opar)
dev.off()


yhat_tr <- predict(data_tr, validation)
rmse(y_obs, yhat_tr)


# 랜덤포레스트
set.seed(1607)
data_rf <- randomForest(medv ~ ., training)
data_rf

png("../../plots/13-4.png", 5.5, 4*.8, units='in', pointsize=9, res=600)
par(mfrow=c(1,2))
plot(data_rf)
varImpPlot(data_rf)
dev.off()


yhat_rf <- predict(data_rf, newdata=validation)
rmse(y_obs, yhat_rf)


# 부스팅
set.seed(1607)
data_gbm <- gbm(medv ~ ., data=training,
              n.trees=40000, cv.folds=3, verbose = TRUE)

png("../../plots/13-5.png", 5.5, 4, units='in', pointsize=9, res=600)
(best_iter = gbm.perf(data_gbm, method="cv"))
dev.off()

yhat_gbm <- predict(data_gbm, n.trees=best_iter, newdata=validation)
rmse(y_obs, yhat_gbm)


# 최종 모형선택과  테스트셋 오차계산
data.frame(lm = rmse(y_obs, yhat_step),
           glmnet = rmse(y_obs, yhat_glmnet),
           rf = rmse(y_obs, yhat_rf),
           gbm = rmse(y_obs, yhat_gbm)) %>%
  reshape2::melt(value.name = 'rmse', variable.name = 'method')

rmse(test$medv, predict(data_rf, newdata = test))


# 회귀분석의 오차의 시각화
png("../../plots/13-6.png", 5.5, 4, units='in', pointsize=9, res=600)
boxplot(list(lm = y_obs-yhat_step,
             glmnet = y_obs-yhat_glmnet,
             rf = y_obs-yhat_rf,
             gbm = y_obs-yhat_gbm), ylab="Error in Validation Set")
abline(h=0, lty=2, col='blue')
dev.off()


png("../../plots/13-7.png", 5.5, 4, units='in', pointsize=9, res=600)
pairs(data.frame(y_obs=y_obs,
                 yhat_lm=yhat_step,
                 yhat_glmnet=c(yhat_glmnet),
                 yhat_rf=yhat_rf,
                 yhat_gbm=yhat_gbm),
      lower.panel=function(x,y){ points(x,y); abline(0, 1, col='red')},
      upper.panel = panel.cor)
dev.off()
