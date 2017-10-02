# 빅데이터 회귀분석. 와인 품질 예측
#
if (!file.exists("winequality-white.csv")){
  system('curl http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv > winequality-red.csv')
  system('curl http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv > winequality-white.csv')
  system('curl http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality.names > winequality.names')
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

data <- tbl_df(read.table("winequality-white.csv", strip.white = TRUE,
                          sep=";", header = TRUE))
glimpse(data)

summary(data)

pairs(data %>% sample_n(min(1000, nrow(data))))

png("../../plots/14-1.png", 5.5*1.2, 4*1.2, units='in', pointsize=10, res=600)
set.seed(1704)
pairs(data %>% sample_n(min(1000, nrow(data))),
      lower.panel=function(x,y){ points(x,y); abline(0, 1, col='red')},
      upper.panel = panel.cor)
dev.off()


library(ggplot2)
library(dplyr)
library(gridExtra)
p1 <- data %>% ggplot(aes(quality)) + geom_bar()
p2 <- data %>% ggplot(aes(factor(quality), alcohol)) + geom_boxplot()
p3 <- data %>% ggplot(aes(factor(quality), density)) + geom_boxplot()
p4 <- data %>% ggplot(aes(alcohol, density)) + geom_point(alpha=.1) + geom_smooth()
grid.arrange(p1, p2, p3, p4, ncol=2)
g <- arrangeGrob(p1, p2, p3, p4, ncol=2)
ggsave("../../plots/14-2.png", g, width=5.5, height=4, units='in', dpi=600)


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
data_lm_full <- lm(quality ~ ., data=training)
summary(data_lm_full)

predict(data_lm_full, newdata = data[1:5,])

# 선형회귀모형에서 변수선택
data_lm_full_2 <- lm(quality ~ .^2, data=training)
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
y_obs <- validation$quality
yhat_lm <- predict(data_lm_full, newdata=validation)
yhat_lm_2 <- predict(data_lm_full_2, newdata=validation)
yhat_step <- predict(data_step, newdata=validation)
rmse(y_obs, yhat_lm)
rmse(y_obs, yhat_lm_2)
rmse(y_obs, yhat_step)


# 라쏘 모형 적합
xx <- model.matrix(quality ~ .^2-1, data)
# xx <- model.matrix(quality ~ .-1, data)
x <- xx[training_idx, ]
y <- training$quality
glimpse(x)

data_cvfit <- cv.glmnet(x, y)

png("../../plots/14-3.png", 5.5, 4, units='in', pointsize=10, res=600)
plot(data_cvfit)
dev.off()


coef(data_cvfit, s = c("lambda.1se"))
coef(data_cvfit, s = c("lambda.min"))

(tmp <- coef(data_cvfit, s = c("lambda.1se")))
length(tmp[abs(tmp)>0])
(tmp <- coef(data_cvfit, s = c("lambda.min")))
length(tmp[abs(tmp)>0])

predict.cv.glmnet(data_cvfit, s="lambda.min", newx = x[1:5,])

y_obs <- validation$quality
yhat_glmnet <- predict(data_cvfit, s="lambda.min", newx=xx[validate_idx,])
yhat_glmnet <- yhat_glmnet[,1] # change to a vector from [n*1] matrix
rmse(y_obs, yhat_glmnet)

# 나무모형
data_tr <- rpart(quality ~ ., data = training)
data_tr

printcp(data_tr)
summary(data_tr)

png("../../plots/14-4.png", 5.5, 4, units='in', pointsize=10, res=600)
opar <- par(mfrow = c(1,1), xpd = NA)
plot(data_tr)
text(data_tr, use.n = TRUE)
par(opar)
dev.off()

yhat_tr <- predict(data_tr, validation)
rmse(y_obs, yhat_tr)


# 랜덤포레스트
set.seed(1607)
data_rf <- randomForest(quality ~ ., training)
data_rf

png("../../plots/14-5.png", 5.5*1.5, 4, units='in', pointsize=9, res=600)
opar <- par(mfrow=c(1,2))
plot(data_rf)
varImpPlot(data_rf)
par(opar)
dev.off()

yhat_rf <- predict(data_rf, newdata=validation)
rmse(y_obs, yhat_rf)


# 부스팅
set.seed(1607)
data_gbm <- gbm(quality ~ ., data=training,
                n.trees=40000, cv.folds=3, verbose = TRUE)

png("../../plots/14-6.png", 5.5, 4, units='in', pointsize=9, res=600)
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

rmse(test$quality, predict(data_rf, newdata = test))


# 회귀분석의 오차의 시각화
boxplot(list(lm = y_obs-yhat_step,
             glmnet = y_obs-yhat_glmnet,
             rf = y_obs-yhat_rf,
             gbm = y_obs-yhat_gbm), ylab="Error in Validation Set")
abline(h=0, lty=2, col='blue')


png("../../plots/14-7.png", 5.5, 4, units='in', pointsize=9, res=600)
pairs(data.frame(y_obs=y_obs,
                 yhat_lm=yhat_step,
                 yhat_glmnet=c(yhat_glmnet),
                 yhat_rf=yhat_rf,
                 yhat_gbm=yhat_gbm),
      lower.panel=function(x,y){ points(x,y); abline(0, 1, col='red')},
      upper.panel = panel.cor)
dev.off()
