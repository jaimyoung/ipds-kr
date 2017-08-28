# 빅데이터 분별분석. 스팸 메일 예측
#
if (!file.exists("spambase.data")){
  system('curl https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data > spambase.data')
  system('curl https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.names > spambase.names')
}

binomial_deviance <- function(y_obs, yhat){
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)
  yhat = ifelse(yhat > 1-epsilon, 1-epsilon, yhat)
  a = ifelse(y_obs==0, 0, y_obs * log(y_obs/yhat))
  b = ifelse(y_obs==1, 0, (1-y_obs) * log((1-y_obs)/(1-yhat)))
  return(2*sum(a + b))
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

data <- tbl_df(read.table("spambase.data", strip.white = TRUE,
                          sep=",", header = FALSE))
names(data) <-
  c('word_freq_make', 'word_freq_address', 'word_freq_all', 'word_freq_3d', 'word_freq_our',
    'word_freq_over', 'word_freq_remove', 'word_freq_internet', 'word_freq_order', 'word_freq_mail',
    'word_freq_receive', 'word_freq_will', 'word_freq_people', 'word_freq_report', 'word_freq_addresses',
    'word_freq_free', 'word_freq_business', 'word_freq_email', 'word_freq_you', 'word_freq_credit',
    'word_freq_your', 'word_freq_font', 'word_freq_000', 'word_freq_money', 'word_freq_hp',
    'word_freq_hpl', 'word_freq_george', 'word_freq_650', 'word_freq_lab', 'word_freq_labs',
    'word_freq_telnet', 'word_freq_857', 'word_freq_data', 'word_freq_415', 'word_freq_85',
    'word_freq_technology', 'word_freq_1999', 'word_freq_parts', 'word_freq_pm', 'word_freq_direct',
    'word_freq_cs', 'word_freq_meeting', 'word_freq_original', 'word_freq_project', 'word_freq_re',
    'word_freq_edu', 'word_freq_table', 'word_freq_conference', 'char_freq_;', 'char_freq_(',
    'char_freq_[', 'char_freq_!', 'char_freq_$', 'char_freq_#', 'capital_run_length_average',
    'capital_run_length_longest', 'capital_run_length_total',
    # 'spam'
    'class'
  )
names(data)[58] <- 'class'
data$class <- factor(data$class)

glimpse(data)

summary(data)

png("../../plots/11-1.png", 5.5*1.2, 4*1.2, units='in', pointsize=10, res=600)
set.seed(1610)
pairs(data %>% dplyr::select(1:10, 58) %>%
        sample_n(min(1000, nrow(data))),
      lower.panel=function(x,y){ points(x,y); abline(0, 1, col='red')},
      upper.panel = panel.cor)
dev.off()


png("../../plots/11-2.png", 5.5*1.2, 4*1.2, units='in', pointsize=10, res=600)
set.seed(1610)
pairs(data %>% dplyr::select(48:57, 58) %>%
        sample_n(min(1000, nrow(data))),
      lower.panel=function(x,y){ points(x,y); abline(0, 1, col='red')},
      upper.panel = panel.cor)
dev.off()

#
tmp <- as.data.frame(cor(data[,-58], as.numeric(data$class)))
tmp <- tmp %>% rename(cor=V1)
tmp$var <- rownames(tmp)
tmp %>%
  ggplot(aes(reorder(var, cor), cor)) +
  geom_point() +
  coord_flip()
ggsave("../../plots/11-3.png", width=5.5*1.8, height=4*1.8, units='in', dpi=400)


library(ggplot2)
library(dplyr)
library(gridExtra)
p1 <- data %>% ggplot(aes(class)) + geom_bar()
p2 <- data %>% ggplot(aes(class, `char_freq_$`)) +
  geom_jitter(col='gray') +
  geom_boxplot(alpha=.5) +
  scale_y_sqrt()
p3 <- data %>% ggplot(aes(`char_freq_$`, group=class, fill=class)) +
  geom_density(alpha=.5) +
  scale_x_sqrt() + scale_y_sqrt()
p4 <- data %>% ggplot(aes(class, capital_run_length_longest)) +
  geom_jitter(col='gray') +
  geom_boxplot(alpha=.5) +
  scale_y_log10()
grid.arrange(p1, p2, p3, p4, ncol=2)

g <- arrangeGrob(p1, p2, p3, p4, ncol=2)
ggsave("../../plots/11-4.png", g, width=5.5, height=4, units='in', dpi=600)

?'`'


# 변수명의 특수문자 처리

old_names <- names(data)
new_names <- make.names(names(data), unique = TRUE)
cbind(old_names, new_names) [old_names!=new_names, ]

names(data) <- new_names

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


#-----------------
# 로지스틱 회귀모형
data_lm_full <- glm(class ~ ., data=training, family=binomial)
summary(data_lm_full)

predict(data_lm_full, newdata = data[1:5,], type='response')

# 모형평가
y_obs <- as.numeric(as.character(validation$class))
yhat_lm <- predict(data_lm_full, newdata = validation, type='response')
pred_lm <- prediction(yhat_lm, y_obs)
performance(pred_lm, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_lm)

#-----------------
# 라쏘 모형 적합
# xx <- model.matrix(class ~ .^2-1, data)
xx <- model.matrix(class ~ .-1, data)
x <- xx[training_idx, ]
y <- as.numeric(as.character(training$class))
glimpse(x)

data_cvfit <- cv.glmnet(x, y, family = "binomial")
plot(data_cvfit)

png("../../plots/11-5.png", 5.5, 4, units='in', pointsize=9, res=600)
plot(data_cvfit)
dev.off()

coef(data_cvfit, s = c("lambda.1se"))
coef(data_cvfit, s = c("lambda.min"))


predict.cv.glmnet(data_cvfit, s="lambda.min", newx = x[1:5,], type='response')

yhat_glmnet <- predict(data_cvfit, s="lambda.min", newx=xx[validate_idx,], type='response')
yhat_glmnet <- yhat_glmnet[,1] # change to a vector from [n*1] matrix
pred_glmnet <- prediction(yhat_glmnet, y_obs)
performance(pred_glmnet, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_glmnet)


#-----------------
# 나무모형
data_tr <- rpart(class ~ ., data = training)
data_tr

printcp(data_tr)
summary(data_tr)

png("../../plots/11-6.png", 5.5, 4, units='in', pointsize=9, res=600)
opar <- par(mfrow = c(1,1), xpd = NA)
plot(data_tr)
text(data_tr, use.n = TRUE)
par(opar)
dev.off()


yhat_tr <- predict(data_tr, validation)
yhat_tr <- yhat_tr[,"1"]
pred_tr <- prediction(yhat_tr, y_obs)
performance(pred_tr, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_tr)


#-----------------
# 랜덤포레스트
set.seed(1607)
data_rf <- randomForest(class ~ ., data=training)
data_rf

png("../../plots/11-7.png", 5.5*1.5, 4*1.2, units='in', pointsize=8, res=600)
opar <- par(mfrow=c(1,2))
plot(data_rf)
varImpPlot(data_rf)
par(opar)
dev.off()


yhat_rf <- predict(data_rf, newdata=validation, type='prob')[,'1']
pred_rf <- prediction(yhat_rf, y_obs)
performance(pred_rf, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_rf)


#-----------------
# 부스팅
set.seed(1607)
data_for_gbm <-
  training %>%
  mutate(class=as.numeric(as.character(class)))
data_gbm <- gbm(class ~ ., data=data_for_gbm, distribution="bernoulli",
              n.trees=100000, cv.folds=3, verbose=TRUE)

png("../../plots/11-8.png", 5.5, 4, units='in', pointsize=9, res=600)
(best_iter = gbm.perf(data_gbm, method="cv"))
dev.off()

yhat_gbm <- predict(data_gbm, n.trees=best_iter, newdata=validation, type='response')
pred_gbm <- prediction(yhat_gbm, y_obs)
performance(pred_gbm, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_gbm)

#------------------
# 최종 모형선택과  테스트셋 오차계산
data.frame(method=c('lm', 'glmnet', 'rf', 'gbm'),
           auc = c(performance(pred_lm, "auc")@y.values[[1]],
                   performance(pred_glmnet, "auc")@y.values[[1]],
                   performance(pred_rf, "auc")@y.values[[1]],
                   performance(pred_gbm, "auc")@y.values[[1]]),
           bin_dev = c(binomial_deviance(y_obs, yhat_lm),
                       binomial_deviance(y_obs, yhat_glmnet),
                       binomial_deviance(y_obs, yhat_rf),
                       binomial_deviance(y_obs, yhat_gbm)))

# glmnet이 최종 승리자인 경우:
y_obs_test <- as.numeric(as.character(test$class))
yhat_glmnet_test <- predict(data_cvfit, s="lambda.min", newx=xx[test_idx,], type='response')
yhat_glmnet_test <- yhat_glmnet_test[,1]
pred_glmnet_test <- prediction(yhat_glmnet_test, y_obs_test)
performance(pred_glmnet_test, "auc")@y.values[[1]]
binomial_deviance(y_obs_test, yhat_glmnet_test)

# 랜덤포레스트가 최종 승리자인 경우:
y_obs_test <- as.numeric(as.character(test$class))
yhat_rf_test <- predict(data_rf, newdata=test, type='prob')[,'1']
pred_rf_test <- prediction(yhat_rf_test, y_obs_test)
performance(pred_rf_test, "auc")@y.values[[1]]
binomial_deviance(y_obs_test, yhat_rf_test)


# 예측값들의 상관관계
pairs(data.frame(y_obs=y_obs,
                 yhat_lm=yhat_lm,
                 yhat_glmnet=c(yhat_glmnet),
                 yhat_rf=yhat_rf,
                 yhat_gbm=yhat_gbm),
      lower.panel=function(x,y){ points(x,y); abline(0, 1, col='red')},
      upper.panel = panel.cor)


#-----------
# ROC 커브
perf_lm <- performance(pred_lm, measure = "tpr", x.measure = "fpr")
perf_glmnet <- performance(pred_glmnet, measure="tpr", x.measure="fpr")
perf_rf <- performance(pred_rf, measure="tpr", x.measure="fpr")
perf_gbm <- performance(pred_gbm, measure="tpr", x.measure="fpr")

png("../../plots/11-9.png", 5.5, 4, units='in', pointsize=9, res=600)
plot(perf_lm, col='black', main="ROC Curve")
plot(perf_glmnet, add=TRUE, col='blue')
plot(perf_rf, add=TRUE, col='red')
plot(perf_gbm, add=TRUE, col='cyan')
abline(0,1)
legend('bottomright', inset=.1,
    legend=c("GLM", "glmnet", "RF", "GBM"),
    col=c('black', 'blue', 'red', 'cyan'), lty=1, lwd=2)
dev.off()


png("../../plots/11-10.png", 5.5, 4, units='in', pointsize=9, res=600)
pairs(data.frame(y_obs=y_obs,
                 yhat_lm=yhat_lm,
                 yhat_glmnet=c(yhat_glmnet),
                 yhat_rf=yhat_rf,
                 yhat_gbm=yhat_gbm),
      lower.panel=function(x,y){ points(x,y); abline(0, 1, col='red')},
      upper.panel = panel.cor)
dev.off()
