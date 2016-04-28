library(ggplot2)
data(diamonds)

diamonds$is_expensive <- diamonds$price > 2400
is_test <- runif(nrow(diamonds)) > 0.75
train <- diamonds[is_test==FALSE,]
test <- diamonds[is_test==TRUE,]

summary(fit <- glm(is_expensive ~ carat + cut + clarity, data=train))

library(ROCR)

prob <- predict(fit, newdata=test, type="response")
pred <- prediction(prob, test$is_expensive)
class(pred)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve w/ AUC=", auc))

mc_roc <- multiclass.roc(pred_ensamble, as.numeric(pred_test$classe), plot=TRUE)
plot.roc(mc_roc)
#summary(mc_roc)
#head(mc_roc$rocs)
#plot(mc_roc$rocs[1])
#plot.roc()

bin_roc <- roc(pred_validation1, binarizedValidate$classe)

plot.roc(bin_roc)

#############################################################

pred <- predict(model1, newdata=binarizedValidate)
pre <- prediction(pred, binarizedValidate$binClasse)
perf <- performance(pre, measure = "tpr", x.measure = "fpr")
auc <- performance(pre, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve w/ AUC=", auc))



#############################################################

pred <- predict(fit_ensemble, newdata=pred_test)
prob <- predict(fit_ensemble, newdata=pred_test, type="prob")

prob$pred <- pred
prob$classe <- pred_test$classe
filter(prob, pred!=classe)

prob_A <- prob %>% mutate(prob=ifelse(pred=="A", A, A), binPred=ifelse(pred=="A", 1, 0), binClasse=ifelse(classe=="A", 1, 0))
filter(prob_A, binPred!=binClasse)
head(prob_A$prob)
head(prob_A$binClasse)
pre <- prediction(prob_A$prob, prob_A$binClasse)
class(pred)
perf <- performance(pre, measure = "tpr", x.measure = "fpr")
auc <- performance(pre, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0("ROC Curve w/ AUC=", auc))





