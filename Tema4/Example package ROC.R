# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
library(ROCR)
data(ROCR.simple)

pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
pred


perf <- performance(pred,"tpr","fpr")
perf
plot(perf, main = "ROC curve", colorize = T)
abline(a = 0, b = 1)

#AUC Value 
auc_ROC <- performance(pred, "auc")
auc_ROC_value <- auc_ROC@y.values[[1]]



#lift charts
perf <- performance(pred,"lift","rpp")
perf
plot(perf)

#Accuracy
acc.perf = performance(pred, measure = "acc")
plot(acc.perf)

ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))

#### ESTOS EJEMPLOS QUE SIGUEN SE USARAN EN CAPITULOS NN y SVM

# SVM and NN
library(ROCR)
data(ROCR.hiv)
attach(ROCR.hiv)
pred.svm <- prediction(hiv.svm$predictions, hiv.svm$labels)
pred.svm
perf.svm <- performance(pred.svm, 'tpr', 'fpr')
perf.svm
pred.nn <- prediction(hiv.nn$predictions, hiv.svm$labels)
pred.nn
perf.nn <- performance(pred.nn, 'tpr', 'fpr')
perf.nn
plot(perf.svm, lty=3, col="red",main="SVMs and NNs for prediction of
     HIV-1 coreceptor usage")
plot(perf.nn, lty=3, col="blue",add=TRUE)
plot(perf.svm, avg="vertical", lwd=3, col="red",
     spread.estimate="stderror",plotCI.lwd=2,add=TRUE)
plot(perf.nn, avg="vertical", lwd=3, col="blue",
     spread.estimate="stderror",plotCI.lwd=2,add=TRUE)
legend(0.6,0.6,c('SVM','NN'),col=c('red','blue'),lwd=3)


# cross-validation
# plot ROC curves for several cross-validation runs (dotted
# in grey), overlaid by the vertical average curve and boxplots
# showing the vertical spread around the average.
library(ROCR)
datos<- data(ROCR.xval)
pred <- prediction(ROCR.xval$predictions, ROCR.xval$labels)
pred
perf <- performance(pred,"tpr","fpr")
perf
plot(perf,col="grey82",lty=3)
plot(perf,lwd=3,avg="vertical",spread.estimate="boxplot",add=TRUE)

