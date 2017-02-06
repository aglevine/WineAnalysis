#Analyze quality of wines
#Author: Aaron Levine, 2017

library(corrplot)
library(ggplot2)
library(e1071)
library(gridExtra)

set.seed(1);
#Red Wine Analysis
redwine <- read.csv("winequality-red.csv",sep=";");
#Correlation Matrix
png(file="red_wine_corr.png");
corrplot(cor(redwine));
dev.off();

######Exploratory Plots##############
#fixed.acidity plot
png(file="fixedacidity_quality_red.png");
ggplot(redwine,aes(fixed.acidity,quality))+geom_point(color="darkred",alpha=1/2)+geom_smooth(method="lm") + labs(x="Fixed Acidity",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#volatile.acidity plot
png(file="volatileacidity_quality_red.png");
ggplot(redwine,aes(volatile.acidity,quality))+geom_point(color="darkred",alpha=1/2)+geom_smooth(method="lm") + labs(x="Volatile Acidity",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#citric.acid plot
png(file="citricacid_quality_red.png");
ggplot(redwine,aes(citric.acid,quality))+geom_point(color="darkred",alpha=1/2)+geom_smooth(method="lm") + labs(x="Citric Acid",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#residual.sugar plot
png(file="residualsugar_quality_red.png");
ggplot(redwine,aes(residual.sugar,quality))+geom_point(color="darkred",alpha=1/2)+geom_smooth(method="lm") + labs(x="Residual Sugar",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#chlorides plot
png(file="chlorides_quality_red.png");
ggplot(redwine,aes(chlorides,quality))+geom_point(color="darkred",alpha=1/2)+geom_smooth(method="lm") + labs(x="Chlorides",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#free.sulfur.dioxide plot
png(file="freesulfurdioxide_quality_red.png");
ggplot(redwine,aes(free.sulfur.dioxide,quality))+geom_point(color="darkred",alpha=1/2)+geom_smooth(method="lm") + labs(x="Free Sulfur Dioxide",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#total.sulfur.dioxide plot
png(file="totalsulfurdioxide_quality_red.png");
ggplot(redwine,aes(total.sulfur.dioxide,quality))+geom_point(color="darkred",alpha=1/2)+geom_smooth(method="lm") + labs(x="Total Sulfur Dioxide",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#density plot
png(file="density_red.png");
ggplot(redwine,aes(density,quality))+geom_point(color="darkred",alpha=1/2)+geom_smooth(method="lm") + labs(x="Density",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#pH plot
png(file="pH_red.png");
ggplot(redwine,aes(pH,quality))+geom_point(color="darkred",alpha=1/2)+geom_smooth(method="lm") + labs(x="pH",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#sulphates plot
png(file="sulphates_quality_red.png");
ggplot(redwine,aes(sulphates,quality))+geom_point(color="darkred",alpha=1/2)+geom_smooth(method="lm") + labs(x="Sulphates",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#alcohol plot
png(file="alcohol_quality_red.png");
ggplot(redwine,aes(alcohol,quality))+geom_point(color="darkred",alpha=1/2)+geom_smooth(method="lm") + labs(x="Alcohol",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

###divide into training, cross validation, and test set
#convert to factors
redwine$quality <- as.factor(redwine$quality)

#divide into train,cv,test sets(60%,20%,20%)
index <- 1:nrow(redwine);
testcvindex <-sample(index,trunc(length(index)*4/10));
cvindex <-testcvindex[1:floor(length(testcvindex)/2)];
testindex <-testcvindex[ceiling(length(testcvindex)/2):length(testcvindex)];

testred <- redwine[testindex,];
cvred <- redwine[cvindex,];
trainred <- redwine[-testcvindex,];

print(table(testred$quality))
print(table(cvred$quality))
print(table(trainred$quality))

#Use cross validation set to find optimal Cost, Gamma
costVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
gammaVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
predVec <- c(); 
#for (c in costVec){
#    for(g in gammaVec){
#        svm.model <- svm(quality ~alcohol+volatile.acidity+fixed.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates,data=trainred,cost=c,gamma=g);
#	#svm.model <- svm(quality ~alcohol+volatile.acidity,data=trainred,cost=c,gamma=g);
#        #svm.model <- svm(quality ~.,data=trainred,cost=c,gamma=g);
#        svm.pred <- predict(svm.model,cvred[,-12]);
#        predtable <- table(pred=svm.pred,true=cvred[,12]);   
#        print(sum(diag(predtable))/sum(predtable));
#	print(c)
#	print(g)
#        predVec <- c(predVec,round(sum(diag(predtable))/sum(predtable),3));
#    }
#}
#colnames <- as.character(costVec);
#rownames <- as.character(gammaVec);
#predMat <- matrix(predVec,length(gammaVec),length(costVec));
#predMat <- as.table(predMat);
#rownames(predMat) <- rownames;
#colnames(predMat) <- colnames;
#png(file="red_quality_cv_cgamma_top8features.png");
#grid.table(predMat);
#dev.off();
svm.model <- svm(quality ~.,data=trainred,cost=1,gamma=1);  
#svm.model <- svm(quality ~alcohol+volatile.acidity,data=trainred,cost=3,gamma=1);
#svm.model <- svm(quality ~alcohol+volatile.acidity+fixed.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates,data=trainred,cost=1,gamma=3);
svm.pred <- predict(svm.model,testred[,-12]);
predtable <- table(pred=svm.pred,true=testred[,12]);  
png(file="red_quality_prediction_table.png");
grid.table(predtable);
dev.off();
print("prediction percentage for red quality");
print(predtable)
print(sum(diag(predtable))/sum(predtable));

#Define exceptional wines (>7)
redbest <- sapply(as.numeric(as.character(redwine$quality)),function(x){if(x<7){x=0;}else{x=1}})

redwine$best <- as.factor(redbest);

index <- 1:nrow(redwine);
testcvindex <-sample(index,trunc(length(index)*4/10));
cvindex <-testcvindex[1:floor(length(testcvindex)/2)];
testindex <-testcvindex[ceiling(length(testcvindex)/2):length(testcvindex)];

testred <- redwine[testindex,];
cvred <- redwine[cvindex,];
trainred <- redwine[-testcvindex,];

#Use cross validation set to find optimal Cost, Gamma
costVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
gammaVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
predVec <- c();
#for (c in costVec){
#    for(g in gammaVec){
#        svm.model <- svm(best ~alcohol+volatile.acidity+fixed.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates,data=trainred,cost=c,gamma=g);
#         #svm.model <- svm(quality ~alcohol+volatile.acidity,data=trainred,cost=c,gamma=g);
#        svm.model <- svm(best ~alcohol+volatile.acidity+chlorides+total.sulfur.dioxide+density+fixed.acidity+citric.acid+residual.sugar+free.sulfur.dioxide+pH+sulphates,data=trainred,cost=c,gamma=g);
#        svm.pred <- predict(svm.model,cvred[,-13]);
#        predtable <- table(pred=svm.pred,true=cvred[,13]);   
#        precision <- predtable[2,2]/(predtable[2,2]+predtable[2,1]);
#        recall <- predtable[2,2]/(predtable[2,2]+predtable[1,2]);
#        f1score <- 2*(precision*recall)/(precision+recall);
#        print(f1score);
#        print(c)
#        print(g)
#        print(predtable)
#        predVec <- c(predVec,round(f1score,3));
#    }
#}
#colnames <- as.character(costVec);
#rownames <- as.character(gammaVec);
#predMat <- matrix(predVec,length(gammaVec),length(costVec));
#predMat <- as.table(predMat);
#rownames(predMat) <- rownames;
#colnames(predMat) <- colnames;
#png(file="red_best_cv_cgamma_allfeatures.png");
#grid.table(predMat);
#dev.off();

#svm.modelBest <- svm(best ~alcohol+volatile.acidity+fixed.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates,data=trainred,cost=100,gamma=0.1);
svm.modelBest <- svm(best ~alcohol+volatile.acidity+chlorides+total.sulfur.dioxide+density+fixed.acidity+citric.acid+residual.sugar+free.sulfur.dioxide+pH+sulphates,data=trainred,cost=100.0,gamma=0.3);
svm.predBest <- predict(svm.modelBest,testred[,-13]);
predtable <- table(pred=svm.predBest,true=testred[,13]);
precision <- predtable[2,2]/(predtable[2,2]+predtable[2,1]);
recall <- predtable[2,2]/(predtable[2,2]+predtable[1,2]);
f1score <- 2*(precision*recall)/(precision+recall);
print("predictions for best wines")
print(f1score);
png(file="red_best_prediction_table.png");
grid.table(predtable);
dev.off();
print(predtable);
table(testred$quality[svm.predBest==1])
table(testred$quality[svm.predBest==0])

#worst if <= 4
redworst <- sapply(as.numeric(as.character(redwine$quality)),function(x){if(x>4){x=0;}else{x=1}})

redwine$worst <- as.factor(redworst);

index <- 1:nrow(redwine);
testcvindex <-sample(index,trunc(length(index)*4/10));
cvindex <-testcvindex[1:floor(length(testcvindex)/2)];
testindex <-testcvindex[ceiling(length(testcvindex)/2):length(testcvindex)];

testred <- redwine[testindex,];
cvred <- redwine[cvindex,];
trainred <- redwine[-testcvindex,];

#Use cross validation set to find optimal Cost, Gamma
costVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
gammaVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
predVec <- c();
#for (c in costVec){
#    for(g in gammaVec){
#       svm.model <- svm(worst ~alcohol+volatile.acidity+chlorides+total.sulfur.dioxide+density+fixed.acidity+citric.acid+residual.sugar+free.sulfur.dioxide+pH+sulphates,data=trainred,cost=c,gamma=g);
        #svm.model <- svm(worst ~volatile.acidity,data=trainred,cost=c,gamma=g);
       #svm.model <- svm(quality ~alcohol+volatile.acidity,data=trainred,cost=c,gamma=g);
       #svm.model <- svm(quality ~.,data=trainred,cost=c,gamma=g);
#        svm.pred <- predict(svm.model,cvred[,-14]);
#        predtable <- table(pred=svm.pred,true=cvred[,14]);
#        precision <- predtable[2,2]/(predtable[2,2]+predtable[2,1]);
#        recall <- predtable[2,2]/(predtable[2,2]+predtable[1,2]);
#        f1score <- 2*(precision*recall)/(precision+recall);
#        print(f1score);
#        print(c)
#        print(g)
#        print(predtable)
#        predVec <- c(predVec,round(f1score,3));
#    }
#}
#colnames <- as.character(costVec);
#rownames <- as.character(gammaVec);
#predMat <- matrix(predVec,length(gammaVec),length(costVec));
#predMat <- as.table(predMat);
#rownames(predMat) <- rownames;
#colnames(predMat) <- colnames;
#png(file="red_worst_cv_cgamma_allfeatures.png");
#grid.table(predMat);
#dev.off();
#svm.modelWorst <- svm(worst ~alcohol+volatile.acidity+fixed.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates+residual.sugar+free.sulfur.dioxide+pH,data=trainred,cost=100,gamma=0.3);
svm.modelWorst <- svm(worst ~volatile.acidity,data=trainred,cost=30,gamma=0.3);
svm.predWorst <- predict(svm.modelWorst,testred[,-14]);
predtable <- table(pred=svm.predBest,true=testred[,14]);
precision <- predtable[2,2]/(predtable[2,2]+predtable[2,1]);
recall <- predtable[2,2]/(predtable[2,2]+predtable[1,2]);
f1score <- 2*(precision*recall)/(precision+recall);
print("predictions for worst wines")
print(f1score);
png(file="red_worst_prediction_table.png");
grid.table(predtable);
dev.off();
print(predtable);
table(testred$quality[svm.predWorst==1])
table(testred$quality[svm.predWorst==0])

redwine$quality <- as.numeric(as.character(redwine$quality))
redwine$best <- as.numeric(as.character(redwine$best));
redwine$worst <- as.numeric(as.character(redwine$worst));
png(file="red_wine_corr_best_worst.png");
corrplot(cor(redwine));
dev.off();


###########################################################################################################################

#White Wine Analysis
whitewine <- read.csv("winequality-white.csv",sep=";");
#Correlation Matrix
png(file="white_wine_corr.png");
corrplot(cor(whitewine));
dev.off();

######Exploratory Plots##############
#fixed.acidity plot
png(file="fixedacidity_quality_white.png");
ggplot(whitewine,aes(fixed.acidity,quality))+geom_point(color="gold4",alpha=1/2)+geom_smooth(method="lm") + labs(x="Fixed Acidity",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#volatile.acidity plot
png(file="volatileacidity_quality_white.png");
ggplot(whitewine,aes(volatile.acidity,quality))+geom_point(color="gold4",alpha=1/2)+geom_smooth(method="lm") + labs(x="Volatile Acidity",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#citric.acid plot
png(file="citricacid_quality_white.png");
ggplot(whitewine,aes(citric.acid,quality))+geom_point(color="gold4",alpha=1/2)+geom_smooth(method="lm") + labs(x="Citric Acid",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#residual.sugar plot
png(file="residualsugar_quality_white.png");
ggplot(whitewine,aes(residual.sugar,quality))+geom_point(color="gold4",alpha=1/2)+geom_smooth(method="lm") + labs(x="Residual Sugar",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#chlorides plot
png(file="chlorides_quality_white.png");
ggplot(whitewine,aes(chlorides,quality))+geom_point(color="gold4",alpha=1/2)+geom_smooth(method="lm") + labs(x="Chlorides",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#free.sulfur.dioxide plot
png(file="freesulfurdioxide_quality_white.png");
ggplot(whitewine,aes(free.sulfur.dioxide,quality))+geom_point(color="gold4",alpha=1/2)+geom_smooth(method="lm") + labs(x="Free Sulfur Dioxide",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#total.sulfur.dioxide plot
png(file="totalsulfurdioxide_quality_white.png");
ggplot(whitewine,aes(total.sulfur.dioxide,quality))+geom_point(color="gold4",alpha=1/2)+geom_smooth(method="lm") + labs(x="Total Sulfur Dioxide",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#density plot
png(file="density_white.png");
ggplot(whitewine,aes(density,quality))+geom_point(color="gold4",alpha=1/2)+geom_smooth(method="lm") + labs(x="Density",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#pH plot
png(file="pH_white.png");
ggplot(whitewine,aes(pH,quality))+geom_point(color="gold4",alpha=1/2)+geom_smooth(method="lm") + labs(x="pH",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#sulphates plot
png(file="sulphates_quality_white.png");
ggplot(whitewine,aes(sulphates,quality))+geom_point(color="gold4",alpha=1/2)+geom_smooth(method="lm") + labs(x="Sulphates",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();

#alcohol plot
png(file="alcohol_quality_white.png");
ggplot(whitewine,aes(alcohol,quality))+geom_point(color="gold4",alpha=1/2)+geom_smooth(method="lm") + labs(x="Alcohol",y="Quality")+theme(axis.text=element_text(size=20),axis.title=element_text(size=28,face="bold"));
dev.off();


###divide into training, cross validation, and test set
#convert to factors
whitewine$quality <- as.factor(whitewine$quality)

#divide into train,cv,test sets(60%,20%,20%)
index <- 1:nrow(whitewine);
testcvindex <-sample(index,trunc(length(index)*4/10));
cvindex <-testcvindex[1:floor(length(testcvindex)/2)];
testindex <-testcvindex[ceiling(length(testcvindex)/2):length(testcvindex)];

testwhite <- whitewine[testindex,];
cvwhite <- whitewine[cvindex,];
trainwhite <- whitewine[-testcvindex,];

#Use cross validation set to find optimal Cost, Gamma
costVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
gammaVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
predVec <- c(); 
#for (c in costVec){
#    for(g in gammaVec){
        #svm.model <- svm(quality ~alcohol+volatile.acidity+fixed.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates,data=trainwhite,cost=c,gamma=g);
#        svm.model <- svm(quality ~alcohol+volatile.acidity+chlorides+total.sulfur.dioxide+density,data=trainwhite,cost=c,gamma=g);
#        svm.model <- svm(quality ~.,data=trainwhite,cost=c,gamma=g);
#        svm.pred <- predict(svm.model,cvwhite[,-12]);
#        predtable <- table(pred=svm.pred,true=cvwhite[,12]);   
#        print(sum(diag(predtable))/sum(predtable));
#	print(c)
#	print(g)
#	print(predtable)
#        predVec <- c(predVec,round(sum(diag(predtable))/sum(predtable),3));
#    }
#}
#colnames <- as.character(costVec);
#rownames <- as.character(gammaVec);
#predMat <- matrix(predVec,length(gammaVec),length(costVec));
#predMat <- as.table(predMat);
#rownames(predMat) <- rownames;
#colnames(predMat) <- colnames;
#png(file="white_quality_cv_cgamma_allfeatures.png");
#grid.table(predMat);
#dev.off();

svm.model <- svm(quality ~alcohol+volatile.acidity+chlorides+total.sulfur.dioxide+density+fixed.acidity+citric.acid+residual.sugar+free.sulfur.dioxide+pH+sulphates,data=trainwhite,cost=1.0,gamma=1.0);
#svm.model <- svm(quality ~alcohol+volatile.acidity+chlorides+total.sulfur.dioxide+density,data=trainwhite,cost=30,gamma=3);
svm.pred <- predict(svm.model,testwhite[,-12]);
predtable <- table(pred=svm.pred,true=testwhite[,12]);
png(file="white_quality_prediction_table.png");
grid.table(predtable);
dev.off();
print("prediction percentage for white quality");
print(predtable)
print(sum(diag(predtable))/sum(predtable));



#Define exceptional white wines (>7)
whitebest <- sapply(as.numeric(as.character(whitewine$quality)),function(x){if(x<7){x=0;}else{x=1}})

whitewine$best <- as.factor(whitebest);

index <- 1:nrow(whitewine);
testcvindex <-sample(index,trunc(length(index)*4/10));
cvindex <-testcvindex[1:floor(length(testcvindex)/2)];
testindex <-testcvindex[ceiling(length(testcvindex)/2):length(testcvindex)];

testwhite <- whitewine[testindex,];
cvwhite <- whitewine[cvindex,];
trainwhite <- whitewine[-testcvindex,];

#Use cross validation set to find optimal Cost, Gamma
costVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
gammaVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
predVec <- c();
#for (c in costVec){
#    for(g in gammaVec){
#        #svm.model <- svm(best ~alcohol+volatile.acidity+fixed.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates,data=trainwhite,cost=c,gamma=g);
#        #svm.model <- svm(quality ~alcohol+volatile.acidity,data=trainwhite,cost=c,gamma=g);
#        svm.model <- svm(best ~alcohol+volatile.acidity+chlorides+total.sulfur.dioxide+density+fixed.acidity+citric.acid+residual.sugar+free.sulfur.dioxide+pH+sulphates,data=trainwhite,cost=c,gamma=g);
#        svm.pred <- predict(svm.model,cvwhite[,-13]);
#        predtable <- table(pred=svm.pred,true=cvwhite[,13]);   
#        precision <- predtable[2,2]/(predtable[2,2]+predtable[2,1]);
#        recall <- predtable[2,2]/(predtable[2,2]+predtable[1,2]);
#        f1score <- 2*(precision*recall)/(precision+recall);
#        print(f1score);
#        print(c)
#        print(g)
#        print(predtable)
#        predVec <- c(predVec,round(f1score,3));
#    }
#}
#colnames <- as.character(costVec);
#rownames <- as.character(gammaVec);
#predMat <- matrix(predVec,length(gammaVec),length(costVec));
#predMat <- as.table(predMat);
#rownames(predMat) <- rownames;
#colnames(predMat) <- colnames;
#png(file="white_best_cv_cgamma_allfeatures.png");
#grid.table(predMat);
#dev.off();

svm.modelBest <- svm(best ~alcohol+volatile.acidity+chlorides+total.sulfur.dioxide+density+fixed.acidity+citric.acid+residual.sugar+free.sulfur.dioxide+pH+sulphates,data=trainwhite,cost=10,gamma=1.0);
svm.predBest <- predict(svm.modelBest,testwhite[,-13]);
predtable <- table(pred=svm.predBest,true=testwhite[,13]);
precision <- predtable[2,2]/(predtable[2,2]+predtable[2,1]);
recall <- predtable[2,2]/(predtable[2,2]+predtable[1,2]);
f1score <- 2*(precision*recall)/(precision+recall);
print("predictions for best wines")
print(f1score);
png(file="white_best_prediction_table.png");
grid.table(predtable);
dev.off();
print(predtable);
table(testwhite$quality[svm.predBest==1])
table(testwhite$quality[svm.predBest==0])

whiteworst <- sapply(as.numeric(as.character(whitewine$quality)),function(x){if(x>4){x=0;}else{x=1}})

whitewine$worst <- as.factor(whiteworst);

index <- 1:nrow(whitewine);
testcvindex <-sample(index,trunc(length(index)*4/10));
cvindex <-testcvindex[1:floor(length(testcvindex)/2)];
testindex <-testcvindex[ceiling(length(testcvindex)/2):length(testcvindex)];

testwhite <- whitewine[testindex,];
cvwhite <- whitewine[cvindex,];
trainwhite <- whitewine[-testcvindex,];

#Use cross validation set to find optimal Cost, Gamma
costVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
gammaVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
predVec <- c();
#for (c in costVec){
#    for(g in gammaVec){
        #svm.model <- svm(worst ~alcohol+volatile.acidity+fixed.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates,data=trainwhite,cost=c,gamma=g);
        #svm.model <- svm(quality ~alcohol+volatile.acidity,data=trainwhite,cost=c,gamma=g);
#        svm.model <- svm(worst ~alcohol+volatile.acidity+chlorides+total.sulfur.dioxide+density+fixed.acidity+citric.acid+residual.sugar+free.sulfur.dioxide+pH+sulphates,data=trainwhite,cost=c,gamma=g);
#        svm.pred <- predict(svm.model,cvwhite[,-14]);
#        predtable <- table(pred=svm.pred,true=cvwhite[,14]);   
#        precision <- predtable[2,2]/(predtable[2,2]+predtable[2,1]);
#        recall <- predtable[2,2]/(predtable[2,2]+predtable[1,2]);
#        f1score <- 2*(precision*recall)/(precision+recall);
#        print(f1score);
#        print(c)
#        print(g)
#        print(predtable)
#        predVec <- c(predVec,round(f1score,3));
#    }
#}
#colnames <- as.character(costVec);
#rownames <- as.character(gammaVec);
#predMat <- matrix(predVec,length(gammaVec),length(costVec));
#predMat <- as.table(predMat);
#rownames(predMat) <- rownames;
#colnames(predMat) <- colnames;
#png(file="white_worst_cv_cgamma_allfeatures.png");
#grid.table(predMat);
#dev.off();

svm.modelWorst <- svm(worst ~alcohol+volatile.acidity+chlorides+total.sulfur.dioxide+density+fixed.acidity+citric.acid+residual.sugar+free.sulfur.dioxide+pH+sulphates,data=trainwhite,cost=100,gamma=0.01);
svm.predWorst <- predict(svm.modelWorst,testwhite[,-14]);
predtable <- table(pred=svm.predWorst,true=testwhite[,14]);
precision <- predtable[2,2]/(predtable[2,2]+predtable[2,1]);
recall <- predtable[2,2]/(predtable[2,2]+predtable[1,2]);
f1score <- 2*(precision*recall)/(precision+recall);
table(testwhite$quality[svm.predWorst==1])
table(testwhite$quality[svm.predWorst==0])
print("predictions for worst wines")
print(f1score);
png(file="white_worst_prediction_table.png");
grid.table(predtable);
dev.off();
print(predtable);
table(testwhite$quality[svm.predWorst==1])
table(testwhite$quality[svm.predWorst==0])


whitewine$quality <- as.numeric(as.character(whitewine$quality))
whitewine$best <- as.numeric(as.character(whitewine$best));
whitewine$worst <- as.numeric(as.character(whitewine$worst));
png(file="white_wine_corr_best_worst.png");
corrplot(cor(whitewine));
dev.off();

whitewine$color <- rep(0,nrow(whitewine));
redwine$color <- rep(1,nrow(redwine));

winesAll <- rbind(whitewine,redwine);
png(file="allwine_corr.png");
corrplot(cor(winesAll));
dev.off();

winesAll$color <- as.factor(winesAll$color)

#divide into train,cv,test sets(60%,20%,20%)
index <- 1:nrow(winesAll);
testcvindex <-sample(index,trunc(length(index)*4/10));
cvindex <-testcvindex[1:floor(length(testcvindex)/2)];
testindex <-testcvindex[ceiling(length(testcvindex)/2):length(testcvindex)];

testall <- winesAll[testindex,];
cvall <- winesAll[cvindex,];
trainall <- winesAll[-testcvindex,];

#Use cross validation set to find optimal Cost, Gamma
#costVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
#gammaVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
#predVec <- c();
#for (c in costVec){
#    for(g in gammaVec){
#        svm.model <- svm(color~volatile.acidity+chlorides+total.sulfur.dioxide+density+fixed.acidity+citric.acid+residual.sugar+free.sulfur.dioxide+pH+sulphates,data=trainall,cost=c,gamma=g);
#        #svm.model <- svm(quality ~alcohol+volatile.acidity,data=trainall,cost=c,gamma=g);
#        #svm.model <- svm(quality ~.,data=trainall,cost=c,gamma=g);
#        svm.pred <- predict(svm.model,cvall[,-15]);
#        predtable <- table(pred=svm.pred,true=cvall[,15]);   
#        precision <- predtable[2,2]/(predtable[2,2]+predtable[2,1]);
#        recall <- predtable[2,2]/(predtable[2,2]+predtable[1,2]);
#        f1score <- 2*(precision*recall)/(precision+recall);
#        print(f1score);
#        print(c)
#        print(g)
#        print(predtable)
#        predVec <- c(predVec,round(f1score,3));
#    }
#}
#colnames <- as.character(costVec);
#rownames <- as.character(gammaVec);
#predMat <- matrix(predVec,length(gammaVec),length(costVec));
#predMat <- as.table(predMat);
#rownames(predMat) <- rownames;
#colnames(predMat) <- colnames;
#png(file="white_red_diff_cv_cgamma.png");
#grid.table(predMat);
#dev.off();
svm.modelColor <- svm(color ~volatile.acidity+chlorides+total.sulfur.dioxide+density+fixed.acidity+citric.acid+residual.sugar+free.sulfur.dioxide+pH+sulphates,data=trainall,cost=10,gamma=0.03);
svm.predColor <- predict(svm.modelColor,testall[,-15]);
predtable <- table(pred=svm.predColor,true=testall[,15]);
precision <- predtable[2,2]/(predtable[2,2]+predtable[2,1]);
recall <- predtable[2,2]/(predtable[2,2]+predtable[1,2]);
f1score <- 2*(precision*recall)/(precision+recall);
print("predictions for wine color")
print(f1score);
png(file="wine_color_prediction_table.png");
grid.table(predtable);
dev.off();
print(predtable);
table(testall$quality[svm.predColor==1& testall$color==0])
table(testall$quality[svm.predColor==0 & testall$color==1])

