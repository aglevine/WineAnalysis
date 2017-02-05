#Analyze quality of wines
#Author: Aaron Levine, 2017

library(corrplot)
library(ggplot2)

#Red Wine Analysis
redwine <- read.csv("winequality-red.csv",sep=";");
#Correlation Matrix
png(file="red_wine_corr.png",width=800,height=800);
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

#Use cross validation set to find optimal Cost, Gamma
costVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
gammaVec <- c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100.0);
predVec <- c(); 
#for (c in costVec){
#    for(g in gammaVec){
#        svm.model <- svm(quality ~alcohol+volatile.acidity+fixed.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates,data=trainred,cost=c,gamma=g);
	#svm.model <- svm(quality ~alcohol+volatile.acidity,data=trainred,cost=c,gamma=g);
        #svm.model <- svm(quality ~.,data=trainred,cost=c,gamma=g);
#        svm.pred <- predict(svm.model,cvred[,-12]);
#        predtable <- table(pred=svm.pred,true=cvred[,12]);   
#        print(sum(diag(predtable))/sum(predtable));
#	print(c)
#	print(g)
#        predVec <- c(predVec,sum(diag(predtable))/sum(predtable));
#    }
#}

#svm.model <- svm(quality ~alcohol+volatile.acidity,data=trainred,cost=100,gamma=1);  
svm.model <- svm(quality ~alcohol+volatile.acidity,data=trainred,cost=3,gamma=3);
svm.model <- svm(quality ~alcohol+volatile.acidity+fixed.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates,data=trainred,cost=3,gamma=3);
svm.pred <- predict(svm.model,testred[,-12]);
predtable <- table(pred=svm.pred,true=testred[,12]);  
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
for (c in costVec){
    for(g in gammaVec){
        svm.model <- svm(best ~alcohol+volatile.acidity+fixed.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates,data=trainred,cost=c,gamma=g);
        #svm.model <- svm(quality ~alcohol+volatile.acidity,data=trainred,cost=c,gamma=g);
        #svm.model <- svm(quality ~.,data=trainred,cost=c,gamma=g);
        svm.pred <- predict(svm.model,cvred[,-13]);
        predtable <- table(pred=svm.pred,true=cvred[,13]);   
        precision <- predtable[2,2]/(predtable[2,2]+predtable[2,1]);
        recall <- predtable[2,2]/(predtable[2,2]+predtable[1,2]);
        f1score <- 2*(precision*recall)/(precision+recall);
        print(f1score);
        print(c)
        print(g)
        print(predtable)
        predVec <- c(predVec,sum(diag(predtable))/sum(predtable));
    }
}
svm.modelBest <- svm(best ~alcohol+volatile.acidity+fixed.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates,data=trainred,cost=100,gamma=0.1);
svm.predBest <- predict(svm.modelBest,testred[,-13]);
predtable <- table(pred=svm.predBest,true=testred[,13]);
precision <- predtable[2,2]/(predtable[2,2]+predtable[2,1]);
recall <- predtable[2,2]/(predtable[2,2]+predtable[1,2]);
f1score <- 2*(precision*recall)/(precision+recall);
print(f1score);
print(c)
print(g)
print(predtable)

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
for (c in costVec){
    for(g in gammaVec){
        svm.model <- svm(worst ~alcohol+volatile.acidity+fixed.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates+residual.sugar+free.sulfur.dioxide+pH,data=trainred,cost=c,gamma=g);
        #svm.model <- svm(quality ~alcohol+volatile.acidity,data=trainred,cost=c,gamma=g);
        #svm.model <- svm(quality ~.,data=trainred,cost=c,gamma=g);
        svm.pred <- predict(svm.model,cvred[,-14]);
        predtable <- table(pred=svm.pred,true=cvred[,14]);
        precision <- predtable[2,2]/(predtable[2,2]+predtable[2,1]);
        recall <- predtable[2,2]/(predtable[2,2]+predtable[1,2]);
        f1score <- 2*(precision*recall)/(precision+recall);
        print(f1score);
        print(c)
        print(g)
        print(predtable)
        predVec <- c(predVec,sum(diag(predtable))/sum(predtable));
    }
}
svm.modelWorst <- svm(worst ~alcohol+volatile.acidity+fixed.acidity+citric.acid+chlorides+total.sulfur.dioxide+density+sulphates,data=trainred,cost=100,gamma=0.1);
svm.predWorst <- predict(svm.modelWorst,testred[,-14]);
predtable <- table(pred=svm.predBest,true=testred[,14]);
precision <- predtable[2,2]/(predtable[2,2]+predtable[2,1]);
recall <- predtable[2,2]/(predtable[2,2]+predtable[1,2]);
f1score <- 2*(precision*recall)/(precision+recall);
print(f1score);
print(c)
print(g)
print(predtable)

###########################################################################################################################

#White Wine Analysis
whitewine <- read.csv("winequality-white.csv",sep=";");
#Correlation Matrix
png(file="white_wine_corr.png",width=800,height=800);
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
