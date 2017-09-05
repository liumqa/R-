#install.packages("ggthemes")
library("ggthemes")
library("ggplot2")
library('dplyr') # 数据处理
library("mice") #用来估算的包
library("randomForest")
library("rpart")
#library(cforest)
###########################################
##熟悉数据整体情况
train<-read.csv("train.csv")
test<-read.csv("test.csv")
data <- bind_rows(train,test)
str(data)
###########################################

###########################################
#判断数据中是否存在缺失值（NA和空值）
sapply(data,function(x) sum(is.na(x)))#判断数值型数据
sapply(data,function(x) sum(x==""))#判断类别性数据

#a. Survived变量因子化
data$Survived<-factor(data$Survived)

###探究幸存率与各个变量的关系
#b. PClass变量：探索与Survived的关系
data$Pclass<-factor(data$Pclass)#因子化
prop.table(table(data$Pclass,data$Survived),1)#计算各等级客舱的幸存率
ggplot(data = data[1:nrow(train),],mapping = aes(x=Pclass,fill=Survived))+
  geom_bar(stat = "count",position = "dodge")+
  xlab("Pclass")+
  ylab("Count")+
  ggtitle("How Pclass impacts survival")+
  geom_text(stat = "count",aes(label=..count..),position = position_dodge(width = 1),vjust=-.6)+
  theme_few()



#c. Name变量：将变量中有关Title的信息抽取出来
data$Name<-as.character(data$Name)
data$Title<-sapply(data$Name,FUN = function(x){strsplit(x, split='[,.]')[[1]][2]})
#data$Title <- sub(' ', '', data$Title)

#data$Title<-sapply(data$Name,FUN = function(x){trimws(strsplit(x,split = "[,.]")[[1]][2])})
data$Title[data$Title %in% c("Mme","Mlle")]<-"Mlle"
data$Title[data$Title %in% c("Capt","Don","Major","Sir")]<-"Sir"
data$Title[data$Title %in% c("Dona","Lady","the Countess","Jonkheer")]<-"Lady"
data$Title<-factor(data$Title)
ggplot(data = data[1:nrow(train),],mapping = aes(x=Title,y=..count..,fill=Survived))+
  geom_bar(stat = "count",position = "stack")+
  xlab("Title")+
  ylab("Count")+
  ggtitle("How Title impacts Survival")+
  geom_text(stat = "Count",aes(label=..count..),position = position_stack(vjust = 0.5))+
  theme_few()

#d. Sex变量：转换数据类型，探索与Survived的关系

data$Sex<-factor(data$Sex)
ggplot(data = data[1:nrow(train),],mapping = aes(x=Sex,y=..count..,fill=Survived))+
  geom_bar(stat = "count",position = "dodge")+
  xlab("Sex")+
  ylab("Count")+
  ggtitle("How Sex impacts Survival")+
  geom_text(stat = "count",aes(label=..count..),position = position_dodge(width = 1),vjust=-.5)+
  theme_few()

#e. Age变量：探索与Survived的关系
ggplot(data = data[(!is.na(data$Age)) & row(as.matrix(data[,"Age"]))<=891,],aes(x=Age,color=Survived))+
  geom_line(aes(label=..count..),stat = "bin",binwidth=5,na.rm = TRUE)+
  labs(title="How Age impacts Survival",x="Age",y="Count",fill="Survived")
#f. SibSp变量：探索与Survived的关系
ggplot(data = data[1:nrow(train),],mapping = aes(x=SibSp,y=..count..,fill=Survived))+
  geom_bar(stat = "count",position = "dodge")+
  xlab("SibSp")+
  ylab("Count")+
  ggtitle("How SibSp impacts Survival")+
  geom_text(stat = "count",aes(label=..count..),position = position_dodge(width = 1),vjust=-.5)+
  theme_few()

#g. Parch变量：探索与Survived的关系

ggplot(data = data[1:nrow(train),],mapping = aes(x=Parch,y=..count..,fill=Survived))+
  geom_bar(stat = "count",position = "dodge")+
  xlab("Parch")+
  ylab("Count")+
  ggtitle("How Parch impacts Survival")+
  geom_text(stat = "count",aes(label=..count..),position = position_dodge(width = 1),vjust=-.5)+
  theme_few()

#h. FamilySize变量：新增变量，探索与Survived的关系
data$FamilySize<-data$Parch+data$SibSp+1
ggplot(data = data[1:nrow(train),],mapping = aes(x=FamilySize,y=..count..,fill=Survived))+
  geom_bar(stat = "count",position = "dodge")+
  xlab("FamilySize")+
  ylab("Count")+
  ggtitle("How FamilySize impacts Survival")+
  geom_text(stat = "count",aes(label=..count..),position = position_dodge(width = 1),vjust=-.5)+
  theme_few()

#i. Fare变量：探索与Survived的关系
ggplot(data = data[(!is.na(data$Fare)) & row(as.matrix(data[,"Fare"]))<=891,],aes(x=Fare,color=Survived))+
  geom_line(aes(label=..count..),stat = "bin",binwidth=5,na.rm = TRUE)+
  labs(title="How Fare impacts Survival",x="Fare",y="Count",fill="Survived")
#j. Embarked变量：探索与Survived的关系

ggplot(data = data[1:nrow(train),],mapping = aes(x=Embarked,y=..count..,fill=Survived))+
  geom_bar(stat = "count",position = "dodge")+
  xlab("Embarked")+
  ylab("Count")+
  ggtitle("How Embarked impacts Survival")+
  geom_text(stat = "count",aes(label=..count..),position = position_dodge(width = 1),vjust=-.5)+
  theme_few()

#填补缺失值
#Fare只有1个缺失值，采取中位数填补法

data$Fare[is.na(data$Fare)]<-median(data$Fare,na.rm = TRUE)
#Embarked有2个缺失值，先将这两个缺失值对应的乘客信息选取出来

#data[is.na(data$Fare),c("PassengerId","Pclass","Fare","Embarked")]
#data[data$Pclass==1 & data$Fare==80,c("PassengerId","Pclass","Fare","Embarked")]
data[data$Embarked=="",c("PassengerId","Pclass","Fare","Embarked")]
#发现Pclass都为1，Fare都为80。

ggplot(data = data[data$Embarked!="",],aes(x=Embarked,y=Fare,fill=Pclass))+
  geom_boxplot()+
  geom_hline(aes(yintercept=80),color="red",linetype="dashed",lwd=2)+
  theme_few()

ggplot(data = data[data$Embarked!="",],aes(x=Embarked,y=Fare,fill=Pclass))+
  geom_boxplot()+
  geom_hline(aes(yintercept=80),color="red",linetype="dashed",lwd=2)+
  theme_few()

#发现Embarked为C的Pclass属于1的Fare中位数正好是80，所以将缺失值填补为C

data$Embarked[c(62,830)]<-"C"
data$Embarked<-factor(data$Embarked)

#预测填补Age的缺失值，这里用到了决策树方法，先看一下已有年龄的分布：
hist(data$Age[!is.na(data$Age)],freq = F,main = "Age Distribution")
age.model<-rpart(Age~Pclass+Sex+SibSp+Parch+Fare+Embarked+Title+FamilySize,data = data[!is.na(data$Age),],method = "anova")
data$Age[is.na(data$Age)]<-predict(age.model,data[is.na(data$Age),])
hist(data$Age,freq = F,main = "Age Distribution")

train <- data[1:891,]
test <- data[892:1309,]
set.seed(754)
# 构建预测模型
train1<-train[,c(-1,-4,-8,-9,-11)]
str(train1)
rf_model22 <- randomForest(factor(Survived) ~ Pclass + Sex + Age + Fare+ Embarked + Title + FamilySize + SibSp,data = train1)
test1<-test[,c(-1,-4,-8,-9,-11)]
y<-test1[2,]
y$Sex<-"male"
class(y$Sex)
y$Sex<-factor(y$Sex,levels=c("male","female"))
levels(y$Sex)
prediction <- predict(rf_model22, y)