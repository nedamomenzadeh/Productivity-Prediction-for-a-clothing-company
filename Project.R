rm(list=ls(all.names=TRUE))
library(mlr)
library(tidyr)
library(randomForest)
library(caret)
library(DataExplorer)
library(dplyr)
library(cowplot)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
#import data and remove spaces in categorical variables
df = read.csv(file = 'garments_worker_productivity.csv',strip.white=TRUE)
df$date = as.Date(df$date,"%m/%d/%Y")

#Visualizing data
plot_intro(df)
plot_missing(df)

#Imputing 42% missing values with col median
df$wip[is.na(df$wip)] = median(df$wip, na.rm=TRUE)
sorted = df[with(df, order(department,team)), ]
TS_data = split(sorted, list(sorted$department,sorted$team), drop = TRUE)
a = df %>% group_by(department,team)
b = a %>% summarise(n = n())
print(as.data.frame(t(b)))

finishing1 = ggplot(data = TS_data$finishing.1, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
finishing2 = ggplot(data = TS_data$finishing.2, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
finishing3 = ggplot(data = TS_data$finishing.3, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
finishing4 = ggplot(data = TS_data$finishing.4, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
finishing5 = ggplot(data = TS_data$finishing.5, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
finishing6 = ggplot(data = TS_data$finishing.6, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
finishing7 = ggplot(data = TS_data$finishing.7, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
finishing8 = ggplot(data = TS_data$finishing.8, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
finishing9 = ggplot(data = TS_data$finishing.9, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
finishing10 = ggplot(data = TS_data$finishing.10, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
finishing11 = ggplot(data = TS_data$finishing.11, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
finishing12 = ggplot(data = TS_data$finishing.12, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
l1 = list('finishing1','finishing2','finishing3','finishing4','finishing5',
          'finishing6','finishing7','finishing8','finishing9','finishing10',
          'finishing11','finishing12')
plot_grid(finishing1,finishing2,finishing3,finishing4,finishing5,
          finishing6,finishing7,finishing8,finishing9,finishing10,
          finishing11,finishing12, labels = l1)

sweing1 = ggplot(data = TS_data$sweing.1, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
sweing2 = ggplot(data = TS_data$sweing.2, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
sweing3 = ggplot(data = TS_data$sweing.3, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
sweing4 = ggplot(data = TS_data$sweing.4, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
sweing5 = ggplot(data = TS_data$sweing.5, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
sweing6 = ggplot(data = TS_data$sweing.6, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
sweing7 = ggplot(data = TS_data$sweing.7, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
sweing8 = ggplot(data = TS_data$sweing.8, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
sweing9 = ggplot(data = TS_data$sweing.9, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
sweing10 = ggplot(data = TS_data$sweing.10, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
sweing11 = ggplot(data = TS_data$sweing.11, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)
sweing12 = ggplot(data = TS_data$sweing.12, aes(x = date, y = actual_productivity)) + 
  geom_line(color = "#3CC96F", size = 1)

l2 = list('sweing1','sweing2','sweing3','sweing4','sweing5',
          'sweing6','sweing7','sweing8','sweing9','sweing10',
          'sweing11','sweing12')
plot_grid(sweing1,sweing2,sweing3,sweing4,sweing5,
          sweing6,sweing7,sweing8,sweing9,sweing10,
          sweing11,sweing12, labels = l2)


df = separate(df, "date", c("Year", "Month", "Day"), sep = "-")
#Visualizing data
plot_bar(df)
plot_bar(df, by = "department")
plot_histogram(df)
plot_correlation(na.omit(df), maxcat = 31L)
plot_scatterplot(na.omit(df),by="incentive")
plot_scatterplot(na.omit(df),by="no_of_workers")

df$diff = df$actual_productivity - df$targeted_productivity
Date = df[,c(1,2,3)]
df = df[,-c(1,2,3)]

df$class = 0
for (i in 1:dim(df)[1]) {
  if(df[i,'diff']>=0){df[i,'class']=1}
  else {df[i,'class']=-1}
}
#removing variables that are used to build label
df = df[,-c(5,14,15)] 
dmy = dummyVars(" ~ .", data = df, fullRank = T)
df = data.frame(predict(dmy, newdata = df))

# Split the data into training and testing set
index = sample(1:nrow(df), round(0.7 * nrow(df)))
train = df[index,]
test = df[-index,]
train$class = as.factor(train$class)
test$class = as.factor(test$class)

#Tree

tr = makeClassifTask(id = "train", data = train, target = "class") 
te = makeClassifTask(id = "test", data = test, target = "class")
lrn = makeLearner("classif.rpart") 
ps = makeParamSet(
  makeNumericParam("cp", lower = 0, upper = 0.2),
  makeIntegerParam("minsplit", lower = 3, upper = 10),
  makeIntegerParam("minbucket", lower = 3, upper = 10),
  makeIntegerParam("maxdepth", lower = 5, upper = 20)
)
control.grid = makeTuneControlGrid()
resamp = makeResampleDesc("CV", iters = 5L) #Setting up cross validation
tuned = tuneParams(lrn, task = tr,resampling = resamp,
                   control = control.grid,par.set = ps, measures = list(acc))
tree.tuned = setHyperPars(learner = lrn, par.vals = tuned$x)
tree = mlr::train(tree.tuned, task = tr)
tree.rpart = getLearnerModel(tree)
rpart.plot(tree.rpart,box.palette = "RdYlGn", roundint = FALSE,tweak = 1.9) 
tree.pred = predict(tree, te)
preddata  = tree.pred$data
print(confusionMatrix(factor(preddata$response),factor(preddata$truth)))

#RandomForest
tr = makeClassifTask(id = "train", data = train, target = "class") 
te = makeClassifTask(id = "test", data = test, target = "class")
lrn = makeLearner("classif.randomForest") 
ps = makeParamSet(
  makeDiscreteParam("ntree",values = list(a = 100,b=500,c=800) ),
  makeDiscreteParam("classwt", values = list(b=c(0.269,0.731))),
  makeDiscreteParam("sampsize", values = list(a=400,b=500,c=600)),
  makeDiscreteParam("maxnodes",values = list(a=5,b=10,c=15,d=20,e=30,f=50)),
  makeLogicalParam("importance")
)
control.grid = makeTuneControlGrid()
resamp = makeResampleDesc("CV", iters = 5L) #Setting up cross validation
tuned = tuneParams(lrn, task = tr,resampling = resamp,
                   control = control.grid,par.set = ps, measures = list(acc))
rf.tuned = setHyperPars(learner = lrn, par.vals = tuned$x)
rf = mlr::train(rf.tuned, task = tr)
rf.pred = predict(rf, te)
preddata  = rf.pred$data
print(confusionMatrix(factor(preddata$response),factor(preddata$truth)))
