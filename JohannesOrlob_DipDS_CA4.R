#load data
library(readxl)
data <- read_excel("Metro_Interstate_Traffic.xlsx")

#check dataset for n/a
apply(data,2,function(x) sum(is.na(x)))

#get dummy variables for day_time
library(fastDummies)
data <- dummy_columns(data)

# Create Vector of Column Max and Min Values
maxs <- apply(data[,4:13], 2, max)
mins <- apply(data[,4:13], 2, min) 

#check new dataset for n/a
apply(data,2,function(x) sum(is.na(x)))

# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(data[,4:13],center = mins, scale =
                                     maxs - mins))
# Check out results
print(head(scaled.data,2))

library(caTools)
set.seed(101)

data_new <- scaled.data

# Create Split (any column is fine)
split = sample.split(data_new$temp, SplitRatio = 0.70)
# Split based off of split Boolean Vector
train = subset(data_new, split == TRUE)
test = subset(data_new, split == FALSE) 
#create fromula for neural net
feats <- list("holiday", "temp", "rain_1h", "snow_1h", "clouds_all","day_time_0_to_5", "day_time_6_to_11",
                "day_time_12_to_17", "day_time_18_to_23")
# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('traffic_volume ~',f)
# Convert to formula
f <- as.formula(f)
f
#create neural net
#install.packages('neuralnet')
library(neuralnet)
nn <- neuralnet(f,train,hidden=c(10,8,6,6,4,2),algorithm = "sag", act.fct="tanh", threshold = 0.15, rep =3, linear.output=T)

#plot the neural network
plot(nn, rep="best")

#compute the predictions on test
predicted.nn.values <- compute(nn,test[1:10]) 

#scale back the results and test
pr.nn_ <- predicted.nn.values$net.result*(max(data$traffic_volume)-min(data$traffic_volume))+min(data$traffic_volume)
test.r <- (test$traffic_volume)*(max(data$traffic_volume)-min(data$traffic_volume))+min(data$traffic_volume)

#compute the mean square error
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test)

print(paste0("Mean square error of: ", MSE.nn))

#plot the real vs predicted values
par(mfrow=c(1,2)) 
plot(test.r,pr.nn_,col='red',main='Real vs predicted Values',pch=18,cex=0.7)
abline(0,1,lwd=2)

#print question
print("Question: What's the trafic volume on 27.09.2018 at 12:00?")
#select the column for the questions and predict using the nn
df.question <- scaled.data[32196, ]
#compute prediction
pr.answer <- compute(nn,df.question[1:10])
#scale backanswer to get a real world result
pr.answer <- pr.answer$net.result*(max(data$traffic_volume)-min(data$traffic_volume))+min(data$traffic_volume)
#print answer
print(paste0("Answer: ", pr.answer))