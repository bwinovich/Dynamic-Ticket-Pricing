library(readr)
p <- read_csv("primary_opp.csv")
s <- read_csv("secondary_opp.csv")

#create days to game variable
TimeToEvent <- as.data.frame((p[,"EventDate"]-p[,"SaleDate"])/24/60)
names(TimeToEvent)[1] <- "DaysToGame" 
p <- cbind(p,TimeToEvent)
rm(TimeToEvent)

#replace purchase price with per ticket price

s$te_purchase_price <- s$te_purchase_price/s$num_seats

#find average sale price for every row in the stadium
library(dplyr)
avgprice <- p %>%
  group_by(SectionName, RowName) %>% 
  summarise_each(funs(mean))
avgprice <- avgprice[,c("SectionName","RowName","PurchasePrice")]
names(avgprice)[3] <- "AvgPurchasePriceForRow"

#add avgprice variable into p
p <- merge(p, avgprice, by=c("SectionName","RowName"))

#compare purchase price to average for that section & row
PriceVsAvg <- as.data.frame((p[,"PurchasePrice"]/p[,"AvgPurchasePriceForRow"]))
p <- cbind(p,PriceVsAvg)
rm(PriceVsAvg)

#keep resale rows only in s
s <- filter(s, activity_name == "TE Resale")

#repeat the above for s
TimeToEvent <- as.data.frame((s[,"event_date"]-s[,"add_datetime"])/60/60/24)
names(TimeToEvent)[1] <- "DaysToGame" 
s <- cbind(s,TimeToEvent)
s$DaysToGame <- as.numeric(s$DaysToGame)
rm(TimeToEvent)

#### Make table for average price per game
avgprice_game <- s %>%
  group_by(EventCode) %>% 
  summarise_each(funs(mean))
avgprice_game <- avgprice_game[,c("EventCode","te_purchase_price")]

### Make table for average price per row + section
avgprice_s <- s %>%
  group_by(section_name, row_name) %>% 
  summarise_each(funs(mean))
avgprice_s <- avgprice_s[,c("section_name","row_name","te_purchase_price")]
names(avgprice_s)[3] <- "AvgPurchasePriceForRow_s"

avgprice_s = avgprice_s[1:3]
s <- merge(s, avgprice_s, by=c("section_name","row_name"))

#divide each purchase price by the average for that row + section
PriceVsAvg_s <- as.data.frame((s[,"te_purchase_price"]/s[,"AvgPurchasePriceForRow_s"]))
s <- cbind(s,PriceVsAvg_s)
names(s)[48] <- "Price/Avg_s"
rm(PriceVsAvg_s)

#clean up data
names(s)[names(s)=="event_name"] <- "EventCode"
names(s)[names(s)=="section_name"] <- "SectionName"
names(s)[names(s)=="row_name"] <- "RowName"
p <- p[,c(1:2,5:ncol(p))]
names(p)[40] <- "Price/Avg"
s <- s[,c(1:2,5:ncol(s))]

#count how many times each row appears in s. remove rows which appear fewer than 20 times
count <- s %>%
  group_by(SectionName, RowName) %>% 
  summarise_each(funs(n()))
count <- count[,1:3]
s <- merge(s, count, by=c("SectionName","RowName"))
s <- filter(s, s[,"index.y"] > 29)
s <- s[,1:46]

#secondary market prices vs. primary market prices
library(ggplot2)
g <- ggplot() + 
  geom_point(data = p, aes(SectionName,AvgPurchasePriceForRow,color = "primary")) + 
  geom_point(data = s, aes(SectionName,AvgPurchasePriceForRow_s,color = "secondary"))
g

#average ticket price per game
game <- ggplot(data= avgprice_game, aes(x = reorder(EventCode,-te_purchase_price),y = te_purchase_price)) + 
  geom_bar(stat = "identity") 
game

#table for average price per row + section grouped by year
avgprice_year <- s %>%
  group_by(SectionName, RowName,season_year) %>% 
  summarise_each(funs(mean))
avgprice_year <- avgprice_year[,c("SectionName","RowName","season_year","te_purchase_price")]
names(avgprice_year)[4] <- "AvgPurchasePriceForRowYear_s"
avgprice_year$"rowsectionyear" = paste(avgprice_year$RowName, avgprice_year$SectionName,avgprice_year$season_year, sep = "_")
sectionyear <- ggplot(data= avgprice_year, aes(x = reorder(rowsectionyear,season_year),y = AvgPurchasePriceForRowYear_s, color = season_year)) + 
  geom_bar(stat = "identity") 
sectionyear

#average ticket price per section+row
avgprice_s$"rowsection" = paste(avgprice_s$row_name, avgprice_s$section_name, sep = "_")
section <- ggplot(data= avgprice_s, aes(x = reorder(rowsection,-AvgPurchasePriceForRow_s),y = AvgPurchasePriceForRow_s)) + 
  geom_bar(stat = "identity") 
section

library(caret)

#convert factors and characters to proper form
f <- c("SectionName","RowName","index.x","seat_num","last_seat","season_year", "Season",
       "Top25Jersey(Ordinal)")
for (i in 1:length(f)){
  s[,f[i]] <- as.factor(s[,f[i]])
}
str(s)

chr <- c("acct_id","assoc_acct_id")

for (i in 1:length(chr)){
  s[,chr[i]] <- as.character(s[,chr[i]])
}
str(s)

#center and scale

pre <- preProcess(s[,c(1:8,10:29,31,34:36,38:41,43:44)], method = c("center","scale"), na.remove = TRUE)
s2 <- predict(pre, s[,c(1:8,10:29,31,34:36,38:41,43:44)])
s2 <- cbind(s2, s[,c(9,30,32,33,37,42,45:46)])

#tag primetime games
month <- format(s2$event_date,"%m")
day <- format(s2$event_date, "%d")
year <- format(s2$event_date, "%y")
Thursday <- as.data.frame(ifelse(month == "12" & day == "14" & year == "17",1,0))
Monday <- as.data.frame(ifelse(month == "09" & day == "15" & year == "14" 
                               |month == "09" & day == "21" & year == "15",1,0))
Sunday = as.data.frame(ifelse(month == "10" & day == "20" & year == "13"
                              |month == "11" & day == "16" & year == "14"
                              |month == "10" & day == "18" & year == "15"
                              |month == "11",1,0))
Night_Game = Thursday + Monday + Sunday
s2 <- cbind(s2,c(Thursday,Monday,Sunday))
names(s2)[47:49] <- c("Thursday_Night","Monday_Night","Sunday_Night")

#tag games by month
September = as.data.frame(ifelse(month == "09",1,0))
October = as.data.frame(ifelse(month == "10",1,0))
November = as.data.frame(ifelse(month == "11",1,0))
December = as.data.frame(ifelse(month == "12" | month == "01",1,0))
s2 <- cbind(s2,September,October,November)
names(s2)[50:52] <- c("Sep","Oct","Nov")

#tag Thanksgiving day games
Thanksgiving = as.data.frame(ifelse(month == "11" & day == "24" & year == "16",1,0))
s2 <- cbind(s2,Thanksgiving)
names(s2)[53] <- "Thanksgiving"

#tag games against Peyton Manning
Peyton = as.data.frame(ifelse(month == "10" & day == "20" & year == "13"
                              |month == "11" & day == "08" & year == "15",1,0))
s2 <- cbind(s2,Peyton)
names(s2)[54] <- "Peyton"

rm(September,October,November,December,Thursday,Monday,Sunday,Thanksgiving,Peyton,Night_Game)

#s2 <- cbind(s2,Night_Game)
#preseason <- as.data.frame(ifelse(month == "08" | month == "09" & day == "01" | month == "09" & day == "03", 1, 0))
#s2 <- cbind(s2, preseason)

#names(s2)[46] <- "preseason"
#s2$preseason <- as.factor(s2$preseason)
str(s2)

s3 <- Filter(is.numeric, s2)
#s3 <- s3[1:17]
str(s3)

#remove highly correlated inputs
descrCorr <- cor(s3, use = "complete.obs")
highCorr <- sum(abs(descrCorr[upper.tri(descrCorr)]) < 0.75)
summary(descrCorr[upper.tri(descrCorr)])

highlycorDescr <- findCorrelation(descrCorr, cutoff = 0.75)
filteredDescr <- s3[,-highlycorDescr]
descrCorr2 <- cor(filteredDescr, use = "complete.obs")
summary(descrCorr2[upper.tri(descrCorr2)])

str(s3)

#drop highly correlated variables: te_posting_price, OppWin, FacebookFans as well as row purchase price,
#num_seats, and Orig_purchase_price
drop <- c("te_posting_price","OppWin","FacebookFans","te_purchase_price","num_seats")
s3 <- s3[ , !names(s3) %in% drop]

rm(filteredDescr, descrCorr, descrCorr2, highCorr, highlycorDescr, drop)

#drop NAs and remove spaces from column names
s3 <- na.omit(s3)
str(s3)
names(s3)[20] <- "Price"
s3
names(s3)[c(4,14,16,17)] = c("Temp_at_Kick","Home_Opener","Colts_Out_of_Contention","OppPlayoff(PrevBin)")

#create data partition
set.seed(12345)
inTrain <- createDataPartition(y = s3$Price, p = 0.8, list = F)

train <- s3[inTrain,]
test <- s3[-inTrain,]
train_info <- train[,c(13,19)]
test_info <- test[,c(13,19)]
train <- train[,c(1:12,14:18,20:28)]
test <- test[,c(1:12,14:18,20:28)]

ctrl <- trainControl(method = "cv", number = 5)

#linear regression
linear <- train(Price ~ .,
                data = train,
                method = "lm",
                trControl = ctrl)
linear
summary(linear)
linpred <- predict(linear, test)
R2(linpred, test$Price)

#boosted linear regression
Boostedlinear <- train(Price ~., data = train, method = "xgbLinear",trControl = ctrl)
summary(Boostedlinear)
boostpred <- predict(Boostedlinear, test)
R2(boostpred, test$Price)

#gradient boosted decision tree
xgbGrid <- expand.grid(nrounds = c(100,150,200),
                       max_depth = c(3,4,7),
                       eta = c(.1,.3,.4),
                       gamma = 0,
                       colsample_bytree = c(.7,.8),
                       min_child_weight = 1,
                       subsample = 1)

#this grid contains the model which performed best
finalGrid <- expand.grid(nrounds = 100, max_depth = 7, eta = 0.1, gamma = 0, colsample_bytree = 0.8, min_child_weight = 1, subsample = 1)

#nonlinear <- train(Price ~., data = train, method = "xgbTree", trControl = ctrl, tuneGrid = xgbGrid)
#nonlinear

finalnl <- train(Price ~., data = train, method = "xgbTree", trControl = ctrl, tuneGrid = finalGrid)
finalnl

#generate predictions
predtrain <- predict(finalnl, train)
predtest <- predict(finalnl, test)
R2(predtest, test$Price)

#add orig_purchase_price and AvgPurchasePriceForRow_s back to the data; combine train2 and test2
train2 <- cbind(train,train_info,predtrain)
test2 <- cbind(test,test_info,predtest)
names(train2)[29] <- "pred"
names(test2)[29] <- "pred"

s4 <- rbind(train2,test2)

#calculate recommended price based on model and potential revenue gain based on that price and the
#actual original purchase price
s4$'rec_price' <- s4$AvgPurchasePriceForRow_s*s4$pred 
s4$'revenue_gain' <- s4$rec_price - s4$Orig_purchase_price

#calculate total potential revenue gain
revenue <- sum(s4$revenue_gain)
revenue
