# Load tidyverse package
library(tidyverse)

# Load the data
pitch_data <- read.csv("PitchData.csv")

# Filter data to only include curveball pitch types (roughly 11% of total observations)
pitch_data <- pitch_data %>% filter(Pitch_Type == "Curveball")

# Convert categorical variables from character to factor data types
pitch_data$Pitcher_Throws <- as.factor(pitch_data$Pitcher_Throws)
pitch_data$Batter_Hits <- as.factor(pitch_data$Batter_Hits)
pitch_data$Pitch_Outcome <- as.factor(pitch_data$Pitch_Outcome)
pitch_data$Pitch_Type <- as.factor(pitch_data$Pitch_Type)

# Remove observations with missing values
pitch_data <- na.omit(pitch_data)

# Count and remove observations with a value of 0 for spin rate... 131 observations removed
sum(pitch_data$release_spin_rate==0)
pitch_data <- pitch_data[!(pitch_data$release_spin_rate==0),]

summary(pitch_data)

# Create "whiff" target variable
pitch_data$whiff <- ifelse(pitch_data$Pitch_Outcome=="StrikeSwinging", 1, 0)
whiff.percent <- sum(pitch_data$whiff) / nrow(pitch_data)

# Select feature variables and create a new dataset with these
features <- c("Inning", "Balls", "Strikes", "Outs", "release_speed", "x_movement", "z_movement", "release_spin_rate", "spin_dir", "release_pos_x", "release_pos_z", "release_extension", "plate_x", "plate_z")
feature_data <- pitch_data %>% select(all_of(features))

# Correlation plot
#install.packages("ggcorrplot")
library(ggcorrplot)
corr <- round(cor(feature_data), 1)
ggcorrplot(corr, hc.order = TRUE) # organize by hierarchial clustering

# Exclude release_pos_x and spin_dir
features <- features[!(features %in% c("release_pos_x", "spin_dir"))]
feature_data <- feature_data %>% select(all_of(features))

# Density plot of curveball whiffs by pitch location
ggplot(pitch_data, aes(plate_x, plate_z))+
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE)+
  xlim(-2, 2)+
  ylim(-.5, 4.5)+
  theme(legend.position='none')+
  annotate("rect", xmin = -.8333, ymin = 1.5, xmax = .8333, ymax = 3.5, color = "red", alpha = 0)+
  labs(title = "Pitch Location of Curveball Whiffs and Non-Whiffs", x = "Horizontal Location (ft)", y = "Vertical Location (ft)")+
  facet_wrap(~whiff, labeller = as_labeller(c('0' = "Non-Whiff", '1' = "Whiff")))

# Create new variable called "Count"
pitch_data$Count <- with(pitch_data, paste(Balls, Strikes, sep = "-"))
pitch_data$Count <- as.factor(pitch_data$Count) # convert from character to factor data type
summary(pitch_data$Count)

# Create bar graph showing the whiff rate for each count
pitch_data %>%
  group_by(Count) %>%
  summarise(whiff_rate = mean(whiff)) %>%
  ggplot(aes(Count, whiff_rate))+
  geom_bar(stat = "identity")+
  theme_classic()+
  labs(title = "Whiff Rates by Count", x = "Count", y = "Whiff Rate")

# Create bar graph showing the whiff rate by number of strikes
pitch_data %>% 
  group_by(Strikes) %>% 
  summarise(whiff_rate = mean(whiff)) %>%
  ggplot(aes(Strikes, whiff_rate))+
  geom_bar(stat = "identity")+
  theme_classic()+
  labs(title = "Whiff Rates by Number of Strikes", x = "Strikes", y = "Whiff Rate")

# Create bar graph showing the whiff rate by number of balls
pitch_data %>% 
  group_by(Balls) %>% 
  summarise(whiff_rate = mean(whiff)) %>%
  ggplot(aes(Balls, whiff_rate))+
  geom_bar(stat = "identity")+
  theme_classic()+
  labs(title = "Whiff Rates by Number of Balls", x = "Balls", y = "Whiff Rate")

# Create bar graph showing the whiff rate by inning, which are grouped into thirds
pitch_data %>% 
  mutate(Stage = case_when(
    Inning %in% 1:3 ~ "Early",
    Inning %in% 4:6 ~ "Middle",
    Inning %in% 7:9 ~ "Late",
    Inning > 9 ~ "Extras"
  ), Stage = factor(Stage, levels = c("Early", "Middle", "Late", "Extras"))) %>% 
  group_by(Stage) %>% 
  summarise(whiff_rate = mean(whiff)) %>%
  ggplot(aes(Stage, whiff_rate))+
  geom_bar(stat = "identity")+
  theme_classic()+
  labs(title = "Whiff Rates by Game Stage", x = NULL, y = "Whiff Rate")

# Create bar graph showing whiff rates by the number of outs
pitch_data %>% 
  group_by(Outs) %>% 
  summarise(whiff_rate = mean(whiff)) %>%
  ggplot(aes(Outs, whiff_rate))+
  geom_bar(stat = "identity")+
  theme_classic()+
  labs(title = "Whiff Rates by Number of Outs", x = "Outs", y = "Whiff Rate")

# Create bar chart showing whiff rates by pitcher/batter handedness
ggplot(pitch_data, aes(Batter_Hits, whiff, group = Pitcher_Throws, fill = Pitcher_Throws))+
  geom_bar(stat = "summary", fun = "mean", position = "dodge")+
  theme_bw()+
  labs(title = "Whiff Rates by Pitcher and Hitter Handedness", x = "Batter Handedness", y = "Whiff Rate", fill = "Pitcher Handedness")

# Removing Outs from features and adding Pitcher_Throws and Batter_Hits
features <- features[!(features == "Outs")]
features <- append(features, c("Pitcher_Throws", "Batter_Hits"))

# Create data containing features and target variable
newdata <- pitch_data %>% select(features, whiff)
colnames(newdata)

# Encode numeric binary variables for Pitcher_Throws and Batter_Hits using fastDummies package
#install.packages("fastDummies")
library(fastDummies)
newdata <- newdata %>% 
  dummy_cols(select_columns = c("Pitcher_Throws", "Batter_Hits")) %>% 
  select(-c("Pitcher_Throws", "Batter_Hits", "Pitcher_Throws_R", "Batter_Hits_R"))

summary(newdata)

# Changing to factors
newdata$whiff <- as.factor(newdata$whiff)
newdata$Pitcher_Throws_L <- as.factor(newdata$Pitcher_Throws_L)
newdata$Batter_Hits_L <- as.factor(newdata$Batter_Hits_L)

# Split data into training and test sets using 80-20 split
set.seed(42)
index <- sample(1:nrow(newdata), .8*nrow(newdata))
train <- newdata[index,]
test <- newdata[-index,]

# Train logistic regression model
lr <- glm(whiff ~ ., data = train, family = "binomial")
summary(lr)

lr.new <- glm(whiff ~ Balls*Strikes + x_movement*z_movement + release_spin_rate + release_pos_z + plate_x*plate_z + Pitcher_Throws_L*Batter_Hits_L, data = train, family = "binomial")
summary(lr.new) # best performing model

# Make predictions on train and test data
lr.predictions.train <- predict(lr.new, train, type = "response")
lr.predictions.test <- predict(lr.new, test, type = "response")

# Converting to binary variable if predicted whiff percentage is greater than league average
train.pred <- ifelse(lr.predictions.train > whiff.percent, 1, 0)
test.pred <- ifelse(lr.predictions.test > whiff.percent, 1, 0)

# Confusion matrix with training predictions vs observed values
lr.conf.matrix.train <- table(train.pred, train$whiff)
lr.conf.matrix.train
lr.pt.train <- prop.table(lr.conf.matrix.train) # proportion table
lr.train.accuracy <- lr.pt.train[1,1] + lr.pt.train[2,2] # 67.1% accurate training data
paste("Logistic Regression Training Accuracy:", lr.train.accuracy)

# Confusion matrix with test predictions vs observed values
lr.conf.matrix.test <- table(test.pred, test$whiff)
lr.conf.matrix.test
lr.pt.test <- prop.table(lr.conf.matrix.test) # proportion table
lr.test.accuracy <- lr.pt.test[1,1] + lr.pt.test[2,2] # 65.2% accurate test data
paste("Logistic Regression Test Accuracy:", lr.test.accuracy)

library(xgboost)

# Separate training features and target
features_train <- train %>% select(-whiff)
features_train$Pitcher_Throws_L <- as.numeric(features_train$Pitcher_Throws_L)
features_train$Batter_Hits_L <- as.numeric(features_train$Batter_Hits_L)
target_train <- as.numeric(as.character(train$whiff))

# Separate test features and target
features_test <- test %>% select(-whiff)
features_test$Pitcher_Throws_L <- as.numeric(features_test$Pitcher_Throws_L)
features_test$Batter_Hits_L <- as.numeric(features_test$Batter_Hits_L)
target_test <- as.numeric(as.character(test$whiff))

# Convert training and test data to 'DMatrix' object
dtrain <- xgb.DMatrix(data = as.matrix(features_train), label = target_train)
dtest <- xgb.DMatrix(data = as.matrix(features_test), label = target_test)

# Parameter to measure learning progress
watchlist <- list(train=dtrain, test=dtest)

# Initial XGBoost model
xgb_new <- xgb.train(
  data=dtrain, 
  max.depth=10, 
  eta=1, 
  nthread = 3, 
  nrounds=20, 
  watchlist=watchlist,
  eval.metric = "logloss",
  objective = "binary:logistic")

# Apply linear boosting
boosted <- xgb.train(
  data=dtrain, 
  booster = "gblinear", 
  nthread = 3, 
  nrounds=20, 
  watchlist=watchlist,
  eval.metric = "logloss",
  objective = "binary:logistic")

# Make predictions on training data
train.predictions <- predict(boosted, dtrain, type = "class")
train.pred.binary <- as.numeric(train.predictions > whiff.percent)
confusion.matrix.train <- table(train.pred.binary, target_train)
confusion.matrix.train
pt.train <- prop.table(confusion.matrix.train)
xgb.train.accuracy <- pt.train[1,1] + pt.train[2,2] # 66.1% accuracy
paste("XGBoost Training Accuracy:", xgb.train.accuracy)

# Make predictions on test data
predictions <- predict(boosted, dtest, type = "class")
pred_binary <- as.numeric(predictions > whiff.percent)
confusion.matrix <- table(pred_binary, target_test) 
confusion.matrix
pt <- prop.table(confusion.matrix)
xgb.test.accuracy <- pt[1,1] + pt[2,2] # 65.6% accuracy
paste("XGBoost Test Accuracy:", xgb.test.accuracy)

# Build feature importance data table
importance <- xgb.importance(feature_names = colnames(dtrain), model = boosted)

# Plot the feature importance
xgb.plot.importance(importance, xlab = "Weight", main = "Feature Importance for XGBoost Model")