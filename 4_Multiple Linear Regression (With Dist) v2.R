# Setup ----
setwd("D:/NUS-ISS MTech EBAC/Year 1 Sem 2/5. Practice Project/Data/")
pacman::p_load(readxl, lubridate, tidyr, tidyverse, stringr, ggplot2, RColorBrewer, relaimpo, corrplot, modelr, car, caret)

# Prepare dataset ----
## Import file ----
price = read_excel("resaleflats_merged_final_copy.xlsx")

## Inspect data ----
str(price)
summary(price)

## Convert Variables ----
# Convert selected columns to categorical (nominal)
cols = c('month', 'town', 'flat_type','block', 'street_name', 'storey_range', 'flat_model', 'lease_commence_date', 'remaining_lease', 'addr', 'place_id', 'output', 'geocode_full_addr')
price[,cols] = lapply(price[,cols], factor)

# Inspect data again after conversion
str(price)
summary(price)

# Data Visualization ----
## Box Plot: Resale Price versus Flat Type ----
ggplot(price, aes(x = flat_type, y = resale_price, fill = flat_type)) + 
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_brewer(palette = "Blues") +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Flat Type", y = "Resale Price", title = "Resale Price versus Flat Type") +
  theme(plot.title = element_text(hjust = 0.5))

## Box Plot: Resale Price versus Flat Model in a Specific Flat Type ----
price %>%
  filter(flat_type %in% "4 ROOM") %>%
  ggplot(aes(x = flat_model, y = resale_price, fill = flat_model)) + 
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_brewer(palette = "Blues") +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Flat Type", y = "Resale Price", title = "Resale Price versus Flat Type") +
  theme(plot.title = element_text(hjust = 0.5))

## Box Plot: Resale Price versus Flat Model by Flat Type (Facet Grid) ----
price %>%
  ggplot(aes(x = flat_model, y = resale_price, fill = flat_model)) + 
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_brewer(palette = "Blues") +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Flat Model", y = "Resale Price", title = "Resale Price versus Flat Model") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle=45,hjust = 1)) +
  facet_grid(cols = vars(flat_type))

# Bar Graph (Normal)
ggplot (price, aes(x = flat_type)) +
  geom_bar() +
  theme_bw() +
  labs(x = "Flat Type", y = "Count", title = "Flat Types sold in Punggol from Jan 2017 to June 2023") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.4)

# Multiple Linear Regression ----
## Create subset dataframe ----
price_subset = price[, c('resale_price_per_sqft', 'flat_type', 'storey_range', 'floor_area_sqft', 'flat_model', 'remaining_lease_mnths', 'distance_to_dhoby_ghaut', 'min_dist_prisch', 'min_dist_rest', 'min_dist_mall', 'min_dist_spmkt', 'rating_median', 'vader_median', 'dist_nearest_mrt', 'time_diff')]
price_subset[,'time_diff_sq'] = (price_subset$time_diff)^2

## Inspect data ----
str(price_subset)
summary(price_subset)

## Inspect distribution of target variable ----
# price is right skewed
ggplot(price_subset,aes(resale_price_per_sqft)) + geom_histogram()

# outlier values
outliers = filter(price_subset, (abs(resale_price_per_sqft - median(resale_price_per_sqft)) > 2*sd(resale_price_per_sqft)))
dim(outliers)

### Remove outliers ----
price_subset_new = filter(price_subset, !(abs(resale_price_per_sqft - median(resale_price_per_sqft)) > 2*sd(resale_price_per_sqft)))
dim(price_subset_new)

## Inspect new distribution of target variable ----
ggplot(price_subset_new,aes(resale_price_per_sqft)) + geom_histogram()

## Correlation between independent vars (numerical) ----
cor(price_subset_new[,c("floor_area_sqft", "remaining_lease_mnths", "distance_to_dhoby_ghaut", "min_dist_prisch", "min_dist_rest", "min_dist_mall", "min_dist_spmkt", "rating_median", "vader_median", "dist_nearest_mrt", 'time_diff')])

corrplot(cor(price_subset_new[, sapply(price_subset_new, is.numeric)],
                                    use = "complete.obs"),
                                    method = "number",
                                    type = "lower",
                                    tl.col = "black", 
                                    tl.srt = 45,
                                    tl.cex = 0.9,
                                    number.cex = 0.9,
                                    col = COL2 ('RdYlBu'))

## Initial Model Building ----
### Model 1 ----
model1 = lm(resale_price_per_sqft ~. -time_diff_sq, data = price_subset_new)
vif(model1)
summary(model1)

### Check Residual for Model 1 ----
par(mfrow = c(2,2))
plot(model1)

## Explore Storey Median ----
### Model 1 Storey Median ----
price_subset_median = price[, c('resale_price_per_sqft', 'flat_type', 'storey_median', 'floor_area_sqft', 'flat_model', 'remaining_lease_mnths', 'distance_to_dhoby_ghaut', 'min_dist_prisch', 'min_dist_rest', 'min_dist_mall', 'min_dist_spmkt', 'rating_median', 'vader_median', 'dist_nearest_mrt', 'time_diff')]

outliers_median = filter(price_subset_median, (abs(resale_price_per_sqft - median(resale_price_per_sqft)) > 2*sd(resale_price_per_sqft)))

# Remove outliers
price_subset_new_median = filter(price_subset_median, !(abs(resale_price_per_sqft - median(resale_price_per_sqft)) > 2*sd(resale_price_per_sqft)))

model1_sm = lm(resale_price_per_sqft ~. , data = price_subset_new_median)
vif(model1_sm)
summary(model1_sm)

### Check Residual for Model 1 Storey Median ----
plot(model1_sm)

# No increment in p-value after changing storey from categorical to numerical
# Model 1 = 0.8144 vs Model 1 SM = 0.8087
# Using numerical storey variable is not required 

# Systematic Model Building ----
# set seed
# perform train-test split of data

# set initial seed for reproducibility
set.seed(123)

# collect the data indices returned in a list
inds = createDataPartition(price_subset_new$resale_price_per_sqft, p = 0.7, list = F, times = 1)

## Generate Training Data ----
train_set = price_subset_new[inds,]
nrow(train_set) / nrow(price_subset_new)
dim(train_set)

## Generate Test Data ----
test_set = price_subset_new[-inds,]
nrow(test_set) / nrow(price_subset_new)
dim(test_set)

## Train Set ----
### Model 2 ----
model2_td1 = lm(resale_price_per_sqft ~. -time_diff_sq, data = train_set)
vif(model2_td1)
summary(model2_td1)
plot(model2_td1)

model2_td2 = lm(resale_price_per_sqft ~. -time_diff , data = train_set)
vif(model2_td2)
summary(model2_td2)
plot(model2_td2)

model2_td12 = lm(resale_price_per_sqft ~. , data = train_set)
vif(model2_td12)
summary(model2_td12)
plot(model2_td12)

### Model 3 ----
# Remove flat type even though floor_area_sqft VIF = 10.6 (high correlation)
model3 = lm(resale_price_per_sqft ~. -flat_type, data = train_set)
vif(model3)
summary(model3)
plot(model3)

### Model 4 ----
# Remove min_dist_mall as p-value = 0.68961 > 0.05 (insignificant variable)
model4 = lm(resale_price_per_sqft ~. -flat_type -min_dist_mall, data = train_set)
vif(model4)
summary(model4)
plot(model4)

# KS-Test
res = residuals(model4)
ks.test(res, 'pnorm')

### Relative Importance of Various Predictors ----
imp = calc.relimp(model4, type = c("lmg"), rela = TRUE)

(imp = data.frame(lmg = imp$lmg,
                  vars = names(imp$lmg),
                  row.names = NULL))

# Visualize the order
imp %>%
  ggplot(aes(x = reorder(vars,-lmg), y = lmg)) +
  geom_bar(stat = 'identity')

## Test Set ----
# testing
# test the learned model on test data

predictTest = predict(model4, newdata = test_set)
predictTest

### Model Evaluation ---- 
# Goodness of Fit: R-Square
rsquare = R2(predictTest, test_set$resale_price_per_sqft)
print(paste("R-Square =", rsquare))

# Errors
root_mse = RMSE(predictTest, test_set$resale_price_per_sqft)
print(paste("RMSE =", root_mse))

mean_absolute = MAE(predictTest, test_set$resale_price_per_sqft)
print(paste("MAE =", mean_absolute))

## Auto Tune ----
modelAT = stepAIC(model2_td12)
summary(modelAT)

### Test Set ----
predictTest_AT = predict(modelAT, newdata = test_set)

### Model Evaluation ----
rsquare_AT = R2(predictTest_AT, test_set$resale_price_per_sqft)
print(paste("R-Square =", rsquare_AT))

root_mse_AT = RMSE(predictTest_AT, test_set$resale_price_per_sqft)
print(paste("RMSE =", root_mse_AT))

mean_absolute_AT = MAE(predictTest_AT, test_set$resale_price_per_sqft)
print(paste("MAE =", mean_absolute_AT))

# Predict selected data row ----
input_data = price_subset[11498,]
predict(model4, newdata = input_data)