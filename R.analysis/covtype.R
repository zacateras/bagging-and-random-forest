# install.packages('caret')
# install.packages('rpart')
# install.packages('e1071')
# install.packages('onehot')
# install.packages('MLmetrics')

library('caret')
library('rpart')
library('e1071')
library('onehot')
library('MLmetrics')

source('./R/ensemble.R')
source('./R/predict.ensemble.R')

source('./R.analysis/tooling.R')

set.seed(1234)

df <- data.frame(read.csv('data/covtype.data', head=FALSE, sep=','))

# categorize one-hot encoded features
for (i in 2:4) { df[['V11']] <- df[['V11']] + (i * df[[paste0('V', 11 + (i - 1))]]) }
for (i in 2:4) { df[[paste0('V', 11 + (i - 1))]] <- NULL }
df[['V11']] <- factor(df[['V11']])

for (i in 2:40) { df[['V15']] <- df[['V15']] + (i * df[[paste0('V', 15 + (i - 1))]]) }
for (i in 2:40) { df[[paste0('V15', 11 + (i - 1))]] <- NULL }
df[['V15']] <- factor(df[['V15']])

# the model should be trained on factorized version of y
df$V55 <- factor(df$V55)

# keep only meaningful features
df <- df[c('V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'V8', 'V9', 'V10', 'V11', 'V15', 'V55')]

colnames(df) <- c(
  'Elevation_in_meters',
  'Aspect_in_degrees_azimuth',
  'Slope_in_degrees',
  'Horz_Dist_to_nearest_surface_water_features',
  'Vert_Dist_to_nearest_surface_water_features',
  'Horz_Dist_to_nearest_roadway',
  'Hillshade_index_at_9am_summer_solstice',
  'Hillshade_index_at_noon_summer_soltice',
  'Hillshade_index_at_3pm_summer_solstice',
  'Horz_Dist_to_nearest_wildfire_ignition_points',
  'Wilderness_area_designation_discrete',
  'Soil_Type_designation_discrete',
  'Cover_Type'
)

# check datatypes
sapply(df, class)

# summary
summary(df)

rpart_fun <- function(formula, data) rpart(formula, data=data)
naiveBayes_fun <- function(formula, data) naiveBayes(formula, data=data)

Train_And_Test_Ensemble(df, 'covtype', rpart_fun, 'rpart', 'Cover_Type')
