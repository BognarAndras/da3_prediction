
library(tidyverse)
library(modelsummary)
library(kableExtra)
library(ggpubr)
library(fixest)
library(xtable)
library(caret)
library(data.table)
library(cowplot)
library(knitr)

# Load data

cps_earnings <- read_csv("https://osf.io/4ay9x/download")

# Check how many missing

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# DROP Y MISSING

data <- data %>%
  drop_na(price)


# IMPUTE IF FEW

data <- data %>%
  mutate(
    n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms), #assume at least 1 bath
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds)
  )

# Drop if many missing but dont care

to_drop <- c("usd_cleaning_fee", "p_host_response_rate","d_reviews_per_month")
data <- data %>%
  select(-one_of(to_drop))


# Many missing + important: replace but add flag to see if it matters if missing

data <- data %>%
  mutate(
    flag_days_since = ifelse(is.na(n_days_since),1, 0),
    n_days_since    =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    flag_review_scores_rating = ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating    = ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_per_month    = ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month       = ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month)
  )

# To check again how many were missing

datasummary( factor(flag_days_since) + factor(flag_review_scores_rating) + factor(flag_reviews_per_month) ~ N , data )

# Functional forms

data <- data %>%
  mutate(
    ln_days_since = log(n_days_since+1),
    ln_days_since2 = log(n_days_since+1)^2,
    ln_days_since3 = log(n_days_since+1)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3,
    ln_review_scores_rating = log(n_review_scores_rating),
    ln_days_since=ifelse(is.na(ln_days_since),0, ln_days_since),
    ln_days_since2=ifelse(is.na(ln_days_since2),0, ln_days_since2),
    ln_days_since3=ifelse(is.na(ln_days_since3),0, ln_days_since3),
  )

# Check y

summary(data$price)

# Should have no missing

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# Drop those you cant predict, too unique

data <- data %>%
  filter(n_accommodates < 8
  )

# Check here

skimr::skim(data)

# Check which variables seem to be important, also to see which to group
# If small and relatively same, group, interaction
datasummary( f_property_type*f_room_type*price + f_bed_type*price ~ Mean + SD + P25 + P75 + N, data = data )
datasummary( f_property_type*f_room_type*f_bed_type*price ~ Mean + SD + P25 + P75 + N, data = data )


# Check y, do you need log

ggplot(data=data, aes(x=price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 10, boundary=0,
                 fill = 'navyblue', color = 'white', size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  coord_cartesian(xlim = c(0, 600)) +
  labs(x = "Price (US dollars)",y = "Percent")+
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.03), labels = scales::percent_format(1)) +
  scale_x_continuous(expand = c(0.00,0.00),limits=c(0,400), breaks = seq(0,400, 50)) +
  theme_bw() 

ggplot(data=datau, aes(x=ln_price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.18,
                 color = 'white', fill = 'navyblue', size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  coord_cartesian(xlim = c(2.5, 6.5)) +
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.05), labels = scales::percent_format(5L)) +
  scale_x_continuous(expand = c(0.00,0.01),breaks = seq(2.4,6.6, 0.6)) +
  labs(x = "ln(price, US dollars)",y = "Percent")+
  theme_bw() 


# Interaction

# For small categorical use source file

source("Interaction_graph.R")

p1 <- wage_diff_by_variables(earnings_it, "no_child", 
                             "female" ,
                             "Number of Children" ,"Gender (Blue female)")
p2 <- wage_diff_by_variables(earnings_it, "poc", "foreign_born" ,
                             "Person of color (1 Yes)" ,"Foreigner (Blue yes)")
p3 <- wage_diff_by_variables(earnings_it, "age", "poc" ,  
                             "Age" ,"Person of color (Blue Yes)")
p4 <- wage_diff_by_variables(earnings_it, "age", "foreign_born" , 
                             "Age" ,"Foreigner (Blue yes)")
p5 <- wage_diff_by_variables(earnings_it, "age", "female" ,  
                             "Age" ,"Gender (Blue female")
p6 <- wage_diff_by_variables(earnings_it, "poc", "female" , 
                             "Person of color (1 Yes)" ,"Gender (Blue female")

g_interactions <- plot_grid(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2)
g_interactions

# For x with big variance use boxplot

ggplot(datau, aes(x = factor(n_accommodates), y = price,
                  fill = factor(f_property_type), color=factor(f_property_type))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Accomodates (Persons)",y = "Price (US dollars)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 400), breaks = seq(0,400, 50))+
  theme_bw() +
  theme(legend.position = c(0.3,0.8)        )


# Group variables


basic_lev  <- c("n_accommodates", "n_beds", "f_property_type", "f_room_type", "n_days_since", "flag_days_since")

basic_add <- c("f_bathroom","f_cancellation_policy","f_bed_type")

reviews <- c("f_number_of_reviews","n_review_scores_rating", "flag_review_scores_rating")

poly_lev <- c("n_accommodates2", "n_days_since2", "n_days_since3")

amenities <-  grep("^d_.*", names(data), value = TRUE)


# Interaction dummies 

X1  <- c("f_room_type*f_property_type",  "f_room_type*d_familykidfriendly")
X2  <- c("d_airconditioning*f_property_type", "d_cats*f_property_type", "d_dogs*f_property_type")
X3  <- c(paste0("(f_property_type + f_room_type + f_cancellation_policy + f_bed_type) * (",
                paste(amenities, collapse=" + "),")"))


# Modelling

# Model levels based on complexity

modellev1 <- " ~ n_accommodates"
modellev2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_lev, basic_add,reviews),collapse = " + "))
modellev4 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2),collapse = " + "))
modellev7 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities),collapse = " + "))
modellev8 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities,X3),collapse = " + "))


# Holdout set

smp_size <- floor(0.2 * nrow(data))
set.seed(20180123)
holdout_ids <- sample( seq_len( nrow( data ) ) , size = smp_size )
data$holdout <- 0
data$holdout[holdout_ids] <- 1
data_holdout <- data %>% filter(holdout == 1)
data_work <- data %>% filter(holdout == 0)

# Checking models on train-test data


## K = 5
k_folds <- 5
# Define seed value
seed_val <- 20210117

# Do the iteration
for ( i in 1:8 ){
  print(paste0( "Estimating model: " ,i ))
  # Get the model name
  model_name <-  paste0("modellev",i)
  model_pretty_name <- paste0("M",i,"")
  # Specify the formula
  yvar <- "price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))
  
  # Estimate model on the whole sample
  model_work_data <- feols( formula , data = data_work , vcov='hetero' )
  #  and get the summary statistics
  fs  <- fitstat(model_work_data,c('rmse','r2','bic'))
  BIC <- fs$bic
  r2  <- fs$r2
  rmse_train <- fs$rmse
  ncoeff <- length( model_work_data$coefficients )
  
  # Do the k-fold estimation
  set.seed(seed_val)
  cv_i <- train( formula, data_work, method = "lm", 
                 trControl = trainControl(method = "cv", number = k_folds))
  rmse_test <- mean( cv_i$resample$RMSE )
  
  # Save the results
  model_add <- tibble(Model=model_pretty_name, Coefficients=ncoeff,
                      R_squared=r2, BIC = BIC, 
                      Training_RMSE = rmse_train, Test_RMSE = rmse_test )
  if ( i == 1 ){
    model_results <- model_add
  } else{
    model_results <- rbind( model_results , model_add )
  }
}

# Check summary table END of OLS
model_results

# LASSO
# Most complex model
vars_model_8 <- c("price", basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities,X3)

# Tuning params
train_control <- trainControl( method = "cv", number = k_folds)
tune_grid     <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))
formula <- formula(paste0("price ~ ", paste(setdiff(vars_model_8, "price"), collapse = " + ")))

# Run LASSO
set.seed(seed_val)
lasso_model <- caret::train(formula,
                            data = data_work,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            na.action=na.exclude)
# Check the output
lasso_model
# Best lambda generated
lasso_model$bestTune
# Just lambda
lasso_model$bestTune$lambda
# Check the RMSE curve also shows best lambda visually
plot(lasso_model)

# Look behind hood, see model performance 


# coefficients as 
lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `s1`)  

print(lasso_coeffs)

# number of coefficients not 0
lasso_coeffs_nz<-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))

# Get the RMSE of the Lasso model 
#   Note you should compare this to the test RMSE
lasso_fitstats <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) 
lasso_fitstats

# Add this to OLS table to compare with OLS results

lasso_add <- tibble(Model='LASSO', Coefficients=nrow(lasso_coeffs_nz),
                    R_squared=lasso_fitstats$Rsquared, BIC = NA, 
                    Training_RMSE = NA, Test_RMSE = lasso_fitstats$RMSE )
model_results <- rbind( model_results , lasso_add )
model_results



# Evaluate performance on the hold-out sample
# Let us check only Models: 3, 7 and LASSO

# we need to re-run Model 3 and 7 on the work data
m3 <- feols( formula(paste0("price",modellev3)) , data = data_work, vcov = 'hetero' )
m7 <- feols( formula(paste0("price",modellev7)) , data = data_work, vcov = 'hetero' )

# Make prediction for the hold-out sample with each models
m3_p <- predict( m3 , newdata = data_holdout )
m7_p <- predict( m7 , newdata = data_holdout )
mL_p <- predict( lasso_model , newdata = data_holdout )

# Calculate the RMSE on hold-out sample
m3_rmse <- RMSE(m3_p,data_holdout$price)
m7_rmse <- RMSE(m7_p,data_holdout$price)
mL_rmse <- RMSE(mL_p,data_holdout$price)
# Create a table
sum <- rbind(m3_rmse,m7_rmse,mL_rmse)
rownames(sum) <- c('Model 3','Model 7','LASSO')
colnames(sum) <- c('RMSE on hold-out sample')
sum

# DIAGNOSTICS
# LASSOS Y YHAT

data_holdout$predLp <- mL_p

ggplot( data_holdout , aes( y = price , x = predLp ) ) +
  geom_point( size = 1 , color = 'blue' ) +
  geom_abline( intercept = 0, slope = 1, size = 1, color = 'green' , linetype = 'dashed') +
  xlim(-1,max(data_holdout$price))+
  ylim(-1,max(data_holdout$price))+
  labs(x='Predicted price (US$)',y='Price (US$)')+
  theme_bw()
