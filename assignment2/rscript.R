
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

data <- read_csv("https://raw.githubusercontent.com/BognarAndras/da3_prediction/main/assignment2/raw/listings.csv")

# Cleaning up variables

unique(data$property_type) 

datasummary(property_type ~ N + Percent(), data = data )

head(as.integer(gsub("[$]" , "" , gsub("," , "", data$price))))
head(data$price)

datau <- data.table(data)
datau$price_fixed <- as.integer(gsub("[$]" , "" , gsub("," , "", datau$price)))

unique(datau$beds)
datau[ , .('mean_price' = mean(price_fixed , na.rm=TRUE) , .N) , by =property_type ]
datau[ , .('mean_bedrooms' = mean(bedrooms , na.rm=TRUE) , .N) , by =property_type ]
# Entire condominium (condo) 1447 NO
# Entire loft	596 NO
# Entire rental unit	13474 YES
# Entire residential home	1054 NO
# Entire serviced apartment	655 YES

# Private room in bed and breakfast	103 NO
# Private room in condominium (condo)	430 NO 
# Private room in rental unit	3258 YES
# Private room in residential home	1029 NO
# Private room in serviced apartment 61 YES

# Room in hotel	Room in boutique hotel	112+43 NO
# Shared room in rental unit	201 NO
# Shared room in residential home	160 NO

data <- data %>%
  filter(property_type %in% c("Entire rental unit" , "Entire serviced apartment" ,
                             "Private room in rental unit" ,
                             "Private room in serviced apartment"))

data <- data %>%
  mutate(f_property_type = factor(property_type))

data$f_property_type <- factor(ifelse(data$f_property_type== "Entire rental unit", 
                                     "Entire/Unit",
                              ifelse(data$f_property_type== "Entire serviced apartment",
                                     "Entire/Apt",
                              ifelse(data$f_property_type== "Private room in serviced apartment",
                                     "Room/Apt",
                              ifelse(data$f_property_type== "Private room in rental unit", 
                                     "Room/Unit", ".")))))


data <- data %>%
  filter(accommodates > 1 & accommodates < 7)

data <- data %>%
  mutate(n_accommodates = accommodates)



datasummary(room_type ~ N + Percent() , data = data )
# 83% entire, 17% room

data <- data %>%
  mutate(f_room_type = factor(room_type))

data$f_room_type <- factor(ifelse(data$f_room_type== "Entire home/apt", "Entire/Apt",
    ifelse(data$f_room_type== "Private room", "Private", ".")))


###### LIST

nums <- unlist(lapply(data, is.numeric))  
data_num <- data[ , nums]

#potential
table(data$calculated_host_listings_count )
table(data$accommodates)
table(data$minimum_nights) # cut method, 1-2-3-4-5-6-7, above dont care/typo
table(data$number_of_reviews) # make any? no
table(data$review_scores_rating) # none
table(data$reviews_per_month)  # none

bols <- unlist(lapply(data, is.logical))  
data_bols <- data[ , bols]

#bol just 0/1:
table(data$host_is_superhost)
table(data$host_identity_verified)
table(data$instant_bookable)


texts <- unlist(lapply(data, is.character))  
data_texts <- data[ , texts]

# 23, 4 factor so far, left: 

data$host_response_time #-4k missing, too much
data$host_response_rate #-4k missing, too much
data$amenities # done ,d 
data$price # done, kept
table(data$host_verifications) # government ID
table(data$neighbourhood_cleansed) # below 100 dummy
data$bathrooms_text # 1-2-many

is.Date <- function(x) inherits(x, 'Date')
datess <- unlist(lapply(data, is.Date))  
data_dates <- data[ , datess]

#potential
table(data$host_since) # some yearly
table(data$last_review)

###### LIST



unique_amenities <-  unique(trimws(gsub("[[]" , "" ,gsub("[]]" , "" ,gsub("\"" , "" ,unlist(strsplit(as.character(data$amenities), ",")))))))

key_words <- c("hdtv", "oven" , "wifi" ,  "refrigerator" , 
               "garage" ,  "pool" ,  "gym" , 
              "grill" , "coffee"  , "dryer" ,
              "washer" ,  "parking" , "sound system" , "air conditioning" ,
              "elevator")

for (x in key_words) {
  
  unique_amenities_mod <- c()
  
  for (i in seq_along(unique_amenities)) {
    new_item <- ifelse(grepl( x , tolower(unique_amenities[i]), fixed = TRUE) , 
                       tolower( x ) , unique_amenities[i] )
    unique_amenities_mod <- c(unique_amenities_mod , new_item)
  }
  
  unique_amenities <-  unique(unique_amenities_mod)
  
}


for(p in key_words) data <- data %>% 
      mutate(!! p := +(ifelse((grepl( p, tolower(data$amenities), fixed = TRUE)),1,0)))

data <- rename(data, c(air_conditioning = `air conditioning`, 
                       sound_system = `sound system` ))


data <- data %>% 
  mutate(  oven = ifelse(data$oven == 1 , 1 , 
                          ifelse((grepl( "stove", tolower(data$amenities), fixed = TRUE)),1,0)) )

data <- data %>% 
  mutate(  air_conditioning = ifelse(data$air_conditioning == 1 , 1 , 
                          ifelse((grepl( "AC unit", tolower(data$amenities), fixed = TRUE)),1,0)) )


data <- data %>% 
  mutate(  television = 
             ifelse((grepl( "tv", gsub("hdtv" , "television" , 
                                       tolower(data$amenities)), fixed = TRUE)),1,0) )
 

dummies <- names(data)[seq(78,93)]

data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))

data$price <- as.integer(gsub("[$]" , "" , gsub("," , "", data$price)))


data <- data %>% 
  mutate(  n_host_governmentid =  ifelse((grepl( "government", tolower(data$host_verifications), fixed = TRUE)),1,0) )


large_neighboorhoods <- data.table(data)[, .(.N ) , 
                                         by = neighbourhood_cleansed ][ N >= 100 , neighbourhood_cleansed ]

data <- data %>% 
  mutate(  f_neighbourhood_cleansed =  
             factor(ifelse(neighbourhood_cleansed %in% large_neighboorhoods,
                    neighbourhood_cleansed,"small neighboorhoods")))

data <- data %>% 
  mutate(  n_bathrooms_text =  
             ifelse(ifelse(is.na(bathrooms_text) ,"1 bath" , bathrooms_text) %in% 
                    bath_missing ,
              "1 bath" , ifelse(is.na(bathrooms_text) ,"1 bath" , bathrooms_text) ))


data <- data %>% 
  mutate(  n_bathrooms_text =  
             as.numeric(unlist(regmatches(n_bathrooms_text,
                                          gregexpr("[[:digit:]]+\\.*[[:digit:]]*",
                                                   n_bathrooms_text)))))

data <- data %>% 
  mutate(  n_bathrooms_text = 
             ifelse(n_bathrooms_text == 0 , 1 , n_bathrooms_text))



data <- data %>% 
  mutate(   flag_minimum_nights =
              ifelse(minimum_nights > 7 , 1,0) ,
              n_minimum_nights =  
              ifelse(minimum_nights > 7 , 7,minimum_nights) )


data <- data %>% 
  mutate(  n_host_is_superhost =  
             ifelse(host_is_superhost , 1,0) )


data <- data %>% 
  mutate(  n_host_identity_verified =  
             ifelse(host_identity_verified , 1,0) )


data <- data %>% 
  mutate(  n_instant_bookable =  
             ifelse(instant_bookable , 1,0) )

data <- data %>% 
  mutate(  n_bedrooms  =  bedrooms )

data <- data %>% 
  mutate(  n_number_of_reviews  =  number_of_reviews )

data <- data %>%
  mutate(
    n_host_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(host_since ,format="%Y-%m-%d")))

# Correlation

data_nums <- keep( data , is.numeric )
cT <- round( cor( data_nums , use = "complete.obs") , 2 )
cT[ upper.tri( cT ) ] <- NA
melted_cormat <- melt( cT , na.rm = TRUE)

data.table(melted_cormat)[Var1 == "price" | Var2 == "price" , .(Var1 , Var2 ,
                                  value = abs(value) )][order(-value)]

#  accomodates > bedrooms , number_of_reviews , n_minimum_nights  
# n_host_is_superhost  
#  
# d_pool, d_refrigerator   d_oven  d_gym  d_coffee   d_television , d_air_conditioning  , washer  ,

ggplot( data = melted_cormat, aes( Var2 , Var1 , fill = value ) )+
  geom_tile( color = "white" ) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_bw()+ 
  theme( axis.text.x = element_text(angle = 45, vjust = 1, 
                                    size = 8, hjust = 1))+
  labs(y="",x="")+
  coord_fixed()


data <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*|^flag_.*"), price)


# DROP Y MISSING

data <- data %>%
  drop_na(price)


# Check how many missing

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]



# IMPUTE IF FEW



data <- data %>%
  mutate(
    n_host_is_superhost =  ifelse(is.na(n_host_is_superhost), 0, n_host_is_superhost), 
    n_host_identity_verified =  ifelse(is.na(n_host_identity_verified), 
                                       0, n_host_identity_verified), 
    flag_n_bedrooms = ifelse(is.na(n_bedrooms), 1 , 0 ),
    n_bedrooms =  ifelse(is.na(n_bedrooms), 
                         median(n_bedrooms , na.rm = T), n_bedrooms),
    n_host_since = ifelse(is.na(n_host_since), median(n_host_since , na.rm = T), 
                          n_host_since))


# Deal with extreme price

data <-  data %>% filter(price <= 1100 & price > 50 )


ggplot( data , aes(x = price)) +
  geom_histogram( fill='navyblue', color = 'white' , bins = 30 ) +
  labs( x = "price") +
  theme_bw()

# To check again how many were missing

datasummary( factor(flag_minimum_nights)  ~ N , data )


chck_sp <- function( x_var , x_lab ){
  ggplot( data , aes(x = x_var, y = price)) +
    geom_point(color='red',size=2,alpha=0.6) +
    geom_smooth(method="loess" , formula = y ~ x )+
    labs(x = x_lab, y = "price") +
    theme_bw() +
    theme(axis.title.y = element_text(size = 10) )
}

chck_sp(data$n_accommodates ,"accomodates") ## linear
chck_sp(data$n_bedrooms,"n_bedrooms") ## linear spline at 2?
chck_sp(data$n_number_of_reviews,"number_of_reviews") + 
  scale_x_continuous(breaks = seq( 0 , 460 , 20)) ## quad / spline 10-30
chck_sp(data$n_minimum_nights,"minimum_nights") ##
chck_sp(data$n_host_since,"n_host_since") ##

table(data$n_host_identity_verified) # room group?

library(skimr)
skim(data_num)

# Functional forms

data <- data %>%
  mutate(
    n_number_of_reviews2=n_number_of_reviews^2,
    n_number_of_reviews3=n_number_of_reviews^3,
    ln_price = log(price)
  )

# Check y

summary(data$price)

# Should have no missing

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# Group those you cant predict, too unique
unique(data$f_property_type)

data <- data %>%
  mutate(f_property_type = ifelse(f_property_type == "Room/Apt" |
                                    f_property_type == "Room/Unit" ,
                                  "Room" ,
                                  f_property_type)
  )



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



basic_lev  <- c("n_accommodates", "n_bedrooms", "f_property_type", "f_room_type", "number_of_reviews", "n_minimum_nights" , "n_host_is_superhost")

basic_add <- c("f_neighbourhood_cleansed","n_instant_bookable")

reviews <- c("n_number_of_reviews" )

host_lev <- c("n_host_governmentid", "n_host_since","n_host_identity_verified")

poly_lev <- c("n_number_of_reviews3" , "n_number_of_reviews2" )

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
modellev3 <- paste0(" ~ ",paste(c(basic_lev, basic_add,host_lev),collapse = " + "))
modellev4 <- paste0(" ~ ",paste(c(basic_lev,basic_add,host_lev,reviews,poly_lev),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,host_lev,poly_lev,X1),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,host_lev,poly_lev,X1,X2),collapse = " + "))
modellev7 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,host_lev,poly_lev,X1,X2,amenities),collapse = " + "))
modellev8 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,host_lev,poly_lev,X1,X2,amenities,X3),collapse = " + "))


# Holdout set

smp_size <- floor(0.2 * nrow(data))
set.seed(20220209)
holdout_ids <- sample( seq_len( nrow( data ) ) , size = smp_size )
data$holdout <- 0
data$holdout[holdout_ids] <- 1
data_holdout <- data %>% filter(holdout == 1)
data_work <- data %>% filter(holdout == 0)

# Checking models on train-test data


## K = 5
k_folds <- 5
# Define seed value
seed_val <- 20220210

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
