
  
  # Data
  
# Data Loading and cleaning

rm(list=ls())
library(tidyverse)
library(modelsummary)
library(kableExtra)
library(ggpubr)
library(fixest)
library(xtable)

# 1 choosing profession

cps_earnings <- read_csv("https://osf.io/4ay9x/download")

cps_earnings_filtered <- cps_earnings %>%
  mutate( sample = ifelse( occ2012 >= 1010 & occ2012 <= 1030  , 1 , 0 ) ) 

earnings_it <- cps_earnings_filtered %>% filter( sample == 1 )

datasummary( log_wages_per_hour * as.factor( occ2012 ) ~ Mean + N + Percent(), data = earnings_it )

# Computer programmers , Software developers , Web developers relatively similar

table(earnings_it$occ2012)
 
# Potential  Variables
# stfips grade92 race age female marital ownchild/chldpres
# prcitshp  state occ2012 prcitshp class unionmme/unioncov
# wages_per_hour  / log_wages_per_hour
datasummary( log_wages_per_hour * as.factor( lfsr94 ) ~ Mean + N + Percent(), data = earnings_it )




# reduced list


Missing <- function(x) {sum(is.na(x))}
datasummary( stfips + grade92 + race + age + female + marital + ownchild + chldpres + 
             prcitshp + state + occ2012 + prcitshp + class + unionmme  + log_wages_per_hour ~ Missing, data = earnings_it )

earnings_it <- earnings_it %>% 
  mutate( female =  as.numeric( sex == 2 )) %>%
  mutate( wages_per_hour = earnwke / uhours) %>%
  mutate( log_wages_per_hour = log( wages_per_hour )) 

earnings_it_num <- keep( earnings_it , is.numeric )


datasummary( grade92 + race + age + female + marital + ownchild + 
               chldpres + occ2012  ~ Missing + N, data = earnings_it_num )

# no numeric missing, see categorical

table(earnings_it$lfsr94)

# no missing from numeric, non numeric: state stfips no
# prcitshp no  (2 groups?) class no (3 class?) unionmme no

# Extreme values


histograms <- function(x_var , x_lab) {
  ggplot( earnings_it , aes(x = wages_per_hour)) +
    geom_histogram( fill='navyblue', color = 'white' ) +
    labs(y = "Count" , x = "wages per hour") +
    theme_bw()
}

P95 <- function(x){quantile(x,0.95,na.rm=T)}
P05 <- function(x){quantile(x,0.05,na.rm=T)}

histograms(wages_per_hour , "wages per hour")

datasummary( grade92 + race + age + female + marital + ownchild + 
               occ2012  ~ Max + P95  + Median + P05 + Min , data = earnings_it_num )

# Recode race, grade92 education

table(earnings_it_num$age) # 22 
table(earnings_it_num$grade92) # 39+40 HS 41+42 Associate 43 Bach 44 MA
table(earnings_it_num$marital) # 1-3 married 4-7 not
table(earnings_it_num$ownchild) # 0-1-2-3?
# wages per hour cant be below 5$
table(earnings_it_num$uhours) # 1-2 poc or not

# Dont include lfsr94 == "Employed-Absent", 44 cant predict

earnings_it <- earnings_it %>% 
  filter( grade92 >= 39 & grade92 <= 44 | grade92 == 46 ) %>%
  filter( age >= 22  ) %>% 
  filter( lfsr94 == "Employed-At Work" ) %>%
  filter( uhours >= 20 ) %>% 
  filter( wages_per_hour >= 5 )

table(earnings_it$grade92)

earnings_it <- earnings_it %>%
  mutate(foreign_born = ifelse(startsWith(earnings_it$prcitshp , "Foreign Born,"), 1,0),
         US_citizen = ifelse(startsWith(earnings_it$prcitshp , "Foreign Born, Not a"), 0,1),
         Gov_empl = ifelse(startsWith(earnings_it$class , "Government "), 1,0),
         Prof_empl = ifelse(class == "Private, For Profit", 1,0),
         NGO_empl = ifelse(class == "Private, Nonprofit", 1,0),
         unionmember = ifelse(unionmme == "Yes", 1,0),
         married = ifelse(marital < 4, 1,0),
         no_child = ifelse(ownchild > 3 , 3,ownchild),
         poc = ifelse(race == 1 , 0,1),
         agesq = age^2,
         uhourssq = uhours^2)


table(earnings_it$race)

# Correlation:

earnings_it_nums <- keep( earnings_it , is.numeric )
cT <- round( cor( earnings_it_nums , use = "complete.obs") , 2 )
cT[ upper.tri( cT ) ] <- NA
melted_cormat <- melt( cT , na.rm = TRUE)

library(data.table)

ggplot( data = melted_cormat, aes( Var2 , Var1 , fill = value ) )+
  geom_tile( color = "white" ) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_bw()+ 
  theme( axis.text.x = element_text(angle = 45, vjust = 1, 
                                    size = 10, hjust = 1))+
  labs(y="",x="")+
  coord_fixed()

# grade92 , age , age^2 ,  female , (chldpres) , uhours  , foreign_born  , 
# no_child , race

data.table(melted_cormat)[Var1 == "wages_per_hour"][order(value)]
data.table(melted_cormat)[Var2 == "wages_per_hour"][order(value)]

# Loess

chck_sp <- function( x_var , x_lab ){
  ggplot( earnings_it , aes(x = x_var, y = wages_per_hour)) +
    geom_point(color='red',size=2,alpha=0.6) +
    geom_smooth(method="loess" , formula = y ~ x )+
    labs(x = x_lab, y = "Wages per hour") +
    theme_bw() +
    theme(axis.title.y = element_text(size = 10) )
}

chck_sp(earnings_it$age,"age")
# sort of quadratic
chck_sp(earnings_it$female,"female")
# female avg is higher, linear
chck_sp(earnings_it$chldpres,"chldpres")
chck_sp(earnings_it$no_child,"no_child")
# makes sense, linear
chck_sp(earnings_it$uhours,"uhours")
# not sure: quad/cube/35-45 splines
chck_sp(earnings_it$foreign_born,"foreign_born")
data.table(earnings_it)[, mean( wages_per_hour ) , by = foreign_born]
data.table(earnings_it)[, mean( grade92 ) , by = foreign_born]
# foreigners actually earn more, linear
table(earnings_it$foreign_born)

chck_sp(earnings_it$grade92,"grade92")
  # sort of linear, 43-44 is bigger, other kinda same
chck_sp(earnings_it$race,"race")
data.table(earnings_it)[, mean( wages_per_hour ) , by = poc]


ggplot(data = earnings_it, aes(x=uhours,y=wages_per_hour)) +
  geom_point(color='red',size=2,alpha=0.6) +
  geom_smooth( aes(colour='red'), method="loess", formula = y ~ x,se=F, size=1) +
  geom_smooth( aes(colour='black'), method="lm", formula = y ~ poly(x,2) , se=F, size=1) +
  scale_x_continuous(breaks = seq(20 , 80 ,5 ))


# Interactions
setwd("C:/Users/abogn/CEU/Study/da3/git/da3_prediction/assignment1/")

source("ch14_aux_fncs.R")
p1 <- price_diff_by_variables2(earnings_it, "no_child", "female" ,  "no_child" ,"female")
p2 <- price_diff_by_variables2(earnings_it, "poc", "foreign_born" ,  "poc" ,"foreign_born")
p3 <- price_diff_by_variables2(earnings_it, "age", "poc" ,  "age" ,"poc")
p4 <- price_diff_by_variables2(earnings_it, "age", "foreign_born" ,  "age" ,"foreign_born")
p5 <- price_diff_by_variables2(earnings_it, "age", "female" ,  "age" ,"female")
p6 <- price_diff_by_variables2(earnings_it, "poc", "female" ,  "poc" ,"female")

# Model 1: Linear regression on grade
model1 <- as.formula(wages_per_hour ~ grade92 )
# Models 2: Multiple linear regression grade + age
model2 <- as.formula(wages_per_hour ~ grade92 + age + agesq )
# Models 3: Multiple linear regression grade + age + female  + uhours + foreign_born +
# no_child + race
model3 <- as.formula(wages_per_hour ~ grade92 + age + agesq + occ2012 + female + uhours + foreign_born + no_child + race)
# Model 4 interaction:
model4 <- as.formula(wages_per_hour ~ grade92 + age + agesq + uhours + occ2012 +
                       female*age + foreign_born*age + no_child*female + race*age)
# model31 <- as.formula(wages_per_hour ~ grade92 + age + agesq + female + uhours + foreign_born + no_child + poc)
# model41 <- as.formula(wages_per_hour ~ grade92 + age + agesq + uhours +
#                        female*age + foreign_born*age + no_child*female + poc*age)
# model42 <- as.formula(wages_per_hour ~ grade92 + age + agesq + uhours +
#                         female*agesq + foreign_born*agesq + no_child*female + poc*agesq)

# Running simple OLS
reg1 <- feols(model1, data=earnings_it, vcov = 'hetero')
reg2 <- feols(model2, data=earnings_it, vcov = 'hetero')
reg3 <- feols(model3, data=earnings_it, vcov = 'hetero')
reg4 <- feols(model4, data=earnings_it, vcov = 'hetero')

# evaluation of the models: using all the sample
fitstat_register("k", function(x){length( x$coefficients ) - 1}, "No. Variables")
etable( reg1 , reg2 , reg3 , reg4  , fitstat = c('aic','bic','rmse','ar2','r2','n','k') )


# convex age, female less, hours less
data.table(earnings_it)[ no_child < 3 , .( avg = mean(wages_per_hour )) , by = female]
data.table(earnings_it)[ no_child == 3 & female == 0 , .( .N ) ]
table(earnings_it$grade92)
# 5B) Cross-validation for better evaluation of predictive performance

k <- 5

# We use the 'train' function which allows many type of model training -> use cross-validation
set.seed(220126)
cv1 <- train(model1, earnings_it, method = "lm", trControl = trainControl(method = "cv", number = k))

# Check the output:
cv1
summary(cv1)
cv3$results[5]
cv3$resample[[2]][5]
cv1$resample[[1]][1]^2

set.seed(220126)
cv2 <- train(model2, earnings_it, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(220126)
cv3 <- train(model3, earnings_it, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")
set.seed(220126)
cv4 <- train(model4, earnings_it, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")


# Calculate RMSE for each fold and the average RMSE as well
cv <- c("cv1", "cv2", "cv3", "cv4")
rmse_cv <- c()

for(i in 1:length(cv)){
  rmse_cv[i] <- sqrt((get(cv[i])$resample[[1]][1]^2 +
                        get(cv[i])$resample[[1]][2]^2 +
                        get(cv[i])$resample[[1]][3]^2 +
                        get(cv[i])$resample[[1]][4]^2)/4)
}


# summarize results
cv_mat <- data.frame(rbind(cv1$resample[4], "Average"),
                     rbind(cv1$resample[1], rmse_cv[1]),
                     rbind(cv2$resample[1], rmse_cv[2]),
                     rbind(cv3$resample[1], rmse_cv[3]),
                     rbind(cv4$resample[1], rmse_cv[4])
)

test_rmsd <- data.table(cv_mat)[Resample != "Average"]
RMSEsd <- c("RMSESD" , test_rmsd[,2:5][,lapply(.SD, sd)])
names(RMSEsd) <- names(cv_mat) 
cv_mat <- rbind(cv_mat , RMSEsd) 



# Show model complexity and out-of-sample RMSE performance
m_comp <- c()
models <- c("reg1", "reg2", "reg3", "reg4")
for( i in 1 : length(cv) ){
  m_comp[ i ] <- length( get( models[i] )$coefficient  - 1 ) 
}
length( get( models[2] )$coefficient ) - 1
m_comp <- tibble( model = models , 
                  complexity = m_comp,
                  RMSE = rmse_cv ,  
                  RMSEsd = unlist(cv_mat[7,2:5]) )

one_sd_rule <- unlist(data.table(m_comp)[RMSE == min(RMSE) ,.(RMSEsd)])

ggplot( m_comp , aes( x = complexity , y = RMSE ) ) +
  geom_point(color='red',size=2) +
  geom_line(color='blue',size=0.5) +
  geom_segment(data = m_comp, aes(x=min(complexity), xend=max(complexity),
                                  y=( min(RMSE) + one_sd_rule ),
                                  yend=( min(RMSE)) + one_sd_rule ),
                                  color = "red" ) +
  geom_label(aes(x = 6.5, y = min(RMSE) + one_sd_rule + 0.05 ), 
             label = "One Standard Deviation from M4" ,
             size = 4, color = 'red', fill = "lightblue" , fontface = "bold") +
  labs(x='Number of explanatory variables',y='Averaged RMSE on test samples',
           title='Prediction performance and model compexity') +
  scale_x_continuous( breaks = seq(2 , 14 ,2))
  theme_bw()


