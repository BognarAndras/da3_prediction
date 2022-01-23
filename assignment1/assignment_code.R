
  
  # Data
  
# Data Loading and cleaning

rm(list=ls())
library(tidyverse)
library(modelsummary)
library(kableExtra)
library(ggpubr)
library(fixest)
library(xtable)


cps_earnings <- read_csv("https://osf.io/4ay9x/download")

cps_earnings_filtered <- cps_earnings %>%
  mutate( sample = ifelse( occ2012 >= 1010 & occ2012 <= 1030  , 1 , 0 ) ) 

earnings_it <- cps_earnings_filtered %>% filter( sample == 1 )

datasummary( log_wages_per_hour * as.factor( occ2012 ) ~ Mean + N + Percent(), data = earnings_it )



glimpse(earnings_it)


earnings_it <- earnings_it %>% 
  mutate( female =  as.numeric( sex == 2 )) %>%
  mutate( wages_per_hour = earnwke / uhours) %>%
  mutate( log_wages_per_hour = log( wages_per_hour )) 

#https://data.nber.org/morg/docs/cpsx.pdf


cps_earnings_filtered <- cps_earnings %>% 
  filter( grade92 >= 44 & grade92 <= 46 ) %>%
  filter( age >= 24 & grade92 <= 65 ) %>% 
  filter( lfsr94 == "Employed-At Work" ) %>%
  filter( uhours >= 20 ) %>% 
  filter( earnwke > 5 )


cps_earnings_filtered <- cps_earnings_filtered %>% 
  mutate( female =  as.numeric( sex == 2 )) %>%
  mutate( wages_per_hour = earnwke / uhours) %>%
  mutate( log_wages_per_hour = log( wages_per_hour )) 

cps_earnings_filtered_teach <- cps_earnings_filtered %>%
  mutate( sample = ifelse( occ2012 == 2200 , 1 , 0 ) ) 

earnings_teachers <- cps_earnings_filtered_teach %>% filter( sample == 1 )



From the [CPS suvery](https://osf.io/4ay9x/download) I analysed higher education teachers' wages. First, in the governmental sector I didn't expect a difference in payments. Second, it is not as dominated by one gender as Nursery or primary schools.

I included only graduate level degrees (generally required for teachers) of age between 24 and 65, excluding self-employed people (not common) and people who reported working less than 20 hours a week. I found 2 people who reported less than 5 dollars per week (working 40+ hours). This may be against minimum wage laws, therefore, I excluded them.

I created additional columns for hourly wages for fair comparison and log hourly wages which I consider to be easier to interpret (difference in USD is less meaningful than in percentages to me).

# Data Summary

# data table

tmp <- earnings_teachers %>% 
  select( female  ,  grade92 ,  
          'Weekly wage' =  earnwke , 
          'Hourly wage' = wages_per_hour ,
          'Ln Hourly wage' = log_wages_per_hour ) %>% 
  mutate( 'Gender' = ifelse(female == 0 , 'Male' , 'Female' ) ,
          'Education' = ifelse(grade92 == 44 , 'Masters' , 
                               ifelse(grade92 == 45 , 'Prof' , 'PhD' )))

P95 <- function(x){ quantile(x,.95,na.rm=T)}
datasummary( (Gender  + `Education`) * `Weekly wage` + `Hourly wage` + `Ln Hourly wage`  ~ Mean + SD + Min + Max + Median + P95 + N , data = tmp ) %>%  kable_styling(full_width = TRUE , latex_options = "striped" )


The main findings (further findings in appendix) from the below table are the following:
  
  - On avg. men earn more in the sample and the spread of their wages is also bigger.
- The number of men and women in the sample are nearly equal with N = 949.
- Most post-secondary teachers hold a PhD or a Masters degree.

\
# Simple Linear Regression Graphs

Based on the non-parametric graphs I created (Appendix), I chose simple linear models. Since I dont have much variation in my x variables the confidence of my findings will decrease.



p1 <- ggplot( data = earnings_teachers , aes( x = female , y = log_wages_per_hour )) +
  geom_point( color = 'red', size = 2, shape = 16, ) + 
  scale_x_continuous(expand=c(0.01, 0.01), limits = c(0, 1),   breaks=seq(0, 1,   by=1)) + 
  scale_y_continuous(expand=c(0.01, 0.01),limits = c(0, 5), breaks=seq(0, 5, by=1)) +
  geom_smooth( method = 'lm', formula = y ~ x) +
  labs(x = "Gender",y = "Log of wages (USD)")


p2 <- ggplot( data = earnings_teachers , aes( x = grade92, y = log_wages_per_hour )) +
  geom_point( color = 'red', size = 2, shape = 16, ) + 
  scale_x_continuous(expand=c(0.01, 0.01), limits = c(44, 46),   breaks=seq(44, 46,   by=1)) + 
  scale_y_continuous(expand=c(0.01, 0.01),limits = c(0, 5), breaks=seq(0, 5, by=1)) +
  geom_smooth( method = 'lm', formula = y ~ x) +
  labs(x = "Level of Education",y = "Log of wages (USD)")



linear_assoc_figs <- ggarrange(p1, p2,
                              hjust = -0.6,
                              ncol = 1, nrow = 2)
linear_assoc_figs


# Regression Models

**Model 1** shows log hourly wages regressed on gender. We get an exact number: females earn on avg. 11% less than males.

**Model 2** shows log hourly wages regressed on education. We see a positive association, a degree one level higher means 14.5% higher hourly wages on avg.

**Model 3** regresses log hourly wages on both independent variables. We find that controlling on education the gender wage differences become smaller: 8.2% lower avg. hourly wages for women of same level of education. This means that on avg. males have higher level of education. The confidence intervals of Model 1 and 3 heavily overlap which means that we cant rule out that in the population the gender wage difference is the same with or without controlling for education.

**Models 4 and 5** show the same multivariate regression with education transformed to a categorical variable. The gender difference is lower but still significant. In model 4 PhD is the reference in Model 5 Masters, because these have the highest sample sizes netting lower standard errors. Both models confirm the significant difference between Masters wages to PhD/Professinal wages among the same gender. There is no significant difference in PhD vs Professinal wages.


All findings in the models are significant at 95% confidence, except for the difference in professional and PhD degrees in Model 4. All other conclusions can be generalized to the population of Post-secondary teachers in the US in 2014 with 95% certainty.


# Regressions + Regression table

# Log wage ~ Gender
reg0 <- feols( log_wages_per_hour ~ female , data = earnings_teachers , vcov = 'hetero' )

# Log wage ~ Education
reg1 <- feols( log_wages_per_hour ~ grade92 , data = earnings_teachers , vcov = 'hetero' )

# Multiple regression 
reg2 <- feols( log_wages_per_hour ~ female + grade92 , data = earnings_teachers  , vcov = 'hetero' )

earnings_teachers_extra <- earnings_teachers %>% 
  mutate( master = as.numeric( grade92 == 44 ),
          professional = as.numeric( grade92 == 45 ),
          phd = as.numeric( grade92 == 46 ) )

# Multivariate Regression with Education as qualitative

reg3 <- feols( log_wages_per_hour ~ female + master + professional , data = earnings_teachers_extra , vcov = 'hetero' )

reg4 <- feols( log_wages_per_hour ~ female + professional + phd , data = earnings_teachers_extra , vcov = 'hetero' )

# Compare the results
# etable( reg0 , reg1 , reg2 , reg3 , reg4 )


msummary(list(reg0, reg1, reg2, reg3, reg4 ),
         fmt="%.3f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Within|AIC|BIC|R2 Pseudo|Std.Errors',
         stars=c('*' = .05, '**' = .01)
) %>%  kable_styling(full_width = TRUE)


