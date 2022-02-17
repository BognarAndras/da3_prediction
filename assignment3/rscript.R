setwd("C:/Users/abogn/CEU/Study/da3/git/da3_prediction/assignment3")
rm(dt)

library(haven)
library(glmnet)
library(purrr)
library(margins)
library(skimr)
library(kableExtra)
library(Hmisc)
library(cowplot)
library(gmodels) 
library(lspline)
library(sandwich)
library(modelsummary)

library(data.table)
library(tidyverse)

data <- fread("https://osf.io/3qyut/download")


data <- data %>%
  select(-c(COGS, finished_prod, net_dom_sales, net_exp_sales, wages)) %>%
  filter(year !=2016)

###########################################################
# label engineering
###########################################################

# add all missing year and comp_id combinations -
# originally missing combinations will have NAs in all other columns
data <- data %>%
  complete(year, comp_id)

# generate status_alive; if sales larger than zero and not-NA, then firm is alive
data  <- data %>%
  mutate(status_alive = sales > 0 & !is.na(sales) %>%
           as.numeric(.))

# defaults in two years if there are sales in this year but no sales two years later
data <- data %>%
  group_by(comp_id) %>%
  mutate(default = ((status_alive == 1) & (lead(status_alive, 2) == 0)) %>%
           as.numeric(.)) %>%
  ungroup()


data <- data %>%
  filter(year <=2015 & year >= 2010)


# Size and growth
summary(data$sales) # There will be NAs, we'll drop them soon


data <- data %>%
  mutate(sales = ifelse(sales < 0, 1, sales),
         ln_sales = ifelse(sales > 0, log(sales), 0),
         sales_mil=sales/1000000,
         sales_mil_log = ifelse(sales > 0, log(sales_mil), 0))

data <- data %>%
  group_by(comp_id) %>%
  mutate(d1_sales_mil_log = sales_mil_log - Lag(sales_mil_log, 1) ) %>%
  ungroup()


# replace w 0 for new firms + add dummy to capture it
data <- data %>%
  mutate(age = (year - founded_year) %>%
           ifelse(. < 0, 0, .),
         new = as.numeric(age <= 1) %>% #  (age could be 0,1 )
           ifelse(balsheet_notfullyear == 1, 1, .),
         d1_sales_mil_log = ifelse(new == 1, 0, d1_sales_mil_log),
         new = ifelse(is.na(d1_sales_mil_log), 1, new),
         d1_sales_mil_log = ifelse(is.na(d1_sales_mil_log), 0, d1_sales_mil_log))



###########################################################
# sample design
###########################################################


# data <- data %>%
#   group_by(comp_id) %>%
#   mutate(d1_sales_growth = (sales / Lag(sales , 1)) ,
#          d2_sales_growth = (Lag(sales , 1) / Lag(sales , 2)) ,
#          d1_fast_sales_growth = ifelse(d1_sales_growth > 1.15 , 1 , 0) ,
#          d1_rapid_sales_growth = ifelse(d1_sales_growth > 1.25 , 1 , 0) ,
#          d2_conseq_fast_sales_growth = ifelse((d1_sales_growth > 1.15) 
#                                             & (d2_sales_growth > 1.15) , 1 , 0) ,
#          d2_avg_conseq_fast_sales_growth = ifelse((d1_sales_growth + 
#                                                      d2_sales_growth)/2 > 1.15 ,
#                                                   1 , 0)) %>%
#   ungroup()

data <- data %>%
  group_by(comp_id) %>%
  mutate(b1_sales = Lag(sales , 1) ,
         b2_sales= Lag(sales , 2),
         d1_sales = lead(sales , 1) ,
         d2_sales = lead(sales , 2)) %>%
  ungroup()


# write_csv(data,"./work/check.csv")
# 
# data <- data %>%
#   group_by(comp_id) %>%
#   mutate(b1_sales_growth = (sales / Lag(sales , 1)) ,
#          b2_sales_growth = (Lag(sales , 1) / Lag(sales , 2)),
#          d1_sales_growth = (lead(sales , 1) / sales) ,
#          d2_sales_growth = (lead(sales , 2) / lead(sales , 1))) %>%
#   as.numeric(.) %>% 
#   ungroup()


default = ((status_alive == 1) & (lead(status_alive, 2) == 0)) %>%
  as.numeric(.)
sum(is.na(data %>% filter(year == 2012) %>%  select(b1_sales_growth)))

sum(is.na(data.table(data)[year == 2012 , .( lead(sales , 1) , lead(sales , 2) ,
                                             (lead(sales , 2) / lead(sales , 1) )) ]))

# look at cross section
data <- data %>%
  filter((year == 2012) & (status_alive == 1)) %>%
  # look at firms below 10m euro revenues and above 1000 euros
  filter(!(sales_mil > 10)) %>%
  filter(!(sales_mil < 0.001))

Hmisc::describe(data$default)

write_csv(data,"./work/sample_design.csv")

###########################################################
# Feature engineering
###########################################################

# change some industry category codes
data <- data %>%
  mutate(ind2_cat = ind2 %>%
           ifelse(. > 56, 60, .)  %>%
           ifelse(. < 26, 20, .) %>%
           ifelse(. < 55 & . > 35, 40, .) %>%
           ifelse(. == 31, 30, .) %>%
           ifelse(is.na(.), 99, .)
  )

table(data$ind2_cat)

# Firm characteristics
data <- data %>%
  mutate(age2 = age^2,
         foreign_management = as.numeric(foreign >= 0.5),
         gender_m = factor(gender, levels = c("female", "male", "mix")),
         m_region_loc = factor(region_m, levels = c("Central", "East", "West")))

###########################################################
# look at more financial variables, create ratios
###########################################################

# assets can't be negative. Change them to 0 and add a flag.
data <-data  %>%
  mutate(flag_asset_problem=ifelse(intang_assets<0 | curr_assets<0 | fixed_assets<0,1,0  ))
table(data$flag_asset_problem)

data <- data %>%
  mutate(intang_assets = ifelse(intang_assets < 0, 0, intang_assets),
         curr_assets = ifelse(curr_assets < 0, 0, curr_assets),
         fixed_assets = ifelse(fixed_assets < 0, 0, fixed_assets))

# generate total assets
data <- data %>%
  mutate(total_assets_bs = intang_assets + curr_assets + fixed_assets)
summary(data$total_assets_bs)


pl_names <- c("extra_exp","extra_inc",  "extra_profit_loss", "inc_bef_tax" ,"inventories",
              "material_exp", "profit_loss_year", "personnel_exp")
bs_names <- c("intang_assets", "curr_liab", "fixed_assets", "liq_assets", "curr_assets",
              "share_eq", "subscribed_cap", "tang_assets" )

# divide all pl_names elements by sales and create new column for it
data <- data %>%
  mutate_at(vars(pl_names), funs("pl"=./sales))

# divide all bs_names elements by total_assets_bs and create new column for it
data <- data %>%
  mutate_at(vars(bs_names), funs("bs"=ifelse(total_assets_bs == 0, 0, ./total_assets_bs)))


########################################################################
# creating flags, and winsorizing tails
########################################################################

# Variables that represent accounting items that cannot be negative (e.g. materials)
zero <-  c("extra_exp_pl", "extra_inc_pl", "inventories_pl", "material_exp_pl", "personnel_exp_pl",
           "curr_liab_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs", "subscribed_cap_bs",
           "intang_assets_bs")

data <- data %>%
  mutate_at(vars(zero), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(zero), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(zero), funs("flag_error"= as.numeric(.< 0))) %>%
  mutate_at(vars(zero), funs(ifelse(.< 0, 0, .)))


# for vars that could be any, but are mostly between -1 and 1
any <-  c("extra_profit_loss_pl", "inc_bef_tax_pl", "profit_loss_year_pl", "share_eq_bs")

data <- data %>%
  mutate_at(vars(any), funs("flag_low"= as.numeric(.< -1))) %>%
  mutate_at(vars(any), funs(ifelse(.< -1, -1, .))) %>%
  mutate_at(vars(any), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(any), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(any), funs("flag_zero"= as.numeric(.== 0))) %>%
  mutate_at(vars(any), funs("quad"= .^2))


# dropping flags with no variation
variances<- data %>%
  select(contains("flag")) %>%
  apply(2, var, na.rm = TRUE) == 0

data <- data %>%
  select(-one_of(names(variances)[variances]))

########################################################################
# additional
# including some imputation
########################################################################

# CEO age
data <- data %>%
  mutate(ceo_age = year-birth_year,
         flag_low_ceo_age = as.numeric(ceo_age < 25 & !is.na(ceo_age)),
         flag_high_ceo_age = as.numeric(ceo_age > 75 & !is.na(ceo_age)),
         flag_miss_ceo_age = as.numeric(is.na(ceo_age)))

data <- data %>%
  mutate(ceo_age = ifelse(ceo_age < 25, 25, ceo_age) %>%
           ifelse(. > 75, 75, .) %>%
           ifelse(is.na(.), mean(., na.rm = TRUE), .),
         ceo_young = as.numeric(ceo_age < 40))


# number emp, very noisy measure
data <- data %>%
  mutate(labor_avg_mod = ifelse(is.na(labor_avg), mean(labor_avg, na.rm = TRUE), labor_avg),
         flag_miss_labor_avg = as.numeric(is.na(labor_avg)))

summary(data$labor_avg)
summary(data$labor_avg_mod)

data <- data %>%
  select(-labor_avg)

# create factors
data <- data %>%
  mutate(urban_m = factor(urban_m, levels = c(1,2,3)),
         ind2_cat = factor(ind2_cat, levels = sort(unique(data$ind2_cat))))

data <- data %>%
  mutate(default_f = factor(default, levels = c(0,1)) %>%
           recode(., `0` = 'no_default', `1` = "default"))

########################################################################
# sales  + change
########################################################################

data <- data %>%
  mutate(sales_mil_log_sq=sales_mil_log^2)




# generate variables ---------------------------------------------------

data <- data %>%
  mutate(flag_low_d1_sales_mil_log = ifelse(d1_sales_mil_log < -1.5, 1, 0),
         flag_high_d1_sales_mil_log = ifelse(d1_sales_mil_log > 1.5, 1, 0),
         d1_sales_mil_log_mod = ifelse(d1_sales_mil_log < -1.5, -1.5,
                                       ifelse(d1_sales_mil_log > 1.5, 1.5, d1_sales_mil_log)),
         d1_sales_mil_log_mod_sq = d1_sales_mil_log_mod^2
  )

# no more imputation, drop obs if key vars missing
data <- data %>%
  filter(!is.na(liq_assets_bs),!is.na(foreign), !is.na(ind))

# drop missing
data <- data %>%
  filter(!is.na(age),!is.na(foreign), !is.na(material_exp_pl), !is.na(m_region_loc))


# drop unused factor levels
data <- data %>%
  mutate_at(vars(colnames(data)[sapply(data, is.factor)]), funs(fct_drop))


write_csv(data,"./work/prepared.csv")


# Higher sales lower default

ggplot(data = data, aes(x=sales_mil_log, y=as.numeric(default))) +
  geom_point(size=2,  shape=20, stroke=2, fill="blue", color="blue") +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), color="yellow", se = F, size=1)+
  geom_smooth(method="loess", se=F, colour="red", size=1.5, span=0.9) +
  labs(x = "sales_mil_log",y = "default") +
  theme_bw()

# Gworth rate quadratic

d1sale_1<-ggplot(data = data, aes(x=d1_sales_mil_log, y=as.numeric(default))) +
  geom_point(size=0.1,  shape=20, stroke=2, fill="blue", color="blue") +
  geom_smooth(method="loess", se=F, colour="red", size=1.5, span=0.9) +
  labs(x = "Growth rate (Diff of ln sales)",y = "default") +
  theme_bw() +
  scale_x_continuous(limits = c(-6,10), breaks = seq(-5,10, 5))
d1sale_1