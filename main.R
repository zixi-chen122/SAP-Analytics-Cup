library(tidyverse)
#install.packages("tidyverse")
library(ggplot2)
#install.packages("ggplot2")
#install.packages("tree")
library(tree)
#install.packages("caTools")
library(caTools)
#install.packages("party")
library(party)
#require(xgboost)
#install.packages("xgboost")
library(xgboost)
#install.packages("DiagrammeR")
library(DiagrammeR)
#install.packages("caret")
library(caret)
#install.packages("splitTools")
library(splitTools)

#memory.limit(size=3000)

set.seed(2021)
companies <- read.csv("companies.csv")
payments <- read.csv("payments.csv")
physicians <- read.csv("physicians.csv")
previously_released_payments <- read.csv("previously_released_payments.csv")

dim(physicians)
dim(payments)
trainPhysicians <- physicians[physicians$set == 'train', ]
trainPayments <- merge(x = trainPhysicians, y = payments, by.x = "id", by.y = "Physician_ID")
finalTableTrain <- merge(x = trainPayments, y = companies, 
                         by.x = "Company_ID", by.y = "Company_ID")
summary(payments)

subset(finalTableTrain, !is.na(Charity))
subset(payments, is.na(Ownership_Indicator))

payments$Ownership_Indicator <- as.factor(payments$Ownership_Indicator)
o_t <- glm(as.factor(Ownership_Indicator) ~ as.factor(Charity), 
           data = subset(finalTableTrain, !is.na(Charity)), family = "binomial")
#Yes level in Charity factor has e(-7.7) relative ownership indicator rate 
# and is highly insignificant (p>0.001).

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### PHYSICIAN TABLE #####
physicians_exp <- physicians
physicians_exp %>% as_tibble()
### CITY COL
# This col has too many factors and therefore either removed or catagorized more.
NROW(unique(physicians_exp$City))
physicians_exp <- dplyr::select(physicians_exp, -c("City"))

### LICENSE STATE COL
# instead of having 5 cols, we are going to have one col (license state)
# and multiple rows.
physicians_exp <- physicians_exp %>% 
  pivot_longer(c('License_State_1', 'License_State_2', 'License_State_3', 
                 'License_State_4', 'License_State_5'), 
               values_to ='License_State', names_repair = "unique")
physicians_exp <- subset(physicians_exp, !is.na(physicians_exp$License_State), select = -c(name))
dim(physicians_exp)

physicians_exp <- physicians_exp %>% 
  mutate(License_State_Dir = case_when(
    License_State == "CT" | License_State == "ME" | 
      License_State == "MA" | License_State == "NH"| 
      License_State == "RI" |  License_State == "VT" ~ "NE",
    License_State == "NY" | License_State == "NJ" | License_State == "PA" ~ "NE",
    License_State == "WI" | License_State == "MI" | License_State == "IL" | License_State == "IN"| 
      License_State == "OH" ~ "MW",
    License_State == "ND" | License_State == "MN" | License_State == "SD" | 
      License_State == "IA"| License_State == "NE" | 
      License_State == "KS" | License_State == "MO" ~ "MW",
    License_State == "DE" | License_State == "FL" | 
      License_State == "GA" | License_State == "MD"| 
      License_State == "NC" | License_State == "SC"|
      License_State == "VA" | License_State == "WV"|
      License_State == "DC" | License_State == "PR" ~ "S",
    License_State == "AL" | License_State == "KY" | 
      License_State == "TN" | License_State == "MS" ~ "S",
    License_State == "AR" | License_State == "TX" | 
      License_State == "OK" | License_State == "LA" ~ "S",
    License_State == "AZ" | License_State == "CO"| 
      License_State == "ID" | License_State == "MT"|
      License_State == "NV" | License_State == "NM"|
      License_State == "UT" | License_State == "WY" ~ "W",
    License_State == "AK" | License_State == "CA" | 
      License_State == "HI" | License_State == "OR" |
      License_State == "WA" ~ "W",
    TRUE ~ "Other State"
  )) %>%
  dplyr::select(-c(License_State))
physicians_exp$License_State_Dir <- as.factor(physicians_exp$License_State_Dir)

### PS COL
physicians_exp$Primary_Specialty <- as.character(physicians_exp$Primary_Specialty)
test_p <- physicians_exp
test_p["primarySpecialty_count"] <- NA
for (row in 1: NROW(test_p)){
  split <- unlist(strsplit(test_p$Primary_Specialty[row], '\\|'))
  test_p$primarySpecialty_count[row] <- NROW(split)
}
unique(test_p$primarySpecialty_count)
## there are max 3 specialities, therefore we can create 3 cols to hold them.

physicians_exp[, "Primary_Specialty_1"] <- as.character(NA)
physicians_exp[, "Primary_Specialty_2"] <- as.character(NA)
physicians_exp[, "Primary_Specialty_3"] <- as.character(NA)
for (row in 1: NROW(physicians_exp)){
  split <- unlist(strsplit(physicians_exp$Primary_Specialty[row], '\\|'))
  for (i in 1:length(split)){
    col_name <- sprintf("Primary_Specialty_%d", i)
    physicians_exp[row, col_name] <- split[i]
  }
}
## Physicians Primary Speciality
#Physicians Primary Speciality checking and if NEUROLOGY then TRUE
physicians_exp$Primary_Specialty[is.na(physicians_exp$Primary_Specialty)]<- "Other"

physicians_exp$Primary_Specialty_1[is.na(physicians_exp$Primary_Specialty_1)]<- "Other"
physicians_exp$Primary_Specialty_2[is.na(physicians_exp$Primary_Specialty_2)]<- "Other"
physicians_exp$Primary_Specialty_3[is.na(physicians_exp$Primary_Specialty_3)]<- "Other"

physicians_exp$PS_Neurology <- ifelse((physicians_exp$Primary_Specialty=="Chiropractic Providers|Chiropractor|Neurology"|physicians_exp$Primary_Specialty=="Allopathic & Osteopathic Physicians|Psychiatry & Neurology|Psychiatry"|physicians_exp$Primary_Specialty=="Allopathic & Osteopathic Physicians|Psychiatry & Neurology|Neuromuscular Medicine"|physicians_exp$Primary_Specialty=="Allopathic & Osteopathic Physicians|Psychiatry & Neurology|Neurology"|physicians_exp$Primary_Specialty=="Allopathic & Osteopathic Physicians|Neurological Surgery"|physicians_exp$Primary_Specialty=="Allopathic & Osteopathic Physicians|Psychiatry & Neurology|Neuromuscular Medicine"|physicians_exp$Primary_Specialty=="Allopathic & Osteopathic Physicians|Psychiatry & Neurology|Psychiatry"|physicians_exp$Primary_Specialty=="Chiropractic Providers|Chiropractor|Neurology"|physicians_exp$Primary_Specialty=="Allopathic & Osteopathic Physicians|Psychiatry & Neurology|Neurology with Special Qualifications in Child Neurology"| physicians_exp$Primary_Specialty_1=="Neurology"|physicians_exp$Primary_Specialty_1=="Psychiatry & Neurology"|physicians_exp$Primary_Specialty_2=="Neurology"|physicians_exp$Primary_Specialty_2=="Psychiatry & Neurology"|physicians_exp$Primary_Specialty_3=="Neurology"|physicians_exp$Primary_Specialty_3=="Psychiatry & Neurology"), TRUE, FALSE)

count(physicians_exp, PS_Neurology == TRUE)

physicians_exp <- dplyr::select(physicians_exp, select = -c("Primary_Specialty"))


glimpse(physicians_exp)
summary(physicians_exp)
physicians_exp$Primary_Specialty_1 <- as.factor(physicians_exp$Primary_Specialty_1)
physicians_exp$Primary_Specialty_2 <- as.factor(physicians_exp$Primary_Specialty_2)
physicians_exp$Primary_Specialty_3 <- as.factor(physicians_exp$Primary_Specialty_3)
levels(physicians_exp$Primary_Specialty_1) #5
levels(physicians_exp$Primary_Specialty_2) #35
levels(physicians_exp$Primary_Specialty_3) # 117
# 4952 NA's in Primary_Specialty_3 and weird values (consider removing this col)
# also this col has around 150 117.
# consider leaving Primary_Specialty_1 and Primary_Specialty_2 only or just 1.

NROW(subset(physicians_exp, is.na(Primary_Specialty_1)))/NROW(physicians_exp) # 0.04%
phys_reduced <- subset(physicians_exp, !is.na(Primary_Specialty_1) && set != "test", 
                       select=-c(Primary_Specialty_2, Primary_Specialty_3))
names(phys_reduced)[names(phys_reduced) == "Primary_Specialty_1"] <- "Primary_Specialty"
physicians_exp <- phys_reduced

## POSSIBLIE SOLUTION: LONGER PIVOT
## PROBLEM: if a physician have multiple license states and multiple specs.
#, it will be seen as if he is specialized in this field in this license state,
# not specialized in this field in general. (?)
#physicians_primary_spec_exp <- physicians_exp %>% 
#  pivot_longer(c('Primary_Specialty_3', 'Primary_Specialty_2', 'Primary_Specialty_1'), 
#               values_to ='Primary_Specialty', names_repair = "unique")
#physicians_primary_spec_exp <- subset(physicians_exp, !is.na(physicians_exp$Primary_Specialty), select = -c(name))
#dim(physicians_primary_spec_exp)
#physicians_primary_spec_exp$Primary_Specialty <- as.factor(physicians_exp$Primary_Specialty)
#levels(physicians_primary_spec_exp$Primary_Specialty) # 154 levels

## SOLUTION 2:
# REMOVING Primary_Specialty_2 and Primary_Specialty_3.
# PRO: LESS SPECIFIC VALUES.
# CON: LESS VALUES IN GENERAL.
#physicians_primary_spec_1 <- subset(physicians_exp, select = -c(Primary_Specialty_2, Primary_Specialty_3))
#dim(physicians_primary_spec_1)
#physicians_primary_spec_1$Primary_Specialty <- as.factor(physicians_exp$Primary_Specialty_1)
#physicians_primary_spec_1 <- select(physicians_primary_spec_1, -c("Primary_Specialty_1"))
#levels(physicians_primary_spec_1$Primary_Specialty) # 5 levels
#physicians_exp <- physicians_primary_spec_1



## ZIP CODE COL (?)
# could be removed.
physicians_exp <- dplyr::select(physicians_exp, -c("Zipcode"))

## PROVINCE COL
physicians_exp <- subset(physicians_exp, select = -c(Province))

# STATE

# simplified_State
  physicians_exp <- physicians_exp %>% 
  mutate(State_Dir = case_when(
    State == "CT" | State == "ME" | 
      State == "MA" | State == "NH"| 
      State == "RI" |  State == "VT" ~ "NE",
    State == "NY" | State == "NJ" | State == "PA" ~ "NE",
    State == "WI" | State == "MI" | State == "IL" | State == "IN"| 
      State == "OH" ~ "MW",
    State == "ND" | State == "MN" | State == "SD" | 
      State == "IA"| State == "NE" | 
      State == "KS" | State == "MO" ~ "MW",
    State == "DE" | State == "FL" | 
      State == "GA" | State == "MD"| 
      State == "NC" | State == "SC"|
      State == "VA" | State == "WV"|
      State == "DC" | State == "PR" ~ "S",
    State == "AL" | State == "KY" | 
      State == "TN" | State == "MS" ~ "S",
    State == "AR" | State == "TX" | 
      State == "OK" | State == "LA" ~ "S",
    State == "AZ" | State == "CO"| 
      State == "ID" | State == "MT"|
      State == "NV" | State == "NM"|
      State == "UT" | State == "WY" ~ "W",
    State == "AK" | State == "CA" | 
      State == "HI" | State == "OR" |
      State == "WA" ~ "W",
    TRUE ~ "Other State"
  )) %>%
  dplyr::select(-c(State))
physicians_exp$State_Dir <- as.factor(physicians_exp$State_Dir)

## COUNTRY COL
# all from US > remove
physicians_exp <- subset(physicians_exp, select = -c(Country))

## REMOVING NULLS
summary(physicians_exp)
subset(physicians_exp, State_Dir == "Other State")
physicians_exp <- subset(physicians_exp, State_Dir != "Other State")
#physicians_exp <- subset(physicians_exp, Country != "UNITED STATES MINOR OUTLYING ISLANDS")

physicians_ML <- physicians_exp %>% dplyr::select(-contains("Name"))
summary(physicians_ML)
physicians_ML$PS_Neurology <- as.factor(physicians_ML$PS_Neurology)
## 2 NA State, 2 "UNITED STATES MINOR OUTLYING ISLANDS" in Country.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### PAYEMENTS TABLE #####
payments_exp <- payments %>% as_tibble()

## DATE
payments_testxxx <- payments_exp
payments_testxxx$new_column_year <- substring(payments_exp$Date, 7, 10) # Adding new year column.

# SWITCH CASE FOR GETTING SEASONS.
swfun <- function(x) {
  switch(x,
         '12' = 'Winter',
         '01' = 'Winter',
         '02' = 'Winter',
         '03' = 'Spring',
         '04' = 'Spring',
         '05' = 'Spring',
         '06' = 'Summer',
         '07' = 'Summer',
         '08' = 'Summer',
         '09' = 'Fall',
         '10' = 'Fall',
         '11' = 'Fall',
         as.character(x)
  )
}
payments_testxxx$new_column_season <- sapply(substring(payments_exp$Date,0, 2), swfun) # this is for creating a new column with different seasons.
payments_testxxx <- subset(payments_testxxx, select = -c(Date)) # This is for removing Date column
payments_exp <- payments_testxxx

## Form_of_Payment_or_Transfer_of_Value
unique(payments_exp$Form_of_Payment_or_Transfer_of_Value)
form_of_pay_col <- as.factor(payments_exp$Form_of_Payment_or_Transfer_of_Value)
summary(form_of_pay_col)
levels(form_of_pay_col)[
  levels(form_of_pay_col)=="Stock, stock option, or any other ownership interest"
  | levels(form_of_pay_col)=="Any other ownership interest"
  | levels(form_of_pay_col)=="Stock"
  | levels(form_of_pay_col)=="Stock option"
  | levels(form_of_pay_col)=="Stock, stock option, or any other ownership interest"
  | levels(form_of_pay_col)=="Dividend, profit or other return on investment"] <- "Other ownership interest"
unique(form_of_pay_col)
summary(form_of_pay_col)
NROW(subset(payments_exp, is.na(form_of_pay_col)))
payments_exp$Form_of_Payment_or_Transfer_of_Value <- form_of_pay_col
summary(payments_exp$Form_of_Payment_or_Transfer_of_Value)

## Nature_of_Payment_or_Transfer_of_Value
nature_of_pay_travel <- subset(payments_exp, Nature_of_Payment_or_Transfer_of_Value == 
              'Travel and Lodging')
NROW(nature_of_pay_travel)
# Since there are only 75179 rows with Travel and Lodging (very low number),
# the cols (City_of_Travel, State_of_Travel and Country_of_Travel)
# will be removed
payments_exp <- dplyr::select(payments_exp, -c("City_of_Travel", "State_of_Travel", "Country_of_Travel"))

## Ownership_Indicator
unique(payments_exp$Ownership_Indicator)
payments_exp$Ownership_Indicator <- as.numeric(
  if_else(payments_exp$Ownership_Indicator == 'Yes', 1, 0))

## Third_Party_Recipient
third_part_rec <- payments_exp$Third_Party_Recipient
unique(third_part_rec)
summary(third_part_rec)
##Entity             Individual No Third Party Payment 
##14742                   4800                1382708 

## Charity
unique(payments_exp$Charity)
na_charity <- subset(payments_exp, is.na(payments_exp$Charity))
NROW(na_charity)
na_charity
yes_charity <- subset(payments_exp, payments_exp$Charity == 'Yes')
no_charity <- subset(payments_exp, payments_exp$Charity == 'No')
yes_charity
NROW(yes_charity) ## 27
NROW(no_charity) ## 767502
## 26 out of 27 yeses has Entity in Third_Party_Recipient
## while this is could be a good corr., I don't think we can use the col
# due to the low number of not null observations and also the huge of No's.
payments_exp <- dplyr::select(payments_exp, -c("Charity"))

## Third_Party_Covered
# lots of nulls and included in Third_Party_Recipient
payments_exp <- dplyr::select(payments_exp, -c("Third_Party_Covered"))

## Contextual Info. 
# hard to get any data from it, therefore removed.
payments_exp <- dplyr::select(payments_exp, -c("Contextual_Information"))

## PRODUCT

# Names and codes will be removed.
payments_exp <- payments_exp %>% dplyr::select(-contains("Name"))
payments_exp <- payments_exp %>% dplyr::select(-contains("Code"))

# pivot product type and category to rows.
NROW(subset(payments_exp, is.na(Product_Type_1)))/NROW(payments_exp) # 0.06%
NROW(subset(payments_exp, is.na(Product_Type_2)))/NROW(payments_exp) # 78%
NROW(subset(payments_exp, is.na(Product_Type_3)))/NROW(payments_exp) # 92%

NROW(subset(payments_exp, is.na(Product_Category_1)))/NROW(payments_exp) # 41%
NROW(subset(payments_exp, is.na(Product_Category_2)))/NROW(payments_exp) # 87%
NROW(subset(payments_exp, is.na(Product_Category_3)))/NROW(payments_exp) # 95%

# payments_exp <- payments_exp %>%
#   pivot_longer(c('Product_Type_1', 'Product_Type_2', 'Product_Type_3'), 
#              values_to ='Product_Type', names_repair = "unique")
# payments_exp <- subset(payments_exp, !is.na(payments_exp$Product_Type), select = -c(name))
#payments_exp <- payments_exp %>%
#  pivot_longer(c('Product_Category_1', 'Product_Category_2', 'Product_Category_3'), 
#               values_to ='Product_Category', names_repair = "unique")
#payments_exp <- subset(payments_exp, !is.na(payments_exp$Product_Type), select = -c(name))

# (?) how to proceed with categories?
# naive solution: removing all the three columns (?)
# payments_exp <- dplyr::select(payments_exp, -c("Product_Category_1", 
#                                         "Product_Category_2",
#                                        "Product_Category_3"))

# solution 2: removing Product_Category_2 and Product_Category_3 and keeping 
# only Product_Category_1. Also, removing rows with null values of category
# risk: removing 38% of the data.

# solution 3: impute.
# hard: since there are 1678 values.
unique(payments_exp$Product_Category_1)
summary(payments_exp$Product_Category_1)

#Product_Category_1,2,3 checking and if NEUROLOGY then TRUE
payments_exp$Product_Category_1[is.na(payments_exp$Product_Category_1)]<- "Other"
payments_exp$Product_Category_2[is.na(payments_exp$Product_Category_2)]<- "Other"
payments_exp$Product_Category_3[is.na(payments_exp$Product_Category_3)]<- "Other"
payments_exp$NC_PC_Is_NEUROLOGY <- ifelse((
  payments_exp$Product_Category_1=="NEUROLOGY"|
    payments_exp$Product_Category_2=="NEUROLOGY"|
    payments_exp$Product_Category_3=="NEUROLOGY"), TRUE, FALSE)
#Deleting Product Categories
payments_exp <- subset(payments_exp, select = -c(Product_Category_1,Product_Category_2,Product_Category_3))

NROW(subset(payments_exp, is.na(Product_Type_1)))/NROW(payments_exp) # 0.04%
pay_reduced <- subset(payments_exp, !is.na(Product_Type_1), 
                       select=-c(Product_Type_2, Product_Type_3))
names(pay_reduced)[names(pay_reduced) == "Product_Type_1"] <- "Product_Type"
payments_exp <- pay_reduced

#Related Product indicator converted to yeses and nos.
payments_exp$Related_Product_Indicator[payments_exp$Related_Product_Indicator == 'Covered' 
                                       | payments_exp$Related_Product_Indicator == 'Combination' 
                                       | payments_exp$Related_Product_Indicator == 'Non-Covered'] <- 'Yes'
payments_exp$Related_Product_Indicator[payments_exp$Related_Product_Indicator == 'None'] <- 'No'

payements_ML <- subset(payments_exp, select=-c(Record_ID))
payements_ML$Nature_of_Payment_or_Transfer_of_Value <- as.factor(payements_ML$Nature_of_Payment_or_Transfer_of_Value)
payements_ML$Third_Party_Recipient <- as.factor(payements_ML$Third_Party_Recipient)
payements_ML$Related_Product_Indicator <- as.factor(payements_ML$Related_Product_Indicator)
payements_ML$Ownership_Indicator <- as.factor(payements_ML$Ownership_Indicator)
payements_ML$Product_Type <- as.factor(payements_ML$Product_Type)
payements_ML$new_column_year <- as.factor(payements_ML$new_column_year)
payements_ML$new_column_season <- as.factor(payements_ML$new_column_season)

payements_ML$NC_PC_Is_NEUROLOGY <- as.factor(payements_ML$NC_PC_Is_NEUROLOGY)
summary(payements_ML)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### COMPANIES TABLE #####
## State
# null if country not US (?)
# naive solution
companies_exp <- companies
companies_exp <- companies_exp %>% 
  mutate(Company_State_Dir = case_when(
    State == "CT" | State == "ME" | 
      State == "MA" | State == "NH"| 
      State == "RI" |  State == "VT" ~ "NE",
    State == "NY" | State == "NJ" | State == "PA" ~ "NE",
    State == "WI" | State == "MI" | State == "IL" | State == "IN"| 
      State == "OH" ~ "MW",
    State == "ND" | State == "MN" | State == "SD" | 
      State == "IA"| State == "NE" | 
      State == "KS" | State == "MO" ~ "MW",
    State == "DE" | State == "FL" | 
      State == "GA" | State == "MD"| 
      State == "NC" | State == "SC"|
      State == "VA" | State == "WV"|
      State == "DC" | State == "PR" ~ "S",
    State == "AL" | State == "KY" | 
      State == "TN" | State == "MS" ~ "S",
    State == "AR" | State == "TX" | 
      State == "OK" | State == "LA" ~ "S",
    State == "AZ" | State == "CO"| 
      State == "ID" | State == "MT"|
      State == "NV" | State == "NM"|
      State == "UT" | State == "WY" ~ "W",
    State == "AK" | State == "CA" | 
      State == "HI" | State == "OR" |
      State == "WA" ~ "W",
      TRUE ~ "Other"
  ))
companies_exp$Company_State_Dir <- as.factor(companies_exp$Company_State_Dir)
companies_exp <- subset(companies_exp, select = -c(Name, Country, State))
companies_ML <- companies_exp
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### BOOSTING ####
companies_ML_oh <- companies_ML
dummy <- dummyVars(~ . + Company_State_Dir, data = companies_ML)
companies_ML_oh <- data.frame(predict(dummy, newdata = companies_ML))

phys_ML_oh <- physicians_ML
dummy <- dummyVars(~ License_State_Dir + Primary_Specialty
                   + PS_Neurology + State_Dir , data = physicians_ML)

phys_ML_num_feats <- select(physicians_ML, c("id", "set"))
phys_ML_oh <- cbind(phys_ML_num_feats,
                    data.frame(predict(dummy, newdata = physicians_ML)))

colnames(payements_ML)
dummy <- dummyVars(~ Form_of_Payment_or_Transfer_of_Value +
                     Nature_of_Payment_or_Transfer_of_Value +
                     Third_Party_Recipient + Related_Product_Indicator +
                     new_column_year + Product_Type + new_column_season +
                     NC_PC_Is_NEUROLOGY, data = payements_ML)

pay_ML_num_feats <- select(payements_ML, c("Physician_ID",
                                           "Company_ID",
                                           "Total_Amount_of_Payment_USDollars",
                                           "Number_of_Payments",
                                           "Ownership_Indicator"))

pay_ML_oh <- cbind(pay_ML_num_feats,
                   data.frame(predict(dummy, newdata = payements_ML)))

colnames(pay_ML_oh)
companies_ML <- companies_ML_oh
payements_ML <- pay_ML_oh
physicians_ML <- phys_ML_oh

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### JOIN ####

summary(companies_ML)
summary(payements_ML)
summary(physicians_ML)



trainPhysicians <- physicians_ML[physicians_ML$set == 'train', ]
testPhysicians <- physicians_ML[physicians_ML$set == 'test', ]

## all = TRUE removed to not join missing rows from both tables with NA (Full
# Outer Join).
trainPayments <- merge(x = trainPhysicians, y = payements_ML, by.x = "id", by.y = "Physician_ID")
dim(trainPayments) # 5 mio.?

testPayments <- merge(x = testPhysicians, y = payements_ML, by.x = "id", by.y = "Physician_ID")
dim(testPayments)
summary(trainPayments)

finalTableTrain <- merge(x = trainPayments, y = companies_ML, 
                         by.x = "Company_ID", by.y = "Company_ID")
inds <- partition(finalTableTrain$Ownership_Indicator, 
                                   p = c(train = 0.8, valid = 0.2))
finalTableTest <- merge(x = testPayments, y = companies_ML, 
                         by.x = "Company_ID", by.y = "Company_ID")

finalTableVal <- finalTableTrain[inds$valid, ]
finalTableTrain <- finalTableTrain[inds$train, ]

summary(finalTableVal)

ids_test <- dplyr::select(finalTableTest, c("id"))
ids_val <-  dplyr::select(finalTableVal, c("id"))
finalTableTest_grped <- finalTableTest %>% 
                      group_by(id) %>%
                      summarise(oi = max(as.numeric(Ownership_Indicator)))

val_grouped_by_id <- finalTableVal %>% 
  group_by(id) %>%
  summarise(oi = max(as.numeric(Ownership_Indicator)))

val_grouped_by_id$oi <- val_grouped_by_id$oi - 1
finalTableTest_grped$oi <- finalTableTest_grped$oi - 1

finalTableTrain <- dplyr::select(finalTableTrain, -c("set", "id", "Company_ID"))

finalTableTest <- dplyr::select(finalTableTest, -c("set", "id", "Company_ID"))

finalTableVal <- dplyr::select(finalTableVal, -c("set", "id", "Company_ID"))

summary(finalTableTrain)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### CLASSIFICATION ####
## ON VALIDATION DATA

## function to evaluate the model.
get_BAC <- function(ids, predictions, ground_truth, test = FALSE){
  #ids - ids used in the prediction
  # predictions - values of the predictions
  # ground_truth - real value used to compare the predictions against
  
  # return a list of length 2 with 1 being the BAC value and 2 being 
  # the table to be evaluated
  preds <- cbind(ids, predictions)
  colnames(preds) <- c("id", "prediction")
  summary(preds)
  preds$prediction <- as.numeric(preds$prediction)

  grouped_data_by_id <- preds %>% 
    group_by(id) %>%
    summarise(prediction = max(prediction))
  
  grouped_data_by_id$prediction <- as.factor(grouped_data_by_id$prediction)
  summary(grouped_data_by_id)
  
  summary(predictions)
  
  summary(ground_truth$oi)
  confusion_mat <- table(grouped_data_by_id$prediction, ground_truth$oi)
  print(confusion_mat)
  
  ## accuracy
  print("accuracy")
  print(sum(diag(confusion_mat))/sum(confusion_mat))
  
  TP <- confusion_mat[1, 1]
  FP <- ifelse(test, 0, confusion_mat[1, 2])
  FN <- confusion_mat[2, 1]
  TN <- ifelse(test, 0, confusion_mat[2, 2])
  
  sensitivty <- TP/(TP+FN)
  specificty <- TN/(FP+TN)
  
  BAC <- (sensitivty + specificty)/2
  
  print("BAC")
  print(BAC)
  return (list(BAC, grouped_data_by_id))
}

## XGBoost

train_labels <- as.numeric(finalTableTrain$Ownership_Indicator) - 1
train_df <- dplyr::select(finalTableTrain, -c("Ownership_Indicator"))
train_data <- data.matrix(train_df)


val_labels <- as.numeric(finalTableVal$Ownership_Indicator) - 1
val_df <- dplyr::select(finalTableVal, -c("Ownership_Indicator"))
val_data <- data.matrix(val_df)

val_xgmat <- xgb.DMatrix(val_data, label = val_labels)
xgmat <- xgb.DMatrix(train_data, label = train_labels)

watchlist <- list("train" = xgmat)

pos_instances <- sum(train_labels == 1)
neg_instances <- sum(train_labels == 0)
scale_weight <- sqrt(neg_instances/pos_instances)
neg_instances
pos_instances
scale_weight

params <- list(
  eta = 0.5,
  max_depth = 6,
  eval_metric = "logloss",
  scale_pos_weight = scale_weight ** 2,
  objective = "binary:logistic", 
  gamma = 1)


bst <- xgb.train(data=xgmat, params=params, nrounds = 150, 
                 early_stopping_rounds = 3, verbose = TRUE, watchlist = watchlist)
bstSparse.preds <- predict(bst, val_data)

summary(bstSparse.preds)

bstSparse.preds.prob <- as.numeric(bstSparse.preds > 0.5)

print(head(bstSparse.preds.prob))

get_BAC(ids_val, bstSparse.preds.prob, val_grouped_by_id)[1]


# 0    1
# 0 4669   18
# 1  156  134
# [1] "accuracy"
# [1] 0.9650392
# [1] "BAC"
# [1] 0.9246237
# [[1]]
# [1] 0.9246237
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Submission

# ex: using a tree


test_data <- data.matrix(dplyr::select(finalTableTest, -c("Ownership_Indicator")))
bstSparse.preds <- predict(bst, test_data)
bstSparse.preds <- as.numeric(bstSparse.preds > 0.5)
xgb_vals_test <- get_BAC(ids_test, bstSparse.preds, finalTableTest_grped, TRUE)
xgb_vals_test_BAC <- xgb_vals_test[1]
xgb_vals_test_table <- xgb_vals_test[2]

#get_BAC(ids_val, bstSparse.preds, val_grouped_by_id)[1]

write.csv(xgb_vals_test_table, "submit.csv", row.names = FALSE, quote=FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
