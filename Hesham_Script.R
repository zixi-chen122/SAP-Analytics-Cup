library(tidyverse)
library(tidymodels)


set.seed(2021)
companies <- read.csv("companies.csv")
payments <- read.csv("payments.csv")
physicians <- read.csv("physicians.csv")
previously_released_payments <- read.csv("previously_released_payments.csv")

dim(physicians)
dim(payments)
#switch(substring(payments$Date,4,5), "01"={"one"}, bar={"two"})
trainPhysicians <- physicians[physicians$set == 'train', ]
testPhysicians <- physicians[physicians$set == 'test', ]
## all = TRUE removed to not join missing rows from both tables with NA (Full
# Outer Join).
trainPayments <- merge(x = trainPhysicians, y = payments, by.x = "id", by.y = "Physician_ID")
dim(trainPayments)
summary(trainPayments)
testPayments <- merge(x = testPhysicians, y = payments, by.x = "id", by.y = "Physician_ID")
dim(testPayments)

##### PHYSICIAN TABLE #####
### CITY COL
# In case of using a tree or a forest, this col will be kept.
NROW(unique(physicians$City))

### LICENSE STATE COL
# instead of having 5 cols, we are going to have one col (license state)
# and multiple rows.
physicians <- physicians %>% 
  pivot_longer(c('License_State_1', 'License_State_2', 'License_State_3', 
                 'License_State_4', 'License_State_5'), 
               values_to ='License_State', names_repair = "unique")
physicians <- subset(physicians, !is.na(physicians$License_State), select = -c(name))
dim(physicians)

### PS COL
physicians$Primary_Specialty <- as.character(physicians$Primary_Specialty)
test_p <- physicians
colnames(test_p) <- c(colnames(test_p), "primarySpecialty_count")
for (row in 1: NROW(test_p)){
  split <- unlist(strsplit(test_p$Primary_Specialty[row], '\\|'))
  test_p$primarySpecialty_count[row] <- NROW(split)
}
unique(test_p$primarySpecialty_count)
## there are max 3 specialities, therefore we can create 3 cols to hold them.

physicians[, "Primary_Specialty_1"] <- as.character(NA)
physicians[, "Primary_Specialty_2"] <- as.character(NA)
physicians[, "Primary_Specialty_3"] <- as.character(NA)
for (row in 1: NROW(physicians)){
  split <- unlist(strsplit(physicians$Primary_Specialty[row], '\\|'))
  for (i in 1:length(split)){
    col_name <- sprintf("Primary_Specialty_%d", i)
    physicians[row, col_name] <- split[i]
  }
}
physicians <- subset(physicians, select =-c(Primary_Specialty))
physicians$Primary_Specialty_1 <- as.factor(physicians$Primary_Specialty_1)
physicians$Primary_Specialty_2 <- as.factor(physicians$Primary_Specialty_2)
physicians$Primary_Specialty_3 <- as.factor(physicians$Primary_Specialty_3)
glimpse(physicians)
summary(physicians)
## 4952 NA's in Primary_Specialty_3 and weird values (consider removing this col)

## POSSIBLIE SOLUTION: LONGER PIVOT
## PROBLEM: if a physician have multiple license states and multiple specs.
#, it will be seen as if he is specialized in this field in this license state,
# not specialized in this field in general. (?)
physicians <- physicians %>% 
  pivot_longer(c('Primary_Specialty_3', 'Primary_Specialty_2', 'Primary_Specialty_1'), 
               values_to ='Primary_Specialty', names_repair = "unique")
physicians <- subset(physicians, !is.na(physicians$Primary_Specialty), select = -c(name))
dim(physicians)


## ZIP CODE COL
## UNDERQUESTION

##PROVINCE COL
physicians <- subset(physicians, select = -c(Province))

physicians_ML <- physicians %>% select(-contains("Name"))

summary(physicians_ML)


## Physicians Primary Speciality
#Physicians Primary Speciality checking and if NEUROLOGY then TRUE
physicians$Primary_Specialty[is.na(physicians$Primary_Specialty)]<- "Other"
physicians$PS_Neurology <- ifelse((physicians$Primary_Specialty=="Chiropractic Providers|Chiropractor|Neurology"|physicians$Primary_Specialty=="Allopathic & Osteopathic Physicians|Psychiatry & Neurology|Psychiatry"|physicians$Primary_Specialty=="Allopathic & Osteopathic Physicians|Psychiatry & Neurology|Neuromuscular Medicine"|physicians$Primary_Specialty=="Allopathic & Osteopathic Physicians|Psychiatry & Neurology|Neurology"|physicians$Primary_Specialty=="Allopathic & Osteopathic Physicians|Neurological Surgery"|physicians$Primary_Specialty=="Allopathic & Osteopathic Physicians|Psychiatry & Neurology|Neuromuscular Medicine"|physicians$Primary_Specialty=="Allopathic & Osteopathic Physicians|Psychiatry & Neurology|Psychiatry"|physicians$Primary_Specialty=="Chiropractic Providers|Chiropractor|Neurology"|physicians$Primary_Specialty=="Allopathic & Osteopathic Physicians|Psychiatry & Neurology|Neurology with Special Qualifications in Child Neurology"), TRUE, FALSE)


## 2 NA State, 2 "UNITED STATES MINOR OUTLYING ISLANDS" in Country.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### PAYEMENTS TABLE #####

payments_exp <- payments %>% as_tibble()
## DATE
## TODO: TO BE REVISITED ##

## NUMBER OF PAYEMENTS
subset(payments_exp, Number_of_Payments == 3)[order(payments_exp$Company_ID), ]
## IS THERE A TRANS ID? == Yes record id.
## ?

## Form_of_Payment_or_Transfer_of_Value
unique(payments_exp$Form_of_Payment_or_Transfer_of_Value)
form_of_pay_col <- payments_exp$Form_of_Payment_or_Transfer_of_Value
levels(form_of_pay_col)[
  levels(form_of_pay_col)=="Stock, stock option, or any other ownership interest"
  | levels(form_of_pay_col)=="Any other ownership interest"] <- "Other ownership interest"
unique(form_of_pay_col)

NROW(subset(payments_exp, is.na(form_of_pay_col)))

## Nature_of_Payment_or_Transfer_of_Value
nature_of_pay_travel <- subset(payments_exp, Nature_of_Payment_or_Transfer_of_Value == 
              'Travel and Lodging')
NROW(nature_of_pay_travel)
# Since there are only 75179 rows with Travel and Lodging (very low number),
# the cols (City_of_Travel, State_of_Travel and Country_of_Travel)
# will be removed
payments_exp <- select(payments_exp, -c("City_of_Travel", "State_of_Travel", "Country_of_Travel"))

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
# (?)

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
payments_exp <- select(payments_exp, -c("Charity"))
# should it be kept (?) if na's are going to be converted to nos then yeah.

## Third_Party_Covered
# lots of nulls and included in Third_Party_Recipient
payments_exp <- select(payments_exp, -c("Third_Party_Covered"))

## Contextual Info. 
# hard to get any data from it, therefore removed.
payments_exp <- select(payments_exp, -c("Contextual_Information"))

## PRODUCT

# Names and codes will be removed.
payments_exp <- payments_exp %>% select(-contains("Name"))
payments_exp <- payments_exp %>% select(-contains("Code"))

# pivot product type and category to rows.
payments_exp <- payments_exp %>%
  pivot_longer(c('Product_Type_1', 'Product_Type_2', 'Product_Type_3'), 
             values_to ='Product_Type', names_repair = "unique")
payments_exp <- subset(payments_exp, !is.na(payments_exp$Product_Type), select = -c(name))
#payments_exp <- payments_exp %>%
#  pivot_longer(c('Product_Category_1', 'Product_Category_2', 'Product_Category_3'), 
#               values_to ='Product_Category', names_repair = "unique")
#payments_exp <- subset(payments_exp, !is.na(payments_exp$Product_Type), select = -c(name))
NROW(subset(payments_exp, is.na(Product_Category_1)))/NROW(payments_exp) # 38%
NROW(subset(payments_exp, is.na(Product_Category_2)))/NROW(payments_exp) # 75%
NROW(subset(payments_exp, is.na(Product_Category_3)))/NROW(payments_exp) # 88%

# (?) how to proceed with categories?
# naive solution: removing all the three columns (?)
#payments_exp <- select(payments_exp, -c("Product_Category_1", 
#                                        "Product_Category_2",
#                                       "Product_Category_3"))

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
payments_exp$NC_PC_Is_NEUROLOGY <- ifelse((payments_exp$Product_Category_1=="NEUROLOGY"|payments_exp$Product_Category_2=="NEUROLOGY"|payments_exp$Product_Category_3=="NEUROLOGY"), TRUE, FALSE)
#Deleting Product Categories
payments_exp <- subset(payments_exp, select = -c(Product_Category_1,Product_Category_2,Product_Category_3))

#Deleting Product Categories
payments_exp <- subset(payments_exp, select = -c(Product_Category_1,Product_Category_2,Product_Category_3))

##### COMPANIES TABLE #####
## State
# null if country not US (?)

#### PHYS NOT FROM US #####
trainPhysicians[trainPhysicians$Country != 'UNITED STATES', ]

companies[is.na(companies$Company_ID), ] # There is not such a company exists.
NROW(companies[companies$State == 'NY', ])
NROW(companies[companies$State != 'NY', ])
subset(companies, State == 'NY')

#### NULL PAYEMENTS WITH PRODUCT #####
NROW(subset(payments, is.na(Product_Type_1)))

NROW(payments[is.na(payments$City_of_Travel), ])
NROW(subset(payments, is.na(City_of_Travel)))

NROW(subset(payments, !is.na(City_of_Travel)))

NROW(subset(payments, Nature_of_Payment_or_Transfer_of_Value == 'Travel and Lodging'))
subset(payments, !is.na(Contextual_Information) & Ownership_Indicator == 'No')[, c('Contextual_Information', 'Ownership_Indicator')]

physicians$License_State_2
NROW(subset(physicians, is.na(physicians$License_State_1)))
NROW(subset(physicians, is.na(physicians$License_State_2)))
NROW(subset(physicians, is.na(physicians$License_State_3)))
NROW(subset(physicians, is.na(physicians$License_State_4)))
NROW(subset(physicians, is.na(physicians$License_State_5)))

glimpse(train)
summary(train)
NROW(payments)
summary(payments)

summary(train_pay)
