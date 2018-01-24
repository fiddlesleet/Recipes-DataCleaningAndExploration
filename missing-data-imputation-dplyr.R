library(dplyr)

# read in csv, with blank values registered as "NA" strings 
#  (NA vals already registered as "NA" by default)
fin <- tbl_df(read.csv("/Users/hannahsmythe/Downloads/Future-500.csv", na.strings = c("")))
fin
str(fin)
summary(fin)

# convert ID to factor
fin$ID <- factor(fin$ID)
str(fin)

# convert Inception (year) to factor
fin$Inception <- factor(fin$Inception)
summary(fin)

#### METHOD FOR CONVERTING FACTORS TO NUMERIC -- MUST CONVERT TO CHAR FIRST 

# convert Expenses factor to numeric
# sample of original structure: 1,026,564 Dollars
fin$Expenses <- gsub(" Dollars", "", fin$Expenses)
summary(fin)
head(fin)
fin$Expenses <- gsub(",", "", fin$Expenses)
summary(fin)
head(fin)
# convert to factor from character type
fin$Expenses <- as.numeric(fin$Expenses)
head(fin)

# convert Revenue factor to numeric 
# sample of original structure: 1,614,585, <factor>
fin$Revenue <- gsub("\\$", "", fin$Revenue) # must use \\$ since $ is a special char in R
summary(fin)
head(fin)
fin$Revenue <- gsub(",", "", fin$Revenue)
summary(fin)
head(fin)
fin$Revenue <- as.numeric(fin$Revenue)

# convert Growth to factor
# sample of original structure: 19% <factor>
fin$Growth <- gsub("%", "", fin$Growth)
summary(fin)
head(fin)
fin$Growth <- as.numeric(fin$Growth)
head(fin)
summary(fin)

# locate missing data
head(fin, 24)
# create boolean vector of which rows have no NA values ( == TRUE)
complete.cases(fin)
# subset fin df to all rows with an NA value
fin[!complete.cases(fin),]

# filtering: using which() for non-missing data
# which() returns only the rows that have the value as "TRUE'; does
# not include rows that have the value as "NA"

# here, returns rows that have value as "NA":
fin[fin$Revenue == 9746272,]

# here, omits the rows that have fin$Revenue as NA; lists the row numbers
which(fin$Revenue == 9746272)
fin[which(fin$Revenue == 9746272),]

# find rows that have NA values in Expenses col
fin[is.na(fin$Expenses),]
# find rows that have NA values in Industry col
fin[is.na(fin$Industry),]

# removing records with missing data
# in this case, will remove rows with col Industry NAs, as we can't do the analysis
# without knowing industry, and can't research industry for this fake dataset

# create backup copy of df thus far to reset to in case make error
fin_backup <- fin
#fin <- fin_backup
# find Industry NAs
fin[is.na(fin$Industry),]
# which rows DON'T have Industry NAs
fin[!is.na(fin$Industry),]

# remove Industry NA rows
fin <- fin[!is.na(fin$Industry),]
fin
fin[is.na(fin$Industry),] # check, indeed have been removed
# rename row numbers accordingly, which R does not auto'ly do with df
# not necessary with tibble
#rownames(fin) <- 1:nrow(fin)
# OR
#rownames(fin) <- NULL # reset row names

# replacing data with missing values: factual analysis

# fill in state abbreviations when we recognize the city
fin[is.na(fin$State),] # get rows with state == NA
# get rows with State == NA & city = NYC
fin[is.na(fin$State) & fin$City == "New York",] 
# update State value for rows with State == NA & city = NYC 
fin[is.na(fin$State) & fin$City == "New York", "State"] <- "NY"
# check remaining State NAs
fin[is.na(fin$State),] # get rows with state == NA
# get rows with State == NA and city = "San Francisco"
fin[is.na(fin$State) & fin$City == "San Francisco",]
# update State value for rows with State == NA and city = "San Francisco"
fin[is.na(fin$State) & fin$City == "San Francisco", "State"] <- "CA"
fin[fin$City == "San Francisco",]

### proxy missing Employee counts with Industry median
# create backup copy of df thus far to reset to in case make error
fin_backup <- fin
#fin <- fin_backup

# median imputation function
med_industries_missing_employees <- function(fin, df) {
  median_employees <- lapply(median(fin$Employees[fin$Industry == df[3]], na.rm=TRUE))
  return(median_employees)
}

# find all entries with missing Employee number
industries_missing_employees <- fin[is.na(fin$Employees),] 
med_industries_missing_employees(fin, industries_missing_employees)

# median number of Employees in retail Industry
median_employee_retail <- median(fin$Employees[fin$Industry == "Retail"], na.rm=TRUE)

# find retail industry entries with Employees == NA
fin[is.na(fin$Employees) & fin$Industry == "Retail",]

# update retail industry entries with Employees == NA with median_employee_retail
fin[is.na(fin$Employees) & fin$Industry == "Retail", "Employees"] <- median_employee_retail

# repeat with Financial Services
# TODO: MAKE FUNCTION
median_employee_fs <- median(fin$Employees[fin$Industry == "Financial Services"], na.rm=TRUE)

# find retail industry entries with Employees == NA
fin[is.na(fin$Employees) & fin$Industry == "Financial Services",]

# update retail industry entries with Employees == NA with median_employee_retail
fin[is.na(fin$Employees) & fin$Industry == "Financial Services", "Employees"] <- median_employee_fs
fin[330,]

## impute missing Growth using industry growth
fin[is.na(fin$Growth),] # Construction is the industry for only growth NA
median_growth_construction <- median(fin$Growth[fin$Industry == "Construction"], na.rm = TRUE)
fin[is.na(fin$Growth) & fin$Industry == "Construction", "Growth"] <- median_growth_construction
fin[8,]


# what do we have left to impute?
fin[!complete.cases(fin),]

## impute Revenue for the two Construction Industry rows missing it
median_revenue_construction <- median(fin$Revenue[fin$Industry == "Construction"], na.rm = TRUE)
fin[is.na(fin$Revenue) & fin$Industry == "Construction", "Revenue"] <- median_revenue_construction

# impute Expenses for 2 construction cases if Profit also missing
median_exp_construction <- median(fin$Expenses[fin$Industry == "Construction"], na.rm = TRUE)
fin[is.na(fin$Expenses) & is.na(fin$Profit) & fin$Industry == "Construction",
    "Expenses"] <- median_exp_construction

# what do we have left to impute?
fin[!complete.cases(fin),]

## impute expenses when profit exists
fin[is.na(fin$Expenses), "Expenses"] <- fin$Revenue[is.na(fin$Expenses)] - fin$Profit[is.na(fin$Expenses)]
fin[fin$Name == "Ganzlax",]
fin[15,]
# what do we have left to impute?
fin[!complete.cases(fin),]

# impute profit for 2 construction industry entries with NA
fin[is.na(fin$Profit) & fin$Industry == "Construction", "Profit"] <- fin$Revenue[is.na(fin$Profit)] -
                                                                     fin$Expenses[is.na(fin$Profit)]
fin[c(8,42),]

# what do we have left to impute?
fin[!complete.cases(fin),]
