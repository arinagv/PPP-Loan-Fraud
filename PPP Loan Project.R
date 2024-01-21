#-------------------------------------------------------------------------
#---- Preparing workspace, loading data 
#-------------------------------------------------------------------------
library("data.table")
library("readr")
library("bit64")
#library("ff")
library("dplyr")
library("tidyverse")
library("ggplot2")

options(scipen = 0)

setwd("/Users/arinavoronina")
#----------Importing data-----------
#### Adding borrower industry, merging using loan number
loans <- fread("~/Documents/Big Data Econ/PPP details.csv",
               select = c("Loan number", "Industry"))

# reformatting name
names(loans)[names(loans) == "Loan number"] <- "LoanNumber"


#### combine with the 150k plus and under 150k datasets 
under150.1 <- fread("~/Documents/Big Data Econ/public_up_to_150k_1.csv")

columns_to_drop <- c("SBAOfficeCode", "BorrowerAddress", "BorrowerCity",
                     "BorrowerZip", "Term", "SBAGuarantyPercentage",
                     "ServicingLenderLocationID", "ServicingLenderAddress",
                     "ServicingLenderCity", "ServicingLenderZip", "NAICSCode",
                     "UndisbursedAmount","OriginatingLenderLocationID", "ForgivenessDate",
                     "ProjectCity", "ProjectCountyName", "ProjectZip", "ProjectState",
                     "DEBT_INTEREST_PROCEED", "OriginatingLenderCity", "REFINANCE_EIDL_PROCEED")


under150.1 <- under150.1[, !(names(under150.1) %in% columns_to_drop), with = F]

under150k.2 <- fread("~/Documents/Big Data Econ/public_up_to_150k_2.csv", drop = columns_to_drop)
under150k.3 <- fread("~/Documents/Big Data Econ/public_up_to_150k_3.csv", drop = columns_to_drop)
under150k.4 <- fread("~/Documents/Big Data Econ/public_up_to_150k_4.csv", drop = columns_to_drop)
under150k.5 <- fread("~/Documents/Big Data Econ/public_up_to_150k_5.csv", drop = columns_to_drop)
under150k.6 <- fread("~/Documents/Big Data Econ/public_up_to_150k_6.csv", drop = columns_to_drop)
under150k.7 <- fread("~/Documents/Big Data Econ/public_up_to_150k_7.csv", drop = columns_to_drop)
under150k.8 <- fread("~/Documents/Big Data Econ/public_up_to_150k_8.csv", drop = columns_to_drop)
under150k.9 <- fread("~/Documents/Big Data Econ/public_up_to_150k_9.csv", drop = columns_to_drop)
under150k.10 <- fread("~/Documents/Big Data Econ/public_up_to_150k_10.csv", drop = columns_to_drop)
under150k.11 <- fread("~/Documents/Big Data Econ/public_up_to_150k_11.csv", drop = columns_to_drop)
under150k.12 <- fread("~/Documents/Big Data Econ/public_up_to_150k_12.csv", drop = columns_to_drop)
over150k <- fread("~/Documents/Big Data Econ/public_150k_plus.csv", drop = columns_to_drop)
names(over150k)[names(over150k) == "CongressDistrict"] <- "CD"



Lenders <- as.matrix(unique(as.factor(loans$)))

lenders.list <- unique(Lenders)

fwrite(lenders.list, "lenders.csv")




loans <- rbind(under150.1, under150k.2, under150k.3, under150k.4)
loans <- rbind(loans, under150k.5, under150k.6, under150k.7, under150k.8)
loans<- rbind(loans, under150k.9, under150k.10, under150k.11, under150k.12)

loans <- rbind(loans, over150k)

loans <- merge(loans, under150.1
               , by = "LoanNumber"
               , all.x = TRUE
               , all.y = FALSE   # LEFT join
               )
loans <- merge(loans, under150k.2
               , by = "LoanNumber"
               , all.x = TRUE, all.y = FALSE)
rm(under150.1)
rm(under150k.2)
loans <- merge(loans, under150k.3
               , by = "LoanNumber"
               , all.x = TRUE, all.y = FALSE)
loans <- merge(loans, under150k.4
               , by = "LoanNumber"
               , all.x = TRUE, all.y = FALSE)
loans <- merge(loans, under150k.5
               , by = "LoanNumber"
               , all.x = TRUE, all.y = FALSE)
loans <- merge(loans, under150k.6
               , by = "LoanNumber"
               , all.x = TRUE, all.y = FALSE)
loans <- merge(loans, under150k.7
               , by = "LoanNumber"
               , all.x = TRUE, all.y = FALSE)
loans <- merge(loans, under150k.8
               , by = "LoanNumber"
               , all.x = TRUE, all.y = FALSE)

head(loans$loan.number)
loans$loan.number <- 

fraudsters <- grep("72539488-07", loans$loan.number)
print(loans[fraudsters, ])
#-------------------------------------------------------------------------
#---- Cleaning data
#-------------------------------------------------------------------------

#### Clean up text strings ####

colnames(loans) <- tolower(colnames(loans))
colnames(loans) <- gsub("\\(|\\)|\\*", "", colnames(loans))    # Get rid of special characters in column names
colnames(loans) <- gsub(" ", ".", colnames(loans))

loans$borrower <- gsub("\\. ", "", loans$borrower)  # Get rid of weird formatting issues in borrower names like ". "
loans$borrower <- gsub("\\: ", "", loans$borrower)
loans$borrower <- gsub("[:;]", "", loans$borrower)
loans$borrower <- gsub("\\?S", "\\'S", loans$borrower) # A lot of business names with CHARLIE?S instead of 'S


#fwrite(loans, <loans_cleaned.csv>)

#-------------------------------------------------------------------------
#---- Exploratory Data Analysis
#-------------------------------------------------------------------------

ABE <- filter(loans, loans$loan.amount == 425000)
print(ABE)


#### Who qualified and who didn't ####


#### How  many jobs were retained ####

table(loans$jobs.reported == 0)

table(loans$payroll == 0)

under150.1$LoanNumber <- as.numeric(under150.1$LoanNumber)

#-------------------------------------------------------------------------
#---- Narrowing scope of the central question
#-------------------------------------------------------------------------




