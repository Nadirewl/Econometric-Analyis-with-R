
# Install required packages (only run once)
install.packages("readxl")    # For reading Excel files
install.packages("plm")       # For panel data models
install.packages("dplyr")     # For data manipulation
install.packages("ggplot2")   # For visualization

# Load libraries
library(readxl)
library(plm)
library(dplyr)
library(ggplot2)

# Set your working directory (replace with the actual path)

setwd("C:/Users/nadira/Desktop")

# Read Excel file - specify sheet name or number
data <- read_excel("/Users/nadira/Desktop/Elec.xlsx", sheet = 1)

# View the first few rows of the data
head(data)

# Check structure of the data
str(data)

# Check column names and structure
colnames(data)

# Convert to panel data format
pdata <- pdata.frame(data, index = c("Entity", "Year"))


# Rename Column names. Replace '.....electricity' with '_percent'
colnames(pdata) <- gsub("\\.\\.\\.\\.\\.electricity", "_percent", colnames(pdata))

# Check the updated column names
print(colnames(pdata))


# Run the Pooling model
model_pooled <- plm(Electricity_Price ~ Gas_percent + Solar_percent + Oil_percent + Coal_percent + Hydro_percent+ Nuclear_percent+Bioenergy_percent
                    +Other.renewables.excluding.bioenergy_percent, 
                    data = pdata, model = "pooling")
summary(model_pooled)


# Run the Fixed Effect model 

model_FE <- plm(Electricity_Price ~ Gas_percent + Solar_percent + Oil_percent + Coal_percent + Hydro_percent+ Nuclear_percent+Bioenergy_percent
                    +Other.renewables.excluding.bioenergy_percent, 
                    data = pdata, model = "within")
summary(model_FE)

# Run the Random Effect Model 

model_re <- plm(Electricity_Price ~ Gas_percent + Solar_percent + Oil_percent + Coal_percent + Hydro_percent+ Nuclear_percent+Bioenergy_percent
                +Other.renewables.excluding.bioenergy_percent, 
                data=pdata, model = "random")

summary(model_re)


# Since the result is hard to be interprated, coefficients too small, I'll log the dependent variable. 
# Logging Electricity Price (Dependent Variable)
pdata$log_Electricity_Price <- log(pdata$Electricity_Price)

colnames(pdata)
pdata
# Rerun the Pooling model with logged dependent variable 
model_pooled <- plm(log_Electricity_Price ~ Gas_percent + Solar_percent + Oil_percent + Coal_percent + Hydro_percent+ Nuclear_percent+Bioenergy_percent
                    +Other.renewables.excluding.bioenergy_percent, 
                    data = pdata, model = "pooling")
summary(model_pooled)

#Check how many 0s are included in electricity price 
summary(pdata$Electricity_Price)
table(pdata$Electricity_Price == 0)

# Check for Multicollinearity:
library(car)
vif(model_pooled)


