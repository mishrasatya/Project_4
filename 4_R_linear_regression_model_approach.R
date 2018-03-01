#--------------------------Gelly Auto----------------------------#
#Scope of work#

#Chinese automobile company Geely Auto wants to setup a manufacturing unit#
#in US so that it can compete in US and Europe markets#
#They want to know on which factors the price depends#
#Primarily it wants to know which variables are significant in predicting the price of car#
#and how well they describe the price of car#
#----------------------------------------------------------------#
#Data Dictionary#

#Car_ID-Unique id-Interger#
#Symboling-Insurance risk rating, A value of +3 indicates that the auto is risky, -3 that it is probably pretty safe.-Categorical#
#carCompany-Name-Categorical#
#fueltype-Fuel type-Categorical#
#aspiration-Aspiration used in a car-Categorical#
#doornumber-No. of doors-Categorical#
#carbody-Body of car-Categorical#
#drivewheel-Type of drive wheel-Categorical#
#enginelocation-Location of car engine-Categorical#
#wheelbase-Weelbase of car-Numeric#
#carlength-Length of car-Numeric#
#carwidth-Width of car-Numeric#
#carheight-Height of car-Numeric#
#curbweight-The weight of a car without occupants or baggage.-Numeric#
#enginetype-Type of engine.-Categorical#
#cylindernumber-Cylinder placed in the car-Categorical#
#enginesize-Size of car-Numeric#
#fuelsystem-Fuel system of car-Categorical#
#boreratio-Boreratio of car-Numeric#
#stroke-Stroke or volume inside the engine-Numeric#
#compressionratio-compression ratio of car-Numeric#
#horsepower-Horsepower-Numeric#
#peakrpm-Car peak rpm-Numeric#
#citympg-Mileage in city-Numeric#
#highwaympg-Mileage on highway-Numeric#
#price-Price of car-Numeric - To be Predicted#
#-------------------------------------------------------------------#



#Loading CarPrice file to R console#
gelly_auto <- read.csv("CarPrice_Assignment.csv",header = TRUE, sep = ",")

View(gelly_auto)

#Examining the structure of the dataset#
str(gelly_auto)

#-------------------------------------------------------------------#

#Loading the required library
library(MASS)
library(car)
library(tidyr)
library(stringr)

#-------------------------------------------------------------------#
#DUMMY VARIABLE CREATION FOR VARIABLES HAVING TWO OPTIONS#
#-------------------------------------------------------------------#

#Checking the structure & summary of variable "fueltype"#
str(gelly_auto$fueltype)
summary(factor(gelly_auto$fueltype))

#Converting fueltype variable to numeric so as to replace the levels- gas and diesel with 1 and 0#
levels(gelly_auto$fueltype)<-c(0,1)

#Storing the numeric values in the same variable#
gelly_auto$fueltype<- as.numeric(levels(gelly_auto$fueltype))[gelly_auto$fueltype]

# Checking the summary of variable after conversion#
summary(factor(gelly_auto$fueltype))

#-------------------------------------------------------------------#

#Checking the structure & summary of variable "aspiration"#
str(gelly_auto$aspiration)
summary(factor(gelly_auto$aspiration))

#Converting aspiration variable to numeric so as to replace the levels- std and turbo with 1 and 0#
levels(gelly_auto$aspiration)<-c(1,0)

#Storing the numeric values in the same variable#
gelly_auto$aspiration<- as.numeric(levels(gelly_auto$aspiration))[gelly_auto$aspiration]

# Checking the summary of variable after conversion#
summary(factor(gelly_auto$aspiration))

#------------------------------------------------------------------#

#Checking the structure & summary of variable "doornumber"#
str(gelly_auto$doornumber)
summary(factor(gelly_auto$doornumber))

#Converting doornumber variable to numeric so as to replace the levels- four and two with 1 and 0#
levels(gelly_auto$doornumber)<-c(1,0)

#Storing the numeric values in the same variable#
gelly_auto$doornumber<- as.numeric(levels(gelly_auto$doornumber))[gelly_auto$doornumber]

# Checking the summary of variable after conversion#
summary(factor(gelly_auto$doornumber))

#-----------------------------------------------------------------#

#Checking the structure & summary of variable "enginelocation"#
str(gelly_auto$enginelocation)
summary(factor(gelly_auto$enginelocation))

#Converting doornumber variable to numeric so as to replace the levels- front and rear with 1 and 0#
levels(gelly_auto$enginelocation)<-c(1,0)

#Storing the numeric values in the same variable#
gelly_auto$enginelocation<- as.numeric(levels(gelly_auto$enginelocation))[gelly_auto$enginelocation]

# Checking the summary of variable after conversion#
summary(factor(gelly_auto$enginelocation))

#-------------------------------------------------------------------#
#DUMMY VARIABLE CREATION FOR VARIABLES HAVING MORE THAN TWO OPTIONS#
#-------------------------------------------------------------------#

#Checking the summary of variable "carbody"#
summary(factor(gelly_auto$carbody))

#Converting "carbody" into dummies#
carbody_dummy <- data.frame(model.matrix( ~carbody, data = gelly_auto))

#Removing the x-intercept from the newly created carbody_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 4 variables#
carbody_dummy <- carbody_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
gelly_auto_carbody <- cbind(gelly_auto[,-7], carbody_dummy)

#-----------------------------------------------------------------#

#Checking the summary of variable "drivewheel"#
summary(factor(gelly_auto_carbody$drivewheel))

#Converting "drivewheel" into dummies#
drivewheel_dummy <- data.frame(model.matrix( ~drivewheel, data = gelly_auto_carbody))

#Removing the x-intercept from the newly created drivewheel_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 2 variables#
drivewheel_dummy <- drivewheel_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
gelly_auto_drivewheel <- cbind(gelly_auto_carbody[,-7], drivewheel_dummy)

#-----------------------------------------------------------------#

#Checking the summary of variable "enginetype"#
summary(factor(gelly_auto_drivewheel$enginetype))

#Converting "enginetype" into dummies#
enginetype_dummy <- data.frame(model.matrix( ~enginetype, data = gelly_auto_drivewheel))

#Removing the x-intercept from the newly created enginetype_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 6 variables#
enginetype_dummy <- enginetype_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
gelly_auto_enginetype <- cbind(gelly_auto_drivewheel[,-13], enginetype_dummy)

#-----------------------------------------------------------------#

#Checking the summary of variable "cylindernumber"#
summary(factor(gelly_auto_enginetype$cylindernumber))

#Converting "cylindernumber" into dummies#
cylindernumber_dummy <- data.frame(model.matrix( ~cylindernumber, data = gelly_auto_enginetype))

#Removing the x-intercept from the newly created cylindernumber_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 6 variables#
cylindernumber_dummy <- cylindernumber_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
gelly_auto_cylindernumber <- cbind(gelly_auto_enginetype[,-13], cylindernumber_dummy)

#-----------------------------------------------------------------#

#Checking the summary of variable "fuelsystem"#
summary(factor(gelly_auto_cylindernumber$fuelsystem))

#Converting "fuelsystem" into dummies#
fuelsystem_dummy <- data.frame(model.matrix( ~fuelsystem, data = gelly_auto_cylindernumber))

#Removing the x-intercept from the newly created fuelsystem_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 7 variables#
fuelsystem_dummy <- fuelsystem_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
gelly_auto_fuelsystem <- cbind(gelly_auto_cylindernumber[,-14], fuelsystem_dummy)

#-----------------------------------------------------------------#
#Cleansing the CarName variable# 
#-----------------------------------------------------------------#

#Separating out the brand and model from "CarName and creating two new variables#
gelly_auto_carname <- separate(gelly_auto_fuelsystem,CarName, c("Brand_Name", "Model_Name"), sep = " ", remove = TRUE)

#Removing the model name column as we do not require it for our analysis#
gelly_auto_carname <- gelly_auto_carname[,-4]

#Making all the data in the "Brand_Name" column to proper case#
gelly_auto_carname$Brand_Name <- str_to_title(gelly_auto_carname$Brand_Name)

#Checking the summary of variable "Brand_Name"#
summary(factor(gelly_auto_carname$Brand_Name))

#Replacing the typo errors in the "Brand_Name" column with the correct name#
gelly_auto_carname$Brand_Name <- factor(gsub("Maxda", "Mazda", gelly_auto_carname$Brand_Name))
gelly_auto_carname$Brand_Name <- factor(gsub("Vw", "Volkswagen", gelly_auto_carname$Brand_Name))
gelly_auto_carname$Brand_Name <- factor(gsub("Vokswagen", "Volkswagen", gelly_auto_carname$Brand_Name))
gelly_auto_carname$Brand_Name <- factor(gsub("Porcshce", "Porsche", gelly_auto_carname$Brand_Name))
gelly_auto_carname$Brand_Name <- factor(gsub("Toyouta", "Toyota", gelly_auto_carname$Brand_Name))

#Converting "Brand_Name" into dummies#
Brand_Name_dummy <- data.frame(model.matrix( ~Brand_Name, data = gelly_auto_carname))

#Removing the x-intercept from the newly created Brand_Name_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 21 variables#
Brand_Name_dummy <- Brand_Name_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
gelly_auto_brandname <- cbind(gelly_auto_carname[,-3], Brand_Name_dummy)

#-----------------------------------------------------------------#
#Cleansing the Symboling variable# 
#-----------------------------------------------------------------#

#Checking the structure & summary of variable "Symboling"#
str(gelly_auto_brandname$symboling)
summary(factor(gelly_auto_brandname$symboling))

#converting the variable to factor#
gelly_auto_brandname$symboling <- as.factor(gelly_auto_brandname$symboling)

#Converting "symboling" into dummies#
symboling_dummy <- data.frame(model.matrix( ~symboling, data = gelly_auto_brandname))

#Removing the x-intercept from the newly created symboling_dummy dataframe,#
#using the n-1 principal of level creation and keeping the rest 5 variables#
symboling_dummy <- symboling_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
gelly_final <- cbind(gelly_auto_brandname[,-2], symboling_dummy)

#-----------------------------------------------------------------#
#Cleansing the "car_ID" variable# 
#-----------------------------------------------------------------#

#Removing the "car_ID" column as it just holds unique identifier of each row#
gelly_final <- gelly_final[-1]

#-----------------------------------------------------------------#
#Eye balling the correlation among all the variables#
#-----------------------------------------------------------------#

#Checking if the correlation matrix to check on insights.
correlate <- cor(gelly_final)
View(correlate)

#-----------------------------------------------------------------#
#Dividing gelly_final into two datasets, train and test datasets
#-----------------------------------------------------------------#

#Setting the seed to 50# 
set.seed(50)

#Now we randomly generate row indices for train dataset
trainindices <- sample(1:nrow(gelly_final), 0.7*nrow(gelly_final))

#Generating the train data set
gelly_train <- gelly_final[trainindices,]

#Storing the rest of the observations into an object "gelly_test".
gelly_test <- gelly_final[-trainindices,]

#----------------------------------------------------------------#
#Model creation begins#
#----------------------------------------------------------------#
#--------------------------MODEL-1-------------------------------#

#Creating the first multilinear model from the training data set#
gellymodel_1 <- lm(price~.,data = gelly_train)

#Checking the summary of model 
summary(gellymodel_1)

#Using the step AIC function to quickly remove not required variables for the calculation# 
gelly_step <- stepAIC(gellymodel_1, direction = "both")

#calling the dataset#
gelly_step

#----------------------------------------------------------------#
#--------------------------MODEL-2-------------------------------#

#Creating the 2nd Model after executing the above mentioned step AIC# 
gellymodel_2 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                     wheelbase + carlength + carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + peakrpm + citympg + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
                     drivewheelrwd + enginetypel + enginetypeohc + enginetypeohcf + 
                     enginetypeohcv + enginetyperotor + cylindernumberfour + cylindernumberthree + 
                     fuelsystem2bbl + fuelsystemmpfi + Brand_NameAudi + Brand_NameBmw + 
                     Brand_NameBuick + Brand_NameDodge + Brand_NameMazda + Brand_NameMitsubishi + 
                     Brand_NameNissan + Brand_NamePlymouth + Brand_NamePorsche + 
                     Brand_NameSaab + symboling2 + symboling3, data = gelly_train)

#Checking the summary of model
summary(gellymodel_2)

# Passing the model into the vif function
vif(gellymodel_2)

#----------------------------------------------------------------#
#--------------------------MODEL-3-------------------------------#

# removing drivewheelrwd variable based on High VIF and insignificance (p>0.05)
# Making a new model without drivewheelrwd variable

#Creating the 3rd Model without the above mentioned field# 
gellymodel_3 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                     wheelbase + carlength + carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + peakrpm + citympg + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
                     enginetypel + enginetypeohc + enginetypeohcf + 
                     enginetypeohcv + enginetyperotor + cylindernumberfour + cylindernumberthree + 
                     fuelsystem2bbl + fuelsystemmpfi + Brand_NameAudi + Brand_NameBmw + 
                     Brand_NameBuick + Brand_NameDodge + Brand_NameMazda + Brand_NameMitsubishi + 
                     Brand_NameNissan + Brand_NamePlymouth + Brand_NamePorsche + 
                     Brand_NameSaab + symboling2 + symboling3, data = gelly_train)


#Checking the summary of model
summary(gellymodel_3)

# Passing the model into the vif function
vif(gellymodel_3)

#----------------------------------------------------------------#
#--------------------------MODEL-4-------------------------------#

# removing drivewheelfwd variable based on High VIF and insignificance (p>0.05)
# Making a new model without drivewheelfwd variable

#Creating the 4th Model without the above mentioned fields# 
gellymodel_4 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                     wheelbase + carlength + carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + peakrpm + citympg + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + carbodywagon +  
                     enginetypel + enginetypeohc + enginetypeohcf + 
                     enginetypeohcv + enginetyperotor + cylindernumberfour + cylindernumberthree + 
                     fuelsystem2bbl + fuelsystemmpfi + Brand_NameAudi + Brand_NameBmw + 
                     Brand_NameBuick + Brand_NameDodge + Brand_NameMazda + Brand_NameMitsubishi + 
                     Brand_NameNissan + Brand_NamePlymouth + Brand_NamePorsche + 
                     Brand_NameSaab + symboling2 + symboling3, data = gelly_train)


#Checking the summary of model
summary(gellymodel_4)

# Passing the model into the vif function
vif(gellymodel_4)

#----------------------------------------------------------------#
#--------------------------MODEL-5-------------------------------#

# removing citympg variable based on High VIF and insignificance (p>0.05)
# Making a new model without citympg variable

#Creating the 5th Model without the above mentioned fields# 
gellymodel_5 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                     wheelbase + carlength + carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + carbodywagon +  
                     enginetypel + enginetypeohc + enginetypeohcf + 
                     enginetypeohcv + enginetyperotor + cylindernumberfour + cylindernumberthree + 
                     fuelsystem2bbl + fuelsystemmpfi + Brand_NameAudi + Brand_NameBmw + 
                     Brand_NameBuick + Brand_NameDodge + Brand_NameMazda + Brand_NameMitsubishi + 
                     Brand_NameNissan + Brand_NamePlymouth + Brand_NamePorsche + 
                     Brand_NameSaab + symboling2 + symboling3, data = gelly_train)


#Checking the summary of model
summary(gellymodel_5)

# Passing the model into the vif function
vif(gellymodel_5)

#----------------------------------------------------------------#
#--------------------------MODEL-6-------------------------------#

# removing Brand_NameMazda variable based on High VIF and insignificance (p>0.05)
# Making a new model without Brand_NameMazda variable

#Creating the 6th Model without the above mentioned fields# 
gellymodel_6 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                     wheelbase + carlength + carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + carbodywagon +  
                     enginetypel + enginetypeohc + enginetypeohcf + 
                     enginetypeohcv + enginetyperotor + cylindernumberfour + cylindernumberthree + 
                     fuelsystem2bbl + fuelsystemmpfi + Brand_NameAudi + Brand_NameBmw + 
                     Brand_NameBuick + Brand_NameDodge + Brand_NameMitsubishi + 
                     Brand_NameNissan + Brand_NamePlymouth + Brand_NamePorsche + 
                     Brand_NameSaab + symboling2 + symboling3, data = gelly_train)


#Checking the summary of model
summary(gellymodel_6)

# Passing the model into the vif function
vif(gellymodel_6)

#----------------------------------------------------------------#
#--------------------------MODEL-7-------------------------------#

# removing Brand_NameNissan variable based on High VIF and insignificance (p>0.05)
# Making a new model without Brand_NameNissan variable

#Creating the 7th Model without the above mentioned fields# 
gellymodel_7 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                     wheelbase + carlength + carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + carbodywagon +  
                     enginetypel + enginetypeohc + enginetypeohcf + 
                     enginetypeohcv + enginetyperotor + cylindernumberfour + cylindernumberthree + 
                     fuelsystem2bbl + fuelsystemmpfi + Brand_NameAudi + Brand_NameBmw + 
                     Brand_NameBuick + Brand_NameDodge + Brand_NameMitsubishi + 
                     Brand_NamePlymouth + Brand_NamePorsche + 
                     Brand_NameSaab + symboling2 + symboling3, data = gelly_train)


#Checking the summary of model
summary(gellymodel_7)

# Passing the model into the vif function
vif(gellymodel_7)

#----------------------------------------------------------------#
#--------------------------MODEL-8-------------------------------#

# removing symboling2 variable based on High VIF and insignificance (p>0.05)
# Making a new model without symboling2 variable

#Creating the 8th Model without the above mentioned fields# 
gellymodel_8 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                     wheelbase + carlength + carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + carbodywagon +  
                     enginetypel + enginetypeohc + enginetypeohcf + 
                     enginetypeohcv + enginetyperotor + cylindernumberfour + cylindernumberthree + 
                     fuelsystem2bbl + fuelsystemmpfi + Brand_NameAudi + Brand_NameBmw + 
                     Brand_NameBuick + Brand_NameDodge + Brand_NameMitsubishi + 
                     Brand_NamePlymouth + Brand_NamePorsche + 
                     Brand_NameSaab + symboling3, data = gelly_train)


#Checking the summary of model
summary(gellymodel_8)

# Passing the model into the vif function
vif(gellymodel_8)

#----------------------------------------------------------------#
#--------------------------MODEL-9-------------------------------#

# removing carlength variable based on High VIF and insignificance (p>0.05)
# Making a new model without carlength variable

#Creating the 9th Model without the above mentioned fields# 
gellymodel_9 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                     wheelbase + carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + carbodywagon +  
                     enginetypel + enginetypeohc + enginetypeohcf + 
                     enginetypeohcv + enginetyperotor + cylindernumberfour + cylindernumberthree + 
                     fuelsystem2bbl + fuelsystemmpfi + Brand_NameAudi + Brand_NameBmw + 
                     Brand_NameBuick + Brand_NameDodge + Brand_NameMitsubishi + 
                     Brand_NamePlymouth + Brand_NamePorsche + 
                     Brand_NameSaab + symboling3, data = gelly_train)


#Checking the summary of model
summary(gellymodel_9)

# Passing the model into the vif function
vif(gellymodel_9)

#----------------------------------------------------------------#
#--------------------------MODEL-10-------------------------------#

# removing wheelbase variable based on High VIF and insignificance (p>0.05)
# Making a new model without wheelbase variable

#Creating the 10th Model without the above mentioned fields# 
gellymodel_10 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                     carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + carbodywagon +  
                     enginetypel + enginetypeohc + enginetypeohcf + 
                     enginetypeohcv + enginetyperotor + cylindernumberfour + cylindernumberthree + 
                     fuelsystem2bbl + fuelsystemmpfi + Brand_NameAudi + Brand_NameBmw + 
                     Brand_NameBuick + Brand_NameDodge + Brand_NameMitsubishi + 
                     Brand_NamePlymouth + Brand_NamePorsche + 
                     Brand_NameSaab + symboling3, data = gelly_train)


#Checking the summary of model
summary(gellymodel_10)

# Passing the model into the vif function
vif(gellymodel_10)

#----------------------------------------------------------------#
#--------------------------MODEL-11-------------------------------#

# removing enginetypel variable based on High VIF and insignificance (p>0.05)
# Making a new model without enginetypel variable

#Creating the 11th Model without the above mentioned fields# 
gellymodel_11 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                      carwidth + carheight + curbweight + 
                      enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                      carbodyhatchback + carbodysedan + carbodywagon +  
                      enginetypeohc + enginetypeohcf + 
                      enginetypeohcv + enginetyperotor + cylindernumberfour + cylindernumberthree + 
                      fuelsystem2bbl + fuelsystemmpfi + Brand_NameAudi + Brand_NameBmw + 
                      Brand_NameBuick + Brand_NameDodge + Brand_NameMitsubishi + 
                      Brand_NamePlymouth + Brand_NamePorsche + 
                      Brand_NameSaab + symboling3, data = gelly_train)


#Checking the summary of model
summary(gellymodel_11)

# Passing the model into the vif function
vif(gellymodel_11)

#----------------------------------------------------------------#
#--------------------------MODEL-12-------------------------------#

# removing carheight variable based on High VIF and insignificance (p>0.05)
# Making a new model without carheight variable

#Creating the 12th Model without the above mentioned fields# 
gellymodel_12 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                      carwidth + curbweight + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                      carbodywagon + enginetypeohc + enginetypeohcf +  
                      enginetypeohcv + enginetyperotor + cylindernumberfour +  
                      cylindernumberthree + fuelsystem2bbl + fuelsystemmpfi + 
                      Brand_NameAudi + Brand_NameBmw + Brand_NameBuick + Brand_NameDodge + 
                      Brand_NameMitsubishi + Brand_NamePlymouth + Brand_NamePorsche +
                      Brand_NameSaab + symboling3, data = gelly_train)


#Checking the summary of model
summary(gellymodel_12)

# Passing the model into the vif function
vif(gellymodel_12)

#----------------------------------------------------------------#
#--------------------------MODEL-13-------------------------------#

# removing symboling3 variable based on High VIF and insignificance (p>0.05)
# Making a new model without symboling3 variable

#Creating the 13th Model without the above mentioned fields# 
gellymodel_13 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                      carwidth + curbweight + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                      carbodywagon + enginetypeohc + enginetypeohcf +  
                      enginetypeohcv + enginetyperotor + cylindernumberfour +  
                      cylindernumberthree + fuelsystem2bbl + fuelsystemmpfi + 
                      Brand_NameAudi + Brand_NameBmw + Brand_NameBuick + Brand_NameDodge + 
                      Brand_NameMitsubishi + Brand_NamePlymouth + Brand_NamePorsche +
                      Brand_NameSaab, data = gelly_train)


#Checking the summary of model
summary(gellymodel_13)

# Passing the model into the vif function
vif(gellymodel_13)

#----------------------------------------------------------------#
#--------------------------MODEL-14-------------------------------#

# removing Brand_NameAudi variable based on High VIF and insignificance (p>0.05)
# Making a new model without Brand_NameAudi variable

#Creating the 14th Model without the above mentioned fields# 
gellymodel_14 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                      carwidth + curbweight + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                      carbodywagon + enginetypeohc + enginetypeohcf +  
                      enginetypeohcv + enginetyperotor + cylindernumberfour +  
                      cylindernumberthree + fuelsystem2bbl + fuelsystemmpfi + 
                      Brand_NameBmw + Brand_NameBuick + Brand_NameDodge + 
                      Brand_NameMitsubishi + Brand_NamePlymouth + Brand_NamePorsche +
                      Brand_NameSaab, data = gelly_train)


#Checking the summary of model
summary(gellymodel_14)

# Passing the model into the vif function
vif(gellymodel_14)

#----------------------------------------------------------------#
#--------------------------MODEL-15-------------------------------#

# removing Brand_NameSaab variable based on High VIF and insignificance (p>0.05)
# Making a new model without Brand_NameSaab variable

#Creating the 15th Model without the above mentioned fields# 
gellymodel_15 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                      carwidth + curbweight + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                      carbodywagon + enginetypeohc + enginetypeohcf +  
                      enginetypeohcv + enginetyperotor + cylindernumberfour +  
                      cylindernumberthree + fuelsystem2bbl + fuelsystemmpfi + 
                      Brand_NameBmw + Brand_NameBuick + Brand_NameDodge + 
                      Brand_NameMitsubishi + Brand_NamePlymouth + 
                      Brand_NamePorsche, data = gelly_train)


#Checking the summary of model
summary(gellymodel_15)

# Passing the model into the vif function
vif(gellymodel_15)

#-----------------------------------------------------------------#
#--------------------------MODEL-16-------------------------------#

# removing fueltype variable based on High VIF and insignificance (p>0.05)
# Making a new model without fueltype variable

#Creating the 16th Model without the above mentioned fields# 
gellymodel_16 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + curbweight + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                      carbodywagon + enginetypeohc + enginetypeohcf +  
                      enginetypeohcv + enginetyperotor + cylindernumberfour +  
                      cylindernumberthree + fuelsystem2bbl + fuelsystemmpfi + 
                      Brand_NameBmw + Brand_NameBuick + Brand_NameDodge + 
                      Brand_NameMitsubishi + Brand_NamePlymouth + 
                      Brand_NamePorsche, data = gelly_train)


#Checking the summary of model
summary(gellymodel_16)

# Passing the model into the vif function
vif(gellymodel_16)

#-----------------------------------------------------------------#
#--------------------------MODEL-17-------------------------------#

# removing fuelsystemmpfi variable based on High VIF and insignificance (p>0.05)
# Making a new model without fuelsystemmpfi variable

#Creating the 17th Model without the above mentioned fields# 
gellymodel_17 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + curbweight + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                      carbodywagon + enginetypeohc + enginetypeohcf +  
                      enginetypeohcv + enginetyperotor + cylindernumberfour +  
                      cylindernumberthree + fuelsystem2bbl + 
                      Brand_NameBmw + Brand_NameBuick + Brand_NameDodge + 
                      Brand_NameMitsubishi + Brand_NamePlymouth + 
                      Brand_NamePorsche, data = gelly_train)


#Checking the summary of model
summary(gellymodel_17)

# Passing the model into the vif function
vif(gellymodel_17)

#-----------------------------------------------------------------#
#--------------------------MODEL-18-------------------------------#

# removing enginetypeohcf variable based on High VIF and insignificance (p>0.05)
# Making a new model without enginetypeohcf variable

#Creating the 18th Model without the above mentioned fields# 
gellymodel_18 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + curbweight + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                      carbodywagon + enginetypeohc + enginetypeohcv + enginetyperotor +   
                      cylindernumberfour + cylindernumberthree + fuelsystem2bbl +  
                      Brand_NameBmw + Brand_NameBuick + Brand_NameDodge + 
                      Brand_NameMitsubishi + Brand_NamePlymouth + 
                      Brand_NamePorsche, data = gelly_train)


#Checking the summary of model
summary(gellymodel_18)

# Passing the model into the vif function
vif(gellymodel_18)

#-----------------------------------------------------------------#
#--------------------------MODEL-19-------------------------------#

# removing fuelsystem2bbl variable based on High VIF and insignificance (p>0.05)
# Making a new model without fuelsystem2bbl variable

#Creating the 19th Model without the above mentioned fields# 
gellymodel_19 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + curbweight + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                      carbodywagon + enginetypeohc + enginetypeohcv + enginetyperotor +   
                      cylindernumberfour + cylindernumberthree +   
                      Brand_NameBmw + Brand_NameBuick + Brand_NameDodge + 
                      Brand_NameMitsubishi + Brand_NamePlymouth + 
                      Brand_NamePorsche, data = gelly_train)


#Checking the summary of model
summary(gellymodel_19)

# Passing the model into the vif function
vif(gellymodel_19)

#-----------------------------------------------------------------#
#--------------------------MODEL-20-------------------------------#

# removing Brand_NameDodge variable based on High VIF and insignificance (p>0.05)
# Making a new model without Brand_NameDodge variable

#Creating the 20th Model without the above mentioned fields# 
gellymodel_20 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + curbweight + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                      carbodywagon + enginetypeohc + enginetypeohcv + enginetyperotor +   
                      cylindernumberfour + cylindernumberthree +   
                      Brand_NameBmw + Brand_NameBuick +  
                      Brand_NameMitsubishi + Brand_NamePlymouth + 
                      Brand_NamePorsche, data = gelly_train)


#Checking the summary of model
summary(gellymodel_20)

# Passing the model into the vif function
vif(gellymodel_20)

#-----------------------------------------------------------------#
#--------------------------MODEL-21-------------------------------#

# removing Brand_NamePlymouth variable based on High VIF and insignificance (p>0.05)
# Making a new model without Brand_NamePlymouth variable

#Creating the 21st Model without the above mentioned fields# 
gellymodel_21 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + curbweight + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                      carbodywagon + enginetypeohc + enginetypeohcv + enginetyperotor +   
                      cylindernumberfour + cylindernumberthree +   
                      Brand_NameBmw + Brand_NameBuick +  
                      Brand_NameMitsubishi + Brand_NamePorsche, data = gelly_train)
                      


#Checking the summary of model
summary(gellymodel_21)

# Passing the model into the vif function
vif(gellymodel_21)

#-----------------------------------------------------------------#
#--------------------------MODEL-22-------------------------------#

# removing cylindernumberfour variable based on High VIF and insignificance (p>0.05)
# Making a new model without cylindernumberfour variable

#Creating the 22nd Model without the above mentioned fields# 
gellymodel_22 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + curbweight + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                      carbodywagon + enginetypeohc + enginetypeohcv + enginetyperotor +   
                      cylindernumberthree + Brand_NameBmw + Brand_NameBuick +  
                      Brand_NameMitsubishi + Brand_NamePorsche, data = gelly_train)

#Checking the summary of model
summary(gellymodel_22)

# Passing the model into the vif function
vif(gellymodel_22)

#-----------------------------------------------------------------#
#--------------------------MODEL-23-------------------------------#

# removing enginetypeohcv variable based on High VIF and insignificance (p>0.05)
# Making a new model without enginetypeohcv variable

#Creating the 23rd Model without the above mentioned fields# 
gellymodel_23 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + curbweight + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                      carbodywagon + enginetypeohc + enginetyperotor +   
                      cylindernumberthree + Brand_NameBmw + Brand_NameBuick +  
                      Brand_NameMitsubishi + Brand_NamePorsche, data = gelly_train)

#Checking the summary of model
summary(gellymodel_23)

# Passing the model into the vif function
vif(gellymodel_23)

#-----------------------------------------------------------------#
#--------------------------MODEL-24-------------------------------#

# removing curbweight variable based on High VIF and insignificance (p>0.05)
# Making a new model without curbweight variable

#Creating the 24th Model without the above mentioned fields# 
gellymodel_24 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                      carbodywagon + enginetypeohc + enginetyperotor +   
                      cylindernumberthree + Brand_NameBmw + Brand_NameBuick +  
                      Brand_NameMitsubishi + Brand_NamePorsche, data = gelly_train)

#Checking the summary of model
summary(gellymodel_24)

# Passing the model into the vif function
vif(gellymodel_24)

#-----------------------------------------------------------------#
#--------------------------MODEL-25-------------------------------#

# removing carbodywagon variable based on High VIF and insignificance (p>0.05)
# Making a new model without carbodywagon variable

#Creating the 25th Model without the above mentioned fields# 
gellymodel_25 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                      enginetypeohc + enginetyperotor +   
                      cylindernumberthree + Brand_NameBmw + Brand_NameBuick +  
                      Brand_NameMitsubishi + Brand_NamePorsche, data = gelly_train)

#Checking the summary of model
summary(gellymodel_25)

# Passing the model into the vif function
vif(gellymodel_25)

#-----------------------------------------------------------------#
#--------------------------MODEL-26-------------------------------#

# removing carbodysedan variable based on High VIF and insignificance (p>0.05)
# Making a new model without carbodysedan variable

#Creating the 26th Model without the above mentioned fields# 
gellymodel_26 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhardtop + carbodyhatchback + 
                      enginetypeohc + enginetyperotor +   
                      cylindernumberthree + Brand_NameBmw + Brand_NameBuick +  
                      Brand_NameMitsubishi + Brand_NamePorsche, data = gelly_train)

#Checking the summary of model
summary(gellymodel_26)

# Passing the model into the vif function
vif(gellymodel_26)

#-----------------------------------------------------------------#
#--------------------------MODEL-27-------------------------------#

# removing carbodyhardtop variable based on High VIF and insignificance (p>0.05)
# Making a new model without carbodyhardtop variable

#Creating the 27th Model without the above mentioned fields# 
gellymodel_27 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhatchback + 
                      enginetypeohc + enginetyperotor +   
                      cylindernumberthree + Brand_NameBmw + Brand_NameBuick +  
                      Brand_NameMitsubishi + Brand_NamePorsche, data = gelly_train)

#Checking the summary of model
summary(gellymodel_27)

# Passing the model into the vif function
vif(gellymodel_27)

#-----------------------------------------------------------------#
#--------------------------MODEL-28-------------------------------#

# removing Brand_NameMitsubishi variable based on High VIF and insignificance (p>0.05)
# Making a new model without Brand_NameMitsubishi variable

#Creating the 28th Model without the above mentioned fields# 
gellymodel_28 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhatchback + 
                      enginetypeohc + enginetyperotor +   
                      cylindernumberthree + Brand_NameBmw + Brand_NameBuick +  
                      Brand_NamePorsche, data = gelly_train)

#Checking the summary of model
summary(gellymodel_28)

# Passing the model into the vif function
vif(gellymodel_28)

#-----------------------------------------------------------------#
#--------------------------MODEL-29-------------------------------#

# removing Brand_NamePorsche variable based on High VIF and insignificance (p>0.05)
# Making a new model without Brand_NamePorsche variable

#Creating the 29th Model without the above mentioned fields# 
gellymodel_29 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + enginesize + boreratio + stroke + 
                      peakrpm + carbodyhatchback + 
                      enginetypeohc + enginetyperotor + cylindernumberthree +   
                      Brand_NameBmw + Brand_NameBuick, data = gelly_train) 
                      

#Checking the summary of model
summary(gellymodel_29)

# Passing the model into the vif function
vif(gellymodel_29)

#-----------------------------------------------------------------#
#--------------------------MODEL-30-------------------------------#

# removing carbodyhatchback variable based on High VIF and insignificance (p>0.05)
# Making a new model without carbodyhatchback variable

#Creating the 30th Model without the above mentioned fields# 
gellymodel_30 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + enginesize + boreratio + stroke + 
                      peakrpm + enginetypeohc + enginetyperotor + cylindernumberthree +
                      Brand_NameBmw + Brand_NameBuick, data = gelly_train) 

#Checking the summary of model
summary(gellymodel_30)

# Passing the model into the vif function
vif(gellymodel_30)

#-----------------------------------------------------------------#
#--------------------------MODEL-31-------------------------------#

# removing boreratio variable based on High VIF and insignificance (p>0.05)
# Making a new model without boreratio variable

#Creating the 31st Model without the above mentioned fields# 
gellymodel_31 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + enginesize + stroke + 
                      peakrpm + enginetypeohc + enginetyperotor + cylindernumberthree +
                      Brand_NameBmw + Brand_NameBuick, data = gelly_train) 

#Checking the summary of model
summary(gellymodel_31)

# Passing the model into the vif function
vif(gellymodel_31)

#-----------------------------------------------------------------#
#--------------------------MODEL-32-------------------------------#

# removing enginetypeohc variable based on High VIF and insignificance (p>0.05)
# Making a new model without enginetypeohc variable

#Creating the 32nd Model without the above mentioned fields# 
gellymodel_32 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + enginesize + stroke + 
                      peakrpm + enginetyperotor + cylindernumberthree +
                      Brand_NameBmw + Brand_NameBuick, data = gelly_train) 

#Checking the summary of model
summary(gellymodel_32)

# Passing the model into the vif function
vif(gellymodel_32)

#-----------------------------------------------------------------#
#--------------------------MODEL-33-------------------------------#

# removing cylindernumberthree variable based on High VIF and insignificance (p>0.05)
# Making a new model without cylindernumberthree variable

#Creating the 33rd Model without the above mentioned fields# 
gellymodel_33 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + enginesize + stroke + 
                      peakrpm + enginetyperotor + 
                      Brand_NameBmw + Brand_NameBuick, data = gelly_train) 

#Checking the summary of model
summary(gellymodel_33)

# Passing the model into the vif function
vif(gellymodel_33)

#-----------------------------------------------------------------#
#--------------------------MODEL-34-------------------------------#

# removing Brand_NameBmw, Brand_NameBuick variable as they were making the model more rigid
# Making a new model without Brand_NameBmw, Brand_NameBuick variable

#Creating the 34th Model without the above mentioned fields# 
gellymodel_34 <- lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + enginesize + stroke + 
                      peakrpm + enginetyperotor, data = gelly_train)
                      

#Checking the summary of model
summary(gellymodel_34)

# Passing the model into the vif function
vif(gellymodel_34)

#-----------------------------------------------------------------#
#--------------------------MODEL-35-------------------------------#

# removing aspiration variable based on High VIF
# Making a new model without aspiration variable

#Creating the 35th Model without the above mentioned fields# 
gellymodel_35 <- lm(formula = price ~ enginelocation + 
                      carwidth + enginesize + stroke + 
                      peakrpm + enginetyperotor, data = gelly_train)


#Checking the summary of model
summary(gellymodel_35)

# Passing the model into the vif function
vif(gellymodel_35)

#-----------------------------------------------------------------#
#The final training model will consist of enginelocation, carwidth,
#enginesize, stroke, peakrpm, enginetyperotor only these 6 variables
#with an adjusted r square of 0.8606 and r square of 0.8665
#-----------------------------------------------------------------#

#-----------------------------------------------------------------#
# Predict the car prices in the testing dataset#
#-----------------------------------------------------------------#

gellyPredict <- predict(gellymodel_35,gelly_test[,-18])
gelly_test$predicted_price <- gellyPredict

#-----------------------------------------------------------------#

#calculating the correlation to find the accuracy of predicted values
r_value <- cor(gelly_test$price,gelly_test$predicted_price)

#we see that the r value is 0.93822, now we will calculate the r-square
rsquared_value <- cor(gelly_test$price,gelly_test$predicted_price)^2
#we see that the r squared value comes to 0.88026 which when
#compared to the training model_35's r square seems to be pretty close

#---------------------------CONCLUSION----------------------------#

#Hence we can conclude that the r square for both training set and
#testing data seems to be pretty close, which tells us that this model
#is good to go. The final variables that were the most significant while
#predicting the price in which gelly auto should launch its product were
#enginelocation, carwidth, enginesize, stroke, peakrpm and enginetyperotor.

#-----------------------END OF CODE-------------------------------#


  



