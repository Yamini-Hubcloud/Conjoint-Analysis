# Install and load required packages

## Install Packages (if needed)
install.packages("conjoint")

## Load Packages and Set Seed
library(conjoint)
set.seed(1)

# Set seed for reproducibility
set.seed(1)


# Set up attributes and levels as a list
attrib.level <- list(
  Price = c("23000", "25000", "27000", "29000"),
  Brand = c("Kia", "Saturn", "Toyota", "Volkswagen"),
  Horsepower = c("220", "250", "280"),
  Color = c("Green", "Blue", "Red"),
  Sunroof = c("Yes", "No")
)

# Create the fractional factorial design
experiment <- expand.grid(attrib.level)
design <- caFactorialDesign(data=experiment, type="fractional", cards=25, seed=1)

# Check for correlation in fractional factorial design
cor_matrix <- cor(caEncodedDesign(design))
print(cor_matrix)

# Export design for survey (not needed as we already have the profiles)
 write.csv(design, "C:/Users/Admin/Downloads/conjoint_profiles.csv", row.names = FALSE)

# Read the data from Excel files
conjoint_preferences <- read_excel("C:/Users/Admin/Downloads/Conjoint Preferences-A2.xlsx")


# Prepare preference data
pref<- conjoint_preferences[, -1]  # Remove the first column (Product Profile)

# Set up attributes and levels as a vector
attrib.vector <- data.frame(unlist(attrib.level, use.names=FALSE))
colnames(attrib.vector) <- c("levels")

# Estimate the part-worths for each respondent
part.worths <- NULL
for (i in 1:ncol(pref)) {
  temp <- caPartUtilities(pref[,i], design, attrib.vector)
  
  # Pick the baseline case
  Base_Price <- temp[,"23000"]
  Base_Brand <- temp[,"Kia"]
  Base_Horsepower <- temp[,"220"]
  Base_Color <- temp[,"Green"]
  Base_Sunroof <- temp[,"Yes"]
  
  # Adjust Intercept
  temp[,"intercept"] <- temp[,"intercept"] - Base_Price - Base_Brand - Base_Horsepower - 
    Base_Color - Base_Sunroof
  
  # Adjust Coefficients
  # Price
  L1 <- length(attrib.level$Price) + 1
  for (j in 2:L1) {temp[,j] <- temp[,j] - Base_Price}
  
  # Brand
  L2 <- length(attrib.level$Brand) + L1
  for (k in (L1+1):L2) {temp[,k] <- temp[,k] - Base_Brand}
  
  # Horsepower
  L3 <- length(attrib.level$Horsepower) + L2
  for (l in (L2+1):L3) {temp[,l] <- temp[,l] - Base_Horsepower}
  
  # Color
  L4 <- length(attrib.level$Color) + L3
  for (m in (L3+1):L4) {temp[,m] <- temp[,m] - Base_Color}
  
  # Sunroof
  L5 <- length(attrib.level$Sunroof) + L4
  for (n in (L4+1):L5) {temp[,n] <- temp[,n] - Base_Sunroof}
  
  part.worths <- rbind(part.worths, temp)
}
rownames(part.worths) <- colnames(pref)

# Export part-worths from analysis
write.csv(part.worths, "conjoint_partworths.csv", row.names = TRUE)

# Print summary of part-worths
print(summary(part.worths))
View(part.worths)



















