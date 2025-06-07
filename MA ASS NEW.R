install.packages("readxl")
library(readxl)
CC<- read_excel("C:/Users/Admin/Desktop/MSc BA/7106 MA/Current Customers.xlsx")
View(CC)

install.packages(c("tidyverse", "lubridate", "dplyr", "cluster","readr", "factoextra","MASS","ggplot2","dataexplorer"))
library(tidyverse)
library(lubridate)
library(dplyr)
library(cluster)    # For clustering algorithms
library(factoextra) # For visualizing clusters
library(fbc)
library(clustersim)

# Display the first few rows
head(CC)

# Check data structure and data types
str(CC)

# Summary statistics
summary(CC)

# Change CustomerID to numeric
library(dplyr)
# Handle missing CustomerID values
CC <- CC%>%
  filter(!is.na(CustomerID))
CC <- na.omit(CC)
CC$CustomerID[is.na(as.numeric(CC$CustomerID))]

CC$CustomerID <- as.numeric(CC$CustomerID)


# Convert InvoiceDate to datetime format
CC$InvoiceDate <- as.character(CC$InvoiceDate)

library(lubridate)

# Convert string to proper date-time format
CC$InvoiceDate <- ymd_hm(CC$InvoiceDate)

library(dplyr)
# Change format (adjust as needed)
CC$InvoiceDate <- as.Date(CC$InvoiceDate, format="%d/%m/%Y")

CC$InvoiceDate <- as.numeric(CC$InvoiceDate)
#Calculate Revenue for each transaction
CC$revenue <- CC$Quantity * CC$UnitPrice

# Calculate the number of distinct orders per customer (num_orders)
CC <- CC %>%
  group_by(CustomerID) %>%
  mutate(num_orders = n_distinct(InvoiceNo)) %>%
  ungroup()
CC$num_orders <- as.numeric(CC$num_orders)

# Missing plot
install.packages("DataExplorer")
library(DataExplorer)
plot_missing(CC)

# Get today's date
today <- Sys.Date()

# Calculate Recency for each customer
CC$recency_days <- as.numeric(difftime(today, CC$InvoiceDate, units = "days"))

# Find the most recent purchase date for each customer
recency <- aggregate(recency_days ~ CustomerID, data = CC, FUN = min)

# Calculate Recency, Frequency, and Monetary Value
library(dplyr)
RFM <- CC %>%
  group_by(CustomerID) %>%
  summarize(
    Recency = as.numeric(difftime(Sys.Date(), max(InvoiceDate), units = "days")),  # Days since last purchase
    Frequency = n_distinct(num_orders),  # Unique purchases (based on InvoiceNo)
    Monetary = sum(Quantity * UnitPrice)  # Total spending
  ) %>%
  ungroup()


CC <- left_join(CC, RFM, by = "CustomerID")
View(CC)


# Load Packages and Set Seed
set.seed(40459080)

# Run hierarchical clustering with bases variable

seg_hclust <- hclust(dist(scale(cbind(CC$InvoiceNo,CC$Quantity,CC$UnitPrice,CC$CustomerID,
                                      CC$married,CC$household_size,CC$income,CC$age,CC$Zip_Code,CC$Work,CC$Education,CC$num_orders,CC$revenue))), method="complete")


# Elbow plot for first 10 segments
x <- c(1:10)
sort_height <- sort(seg_hclust$height,decreasing=TRUE)
y <- sort_height[1:10]
plot(x,y); lines(x,y,col="pink")

plot(seg_hclust, main = "Hierarchical Clustering Dendrogram", xlab = "Observations", ylab = "Height")
abline(h = 100, col = "red") 
# Run k-means with 6 segments
seg_kmeans <- kmeans(x = data.frame(CC$InvoiceNo,CC$Quantity,CC$UnitPrice,CC$CustomerID,
                                    CC$married,CC$household_size,CC$income,CC$age,CC$Zip_Code,CC$Work,CC$Education,CC$num_orders,CC$revenue), 6)

# Add segment number back to original data
segment = seg_kmeans$cluster
seg_result <- cbind(CC, segment)
View(seg_result)

library(readr)
# Export data to a CSV file

write.csv(seg_result, file = "C:\\Users\\Admin\\Documents\\seg_result.csv", row.names = FALSE)

#visualize segment wise 

library(ggplot2)
ggplot(seg_result, aes(x = segment, fill = segment)) + 
  geom_bar() +
  theme_minimal() +
  labs(title = "Customer Distribution Across Segments")

library(dplyr)
seg_result %>%
  group_by(segment) %>%
  summarise(
    Avg_Recency = mean(Recency, na.rm = TRUE),
    Avg_Orders = mean(num_orders, na.rm = TRUE),
    Avg_Revenue = mean(revenue, na.rm = TRUE),
    Avg_Age = mean(age, na.rm = TRUE),
    Count = n()
  )
#recency and frequency
ggplot(seg_result, aes(x = Recency, y = num_orders, color = segment)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Recency vs Number of Orders by Segment")

#Revenue Distribution by Segment
seg_result$segment <- as.factor(seg_result$segment) 
ggplot(seg_result, aes(x = revenue, fill = segment)) + 
  geom_density(alpha = 0.6) + 
  theme_minimal() + 
  labs(title = "Revenue Distribution by Segment")

#Orders per segment
library(ggplot2)
ggplot(seg_result, aes(x = segment, y = num_orders, fill = segment)) + 
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Orders per Segment")

install.packages("reshape2")
library(reshape2)
heatmap_data <- seg_result %>%
  group_by(segment) %>%
  summarise(across(c(Recency, num_orders, revenue, age), mean, na.rm = TRUE))

heatmap_melt <- melt(heatmap_data, id.vars = "segment")

ggplot(heatmap_melt, aes(x = variable, y = segment, fill = value)) + 
  geom_tile() +
  scale_fill_gradient(low = "violet", high = "yellow") +
  theme_minimal() +
  labs(title = "Segment-wise Customer Behavior Heatmap")

set.seed(40459080)
## How many levels for each
groups <- 6

## Run RFM Analysis with Independent Sort
library(dplyr)
seg_result$recency_score <- ntile(seg_result$recency_days*-1, groups)
seg_result <- seg_result %>%
  filter(!is.na(num_orders))
seg_result$frequency_score <- ntile(seg_result$num_orders, groups)
seg_result$monetary_score <- ntile(seg_result$revenue, groups)
seg_result$rfm_score <- paste(seg_result$recency_score*100 + seg_result$frequency_score * 10 + seg_result$monetary_score)

## Run RFM Analysis with Sequential Sort
seg_result$recency_score_seq <- ntile(seg_result$recency_days*-1, groups)
r_groups <- NULL; rf_groups <- NULL; temp <- NULL ## Initialize empty matrices
for (r in 1:groups) {
  r_groups[[r]] <- filter(seg_result, seg_result$recency_score_seq == r)
  r_groups[[r]]$frequency_score_seq <- ntile(r_groups[[r]]$num_orders, groups)
  for (m in 1:groups) {
    rf_groups[[m]] <- filter(r_groups[[r]], r_groups[[r]]$frequency_score_seq == m)
    rf_groups[[m]]$monetary_score_seq <- ntile(rf_groups[[m]]$revenue, groups)
    temp <- bind_rows(temp, rf_groups[[m]])
  }	
}
rfm_result <- temp[order(temp$CustomerID),]
rfm_result$rfm_score_seq <- paste(rfm_result$recency_score_seq*100 + rfm_result$frequency_score_seq * 10 + rfm_result$monetary_score_seq)
View(rfm_result)
## Export RFM Results with Independent and Sequential Sort
write.csv(rfm_result, file = "C:\\Users\\Admin\\Desktop\\MSc BA\\7106 MA\\rfm_result.csv", row.names = FALSE)

# Customer profiling
# Customer profiling- Combine all metrics into one dataframe
rfm_result$Customer_profile <- case_when(
  rfm_result$rfm_score_seq<= 100 ~ "Very Low-value Customer",        
  rfm_result$rfm_score_seq <= 200~ "Low-value Customer",     
  rfm_result$rfm_score_seq <= 300 ~ "Medium-value Customer",  
  rfm_result$rfm_score_seq <= 400 ~ "High-value Customer",      
  TRUE ~ "Very High-value Customer"                              
)

library(ggplot2)
ggplot(rfm_result, aes(x = Customer_profile, fill = Customer_profile)) + 
  geom_bar() +
  theme_minimal() +
  labs(title = "Customer Segments Based on RFM Score", y = "Number of Customers")


set.seed(40459080)

## Read in Segment Data and Classification Data 
seg <- read.csv("C:/Users/Admin/Desktop/MSc BA/7106 MA/rfm_result.csv") ## Choose segmentation_result.csv file
library(readxl)
class <- read_excel("C:/Users/Admin/Desktop/MSc BA/7106 MA/Prospect Customers.xlsx") ## Choose retail_classification.csv file

## Run Discriminant Analysis

library(MASS)
fit <- lda(segment ~ married+household_size+income+ age+Zip_Code+Work+Education, data = seg)
fit

## Check which Discriminant Functions are Significant
ldaPred <- predict(fit, seg)
ld <- ldaPred$x
anova(lm(ld[,1] ~ seg$segment))
anova(lm(ld[,2] ~ seg$segment))
anova(lm(ld[,3] ~ seg$segment))
anova(lm(ld[,4] ~ seg$segment))
anova(lm(ld[,5] ~ seg$segment))
anova(lm(ld[,6] ~ seg$segment))

## Check Disciminant Model Fit
pred.seg <- predict(fit)$class
tseg <- table(seg$segment, pred.seg)
tseg # print table
sum(diag(tseg))/nrow(seg) # print percent correct

## Run Classification Using Discriminant Function
class <- as.data.frame(class) 
pred.class <- predict(fit, class)$class
tclass <- table(pred.class)
tclass # print table

###### Fit 1 ###
## Run Discriminant Analysis

library(MASS)
fit1 <- lda(segment ~ married+household_size+income+ age+Zip_Code+Work, data = seg)
fit1

## Check which Discriminant Functions are Significant
ldaPred <- predict(fit1, seg)
ld <- ldaPred$x
anova(lm(ld[,1] ~ seg$segment))
anova(lm(ld[,2] ~ seg$segment))
anova(lm(ld[,3] ~ seg$segment))
anova(lm(ld[,4] ~ seg$segment))
anova(lm(ld[,5] ~ seg$segment))

## Check Disciminant Model Fit
pred.seg <- predict(fit1)$class
tseg <- table(seg$segment, pred.seg)
tseg # print table
sum(diag(tseg))/nrow(seg) # print percent correct

## Run Classification Using Discriminant Function
class <- as.data.frame(class) 
pred.class <- predict(fit1, class)$class
tclass <- table(pred.class)
tclass # print table

###### Fit 2 ###
## Run Discriminant Analysis

library(MASS)
fit2 <- lda(segment ~ married+household_size+income+ age+Zip_Code+Education, data = seg)
fit2

## Check which Discriminant Functions are Significant
ldaPred <- predict(fit2, seg)
ld <- ldaPred$x
anova(lm(ld[,1] ~ seg$segment))
anova(lm(ld[,2] ~ seg$segment))
anova(lm(ld[,3] ~ seg$segment))
anova(lm(ld[,4] ~ seg$segment))
anova(lm(ld[,5] ~ seg$segment))

## Check Disciminant Model Fit
pred.seg <- predict(fit2)$class
tseg <- table(seg$segment, pred.seg)
tseg # print table
sum(diag(tseg))/nrow(seg) # print percent correct

## Run Classification Using Discriminant Function
class <- as.data.frame(class) 
pred.class <- predict(fit2, class)$class
tclass <- table(pred.class)
tclass # print table

###### Fit 3 ###
## Run Discriminant Analysis
library(MASS)
fit3 <- lda(segment ~ married+household_size+income+ age+Zip_Code, data = seg)
fit3

## Check which Discriminant Functions are Significant
ldaPred <- predict(fit3, seg)
ld <- ldaPred$x
anova(lm(ld[,1] ~ seg$segment))
anova(lm(ld[,2] ~ seg$segment))
anova(lm(ld[,3] ~ seg$segment))
anova(lm(ld[,4] ~ seg$segment))
anova(lm(ld[,5] ~ seg$segment))

## Check Disciminant Model Fit
pred.seg <- predict(fit3)$class
tseg <- table(seg$segment, pred.seg)
tseg # print table
sum(diag(tseg))/nrow(seg) # print percent correct

## Run Classification Using Discriminant Function
class <- as.data.frame(class) 
pred.class <- predict(fit3, class)$class
tclass <- table(pred.class)
tclass # print table

###### Fit 4 ###
## Run Discriminant Analysis
library(MASS)
fit4 <- lda(segment ~ married+income+ age+Zip_Code+Work+Education, data = seg)
fit4

## Check which Discriminant Functions are Significant
ldaPred <- predict(fit4, seg)
ld <- ldaPred$x
anova(lm(ld[,1] ~ seg$segment))
anova(lm(ld[,2] ~ seg$segment))
anova(lm(ld[,3] ~ seg$segment))
anova(lm(ld[,4] ~ seg$segment))
anova(lm(ld[,5] ~ seg$segment))

## Check Disciminant Model Fit
pred.seg <- predict(fit4)$class
tseg <- table(seg$segment, pred.seg)
tseg # print table
sum(diag(tseg))/nrow(seg) # print percent correct

## Run Classification Using Discriminant Function
class <- as.data.frame(class) 
pred.class <- predict(fit4, class)$class
tclass <- table(pred.class)
tclass # print table
