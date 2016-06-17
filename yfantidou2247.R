# NEW PROJECT IN R #########################################################

# REQUIREMENTS #############################################################

require(ggplot2) # Package ggplot2 must be installed first

# EXCERCISE 1 ##############################################################

# Set working directory; Needs to be changed accordingly
setwd("C:\\Users\\Sofia\\Desktop\\R")
# Import data
data_raw <- read.csv(file="GroceriesInitial.csv",header=TRUE,sep=",")
# View(data_raw)
# Transforming the data
product_names <- levels(unlist(data_raw[,4:35])) # Identify all unique products
# Identify the products asked inside the product_names list
citrus <- which(product_names == "citrus fruit")
tropical <- which(product_names == "tropical fruit")  
milk <- which(product_names == "whole milk") 
other_vegetables <- which(product_names == "other vegetables") 
rolls_buns <- which(product_names == "rolls/buns")  
chocolate <- which(product_names == "chocolate")  
water <- which(product_names == "bottled water") 
yogurt <- which(product_names == "yogurt")  
sausage <- which(product_names == "sausage") 
root_vegetables <- which(product_names == "root vegetables")  
pastry <- which(product_names == "pastry")  
soda <- which(product_names == "soda") 
cream <- which(product_names == "cream") 
# Define asked products vector
product_vector <- c(citrus,tropical, milk, other_vegetables, rolls_buns, chocolate, water, yogurt, sausage, root_vegetables, pastry, soda, cream)
# Keep only the product names needed
product_names <- product_names[product_vector]

#c(1,2,3) %in% c(3,4,5,8)
# Create the binary table without headers or extra information
products <- as.data.frame(t(apply(data_raw[,4:35],1,  function(x) # For each transcation line of data_raw
  (product_names) %in% as.character(unlist(x))))) 
# %in% returns a vector as long as product_names (left) with TRUE or FALSE 
# depending on the existance of the product in the specified transaction line

names(products) <- product_names # Define the headers
# Create the initial binary table without the qualitative basket values
data_binary <- cbind(data_raw[,1:3],products) # Join the products table with the extra transaction information

# Discretizing the continouous variables 
data_discrete <- data_binary
cut_points <- quantile(data_discrete$basket_value, probs = c(0,0.33, 0.66,1), na.rm = TRUE,names = FALSE)

data_discrete$basket_value_bin <- cut(data_discrete$basket_value,breaks = cut_points,labels=c("Low","Medium","High"),include.lowest = TRUE)
table(data_discrete$basket_value_bin)

#Below, an inline function to make the binary format again, now that we have discretized any continuous variables
binarize <-function(data_columns,extra_columns=NULL){
  
  column_names <- levels(unlist(data_columns))
  cat(column_names)
  blank <- which(column_names == "") # Remove blank columns
  if (length(blank) !=0)
    column_names <- column_names[-c(blank)]
  
  binary_result <- as.data.frame(t(apply(data_columns,1,  function(x) column_names %in% as.character(unlist(x)))))
  names(binary_result) <- column_names
  if (is.null(extra_columns)==FALSE)
    binary_result<- cbind(extra_columns,binary_result)
  return(binary_result)
}
# Converting basket_value_bin to binary format instead of qualitative char data (low, medium, high)
data_discrete <- binarize(as.data.frame(data_discrete$basket_value_bin),data_discrete) 

data_discrete <- data_discrete[,-c(which(colnames(data_discrete)=="basket_value_bin"))] # Remove the non-binary column
View(data_discrete)

# Discretizing continuous variables - METHOD 2 
# # Find the basket value limits for each category (low, medium, high)
# basket_values <- data_binary[,c("basket_value")]
# basket_values_sorted <- sort(basket_values) 
# 
# # There should be approximately 7,536/3 = 2,512 values in each basket value category (total transactions/distinct categories)
# # If the 2,513 etc. value is the same as the previous one, then we classify it to the previous category (here low instead of medium)
# first_limit <- basket_values_sorted[2512]
# second_limit <- basket_values_sorted[5024]
# #Dispay the suggested limits
# cat("Suggested basket value categories' limits; Manual inspection will be performed to finalize the limits.",
#     "\nLow basket value limit (index 2,5125): ", first_limit, "\nMedium basket value limit (index 5,024): ", second_limit) # 2.7, 5.8
# 
# # Finding the new final limits
# cat("Finding the new final limits.\nLow limits:",
#     "\nValues of 2.7 that are before index 2,512: ", length(basket_values_sorted[2485:2512])-1, # 27
#     "\nValues of 2.7 that are after index 2,512: ", length(basket_values_sorted[2512:2573])-1, # 61
#     "\nSubsequently, we classify 2.7 as medium, in order to maintain better balance.",
#     "\nFinal Low basket value limit: ", 2.6,
#     "\nMedium limits:",
#     "\nValues of 5.8 that are before index 5,024: ", length(basket_values_sorted[4960:5024])-1, # 64
#     "\nValues of 5.8 that are after index 5,024: ", length(basket_values_sorted[5024:5032])-1, # 8
#     "\nSubsequently, we classify 5.8 as medium, in order to maintain better balance.",
#     "\nFinal Medium basket value limit: ", 5.8) 
# # Make basket values qualitative
# for(i in 1:length(basket_values)) {
#   x <- basket_values[i]
#   if(x <= 2.6) {
#     x <- "low"
#   } else if(x <= 5.8) {
#     x <- "medium"
#   } else {
#     x <- "high"
#   }
#   data_binary[i,"basket_value"] <- x
# }
# # Final binary data format
# View(data_binary)

# EXCERCISE 2 ##############################################################
require(arules) # Needs to be installed first
require(arulesViz)

# Part a ###################################################################
rules <- apriori(data_discrete[,4:ncol(data_discrete)],parameter = list(minlen=2, maxlen=5, supp=0.01, conf=1))
# rules <- apriori(data_discrete[,4:ncol(data_discrete)],parameter = list(minlen=2, maxlen=5, supp=0.001, conf=1))
# rules <- apriori(data_discrete[,4:ncol(data_discrete)],parameter = list(minlen=2, maxlen=5, supp=0.005, conf=1))
# rules <- apriori(data_discrete[,4:ncol(data_discrete)],parameter = list(minlen=2, maxlen=5, supp=0.08, conf=1))
# rules <- apriori(data_discrete[,4:ncol(data_discrete)],parameter = list(minlen=2, maxlen=5, supp=0.015, conf=1))
# rules <- apriori(data_discrete[,4:ncol(data_discrete)],parameter = list(minlen=2, maxlen=5, supp=0.002, conf=1))
rules.sorted <- sort(rules, by="support")
inspect(rules.sorted)
plot(rules, method="paracoord", control=list(reorder=TRUE))

# Part b ###################################################################
rules <- apriori(data_discrete[,4:16],parameter = list(minlen=2, maxlen=5, supp=0.001, conf=0.5))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)
plot(rules)
# plot(rules, method="grouped")
plot(rules, method="graph", control=list(type="items"))

# Part c ###################################################################
rules <- apriori(data_discrete[,4:ncol(data_discrete)],parameter = list(minlen=2, supp=0.01, conf=1))
rules.sorted <- sort(rules, by="support")
inspect(rules.sorted)
# plot(rules, method="paracoord", control=list(reorder=TRUE))
plot(rules, method="grouped")
plot(rules, method="graph", control=list(type="items"))

# EXCERCISE 3 ##############################################################
require(plyr) # Needs to be installed first

# Part a ###################################################################
data_kmeans <- data_discrete[,2:3]
# K-Means Cluster Analysis
data_kmeans$Cluster <- kmeans(scale(data_kmeans), 5)$cluster # standardize variables before clustering
# get cluster means
means <- ddply(data_kmeans, c("Cluster"), colwise(mean))

# append cluster assignment
data_discrete$Cluster <- as.character(data_kmeans$Cluster)
table(data_kmeans$Cluster)

# Converting Cluster to binary format instead of qualitative data (1,2,3,4,5)
data_discrete <- binarize(as.data.frame(data_discrete$Cluster),data_discrete) 
data_discrete <- data_discrete[,-c(which(colnames(data_discrete)=="Cluster"))] # Remove the non-binary column
View(data_discrete)

# View cluster means in non-normalized format
means
plot(means[,2:3])

# Part b ###################################################################
standard_deviation <- ddply(data_kmeans, c("Cluster"), colwise(sd)) # Get standard deviation
standard_deviation

# EXCERCISE 4 ##############################################################

# Part a ###################################################################
columns <- c(4:16,20:24)
rules <- apriori(data_discrete[,columns], parameter = list(minlen=2, supp=0.01, conf=0.65), appearance = list(rhs=c("1","2","3","4","5"), default="lhs"))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)
plot(rules, method="grouped")
plot(rules, method="paracoord", control=list(reorder=TRUE))
pltoplot(rules, method="graph", control=list(type="items"))

#rules <- apriori(data_discrete[,columns], parameter = list(minlen=2, supp=0.01, conf=0.1), appearance = list(rhs=c("4"), default="lhs"))

# Part b ###################################################################
rules <- apriori(data_discrete[,4:24], parameter = list(minlen=2, supp=0.01, conf=0.65))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)
plot(rules, method="grouped")
plot(rules, method="paracoord", control=list(reorder=TRUE))
plot(rules, method="graph", control=list(type="items"))

# rules <- apriori(data_discrete[,4:24], parameter = list(minlen=2, supp=0.01, conf=1))
# rules.sorted <- sort(rules, by="lift")
# inspect(rules.sorted)
# rules.sorted <- sort(rules, by="support")
# inspect(rules.sorted)


