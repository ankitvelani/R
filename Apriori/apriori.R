# Load the libraries
library(arules)
library(arulesViz)
library(datasets)

# Load the data set
data(Groceries)

# Create an item frequency plot for the top 20 items
itemFrequencyPlot(Groceries,topN=20,type="absolute")

# Generate Frequent Itemset
# Apriori algorithm except minimum supports and confidence
# Setting Minimum Support 0.01  ( 1% )


itemsets <- apriori(Groceries,parameter = list(minlen=1,support=0.01,target="frequent itemsets"))

# Summary of Frequent itemsets
summary(itemsets)
# 333 items sets 
# listed most frequent item 
# itemset iteration wise : 1st iteration 88 ,2nd iteration 213 and 3rd iteration 32
# Then we have descriptive summary for support and it range 0.010 to 0.256

inspect(itemsets)


# Generate Frequent Itemset
# Setting Minimum Supports 0.01 (1%)
# Setting Minimum Confidance 0.5 ( 50% )
rules <- apriori(Groceries,parameter = list(minlen=1,support=0.01,confidence=0.5,target="rules"))
options(digits=2) # setting limits for decimal number , it's available in R base package 

inspect(rules)
# List the available rules for the transcation and which satisfy the given critearea.

#lhs                                      rhs                support    confidence lift    
#[1]  {curd,yogurt}                         => {whole milk}       0.01006609 0.5823529  2.279125
#[2]  {other vegetables,butter}             => {whole milk}       0.01148958 0.5736041  2.244885
#[3]  {other vegetables,domestic eggs}      => {whole milk}       0.01230300 0.5525114  2.162336

#When Other Vegetable and Butter is purchased , 1.1% of the times Whole Milk is also purchased with 57% of confidance level 
  
# Summary of rules
# There are 15 rules for this given critearea
# Rule length distribution ( Number of Items in one pattern ) LHS + RHS 
# Size indicate the number of rules that satisfied given critearea
# We have descriptive summary for the Terminology ( Support , Confidence , Lift )
# At the end we have Mining information , given criteria for the rules  , Total number of transactions , given support and confidence
summary(rules)


# Sorting out the Rules by confidence 
rules <- sort(rules,by="confidence",decreasing = T)
inspect(rules)


# Redundancies
# Sometimes Rules will repeat  , we need to drop the repeated rules.

subset.matrix <- is.subset(rules,rules)
subset.matrix[lower.tri(subset.matrix,diag = T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1

rules.pruned <- rules[!redundant]
rules<-rules.pruned


# Visualization
library(arulesViz)
plot(rules) # Visulizing the terminolgy by Scatter plot
plot(rules@quality) # Checking cor-relation between terminology

plot(rules,method="matrix",measure = c("lift","confidence"))
plot(rules,method="graph")


# Targeting Items
# Question : What are customers likely to buy before buying whole milk
rules <- apriori(data=Groceries,parameter = list(support=0.01,conf=0.5),
                 appearance = list(default="lhs",rhs="whole milk"),
                 control=list(verbose=F)
                 )
inspect(rules)

# Questions : What are customers likely to buy if they purchase whole milk
rules <- apriori(data=Groceries,parameter = list(support=0.01,conf=0.1),
                 appearance = list(default="rhs",rhs="whole milk"),
                 control=list(verbose=F)
)
inspect(rules)
