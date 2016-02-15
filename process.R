
#CLEANING OF DATA:

#read data into data frame:
DF<- read.csv("data.csv", sep=",")

complete.cases(DF) #check for any incomplete data rows

#loop to make all columns logical:
for (i in 2:51)
{
  DF[,i] = as.logical(DF[,i])
}

#Test and see:
DF[5,25]
head(DF)

#drop the first column holding the ids:
DF$id<- NULL

#Test:
head(DF)

#Transform the dataframe to a sparse matrix of transactional data:
Raw <- as(DF,"transactions")

#ANALYSIS:

#1. Exploratory:

summary(Raw)

itemFrequencyPlot(Raw, type="absolute", topN=45)
#See the items with the highest frequency

itemFrequencyPlot(Raw, type="absolute", topN=15)

#inspect the sparse matrix:
inspect(Raw[1:3])

#frequency of the first six items in alphabetical order:
itemFrequency(Raw[,1:6])

#items that are present in over 20% transactions:
itemFrequencyPlot(Raw, support= 0.20)
itemFrequencyPlot(Raw, topN=5, type="relative")
itemFrequencyPlot(Raw, topN=15, type="relative")

#DATA MODELING:
rules_2 <- apriori(Raw,parameter = list(supp=0.002, conf=0.90, minlen=2))

rules <- apriori(Raw) #taking default values for support and confidence

summary(rules)

inspect(sort(rules, by = "lift")[1:5])

#develop a data model graph:
rules_1 <- apriori(Raw,parameter = list(supp=0.1, conf=0.70, minlen=2))

rules_1 <- apriori(Raw,parameter = list(supp=0.1, conf=0.70, minlen=3), appearance = list(default="lhs", rhs=c("item_35")), control = list(verbose=F))

plot(rules_1, method="graph", shading="confidence")   #plot a graph to visualize the association rules

rules_2 <- apriori(Raw,parameter = list(supp=0.002, conf=0.90, minlen=2)) #observe rules for lower selling items in relation to higher selling ones
inspect(rules_2[170:180])
inspect(rules_2[100:120])   #inspect rules to find items whose sales can be boosted by tying offers with high ticket items.


