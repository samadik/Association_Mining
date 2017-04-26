library(psych)
library(RWeka)
library(lattice)
library(ggplot2)
library(arules)
library(tidyr)
library(plyr)
library(reshape2)
library(stringr)

if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}

getwd()
q1parts <- read.csv(file = "2017Q1Parts.csv")
str(q1parts)
head(q1parts)
tail(q1parts)


#Data Cleaning
df_itemList <- ddply(q1parts,
                     .(InvoiceDate,OrderNumber),
                     summarize,
                     Parts = paste0(PartNumber,collapse = ","))

df_itemList

df_itemList$InvoiceDate <- NULL
df_itemList$OrderNumber <- NULL

#colnames(df_itemList) <- c("itemList")


write.csv(df_itemList,"Q1ItemList.csv", row.names = TRUE)


#Data Mining
partSOBasket <- read.transactions("Q1ItemList.csv",format="basket")


partSOBasket@itemInfo$labels <- gsub(" ","",partSOBasket@itemInfo$labels)
partSOBasket@itemInfo$labels <- gsub("\"","",partSOBasket@itemInfo$labels)

inspect(partSOBasket[1:5])
summary(partSOBasket)

itemFrequency(partSOBasket[, 1:5])

itemFrequencyPlot((partSOBasket[, 1:2]),support = 0.02)


#Applying algorithms to the dataset
#Using Apriori
itemRuleSet1 <- apriori(data=partSOBasket,
                       parameter = list(support = 0.000884565432107369, 
                                        confidence = 0.9,
                                        minlen=10))

summary(itemRuleSet1)
inspect(itemRuleSet1[1:100])

output1 <- capture.output(inspect(itemRuleSet1[1:100]))
gsub("[^{]+\\{([^}]*)\\}[^{]+\\{([^}]*)\\}.*", "\\2", output1)[-1]

write.csv(output1,file = "Market_Basket_Analysis1")

head(inspect(sort(itemRuleSet1, by="lift")))
head(inspect(sort(itemRuleSet1, by="confidence")))
head(inspect(sort(itemRuleSet1, by="support")))

#Using Eclat
itemRuleSet2 <- eclat(partSOBasket,parameter = list(supp = 0.0009, maxlen = 4))

summary(itemRuleSet2)
inspect(itemRuleSet2)

output2 <- capture.output(inspect(itemRuleSet2))
write.csv(output2,file = "Market_Basket_Analysis2")


