#LOAD TRANSACTION DATA

library(arules)

tr <- read.transactions("ElectronidexTransactions2017.csv", 
                        format = "basket", 
                        sep=",", skip = 0)
#inspect(tr)
#tr_len<-length(tr)
#tr_sz<-size(tr)
#LIST(tr)

# VISUALIZE DATASET

library(arulesViz)
library(RColorBrewer)

# viz items frequency (top 20)
itemFrequencyPlot(tr, topN = 20, 
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frequency Plot',
                          type = "relative",
                          ylab = "Item Frequency (Relative)")


#obtain top 20 items
itms <- itemFrequency(tr, type = "relative")
head(sort(itms, decreasing = TRUE), n = 20)

itms_sorted<-sort(itms,decreasing=TRUE)
itms_sorted_top20<-itms_sorted[1:20] # these are the top 20 items (vector of freq)

write.csv(itms_sorted_top20, "Top20_items.csv")


#find top 20 items on tr

#temp<-subset(tr, items %in% itms_sorted_top20)


# image of 200 random transactions

image(sample(tr,200))

##### RULES 
tr_rules_apriori <- apriori(tr,parameter = list(supp = 0.01, conf = 0.2))


#inspect first 10 rules
inspect(tr_rules_apriori[1:10])

# sort rules by confidence, to obtain top 10
tr_rules_apriori_sorted<-sort(tr_rules_apriori, by="confidence", decreasing=TRUE)
inspect(tr_rules_apriori_sorted[1:10])

#inspectDT(tr_rules_apriori_sorted)


#identify redundant rules
redun_id<-is.redundant(tr_rules_apriori_sorted)

#for a particular item, determine 
#the # of rules it participates
item_in_rules<-subset(tr_rules_apriori_sorted, 
             items %in% "Acer Desktop")
item_in_rules

# Visualize Rules

plot(tr_rules_apriori_sorted)
#plot(tr_rules_apriori_sorted,engine="plotly")

plot(tr_rules_apriori_sorted[1:10],method="graph")
control=list(type="items")

# top 10 rules by support
tr_rules_apriori_sorted_sup<-sort(tr_rules_apriori, by="support", decreasing=TRUE)
inspect(tr_rules_apriori_sorted_sup[1:10])

plot(tr_rules_apriori_sorted_sup[1:10],method="graph")
control=list(type="items")

# top 10 rules by lift
tr_rules_apriori_sorted_lift<-sort(tr_rules_apriori, by="lift", decreasing=TRUE)
inspect(tr_rules_apriori_sorted_lift[1:10])

plot(tr_rules_apriori_sorted_lift[1:10],method="graph")
control=list(type="items")

