library(readr)
library(plotrix)

setwd("set you respective working directory")
cc <- read.csv("chemicals_in_makeup.csv")

nrow(cc)
ncol(cc)
dim(cc)


# R1: Which is the Most Common Chemical appearing in the makeup?
print("R1: The chemical appearing the most -")                
r1=max(cc$ChemicalName)
print(r1)
View(cc)




# R2:For easy finding, Sorting based on product name and then finding
#specific product name.
r2 <- cc[order(cc$ProductName), ]
R2 <- subset(r2, ProductName == "BlueFX")#search for any product
print("r2: Sorting based on product name -")
print(r2)
print(" R2: Filtered BlueFX entries after sorting:")
print(R2)




# R3: As the Nail art is currently in demand, we have analyzed 
#rows where primary category is nail products
print("R3: Primary categories makeup (temporary)")
r3=subset(cc,PrimaryCategory == "Nail Products")
print(r3)




# R4: What are the top 5 frequently use products
print("R5: Top 5 Most Frequently Reported Products")
r5 <- sort(table(cc$ProductName), decreasing = TRUE)
print(head(r5, 5))




# R5: Finding products with missing values in CSFID
print("R5: Products with missing values-")
r5=subset(cc, is.na(CSFId))
print(r5)




#R6: Which is the Minimum products in primary category used by customers?
print("R6: Least product in Primary category-")
r6 = min(cc$PrimaryCategory)
print(r6)








# GRAPHICAL ANALYSIS:
# 7. Pie chart of total counts in primary category.


r7<- table(cc$PrimaryCategory)[1:5]
print("Total counts in primary category-")
print(r7)

pie3D(r7,
      labels = paste(round(100 * r7 / sum(r7),1),'%'),
      col = rainbow(length(r7)),
      main = "Pie chart representing top 5 products in primary category-")

legend("topright",fill = rainbow(length(r7)),
       legend = names(r7),cex = 0.5,)






# 8. Bar graph showing total count of top 7 chemicals

cc <- read.csv("chemicals_in_makeup.csv")
r8 <- table(cc$ChemicalName)[1:7]
print(r8)
barplot(r8,col = rainbow(length(r8)),
        las = 2,
        main = "Bar graph showing total count of top 7 chemicals-")





# 9. Histogram showing Chemical Count of Products in subcategory (lip gloss) and chemical MICA

cc <- read.csv("chemicals_in_makeup.csv")
r9 <- subset(cc, SubCategory == "Lip Gloss/Shine" & ChemicalName == "Mica")
print(r9)

r9$ChemicalCount <- as.numeric(r9$ChemicalCount)

hist(r9$ChemicalCount,
     main = "Chemical Count of Products in subcategory (lip gloss) and chemical MICA",
     xlab = "Chemical Count",
     col = "lightblue")






# 10. Line Graph- Group the data by ProductType and count the products by chemical count by CDPHI
r10 <- tapply(cc$CDPHId,cc$ChemicalCount, mean)
print(r10)
plot(r10,type = 'o',col = 'red',xlim = c(0,10),
     xlab = "Chemical Count",ylab = "Chemical CDPHID",
     main= "Chemical count by CDPHId")
grid()





# 11. Boxplot of Chemical Count by Primary Category

cc <- na.omit(cc[, c("ChemicalCount", "PrimaryCategory")])
cc$PrimaryCategory <- as.factor(cc$PrimaryCategory)
cc$ChemicalCount <- as.numeric(cc$ChemicalCount)

boxplot(ChemicalCount ~ PrimaryCategory, data = cc,
        main = "Distribution of Chemical Count by Primary Category",
        xlab = "Primary Category",
        ylab = "Chemical Count",
        col = rainbow(length(unique(cc$PrimaryCategory))),
        las = 2)





# 12. Histogram showing distribution of All Chemical Counts

hist(cc$ChemicalCount,
     main = "Distribution of Chemical Counts",
     xlab = "Chemical Count",
     col = "orange",
     breaks = 10)




# 13. Pie Chart showing distribution of Top 5 Brands
cc <- read.csv("chemicals_in_makeup.csv")

my_colors <- c("tomato", "gold", "skyblue", "orchid", "forestgreen")

top_brands <- table(cc$Brand)
top5_brands <- sort(top_brands, decreasing = TRUE)[1:5]
pie3D(top5_brands,
      labels = paste(round(100 * top5_brands / sum(top5_brands), 1), '%'),
      col = my_colors,
      main = "Top 5 Brands by Product Count")
legend("topright",
       fill = my_colors,
       legend = names(top5_brands),
       cex = 0.6)






# 14. Bar Graph showing Top 7 Subcategories

cc <- read.csv("chemicals_in_makeup.csv")

subcat_counts <- sort(table(cc$SubCategory), decreasing = TRUE)[1:7]
my_colors <- c("tomato", "gold", "skyblue", "orchid", "forestgreen", "darkorange", "mediumseagreen")

barplot(subcat_counts,
        col = my_colors,
        las = 2,
        main = "Top 7 Subcategories by Product Count")


