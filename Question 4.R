#The following is the log10 of the genome size (measured in picograms of DNA per
#haploid cell) of 3 different types of crustaceans. The log10 transformation is taken to
#make the variance along the groups uniform.
#i. Use a one-way ANOVA test at 0.05 level of significance to determine whether
#the mean log10 of the genome size is significantly different between crustacean
#groupings. Furthermore, construct a one-way ANOVA model for the above
#data. Also, use a 0.05 level of significance to check the normality,
#homoscedasticity and independence assumptions for the error terms of this
#model.
#3
#ii. At a 0.05 level of significance, implement pairwise post-hoc analysis on the
#levels by implementing Bonferroni, Tukey and Scheffé methods for pairwise
#comparisons. Interpret the result for each. 

install.packages('multcomp')
library('multcomp')

crustaceans <- read.csv("crustaceans.csv")

crustaceansData <- list(
  Copepod = c(0.25, 0.25, 0.58, 0.97),
  Barnacle = c(0.67, 0.9, 1.23, 1.4),
  Isopods = c(1.71, 2.35, 2.4, 1.65)
)

par(mfrow = c(1, 3))
for (i in 1:length(crustaceansData)) {
  plot(crustaceansData[[i]], pch = 20, main = names(crustaceansData)[i])
  qqline(crustaceansData[[i]])
}

boxplot(crustaceansData, names = names(crustaceansData), ylab = "Log10 Genome Size", main = "Boxplot - Genome Size by Crustacean Group")

# Combine all the data into a single vector
all_values <- unlist(crustaceansData)

# Perform the ANOVA test
anova_result <- aov(all_values ~ rep(names(crustaceansData), lengths(crustaceansData)))

# Print the ANOVA summary
summary(anova_result)

# Perform Tukey's post-hoc test
tukey_result <- TukeyHSD(anova_result)

# Print the pairwise comparisons
print(tukey_result)

# Perform pairwise comparisons using Bonferroni correction
bonferroni_result <- pairwise.t.test(all_values, rep(names(crustaceansData), lengths(crustaceansData)),
                                     p.adjust.method = "bonferroni")

# Print the pairwise comparisons
print(bonferroni_result)

# Perform pairwise comparisons using Scheffé correction
scheffe_result <- glht(anova_result, linfct = mcp(Group = "Tukey"))

# Print the pairwise comparisons
summary(scheffe_result)

################ Part 2 ###############
genexpdata <- read.csv("genexpdata.csv")
attach(genexpdata)
genexpFrameData <- data.frame(genexpdata$Method, genexpdata$Treatment, genexpdata$Gene.Expression)

# Perform regression analysis
genexpdataModel = lm(genexpdata$Gene.Expression ~ genexpdata$Method + genexpdata$Treatment, data = genexpFrameData)
summary(genexpdataModel)
