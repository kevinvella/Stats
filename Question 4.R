
install.packages('multcomp')
library('multcomp')

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

#Part 3 #####
install.packages('pwr')
library(pwr)

# Calculate power
power <- pwr.anova.test(k = 5, n = 39, f = 0.25, sig.level = 0.05)$power
power

sample_size <- pwr.anova.test(k = 5, power = 0.95, f = 0.25, sig.level = 0.05)$n
sample_size

#### #Part 4 #######
patients = read.csv('patients.csv')
attach(patients)

patientWithoutFirstCol = patients[, -1]
patientsMatrix = data.matrix(patientWithoutFirstCol)

friedman.test(y = patientsMatrix)
