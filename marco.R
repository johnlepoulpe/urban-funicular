setwd("~/marco/Documents/Cours/sem2/Stat/TPs/Eval/urban-funicular")
tab <- read.table("programs.txt", header=TRUE, sep= "\t")
tab_lines = tab[order(tab$LinesOfCode, decreasing = TRUE),]

tab_dupl = tab[order(tab$DuplicatedBlocks, decreasing = TRUE),]
tab_dupl_noNA = tab_dupl[!is.na(tab_dupl$DuplicatedBlocks),]

tab_C = tab[tab$Language == "C",]
tab_CO = tab[tab$Language %in% c("C++", "C/C++"),]

tab_short = tab[tab$LinesOfCode <= median(tab$LinesOfCode),]
tab_long = tab[tab$LinesOfCode > median(tab$LinesOfCode),]

# QUESTION 1
# tab$assert: number of assert() for 1000 lines of code
######################################################

barplot(table(tab$Language), main = "Number of programs written in a given language")

######################################################

# Modify graphical parameters to fit vertical labels
op = par(mar = c(9, 4, 4, 2) + 0.1)
yticks = seq()
barplot(table(tab$Domain), main = "Number of programs for each scientific domain", las = 2)

######################################################

op = par(mar = c(7, 4, 4, 2) + 0.1)
barplot(tab_lines$LinesOfCode, names.arg = tab_lines$Code, las = 2, main = "Number of lines of code for each program")

######################################################

op = par(mar = c(7, 4, 4, 2) + 0.1)
barplot(tab_dupl_noNA$DuplicatedBlocks, names.arg = tab_dupl_noNA$Code, las = 2, main = "Number of duplicated code blocks for each program")

# QUESTION 2
######################################################

tabP = na.omit(tab[, c("Code", "Domain", "DuplicatedLines", "LinesOfCode")])
tabP$DupLin_per = tabP$DuplicatedLines / tabP$LinesOfCode

tabP = tabP[order(tabP$DupLin_per, decreasing = TRUE),]

op = par(mar = c(7, 4, 4, 2) + 0.1)
barplot(tabP$DupLin_per, names.arg = tabP$Code, las = 2, main = "Number of duplicated lines of code per line of code for each program")

# QUESTION 4
######################################################

tabP_C = na.omit(tab_C[, c("Code", "Domain", "DuplicatedLines", "LinesOfCode")])
tabP_C$DupLin_per = tabP_C$DuplicatedLines / tabP_C$LinesOfCode

tabP_CO = na.omit(tab_CO[, c("Code", "Domain", "DuplicatedLines", "LinesOfCode")])
tabP_CO$DupLin_per = tabP_CO$DuplicatedLines / tabP_CO$LinesOfCode

t.test(tabP_C$DupLin_per, conf.level = 0.9)
t.test(tabP_CO$DupLin_per, conf.level = 0.9)


# QUESTION 5
######################################################

wilcox.test(tab_short$ClangWarning, tab_long$ClangWarning, alternative = "less")

# Reset graphic parameters
op = par(mar = c(5, 4, 4, 2) + 0.1)

# QUESTION 6
#####################################################

# nb de warnings gcc ~ qual valgrind

tabW = na.omit(tab[, c("MajorWarning", "MinorWarning", "ClangWarning", "Valgrind")])

# This is equivalent to a wilcoxon test if there are only two groups
# ------------------------------------------------- #
tabW_CL = tabW[tabW$Valgrind == "clean" | tabW$Valgrind == "leaks",]
Clean = tabW_CL[tabW$Valgrind == "clean",]
Leaks = tabW_CL[tabW$Valgrind == "leaks",]

# pas normale
shapiro.test(Clean$MinorWarning)
shapiro.test(Leaks$MinorWarning)

# p-value = 1.459e-05 -> non-homoscédastique à 5%
bartlett.test(tabW_CL$MajorWarning ~ tabW_CL$Valgrind)
# p-value = 0.08678 -> homoscédastique à 5%
bartlett.test(tabW_CL$MinorWarning ~ tabW_CL$Valgrind)
# p-value = 0.9156 -> homoscédastique à 5%
bartlett.test(tabW_CL$ClangWarning ~ tabW_CL$Valgrind)

kruskal.test(tabW_CL$MajorWarning ~ tabW_CL$Valgrind)
kruskal.test(tabW_CL$MinorWarning ~ tabW_CL$Valgrind)
kruskal.test(tabW_CL$ClangWarning ~ tabW_CL$Valgrind)

# Significant difference only with major warnings at 5%
wilcox.test(Clean$MajorWarning, Leaks$MajorWarning)
wilcox.test(Clean$MinorWarning, Leaks$MinorWarning)
wilcox.test(Clean$ClangWarning, Leaks$ClangWarning)
# ------------------------------------------------- #

# No significant differences
kruskal.test(tabW$MajorWarning ~ tabW$Valgrind)
kruskal.test(tabW$MinorWarning ~ tabW$Valgrind)
kruskal.test(tabW$ClangWarning ~ tabW$Valgrind)
####################################################

# pourcentage de lignes dupliquées ~ domaine scientifique

# pas de différences significatives
kruskal.test(tabP$DupLin_per ~ tabP$Domain)
