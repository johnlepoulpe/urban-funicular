setwd("~/marco/Documents/Cours/sem2/Stat/TPs/Eval/urban-funicular")
sink("rapport/tests/tests.txt", append = TRUE)
tab = read.table("programs.txt", header = TRUE, sep = "\t" )
head(tab)
summary(tab)

###############################################################################

# Question 1

## Nombre de programmes par langage
pdf('rapport/figures/prog_lang.pdf')
barplot(table(tab$Language),
        main = "Nombre de programmes par langage",
        xlab = "Langage")
dev.off()

## Nombre de programmes par domaine
pdf('rapport/figures/prog_dom.pdf')
op = par(mar = c(9, 4, 4, 2) + 0.1)
barplot(table(tab$Domain),
        main = "Nombre de programmes par domaine",
        las = 2)
dev.off()

## Nombre de lignes de code par programme, ordonné par ordre décroissant
pdf('rapport/figures/lin_prog.pdf')
op = par(mar = c(7, 4, 4, 2) + 0.1)
tab_lin = tab[order(tab$LinesOfCode, decreasing = TRUE),]
barplot(tab_lin$LinesOfCode,
        names.arg = tab_lin$Code,
        las = 2,
        main = "Nombre de lignes de code par programme")
dev.off()

## Nombre de lignes dupliquées par programme, ordonné par ordre décroissant
pdf('rapport/figures/dlin_prog.pdf')
tab_dlines = tab[order(tab$DuplicatedLines, decreasing = TRUE),]
tab_dlines_noNA = tab_dlines[!is.na(tab_dlines$DuplicatedLines),]
barplot(tab_dlines_noNA$DuplicatedLines, 
        names.arg = tab_dlines_noNA$Code,
        las = 2,
        main = 'Nombre de lignes de code dupliquées par programme')
dev.off()

## Nombre de blocs dupliquées par programme, ordonné par ordre décroissant
pdf('rapport/figures/dbl_prog.pdf')
tab_dblocks = tab[order(tab$DuplicatedBlocks, decreasing = TRUE),]
tab_dblocks_noNA = tab_dblocks[!is.na(tab_dblocks$DuplicatedBlocks),]
barplot(tab_dblocks_noNA$DuplicatedBlocks, 
        names.arg = tab_dblocks_noNA$Code,
        las = 2,
        main = 'Nombre de blocs de code dupliqués par programme')
dev.off()

###############################################################################

# Question 2

tabP = na.omit(tab[, c("Code", "Domain", "Language", "DuplicatedLines", "LinesOfCode")])
tabP$DupLin_per = tabP$DuplicatedLines / tabP$LinesOfCode * 100
tabP = tabP[order(tabP$DupLin_per, decreasing = TRUE),]

## Pourcentage de lignes dupliquées par programme
pdf('rapport/figures/pdlin_prog.pdf')
barplot(tabP$DupLin_per,
        names.arg = tabP$Code,
        las = 2,
        main = "Pourcentage de lignes dupliquées par programme")
dev.off()

###############################################################################

# Question 3

tabP_C = tabP[tab$Language == "C",]
tabP_CPP = tabP[tab$Language == "C++",]

wilcox.test(tabP_CPP$DuplicatedLines, tabP_C$DuplicatedLines)
wilcox.test(tabP_C$DupLin_per, tabP_CPP$DupLin_per)

###############################################################################

# Question 4

tabP_CO = tabP[tab$Language %in% c("C++", "C/C++"),]

t.test(tabP_C$DupLin_per, conf.level = 0.9)
t.test(tabP_CO$DupLin_per, conf.level = 0.9)

###############################################################################

# Question 5

tab_Clang = tab[!is.na(tab$ClangWarning), ]

tab_short = tab_Clang[tab_Clang$LinesOfCode <= median(tab_Clang$LinesOfCode),]
tab_long = tab_Clang[tab_Clang$LinesOfCode > median(tab_Clang$LinesOfCode),]

wilcox.test(tab_short$ClangWarning, tab_long$ClangWarning, alternative = "less")
wilcox.test(tab_short$MinorWraning, tab_long$MinorWraning, alternative = "less")
wilcox.test(tab_short$MajorWarning, tab_long$MajorWarning, alternative = "less")

# ne suivent pas une loi normale
shapiro.test(tab_Clang$LinesOfCode)
shapiro.test(tab_Clang$ClangWarning[na.rm = TRUE])

# Test de corrélation paramétrique de Pearson
cor.test(tab_Clang$ClangWarning, tab_Clang$LinesOfCode)
# Test de corrélation non-paramétrique de Spearman
cor.test(tab_Clang$ClangWarning, tab_Clang$LinesOfCode, method = "spearman")

pdf('rapport/figures/clang_lin.pdf')
plot(tab_Clang$LinesOfCode, tab_Clang$ClangWarning,
     xlab = "Lignes de code", ylab = "Nombre de warnings Clang",
     main = "Nombre de warnings Clang en fonction du nombre de lignes de code")
mod = lm(tab_Clang$ClangWarning ~ tab_Clang$LinesOfCode)
abline(mod)
dev.off()

# Reset graphic parameters
op = par(mar = c(5, 4, 4, 2) + 0.1)

###############################################################################

# Question 6

## Influence des evaluations de valgrind sur le nombre de warnings

tabW = na.omit(tab[, c("MajorWarning", "MinorWarning", "ClangWarning", "Valgrind")])

tabW_CL = tabW[tabW$Valgrind == "clean" | tabW$Valgrind == "leaks",]
Clean = tabW_CL[tabW_CL$Valgrind == "clean",]
NClean = tabW_CL[tabW_CL$Valgrind != "clean",]

# Comparaison des moyennes des programmes ayant une évaluation valgrind "CLEAN" et les autres

# > On peut affirmer qu'il existe une différence significative entre les
# > moyennes au risque de 5% pour les major warnings
wilcox.test(Clean$MajorWarning, NClean$MajorWarning)
wilcox.test(Clean$MinorWarning, NClean$MinorWarning)
wilcox.test(Clean$ClangWarning, NClean$ClangWarning)

## Influence du domaine scientifique sur le pourcentage de lignes dupliquées

# Pas de différence des moyennes significative au risque de 5%
kruskal.test(tabP$DupLin_per ~ tabP$Domain)
