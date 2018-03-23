setwd("~/Travail/2017/second_semestre/Stat/Evaluation/urban-funicular")
tab = read.table("programs.txt", header=T, sep = "\t" )
head(tab)

# Question 1

pdf('programme_language.pdf')
barplot(table(tab$Language), main = "Nombre de programme par langage", xlab = "Langage")
dev.off()

barplot(table(tab$Valgrind), main = "Nombre de programme par état Valgrind", xlab = "État")
barplot(table(tab$Malloc), main = "Nombre de programme par état Malloc", xlab = "État")

op = par(mar = c(9, 4, 4, 2) + 0.1)
barplot(table(tab$Domain), main = "Nombre de programme par Domaine",  las = 2)

pdf('lines_programme.pdf')
op = par(mar = c(9, 4, 4, 2) + 0.1)
barplot(tab[order(tab$DuplicatedLines, decreasing = TRUE),]$DuplicatedLines, names.arg = tab[order(tab$DuplicatedLines, decreasing = TRUE),]$Code, las = 2,
            main = 'Number of lines duplicated for each program')
dev.off()

op = par(mar = c(9, 4, 4, 2) + 0.1)
tabbis = tab[is.na(tab$DuplicatedBlocks) == FALSE] 
barplot(tab[order(tab$DuplicatedBlocks, decreasing = TRUE),]$DuplicatedBlocks[is.na(tab$DuplicatedBlocks) == FALSE], 
        names.arg = tab[order(tab$DuplicatedBlocks, decreasing = TRUE),]$Code[is.na(tab$DuplicatedBlocks) == FALSE], las = 2,
        main = 'Number of blocks duplicated for each program')

# Question 3

# Plutôt que le nombre de lignes de code dupliquées par programme, nous avons décidé, de comparer les pourcentages correspondant afin que la longueur des fichiers ne biaise pas le test.

temp_noNA_dupl = tab$DuplicatedLines[is.na(tab$DuplicatedLines) == FALSE]
temp_noNA_lines = tab$LinesOfCode[is.na(tab$DuplicatedLines) == FALSE]
temp_noNA_code = tab$Code[is.na(tab$DuplicatedLines) == FALSE]
temp_noNA_language = tab$Language[is.na(tab$DuplicatedLines) == FALSE]


data = data.frame(temp_noNA_code, temp_noNA_dupl / temp_noNA_lines, temp_noNA_language)
colnames(data) = c("Program", "DupLin_per", "Language")
head(data)
wilcox.test(data[data$Language == "C++",]$DupLin_per, data[data$Language == "C",]$DupLin_per)

# Hypothese H0: Il n'y a pas de différences significatives du pourcentage de lignes de code dupliquées en fonction du langage de programmation utilisé.
# Au risque d'erreur 5% , nous ne pouvons pas rejeter H0.
# Nous en concluons que le langage de programmtion n'influe sur le pourcentage de lignes de code dupliquées.

# Question 6
