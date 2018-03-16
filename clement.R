setwd("~/Travail/2017/second_semestre/Stat/Evaluation/urban-funicular")
tab = read.table("programs.txt", header=T, sep = "\t" )
head(tab)

# question 1

barplot(table(tab$Language), main = "Nombre de programme par langage", xlab = "Langage")
barplot(table(tab$Valgrind), main = "Nombre de programme par état Valgrind", xlab = "État")
barplot(table(tab$Malloc), main = "Nombre de programme par état Malloc", xlab = "État")

op = par(mar = c(9, 4, 4, 2) + 0.1)
barplot(table(tab$Domain), main = "Nombre de programme par Domaine",  las = 2)

op = par(mar = c(9, 4, 4, 2) + 0.1)
barplot(tab[order(tab$DuplicatedLines, decreasing = TRUE),]$DuplicatedLines, names.arg = tab[order(tab$DuplicatedLines, decreasing = TRUE),]$Code, las = 2,
            main = 'Number of lines duplicated for each program')

op = par(mar = c(9, 4, 4, 2) + 0.1)

tabbis = tab[is.na(tab$DuplicatedBlocks) == FALSE] 
barplot(tab[order(tab$DuplicatedBlocks, decreasing = TRUE),]$DuplicatedBlocks[is.na(tab$DuplicatedBlocks) == FALSE], 
        names.arg = tab[order(tab$DuplicatedBlocks, decreasing = TRUE),]$Code[is.na(tab$DuplicatedBlocks) == FALSE], las = 2,
        main = 'Number of blocks duplicated for each program')

# question 3
