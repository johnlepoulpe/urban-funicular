setwd("~/Travail/2017/second_semestre/Stat/Evaluation/urban-funicular")
tab = read.table("programs.txt", header=T, sep = "\t" )
head(tab)

# Question 1

## Nombre de programmes par langage
pdf('programme_langage.pdf')
barplot(table(tab$Language), main = "Nombre de programme par langage", xlab = "Langage")
dev.off()

## Nombre de lignes dupliquées par programme, ordonnées par ordre décroissant

pdf('lines_programme.pdf')
op = par(mar = c(9, 4, 4, 2) + 0.1) # permet de l'aisser un espace suffisant sous le graphique, afin d'écrire les noms de colonnes en vertical.
duplicated_lines = tab[order(tab$DuplicatedLines, decreasing = TRUE),]$DuplicatedLines[!is.na(tab$DuplicatedLines)]
duplicated_lines_arg = tab[order(tab$DuplicatedLines, decreasing = TRUE),]$Code[!is.na(tab$DuplicatedLines)]
barplot(duplicated_lines, 
        names.arg = duplicated_lines_arg,
        las = 2,
        main = 'Number of lines duplicated for each program')
dev.off()

