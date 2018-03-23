setwd("~/Travail/2017/second_semestre/Stat/Evaluation/urban-funicular")
tab = read.table("programs.txt", header=T, sep = "\t" )
head(tab)

# Question 1

## Nombre de programmes par langage
pdf('rapport/figures/programme_langage.pdf')
barplot(table(tab$Language), main = "Nombre de programme par langage", xlab = "Langage")
dev.off()

## Nombre de lignes dupliquées par programme, ordonné par ordre décroissant

pdf('rapport/figures/lines_programme.pdf')
op = par(mar = c(9, 4, 4, 2) + 0.1) # permet de laisser un espace suffisant sous le graphique, afin d'écrire les noms de colonnes en vertical.
tab_no_dupl_lines_NA = tab[order(tab$DuplicatedLines, decreasing = TRUE),]
tab_no_dupl_lines_NA = tab_no_dupl_lines_NA[!is.na(tab_no_dupl_lines_NA$DuplicatedLines),]
barplot(tab_no_dupl_lines_NA$DuplicatedLines, 
        names.arg = tab_no_dupl_lines_NA$Code,
        las = 2,
        main = 'Number of lines duplicated for each program')
dev.off()

## Nombre de blocs dupliquées par programme, ordonné par ordre décroissant

pdf('rapport/figures/blocks_programme.pdf')
op = par(mar = c(9, 4, 4, 2) + 0.1) # permet de laisser un espace suffisant sous le graphique, afin d'écrire les noms de colonnes en vertical.
tab_no_dupl_block_NA = tab[order(tab$DuplicatedBlocks, decreasing = TRUE),]
tab_no_dupl_block_NA = tab_no_dupl_block_NA[!is.na(tab_no_dupl_block_NA$DuplicatedBlocks),]
barplot(tab_no_dupl_block_NA$DuplicatedBlocks, 
        names.arg = tab_no_dupl_block_NA$Code,
        las = 2,
        main = 'Number of blocks duplicated for each program')
dev.off()

