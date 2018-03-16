setwd("~/marco/Documents/Cours/sem2/Stat/TPs/Eval/urban-funicular")
tab <- read.table("programs.txt", header=TRUE, sep= "\t")
tab_lines = tab[order(tab$LinesOfCode, decreasing = TRUE),]
tab_dupl = tab[order(tab$DuplicatedBlocks, decreasing = TRUE),]

#tab$assert: number of assert() for 1000 lines of code

barplot(table(tab$Language), main = "Number of programs written in a given language")

# Modify graphical parameters to fit vertical labels
op = par(mar = c(9, 4, 4, 2) + 0.1)
yticks = seq()
barplot(table(tab$Domain), main = "Number of programs for each scientific domain", las = 2)

op = par(mar = c(7, 4, 4, 2) + 0.1)
barplot(tab_lines$LinesOfCode, names.arg = tab_lines$Code, las = 2, main = "Number of lines of code for each program")

op = par(mar = c(7, 4, 4, 2) + 0.1)
barplot(tab_dupl$DuplicatedBlocks, names.arg = tab_dupl$Code, las = 2, main = "Number of duplicated code blocks for each program")

data = data.frame(temp_noNA_code, temp_noNA_dupl / temp_noNA_lines)
colnames(data) = c("Program", "DupLin_per")
data = data[order(data$DupLin_per, decreasing = TRUE),]
op = par(mar = c(7, 4, 4, 2) + 0.1)
barplot(data$DupLin_per, names.arg = data$Program, las = 2, main = "Number of duplicated lines of code per line of code for each program")

# Reset graphic parameters
op = par(mar = c(5, 4, 4, 2) + 0.1)

