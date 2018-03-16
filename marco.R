setwd("~/marco/Documents/Cours/sem2/Stat/TPs/Eval/urban-funicular")
tab <- read.table("programs.txt", header=TRUE, sep= "\t")
tab_lines = tab[order(tab$LinesOfCode, decreasing = TRUE),]
tab_dupl = tab[order(tab$DuplicatedBlocks, decreasing = TRUE),]
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
barplot(tab_dupl$DuplicatedBlocks, names.arg = tab_dupl$Code, las = 2, main = "Number of duplicated code blocks for each program")

# QUESTION 2
######################################################

temp_noNA_dupl = tab_lines$DuplicatedLines[is.na(tab_lines$DuplicatedLines) == FALSE]
temp_noNA_lines = tab_lines$LinesOfCode[is.na(tab_lines$DuplicatedLines) == FALSE]
temp_noNA_code = tab_lines$Code[is.na(tab_lines$DuplicatedLines) == FALSE]

data = data.frame(temp_noNA_code, temp_noNA_dupl / temp_noNA_lines)
colnames(data) = c("Program", "DupLin_per")
data = data[order(data$DupLin_per, decreasing = TRUE),]

op = par(mar = c(7, 4, 4, 2) + 0.1)
barplot(data$DupLin_per, names.arg = data$Program, las = 2, main = "Number of duplicated lines of code per line of code for each program")

# QUESTION 4
######################################################

temp_noNA_dupl_C = tab_C$DuplicatedLines[is.na(tab_C$DuplicatedLines) == FALSE]
temp_noNA_lines_C = tab_C$LinesOfCode[is.na(tab_C$DuplicatedLines) == FALSE]

temp_noNA_dupl_CO = tab_CO$DuplicatedLines[is.na(tab_CO$DuplicatedLines) == FALSE]
temp_noNA_lines_CO = tab_CO$LinesOfCode[is.na(tab_CO$DuplicatedLines) == FALSE]

dupLin_per_C = temp_noNA_dupl_C / temp_noNA_lines_C
dupLin_per_CO = temp_noNA_dupl_CO / temp_noNA_lines_CO

t.test(dupLin_per_C, conf.level = 0.9)
t.test(dupLin_per_CO, conf.level = 0.9)

# QUESTION 5
######################################################


# Reset graphic parameters
op = par(mar = c(5, 4, 4, 2) + 0.1)

