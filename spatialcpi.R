# load necessary packages
library(readxl) ## to read .xlsx files
library(combinat) ## to use combinatrics

# import data
data <- read_excel("./data5.xlsx")

# to extend the limited print output
options(max.print = 10^8)

# Variable declaration and assignment
exp <- data$Expenditure
group <- data$Country
obs <- data$Commodity