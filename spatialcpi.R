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


# Deriving the quantity values
data$Quantity <- exp / data$Price


#############################NOTE#################################
# in solving any "price" index, primary:price::secondary:quantity
# else, primary:quantity::secondary:price
# in this code, we will solve the price indices of the following
##################################################################

primary <- data$Price
secondary <- data$Quantity

## before we can turn it into matrices, we need the size of dimension
group_unique <- unique(group) # list all the unique countries involved
obs_unique <- unique(obs) # list all the unique commodities

## we are not interested with the list, but on the number of elements
group <- length(group_unique)
obs <- length(obs_unique)

## matrix can be made using array()
pmat <- array(primary, dim = c(obs, group)) # pmat for primary matrix
smat <- array(secondary, dim = c(obs, group)) # smat for secondary matrix

## cross-country expenditure solver
crossproduct <- function(primary, secondary) {
    crossprod((primary), (secondary))
}

cross_expenditure <- crossproduct(pmat, smat)

#### FISHER INDEX ####

## Fisher index requires Laspeyres's and Paasche's

# compute laspeyres's and paasche's indices first

laspeyres <- matrix(nrow = group, ncol = group)
paasche <- matrix(nrow = group, ncol = group)

for (c in 1:group) {
    for (b in 1:group) {
        laspeyres[c, b] <- cross_expenditure[c, b] / cross_expenditure[b, b]
    }
}

for (c in 1:group) {
    for (b in 1:group) {
        paasche[c, b] <- cross_expenditure[c, c] / cross_expenditure[b, c]
    }
}

# Fisher Index

fisher <- matrix(nrow = group, ncol = group)

for (c in 1:group) {
    for (b in 1:group) {
        fisher[c, b] <- (laspeyres[c, b] * paasche[c, b]) ^ (0.5)
    }
}
fisher