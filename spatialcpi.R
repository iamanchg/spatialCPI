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

#### EDGEWORTH-MARSHALL INDEX ####

# Derive the dimension of average secondaries
amdimension <- choose(group, 2)

# compute for the arithmetic mean of the secondary
i <- 1 ## initialization

arithmean <- matrix(nrow = obs, ncol = amdimension)

for (c in 1:group) {
    for (b in 1:group) {
        if (c < b) {
            arithmean[, i] <- (smat[, b] + smat[, c]) / 2
            i <- i + 1
        }
    }
}

sumem <- crossproduct(pmat, arithmean)

edgeworth_marshall <- matrix(nrow = group, ncol = group)

ctru <- 1
ctrd <- 1
ctrm <- 1

# upper triangular matrix (up)
for (c in 1:group) {
    for (b in 1:group) {
        if (c < b) {
            edgeworth_marshall[c, b] <- sumem[c, ctru] / sumem[b, ctru]
            ctru <- ctru + 1
        }
    }
}

# lower triangular matrix (down)
for (b in 1:group) {
    for (c in 1:group) {
        if (c > b) {
            edgeworth_marshall[c, b] <- sumem[c, ctrd] / sumem[b, ctrd]
            ctrd <- ctrd + 1
        }
    }
}

# for the main diagonal (middle)
for (m in 1:group) {
    edgeworth_marshall[m, m] <- sumem[m, ctrm] / sumem[m, ctrm]
    ctrm <- ctrm + 1
}

edgeworth_marshall