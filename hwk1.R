### Homework 1 Solution

## Some visualizations of the Marvel comic data

# ---- C1 ----
# Load packages

library("ggplot2")
# Must load other packages first
library("sand")

# ---- C2 ----
# Load data

path <- "/class 480/hwk1"
setwd(path)

torch <- read.graph("torch3.graphml", format="graphml")
summary(torch)

# ---- C3 ----
# Character names

V(torch)$label

# ---- C4a ----
# Delete edges with weight less than 50

torch_greater50 <- delete_edges(torch, E(torch)[E(torch)$weight < 50])


# ---- C5 ----
# Plot network

plot(torch_greater50)

# ---- C6 ----
# Degree

deg <- degree(torch_greater50)
print(deg)
summary(deg)

# ---- C7a ----
# Degree distribution histogram

hist(deg)

# ---- C7b ----
# GGPlot version

g <- ggplot(data.frame(Degree = deg), aes(x = Degree ))
g <- g + geom_histogram(stat = "count")
print(g + ggtitle("Degree distribution Histogram"))

# ---- C8a ----
# plot edge weight distribution


hist(E(torch_greater50)$weight, xlab = "Weights", breaks = seq(50,769,25))
     
     
# ---- C8b ----
# plot edge weight distribution

g <- ggplot(data.frame(Weight = E(torch_greater50)$weight), aes(x = Weight))
g <- g + geom_histogram(stat = "count", bins = 25)
print(g + ggtitle("Weight"))

# ---- C9 ----
# Calculate wt attribute

wt <- E(torch_greater50)$weight /50

# ---- C10 ----
# Compute weighted degree

Weighted_deg <- graph.strength(torch_greater50, weights =wt)

summary(Weighted_deg)

# ---- C11 ----
# Visualization with GGPlot

hist(Weighted_deg, xlab = "Weighted-degree", breaks = seq(9.22,104.04,10))


# ---- C12a ----
# Distribution of weighted degree for male and female characters
marvel.df <- data.frame(Degree=Weighted_deg, Sex =V(torch_greater50)$Gender)


# ---- C12b ----
# ggplot boxplot

g <- ggplot(marvel.df, aes(x = Sex, y=Degree))
g <- g + geom_boxplot()
print(g)

# ---- Extra ----
# Label of the highest weighted degree node one line
V(torch_greater50)$label[which.max(Weighted_deg)]

