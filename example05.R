# Example 5

## Working with CUG testing

# ---- C1 ----
library("ggplot2")
library("GGally")
library("mcclust")
library("sna")
# Must load other packages first
library("sand")


# ---- C2 ----
#path <- "C:/Users/rburke/Dropbox/2016_cFall/csc495DB/example/ex5"
path <- "/Volumes/BOYDDEPAUL/Fall Quarter 2018/DSC 480/Week 6/ex5"
setwd(path)

lotr1 <- read.graph("lotr1.graphml", format="graphml")
lotr3 <- read.graph("lotr3.graphml", format="graphml")

summary(lotr1)
summary(lotr3)

# ---- C3 ----

wtdeg1 <- graph.strength(lotr1)
wbin1 <-tabulate(wtdeg1)

#make a dataframe

bindf1 <- data.frame(bin=seq(1, length(wbin1)), count=wbin1, movie = "LOTR1")
bindf1 <- bindf1[bindf1$count>0, ]
bindf1$freq <- bindf1$count /vcount(lotr1)

wtdeg3 <- graph.strength(lotr3)
wbin3 <-tabulate(wtdeg3)

#make a dataframe

bindf3 <- data.frame(bin=seq(1, length(wbin3)), count=wbin3, movie = "LOTR3")
bindf3 <- bindf3[bindf3$count>0, ]
bindf3$freq <- bindf3$count /vcount(lotr3)


wdeg.df <-rbind(bindf1, bindf3)
wdeg.df
# ---- C4 ----

g <- ggplot(data = wdeg.df, aes(x=bin, y=freq, color=movie))
g <- g + geom_point()
g <- g + geom_smooth(se=F)
g <- g + scale_y_log10("Frequency")
g <- g + scale_x_log10("Wt. Degree")
print(g)

# ---- C5 ----

plf1 <-fit_power_law(wtdeg1)
plf1$alpha

plf1$KS.stat
plf1$KS.p
plf1$xmin


plf3 <-fit_power_law(wtdeg3)
plf3$alpha

plf3$KS.stat
plf3$KS.p
plf3$xmin

# ---- C6 ----

g <- barabasi.game(1000)
deg <- degree(g, mode = "in")
pltba <- fit_power_law(deg)

pltba$alpha
pltba$KS.stat
pltba$KS.p

# ---- C7 ----

xmin <- plf3$xmin
alpha <- plf3$alpha

xmax <- max(wdeg.df[wdeg.df$movie=="LOTR3", ]$bin)

xvals <- seq(from = xmin, to=xmax, by=1)
yval = xvals^(-alpha)
fit.df <-data.frame(bin=xvals, freq= yval, movie="LOTR3-FIT",count=0)

wdeg.df <- rbind(wdeg.df, fit.df)

# ---- C8 ----
g <- ggplot(data = wdeg.df[wdeg.df$movie !="LOTR1",], aes(x=bin, y=freq, color=movie))
g <- g + geom_point()
g <- g + geom_smooth(se=F)
g <- g + scale_y_log10("Frequency")
g <- g + scale_x_log10("Wt. Degree")
print(g)
# ---- C9 ----
source("mycugtest.R")

# ---- C10 ----

transitivity(lotr1, type = "global")

lotr1.cug <- mycugtest(lotr1, transitivity, cmode="edges", directed=FALSE,
                       type="global")

# ---- C11 ----
print.cug.test(lotr1.cug)
plot.cug.test(lotr1.cug)
# ---- C12 ----
transitivity(lotr3, type = "global")

lotr3.cug <- mycugtest(lotr3, transitivity, cmode="edges", directed=FALSE,
                       type="global")
print.cug.test(lotr3.cug)
plot.cug.test(lotr3.cug)

# ---- C13 ----

assortativity_nominal(lotr1, types=factor(V(lotr1)$Race))

lotr1.cug <- mycugtest(lotr1, assortativity_nominal, cmode = "edges",
                       directed = FALSE, types=factor(V(lotr1)$Race))
print.cug.test(lotr1.cug)

plot.cug.test(lotr1.cug)
# ---- C14 ----
assortativity_nominal(lotr3, types=factor(V(lotr3)$Race))

lotr3.cug <- mycugtest(lotr3, assortativity_nominal, cmode = "edges",
                       directed = FALSE, types=factor(V(lotr3)$Race))
print.cug.test(lotr3.cug)

plot.cug.test(lotr3.cug)