# Homwework 3

# ---- C1 ----
library("ggplot2")
library("GGally")
# Must load other packages first
library("sand")
library("scales")
# ---- C2 ----
#path <- "/Users/robinburke/Box Sync/2018_cFall/dsc480Bx/hwk/hwk3"
#setwd("C:/Users/rburke4/Box Sync/2018_cFall/dsc480Bx/hwk/hwk3")

#setwd("F:/Fall Quarter 2018/DSC 480/Week 3/hwk3")
setwd("/Volumes/BOYDDEPAUL/Fall Quarter 2018/DSC 480/Week 3/hwk3")

antony <- read.graph("antony-c2c.graphml", format="graphml")


# ---- C3 -----
# Filter

antonyf <- delete_edges(antony, E(antony)[E(antony)$weight == 1] )
plot(antonyf)

# Remove singleton nodes (delete vertices because its a node. The square bracket calls something within the node)

antonyf_cl <- delete_vertices(antonyf, V(antonyf)[degree(antonyf) == 0])
plot(antonyf_cl)

antonyf_cl_de <- decompose(antonyf_cl)

sapply(antonyf_cl_de, vcount)

antony_comp <- antonyf_cl_de[[3]]

#### another way to compute

# c <-components(antonyf_cl, mode =c("strong")) #used component function instead inorder to decompose graph to contain size of cluster
# y <-decompose.graph(antonyf_cl)[[which(c$csize==max(c$csize))]]


# ---- C4 ----

lo <- layout_with_fr(antony_comp)
plot(antony_comp, layout=lo, edge.arrow.size=0.5)

# ---- C5 ----

antony_comp.wdeg <- graph.strength(antony_comp)
antony_comp.name <- V(antony_comp)$label

antony_comp.dc <- degree(antony_comp, normalized = TRUE, mode="in")
antony_comp.bet <-betweenness(antony_comp, normalized = TRUE, weights= NA)
antony_comp.clo <- closeness(antony_comp, normalized = TRUE,  weights = NA)
antony_comp.pr <- page_rank(antony_comp)
antony_comp.eig <- eigen_centrality(antony_comp, directed =TRUE)

# ---- C6 ----

antony_comp.cent <- data.frame(name =antony_comp.name,
                               degree = antony_comp.dc, 
                            wdeg = antony_comp.wdeg,
                           pr = antony_comp.pr$vector, 
                           eig=antony_comp.eig$vector,
                           betw=antony_comp.bet, 
                           clo=antony_comp.clo)

ggcorr(antony_comp.cent, label=TRUE )

# ---- C7 ----
# Range 5 - 15

eig_sizes = rescale(antony_comp.cent$eig, to = c(5,15))
betw_sizes = rescale(antony_comp.cent$betw, to = c(5,15))
par(mfrow =c(1,2))

plot(antony_comp,layout= lo, vertex.size =eig_sizes)
plot(antony_comp,layout= lo, vertex.size =betw_sizes)

par(mfrow =c(1,1))

# ---- C8 ----

betw.df <- data.frame(Betweenness= antony_comp.bet, Nat=as.factor(V(antony_comp)$Nationality))
#View(betw.df)

g <- ggplot(betw.df, aes(x=Betweenness, fill=Nat))
g<- g+geom_density(aes(alpha = 0.5))
g <-g + guides(alpha=FALSE)
print(g)  #based on the discrete values 


# ---- C9 ----

eig.df <- data.frame(EV_Centrality= antony_comp.eig$vector, Nat=as.factor(V(antony_comp)$Nationality))

#View(eig.df)

g <- ggplot(eig.df, aes(y=EV_Centrality, x=Nat))
g<- g+geom_violin() + geom_jitter(height = 0, width = 0.05)

print(g)  #based on the discrete values

# ---- C10 ----
# Antony and Cleopatra subset

antony_sub <- antony_comp.cent[which(antony_comp.cent$name == 'antony' | antony_comp.cent$name == 'cleopatra'),]
antony_sub

difference <- (antony_sub[1,2:7]-antony_sub[2,2:7])/antony_sub[2,2:7]
difference
# ---- C11 ----

