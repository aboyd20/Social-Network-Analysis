# Homwework 4

# ---- C1 ----
library("ggplot2")
library("GGally")
# Must load other packages first
library("sand")
library("scales")
library("intergraph")

# ---- C2 ----

setwd("F:/Fall Quarter 2018/DSC 480/Week 4/hwk4/hwk4")

lesmis <- read.graph("les-mis-full.graphml", format="graphml")

# ---- C3 -----

list.vertex.attributes(lesmis)

head(vertex_attr(lesmis, "bipartite", V(lesmis)))

V(lesmis)$type <- V(lesmis)$bipartite

table(V(lesmis)$type)


# ---- C4 ----

lesmis_s2s <- bipartite_projection(lesmis, which = "TRUE")

lesmis_s2s

summary(lesmis_s2s)

# ---- C5 ----

lesmis_wts <- E(lesmis_s2s)$weight

p <- ggplot(data.frame(lesmis_wts = lesmis_wts), aes(x = lesmis_wts))
p <- p + geom_histogram(stat = "count") + labs(title="Histogram of Section Weights")
print(p)

# ---- C6 ----
# Filter edges of weight = 1

lesmis_s2sf <- delete_edges(lesmis_s2s, E(lesmis_s2s)[E(lesmis_s2s)$weight == 1] )
plot(lesmis_s2sf)

summary(lesmis_s2sf)

plot(lesmis_s2sf, layout=layout_with_fr) # this is a different layout; more clusters

# Remove singleton nodes (delete vertices because its a node. The square bracket calls something within the node)

lesmis_cl <- delete_vertices(lesmis_s2sf, V(lesmis_s2sf)[degree(lesmis_s2sf) == 0])
plot(lesmis_cl)

summary(lesmis_cl)


# ---- C7 ----
# Decompose 

lesmis_de <- decompose(lesmis_cl)

sapply(lesmis_de, vcount)


# ---- C8 ----

lesmis_comp <- lesmis_de[[1]]


summary(lesmis_comp)

# ---- C9 ----

write_graph(lesmis_comp, file="lesmis.graphml", format = "graphml")

# ---- C10 ----

# ---- C11 ----

