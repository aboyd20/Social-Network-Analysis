### Homework 2 Solution

## Working the bipartite network from the Beer Advocate data.

# ---- C1 ----
# Load libraries

library("ggplot2")
# Must load other packages first
library("sand")
library("intergraph")

# ---- C2 ----
# Load edge and attribute data

edge.df <- read.csv("depp_edges.csv")
node.df <- read.csv("depp_nodes.csv")

# ---- C3 ----
# Convert to graph

gr <-graph.data.frame(edge.df, vertices = node.df, directed=FALSE)

is_bipartite(gr)

# ---- C4 ----
# Examine attributes

list.vertex.attributes(gr)

head(vertex_attr(gr, "name", V(gr)))

V(gr) # i dont know which vertices are the actors or movies
V(gr)$type # even though i get true and false values i still dont know which are actors or movies

# ---- C5 ----
# Projections

#actor to actor bipartite graph 
gr_a2a <-bipartite_projection(gr, which = "FALSE") 
summary(gr_a2a)

#movie to movie bipartite graph 
gr_m2m <-bipartite_projection(gr, which = "TRUE")
summary(gr_m2m)

#creating two bipartite graphs by splitting the actors and movies 
gr_p2p <-bipartite.projection(gr, which="both")
summary(gr_p2p)

# ---- C6 ----
# Remove non-matching attributes

list.vertex.attributes(gr_a2a)
# for actors, we need to delete the "budget" attribute 
gr_a2a <- delete_vertex_attr(gr_a2a, "budget")
list.vertex.attributes(gr_a2a)

list.vertex.attributes(gr_m2m)
# for movies, we need to delete the "decade" attribute
gr_m2m <- delete_vertex_attr(gr_m2m, "decade")
list.vertex.attributes(gr_m2m)

# ---- C7a ----
# Edge weight histogrqm (base)


# ---- C7b ----
# Edge weight histogram (ggplot)

actor_wts <- E(gr_a2a)$weight

p <- ggplot(data.frame(actor_wts = actor_wts), aes(x = actor_wts))
p <- p + geom_histogram(stat = "count") + labs(title="Histogram of Actors Weights")
print(p)

# ---- C8 ----
# Filter edges of weight = 1

gr_a2af <- delete_edges(gr_a2a, E(gr_a2a)[E(gr_a2a)$weight == 1] )
plot(gr_a2af)

plot(gr_a2af, layout=layout_with_fr) # this is a different layout; more clusters

# ---- C9 ----
# Remove singleton nodes

gr_a2af_cl <- delete_edges(gr_a2af, which(E(gr_a2af)$degree == 0))

# ---- C10 ----
# Plot

plot(gr_a2af_cl, layout=layout_with_kk)

# ---- C11a ----
# Weighted degree histogram (base)


# ---- C11b ----
# Weighted degree histogram (ggplot)

wdeg <- graph.strength(gr_a2af_cl) #using the filtered version 
summary(wdeg)

p <- ggplot(data=data.frame(WDeg= wdeg), aes(x = WDeg))
## got the breaks by checking the summary(wdeg) and took the min and max
p <- p + geom_histogram(breaks=seq(0,50, by=5), col = "green") +labs(title="Histogram of Filtered Weighted degree")
print(p)

# ---- C12 ----
# Find the 2 highest weighted degree actors (other than Depp)

tail(sort(wdeg),3)

# ---- C13 ----
# Plot ego networks

##this is how you find the nodes Bloom and Carter
which(V(gr_a2af_cl)$name=="Orlando_Bloom") #19
which(V(gr_a2af_cl)$name=="Helena_Bonham_Carter") #137

egos <- make_ego_graph(gr_a2af_cl, 1, c(19,137)) # the one stands for 1 degree

bloom.gr <- egos[[1]]
carter.gr <- egos[[2]]

#these two lines do not work below--ask why?????
#bloom.lo <- layout_as_star(bloom.gr, V(bloom.gr)[19]) #nice to put the person in the middle 
#carter.lo <- layout_as_star(carter.gr, V(carter.gr)[137])

bloom.lo <- layout_as_star(bloom.gr, V(bloom.gr)[(V(bloom.gr)$name=="Orlando_Bloom")])
carter.lo <- layout_as_star(carter.gr, V(carter.gr)[(V(carter.gr)$name=="Helena_Bonham_Carter")])

par(mfrow =c(1,2))

plot(bloom.gr, layout = bloom.lo)
plot(carter.gr, layout = carter.lo)

par(mfrow =c(1,1))

