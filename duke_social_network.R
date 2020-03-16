# loading packages
library(igraph)
library(readr)
library(haven)
library(ggplot2)

# read data
colleague_network = read_csv('dataset/PCMI_Personally Know_Combined Edgelist.csv')
discussion_network = read_csv('dataset/PCMI_Discussion Network_Combined_Edgelist.csv')

# create igraph style edge list and graph
colleague_edgelist = colleague_network
colleague_graph = graph.data.frame(colleague_edgelist, directed=TRUE)

discussion_edgelist = discussion_network
discussion_graph = graph.data.frame(discussion_edgelist, directed=TRUE)

# -----------visualizations------------
#first try
# layout
set.seed(3952)
layout1 = layout.fruchterman.reingold(colleague_graph)
# node or vertex
V(colleague_graph)$color = "grey"
V(colleague_graph)[degree(colleague_graph, mode="in")<8]$color = "yellow"
#edge
E(colleague_graph)$color = "grey"
#plot
plot(colleague_graph)


#second try
#make node better
V(colleague_graph)$size=degree(colleague_graph, mode="in")/5

V(colleague_graph)$color = "grey"
V(colleague_graph)[degree(colleague_graph, mode="in")<8]$color = "yellow"

E(colleague_graph)$color = "grey"
plot(colleague_graph)


#third try
# layout
set.seed(3952)
layout1 = layout.fruchterman.reingold(colleague_graph)
# node or vertex
V(colleague_graph)$color = "grey"
V(colleague_graph)[degree(colleague_graph, mode="in")<8]$color = "yellow"
V(colleague_graph)$size=degree(colleague_graph, mode="in")/5

#edge
E(colleague_graph)$color = "grey"

#plot
plot(colleague_graph, edge.arrow.size=0.25, edge.arrow.mode="-")


#fourth try
#remove self-loops
colleague_graph2= simplify(colleague_graph, remove.multiple=TRUE, remove.loops=TRUE)

set.seed(3952)
layout1 = layout.fruchterman.reingold(colleague_graph2)
# node or vertex
V(colleague_graph2)$color = "grey"
V(colleague_graph2)[degree(colleague_graph, mode="in")<8]$color = "yellow"
V(colleague_graph2)$size=degree(colleague_graph2, mode="in")/5

#edge
E(colleague_graph2)$color = "grey"

#plot
plot(colleague_graph2, edge.arrow.size=0.25, edge.arrow.mode="-")


#fifth try
#import sample attirbutes
colleague_attributes = read_csv('dataset/PCMI_Know Personally_Combined_Nodelist.csv')

set.seed(3952)
layout1 = layout.fruchterman.reingold(colleague_graph2)
# node or vertex
V(colleague_graph2)$color = "grey"
V(colleague_graph2)[degree(colleague_graph, mode="in")<8]$color = "yellow"
V(colleague_graph2)$size = degree(colleague_graph, mode="in")/5

V(colleague_graph2)$color = ifelse(colleague_attributes[V(colleague_graph2),2]
                                   == "Researcher", "blue", "red")

#edge
E(colleague_graph2)$color = "grey"

#plot
plot(colleague_graph2, edge.arrow.size=0.25, edge.arrow.mode="-")


#sixth try
colleague_attributes = read_csv('dataset/PCMI_Know Personally_Combined_Nodelist.csv')

set.seed(3952)
layout1 = layout.fruchterman.reingold(colleague_graph2, nither=500)
# node or vertex
V(colleague_graph2)$size = degree(colleague_graph, mode="in")/5
V(colleague_graph2)$color = ifelse(colleague_attributes[V(colleague_graph2),2] == "Researcher", "blue", "red")
#edge
E(colleague_graph2)$color = "grey"
#plot
plot(colleague_graph2, edge.arrow.size=0.25, edge.arrow.mode="-", vertex.label = NA)



#discussion network
discussion_graph2 = simplify(discussion_graph, remove.multiple = TRUE, remove.loops = TRUE)
discussion_attributes = read_csv('dataset/PCMI_Discussion Network_Combined_Nodelist.csv')


set.seed(3952)
layout1 = layout.fruchterman.reingold(discussion_graph2, nither=500)
# node or vertex
V(discussion_graph2)$size = degree(discussion_graph2, mode="in")/5
V(discussion_graph2)$color = ifelse(discussion_attributes[V(discussion_graph2),2] == "Researcher", "blue", "red")
#edge
E(discussion_graph2)$color = "grey"
#plot
plot(discussion_graph2, edge.arrow.size=0.25, edge.arrow.mode="-", vertex.label = NA)


# discussion network 2
# try different layout
set.seed(3952)
layout1 = layout_with_kk(discussion_graph2)

V(discussion_graph2)$size = degree(discussion_graph2, mode="in")/5
V(discussion_graph2)$color = ifelse(discussion_attributes[V(discussion_graph2),2] == "Researcher", "blue", "red")
#edge
E(discussion_graph2)$color = "grey"
#plot
plot(discussion_graph2, edge.arrow.size=0.25, edge.arrow.mode="-", vertex.label = NA)



### connectivity
# density 
graph.density(discussion_graph2, loop=FALSE)
graph.density(colleague_graph2, loop=FALSE)

# average path length
# larger path distance = less dense network (but not always)
mean_distance(discussion_graph2)
mean_distance(colleague_graph2)

# degree distribution
degree_distribution(discussion_graph2)
discussion_degreeDis = degree_distribution(discussion_graph2)

discussion_degreeDis2 = as.data.frame(discussion_degreeDis)
qplot(discussion_degreeDis, data=discussion_degreeDis2, geom="histogram", binwidth= .001)


### clustering coefficient
discussion_trans = transitivity(discussion_graph2)
colleague_trans = transitivity(colleague_graph2)


### positional features
# degree: in, out, all centrality
discussion_outdegree = degree(discussion_graph2, mode="out")
discussion_outdegree = as.data.frame(discussion_outdegree)

discussion_indegree = degree(discussion_graph2, mode="in")
discussion_indegree = as.data.frame(discussion_indegree)

## in degree
#layout
set.seed(3952)
layout1 = layout.fruchterman.reingold(discussion_graph2, niter=500)

# node options
V(discussion_graph2)$size = degree(discussion_graph, mode="in")/5
V(discussion_graph2)$color = ifelse(discussion_attributes[V(discussion_graph2), 2] == "Researcher", "blue", "red")

# edge options
E(discussion_graph2)$color = "grey"

plot(discussion_graph2, edge.arrow.size=0.25, edge.arrow.mode="-", vertex.label = NA)


## out degree
#layout
set.seed(3952)
layout1 = layout.fruchterman.reingold(discussion_graph2, niter=500)

# node options
V(discussion_graph2)$size = degree(discussion_graph, mode="out")/5
V(discussion_graph2)$color = ifelse(discussion_attributes[V(discussion_graph2), 2] == "Researcher", "blue", "red")

# edge options
E(discussion_graph2)$color = "grey"

plot(discussion_graph2, edge.arrow.size=0.25, edge.arrow.mode="-", vertex.label = NA)
## result: mostly blue, more concentration   


## all degree
#layout
set.seed(3952)
layout1 = layout.fruchterman.reingold(discussion_graph2, niter=500)

# node options
V(discussion_graph2)$size = degree(discussion_graph, mode="all")/5
V(discussion_graph2)$color = ifelse(discussion_attributes[V(discussion_graph2), 2] == "Researcher", "blue", "red")

# edge options
E(discussion_graph2)$color = "grey"

plot(discussion_graph2, edge.arrow.size=0.25, edge.arrow.mode="-", vertex.label = NA)
## result: more evenly distribution of red and blue, but bigger ones with blue



### closeness centrality
discussion_Incloseness = closeness(discussion_graph2, mode="in")
discussion_Incloseness = as.data.frame(discussion_Incloseness)

discussion_Outcloseness = closeness(discussion_graph2, mode="out")
discussion_Outcloseness = as.data.frame(discussion_Outcloseness)

discussion_Allcloseness = closeness(discussion_graph2, mode="all")
discussion_Allcloseness = as.data.frame(discussion_Allcloseness)


### betweenness centrality
discussion_betweenness = betweenness(discussion_graph2)
discussion_betweenness = as.data.frame(discussion_betweeness)

## visualize
#layout
set.seed(3952)
layout1 = layout.fruchterman.reingold(discussion_graph2, niter=500)

# node options
V(discussion_graph2)$size = betweenness(discussion_graph)/200
V(discussion_graph2)$color = ifelse(discussion_attributes[V(discussion_graph2), 2] == "Researcher", "blue", "red")

# edge options
E(discussion_graph2)$color = "grey"

plot(discussion_graph2, edge.arrow.size=0.25, edge.arrow.mode="-", vertex.label = NA)

##result: only three high betweenness people



### eigen vector centrality: people who are most connected to other highlt connected ones
discussion_eigencentrality = eigen_centrality(discussion_graph2)
discussion_eigencentrality = as.data.frame(discussion_eigencentrality)

## visualize
#layout
set.seed(3952)
layout1 = layout.fruchterman.reingold(discussion_graph2, niter=500)

# node options
V(discussion_graph2)$size = eigen_centrality(discussion_graph)/5
V(discussion_graph2)$color = ifelse(discussion_attributes[V(discussion_graph2), 2] == "Researcher", "blue", "red")

# edge options
E(discussion_graph2)$color = "grey"

plot(discussion_graph2, edge.arrow.size=0.25, edge.arrow.mode="-", vertex.label=NA)

## result: highly central people, not same as the betweenness



### we want to know how assorted the discussion network is
### -> community detection techniques
### community detection algorithms include
### 1. by iteratively calculating edges betweenness
### 2. by using eigenvector matrices
### 3. by iteratively optimizing for modularity
### 4. using random walk methods
### 5. using label propogaiton techniques

# edge-betweenness
GNC = cluster_edge_betweenness(discussion_graph2, weights=NULL)
V(discussion_graph2)$color = membership(GNC)
discussion_graph2$paletter = diverging_pal(length(GNC))
plot(discussion_graph2, edge.arrow.size=0.25, edge.arrow.mode="-", vertex.label=NA)


# eigenvector matrices
EigenVec = leading.eigenvector.community(discussion_graph2, steps = -1,
                                         options = igraph.arpack.default)
V(discussion_graph2)$color = membership(EigenVec)
discussion_graph2$paletter = diverging_pal(length(EigenVec))
plot(discussion_graph2, edge.arrow.size=0.25, edge.arrow.mode="-", vertex.label=NA)


#infomap community finding
InfoMap = cluster_infomap(discussion_graph2, e.weights = NULL, v.weights = NULL,
                          nb.trials = 10)
plot(discussion_graph2, edge.arrow.size=0.25, edge.arrow.mode="-", vertex.label=NA)



### interactive visualizations
require(visNetwork)

## discussion graph
# transform data 
visnetwork_Disdata = toVisNetworkData(discussion_graph2, idToLabel = TRUE)
dis_nodes = visnetwork_Disdata$nodes
dis_edges = visnetwork_Disdata$edges
# visualization
visNetwork(dis_nodes, dis_edges)


## colleague graph
# transform data
visnetwork_Colldata = toVisNetworkData(colleague_graph2, idToLabel = TRUE)
coll_nodes = visnetwork_Colldata$nodes
coll_edges = visnetwork_Colldata$edges
# visualization

visNetwork(coll_nodes, coll_edges)

