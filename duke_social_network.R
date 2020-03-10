# loading packages
library(igraph)
library(readr)
library(haven)
library(ggplot2)

# read data
colleague_network = read_csv('PCMI_Personally Know_Combined Edgelist.csv')
discussion_network = read_csv('PCMI_Discussion Network_Combined_Edgelist.csv')

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
colleague_attributes = read_csv('PCMI_Know Personally_Combined_Nodelist.csv')

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
colleague_attributes = read_csv('PCMI_Know Personally_Combined_Nodelist.csv')

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
discussion_attributes = read_csv('PCMI_Discussion Network_Combined_Nodelist.csv')


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


