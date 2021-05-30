library(igraph)
library(dplyr)
library(ggplot2)

# Read file
edges = read.csv(file.choose(),header=T, sep = ",")
edges <-edges[,c(1,2,5)]
edges

nodes = read.csv(file.choose(),header=T, sep = ",")

# Check For NA values in file
na <- colSums(is.na(edges))
na[na>0]


#################################################################################
###                           Question 1
#################################################################################

g <- graph_from_data_frame(edges, directed=FALSE, vertices=nodes)


#################################################################################
###                           Question 2
#################################################################################

# number of vertices
vcount(g)
# number of edges
ecount(g)
# diameter
diameter(g)
# triangles
cl.tri=cliques(g,min=3,max=3)
length(cl.tri)
#alternative
sum(count_triangles(g))/3

# number of degrees of each node
deg <- degree(g, mode="all")
deg_df <- as.data.frame(deg)
deg_df$nodes <- row.names(deg_df)
row.names(deg_df) <- 1:796
deg_df_order <- deg_df[order(deg_df$deg,decreasing = TRUE),]


# top 10 degrees
head(deg_df_order, 10)

# top 10 degrees with weights
deg1 <- strength(g, vids = V(g), loops = TRUE)
deg1_df <- as.data.frame(deg1)
deg1_df$nodes <- row.names(deg1_df)
row.names(deg1_df) <- 1:796
deg1_df_order <- deg1_df[order(deg1_df$deg1,decreasing = TRUE),]

head(deg1_df_order, 10)

# Reference https://igraph.org/r/doc/strength.html

#################################################################################
###                           Question 3
#################################################################################

plot(g, vertex.size=4, vertex.color = rainbow(10, .8, .8, alpha= .8),vertex.label = NA,
     edge.arrow.size = 0.3, edge.arrow.width = 4, edge.color = "darkgrey", 
     main = "Plot of the Network \"A Song of Ice and Fire\"")


new_graph <- induced.subgraph(g, V(g)[degree(g)>10])
plot(new_graph, vertex.size=10, vertex.color = rainbow(10, .8, .8, alpha= .8),vertex.label = NA,
     edge.arrow.size = 0.6, edge.arrow.width = 18, edge.color = "darkgrey", 
     main = "Plot of the Network \"A Song of Ice and Fire\"
     Top Vertices")

edge_density(g)
edge_density(new_graph)
#################################################################################
###                           Question 4
#################################################################################
options(scipen = 999)


# reference https://igraph.org/r/doc/closeness.html
# closeness centrality 
centr_close = closeness(g, vids = V(g),normalized = FALSE)
centr_close_df <- as.data.frame(centr_close)
centr_close_df$nodes <- row.names(centr_close_df)
row.names(centr_close_df) <- 1:796
centr_close_df_order <- centr_close_df[order(centr_close_df$centr_close,decreasing = TRUE),]
head(centr_close_df_order, 15)

# reference https://igraph.org/r/doc/betweenness.html
# betweenness centrality 
centr_between = betweenness(g, v = V(g),normalized = FALSE)
centr_between_df <- as.data.frame(centr_between)
centr_between_df$nodes <- row.names(centr_between_df)
row.names(centr_between_df) <- 1:796
centr_between_df_order <- centr_between_df[order(centr_between_df$centr_between,decreasing = TRUE),]
head(centr_between_df_order, 15)

#################################################################################
###                           Question 5
#################################################################################

#PageRank    
pagerank<-page_rank(g, algo = "prpack", vids = V(g), damping = 0.85,
                personalized = NULL, weights = E(g)$weights)

plot(g, vertex.size=pagerank$vector*1000, vertex.color = rainbow(10, .8, .8, alpha= .8),vertex.label = NA,
     edge.arrow.size = 0.6, edge.arrow.width = 18, edge.color = "grey", 
     main = "Plot of the Network's Pagerank value 
     \"A Song of Ice and Fire\"
     ")
