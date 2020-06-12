library('ProjectTemplate')
load.project()

#tags.df %>% arrange(desc(as.numeric(Count))) %>% head(20)

# Create the tag graph with weights from coccurances
tag_graph <- graph_from_data_frame(tagpairs, directed=TRUE, vertices=tags[,c("TagName", "Count")])
E(tag_graph)$weight <- count_multiple(tag_graph)
tag_graph <- igraph::simplify(tag_graph, remove.multiple=TRUE, remove.loops = TRUE)
summary(tag_graph)


# Tibely et al, algorithm A
# Tibeĺy, G., Pollner, P., Vicsek, T., & Palla, G. (2013). 
# Extracting tag hierarchies. PLoS ONE, 8(12), 1–46. https://doi.org/10.1371/journal.pone.0084133

# 1) Prune based on weight fractions of incoming links
for (i in 1:vcount(tag_graph)){
  v <- V(tag_graph)[i]
  #print(v$name)
  # get in edges
  e <- incident(tag_graph, v, mode="in")
  # this is the weight threshold, a fraction of the largest
  maxweight <- max(e$weight) * 0.4
  prunable <- e[e$weight < maxweight]
  tag_graph <- delete.edges(tag_graph, prunable )
}
summary(tag_graph)

# 2) Calculate z scores for edges and save as attribute

#total objects
q <- nrow(posts)

for(i in 1:ecount(tag_graph)){
  e <- E(tag_graph)[i]
  qij <- e$weight
  # get the vertices either side
  vends <- ends(tag_graph, e)
  
  # get prevalence of those tags
  qi <- as.numeric(vertex_attr(tag_graph, "Count", vends[1]))
  qj <- as.numeric(vertex_attr(tag_graph, "Count", vends[2]))
  sd <- ((qi * qj) / q) * ((q - qi) / q) * ((q - qj) / q-1)
  z <- ((qij - (qi * qj)) / q) / sd
  edge_attr(tag_graph, "z-score", e) <- z
}

# 3) Select max z score as ancestor
for (i in 3:vcount(tag_graph)){
  v <- V(tag_graph)[i]
  # get in edges
  e <- incident(tag_graph, v, mode="in")
  if(length(e) > 1){
    e_out <- incident(tag_graph, v, mode="out")
    maxz <- max(e$`z-score`) 
    prunable <- e[e$`z-score` < maxz]
    tag_graph <- delete.edges(tag_graph, prunable )
  }
}
summary(tag_graph)

# 4) Select root node by removing in link from most common pair
topedge <- E(tag_graph)[weight == max(E(tag_graph)$weight)]
topv <- ends(tag_graph, topedge)[1]
topin <- incident(tag_graph, topv, mode="in")
tag_graph <- delete.edges(tag_graph, topin )

# A reduced graph for viz
tag_graph_reduced <- delete.edges(tag_graph , which(E(tag_graph)$weight < 1000))
tag_graph_reduced <- delete.vertices(tag_graph_reduced, which(degree(tag_graph_reduced, mode = "all") == 0))
summary(tag_graph_reduced)

plot(tag_graph_reduced, layout = layout.reingold.tilford(tag_graph_reduced, root=V(tag_graph_reduced)[topv]))

# Plot tag graph
tag_graph_reduced %>% ggraph(layout = 'dendrogram') +
  geom_edge_link(aes(alpha = weight, width = weight)) +
  geom_node_label(aes(label = name)) +
  theme_void()


