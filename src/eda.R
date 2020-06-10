library('ProjectTemplate')
load.project()

#tags.df %>% arrange(desc(as.numeric(Count))) %>% head(20)

# Create the tag graph with weights from coccurances
tag_graph <- graph_from_data_frame(tagpairs, directed=TRUE, vertices=tags[,c("TagName", "Count")])
E(tag_graph)$weight <- count_multiple(tag_graph)
tag_graph <- igraph::simplify(tag_graph, remove.multiple=TRUE)
summary(tag_graph)


# A reduced graph for viz
tag_graph_reduced <- delete.edges(tag_graph , which(E(tag_graph)$weight < 1000))
tag_graph_reduced <- delete.vertices(tag_graph_reduced, which(degree(tag_graph_reduced, mode = "all") == 0))
summary(tag_graph_reduced)
is_connected(tag_graph_reduced)

# Plot tag graph
tag_graph_reduced %>% ggraph(layout = 'stress') +
  geom_edge_link(aes(alpha = weight, width = weight)) +
  geom_node_label(aes(label = name)) +
  theme_void()

# Tibely et al, algorithm A
# Tibeĺy, G., Pollner, P., Vicsek, T., & Palla, G. (2013). 
# Extracting tag hierarchies. PLoS ONE, 8(12), 1–46. https://doi.org/10.1371/journal.pone.0084133

# 1) Duplicate graph to get two way relations
edges <- get.edgelist(tag_graph)
tag_graph <- add_edges(tag_graph,  matrix(c(edges[,2], edges[,1])))
summary(tag_graph)

#2 Prune based on weight fractions of incoming links




