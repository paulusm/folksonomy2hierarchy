############ Viz

# A reduced graph for viz
tag_graph_reduced <- delete.edges(tag_graph , which(E(tag_graph)$weight < 750))
tag_graph_reduced <- delete.vertices(tag_graph_reduced, which(degree(tag_graph_reduced, mode = "all") == 0))
summary(tag_graph_reduced)

#plot(tag_graph, layout = layout.reingold.tilford(tag_graph, root=global_root))

#print(tag_graph, full = TRUE )

# Plot tag graph
tag_graph_reduced %>% ggraph(layout = 'dendrogram', circular = TRUE) + #circlepack
  geom_edge_link(aes(alpha = weight, width = weight)) +
  #geom_edge_elbow()+
  geom_node_label(aes(label = name)) + #,repel=TRUE'
  theme_void()