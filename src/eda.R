library('ProjectTemplate')
load.project()

tags.df %>% arrange(desc(as.numeric(Count))) %>% head(20)

# Organise and simplify the tag graph
tag_graph <- graph_from_data_frame(tagpairs, directed=FALSE, vertices=tags[,c("TagName", "Count")])
E(tag_graph)$weight <- count_multiple(tag_graph)
tag_graph <- igraph::simplify(tag_graph, remove.multiple=TRUE)

tag_graph_reduced <- delete.edges(tag_graph , which(E(tag_graph)$weight < 1000))
tag_graph_reduced <- delete.vertices(tag_graph_reduced, which(degree(tag_graph_reduced, mode = "all") == 0))
summary(tag_graph_reduced)
is_connected(tag_graph_reduced)

# Plot tag graph
tag_graph_reduced %>% ggraph(layout="auto") +
  geom_edge_link(aes(alpha = weight, width = weight)) +
  geom_node_label(aes(label = name)) +
  theme_void()

#geom_node_text(aes(label = name), repel = TRUE) +