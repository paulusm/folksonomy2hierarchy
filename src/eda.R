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

# Progress bar
pb <- progress_bar$new(total = vcount(tag_graph))
message("Pruning by 0.3..")

########## 1) Prune based on weight fractions of incoming links
for (i in 1:vcount(tag_graph)){
  v <- V(tag_graph)[i]
  #print(v$name)
  # get in edges
  e <- incident(tag_graph, v, mode="in")
  # this is the weight threshold, a fraction of the largest
  maxweight <- max(e$weight) * 0.3
  prunable <- e[e$weight < maxweight]
  tag_graph <- delete.edges(tag_graph, prunable )
  pb$tick()
}
summary(tag_graph)

##########  2) Calculate z scores for edges and save as attribute

#total objects
q <- nrow(posts)
pb <- progress_bar$new(total = ecount(tag_graph))
message("Calculating edge z-scores..")

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
  pb$tick()
}

##########  3) Select max z score as ancestor

# Function to check for mutual links
select_ancestor <- function(ei, eo){
  
  sibling <- FALSE
  
  # Get the max z score and the edge with that score
  maxz <- max(ei$`z-score`) 
  emax <- ei[ei$`z-score` == maxz]
  
  et <- tail_of(tag_graph, emax)
  
  # Check the out links for mutuality
  for(eoi in eo){
    eh <- head_of(tag_graph, eoi)
    if(et == eh){
      sibling <- TRUE
    }
  }
  # Recurse if that one has a sibling
  if(sibling == TRUE){
    dife <- difference(ei, emax)
    if (length(dife) ==0){
      select_ancestor <- dife
    }
    else{
      select_ancestor <- select_ancestor (dife, eo)
    }
  }
  else{
    select_ancestor  <- emax
  }
}
pb <- progress_bar$new(total = vcount(tag_graph))
message("Select ancestors..")

for (i in 1:vcount(tag_graph)){
  v <- V(tag_graph)[i]
  # get in and out edges
  e_in <- incident(tag_graph, v, mode="in")
  e_out <- incident(tag_graph, v, mode="out")
  ans <- select_ancestor(e_in, e_out)
  prunable <- difference(e_in, ans)
  tag_graph <- delete.edges(tag_graph, prunable )
  pb$tick()
}
summary(tag_graph)

##########  4) Select global root node 

message("select global root..")
# Get all candidates
roots <- V(tag_graph)[degree(tag_graph, mode="in")==0]
pb <- progress_bar$new(total = length(roots))
for(i in 1:length(roots)){
  # get outlinks
  e_root <- incident(tag_graph, roots[i])
  w_root <- edge_attr(tag_graph, "weight", e_root)
  wtot_root <- sum(w_root)
  ent_root <- sum(w_root/wtot_root * log(w_root/wtot_root)) * -1
  vertex_attr(tag_graph, "entropy", index = roots[i]) <- ent_root
  print(roots[i])
  print(ent_root)
}
# Select winner
max_ent <- max(vertex_attr(tag_graph,"entropy", roots))
global_root <- roots[roots$entropy == max_ent]

# reattach remainder
orphan_roots <- difference(roots, global_root)

for(i in 2:2){#length(orphan_roots)){
  e_root <- incident(tag_graph, orphan_roots[i])
  w_max <- max(edge_attr(tag_graph, "weight", e_root))
  e_max <- e_root[e_root$weight == w_max]
  tag_graph <- add_edges(tag_graph, c())
  #tag_graph <- delete_edges(tag_graph, e_max)
}

############ Viz


# A reduced graph for viz
tag_graph_reduced <- delete.edges(tag_graph , which(E(tag_graph)$weight < 1000))
tag_graph_reduced <- delete.vertices(tag_graph_reduced, which(degree(tag_graph_reduced, mode = "all") == 0))
summary(tag_graph_reduced)

plot(tag_graph_reduced, layout = layout.reingold.tilford(tag_graph_reduced, root=V(tag_graph_reduced)[name=="website-design"]))

print(tag_graph, full = TRUE )

# Plot tag graph
tag_graph_reduced %>% ggraph(layout = 'dendrogram') +
  geom_edge_link(aes(alpha = weight, width = weight)) +
  geom_node_label(aes(label = name)) +
  theme_void()


