# Data tree 

tagtree <- tag_graph %>% as_data_frame(what="both")

tagtree$edges <- tagtree$edges %>% arrange(from, to)

tags.dt <- FromDataFrameNetwork(tagtree$edges)

print(tags.dt, "weight", limit = 100)

save(tags.dt, file="docs/tags.RData")