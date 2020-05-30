library('ProjectTemplate')
load.project()

tags.df %>% arrange(desc(as.numeric(Count))) %>% head(20)