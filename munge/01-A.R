
# function to read in stack overflow XML to data table
read_se_xml <- function(tablename){
 message("reading file..")
 raw_xml<- read_xml(paste0("./data/", tablename ,".xml"))
 message("converting rows..")
 xml_nodes <- xml_find_all(raw_xml, "row")
 xml_attrs <- xml_attrs(xml_nodes)
 xml_attrs <- lapply(xml_attrs, as.list)
 message("binding..")
 read_se_xml <- rbindlist(xml_attrs, fill=TRUE)
}


posts <- read_se_xml("Posts")

tags  <- read_se_xml("Tags")


# function to munge tags and create a graph

tagCombinations <- function(taglist){
        matches<- str_match_all(taglist, "<([a-z0-9\\-]+)>")
        tags_matched <- matches[[1]][,2]
        if (length(tags_matched)<2){
                tagCombinations  <- data.frame(from=NA, to=NA)
        }else{
                retdf  <- as.data.frame(t(combn(tags_matched,2)))
                names(retdf) <- c("from", "to")
                tagCombinations <- retdf
        }
}

tagpairs <- map_dfr(posts$Tags, tagCombinations)
tagpairs <- tagpairs[!is.na(tagpairs$from),]
# Add opposites for directed graph
tagpairs <- rbind(tagpairs, data.frame(from=tagpairs$to, to=tagpairs$from))

# test <- "<one><two><three-four>"
# test2 <-"<one>"
# print(tagCombinations(test))