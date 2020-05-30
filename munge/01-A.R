
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

