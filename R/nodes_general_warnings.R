

nodes_general_warnings <- function(net_nodes = net_nodes){

if(is.data.frame(net_nodes) == FALSE){stop("nodes must be a dataframe")}
#if("internal_color" %in% colnames(net_nodes)){warning("internal_color already exist in edges. This might cause problems")}
if("id" %in% colnames(net_nodes) == FALSE){stop("id column in nodes is missing")}
if(any(is.na(net_nodes$id))){stop("No Na´s allowed in nodes$id")}
if(any(is.na(net_nodes$size))){stop("No Na´s allowed in nodes$size")}
}
