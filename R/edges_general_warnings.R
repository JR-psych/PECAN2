#' Title
#'
#' @param net_edges
#'
#' @return
#' @export
#'
#' @examples
edges_general_warnings <- function(net_edges){

if(is.data.frame(net_edges) == FALSE){stop("edges must be a dataframe")}
if("from" %in% colnames(net_edges) == FALSE){stop("'from' column in edges is missing")}
if("to" %in% colnames(net_edges) == FALSE){stop("'to' column in edges is missing")}
if("width" %in% colnames(net_edges) & is.numeric(net_edges$width) == FALSE){stop("'width' must be numeric")}
if(any(is.na(net_edges$from))){stop("No Na´s allowed in edges$from")}
if(any(is.na(net_edges$to))){stop("No Na´s allowed in edges$to")}
if("width" %in% colnames(net_edges) & any(is.na(net_edges$width))){stop("No Na´s allowed in edges$width")}
}
