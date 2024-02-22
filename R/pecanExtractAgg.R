


#' Title
#'
#' @param nodes_include
#' @param edges_include
#' @param attributes_include
#' @param nodes_drop_NA0
#' @param edges_drop_NA0
#' @param attribute_drop_NA0
#' @param edges_direction
#' @param data
#'
#' @return
#' @export
#'
#' @examples
pecanExtractAgg <- function(data, nodes_include, edges_include, attributes_include = NULL,
                                  nodes_drop_NA0 = NULL, edges_drop_NA0 = NULL,
                                  attribute_drop_NA0 = NULL, edges_direction = "from_to"){

if(!inherits(data, "pecanAgg")){stop("Invalid input. Expecting an object of class 'pecanAgg'.")}

agg_info <- data$agg_info
edges_sep <- agg_info$edges_sep

if(!is.null(attributes_include)){
  if(!exists(data$attributes)){stop("Include atrtibutes is set to TRUE but now attributes were found")}
  if(is.null(attribute_pattern)){stop("Attribute patter was found in the pecanAgg object")}
  }


if(all(edges_include != "all")){
edges_include <- c("edges",edges_include)
ex_edges <- data$edges
ex_edges <- ex_edges[,edges_include,drop = FALSE]}


if(any(edges_include == "all")){
ex_edges <- data$edges}

if(all(nodes_include != "all")){
  nodes_include <- c("nodes",nodes_include)
  ex_nodes <- data$nodes
  ex_nodes <- ex_nodes[,nodes_include,drop = FALSE]}

if(any(nodes_include == "all")){
  ex_nodes <- data$nodes}

if(!is.null(attributes_include)){
  if(all(attributes_include != "all")){
    attributes_include <- c("nodes",attributes_include)
  ex_att <- data$attributes
  ex_att <- ex_att[,attributes_include,drop = FALSE]}

  if(any(attributes_include == "all")){
    ex_att <- data$attributes}}



ex_edges <- ex_edges %>% tidyr::separate(edges,c("from","to"),"_")

if(edges_direction == "to_from"){
  ex_edges <- ex_edges %>% dplyr::rename(to = from, from = to)
  ex_edges <- ex_edges %>% dplyr::select(from,to,everything())}


names(ex_nodes)[names(ex_nodes) == "nodes"] <- "id"


if(!is.null(attributes_include)){
  names(ex_att)[names(ex_att) == "nodes"] <- "id"
}


if(!is.null(nodes_drop_NA0)){
  for (col in nodes_drop_NA0){
    ex_nodes <- ex_nodes[(ex_nodes[[col]] != 0 & !is.na(ex_nodes[[col]])),]
    rownames(ex_nodes) <- NULL}
ex_nodes
}


if(!is.null(edges_drop_NA0)){
  for (col in edges_drop_NA0){
    ex_edges <- ex_edges[(ex_edges[[col]] != 0 & !is.na(ex_edges[[col]])),]
    rownames(ex_edges) <- NULL}
  ex_edges
}


if(!is.null(attribute_drop_NA0)){
  for (col in attribute_drop_NA0){
    ex_att <- ex_att[(ex_att[[col]] != 0 & !is.na(ex_att[[col]])),]
    rownames(ex_att) <- NULL}
  ex_att
}

if(!is.null(attributes_include)){
colnames(ex_att)[-which(colnames(ex_att) == "id")] <- paste0(colnames(ex_att)[-which(colnames(ex_att) == "id")],"_att")
}


if(!is.null(attributes_include)){ex_nodes <- merge(ex_nodes,ex_att, by = "id", all = TRUE)}


data <- list(nodes = ex_nodes,
             edges = ex_edges,
             agg_info = agg_info)

data
}
