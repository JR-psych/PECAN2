


#' Title
#'
#' @param nodes_include
#' @param edges_include
#' @param attributes_include
#' @param nodes_drop_NA0
#' @param edges_drop_NA0
#' @param edges_direction
#' @param data
#'
#' @return
#' @export
#'
#' @examples
pecanExtractAgg <- function(data, nodes_include, edges_include, attributes_include = NULL,
                                  nodes_drop_NA0 = NULL, edges_drop_NA0 = NULL,
                                  edges_direction = "from_to"){

if(!inherits(data, "pecanAgg")){stop("Invalid input. Expecting an object of class 'pecanAgg'.")}

agg_info <- data$agg_info
edges_sep <- agg_info$edges_sep
attribute_pattern <- agg_info$attribute_pattern

if(!is.null(attributes_include)){
  if(!("attributes" %in% names(data))){stop("Include atrtibutes is set to TRUE but no attributes were found")}
  if(is.null(agg_info$attribute_pattern)){stop("No attribute pattern was found in the pecanAgg object")}
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
    ex_att <- data$attributes
    ex_att$nodes <- sub(pattern = attribute_pattern, replacement = "",ex_att$nodes)}}



ex_edges <- ex_edges %>% tidyr::separate_wider_delim(edges,names = c("from","to"), delim = edges_sep)

if(edges_direction == "to_from"){
  ex_edges <- ex_edges %>% dplyr::rename(to = from, from = to)
  ex_edges <- ex_edges %>% dplyr::select(from,to,everything())}


names(ex_nodes)[names(ex_nodes) == "nodes"] <- "id"


if(!is.null(attributes_include)){
  names(ex_att)[names(ex_att) == "nodes"] <- "id"
}

if(!is.null(attributes_include)){
  colnames(ex_att)[-which(colnames(ex_att) == "id")] <- paste0(colnames(ex_att)[-which(colnames(ex_att) == "id")],"_att")
}


if(!is.null(attributes_include)){ex_nodes <- merge(ex_nodes,ex_att, by = "id", all = TRUE)}


nNodes_og <- as.numeric(nrow(ex_nodes))
if(!is.null(nodes_drop_NA0)){
  for (col in nodes_drop_NA0){
    ex_nodes <- ex_nodes[(ex_nodes[[col]] != 0 & !is.na(ex_nodes[[col]])),]
    rownames(ex_nodes) <- NULL}
ex_nodes
}

nEdges_og <- as.numeric(nrow(ex_edges))

keep_edges <- NULL

for (i in seq_len(nrow(ex_edges))){
  if(ex_edges[[i,"from"]] %in% ex_nodes$id & ex_edges[[i,"to"]] %in% ex_nodes$id){
    keep_edges <- cbind(keep_edges,i)}
}

keep_edges <- as.vector(keep_edges)
ex_edges <- ex_edges[keep_edges,,drop = FALSE]
rownames(ex_edges) <- NULL

if(nEdges_og != as.numeric(base::nrow(ex_edges))){
  warning("Edges have been dropped as no reference node was found")}






if(!is.null(edges_drop_NA0)){
  for (col in edges_drop_NA0){
    ex_edges <- ex_edges[(ex_edges[[col]] != 0 & !is.na(ex_edges[[col]])),]
    rownames(ex_edges) <- NULL}
  ex_edges
}





ex_info <- list(nodes_include = nodes_include,
                edges_include = edges_include,
                attributes_include = attributes_include,
                nNodes_og = nNodes_og,
                nNodes = as.numeric(nrow(ex_nodes)),
                nEdges_og = nEdges_og,
                nEdges = as.numeric(nrow(ex_edges)),
                nodes_drop_NA0 = nodes_drop_NA0,
                edges_drop_NA0 = edges_drop_NA0 ,
                edges_direction = edges_direction)

data <- list(nodes = ex_nodes,
             edges = ex_edges,
             agg_info = agg_info,
             ex_info = ex_info)

data
}
