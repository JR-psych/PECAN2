

#' Title
#'
#' @param net_nodes
#' @param nodes_color
#' @param nodes_color_max
#' @param nodes_color_scaling
#' @param nodes_color_by
#' @param nodes_community
#'
#' @return
#' @export
#'
#' @examples
nodes_color_warnings <- function(net_nodes, nodes_color, nodes_color_max,nodes_color_scaling,nodes_color_by,
                                 nodes_community){

if(is.null(nodes_community)){

  if(is.null(nodes_color_by)){
    if(is.list(nodes_color)){stop("nodes_color can not be a list if nodes_color by is not defined")}
#    if(length(nodes_color) > 1){if(as.numeric(length(nodes_color) != as.numeric(nrow(net_nodes)))) # probably not needed
#    {stop("nodes color vector is not the same size as number of nodes")}} # probably not needed

    }

  if(!is.null(nodes_color_by)){
    if(!is.list(nodes_color)){stop("nodes_color must be a list if nodes_color by is defined")}
    if(length(nodes_color) != 2){stop("nodes_color needs to be a list of two colors if nodes_color_by is defined")}
    if(any(lengths(nodes_color) != 1)){stop("nodes_color needs to be a list of two colors if nodes_color_by is defined")}
    if(nodes_color_scaling != "scaled" & nodes_color_scaling != "fixed"){stop("nodes_color_scaling must be 'scaled' or 'fixed'")}
    if(!is.null(nodes_color_max)){
      if(is.numeric(nodes_color_max) == FALSE | 0 > nodes_color_max){
        stop("nodes_color_max must be NUll or numeric and above 0")}}
    if(!is.numeric(as.vector(net_nodes[,"color"]))){stop("nodes_color_by must be numeric")}}


}

if(!is.null(nodes_community)){
  if(is.null(nodes_color_by)){stop("nodes_color_by must be defined if nodes community is used")}
  if(!is.list(nodes_color)){stop("nodes_color must be a list if nodes_color by is defined")}
  if(any(as.numeric(lengths(nodes_color)) != 2)){stop("Wrong number of elements in nodes_color")}
  if(as.numeric(nlevels(as.factor(net_nodes[,"com"]))) != as.numeric(length(nodes_color))){#nodes_community
                            stop("number of groups does not match elements in the nodes_color list")}
  if(any(is.na(net_nodes[,"com"]))){stop("No Na´s allowed in nodes_community")}
  if(nodes_color_scaling != "scaled" & nodes_color_scaling != "fixed"){stop("nodes_color_scaling must be 'scaled' or 'fixed'")}
  if(!is.null(nodes_color_max)){
    if(is.numeric(nodes_color_max) == FALSE | 0 > nodes_color_max){
      stop("nodes_color_max must be numeric and above 0")}}
  if(!is.numeric(as.vector(net_nodes[,"color"]))){stop("nodes_color_by must be numeric")}}


  }



# Old code. Delete if the new one works

#browser()
#if(is.list(nodes_color)){
#    if(is.null(nodes_color_by)){stop("Visualization stoped! nodes_color_by is not defined")}

#    if(is.numeric(as.vector(net_nodes[,"color"])) == FALSE){stop("nodes_color_by must be numeric")}
#    if(as.numeric(length(nodes_color)) == 2){if(all(as.numeric(lengths(nodes_color))) != 1 & all(as.numeric(lengths(nodes_color)) != 2))
#                                                {stop("Wrong number of elements in nodes_color")}}
#    else{ lengths_test <- as.numeric(lengths(nodes_color)) != 2
#      if(any(lengths_test)){stop("Wrong number of elements in nodes_color")}}

#  if(as.numeric(length(nodes_color)) >= 2 & (all(lengths(nodes_color) == 2))){
#    if(is.null(nodes_community)){stop("nodes_community is not defined")}
   # browser()
#    if(as.numeric(nlevels(as.factor(net_nodes[,"com"]))) != as.numeric(length(nodes_color))){#nodes_community
#      stop("number of groups does not match elements in the nodes_color list")}
#    if(any(is.na(net_nodes[,"com"]))){stop("No Na´s allowed in nodes_community")}}#nodes_community
#  if(all(lengths(nodes_color)) != 2){"Some objects in nodes_color dont have 2 elements"}
#  }


#  if(is.list(nodes_color) == FALSE){
#    if(length(nodes_color) > 1){if(as.numeric(length(nodes_color) != as.numeric(nrow(net_nodes))))
#    {stop("nodes color vector is not the same size as number of nodes")}}}



#  }


