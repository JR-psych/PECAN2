#' Title
#'
#' @param net_nodes
#' @param nodes_color
#' @param nodes_color_max
#' @param nodes_color_scaling
#' @param nodes_color_by
#'
#' @return
#' @export
#'
#' @examples
vis_nodes_color <- function(net_nodes, nodes_color, nodes_color_max, nodes_color_scaling){

#browser()
#######################################################
if("com" %in% names(net_nodes)){
  if(is.null(nodes_color_max)){nodes_color_max <- max(net_nodes$color)}
  net_nodes$st_id <- 1:nrow(net_nodes)
  net_nodes <- vis_nodes_color_max(net_nodes = net_nodes,nodes_color_max = nodes_color_max)
  nodes_combined <- data.frame()
  unique_groups <- sort(unique(net_nodes$com))

  for (i in unique(net_nodes$com)){
    i_nodes <- net_nodes %>% filter(com == i)
    j <- as.numeric(which(unique_groups == i))

    if(nodes_color_scaling == "fixed"){
      rgb_i <- colorRamp(c(nodes_color[[j]][1],nodes_color[[j]][2]))(i_nodes$color/10)

      i_colors <- rgb(rgb_i[, 1], rgb_i[, 2], rgb_i[, 3], maxColorValue = 255)}
    if(nodes_color_scaling == "scaled"){
      if(min(i_nodes$color) == max(i_nodes$color)){
        rgb_i <- colorRamp(c(nodes_color[[j]][1],nodes_color[[j]][2]))(i_nodes$color/10)} # Avoid dividing by 0 if min=max
      else{
        rgb_i <- colorRamp(c(nodes_color[[j]][1],nodes_color[[j]][2]))(
          (i_nodes$color - min(i_nodes$color))/(max(i_nodes$color) - min(i_nodes$color)))
      } # browser()
      i_colors <- rgb(rgb_i[, 1], rgb_i[, 2], rgb_i[, 3], maxColorValue = 255)}
    #cate if range is 0
    #       browser()
    i_nodes$color.background <- i_colors
    #       browser()
    nodes_combined <- rbind(nodes_combined, i_nodes)
    #browser()
    nodes_combined}

  net_nodes <- nodes_combined[order(nodes_combined$st_id),]
  drop_cols <- c("st_id","color")
  net_nodes <- net_nodes[,!names(net_nodes) %in% drop_cols]
  net_nodes}

if(!("com" %in% names(net_nodes))){
  if(is.null(nodes_color_max)){nodes_color_max <- max(net_nodes$color)}
  net_nodes <- vis_nodes_color_max(net_nodes = net_nodes,nodes_color_max = nodes_color_max)

  if(nodes_color_scaling == "fixed"){
    rgb_i <- colorRamp(c(nodes_color[[1]],nodes_color[[2]]))((net_nodes$color)/10)
    i_colors <- rgb(rgb_i[, 1], rgb_i[, 2], rgb_i[, 3], maxColorValue = 255)
    #  browser()
    net_nodes$color.background <- i_colors
    net_nodes}
  #browser()
  if(nodes_color_scaling == "scaled"){
    if(min(net_nodes$color) == max(net_nodes$color)){
      rgb_i <- colorRamp(c(nodes_color[[1]],nodes_color[[2]]))((net_nodes$color)/10)} #avoid dividing by 0
    else{rgb_i <- colorRamp(c(nodes_color[[1]],nodes_color[[2]]))(
      (net_nodes$color - min(net_nodes$color))/(max(net_nodes$color) - min(net_nodes$color)))
    }
    i_colors <- rgb(rgb_i[, 1], rgb_i[, 2], rgb_i[, 3], maxColorValue = 255)
    #  browser()
    net_nodes$color.background <- i_colors
    net_nodes}
  drop_cols <- "color"
  net_nodes <- net_nodes[,!names(net_nodes) %in% drop_cols]
  net_nodes}

}


#######################################################
#if(is.list(nodes_color) & (all(lengths(nodes_color) == 2))){
#
#    if(!("com" %in% names(net_nodes))){stop("nodes_community is not defined")}
#
#    #set node_color_max if not defined
#    if(is.null(nodes_color_max)){nodes_color_max <- max(net_nodes$color)}
#    else{if(is.numeric(nodes_color_max) == FALSE | 0 > nodes_color_max){
#            stop("nodes_color_max must be numeric and above 0")}}
#
#    net_nodes$st_id <- 1:nrow(net_nodes)
#    net_nodes <- vis_nodes_color_max(net_nodes = net_nodes,nodes_color_max = nodes_color_max)
#
#    nodes_combined <- data.frame()
#    unique_groups <- sort(unique(net_nodes$com))
#
#    #warnung hast du schon in nodes_color warnings geschrieben
#
#    for (i in unique(net_nodes$com)){
#         i_nodes <- net_nodes %>% filter(com == i)
#         j <- as.numeric(which(unique_groups == i))
#
#            if(nodes_color_scaling == "fixed"){
#               rgb_i <- colorRamp(c(nodes_color[[j]][1],nodes_color[[j]][2]))(i_nodes$color/10)
#
#               i_colors <- rgb(rgb_i[, 1], rgb_i[, 2], rgb_i[, 3], maxColorValue = 255)}
#            if(nodes_color_scaling == "scaled"){
#               if(min(i_nodes$color) == max(i_nodes$color)){
#                 rgb_i <- colorRamp(c(nodes_color[[j]][1],nodes_color[[j]][2]))(i_nodes$color/10)} # Avoid dividing by 0 if min=max
#              else{
#               rgb_i <- colorRamp(c(nodes_color[[j]][1],nodes_color[[j]][2]))(
#              (i_nodes$color - min(i_nodes$color))/(max(i_nodes$color) - min(i_nodes$color)))
#               } # browser()
#               i_colors <- rgb(rgb_i[, 1], rgb_i[, 2], rgb_i[, 3], maxColorValue = 255)}
#cate if range is 0
#  #       browser()
#        i_nodes$color.background <- i_colors
#  #       browser()
#         nodes_combined <- rbind(nodes_combined, i_nodes)
#         #browser()
#         nodes_combined}
#    #is_valid_color(nodes_combined$color)
#  #  browser()
#    net_nodes <- nodes_combined[order(nodes_combined$st_id),]
#    drop_cols <- c("st_id","color")
#    net_nodes <- net_nodes[,!names(net_nodes) %in% drop_cols]
#    net_nodes}
#
#if(is.list(nodes_color) & length(nodes_color) == 2 & (all(lengths(nodes_color) == 1))){
#  #set node_color_max if not defined
#  if(is.null(nodes_color_max)){nodes_color_max <- max(net_nodes$color)}
#  else{if(!is.numeric(nodes_color_max) | 0 > nodes_color_max){
#    stop("nodes_color_max must be numeric and above 0")}}
# # browser()
#  net_nodes <- vis_nodes_color_max(net_nodes = net_nodes,nodes_color_max = nodes_color_max)
# # browser()
#   if(nodes_color_scaling == "fixed"){
#      rgb_i <- colorRamp(c(nodes_color[[1]],nodes_color[[2]]))((net_nodes$color)/10)
#      i_colors <- rgb(rgb_i[, 1], rgb_i[, 2], rgb_i[, 3], maxColorValue = 255)
#    #  browser()
#      net_nodes$color.background <- i_colors
#      net_nodes}
#  #browser()
#   if(nodes_color_scaling == "scaled"){
#     if(min(net_nodes$color) == max(net_nodes$color)){
#       rgb_i <- colorRamp(c(nodes_color[[1]],nodes_color[[2]]))((net_nodes$color)/10)} #avoid dividing by 0
#     else{rgb_i <- colorRamp(c(nodes_color[[1]],nodes_color[[2]]))(
#      (net_nodes$color - min(net_nodes$color))/(max(net_nodes$color) - min(net_nodes$color)))
#         }
#      i_colors <- rgb(rgb_i[, 1], rgb_i[, 2], rgb_i[, 3], maxColorValue = 255)
#    #  browser()
#      net_nodes$color.background <- i_colors
#      net_nodes}
#  drop_cols <- "color"
#  net_nodes <- net_nodes[,!names(net_nodes) %in% drop_cols]
#net_nodes}
##browser()
#net_nodes}











