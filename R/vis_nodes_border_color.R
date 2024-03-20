#' Title
#'
#' @param nodes_border_color
#' @param nodes_border_color_max
#' @param nodes_border_color_scaling
#' @param net_nodes
#'
#' @return
#' @export
#'
#' @examples
vis_nodes_border_color <- function(net_nodes, nodes_border_color, nodes_border_color_max, nodes_border_color_scaling){

  if(is.list(nodes_border_color) & (all(lengths(nodes_border_color) == 2))){

    if(!("com" %in% names(net_nodes))){stop("nodes_community is not defined")}

    #set node_color_max if not defined
    if(is.null(nodes_border_color_max)){nodes_border_color_max <- max(net_nodes$border_color)}
    else{if(is.numeric(nodes_border_color_max) == FALSE | 0 > nodes_border_color_max){
      stop("nodes_border_color_max must be numeric and above 0")}}

    net_nodes$st_id <- 1:nrow(net_nodes)
    net_nodes <- vis_nodes_border_color_max(net_nodes = net_nodes,nodes_border_color_max = nodes_border_color_max)

    nodes_combined <- data.frame()
    unique_groups <- sort(unique(net_nodes$com))

    #warnung hast du schon in nodes_color warnings geschrieben

    for (i in unique(net_nodes$com)){
      i_nodes <- net_nodes %>% filter(com == i)
      j <- as.numeric(which(unique_groups == i))

      if(nodes_border_color_scaling == "fixed"){
        rgb_i <- colorRamp(c(nodes_border_color[[j]][1],nodes_border_color[[j]][2]))(i_nodes$border_color/10)

        i_colors <- rgb(rgb_i[, 1], rgb_i[, 2], rgb_i[, 3], maxColorValue = 255)}
      if(nodes_border_color_scaling == "scaled"){
        if(min(i_nodes$border_color) == max(i_nodes$border_color)){
          rgb_i <- colorRamp(c(nodes_border_color[[j]][1],nodes_border_color[[j]][2]))(i_nodes$border_color/10)} # Avoid dividing by 0 if min=max
        else{
          rgb_i <- colorRamp(c(nodes_border_color[[j]][1],nodes_border_color[[j]][2]))(
            (i_nodes$border_color - min(i_nodes$border_color))/(max(i_nodes$border_color) - min(i_nodes$border_color)))
        } # browser()
        i_colors <- rgb(rgb_i[, 1], rgb_i[, 2], rgb_i[, 3], maxColorValue = 255)}
      #cate if range is 0
      i_nodes$color.border <- i_colors
      nodes_combined <- rbind(nodes_combined, i_nodes)
      #browser()
      nodes_combined}
    #is_valid_color(nodes_combined$color)
    # browser()
    net_nodes <- nodes_combined[order(nodes_combined$st_id),]
    drop_cols <- c("st_id","border_color")
    net_nodes <- net_nodes[,!names(net_nodes) %in% drop_cols]
    net_nodes}

  if(is.list(nodes_border_color) & length(nodes_border_color) == 2 & (all(lengths(nodes_border_color) == 1))){
    #set node_color_max if not defined
    if(is.null(nodes_border_color_max)){nodes_border_color_max <- max(net_nodes$border_color)}
    else{if(!is.numeric(nodes_border_color_max) | 0 > nodes_border_color_max){
      stop("nodes_border_color_max must be numeric and above 0")}}

    net_nodes <- vis_nodes_border_color_max(net_nodes = net_nodes,nodes_border_color_max = nodes_border_color_max)

    if(nodes_border_color_scaling == "fixed"){
      rgb_i <- colorRamp(c(nodes_border_color[[1]],nodes_border_color[[2]]))((net_nodes$border_color)/10)
      i_colors <- rgb(rgb_i[, 1], rgb_i[, 2], rgb_i[, 3], maxColorValue = 255)
      net_nodes$color.border <- i_colors
      net_nodes}

    if(nodes_border_color_scaling == "scaled"){
      if(min(net_nodes$border_color) == max(net_nodes$border_color)){
        rgb_i <- colorRamp(c(nodes_border_color[[1]],nodes_border_color[[2]]))((net_nodes$border_color)/10)} #avoid dividing by 0
      else{rgb_i <- colorRamp(c(nodes_border_color[[1]],nodes_border_color[[2]]))(
        (net_nodes$border_color - min(net_nodes$border_color))/(max(net_nodes$border_color) - min(net_nodes$border_color)))
      }
      i_colors <- rgb(rgb_i[, 1], rgb_i[, 2], rgb_i[, 3], maxColorValue = 255)
      net_nodes$color.border <- i_colors
      net_nodes}
    drop_cols <- "border_color"
    net_nodes <- net_nodes[,!names(net_nodes) %in% drop_cols]
    net_nodes}

  net_nodes}




