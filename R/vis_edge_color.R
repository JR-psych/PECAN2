
#' Title vis_edge_color
#'
#' @param edges_color
#' @param neg_edges_color
#' @param edge_color_scalig
#' @param net_edges
#'
#' @import dplyr
#' @import grDevices
#' @return
#'
#' @examples
vis_edge_color <- function(net_edges, edges_color, neg_edges_color, edge_color_scalig){

if(any(net_edges$neg_color == 1)){
      net_edges$st_id <- 1:nrow(net_edges)

      pos_edges <- net_edges[net_edges$neg_color == 0, ]
      neg_edges <- net_edges[net_edges$neg_color == 1, ]

      if(length(edges_color) == 2){
                       if(edge_color_scalig == "fixed"){
                          rgb_pos <- colorRamp(c(edges_color[1],edges_color[2]))((pos_edges$color)/10)
                          #browser()
                          pos_colors <- rgb(rgb_pos[, 1], rgb_pos[, 2], rgb_pos[, 3], maxColorValue = 255)}
                      if(edge_color_scalig == "scaled"){
                          rgb_pos <- colorRamp(c(edges_color[1],edges_color[2]))(
                                    (pos_edges$color - min(pos_edges$color))/(max(pos_edges$color) - min(pos_edges$color)))
                          pos_colors <- rgb(rgb_pos[, 1], rgb_pos[, 2], rgb_pos[, 3], maxColorValue = 255) }}

      if(length(edges_color) == 1){pos_colors <- edges_color}

      if(length(neg_edges_color) == 2){
      if(edge_color_scalig == "fixed"){
         rgb_neg <- colorRamp(c(neg_edges_color[1],neg_edges_color[2]))((neg_edges$color)/10)
         neg_colors <- rgb(rgb_neg[, 1], rgb_neg[, 2], rgb_neg[, 3], maxColorValue = 255)}
      if(edge_color_scalig == "scaled"){
         rgb_neg <- colorRamp(c(neg_edges_color[1],neg_edges_color[2]))(
                          (neg_edges$color - min(neg_edges$color))/(max(neg_edges$color) - min(neg_edges$color)))
         neg_colors <- rgb(rgb_neg[, 1], rgb_neg[, 2], rgb_neg[, 3], maxColorValue = 255)}}



      if(length(neg_edges_color) == 1){neg_colors <- neg_edges_color}

  pos_edges$color.color <- pos_colors
  neg_edges$color.color <- neg_colors

  net_edges <- rbind(pos_edges,neg_edges)
 # browser()
  net_edges <- net_edges[order(net_edges$st_id),]
  net_edges <- net_edges %>% dplyr::select(-st_id)
  net_edges}

else{
      if(length(edges_color) == 2){
            if(edge_color_scalig == "fixed"){
               rgb_pos <- colorRamp(c(edges_color[1],edges_color[2]))((net_edges$color)/10)
               browser()
               pos_colors <- rgb(rgb_pos[, 1], rgb_pos[, 2], rgb_pos[, 3], maxColorValue = 255)}
            if(edge_color_scalig == "scaled"){
               if(min(net_edges$color) == max(net_edges$color)){
                 rgb_pos <- colorRamp(c(edges_color[1],edges_color[2]))((net_edges$color)/10)} #Avoid dividing by 0
               else {rgb_pos <- colorRamp(c(edges_color[1],edges_color[2]))(
               (net_edges$color - min(net_edges$color))/(max(net_edges$color) - min(net_edges$color)))}
                pos_colors <- rgb(rgb_pos[, 1], rgb_pos[, 2], rgb_pos[, 3], maxColorValue = 255)}}

      if(length(edges_color) == 1){pos_colors <- edges_color}
  net_edges$color.color <- pos_colors
  net_edges}
  color_drop <- c("color","neg_color")
  net_edges <- net_edges[,!names(net_edges) %in% color_drop]
net_edges}



























