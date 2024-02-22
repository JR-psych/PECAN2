


#' Title
#' @title pecanVis
#'
#'
#'
#'
#'
#' @param nodes nodes data.frame. Must contain an "id" variable for the nodes
#' @param edges edges data.frame. Must contain a "from" , a "to"  and still a "width" variable. Plan is that width can also be an other variable soon.
#' @param nodes_color Color of the nodes. Default is "red"
#' @param edges_color Color of the edges. Default  is "black". Can also be "default10" when edges have a width between 1 and 10. "default10"
#'                    is a combination of grey and black depending on the width of an edge.
#'
#' @param use_seed "Default is to random" If u want recreate a network with a certain seed you can set that seed here
#' @param c_width
#' @param c_height
#' @param neg_edges_color
#' @param nodes_color_by
#' @param nodes_color_max
#' @param nodes_color_scaling
#' @param edges_width_max
#' @param nodes_label
#' @param nodes_border_color
#' @param nodes_font
#' @param edges_width
#' @param edges_arrows
#' @param edges_dashes
#' @param edges_label
#' @param edges_font
#' @param edges_arrowsScaleFactor
#' @param edges_color.inherit
#' @param edges_selfReference
#' @param edges_smooth
#' @param edges_widthConstraint
#' @param edges_arrowStrikethrough
#' @param edges_endPointOffset
#' @param edit
#' @param export
#' @param auto_detect
#' @param p_id
#' @param edges_color_scaling
#' @param edges_color_by
#' @param edges_color_max
#' @param nodes_size
#' @param nodes_size_max
#' @param nodes_community
#' @param nodes_hide_isolated
#' @param nodes_border_color_by
#' @param nodes_border_color_max
#' @param nodes_border_width
#' @param nodes_border_color_use_com
#' @param nodes_hidden
#' @param edges_regulation
#' @param edges_hidden
#'
#' @return A visNetwork object.
#' @export
#'
#' @examples
#'
#' @import visNetwork
#' @import dplyr
#'
pecanVis <- function(nodes,edges,

                     nodes_size = "auto", #done?
                     nodes_size_max = "auto", # done?
                     nodes_color = "red", #color, color column name or list # done
                     nodes_color_by = NULL, # done?
                     nodes_color_max = NULL, # Done
                     nodes_color_scaling = "scaled",
                     nodes_community = NULL, # Auto or column name

                     nodes_hide_isolated = FALSE, # done

                     nodes_label = "auto", # done
                     nodes_border_color = NULL, # done
                     nodes_border_color_by = NULL, #done
                     nodes_border_color_max = NULL, # done
                     nodes_border_width = NULL, # numerisch # done
                     nodes_border_color_use_com = TRUE, # logical should communities be used if there
                     nodes_font = NULL, # done
                     nodes_hidden = NULL,#done


                     edges_color = "black",   # spezial Fall
                     neg_edges_color = "mediumblue", # spezial Fall
                     edges_color_scaling = "fixed", # spezial Fall
                     edges_color_by = NULL, # das musst du noch erstellen!
                     edges_color_max = NULL,



                     edges_regulation = NULL, # done # has to be NULL or a list


                     edges_width = "auto", # Auto or column name
                     edges_width_max = "auto", # done?

                     edges_arrows = "to", # auto

                     # no auto
                     edges_dashes = NULL, # auto

                     edges_hidden = "auto", # auto
                     edges_label = "auto", # auto

                     edges_font = NULL, # auto
                     edges_arrowsScaleFactor = 1,
                     edges_color.inherit = FALSE, # kein auto
                     edges_selfReference = NULL, # kein auto
                     edges_smooth = TRUE, # kein auto
                     edges_widthConstraint = FALSE, # kein auto
                     edges_arrowStrikethrough = FALSE, # kein auto
                     edges_endPointOffset = NULL, # kein auto

                     use_seed = "random", c_width = "100%", c_height = "100%",
                     edit = TRUE, export = TRUE,auto_detect = TRUE, p_id = NULL){

  options(warn = 2)
  color_inherit <- FALSE
  size_set_auto <- FALSE
  width_set_auto <- FALSE

  net_edges <- edges[,c("from","to"), drop = FALSE]
  net_edges <- as.data.frame(net_edges)

  # If width is numeric every edge gets the number
  if(is.numeric(edges_width)){net_edges$width <- edges$width
                              width_set_auto <- TRUE}

  # if its not numeric we first check whether its set to auto and auto detect is on
  # If so we try to find a width column in the dataframe

  if(!is.numeric(edges_width)){
    if(edges_width == "auto" & auto_detect){
       if("width" %in% names(edges)){net_edges$width <- abs(edges$width)}
       else{net_edges$width <- 5
            width_set_auto <- TRUE
            warning("'width' column in edges is not defined. Width was set to 5")}}

    if(edges_width != "auto"){
       if(edges_width %in% names(edges)){net_edges$width <- abs(edges[,edges_width])}
       else{warning(stop("edges_width column not in edges"))}
    }}


  if(!is.null(edges_regulation)){
    regulation_warnings(edges_regulation = edges_regulation,edges = edges)
    reg_by <- edges_regulation$reg_by
    net_edges$internal_reg <- abs(edges[,reg_by])
  }

  if(edges_color.inherit == TRUE | edges_color.inherit == "from" | edges_color.inherit == "to" | edges_color.inherit == "both"){
    net_edges$color.inherit <- edges_color.inherit
    color_inherit <- TRUE
    }

  # If edge color is not inherit and edge_color_by is defined we get this column
  if(edges_color.inherit == FALSE){
    if(!is.null(edges_color_by)){
      if(edges_color_by == "auto"){
        if("color_by" %in% names(edges)){net_edges$color <- edges$color_by}
        else{stop("color_by column not in edges")}}

      if(edges_color_by != "auto"){
        if(edges_color_by %in% names(edges)){net_edges$color <- edges[,edges_color_by]}
        else{stop("color_by column not in edges")}}}

    if(is.null(edges_color_by)){
       if(is_valid_color(edges_color)){net_edges$color.color <- edges_color
                                       } #edges_color_is_set <- TRUE
       else{   if(edges_color == "auto"){
                 if("color" %in% names(edges)){net_edges$color.color <- edges$color
                                               } #edges_color_is_set <- TRUE
                 else{stop("edges_color is either not a valid color or not in edges names")}}
               if(edges_color != "auto"){
                  if(edges_color %in% names(edges)){net_edges$color.color <- edges[,edges_color]
                                                                          # edges_color_is_set <- TRUE
                                                   if(!is_valid_color(net_edges$color.color)){stop(
                                                     "color columns consists of non valid colors")}}
                  else{stop("edges_color is either not a valid color or not in edges names")}}}}}




  if(edges_label == "auto" & auto_detect){
    if("label" %in% names(edges)){net_edges$label <- edges$label}}

  if(edges_label != "auto"){
    if(edges_label %in% names(edges)){net_edges$label <- edges[,edges_label]}
    else{stop("edges_label not in edges")}
  }

# Das ist noch komisch
  if(!is.null(edges_font)){
    if(!is.list(edges_font)){stop("edges_font must be NULL or a list")}}
  if(is.null(edges_font)){edges_font <- FALSE}

if(!is.null(edges_dashes)){
  if(edges_dashes == "auto" & auto_detect){
    if("dashes" %in% names(edges)){
      net_edges$dashes <- edges$dashes
    }
  }

 if(edges_dashes == TRUE){
  net_edges$dashes <- TRUE
 }

 # Brauchen wir das eigentlich?
 if(edges_dashes == FALSE){
    net_edges$dashes <- FALSE
  }}


  arrows_from <- grepl("from", edges_arrows)
  arrows_to <- grepl("to", edges_arrows)
  arrows_middle <- grepl("middle", edges_arrows)

  if(any(arrows_from,arrows_to,arrows_middle)){
    # Create a character containing the present patterns separated by "_"
    present_patterns <- character()
    if (arrows_from){present_patterns <- c(present_patterns, "from")}
    if (arrows_to){present_patterns <- c(present_patterns, "to")}
    if (arrows_middle){present_patterns <- c(present_patterns, "middle")}

    arrow_result <- paste(present_patterns, collapse = "_")
    net_edges$arrows <- arrow_result
      }

if(edges_arrows == "none"){net_edges$arrows <- FALSE}
if(edges_arrows == "auto" & auto_detect){
    if("arrows" %in% names(edges)){net_edges$arrows <- edges$arrows}
  }

 if(!is.numeric(edges_arrowsScaleFactor)){stop("edges_arrowsScaleFactor must be numeric")}




 if(edges_hidden == "auto" & auto_detect){if("hidden" %in% names(edges)){net_edges$hidden <- edges$hidden
                                                                         if(!is.logical(net_edges$hidden)){
                                                                           stop("edges_hidden is not logical")}}}
 if(edges_hidden != "auto" & edges_hidden != TRUE & edges_hidden != FALSE){
   if(edges_hidden %in% names(edges)){net_edges$hidden <- edges[,edges_hidden]
                                     if(!is.logical(net_edges$hidden)){stop("edges_hidden is not logical")}}
  else{stop("edges_hidden not in edges")}}
 #if(edges_hidden == TRUE){net_edges$hidden <- TRUE}
 #if(edges_hidden == FALSE){net_edges$hidden <- FALSE}


### Hier müssen noch wanrings rein
 if(is.null(edges_selfReference)){edges_selfReference <- list(size = 15)}
 if(!is.null(edges_selfReference)){if(!is.list(edges_selfReference)){stop("edges_selfReference must be NULL or a list")}}

 if(!is.logical(edges_smooth)){if(!is.list(edges_smooth)){stop("edges_smooth must be logical or list")}}

 if(!is.logical(edges_widthConstraint) & !is.numeric(edges_widthConstraint)){
   stop("edges_widthConstraint must be logical or numeric")}

 if(!is.logical(edges_arrowStrikethrough)){stop("edges_arrowStrikethrough must be logial")}

 if(!is.null(edges_endPointOffset)){if(!is.list(edges_endPointOffset)){stop("edges_endPointOffset must be NULL or list")}}
 if(is.null(edges_endPointOffset)){edges_endPointOffset <- list(from = 0, to = 10)}



net_nodes <- nodes[,c("id"), drop = FALSE]
net_nodes <- as.data.frame(net_nodes)

   if(is.numeric(nodes_size)){net_nodes$size <- nodes$size
   size_set_auto <- TRUE}

   # if its not numeric we first check whether its set to auto and auto detect is on
   # If so we try to find a width column in the dataframe

   if(!is.numeric(nodes_size)){
     if(nodes_size == "auto" & auto_detect){
       if("size" %in% names(nodes)){net_nodes$size <- abs(nodes$size)}
       else{net_nodes$size <- 28
       size_set_auto <- TRUE
       warning("'Size' column in nodes is not defined. Size was set to 28")}}

     if(nodes_size != "auto"){
       if(nodes_size %in% names(nodes)){net_nodes$size <- abs(nodes[,nodes_size])}
       else{warning(stop("nodes_size column not in nodes"))}
     }}



     if(!is.null(nodes_color_by)){
       if(nodes_color_by == "auto"){
         if("color_by" %in% names(nodes)){net_nodes$color <- abs(nodes$color_by)}
         else{stop("color_by column not in nodes")}}

       if(nodes_color_by != "auto"){
         if(nodes_color_by %in% names(nodes)){net_nodes$color <- abs(nodes[,nodes_color_by])}
         else{stop("color_by column not in nodes")}}}

     if(is.null(nodes_color_by)){
       if(is_valid_color(nodes_color)){net_nodes$color.background <- nodes_color
                                       }    #nodes_color_is_set <- TRUE
       else{   if(nodes_color == "auto"){
         if("color" %in% names(nodes)){net_nodes$color.background <- nodes$color
                                       } #nodes_color_is_set <- TRUE
         else{stop("nodes_color is either not a valid color or not in node names")}}
         if(nodes_color != "auto"){
           if(nodes_color %in% names(nodes)){net_nodes$color.background <- nodes[,nodes_color]
                                                          # nodes_color_is_set <- TRUE
           if(!is_valid_color(net_nodes$color.background)){stop(
             "nodes color columns consists of non valid colors")}}
           else{stop("nodes_color is either not a valid color or not in nodes names")}}}}

if(!is.null(nodes_community)){
  if(nodes_community == "auto"){
    if("community" %in% names(nodes)){net_nodes$com <- nodes$community}
    else{stop("community column not in nodes")}}

  if(nodes_community != "auto"){
    if(nodes_community %in% names(nodes)){net_nodes$com <- nodes[,nodes_community]}
    else{stop("community column not in nodes")}}}




if(!is.null(nodes_border_width)){
  if(nodes_border_width %in% names(nodes)){
    net_nodes$borderWidth <- nodes[,nodes_border_width]
    if(!is.numeric(net_nodes$borderWidth)){stop("nodes_border_width column is not numeric")}}
  else{if(is.numeric(nodes_border_width)){net_nodes$borderWidth <- nodes_border_width}
       else{stop("nodes_border width must be a numeric column or numeric")}}
  }



   if(!is.null(nodes_border_color_by)){
     if(nodes_border_color_by == "auto"){
       if("border_color_by" %in% names(nodes)){net_nodes$border_color <- nodes$border_color_by}
       else{stop("border_color_by column not in nodes")}}

     if(nodes_border_color_by != "auto"){
       if(nodes_border_color_by %in% names(nodes)){net_nodes$border_color <- nodes[,nodes_border_color_by]}
       else{stop("border_color_by column not in nodes")}}}

   if(is.null(nodes_border_color_by)){
     if(is_valid_color(nodes_border_color)){net_nodes$color.border <- nodes_border_color
                                            } #nodes_border_color_is_set <- TRUE
     else{   if(nodes_border_color == "auto"){
       if("border_color" %in% names(nodes)){net_nodes$color.border <- nodes$border_color
                                            } #nodes_border_color_is_set <- TRUE
       else{stop("nodes_border_color is either not a valid color or not in node names")}}
       if(nodes_border_color != "auto"){
         if(nodes_border_color %in% names(nodes)){net_nodes$color.border <- nodes[,nodes_border_color]
                                               # nodes_border_color_is_set <- TRUE
         if(!is_valid_color(net_nodes$color.border)){stop(
           "nodes border color columns consists of non valid colors")}}
         else{stop("nodes_border_color is either not a valid color or not in nodes names")}}}}


   if(nodes_label == "auto" & auto_detect){
     if("label" %in% names(nodes)){net_nodes$label <- nodes$label}}

   if(nodes_label != "auto"){
     if(nodes_label %in% names(nodes)){net_nodes$label <- nodes[,nodes_label]}
     else{stop("nodes_label not in nodes")}
   }





   if(!is.null(nodes_font)){
     if(!is.list(nodes_font)){stop("edges_font must be NULL or a list")}}

   if(is.null(nodes_font)){nodes_font <- list(align = 'left', size = 30, bold = TRUE,
                                              strokeWidth = 0.3, strokeColor = "black", background = "white")

   }

if(!is.null(nodes_hidden)){
   if(auto_detect & nodes_hidden == "auto"){
      if("hidden" %in% names(nodes)){
        net_nodes$hidden <- nodes$hidden
                         if(!is.logical(net_nodes$hidden)){stop("nodes_hidden is not logical")}
                         if(any(is.na(net_nodes$hidden))){stop("No Na´s allowed in nodes_hidden")}}}


    if(nodes_hidden != "auto" & nodes_hidden != TRUE & nodes_hidden != FALSE){
     if(nodes_hidden %in% names(nodes)){net_nodes$hidden <- nodes[,nodes_hidden]
     if(any(is.na(net_nodes$hidden))){stop("No Na´s allowed in nodes_hidden")}
     if(!is.logical(net_nodes$hidden)){stop("nodes_hidden is not logical")}}
     else{stop("nodes_hidden not in edges")}}
   if(nodes_hidden == TRUE){net_nodes$hidden <- TRUE}
   if(nodes_hidden == FALSE){net_nodes$hidden <- FALSE}}


   if(edit){manipulation_list <- list(enabled = TRUE, editEdgeCols = c("arrows","color.color","width","label"),
                                          editNodeCols = c("id","label","color.background","color.border","size","boderWidth"))}

   if(!edit){manipulation_list <- list(enabled = FALSE)}




  nodes_general_warnings(net_nodes = net_nodes)
  edges_general_warnings(net_edges = net_edges)



  if(!size_set_auto){net_nodes <- size_max(net_nodes = net_nodes, nodes_size_max = nodes_size_max)}

  nodes_color_warnings(net_nodes = net_nodes, nodes_color = nodes_color, nodes_color_max = nodes_color_max,
                       nodes_color_scaling = nodes_color_scaling, nodes_color_by = nodes_color_by,
                       nodes_community = nodes_community)  # Check dependencies fpr nodes color   ADD Waring that the clomun internal_color should not exist?

  if(!is.null(nodes_color_by)){
  net_nodes <- vis_node_color(net_nodes = net_nodes, nodes_color = nodes_color,
                              nodes_color_max = nodes_color_max,nodes_color_scaling = nodes_color_scaling)}


  if(!is.null(nodes_border_color_by)){
    net_nodes <- vis_node_border_color(net_nodes = net_nodes, nodes_border_color = nodes_border_color,
                                       nodes_border_color_max = nodes_border_color_max,nodes_border_color_scaling = nodes_border_color_scaling)
  }

  if(!("color.border" %in% names(net_nodes))){net_nodes$color.border <- net_nodes$color.background}

  if("color" %in% names(net_edges)){net_edges$neg_color <- ifelse(net_edges$color < 0, 1, 0)
                                    net_edges$color <- abs(net_edges$color)}

  if(any(0 %in% net_edges$width)){net_edges <- filter_zero_edges(edges = net_edges)}
  if(!is.null(edges_regulation)){net_edges <- vis_regulation(net_edges = net_edges, net_nodes = net_nodes, edges_regulation = edges_regulation)}
  if(!width_set_auto){net_edges <- width_max(net_edges = net_edges, edges_width_max = edges_width_max)}

  if(edges_color.inherit == FALSE & !is.null(edges_color_by)){
  # Hier nochmal überlegen
  edge_color_warnings(edges_color = edges_color, neg_edges_color = neg_edges_color)
  net_edges <- e_color_max(net_edges, edges_color_max)
  net_edges <- vis_edge_color(net_edges = net_edges, edges_color = edges_color, edges_color_by,
                              neg_edges_color = neg_edges_color, edge_color_scalig = edge_color_scaling)}

  # replaced further on top net_nodes <- net_nodes %>% dplyr::mutate(color = nodes_color)

  ## zuordnung von vpn zu den grafiken



  #setting seed
  if(use_seed != "random"){if(is.numeric(use_seed)){s1 <- use_seed}
                            else{stop("use_seed must be numeric")}}
  if(use_seed == "random"){s1 <- floor(runif(1,-1000000,1000000))}

  # name variable

  if(!is.null(p_id)){s1n <- paste((paste("network_id",p_id,"seed(", sep = "_")),s1,")", sep= "")}
  else {s1n <- paste((paste("network","seed(", sep = "_")),s1,")", sep= "")}

  #pecan_edges <<- net_edges %>% dplyr::mutate(used_seed = s1,
  #                               used_width = ifelse(width <= 4, width*1.25,ifelse(width <=6,width*1.5,width*2)))

  #pecan_nodes <<- net_nodes %>% dplyr::mutate(used_seed = s1)
  if(nodes_hide_isolated == TRUE){
    net_nodes$isolated <- as.logical(!(net_nodes$id %in% c(net_edges$from, net_edges$to)))
    if(any(TRUE %in% net_nodes$isolated)){warning("Isolated nodes are hidden in the network")}

    net_nodes$hidden <- net_nodes$hidden + net_nodes$isolated
    net_nodes$hidden <- ifelse(net_nodes$hidden >= 1,TRUE,FALSE)
  }


#  net_edges <- net_edges %>% dplyr::mutate(width = ifelse(width <= 4, width*1.25,ifelse(width <=6,width*1.5,width*2)))

  ### eingabe für visNetwork
  if(export){nw <- visNetwork::visNetwork(nodes = net_nodes, edges = net_edges, width = c_width, height = c_height) %>%
    visNodes(scaling =list(label= FALSE, min = 30,max =50),
             font = nodes_font,
             chosen = FALSE) %>%
    visOptions(highlightNearest = TRUE,manipulation = list(enabled = TRUE, editEdgeCols = c("arrows","color","width","label"),
                                                           editNodeCols = c("id","label","color","size"))) %>%
    visInteraction(selectable= TRUE) %>%
    visEdges(arrowStrikethrough = edges_arrowStrikethrough,
             arrows = list(to = list(scaleFactor = edges_arrowsScaleFactor),
                           from = list(scaleFactor = edges_arrowsScaleFactor),
                           middle = list(scaleFactor = edges_arrowsScaleFactor)),
             selfReference = edges_selfReference,    #arrows = arrows,
             smooth = edges_smooth,
             widthConstraint = edges_widthConstraint,
             endPointOffset = edges_endPointOffset,
             font = edges_font,
             color = list(inherit = edges_color.inherit),
             chosen = FALSE) %>%
    #visEdges(color = list(color = edges$color, opacity = 0.1)) %>%
    visLayout(improvedLayout = TRUE, randomSeed = s1)%>%
    visPhysics(solver = "barnesHut", barnesHut = list(nodeDistance = 400, avoidOverlap = 1, springConstant=0.001)) %>%
    #visLegend(addEdges = ledges, width = 0.15) %>%
    visExport(type = "png", name = s1n,
              float = "left", label = "Save network")}



  if(!export){nw <- visNetwork::visNetwork(nodes = net_nodes, edges = net_edges, width = c_width, height = c_height) %>%
    visNodes(scaling =list(label= FALSE, min = 30,max =50),
             font = nodes_font,
             chosen = FALSE) %>%
    visOptions(highlightNearest = TRUE,manipulation = list(enabled = TRUE, editEdgeCols = c("arrows","color","width","label"),
                                                           editNodeCols = c("id","label","color","size"))) %>%
    visInteraction(selectable= TRUE) %>%
    visEdges(arrowStrikethrough = edges_arrowStrikethrough,
             arrows = list(to = list(scaleFactor = edges_arrowsScaleFactor),
                           from = list(scaleFactor = edges_arrowsScaleFactor),
                           middle = list(scaleFactor = edges_arrowsScaleFactor)),
             selfReference = edges_selfReference,    #arrows = arrows,
             smooth = edges_smooth,
             widthConstraint = edges_widthConstraint,
             endPointOffset = edges_endPointOffset,
             font = edges_font,
             color = list(inherit = edges_color.inherit),
             chosen = FALSE) %>%
    #visEdges(color = list(color = edges$color, opacity = 0.1)) %>%
    visLayout(improvedLayout = TRUE, randomSeed = s1)%>%
    visPhysics(solver = "barnesHut", barnesHut = list(nodeDistance = 400, avoidOverlap = 1, springConstant=0.001))}



nw}
