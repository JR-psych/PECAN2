


#' Title
#' @title pecanVis
#'
#'
#'
#'
#'
#'
#'
#' @param nodes A data frame containing the nodes values. Must have at least a column named “id” to
#' uniquely identify each node (see https://datastorm-open.github.io/visNetwork/nodes.html).
#' @param edges A data frame containing the edge values. Each row represents an edge and
#' must have at least two columns named "from" and "to" to specify the source and target nodes
#' of each edge (see https://datastorm-open.github.io/visNetwork/edges.html).
#' @param nodes_size Specifies the column name, number, or numeric vector on which the node
#' size should be based.
#' This parameter determines the size of nodes in the plot. By default, it is set to  28.
#'  If a "size" column is specified, the values will be rescaled so that the maximum node
#'  size is 33. Alternatively, if the input is a number or numeric vector, the size of the
#'  nodes will be equal to that vector.
#' @param nodes_size_max A number which specifies the maximum possible value of the
#' “size” column if it is detected or specified. This parameter is useful, for example,
#'  if size is based on severity (ranging from 1 to 100), but no participant reported the
#'   maximum value of 100. If nodes_size_max is set to 100, then the rescaling will be based
#'    on that value rather than the maximum input. If this parameter is not defined, it will be
#'     set to the maximum of the input values. Default is NULL.
#' @param nodes_color_by Specifies whether nodes should be colored based on values of a
#' specific column. If this parameter is set, it should be the name of the column in the
#'  dataset to base the node colors on. By default, this parameter is set to NULL, indicating
#'  that no specific column is selected for coloring nodes.
#' @param nodes_color Specifies the colors of the nodes. If nodes_color_by is defined,
#'  it must be a list containing the desired color spectrum.
#'  For example: list("red", "darkred"). If communities are defined (see nodes_community),
#'   the list needs to contain one vector for each community, with two colors specified for
#'   each community. For example: list(c("red", "darkred"), c("lightblue", "blue")).
#'   If nodes_color_by is not defined, it can take one of the following forms:
#'   The column name containing the colors or a single color specified as a string.
#'   For example: "red" (default)
#' @param nodes_color_max A number which specifies the maximum possible value of the
#' “nodes_color_by” column if it is detected or specified. This parameter is useful,
#' for example, if color is based on severity (ranging from 1 to 100), but no participant
#' reported the maximum value of 100. If nodes_color_max is set to 100, then the rescaling
#' will be based on that value rather than the maximum input. If this parameter is not defined,
#'  it will be set to the maximum of the input values.
#' @param nodes_color_scaling Specifies how the distance between the coloring of the values
#'  should be defined. It can take one of two values: "fixed": The distance between the colors
#'   is evenly spread from 0 to the maximum value. "scaled": The distance between the colors is
#'    scaled from the minimum to the maximum value, enabling clearer coloring separation within
#'     the network. By default, this parameter is set to "scaled".
#' @param nodes_community Specifies the name of the column which specifies the communities for
#' each node. This parameter allows users to define communities within the network by specifying
#'  the column name containing the community information for each node. By default, this
#'   parameter is set to NULL.
#' @param nodes_hide_isolated Specifies whether nodes which are not connected to other nodes
#' should be hidden in the network. This parameter is a logical value. When set to TRUE, nodes
#'  without any connections to other nodes will be hidden in the network visualization.
#'  By default, this parameter is set to FALSE.
#' @param nodes_label Specifies the column name containing the labels for nodes. This parameter
#' allows users to define labels for nodes based on a specified column in the dataset.
#' By default, this parameter is set to NULL

#' @param edges_color_by Specifies whether edges should be colored based on values of a
#' specific column. If this parameter is set, it should be the name of the column in the
#' dataset to base the edge colors on. By default, this parameter is set to NULL.
#' @param edges_color Specifies the colors of the (positive) edges. If edges_color_by is
#' defined, it must be a vector containing the desired color spectrum.
#' For example: c("lightgrey", "black"). If edges_color_by is not defined, it can take one
#'  of the following forms: The column name containing the colors or a single color specified
#'  as a string. For example: "black" (default)
#' @param neg_edges_color Specifies the colors of the (negative) edges, which is only applied
#' if edges_color_by is defined. This parameter can either be: One color specified as a string.
#'  For example:        "blue" or a color spectrum specified as a vector.
#'  For example: c("lightblue", "blue").
#' @param edges_color_scaling Specifies how the distance between the coloring of the
#' values should be defined. It can take one of two values: "fixed": The distance between
#' the colors is evenly spread from 0 to the maximum value. "scaled": The distance between
#' the colors is scaled from the minimum to the maximum value, enabling clearer coloring
#' separation within the network. By default, this parameter is set to "scaled".
#' @param edges_color_max Specifies the maximum possible value of the “edges_color_by” column
#' if it is specified. This parameter is useful, for example, if color is based on edge
#'  strength (ranging from 1 to 10), but no participant reported the maximum value of 10.
#'  If edges_color_max is set to 10, then the rescaling will be based on that value rather
#'  than the maximum input. If this parameter is not defined, it will be set to the maximum
#'  of the input values.
#' @param edges_regulation Specifies whether edges should be regulated. It requires a list
#' with the following elements: “reg_by”,”reg_type”,”reg_value”. reg_by: The name of the
#' column used to base the regulation on. reg_type: The type of regulation, which can be one
#' of the following: (1) "nodes": The number of edges in the network equals the number of
#'  nodes. In this case, the parameter reg_value defines how scaled this should be.
#'  For example, if reg_type is set to "nodes" and reg_value is set to 1.5 in a network of
#'   10 nodes, it allows for 15 edges. (2) "number": The maximum number of edges equals the
#'   number of edges defined by reg_value. For example, if reg_type is set to "number" and
#'   reg_value is set to 12, it allows a maximum of 12 edges. (3) "value": Edges weaker than
#'   a value defined by reg_value are included in the network. For example, if reg_type is
#'   set to "value" and reg_value is set to 5, only edges with a value of 5 and higher are
#'   included. Only absolute values are used in reg_by. By default this parameter is set to
#'   NULL
#' @param edges_width Specifies the width of an edge. This parameter can be: A numeric value
#' indicating the width of the edge. A column name from which the edge width should be based.
#'  A numeric vector specifying different widths for each edge. If the input is a column name,
#'   values are rescaled so that the maximum width is 10. By default, the width is set to 5.
#' @param edges_width_max Specifies the maximum possible value of the "edges" column if it is
#'  specified. This parameter is useful, for example, if the width is based on edge strength
#'   (ranging from 1 to 100), but no participant reported the maximum value of 100. If
#'   edges_width_max is set to 100, then the rescaling will be based on that value rather than the
#'    maximum input. If this parameter is not defined, it will be set to the maximum of the
#'    input values.
#' @param edges_arrows Specifies the direction of the arrows for the edges. This parameter can
#' take one of the following values: "from": Arrows point from the source nodes to the target
#' nodes. "to": Arrows point from the source nodes to the target nodes. "middle": Arrows are
#' placed in the middle of the edges."none": No arrows are displayed. "auto": The edges
#' dataframe must contain a column named "arrows" which provides the arrow information for
#' each edge. Can also be a combination of “from”, “to” and “middle” By default, the arrows
#' are set to "to".

#' @param edges_smooth This parameter must be either NULL or a list containing curve properties
#'  of edges (See https://visjs.github.io/vis-network/docs/network/edges.html)

#' @param use_seed Specifies the method for placing nodes. This parameter can be set to:
#' "random": Nodes are placed randomly. A numeric value: Specifies the seed for random
#' placement, ensuring the same network layout for reproducibility. By default, this parameter
#'  is set to "random".
#' @param c_width Width of the canvas in percentage or pixel.
#' (see https://visjs.github.io/vis-network/docs/network/edges.html)
#' @param c_height Height of the canvas in percentage or pixel.
#' (see https://visjs.github.io/vis-network/docs/network/edges.html)
#' @param edit Specifies whether the visNetwork manipulation module should be enabled.
#' This parameter is a logical value. When set to TRUE, the manipulation module is enabled.
#'  By default, this parameter is set to TRUE.
#'  See (https://visjs.github.io/vis-network/docs/network/manipulation.html#)
#' @param export Specifies whether the visNetwork export module for PNGs should be enabled.
#' This parameter is a logical value. When set to TRUE, the export module for PNGs is enabled,
#'  allowing users to export the network visualization as PNG images. By default, this
#'  parameter is set to TRUE
#' @param p_id Specifies whether a participant ID should be included in the name of the
#' exported picture. This parameter can be either a character or a number. If specified,
#' the participant ID will be included in the name of the exported picture. By default,
#' this parameter is set to NULL, indicating that no participant ID will be included.
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

                      nodes_size = 28, #done
                      nodes_size_max = NULL, # done?
                      nodes_color_by = NULL,  # done?
                      nodes_color = "red", #color, color column name or list # done
                      nodes_color_max = NULL, # Done
                      nodes_color_scaling = "scaled",
                      nodes_community = NULL, #  or column name

                      nodes_hide_isolated = FALSE, # done

                      nodes_label = NULL, # done

                      edges_color = "black",   # spezial Fall
                      neg_edges_color = NULL, # spezial Fall
                      edges_color_scaling = "scaled", # spezial Fall
                      edges_color_by = NULL, # das musst du noch erstellen!
                      edges_color_max = NULL,

                      edges_regulation = NULL, # done # has to be NULL or a list
                      edges_width = 5, #  column name
                      edges_width_max = NULL, # done?
                      edges_arrows = "to",
                      edges_smooth = TRUE,


                      use_seed = "random", c_width = 1200, c_height = 750,
                      edit = TRUE, export = TRUE, p_id = NULL
){



  nodes <- nodes
  edges <- edges
  nodes_size <- nodes_size
  nodes_size_max  <- nodes_size_max
  nodes_color_by  <- nodes_color_by
  nodes_color <-  nodes_color
  nodes_color_max <- nodes_color_max
  nodes_color_scaling <- nodes_color_scaling
  nodes_community <- nodes_community
  nodes_hide_isolated <- nodes_hide_isolated
  nodes_label <- nodes_label

  edges_color <- edges_color
  neg_edges_color <- neg_edges_color
  edges_color_scaling <- edges_color_scaling
  edges_color_by <- edges_color_by
  edges_color_max <- edges_color_max

  edges_regulation <- edges_regulation
  edges_width <- edges_width
  edges_width_max <- edges_width_max
  edges_arrows <- edges_arrows
  edges_smooth <- edges_smooth

  p_vis_list <- pecanPrepare(nodes = nodes,
                             edges = edges,
                             nodes_size = nodes_size,
                             nodes_size_max = nodes_size_max,
                             nodes_color_by = nodes_color_by,
                             nodes_color =  nodes_color,
                             nodes_color_max = nodes_color_max,
                             nodes_color_scaling = nodes_color_scaling,
                             nodes_community = nodes_community,

                             nodes_hide_isolated =  nodes_hide_isolated,

                             nodes_label = nodes_label,

                             edges_color = edges_color,
                             neg_edges_color = neg_edges_color,
                             edges_color_scaling = edges_color_scaling,
                             edges_color_by = edges_color_by,
                             edges_color_max = edges_color_max,

                             edges_regulation = edges_regulation,
                             edges_width = edges_width,
                             edges_width_max = edges_width_max,
                             edges_arrows = edges_arrows,
                             edges_smooth = edges_smooth)






  if(edit == TRUE){manipulation_list <- list(enabled = TRUE, editEdgeCols = c("arrows","color","width","label"),#color.color
                                             editNodeCols = c("id","label","color","size",))}#"color.background","color.border","boderWidth"

  if(edit == FALSE){manipulation_list <- list(enabled = FALSE)}


    #setting seed
  if(use_seed != "random"){if(is.numeric(use_seed)){s1 <- use_seed}
    else{stop("use_seed must be numeric")}}
  if(use_seed == "random"){s1 <- floor(runif(1,-1000000,1000000))}

  # name variable

  if(!is.null(p_id)){s1n <- paste((paste("network_id",p_id,"seed(", sep = "_")),s1,")", sep= "")}
  else{s1n <- paste((paste("network","seed(", sep = "_")),s1,")", sep= "")}


  ### eingabe für visNetwork
  if(export){nw <- visNetwork::visNetwork(nodes = p_vis_list$nodes, edges = p_vis_list$edges, width = c_width, height = c_height) %>%
    visNodes(chosen = FALSE) %>%
    visOptions(highlightNearest = TRUE,manipulation = manipulation_list) %>%
    visInteraction(selectable= TRUE) %>%
    visEdges(selfReference = list(size = 20, angle = pi/4, renderBehindTheNode = TRUE),    #arrows = arrows,
             chosen = FALSE) %>%
    #visEdges(color = list(color = edges$color, opacity = 0.1)) %>%
    visLayout(improvedLayout = TRUE, randomSeed = s1)%>%
    visPhysics(solver = "barnesHut", barnesHut = list(nodeDistance = 400, avoidOverlap = 1, springConstant=0.001)) %>%

    visExport(type = "png", name = s1n,
              float = "left", label = "Save network")}



  if(!export){nw <- visNetwork::visNetwork(nodes = p_vis_list$nodes, edges = p_vis_list$edges, width = c_width, height = c_height) %>%
    visNodes(chosen = FALSE) %>%
    visOptions(highlightNearest = TRUE,manipulation = manipulation_list) %>%
    visInteraction(selectable= TRUE) %>%
    visEdges(selfReference = list(size = 20, angle = pi/4, renderBehindTheNode = TRUE),    #arrows = arrows,
             chosen = FALSE) %>%
    #visEdges(color = list(color = edges$color, opacity = 0.1)) %>%
    visLayout(improvedLayout = TRUE, randomSeed = s1)%>%
    visPhysics(solver = "barnesHut", barnesHut = list(nodeDistance = 400, avoidOverlap = 1, springConstant=0.001))}



  nw}
