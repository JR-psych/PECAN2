


#' Title
#' @title pecanVis
#'
#'
#'
#'
#'
#' @param nodes nodes data.frame. Must contain an "id" variable for the nodes
#' @param edges edges data.frame. Must contain a "from" , a "to"  and still a "width" variable. Plan is that width can also be an other variable soon.
#' @param regulation_by Should there be a regularization?
#'                      Interacts with the cut_off parameter.
#'
#'                      Can be "nodes","number" or "value". Default is "none"
#'
#'                      If nodes: Number of edges equals the number of nodes*cut_off
#'
#'                      If number: The cut_off parameter sets the  number of edges in the network
#'
#'                      If value: Edges under a specific cut_off are excluded from the network
#'
#'
#' @param cut_by  Only variable that still need to be specified with a the $ sign e.g. edges$with
#'                Thats gonna change soon.
#'
#'                The variable on the basis of which the number of edges is selected. E.g, Width or certainty
#'
#'
#' @param cut_off
#'                If regulation_by = nodes: Scaling factor on how many edges should be selected: E.g. if cut_off = 1.5 and there are 10 nodes
#'                                          the 15 strongest edges of the cut_by variable are selected
#'
#'                If regulation_by = number: Number of edges that should be in the network. E.g. if 10 the 10 strongest edges of the
#'                cut_by variable are selected
#'
#'                If regulation_by = value: Cut off value for the edges. Edges that have a cut_by vparameter under that value are excluded from the network
#'
#'
#'
#'
#' @param nodes_color Color of the nodes. Default is "red"
#' @param edges_color Color of the edges. Default  is "black". Can also be "default10" when edges have a width between 1 and 10. "default10"
#'                    is a combination of grey and black depending on the width of an edge.
#'
#' @param max10      The possible maximum of your width. Must be numeric.
#'                   When your width is not on the scale from 1-10 you can apply this function to scale the maximum of your width to 10.
#'
#'
#'
#' @param arrows     Where should the arrows be drawn? Can be "to","from","middle" or 'to;from'. Default is "to"
#' @param unweigthed Not yet implemented. If TRUE it will set default options for unweigthed networks
#' @param shadow
#'                   Should some edges have a shadow e.g. to indicate certainty of a specific edge?
#'                   A numeric vector indicating the cutoffs for the variable defined by "shadow_by". E.g. c(2,5) --> if an edges has a shadow_by value below or equal to 2 it won´t get a shadow
#'                   if it has a shadow_by value lower or equal to 5 it will get a shadow. If it has a value over 5 it will get a thicker shadow.
#'
#'
#'
#' @param shadow_color Must be a vector of two colors default is c("#87CEFA","#0000CD").Second color for the thicker edge
#' @param shadow_by Variable on which basis of the shadows are applied.
#'
#' @param use_ranking Should only edges with a certain rank be used? Must be the column which includes the ranks in edges dataframe. E.g, edges$rank
#' @param min_rank Minimum ranking value included egdes should have. e.g min_rank 3 only includes edges which are ranked 3 or below. Care with negative values Must be numeric
#' @param use_seed "Default is to random" If u want recreate a network with a certain seed you can set that seed here
#' @param c_width
#' @param c_height
#'
#' @return A visNetwork object.
#' @export
#'
#' @examples
#'
#' @import visNetwork
#' @import dplyr
#'
pecanVis <- function(nodes,edges,regulation_by = "none",cut_by = "none",cut_off = "none",nodes_color = "red",edges_color = "black",unweigthed,max10 = 10,arrows = "to",
                     shadow = "none",shadow_color = "default10",shadow_by = "none",use_ranking = "none",min_rank = "none", use_seed = "random", c_width = "100%", c_height = "100%"){



  ## Checking if all necessities are defined
  if(is.data.frame(edges) == FALSE){stop("edges must be a dataframe")}
  if(is.data.frame(nodes) == FALSE){stop("nodes must be a dataframe")}

  if("id" %in% colnames(nodes) == FALSE){stop("id column in nodes is missing")}

  if("from" %in% colnames(edges) == FALSE){stop("'from' column in edges is missing")}
  if("to" %in% colnames(edges) == FALSE){stop("'to' column in edges is missing")}
  if("width" %in% colnames(edges) == FALSE){stop("'width' column in edges is missing")}
  if(any(edges[,"width"] < 0)){stop("no negative values in 'width' allowed")}

  ## Write nodes and edges into a new object
  net_edges <- edges
  net_nodes <- nodes
  cut_by <- cut_by
  cut_off <- cut_off
  regulation_by <- regulation_by
  shadow <- shadow
  shadow_color <- shadow_color
  shadow_by <- shadow_by
  nodes_color <- nodes_color
  edges_color <- edges_color


  if(use_ranking != "none"){net_edges <- vis_Rank(net_edges = net_edges, use_ranking = use_ranking, min_rank = min_rank, vis.env = vis.env)}

  if(regulation_by != "none"){net_edges <- vis_regulation(net_edges = net_edges, net_nodes = net_nodes, regulation_by = regulation_by, cut_by = cut_by, cut_off = cut_off)}

  if(max10 != 10){net_edges <- vis_max_10(net_edges = net_edges, max10 = max10)}

  if(any(0 %in% net_edges$width)){net_edges <- filter_zero_edges(edges = net_edges)}

  net_nodes <- net_nodes %>% dplyr::mutate(color = nodes_color)

  net_edges <- vis_edge_color(net_edges = net_edges, edges_color = edges_color)

  if(shadow[1] != "none" & shadow_by[1] != "none"){net_edges <- vis_shadow(net_edges = net_edges, shadow = shadow, shadow_color = shadow_color,shadow_by = shadow_by)}

  ## zuordnung von vpn zu den grafiken

  if("p_id" %in% colnames(net_nodes)){p_id <- net_nodes$p_id[1]}

  #setting seed

  if(use_seed != "random"){if(is.numeric(use_seed)){s1 <- use_seed}
                           else{stop("use_seed must be numeric")}}
  else{s1 <- floor(runif(1,-1000000,1000000))}

  # name variable

  if("p_id" %in% colnames(net_nodes)){s1n <- paste((paste("network_id",p_id,"seed(", sep = "_")),s1,")", sep= "")}
  else {s1n <- paste((paste("network","seed(", sep = "_")),s1,")", sep= "")}

  #pecan_edges <<- net_edges %>% dplyr::mutate(used_seed = s1,
       #                               used_width = ifelse(width <= 4, width*1.25,ifelse(width <=6,width*1.5,width*2)))

  #pecan_nodes <<- net_nodes %>% dplyr::mutate(used_seed = s1)

   net_edges <- net_edges %>% dplyr::mutate(width_og = width)
   net_edges <- net_edges %>% dplyr::mutate(width = ifelse(width <= 4, width*1.25,ifelse(width <=6,width*1.5,width*2)))

 ### eingabe für visNetwork
  visNetwork::visNetwork(nodes = net_nodes, edges = net_edges, width = c_width, height = c_height) %>%
    visNodes(scaling =list(label= FALSE, min = 30,max =50),
             font = list(align = 'left', size = 30, bold = TRUE, strokeWidth = 0.3, strokeColor = "black", background = "white")) %>%
    visOptions(highlightNearest = TRUE,manipulation = list(enabled = TRUE, editEdgeCols = c("arrows","color","width","label"),
                                                           editNodeCols = c("id","label","color","value"))) %>%
    visInteraction(selectable= TRUE) %>%
    visEdges(arrowStrikethrough = FALSE,arrows = arrows) %>%
    #visEdges(color = list(color = edges$color, opacity = 0.1)) %>%
    visLayout(improvedLayout = TRUE, randomSeed = s1)%>%
    visPhysics(solver = "barnesHut", barnesHut = list(nodeDistance = 400, avoidOverlap = 1, springConstant=0.001)) %>%
    #visLegend(addEdges = ledges, width = 0.15) %>%
    visExport(type = "png", name = s1n,
              float = "left", label = "Save network")

  }



