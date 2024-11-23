
#' Title
#' @description A function to aggregate PECAN-data (e.g over persons or timepoints). 0 values
#' are treated as NA values as in most studies 0 indicates the absence of a node or edge. Edges with no corresponding
#' nodes (both have to be there) are dropped and a warning is displayed.
#'
#'
#' @param data This parameter should be a dataframe where each row represents a person or timepoint, and the columns contain
#' specific values for the nodes and edges corresponding to that person or timepoint.
#' @param nodes Specifies the positions of nodes columns in the dataframe. E.g. 1:10
#' @param edges Specifies the positions of edges columns in the dataframe. E.g. 11:100.  Each edge should be represented by a
#' combination of node names separated by a specified separator. For example, if an edge connects nodes "pain" and "insomnia",
#' it could be represented as "pain_insomnia" in the dataframe.
#' @param edges_sep Specifies the separator for the edges in the dataframe. E.g. “_”
#' @param nodes_attribute Specifies the positions of columns containing a second node attribute. E.g. 11:20. This could be additional
#' information collected alongside the main attribute (e.g., frequency) for each node. For example, if data is collected on both
#' frequency and severity for each node, these columns could contain the severity values.
#' @param attribute_pattern Specifies the pattern used in the attributes columns. This parameter should be a character specifying the
#' pattern used in the column names of the attributes to assign them to the nodes. The column names of the attributes must be constructed
#' by combining the names of the nodes with this specified pattern. For example, if the pattern is ".freq", and the nodes are named "pain"
#' and "insomnia", the attribute columns should be named "pain.freq" and "insomnia.freq".
#' @param drop_nodes Specifies the threshold for excluding nodes not present in a certain proportion of the population. This parameter should
#' be a number between 0 and 1, indicating the proportion of the population below which nodes should be excluded. For example, if set to 0.2,
#' nodes that are not present in at least 20% of the population will be excluded from the analysis.
#' @param drop_nodes_att Specifies the threshold for excluding nodes based on whether the attribute is not present in a certain proportion of
#' the population. This parameter should be a number between 0 and 1, indicating the proportion of the population below which nodes should be
#' excluded if the attribute is not present for them. For example, if set to 0.2, nodes for which the attribute is not present in at least 20%
#' of the population will be excluded from the analysis.
#' @param absV Should absolute values be used to aggregate? Default is FALSE
#' @export
#' @return A list with the following components:
#' \describe{
#'     \item{agg_nodes}{A dataframe with follwoing columns:
#'       \describe{
#'        \item{nodes}{Node names}
#'         \item{per}{Percent of timepoint/person for wich there was a non-NA value}
#'         \item{mean_oa}{The mean if Na´s are treated as 0}
#'         \item{sd_oa}{The sd if Na´s are treated as 0}
#'         \item{mean}{The mean if the node was present (na.rm = TRUE)}
#'         \item{sd}{The sd if the node was present (na.rm = TRUE)}
#'         \item{median_oa}{The median if Na´s are treated as 0}
#'         \item{iqr_oa}{The iqr if Na´s are treated as 0}
#'         \item{median}{The median if the node was present (na.rm = TRUE)}
#'         \item{iqr}{The iqr if the node was present (na.rm = TRUE)}
#'       }
#'     }
#'     \item{agg_edges}{
#'     \describe{
#'         \item{edges}{Edge names}
#'         \item{per_oa}{Percentage of persons/timepoints who reported the edges}
#'         \item{per_ch}{Percentage of persons/timepoints for which the edge could potentially exist (both nodes were present).}
#'         \item{per_ip}{Percentage of individuals/timepoints reporting the presence of the edge when both nodes were present.}
#'         \item{mean_oa}{The mean if Na´s are treated as 0}
#'         \item{sd_oa}{The sd if Na´s are treated as 0}
#'         \item{mean}{The mean if the edge was present (na.rm = TRUE)}
#'         \item{sd}{The sd if the edge was present (na.rm = TRUE)}
#'         \item{mean_ip}{The mean value across all individuals/timepoints (Na treated as 0) where the edge could potentially exist (both nodes were present)}
#'         \item{sd_ip}{The sd value across all individuals/timepoints (Na treated as 0) where the edge could potentially exist (both nodes were present))}
#'         \item{median_oa}{The median if Na´s are treated as 0}
#'         \item{iqr_oa}{The iqr  if Na´s are treated as 0}
#'         \item{median}{The median if the edge was present (na.rm = TRUE)}
#'         \item{iqr}{The iqr if the edge was present (na.rm = TRUE)}
#'         \item{median_ip}{The median value across all individuals/timepoints (Na treated as 0) where the edge could potentially exist (both nodes were present)}
#'         \item{iqr_ip}{The iqr value across all individuals/timepoints (Na treated as 0) where the edge could potentially exist (both nodes were present)}

#'       }}
#'     \item{agg_info}{
#'     \describe{
#'     \item{drop_nodes}{Function input}
#'     \item{drop_nodes_att}{Function input}
#'     \item{nNodes}{Final number  of nodes in agg_nodes}
#'     \item{nNodes_og}{Original number of nodes}
#'     \item{nEdges}{Final number  of edges in agg_edges}
#'     \item{nEdges_og}{Original number of edges}
#'     \item{edges_sep}{Function input}
#'     \item{attribute_pattern}{Function input}}
#'   }}
#'
#'
#'

pecanAggregate <- function(data, nodes = NULL,edges = NULL,
                           edges_sep = NULL, nodes_attribute = NULL,
                           drop_nodes = NULL, drop_nodes_att = NULL, attribute_pattern = NULL, absV = FALSE){


if(!is.data.frame(data)){stop("data mus be a data.frame")}
data <- as.data.frame(data)

if(is.null(edges_sep)){stop("Please set edges_sep")}
edges_sep <- as.character(edges_sep)





if(as.numeric(nrow(data)) < 2){stop("data needs at least two rows")}
if(is.null(nodes_attribute)){attribute_pattern <- NULL}

agg_data <- data

agg_nodes <- data[,nodes, drop = FALSE]
agg_edges <- data[,edges, drop = FALSE]

absolute_values <- FALSE
if(absV == TRUE){agg_nodes <- abs(agg_nodes)
                 agg_edges <- abs(agg_edges)
                 absolute_values <- TRUE}




nNodes_og <- as.numeric(base::ncol(agg_nodes))

if(!is.null(nodes_attribute)){agg_att <- agg_data[,nodes_attribute,drop = FALSE]
                              agg_att <- setNames(agg_att, gsub(attribute_pattern, "", colnames(agg_att)))
                              if(absV == TRUE){agg_att <- abs(agg_att)}
                              if(ncol(agg_nodes) != ncol(agg_att)){
                                warning("Nodes and Nodes attributes are not of the same length")}}

nEdges_og <- as.numeric(base::ncol(agg_edges))


if(any(agg_nodes == 0, na.rm = TRUE)){
  agg_nodes[agg_nodes == 0] <- NA
                          warning("0 were found in nodes and were treated as missing")}

if(any(agg_edges == 0, na.rm = TRUE)){agg_edges[agg_edges == 0] <- NA
                          warning("0 were found in edges and were treated as missing")}

if(!is.null(nodes_attribute)){
  if(any(agg_att == 0, na.rm = TRUE)){agg_att[agg_att == 0] <- NA
                          warning("0 were found in nodes attribute and were treated as missing")}}


if(is.null(drop_nodes) == FALSE & is.null(drop_nodes_att)){
  if(is.numeric(drop_nodes) == FALSE){stop("drop_nodes must be numeric or NULL")}
  if((drop_nodes < 1 & drop_nodes > 0) == FALSE){stop("drop nodes must be between 0 and 1")}

  not_na <- colMeans(!is.na(agg_nodes))### to get the percentages?

  keep_nodes <- names(not_na)[not_na >= drop_nodes]

  agg_nodes <- agg_nodes[,keep_nodes,drop = FALSE]

  if(!is.null(nodes_attribute)){agg_att <- agg_att[,keep_nodes, drop = FALSE]
                              #  colnames(agg_att) <- paste0(colnames(agg_att),attribute_pattern) bruachen wir vllt nicht
                                }

                                                          }

if(is.null(drop_nodes) & is.null(drop_nodes_att) == FALSE){
  if(is.numeric(drop_nodes_att) == FALSE){stop("drop_nodes_att must be numeric or NULL")}
  if((drop_nodes_att < 1 & drop_nodes_att > 0) == FALSE){stop("drop_nodes_att must be between 0 and 1")}

  not_na <- colMeans(!is.na(agg_att))### to get the percentages?
  keep_nodes <- names(not_na)[not_na >= drop_nodes_att]
  agg_att <- agg_att[,keep_nodes, drop = FALSE]
  agg_nodes <- agg_nodes[,keep_nodes, drop = FALSE]
}

if(is.null(drop_nodes) == FALSE & is.null(drop_nodes_att) == FALSE){
  if(is.numeric(drop_nodes_att) == FALSE){stop("drop_nodes_att must be numeric or NULL")}
  if((drop_nodes_att < 1 & drop_nodes_att > 0) == FALSE){stop("drop_nodes_att must be between 0 and 1")}
  if(is.numeric(drop_nodes) == FALSE){stop("drop_nodes must be numeric or NULL")}
  if((drop_nodes < 1 & drop_nodes > 0) == FALSE){stop("drop nodes must be between 0 and 1")}
   not_na_nodes <- colMeans(!is.na(agg_nodes))### to get the percentages?
   keep_nodes <- names(not_na_nodes)[not_na_nodes >= drop_nodes]## das nuss noch verbessert werden, Muss das nicht auch irgendwie für nodes 2 gelten?

   not_na_att <- colMeans(!is.na(agg_att))### to get the percentages?
   keep_nodes_att <- names(not_na_att)[not_na_att >= drop_nodes_att]

   keep_combined <- base::intersect(keep_nodes,keep_nodes_att)

   agg_nodes <- agg_nodes[,keep_combined, drop = FALSE]
   agg_att <- agg_att[,keep_combined, drop = FALSE]
}

node_names <- colnames(agg_nodes)
edge_names <- colnames(agg_edges)
keep_edges <- NULL

for (i in seq_along(edge_names)) {
  nodes_in_edge <- unlist(strsplit(as.character(edge_names[i]), edges_sep))
  if(nodes_in_edge[1] %in% node_names & nodes_in_edge[2] %in% node_names){
    keep_edges <- cbind(keep_edges,edge_names[i])}
    }
keep_edges <- as.vector(keep_edges)
agg_edges <- agg_edges[,keep_edges, drop = FALSE]

if(nEdges_og != as.numeric(base::ncol(agg_edges))){
  warning("Edges have been dropped as no reference node was found")}
## Some information would be good like how many edges have been dropped etc

## We also need to drop edges that are not in the nodes anymore ...

#####################  Aggregate edges

# How many people had the edge overall?
edges_percentage_oa <- agg_edges
edges_percentage_oa[!is.na(edges_percentage_oa)] <- 1
edges_per_oa <- data.frame(per_oa = colSums(edges_percentage_oa, na.rm = TRUE))
#edges_per_oa <- as.data.frame(t(edges_per_oa))
edges_per_oa <- edges_per_oa / as.numeric(nrow(agg_edges))
edges_per_oa$edges <- rownames(edges_per_oa)
rownames(edges_per_oa) <- NULL


#How strong was it overall if Na´s were treated as zeros
###Mean + SD
agg_edges_na_as_0 <- agg_edges
agg_edges_na_as_0[is.na(agg_edges_na_as_0)] <- 0
edges_mean_na_as_0 <- data.frame(mean_na_as_0 = colMeans(agg_edges_na_as_0, na.rm = TRUE))
#edges_mean_na_as_0  <- as.data.frame(t(edges_mean_na_as_0 ))
edges_mean_na_as_0$edges <- rownames(edges_mean_na_as_0)
rownames(edges_mean_na_as_0) <- NULL

edges_sd_na_as_0 <- data.frame(sd_na_as_0 = apply(agg_edges_na_as_0, 2, sd, na.rm = TRUE))
edges_sd_na_as_0$edges <- rownames(edges_sd_na_as_0)
rownames(edges_sd_na_as_0) <- NULL
#edges_sd_na_as_0 <- as.data.frame(t(edges_sd_na_as_0))

###Median + IQR
edges_median_na_as_0 <- data.frame(median_na_as_0 = apply(agg_edges_na_as_0,2, median, na.rm = TRUE))
#edges_median_na_as_0  <- as.data.frame(t(edges_median_na_as_0 ))
edges_median_na_as_0$edges <- rownames(edges_median_na_as_0)
rownames(edges_median_na_as_0) <- NULL

edges_iqr_na_as_0 <- data.frame(iqr_na_as_0  = apply(agg_edges_na_as_0, 2, IQR, na.rm = TRUE))
edges_iqr_na_as_0$edges <- rownames(edges_iqr_na_as_0)
rownames(edges_iqr_na_as_0) <- NULL
#edges_iqr_na_as_0 <- as.data.frame(t(edges_iqr_na_as_0))}

#How strong was it if it was present
###Mean + SD
edges_mean <- data.frame(mean = colMeans(agg_edges, na.rm = TRUE))
edges_mean$edges <- rownames(edges_mean)
rownames(edges_mean) <- NULL

#edges_mean <- as.data.frame(t(edges_mean))
# Check if calculating SD is possible
sd_col_0 <- NULL

for (col in names(agg_edges)) {
  if (sum(!is.na(agg_edges[[col]])) == 1) {
    warning(paste("sd cannot be calculated for edge '", col, "'. Only one non-NA value. Therefore
                  sd has been set to 0"))
    sd_col_0 <- c(sd_col_0,col)
    }
}

edges_sd <- data.frame(sd = apply(agg_edges, 2, sd, na.rm = TRUE))

#edges_sd[is.na(edges_sd)] <- 0
edges_sd$edges <- rownames(edges_sd)
rownames(edges_sd) <- NULL
if(!is.null(sd_col_0)){edges_sd$sd[which(edges_sd$edges %in% sd_col_0)] <- 0}
#edges_sd <- as.data.frame(t(edges_sd))
###Median + IQR
edges_median <- data.frame(median = apply(agg_edges, 2, median, na.rm = TRUE))
edges_median$edges <- rownames(edges_median)
rownames(edges_median) <- NULL

#edges_median <- as.data.frame(t(edges_median))
edges_iqr <- data.frame(iqr = apply(agg_edges, 2, IQR, na.rm = TRUE))
edges_iqr$edges <- rownames(edges_iqr)
rownames(edges_iqr) <- NULL
#edges_iqr <- as.data.frame(t(edges_iqr))

# How many percent could have it?
#Preperation
agg_edges_0 <- agg_edges
agg_edges_0[is.na(agg_edges_0)] <- 0 # Ja das macht Sinn! # Das ist ja für
edge_names <- colnames(agg_edges)

## Create a logical matrix indicating presence of nodes and edges
node_presence_matrix <- !is.na(agg_nodes)
edge_presence_matrix <- agg_edges_0
edge_presence_matrix[,] <- TRUE
# Update the edge present matrix based on node presence
for (i in seq_along(edge_names)) {
  nodes_in_edge <- unlist(strsplit(as.character(edge_names[i]), edges_sep))
  edge_presence_matrix[, i] <- rowSums(node_presence_matrix[, nodes_in_edge]) == 2
}
# Es wird die matrix durch eine summe rwereitert. Wenn beide Knotenpunkt da sind kommt ein TRUE und wenn nicht dann kommt ein false
#  basiert darauf, dass true + true = 2 sind und true + fasle = 1 , und true + na = NA

#browser()
edge_presence_matrix <- as.matrix(edge_presence_matrix)
agg_edges_0 <- agg_edges_0 * ifelse(edge_presence_matrix, 1, NA)
# it works because 0*Na is NA.


# How many percent could have it?
edges_per_ch <- data.frame(per_ch = colMeans(edge_presence_matrix))
edges_per_ch$edges <- rownames(edges_per_ch)
rownames(edges_per_ch) <- NULL
#edges_per_ch <- as.data.frame(t(edges_per_ch))

# How many percent of that actually had it?
agg_edges_percentage <- replace(agg_edges_0, agg_edges_0 != 0 & !is.na(agg_edges_0), 1)
edges_per_ip <- data.frame(per_ip = colMeans(agg_edges_percentage,na.rm = TRUE))
edges_per_ip$edges <- rownames(edges_per_ip)
rownames(edges_per_ip) <- NULL
#edges_per_ip <- as.data.frame(t(edges_per_ip))

# Mean if possibility
edges_mean_ip <- data.frame(mean_ip = colMeans(agg_edges_0, na.rm = TRUE))
edges_mean_ip$edges <- rownames(edges_mean_ip)
rownames(edges_mean_ip) <- NULL

# Check if calculating SD is possible
sd_col_0_ip <- NULL
for (col in names(agg_edges_0)) {
  if (sum(!is.na(agg_edges_0[[col]])) == 1) {
    warning(paste("sd_ip cannot be calculated for edge '", col, "'. Only one non-NA value. Therefore
                  sd_ip has been set to 0"))
    sd_col_0_ip <- c(sd_col_0_ip,col)
    }
}

#edges_mean_ip <- as.data.frame(t(edges_mean_ip))
edges_sd_ip <- data.frame(sd_ip = apply(agg_edges_0, 2, sd, na.rm = TRUE))
edges_sd_ip$edges <- rownames(edges_sd_ip)
rownames(edges_sd_ip) <- NULL
if(!is.null(sd_col_0_ip)){edges_sd_ip$sd_ip[which(edges_sd_ip$edges %in% sd_col_0_ip)] <- 0}
#edges_sd_ip <- as.data.frame(t(edges_sd_ip))}


edges_median_ip <- data.frame(median_ip = apply(agg_edges_0, 2, median, na.rm = TRUE))
edges_median_ip$edges <- rownames(edges_median_ip)
rownames(edges_median_ip) <- NULL
#edges_median_ip <- as.data.frame(t(edges_median_ip))
edges_iqr_ip <- data.frame(iqr_ip = apply(agg_edges_0, 2, IQR, na.rm = TRUE))
edges_iqr_ip$edges <- rownames(edges_iqr_ip)
rownames(edges_iqr_ip) <- NULL
#edges_iqr_ip <- as.data.frame(t(edges_iqr_ip))


net_edges <- Reduce(function(x, y) merge(x, y, by = "edges", all = TRUE), list(edges_per_oa,
                                                                               edges_per_ch,
                                                                               edges_per_ip,
                                                                               edges_mean_na_as_0,
                                                                               edges_sd_na_as_0,
                                                                               edges_mean,
                                                                               edges_sd,
                                                                               edges_mean_ip,
                                                                               edges_sd_ip,
                                                                               edges_median_na_as_0,
                                                                               edges_iqr_na_as_0,
                                                                               edges_median,
                                                                               edges_iqr,
                                                                               edges_median_ip,
                                                                               edges_iqr_ip))



names(net_edges)[names(net_edges) == "mean_na_as_0"] <- "mean_oa"
names(net_edges)[names(net_edges) == "sd_na_as_0"] <- "sd_oa"
names(net_edges)[names(net_edges) == "median_na_as_0"] <- "median_oa"
names(net_edges)[names(net_edges) == "iqr_na_as_0"] <- "iqr_oa"
# Aggregate the nodes

n_nodes <- as.numeric(base::ncol(agg_nodes))
if(!is.null(nodes_attribute)){
  colnames(agg_att) <- paste0(colnames(agg_att),attribute_pattern)
  att_names <- colnames(agg_att)
  node_names <- colnames(agg_nodes)
  agg_nodes <- cbind(agg_nodes,agg_att)}

agg_nodes_percentage <- agg_nodes
agg_nodes_percentage[!is.na(agg_nodes_percentage)] <- 1
agg_nodes_per <- data.frame(per = colSums(agg_nodes_percentage, na.rm = TRUE))
agg_nodes_per <- agg_nodes_per/nrow(agg_nodes)
agg_nodes_per$nodes <- rownames(agg_nodes_per)

rownames(agg_nodes_per) <- NULL

agg_nodes_mean <- data.frame(mean = colMeans(agg_nodes, na.rm = TRUE))
agg_nodes_mean$nodes <- rownames(agg_nodes_mean)
rownames(agg_nodes_mean) <- NULL

# Check if calculating SD is possible
nodes_sd_col_0 <- NULL

for (col in names(agg_nodes)) {
  if (sum(!is.na(agg_nodes[[col]])) == 1) {
    warning(paste("sd cannot be calculated for node '", col, "'. Only one non-NA value. Therefore
                  sd has been set to 0"))
    nodes_sd_col_0 <- c(nodes_sd_col_0,col)
  }
}

#agg_nodes_mean <- as.data.frame(t(agg_nodes_center))
agg_nodes_sd <- data.frame(sd = apply(agg_nodes, 2, sd, na.rm = TRUE))
agg_nodes_sd$nodes <- rownames(agg_nodes_sd)
rownames(agg_nodes_sd) <- NULL
if(!is.null(nodes_sd_col_0)){agg_nodes_sd$sd[which(agg_nodes_sd$nodes %in% nodes_sd_col_0)] <- 0}
#agg_nodes_sd <- as.data.frame(t(agg_nodes_spread))

agg_nodes_median <- data.frame(median = apply(agg_nodes, 2, median, na.rm = TRUE))
agg_nodes_median$nodes <- rownames(agg_nodes_median)
rownames(agg_nodes_median) <- NULL

#agg_nodes_center <- as.data.frame(t(agg_nodes_center))
agg_nodes_iqr <- data.frame(iqr = apply(agg_nodes, 2, IQR, na.rm = TRUE))
agg_nodes_iqr$nodes <- rownames(agg_nodes_iqr)
rownames(agg_nodes_iqr) <- NULL
#agg_nodes_spread <- as.data.frame(t(agg_nodes_spread))}




agg_nodes_0 <- agg_nodes
agg_nodes_0[is.na(agg_nodes_0)] <- 0


agg_nodes_mean_oa <- data.frame(mean_oa = colMeans(agg_nodes_0, na.rm = TRUE))
agg_nodes_mean_oa$nodes <- rownames(agg_nodes_mean_oa)
rownames(agg_nodes_mean_oa) <- NULL

#agg_nodes_mean <- as.data.frame(t(agg_nodes_center))
nodes_sd_col_oa <- NULL

for (col in names(agg_nodes_0)) {
  if (sum(!is.na(agg_nodes_0[[col]])) == 1) {
    warning(paste("sd cannot be calculated for node '", col, "'. Only one non-NA value. Therefore
                  sd has been set to 0"))
    nodes_sd_col_oa <- c(nodes_sd_col_oa,col)
  }
}



agg_nodes_sd_oa <- data.frame(sd_oa = apply(agg_nodes_0, 2, sd, na.rm = TRUE))
agg_nodes_sd_oa$nodes <- rownames(agg_nodes_sd_oa)
rownames(agg_nodes_sd_oa) <- NULL
if(!is.null(nodes_sd_col_oa)){agg_nodes_sd_oa$sd[which(agg_nodes_sd_oa$nodes %in% nodes_sd_col_oa)] <- 0}
#agg_nodes_sd <- as.data.frame(t(agg_nodes_spread))

agg_nodes_median_oa <- data.frame(median_oa = apply(agg_nodes_0, 2, median, na.rm = TRUE))
agg_nodes_median_oa$nodes <- rownames(agg_nodes_median_oa)
rownames(agg_nodes_median_oa) <- NULL

#agg_nodes_center <- as.data.frame(t(agg_nodes_center))
agg_nodes_iqr_oa <- data.frame(iqr_oa = apply(agg_nodes_0, 2, IQR, na.rm = TRUE))
agg_nodes_iqr_oa$nodes <- rownames(agg_nodes_iqr_oa)
rownames(agg_nodes_iqr_oa) <- NULL



agg_info <- list(drop_nodes = drop_nodes,
                 drop_nodes_att = drop_nodes_att,
                 nNodes = n_nodes,
                 nNodes_og = nNodes_og,
                 nEdges = as.numeric(base::ncol(agg_edges)),
                 nEdges_og = nEdges_og,
                 edges_sep = edges_sep,
                 attribute_pattern = attribute_pattern,
                 absolute_values = absolute_values)

# Split nodes and attribute again because

if(!is.null(nodes_attribute)){
agg_att_per <- agg_nodes_per[agg_nodes_per$nodes == att_names,, drop = FALSE]
agg_att_mean_oa <- agg_nodes_mean_oa[agg_nodes_mean_oa$nodes == att_names,, drop = FALSE]
agg_att_sd_oa <- agg_nodes_sd_oa[agg_nodes_sd_oa$nodes == att_names,, drop = FALSE]
agg_att_mean <- agg_nodes_mean[agg_nodes_mean$nodes == att_names,, drop = FALSE]
agg_att_sd <- agg_nodes_sd[agg_nodes_sd$nodes == att_names,, drop = FALSE]
agg_att_median_oa <- agg_nodes_median_oa[agg_nodes_median_oa$nodes == att_names,, drop = FALSE]
agg_att_iqr_oa <- agg_nodes_iqr_oa[agg_nodes_iqr_oa$nodes == att_names,, drop = FALSE]
agg_att_median <- agg_nodes_median[agg_nodes_median$nodes == att_names,, drop = FALSE]
agg_att_iqr <- agg_nodes_iqr[agg_nodes_iqr$nodes == att_names,, drop = FALSE]

agg_nodes_per <- agg_nodes_per[agg_nodes_per$nodes == node_names,, drop = FALSE]
agg_nodes_mean_oa <- agg_nodes_mean_oa[agg_nodes_mean_oa$nodes == node_names,, drop = FALSE]
agg_nodes_sd_oa <- agg_nodes_sd_oa[agg_nodes_sd_oa$nodes == node_names,, drop = FALSE]
agg_nodes_mean <- agg_nodes_mean[agg_nodes_mean$nodes == node_names,, drop = FALSE]
agg_nodes_sd <- agg_nodes_sd[agg_nodes_sd$nodes == node_names,, drop = FALSE]
agg_nodes_median_oa <- agg_nodes_median_oa[agg_nodes_median_oa$nodes == node_names,, drop = FALSE]
agg_nodes_iqr_oa <- agg_nodes_iqr_oa[agg_nodes_iqr_oa$nodes == node_names,, drop = FALSE]
agg_nodes_median <- agg_nodes_median[agg_nodes_median$nodes == node_names,, drop = FALSE]
agg_nodes_iqr <- agg_nodes_iqr[agg_nodes_iqr$nodes == node_names,, drop = FALSE]

net_nodes <- Reduce(function(x, y) merge(x, y, by = "nodes", all = TRUE), list(agg_nodes_per,
                                                                               agg_nodes_mean_oa,
                                                                               agg_nodes_sd_oa,
                                                                               agg_nodes_mean,
                                                                               agg_nodes_sd,
                                                                               agg_nodes_median_oa,
                                                                               agg_nodes_iqr_oa,
                                                                               agg_nodes_median,
                                                                               agg_nodes_iqr))


net_att <- Reduce(function(x, y) merge(x, y, by = "nodes", all = TRUE), list(agg_att_per,
                                                                             agg_att_mean_oa,
                                                                             agg_att_sd_oa,
                                                                             agg_att_mean,
                                                                             agg_att_sd,
                                                                             agg_att_median_oa,
                                                                             agg_att_iqr_oa,
                                                                             agg_att_median,
                                                                             agg_att_iqr))

aggregated_data <- list(nodes = net_nodes,
                        attributes = net_att,
                        edges = net_edges,
                        agg_info = agg_info)}


if(is.null(nodes_attribute)){
net_nodes <- Reduce(function(x, y) merge(x, y, by = "nodes", all = TRUE), list(agg_nodes_per,
                                                                               agg_nodes_mean_oa,
                                                                               agg_nodes_sd_oa,
                                                                               agg_nodes_mean,
                                                                               agg_nodes_sd,
                                                                               agg_nodes_median_oa,
                                                                               agg_nodes_iqr_oa,
                                                                               agg_nodes_median,
                                                                               agg_nodes_iqr))




aggregated_data <- list(nodes = net_nodes,
                        edges = net_edges,
                        agg_info = agg_info)}

class(aggregated_data) <- "pecanAgg"
return(aggregated_data)}


