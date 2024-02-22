
#' Title
#'
#' @param data
#' @param nodes
#' @param edges
#' @param nodes_attribute
#' @param drop_nodes
#' @param drop_nodes_att
#' @param attribute_pattern
#' @param edges_sep
#'
#' @return
#' @export
#'
#' @examples
pecanAggregate <- function(data, nodes = NULL,edges = NULL, nodes_attribute = NULL,
                           drop_nodes = NULL, drop_nodes_att = NULL, attribute_pattern = NULL,
                           edges_sep = NULL){ # DO we need zero as NA for edges?


if(!is.data.frame(data)){stop("data mus be a data.frame")}
data <- as.data.frame(data)

if(is.null(edges_sep)){stop("Please set edges_sep")}
edges_sep <- as.character(edges_sep)
#if(!(nodes_center == "mean" | nodes_center == "median")){stop("Center must be mean or median")}


#You should check how many columns there are

if(as.numeric(nrow(data)) < 2){stop("data needs at least two rows")}
if(is.null(nodes_attribute)){attribute_pattern <- NULL}

agg_data <- data

agg_nodes <- data[,nodes]
agg_edges <- data[,edges]

nNodes_og <- as.numeric(base::ncol(agg_nodes))

if(!is.null(nodes_attribute)){agg_att <- agg_data %>% select(nodes_attribute)
                              agg_att <- setNames(agg_att, gsub(attribute_pattern, "", colnames(agg_att)))
                              if(ncol(agg_nodes) != ncol(agg_att)){
                                warning("Nodes and Nodes attributes are not of the same length")}}
agg_edges <- agg_data %>% select(edges)
nEdges_og <- as.numeric(base::ncol(agg_edges))


if(any(agg_nodes == 0, na.rm = TRUE)){
  agg_nodes[agg_nodes == 0] <- NA
                          warning("0 were found in nodes and were treated as missing")}

if(any(agg_edges == 0, na.rm = TRUE)){agg_edges[agg_edges == 0] <- NA
                          warning("0 were found in nodes and were treated as missing")}

if(!is.null(nodes_attribute)){
  if(any(agg_att == 0, na.rm = TRUE)){agg_att[agg_att == 0] <- NA
                          warning("0 were found in nodes and were treated as missing")}}


if(is.null(drop_nodes) == FALSE & is.null(drop_nodes_att)){
  if(is.numeric(drop_nodes) == FALSE){stop("drop_nodes must be numeric or NULL")}
  if((drop_nodes < 1 & drop_nodes > 0) == FALSE){stop("drop nodes must be between 0 and 1")}

  not_na <- colMeans(!is.na(agg_nodes))### to get the percentages?
  keep_nodes <- names(not_na >= drop_nodes)## das nuss noch verbessert werden, Muss das nicht auch irgendwie für nodes 2 gelten?
  agg_nodes <- agg_nodes[,keep_nodes]
  if(!is.null(nodes_attribute)){agg_att <- agg_att[,keep_nodes]
                              #  colnames(agg_att) <- paste0(colnames(agg_att),attribute_pattern) bruachen wir vllt nicht
                                }

                                                          }

if(is.null(drop_nodes) & is.null(drop_nodes_att) == FALSE){
  if(is.numeric(drop_nodes_att) == FALSE){stop("drop_nodes_att must be numeric or NULL")}
  if((drop_nodes_att < 1 & drop_nodes_att > 0) == FALSE){stop("drop_nodes_att must be between 0 and 1")}

  not_na <- colMeans(!is.na(agg_att))### to get the percentages?
  keep_nodes <- names(not_na >= drop_nodes_att)
  agg_att <- agg_att[,keep_nodes]
  agg_nodes <- agg_nodes[,keep_nodes]
}

if(is.null(drop_nodes) == FALSE & is.null(drop_nodes_att) == FALSE){
  if(is.numeric(drop_nodes_att) == FALSE){stop("drop_nodes_att must be numeric or NULL")}
  if((drop_nodes_att < 1 & drop_nodes_att > 0) == FALSE){stop("drop_nodes_att must be between 0 and 1")}
  if(is.numeric(drop_nodes) == FALSE){stop("drop_nodes must be numeric or NULL")}
  if((drop_nodes < 1 & drop_nodes > 0) == FALSE){stop("drop nodes must be between 0 and 1")}
   not_na_nodes <- colMeans(!is.na(agg_nodes))### to get the percentages?
   keep_nodes <- names(not_na_nodes >= drop_nodes)## das nuss noch verbessert werden, Muss das nicht auch irgendwie für nodes 2 gelten?

   not_na_att <- colMeans(!is.na(agg_att))### to get the percentages?
   keep_nodes_att <- names(not_na_att >= drop_nodes_att)

   keep_combined <- base::intersect(keep_nodes,keep_nodes_att)

   agg_nodes <- agg_nodes[,keep_combined]
   agg_att <- agg_att[,keep_combined]
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
agg_edges <- agg_edges[,keep_edges]

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
for (col in names(agg_edges)) {
  if (sum(!is.na(agg_edges[[col]])) < 2) {
    warning(paste("sd cannot be calculated for edge '", col, "'. Not enough non-NA values. Therefore
                  sd has been set to 0"))
  }
}

edges_sd <- data.frame(sd = apply(agg_edges, 2, sd, na.rm = TRUE))
edges_sd[is.na(edges_sd)] <- 0
edges_sd$edges <- rownames(edges_sd)
rownames(edges_sd) <- NULL

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
for (col in names(agg_edges_0)) {
  if (sum(!is.na(agg_edges_0[[col]])) < 2) {
    warning(paste("sd_ip cannot be calculated for edge '", col, "'. Not enough non-NA values. Therefore
                  sd_ip has been set to 0"))
  }
}

#edges_mean_ip <- as.data.frame(t(edges_mean_ip))
edges_sd_ip <- data.frame(sd_ip = apply(agg_edges_0, 2, sd, na.rm = TRUE))
edges_sd_ip$edges <- rownames(edges_sd_ip)
rownames(edges_sd_ip) <- NULL
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
for (col in names(agg_nodes)) {
  if (sum(!is.na(agg_nodes[[col]])) < 2) {
    warning(paste("sd cannot be calculated for node '", col, "'. Not enough non-NA values. Therefore
                  sd has been set to 0"))
  }
}
#agg_nodes_mean <- as.data.frame(t(agg_nodes_center))
agg_nodes_sd <- data.frame(sd = apply(agg_nodes, 2, sd, na.rm = TRUE))
agg_nodes_sd$nodes <- rownames(agg_nodes_sd)
rownames(agg_nodes_sd) <- NULL
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
agg_nodes_sd_oa <- data.frame(sd_oa = apply(agg_nodes_0, 2, sd, na.rm = TRUE))
agg_nodes_sd_oa$nodes <- rownames(agg_nodes_sd_oa)
rownames(agg_nodes_sd_oa) <- NULL
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
                 attribute_pattern = attribute_pattern)

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


