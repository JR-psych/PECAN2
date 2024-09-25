


#' Title
#'
#' @param regulation
#' @param net_edges
#' @param net_nodes
#'
#' @return
#' @export
#'
#' @examples
regulation_warnings <- function(edges_regulation,edges){


  if(is.list(edges_regulation) == FALSE){stop("edges_regulation must be NULL or a list")}

  if(("reg_type" %in% names(edges_regulation)) == FALSE){stop("reg_type is missing in regulation")}

  if(("reg_by" %in% names(edges_regulation)) == FALSE){stop("reg_by is missing in regulation")}

  if(("reg_value" %in% names(edges_regulation)) == FALSE){stop("reg_value is missing in regulation")}

  if(is.numeric(edges_regulation$reg_value) == FALSE){stop("reg_value must be numeric")}

  if(is.numeric(as.vector(edges[,edges_regulation$reg_by, drop = TRUE])) == FALSE){stop("reg_by must be numeric")}

  reg_test <- as.numeric(edges[[edges_regulation$reg_by]]) < 0
  if(any(reg_test)){warning("Negative values in reg_by! Abs values are used for
                                               regulation.")}

  #browser()
  if((edges_regulation$reg_type != "nodes" & edges_regulation$reg_type != "number" & edges_regulation$reg_type != "value")){stop("reg_type must be 'nodes','number' or 'value'")}

  }

