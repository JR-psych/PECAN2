edge_color_warnings <- function(edges_color = edges_color,   # spezial Fall
                                neg_edges_color = neg_edges_color,
                                edges_color_scaling = edges_color_scaling,
                                edges_color_by = edges_color_by,
                                edges_color_max = edges_color_max){


  #Add warning messages.
    # Check whether each element of edges_color and net_edges color is a valid color

  if(is.null(edges_color_by)){
    if(length(edges_color) > 1){stop("Length 'edges_color' can not be > 1 if edges_color_by is defined")}
    if(is.list(edges_color)){stop("'edges_color' can not be a list if edges_color_by is defined")}
    }

  if(!is.null(edges_color_by)){

     if(!is.list(edges_color)){stop("edges_color must be a list if edges_color by is defined")}
     if(length(edges_color) != 2){stop("edges_color needs to be a list of two colors if edges_color_by is defined")}
     if(any(lengths(edges_color) != 1)){stop("edges_color needs to be a list of two colors if edges_color_by is defined")}
     if(edges_color_scaling != "scaled" & edges_color_scaling != "fixed"){stop("edges_color_scaling must be 'scaled' or 'fixed'")}
     if(!is.null(edges_color_max)){
     if(is.numeric(edges_color_max) == FALSE | 0 > edges_color_max){
          stop("edges_color_max must be NUll or numeric and above 0")}}

     if(!is.null(neg_edges_color)){
       if(!is.list(neg_edges_color)){stop("neg_edges_color needs to be a list of two colors if edges_color_by is defined")}
       if(length(neg_edges_color) != 2){stop("neg_edges_color needs to be a list of two colors if edges_color_by is defined")}
       if(any(lengths(neg_edges_color) != 1)){stop("edges_color needs to be a list of two colors if edges_color_by is defined")}
       }

  }}
  #if (length(edges_color) > 2) {stop("edges_color has more than 2 elements")}
  #if (length(neg_edges_color) > 2) {stop("neg_edges_color has more than 2 elements")}
  #if (is_valid_color(color_v = edges_color) == FALSE){stop("one or more colors in edges_color are not valid")}
  #if (is_valid_color(color_v = neg_edges_color) == FALSE){stop("one or more colors in neg_edges_color are not valid")}
