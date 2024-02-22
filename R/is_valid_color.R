


#' is_valid_color
#'
#' @param color_v
#'
#' @return
#' @export
#' @import grDevices
#' @examples
is_valid_color <- function(color_v){
  valid_color <- vector()

  for(color in unlist(color_v)){
    v_n <- TRUE
    v_c <- NULL
    if(grepl("^-?\\d*\\.?\\d+$",color)){v_n <- FALSE}
    v_c <- tryCatch({
      # Attempt to convert the string to a color
      suppressWarnings(grDevices::col2rgb(color))
      v_c <- TRUE
    }, error = function(e){v_c <- FALSE})

    valid_color <- cbind(valid_color,v_n,v_c)
  }

  if(any(FALSE %in% valid_color)){return(FALSE)}
  else{return(TRUE)}
}
