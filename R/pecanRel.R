




#' Title pecanRel
#'

#'
#' @return A df with different edge reliability measures Pearson_standard and Spearman_standard only
#' include edges which are eithe prensent in t1 or t2. Pearson_all_pe and Spearan_all_pe include all possible edges
#' @export
#'
#' @examples
#' @import dplyr

#'
#' @param data_t1 edges t1. "from", "to" and "width" are required
#' @param data_t2 edges t2. "from", "to" and "width" are required
#' @param n_nodes number of nodes
pecanRel <- function(data_t1,data_t2,n_nodes){



  # check requirements
  if(is.data.frame(data_t1) == FALSE){stop("data_t1 must be a datframe")}
  if(is.data.frame(data_t2) == FALSE){stop("data_t2 must be a dataframe")}


  n_nodes <- n_nodes
  data1 <- data_t1
  data2 <- data_t2


  if("from" %in% colnames(data_t1) == FALSE){stop("from column in data_t1 is missing")}
  if("to" %in% colnames(data_t1) == FALSE){stop("to column in data_t1 is missing")}
  if("width" %in% colnames(data_t1) == FALSE){stop("width column in data_t1 is missing")}

  if("from" %in% colnames(data_t2) == FALSE){stop("from column in data_t2 is missing")}
  if("to" %in% colnames(data_t2) == FALSE){stop("to column in data_t2 is missing")}
  if("width" %in% colnames(data_t2) == FALSE){stop("width column in data_t2 is missing")}


    # prepare edge df t1
    d1 <- data1 %>% dplyr::select(from,to,width)
   # d1 <<- d1 %>% mutate(edge = paste(from,to,sep = "_")) %>% select(edge,width) %>% rename(w_t1 = width)
    d1 <- d1 %>% dplyr::mutate(edge = paste(from,to,sep = "_")) %>% dplyr::select(edge,width) %>% dplyr::rename(w_t1 = width)

    # prepare dge df t2
    d2 <- data2 %>% dplyr::select(from,to,width)
    #d2 <<- d2 %>% mutate(edge = paste(from,to,sep = "_")) %>% select(edge,width) %>% rename(w_t2 = width)
    d2 <- d2 %>% dplyr::mutate(edge = paste(from,to,sep = "_")) %>% dplyr::select(edge,width) %>% dplyr::rename(w_t2 = width)

    # merge t1 and t2
    rel_cor <- merge(d1,d2, by = "edge", all.x = TRUE, all.y = TRUE)
 #   assign("rel_cor", rel_cor, envir = .GlobalEnv)
    rel_cor0 <- rel_cor


    # add edges with a vlue of null until the number of possible edges is reached
    y <- as.numeric(nrow(rel_cor0))
    z <- n_nodes*(n_nodes-1)
    repeat{
      rel_cor0 <- rel_cor0 %>% dplyr::add_row(edge = "new", w_t1 = 0, w_t2 = 0)
      y <- y + 1
      if (y == z){
        break}}

    # replace Nas with 0
    rel_cor_f <<- rel_cor %>% replace(is.na(.), 0)
    rel_cor0_f <<- rel_cor0 %>% replace(is.na(.), 0)

    # calculate reliability
    rel_f <<- data.frame(Pearson_standard = cor(rel_cor_f$w_t1,rel_cor_f$w_t2),
                        Spearman_standard = cor(rel_cor_f$w_t1,rel_cor_f$w_t2,method = "spearman"),
                        Pearson_all_pe = cor(rel_cor0_f$w_t1,rel_cor0_f$w_t2),
                        Spearman_all_pe = cor(rel_cor0_f$w_t1,rel_cor0_f$w_t2,method = "spearman"))
    rel_f


   }

  #rel_f <<- data.frame(Pearson_standard = cor(rel_cor_f$w_t1,rel_cor_f$w_t2),
  #                     Spearman_standard = cor(rel_cor_f$w_t1,rel_cor_f$w_t2,method = "spearman"),
  #                     Pearson_all_p = cor(rel_cor0_f$w_t1,rel_cor0_f$w_t2),
  #                     Spearman_standard = cor(rel_cor0_f$w_t1,rel_cor0_f$w_t2,method = "spearman"))
  #abc <<- c(1,2,3)}
