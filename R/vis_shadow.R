
#' Title vis_shadow
#'
#' @param net_edges
#' @param use_ranking
#' @param min_rank
#' @param vis.env
#' @import dplyr
#' @return
#'
#' @examples
vis_shadow <- function(net_edges, shadow, shadow_color, shadow_by){  #setting shadow colore

  if(shadow_color == "default10"){s_color <- c("#87CEFA","#0000CD")} else {s_color <- shadow_color}

  l_e <- as.numeric(nrow(net_edges))
  l_c <- as.numeric(length(s_color))

  # 2 cutoffs for the shadow function. edges with a width equal or smaller than the first valur get not shadow. between the
  # two values get a medium and lighter shadow and above the second value get a thicker and darker shadow
  sh_1 <- shadow[1]
  sh_2 <- shadow[2]
  #sb <- shadow_by

  ## alles in allem die shadow function muss ich noch umschreiben. Man muss in der function oben noch eine variable in dem format variable$a
  ## angeben das möchte ich ändern. aber ggf könntest du schauen ob sie vom prinzip her funktioniert

  if(shadow[1] != "none"){if(is.numeric(shadow[1]) & is.numeric(shadow[2]) & l_e >= 3 & l_c == 2){

    net_edges["shadow_by"] <- net_edges[,shadow_by]
    net_edges <- net_edges %>% dplyr::mutate(shadow.enabled = ifelse(shadow_by <= sh_1,FALSE,TRUE),
                                      shadow.color = ifelse(shadow_by <= sh_1, NA, ifelse(shadow_by <= sh_2,s_color[1],s_color[2])),
                                      shadow.size = ifelse(shadow_by <= shadow[1], NA, ifelse(shadow_by <= shadow[2],5,10)))}}





  net_edges}




# net_edges <- net_edges %>% rename(shadow_by = shadow_by) ## würde ich heute anders schreiben hatte probleme damit spezifische spalten auszuwählen

