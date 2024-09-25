test_that("default tests", {
  test_path("test_files","pecan_example_edges.rda")
  test_path("test_files","pecan_example_nodes.rda")
  edges <- pecan_example_edges
  nodes <- pecan_example_nodes
  nodes_value_v <- nodes$value
  edges_width_v <- edges$width

  expect_equal(is.data.frame(nodes),TRUE)
  expect_equal(is.data.frame(nodes),TRUE)

  test_default <- pecanPrepare(nodes = nodes,edges = edges)
  default_nodes <- test_default$nodes
  default_edges <- test_default$edges


  #browser()

     c("id","size","color.border","color.background")

  expect_equal(colnames(default_nodes),c("id","size","color.background","color.border"))
  expect_equal(all(default_nodes$color.background == "red"),TRUE)
  expect_equal(all(default_nodes$color.border == "red"),TRUE)
  expect_equal(all(default_nodes$size == 28),TRUE)
  expect_equal(default_nodes$id,1:11)
  #expect_equal(all(default_nodes$size),28)
  expect_equal(colnames(default_edges),c("from","to","width","color.color","arrows","smooth"))
  expect_equal(all(default_edges$color.color == "black"),TRUE)
  expect_equal(all(default_edges$arrows == "to"),TRUE)
  expect_equal(all(default_edges$smooth == TRUE),TRUE)

  # Test what happens if you use edges with 0 rows and nodes with 0 rows
  edges0 <- data.frame(from = character(0), to = character(0))
  nodes0 <- data.frame(id = character(0))

  test0 <- pecanPrepare(nodes = nodes,edges = edges0)
  expect_equal(nrow(test0$edges),0)
  test0_1 <- pecanPrepare(nodes = nodes,edges = edges0,edges_color = "red")
  expect_equal(nrow(test0_1$edges),0)

  expect_error(pecanPrepare(nodes = nodes0,edges = edges),"There must be at least one node in the network")

  # Test size variations
  size_1 <- pecanPrepare(nodes = nodes,edges = edges, nodes_size = 20)
  size_2 <- pecanPrepare(nodes = nodes,edges = edges, nodes_size = 1:nrow(nodes))
  size_3 <- pecanPrepare(nodes = nodes,edges = edges, nodes_size = nodes$value)
  size_4 <- pecanPrepare(nodes = nodes,edges = edges, nodes_size = "value")

  expect_equal(all(size_1$nodes$size == 20),TRUE)
  expect_equal(all((size_2$nodes$size == 1:nrow(nodes)) == TRUE),TRUE)
  expect_equal(all((size_3$nodes$size == nodes$value)) == TRUE,TRUE)

  nodes_s_max <- max(nodes$value)
  if(nodes_s_max < 33){test_nodes_size <- nodes$value * (33/nodes_s_max)}
  if(nodes_s_max > 33){test_nodes_size <- nodes$value / (nodes_s_max/33)}
  expect_equal(all((size_4$nodes$size == test_nodes_size)) == TRUE,TRUE)
# test interplay of nodes_size and nodes_size_max

  # nothing should happen// use same test
  # nodes range from 30 to 70

  #nothing should happen
  size_5 <- pecanPrepare(nodes = nodes,edges = edges, nodes_size = 20, nodes_size_max = 90)
  expect_equal(all(size_5$nodes$size == 20),TRUE)


  #nothing should happen
  size_6 <- pecanPrepare(nodes = nodes,edges = edges, nodes_size = 1:nrow(nodes), nodes_size_max = 90)
  expect_equal(all((size_6$nodes$size == 1:nrow(nodes)) == TRUE),TRUE)

  #nothing should happen
  size_7 <- pecanPrepare(nodes = nodes,edges = edges, nodes_size = nodes$value, nodes_size_max = 80)
  expect_equal(all((size_7$nodes$size == nodes$value)) == TRUE,TRUE)


  size_8 <- pecanPrepare(nodes = nodes,edges = edges, nodes_size = "value", nodes_size_max = 90)
  nodes_custom_max <- nodes$value / (90/33)
  expect_equal(all((size_8$nodes$size == nodes_custom_max)) == TRUE,TRUE)

  # if nodes_size_max is less than the max value of size column an error should occur
  expect_error(pecanPrepare(nodes = nodes,edges = edges, nodes_size = "value", nodes_size_max = 10),
               "nodes_size_max must be greater or equal than the maximum value of nodes size")

  #### Test color

  nodes$colorA <- c(rep("green",10),"red")
  nodes$colorB <- c(rep("green",10),"wrong_color")

  expect_error(pecanPrepare(nodes = nodes,edges = edges, nodes_color = "colorB"))

  color_1 <- pecanPrepare(nodes = nodes,edges = edges, nodes_color = "blue")
  expect_equal(all(color_1$nodes$color.background == "blue"),TRUE)
  expect_equal(all(color_1$nodes$color.border == "blue"),TRUE)

  color_2 <- pecanPrepare(nodes = nodes,edges = edges, nodes_color = "colorA")
  expect_equal(all(color_2$nodes$color.background == nodes$colorA),TRUE)
  expect_equal(all(color_2$nodes$color.border == nodes$colorA),TRUE)

  color_3 <- pecanPrepare(nodes = nodes,edges = edges, nodes_color = nodes$colorA)
  expect_equal(all(color_3$nodes$color.background == nodes$colorA),TRUE)
  expect_equal(all(color_3$nodes$color.border == nodes$colorA),TRUE)

  color_v <- c("red","green","blue","yellow","red","green","blue","yellow","red","green","blue")
  color_4 <- pecanPrepare(nodes = nodes,edges = edges, nodes_color = color_v)
  expect_equal(all(color_4$nodes$color.background == color_v),TRUE)
  expect_equal(all(color_4$nodes$color.border == color_v),TRUE)

  # Test interplay of nodes_color and nodes_color_by

  # Test should throw error because nodes:color is not a list but nodes_color_by is defined
  expect_error(pecanPrepare(nodes = nodes,edges = edges, nodes_color = "blue",
                            nodes_color_by = "value"))

  expect_error(pecanPrepare(nodes = nodes,edges = edges, nodes_color = color_v,
                            nodes_color_by = "value"))

  expect_error(pecanPrepare(nodes = nodes,edges = edges, nodes_color = "colorA",
                            nodes_color_by = "value"))

  expect_error(pecanPrepare(nodes = nodes,edges = edges, nodes_color = nodes$colorA,
                            nodes_color_by = "value"))


  # Test interplay of nodes_color, nodes_color_by and nodes_color_max and nodes_color_scaling


  color_5  <- pecanPrepare(nodes = nodes,edges = edges, nodes_color = list("blue","darkblue"),
                           nodes_color_by = "value")

  nodes_value_v5 <- nodes_value_v


  if(max(nodes_value_v) < 10){nodes_value_v5 <- nodes_value_v5 * (10/(max(nodes_value_v5)))}
  if(max(nodes_value_v) > 10){nodes_value_v5 <- nodes_value_v5 / (max(nodes_value_v5)/10)}

  rgb_5 <- colorRamp(c("blue","darkblue"))(
    (nodes_value_v5 - min(nodes_value_v5))/(10 - min(nodes_value_v5)))

  i_colors_5 <- rgb(rgb_5[, 1], rgb_5[, 2], rgb_5[, 3], maxColorValue = 255)

  #browser()

  expect_equal(all(color_5$nodes$color.background == i_colors_5),TRUE)
  expect_equal(all(color_5$nodes$color.border == i_colors_5),TRUE)


  expect_error(pecanPrepare(nodes = nodes,edges = edges, nodes_color = list("yellow","green"),
                            nodes_color_by = "value", nodes_color_max = 10),
               "nodes_color_max must be greater or equal than the maximum value of nodes_color")

  # nodes_color_scaling default is "scaled"
  color_6  <- pecanPrepare(nodes = nodes,edges = edges, nodes_color = list("yellow","green"),
                           nodes_color_by = "value", nodes_color_max = 90)

  nodes_value_v6 <- nodes_value_v
  nodes_value_v6 <- nodes_value_v6 / (90/10)

  rgb_6 <- colorRamp(c("yellow","green"))(
    (nodes_value_v6 - min(nodes_value_v6))/(10 - min(nodes_value_v6)))

  i_colors_6 <- rgb(rgb_6[, 1], rgb_6[, 2], rgb_6[, 3], maxColorValue = 255)

  #browser()


  expect_equal(all(color_6$nodes$color.background == i_colors_6),TRUE)
  expect_equal(all(color_6$nodes$color.border == i_colors_6),TRUE)


  color_7  <- pecanPrepare(nodes = nodes,edges = edges, nodes_color = list("yellow","green"),
                           nodes_color_by = "value", nodes_color_scaling = "scaled")


  nodes_value_v7 <- nodes_value_v


  if(max(nodes_value_v) < 10){nodes_value_v7 <- nodes_value_v7 * (10/(max(nodes_value_v7)))}
  if(max(nodes_value_v) > 10){nodes_value_v7 <- nodes_value_v7 / (max(nodes_value_v7)/10)}

  rgb_7 <- colorRamp(c("yellow","green"))(
    (nodes_value_v7 - min(nodes_value_v7))/(10 - min(nodes_value_v7)))
  i_colors_7 <- rgb(rgb_7[, 1], rgb_7[, 2], rgb_7[, 3], maxColorValue = 255)

  expect_equal(all(color_7$nodes$color.background == i_colors_7),TRUE)
  expect_equal(all(color_7$nodes$color.border == i_colors_7),TRUE)

  color_8  <- pecanPrepare(nodes = nodes,edges = edges, nodes_color = list("yellow","green"),
                           nodes_color_by = "value", nodes_color_scaling = "fixed")


  nodes_value_v8 <- nodes_value_v


  if(max(nodes_value_v) < 10){nodes_value_v8 <- nodes_value_v8 * (10/(max(nodes_value_v8)))}
  if(max(nodes_value_v) > 10){nodes_value_v8 <- nodes_value_v8 / (max(nodes_value_v8)/10)}

  rgb_8 <- colorRamp(c("yellow","green"))(
    (nodes_value_v8/10))
  i_colors_8 <- rgb(rgb_8[, 1], rgb_8[, 2], rgb_8[, 3], maxColorValue = 255)

  expect_equal(all(color_8$nodes$color.background == i_colors_8),TRUE)
  expect_equal(all(color_8$nodes$color.border == i_colors_8),TRUE)


  # Should be the same as 6 as default is "scaled"
  color_9 <- pecanPrepare(nodes = nodes,edges = edges, nodes_color = list("yellow","green"),
                            nodes_color_by = "value", nodes_color_max = 90, nodes_color_scaling = "scaled")


  expect_equal(all(color_9$nodes$color.background == i_colors_6),TRUE)
  expect_equal(all(color_9$nodes$color.border == i_colors_6),TRUE)



  color_10 <- pecanPrepare(nodes = nodes,edges = edges, nodes_color = list("yellow","green"),
                          nodes_color_by = "value", nodes_color_max = 90, nodes_color_scaling = "fixed")


  nodes_value_v10 <- nodes_value_v
  nodes_value_v10 <- nodes_value_v10 / (90/10)

  rgb_10 <- colorRamp(c("yellow","green"))(
    (nodes_value_v10/10))
  i_colors_10 <- rgb(rgb_10[, 1], rgb_10[, 2], rgb_10[, 3], maxColorValue = 255)

  expect_equal(all(color_10$nodes$color.background == i_colors_10),TRUE)
  expect_equal(all(color_10$nodes$color.border == i_colors_10),TRUE)


  nodes$com1 <- c(rep("A",3),rep("b",3),"A",rep(1,4))


  ###
  ###
  ###


  color_11 <- pecanPrepare(nodes = nodes,edges = edges, nodes_color = list(c("blue","darkblue"),c("yellow","green"),
                                                                           c("red","darkred")),
               nodes_color_by = "value", nodes_community = "com1")

  unique_groups <- sort(unique(nodes$com1))
  com11_1 <- nodes[nodes$com1 == unique_groups[1],]
  com11_2 <- nodes[nodes$com1 == unique_groups[2],]
  com11_3 <- nodes[nodes$com1 == unique_groups[3],]

  com_11_1_v <- com11_1$value
  com_11_2_v <- com11_2$value
  com_11_3_v <- com11_3$value

  if(max(com_11_1_v) < 10){com11_1$value <- com11_1$value * (10/(max(com11_1$value)))}
  if(max(com_11_1_v) > 10){com11_1$value <- com11_1$value / (max(com11_1$value)/10)}
  if(max(com_11_2_v) < 10){com11_2$value <- com11_2$value * (10/(max(com11_2$value)))}
  if(max(com_11_2_v) > 10){com11_2$value <- com11_2$value / (max(com11_2$value)/10)}
  if(max(com_11_3_v) < 10){com11_3$value <- com11_3$value * (10/(max(com11_3$value)))}
  if(max(com_11_3_v) > 10){com11_3$value <- com11_3$value / (max(com11_3$value)/10)}

 # browser()

  if(min(com11_1$value) == max(com11_1$value)){ #avoid dividing by 0
    rgb_11_1 <- colorRamp(c("blue","darkblue"))(com11_1$value/10)}
  else{rgb_11_1 <- colorRamp(c("blue","darkblue"))(
    (com11_1$value - min(com11_1$value))/(10 - min(com11_1$value)))}

  i_colors_11_1 <- rgb(rgb_11_1[, 1], rgb_11_1[, 2], rgb_11_1[, 3], maxColorValue = 255)
  com11_1$color <- i_colors_11_1

  if(min(com11_2$value) == max(com11_2$value)){ #avoid dividing by 0
    rgb_11_2 <- colorRamp(c("yellow","green"))(com11_2$value/10)}
  else{rgb_11_2 <- colorRamp(c("yellow","green"))(
    (com11_2$value - min(com11_2$value))/(10 - min(com11_2$value)))}

  i_colors_11_2 <- rgb(rgb_11_2[, 1], rgb_11_2[, 2], rgb_11_2[, 3], maxColorValue = 255)
  com11_2$color <- i_colors_11_2

  if(min(com11_3$value) == max(com11_3$value)){ #avoid dividing by 0
    rgb_11_3 <- colorRamp(c("red","darkred"))(com11_3$value/10)}
  else{rgb_11_3 <- colorRamp(c("red","darkred"))(
    (com11_3$value - min(com11_3$value))/(10 - min(com11_3$value)))}

  i_colors_11_3 <- rgb(rgb_11_3[, 1], rgb_11_3[, 2], rgb_11_3[, 3], maxColorValue = 255)
  com11_3$color <- i_colors_11_3


  com11_1_2 <- merge(com11_1,com11_2, all= TRUE)
  com11_1_2_3 <- merge(com11_1_2,com11_3, all = TRUE)


  #browser()
#scales::show_col(com11_1_2_3$color)

  expect_equal(all(color_11$nodes$color.background == com11_1_2_3$color),TRUE)


  #should be equals as color_11
  color_12 <- pecanPrepare(nodes = nodes,edges = edges, nodes_color = list(c("blue","darkblue"),c("yellow","green"),
                                                                           c("red","darkred")),
                           nodes_color_by = "value", nodes_community = "com1", nodes_color_scaling = "scaled")


  expect_equal(all(color_12$nodes$color.background == com11_1_2_3$color),TRUE)

  color_13 <- pecanPrepare(nodes = nodes,edges = edges, nodes_color = list(c("blue","darkblue"),c("yellow","green"),
                                                                           c("red","darkred")),
                           nodes_color_by = "value", nodes_community = "com1", nodes_color_scaling = "fixed")



  unique_groups <- sort(unique(nodes$com1))
  com13_1 <- nodes[nodes$com1 == unique_groups[1],]
  com13_2 <- nodes[nodes$com1 == unique_groups[2],]
  com13_3 <- nodes[nodes$com1 == unique_groups[3],]

  com_13_1_v <- com13_1$value
  com_13_2_v <- com13_2$value
  com_13_3_v <- com13_3$value

  if(max(com_13_1_v) < 10){com13_1$value <- com13_1$value * (10/(max(com13_1$value)))}
  if(max(com_13_1_v) > 10){com13_1$value <- com13_1$value / (max(com13_1$value)/10)}
  if(max(com_13_2_v) < 10){com13_2$value <- com13_2$value * (10/(max(com13_2$value)))}
  if(max(com_13_2_v) > 10){com13_2$value <- com13_2$value / (max(com13_2$value)/10)}
  if(max(com_13_3_v) < 10){com13_3$value <- com13_3$value * (10/(max(com13_3$value)))}
  if(max(com_13_3_v) > 10){com13_3$value <- com13_3$value / (max(com13_3$value)/10)}


  rgb_13_1 <- colorRamp(c("blue","darkblue"))(com13_1$value/10)


  i_colors_13_1 <- rgb(rgb_13_1[, 1], rgb_13_1[, 2], rgb_13_1[, 3], maxColorValue = 255)
  com13_1$color <- i_colors_13_1


  rgb_13_2 <- colorRamp(c("yellow","green"))(com13_2$value/10)


  i_colors_13_2 <- rgb(rgb_13_2[, 1], rgb_13_2[, 2], rgb_13_2[, 3], maxColorValue = 255)
  com13_2$color <- i_colors_13_2

  rgb_13_3 <- colorRamp(c("red","darkred"))(com13_3$value/10)

  i_colors_13_3 <- rgb(rgb_13_3[, 1], rgb_13_3[, 2], rgb_13_3[, 3], maxColorValue = 255)
  com13_3$color <- i_colors_13_3


  com13_1_2 <- merge(com13_1,com13_2, all= TRUE)
  com13_1_2_3 <- merge(com13_1_2,com13_3, all = TRUE)


  expect_equal(all(color_13$nodes$color.background == com13_1_2_3$color),TRUE)



  color_14 <- pecanPrepare(nodes = nodes,edges = edges, nodes_color = list(c("blue","darkblue"),c("yellow","green"),
                                                                           c("red","darkred")),
                           nodes_color_by = "value", nodes_community = "com1", nodes_color_scaling = "scaled",nodes_color_max = 90)


  unique_groups <- sort(unique(nodes$com1))
  com14_1 <- nodes[nodes$com1 == unique_groups[1],]
  com14_2 <- nodes[nodes$com1 == unique_groups[2],]
  com14_3 <- nodes[nodes$com1 == unique_groups[3],]

  com_14_1_v <- com14_1$value
  com_14_2_v <- com14_2$value
  com_14_3_v <- com14_3$value

  if(max(com_14_1_v) < 10){com14_1$value <- com14_1$value * (10/90)}
  if(max(com_14_1_v) > 10){com14_1$value <- com14_1$value / (90/10)}
  if(max(com_14_2_v) < 10){com14_2$value <- com14_2$value * (10/90)}
  if(max(com_14_2_v) > 10){com14_2$value <- com14_2$value / (90/10)}
  if(max(com_14_3_v) < 10){com14_3$value <- com14_3$value * (10/90)}
  if(max(com_14_3_v) > 10){com14_3$value <- com14_3$value / (90/10)}

  # browser()

  if(min(com14_1$value) == 10){ #avoid dividing by 0
    rgb_14_1 <- colorRamp(c("blue","darkblue"))(com14_1$value/10)}
  else{rgb_14_1 <- colorRamp(c("blue","darkblue"))(
    (com14_1$value - min(com14_1$value))/(10 - min(com14_1$value)))}

  i_colors_14_1 <- rgb(rgb_14_1[, 1], rgb_14_1[, 2], rgb_14_1[, 3], maxColorValue = 255)
  com14_1$color <- i_colors_14_1

  if(min(com14_2$value) == 10){ #avoid dividing by 0
    rgb_14_2 <- colorRamp(c("yellow","green"))(com14_2$value/10)}
  else{rgb_14_2 <- colorRamp(c("yellow","green"))(
    (com14_2$value - min(com14_2$value))/(10 - min(com14_2$value)))}

  i_colors_14_2 <- rgb(rgb_14_2[, 1], rgb_14_2[, 2], rgb_14_2[, 3], maxColorValue = 255)
  com14_2$color <- i_colors_14_2

  if(min(com14_3$value) == 10){ #avoid dividing by 0
    rgb_14_3 <- colorRamp(c("red","darkred"))(com14_3$value/10)}
  else{rgb_14_3 <- colorRamp(c("red","darkred"))(
    (com14_3$value - min(com14_3$value))/(10 - min(com14_3$value)))}

  i_colors_14_3 <- rgb(rgb_14_3[, 1], rgb_14_3[, 2], rgb_14_3[, 3], maxColorValue = 255)
  com14_3$color <- i_colors_14_3


  com14_1_2 <- merge(com14_1,com14_2, all= TRUE)
  com14_1_2_3 <- merge(com14_1_2,com14_3, all = TRUE)

  expect_equal(all(color_14$nodes$color.background == com14_1_2_3$color),TRUE)



  color_15 <- pecanPrepare(nodes = nodes,edges = edges, nodes_color = list(c("blue","darkblue"),c("yellow","green"),
                                                                           c("red","darkred")),
                           nodes_color_by = "value", nodes_community = "com1", nodes_color_scaling = "fixed",nodes_color_max = 90)


  unique_groups <- sort(unique(nodes$com1))
  com15_1 <- nodes[nodes$com1 == unique_groups[1],]
  com15_2 <- nodes[nodes$com1 == unique_groups[2],]
  com15_3 <- nodes[nodes$com1 == unique_groups[3],]

  com_15_1_v <- com15_1$value
  com_15_2_v <- com15_2$value
  com_15_3_v <- com15_3$value

  if(max(com_15_1_v) < 10){com15_1$value <- com15_1$value * (10/90)}
  if(max(com_15_1_v) > 10){com15_1$value <- com15_1$value / (90/10)}
  if(max(com_15_2_v) < 10){com15_2$value <- com15_2$value * (10/90)}
  if(max(com_15_2_v) > 10){com15_2$value <- com15_2$value / (90/10)}
  if(max(com_15_3_v) < 10){com15_3$value <- com15_3$value * (10/90)}
  if(max(com_15_3_v) > 10){com15_3$value <- com15_3$value / (90/10)}


  rgb_15_1 <- colorRamp(c("blue","darkblue"))(com15_1$value/10)


  i_colors_15_1 <- rgb(rgb_15_1[, 1], rgb_15_1[, 2], rgb_15_1[, 3], maxColorValue = 255)
  com15_1$color <- i_colors_15_1


  rgb_15_2 <- colorRamp(c("yellow","green"))(com15_2$value/10)


  i_colors_15_2 <- rgb(rgb_15_2[, 1], rgb_15_2[, 2], rgb_15_2[, 3], maxColorValue = 255)
  com15_2$color <- i_colors_15_2

  rgb_15_3 <- colorRamp(c("red","darkred"))(com15_3$value/10)

  i_colors_15_3 <- rgb(rgb_15_3[, 1], rgb_15_3[, 2], rgb_15_3[, 3], maxColorValue = 255)
  com15_3$color <- i_colors_15_3


  com15_1_2 <- merge(com15_1,com15_2, all= TRUE)
  com15_1_2_3 <- merge(com15_1_2,com15_3, all = TRUE)


  expect_equal(all(color_15$nodes$color.background == com15_1_2_3$color),TRUE)

  #Test nodes label

  label_1 <- pecanPrepare(nodes = nodes,edges = edges, nodes_label = "label")
  expect_equal(all(label_1$nodes$label == nodes$label),TRUE)

  label_2 <- pecanPrepare(nodes = nodes,edges = edges, nodes_label = NULL)
  expect_equal("label" %in% colnames(label_2$nodes),FALSE)


  # Test nodes_hide_isolated
  # Test also with community

  hide_1 <- pecanPrepare(nodes = nodes,edges = edges, nodes_hide_isolated = TRUE)
  expect_equal(all(hide_1$nodes$hidden == FALSE),TRUE)


  nodes[12,] <- NA
  nodes[12,1] <- 12
  nodes[12,2] <- 50
  nodes[12,3] <- "Node12"
  nodes[12,4] <- NA
  nodes[12,5] <- "green"
  nodes[12,6] <- "green"
  nodes[12,7] <- "b"
  nodes[13,] <- NA
  nodes[13,1] <- 13
  nodes[13,2] <- 60
  nodes[13,3] <- "Node13"
  nodes[13,4] <- NA
  nodes[13,5] <- "green"
  nodes[13,6] <- "green"
  nodes[13,7] <- "5"
  nodes[14,] <- NA
  nodes[14,1] <- 14
  nodes[14,2] <- 70
  nodes[14,3] <- "Node14"
  nodes[14,4] <- NA
  nodes[14,5] <- "green"
  nodes[14,6] <- "green"
  nodes[14,7] <- "5"

  hide_2 <- pecanPrepare(nodes = nodes,edges = edges, nodes_hide_isolated = TRUE)

  hide_2_string <- c(rep(FALSE,11),TRUE,TRUE,TRUE)

  expect_equal(all(hide_2$nodes$hidden == hide_2_string),TRUE)


  ####### edges width ####################
  e_width_1 <- pecanPrepare(nodes = nodes,edges = edges, edges_width = 20)
  e_width_2 <- pecanPrepare(nodes = nodes,edges = edges, edges_width = 1:nrow(edges))
  e_width_3 <- pecanPrepare(nodes = nodes,edges = edges, edges_width = edges$width)
  e_width_4 <- pecanPrepare(nodes = nodes,edges = edges, edges_width = "width")



  expect_equal(all(e_width_1$edges$width == 20),TRUE)
  expect_equal(e_width_2$edges$width,1:nrow(edges))
  expect_equal(e_width_3$edges$width,edges$width)




  edges_w_max <- max(edges$width)
  edges_v4 <- edges$width

  if(edges_w_max < 10){edges_v4  <- edges_v4 * (10/edges_w_max)}
  if(edges_w_max > 10){edges_v4  <- edges_v4 / (edges_w_max/10)}
  expect_equal(e_width_4$edges$width,edges_v4)
  # test interplay of nodes_size and nodes_size_max

  # nothing should happen// use same test
  # nodes range from 30 to 70

  #nothing should happen
  e_width_5 <- pecanPrepare(nodes = nodes,edges = edges, edges_width = 20, edges_width_max = 90)
  expect_equal(all(e_width_5$edges$width == 20),TRUE)


  #nothing should happen
  e_width_6 <- pecanPrepare(nodes = nodes,edges = edges, edges_width = 1:nrow(edges), edges_width_max = 90)
  expect_equal(e_width_6$edges$width, 1:nrow(edges))

  #nothing should happen
  e_width_7 <- pecanPrepare(nodes = nodes,edges = edges, edges_width = edges$width, edges_width_max = 80)
  expect_equal(e_width_7$edges$width,edges$width)


  e_width_8 <- pecanPrepare(nodes = nodes,edges = edges, edges_width = "width", edges_width_max = 30)
  edges_custom_max <- edges$width / (30/10)
  expect_equal(e_width_8$edges$width,edges_custom_max)

  # if nodes_size_max is less than the max value of size column an error should occur
  expect_error(pecanPrepare(nodes = nodes,edges = edges, edges_width = "width", edges_width_max = 1),
               "edges_width_max must be greater or equal than the maximum value of edges_width")


  edges$width2 <- edges$width * 15

  e_width_9 <- pecanPrepare(nodes = nodes,edges = edges, edges_width = "width2")

  edges_w_max2 <- max(edges$width2)
  edges_v9 <- edges$width2

  if(edges_w_max2 < 10){edges_v9  <- edges_v9 * (10/edges_w_max2)}
  if(edges_w_max2 > 10){edges_v9  <- edges_v9 / (edges_w_max2/10)}
  expect_equal(e_width_9$edges$width,edges_v9)

  e_width_10 <- pecanPrepare(nodes = nodes,edges = edges, edges_width = "width2",edges_width_max = 110)

  edges_v10 <- edges$width2

  edges_v10  <- edges_v10 / (110/10)
  expect_equal(e_width_10$edges$width,edges_v10)

  ######################################

  ### test edges color, check also whether color and with are independent. Which they should be

  edges$colorA <- c(rep("red",6),rep("blue",6),rep("green",6),rep("yellow",6))


  e_color_1 <- pecanPrepare(nodes = nodes,edges = edges, edges_color = "blue",edges_width = 20)
  expect_equal(all(e_color_1$edges$color.color == "blue"),TRUE)
  expect_equal(all(e_color_1$edges$width == 20),TRUE)
  expect_error(pecanPrepare(nodes = nodes,edges = edges, edges_color = list("blue","red")))
  expect_error(pecanPrepare(nodes = nodes,edges = edges, edges_color = c("blue","red")))
  expect_error(pecanPrepare(nodes = nodes,edges = edges, edges_color = list("blue")))

  e_color_2 <- pecanPrepare(nodes = nodes,edges = edges, edges_color = "colorA",edges_width = "width",
                            edges_width_max = 30)
  expect_equal(e_color_2$edges$color.color,edges$colorA)
  expect_equal(e_color_2$edges$width,edges_custom_max)


  expect_error(pecanPrepare(nodes = nodes,edges = edges, edges_color = "blue",edges_color_by = "width"))
  expect_error(pecanPrepare(nodes = nodes, edges = edges, edges_color = list("blue"),
                            edges_color_by = "width"))



 e_color_3 <- pecanPrepare(nodes = nodes, edges = edges, edges_color = list("grey","black"),
                           edges_color_by = "width",edges_width = edges$width)
#
 # browser()

  expect_equal(e_color_3$edges$width,edges$width)

  edges_width_v3 <- edges_width_v


  if(max(edges_width_v) < 10){edges_width_v3 <- edges_width_v3 * (10/(max(edges_width_v3)))}
  if(max(edges_width_v) > 10){edges_width_v3 <- edges_width_v3 / (max(edges_width_v3)/10)}

  rgb_e_3 <- colorRamp(c("grey","black"))(
    (edges_width_v3 - min(edges_width_v3))/(10 - min(edges_width_v3)))

  e_i_colors_3 <- rgb(rgb_e_3[, 1], rgb_e_3[, 2], rgb_e_3[, 3], maxColorValue = 255)

  #browser()
  expect_equal(e_color_3$edges$color.color,e_i_colors_3)

 #  browser()

  expect_error(pecanPrepare(nodes = nodes,edges = edges, edges_color = list("grey","black"),
                            edges_color_by = "width", edges_color_max = 0.1),"edges_color_max must be greater or equal than the maximum value of edges_color_by")

  # edges_color_scaling default is "scaled"
  e_color_4   <- pecanPrepare(nodes = nodes, edges = edges, edges_color = list("grey","black"),
                              edges_color_by = "width",edges_color_max = 11,
                              edges_width = "width", edges_width_max = 30)

  expect_equal(e_color_4$edges$width,edges_custom_max)


  edges_width_v4 <- edges_width_v
  edges_width_v4 <- edges_width_v4 / (11/10)

  rgb_e_4 <- colorRamp(c("grey","black"))(
    (edges_width_v4 - min(edges_width_v4))/(10 - min(edges_width_v4)))

  e_i_colors_4 <- rgb(rgb_e_4[, 1], rgb_e_4[, 2], rgb_e_4[, 3], maxColorValue = 255)

  expect_equal(e_color_4$edges$color.color,e_i_colors_4)


  e_color_5   <- pecanPrepare(nodes = nodes, edges = edges, edges_color = list("grey","black"),
                              edges_color_by = "width",edges_color_scaling = "scaled",
                              edges_width = "width2")


  expect_equal(e_color_5$edges$width,edges_v9)

  expect_equal(e_color_5$edges$color.color,e_i_colors_3)


  e_color_6  <- pecanPrepare(nodes = nodes,edges = edges, edges_color = list("grey","black"),
                             edges_color_by = "width",edges_color_scaling = "fixed",
                             edges_width = "width2", edges_width_max = 110)

  expect_equal(e_color_6$edges$width,edges_v10)
  edges_width_v6 <- edges_width_v


  if(max(edges_width_v) < 10){edges_width_v6 <- edges_width_v6 * (10/(max(edges_width_v6)))}
  if(max(edges_width_v) > 10){edges_width_v6 <- edges_width_v6 / (max(edges_width_v6)/10)}

  rgb_e_6 <- colorRamp(c("grey","black"))(
    (edges_width_v6/10))
  e_i_colors_6 <- rgb(rgb_e_6[, 1], rgb_e_6[, 2], rgb_e_6[, 3], maxColorValue = 255)

  expect_equal(e_color_6$edges$color.color,e_i_colors_6)



  e_color_7 <- pecanPrepare(nodes = nodes,edges = edges, edges_color = list("grey","black"),
                            edges_color_by = "width",edges_color_max = 11,edges_color_scaling = "scaled",
                            edges_width = 1:nrow(edges))

  expect_equal(e_color_7$edges$width,1:nrow(edges))
  expect_equal(e_color_7$edges$color.color,e_i_colors_4)


  e_color_8 <- pecanPrepare(nodes = nodes,edges = edges, edges_color = list("grey","black"),
                            edges_color_by = "width",edges_color_max = 11,edges_color_scaling = "fixed",
                            edges_width = "width", edges_width_max = 30)

  expect_equal(e_color_8$edges$width,edges_custom_max)

  edges_width_v8 <- edges_width_v
  edges_width_v8  <- edges_width_v8  / (11/10)

  rgb_e_8 <- colorRamp(c("grey","black"))(
    (edges_width_v8/10))
  e_i_colors_8 <- rgb(rgb_e_8[, 1], rgb_e_8[, 2], rgb_e_8[, 3], maxColorValue = 255)

  expect_equal(e_color_8$edges$color,e_i_colors_8)

  #browser()

  #jetzt ersteinmal negative edges

  negative_edges <- edges

  negative_edges[3,"width"] <- negative_edges[3,"width",drop = TRUE] * (-1)
  negative_edges[12,"width"] <- negative_edges[12,"width",drop = TRUE] * (-1)
  negative_edges[17,"width"] <- negative_edges[17,"width",drop = TRUE] * (-1)
  negative_edges[23,"width"] <- negative_edges[23,"width",drop = TRUE] * (-1)

  #browser()

  e_color_9 <- pecanPrepare(nodes = nodes, edges = negative_edges, edges_color = list("grey","black"),
                            edges_color_by = "width",edges_width = "width")


  expect_equal(e_color_9$edges$width,edges_v4)
  expect_equal(e_color_9$edges$color.color,e_i_colors_3)


  e_color_10 <- pecanPrepare(nodes = nodes, edges = negative_edges, edges_color = list("grey","black"),
                            edges_color_by = "width", neg_edges_color = list("red","darkred"),
                            edges_width = negative_edges$width)

  expect_equal(e_color_10$edges$width,abs(negative_edges$width))

  edges_width_v10 <- edges_width_v

  if(max(edges_width_v) < 10){edges_width_v10 <- edges_width_v10 * (10/(max(edges_width_v10)))}
  if(max(edges_width_v) > 10){edges_width_v10 <- edges_width_v10 / (max(edges_width_v10)/10)}



  pos_edges_v10 <- edges_width_v10[-c(3,12,17,23)]
  neg_edges_v10 <- edges_width_v10[c(3,12,17,23)]

  #browser()

  #browser()
  rgb_e_pos_10 <- colorRamp(c("grey","black"))(
  (pos_edges_v10 - min(edges_width_v10))/(10 - min(edges_width_v10)))
   pos_colors_10 <- rgb(rgb_e_pos_10[, 1], rgb_e_pos_10[, 2], rgb_e_pos_10[, 3], maxColorValue = 255)


  rgb_e_neg_10 <- colorRamp(c("red","darkred"))(
    (neg_edges_v10 - min(edges_width_v10))/(10 - min(edges_width_v10)))
    neg_colors_10 <- rgb(rgb_e_neg_10[, 1], rgb_e_neg_10[, 2], rgb_e_neg_10[, 3], maxColorValue = 255)

 combined_colors_10 <- pos_colors_10
 combined_colors_10 <- append(combined_colors_10,neg_colors_10[1],after = 2)
 combined_colors_10 <- append(combined_colors_10,neg_colors_10[2],after = 11)
 combined_colors_10 <- append(combined_colors_10,neg_colors_10[3],after = 16)
 combined_colors_10 <- append(combined_colors_10,neg_colors_10[4],after = 22)

 expect_equal(e_color_10$edges$color.color,combined_colors_10)

 e_color_11 <- pecanPrepare(nodes = nodes, edges = negative_edges, edges_color = list("grey","black"),
                            edges_color_by = "width", neg_edges_color = list("red","darkred"),
                            edges_color_scaling = "scaled",
                            edges_width = "width2",edges_width_max = 110)

 expect_equal(e_color_11$edges$width,edges_v10)
 expect_equal(e_color_11$edges$color.color,combined_colors_10)

 e_color_12 <- pecanPrepare(nodes = nodes, edges = negative_edges, edges_color = list("grey","black"),
                            edges_color_by = "width", neg_edges_color = list("red","darkred"), edges_color_scaling = "scaled",
                            edges_color_max = 11,
                            edges_width = "width", edges_width_max = 30)


 expect_equal(e_color_12$edges$width,edges_custom_max)



 edges_width_v12 <- edges_width_v
 edges_width_v12 <- edges_width_v12 / (11/10)

 pos_edges_v12 <- edges_width_v12[-c(3,12,17,23)]
 neg_edges_v12 <- edges_width_v12[c(3,12,17,23)]



 rgb_e_pos_12 <- colorRamp(c("grey","black"))(
   (pos_edges_v12 - min(edges_width_v12))/(10 - min(edges_width_v12)))
 pos_colors_12 <- rgb(rgb_e_pos_12[, 1], rgb_e_pos_12[, 2], rgb_e_pos_12[, 3], maxColorValue = 255)


 rgb_e_neg_12 <- colorRamp(c("red","darkred"))(
   (neg_edges_v12 - min(edges_width_v12))/(10 - min(edges_width_v12)))
 neg_colors_12 <- rgb(rgb_e_neg_12[, 1], rgb_e_neg_12[, 2], rgb_e_neg_12[, 3], maxColorValue = 255)

 combined_colors_12 <- pos_colors_12
 combined_colors_12 <- append(combined_colors_12,neg_colors_12[1],after = 2)
 combined_colors_12 <- append(combined_colors_12,neg_colors_12[2],after = 11)
 combined_colors_12 <- append(combined_colors_12,neg_colors_12[3],after = 16)
 combined_colors_12 <- append(combined_colors_12,neg_colors_12[4],after = 22)

 expect_equal(e_color_12$edges$color.color,combined_colors_12)



 negative_edges13 <- edges

 negative_edges13[2,"width"] <- negative_edges13[2,"width",drop = TRUE] * (-1)
 negative_edges13[12,"width"] <- negative_edges13[12,"width",drop = TRUE] * (-1)
 negative_edges13[17,"width"] <- negative_edges13[17,"width",drop = TRUE] * (-1)
 negative_edges13[23,"width"] <- negative_edges13[23,"width",drop = TRUE] * (-1)

 #browser()


 e_color_13 <- pecanPrepare(nodes = nodes, edges = negative_edges13, edges_color = list("grey","black"),
                            edges_color_by = "width", neg_edges_color = list("red","darkred"),
                            edges_color_scaling = "fixed",edges_width = 12)

 expect_equal(all(e_color_13$edges$width == 12),TRUE)
 edges_width_v13 <- edges_width_v

 if(max(edges_width_v) < 10){edges_width_v13 <- edges_width_v13 * (10/(max(edges_width_v13)))}
 if(max(edges_width_v) > 10){edges_width_v13 <- edges_width_v13 / (max(edges_width_v13)/10)}

 pos_edges_v13 <- edges_width_v13[-c(2,12,17,23)]
 neg_edges_v13 <- edges_width_v13[c(2,12,17,23)]


 rgb_e_pos_13 <- colorRamp(c("grey","black"))((pos_edges_v13)/10)
 pos_colors_13 <- rgb(rgb_e_pos_13[, 1], rgb_e_pos_13[, 2], rgb_e_pos_13[, 3], maxColorValue = 255)


 rgb_e_neg_13 <- colorRamp(c("red","darkred"))((neg_edges_v13)/10)
 neg_colors_13 <- rgb(rgb_e_neg_13[, 1], rgb_e_neg_13[, 2], rgb_e_neg_13[, 3], maxColorValue = 255)


 combined_colors_13 <- pos_colors_13
 combined_colors_13 <- append(combined_colors_13,neg_colors_13[1],after = 1)
 combined_colors_13 <- append(combined_colors_13,neg_colors_13[2],after = 11)
 combined_colors_13 <- append(combined_colors_13,neg_colors_13[3],after = 16)
 combined_colors_13 <- append(combined_colors_13,neg_colors_13[4],after = 22)

 #browser()

 expect_equal(e_color_13$edges$color.color,combined_colors_13)


 #####################################
 negative_edges13$width3 <- negative_edges13$width * 10

 e_color_13_2 <- pecanPrepare(nodes = nodes, edges = negative_edges13, edges_color = list("grey","black"),
                            edges_color_by = "width3", neg_edges_color = list("red","darkred"),
                            edges_color_scaling = "fixed",edges_width = negative_edges13$width)


 expect_equal(e_color_13_2$edges$width,abs(negative_edges13$width))
 edges_width_v_100s <- edges_width_v*10
 edges_width_v13_2 <- edges_width_v_100s

 if(max(edges_width_v_100s) < 10){edges_width_v13_2 <- edges_width_v13_2 * (10/(max(edges_width_v13_2)))}
 if(max(edges_width_v_100s) > 10){edges_width_v13_2 <- edges_width_v13_2 / (max(edges_width_v13_2)/10)}

 pos_edges_v13_2 <- edges_width_v13_2[-c(2,12,17,23)]
 neg_edges_v13_2 <- edges_width_v13_2[c(2,12,17,23)]


 rgb_e_pos_13_2 <- colorRamp(c("grey","black"))((pos_edges_v13_2)/10)
 pos_colors_13_2 <- rgb(rgb_e_pos_13_2[, 1], rgb_e_pos_13_2[, 2], rgb_e_pos_13_2[, 3], maxColorValue = 255)


 rgb_e_neg_13_2 <- colorRamp(c("red","darkred"))((neg_edges_v13_2)/10)
 neg_colors_13_2 <- rgb(rgb_e_neg_13_2[, 1], rgb_e_neg_13_2[, 2], rgb_e_neg_13_2[, 3], maxColorValue = 255)


 combined_colors_13_2 <- pos_colors_13_2
 combined_colors_13_2 <- append(combined_colors_13_2,neg_colors_13_2[1],after = 1)
 combined_colors_13_2 <- append(combined_colors_13_2,neg_colors_13_2[2],after = 11)
 combined_colors_13_2 <- append(combined_colors_13_2,neg_colors_13_2[3],after = 16)
 combined_colors_13_2 <- append(combined_colors_13_2,neg_colors_13_2[4],after = 22)

 expect_equal(e_color_13_2$edges$color.color,combined_colors_13_2)


 ######################################

 e_color_14 <- pecanPrepare(nodes = nodes, edges = negative_edges13, edges_color = list("grey","black"),
                            edges_color_by = "width", neg_edges_color = list("red","darkred"), edges_color_scaling = "fixed",
                            edges_color_max = 11, edges_width = "width")

 expect_equal(e_color_14$edges$width,edges_v4)

 edges_width_v14 <- edges_width_v
 edges_width_v14 <- edges_width_v14 / (11/10)

 pos_edges_v14 <- edges_width_v14[-c(2,12,17,23)]
 neg_edges_v14 <- edges_width_v14[c(2,12,17,23)]


 rgb_e_pos_14 <- colorRamp(c("grey","black"))((pos_edges_v14)/10)
 pos_colors_14 <- rgb(rgb_e_pos_14[, 1], rgb_e_pos_14[, 2], rgb_e_pos_14[, 3], maxColorValue = 255)


 rgb_e_neg_14 <- colorRamp(c("red","darkred"))((neg_edges_v14)/10)
 neg_colors_14 <- rgb(rgb_e_neg_14[, 1], rgb_e_neg_14[, 2], rgb_e_neg_14[, 3], maxColorValue = 255)


 combined_colors_14 <- pos_colors_14
 combined_colors_14 <- append(combined_colors_14,neg_colors_14[1],after = 1)
 combined_colors_14 <- append(combined_colors_14,neg_colors_14[2],after = 11)
 combined_colors_14 <- append(combined_colors_14,neg_colors_14[3],after = 16)
 combined_colors_14 <- append(combined_colors_14,neg_colors_14[4],after = 22)



 expect_equal(e_color_14$edges$color.color,combined_colors_14)


 edges$same_value <- 9
 edges$same_value2 <- 10


 e_color_15 <- pecanPrepare(nodes = nodes, edges = edges, edges_color = list("grey","black"),
                            edges_color_by = "same_value",edges_width = "width", edges_width_max = 30)

 e_color_16 <- pecanPrepare(nodes = nodes, edges = edges, edges_color = list("grey","black"),
                            edges_color_by = "same_value2",edges_width = "width", edges_width_max = 30)

 e_color_17 <- pecanPrepare(nodes = nodes, edges = edges, edges_color = list("grey","black"),
                            edges_color_by = "same_value",edges_width = "width", edges_width_max = 30,
                            edges_color_max = 11)


 edges_width_v15 <- edges$same_value
 edges_width_v15 <-  edges_width_v15 * (10/(max(edges_width_v15)))

 rgb_e_15 <- colorRamp(c("grey","black"))((edges_width_v15)/10)

 e_i_colors_15 <- rgb(rgb_e_15[, 1], rgb_e_15[, 2], rgb_e_15[, 3], maxColorValue = 255)

 expect_equal(e_color_15$edges$color.color,e_i_colors_15)
 expect_equal(e_color_16$edges$color.color,e_i_colors_15)


 edges_width_v17 <- edges$same_value
 edges_width_v17 <- edges_width_v17 / (11/10)

 rgb_e_17 <- colorRamp(c("grey","black"))(
   (edges_width_v17 - min(edges_width_v17))/(10 - min(edges_width_v17)))

 e_i_colors_17 <- rgb(rgb_e_17[, 1], rgb_e_17[, 2], rgb_e_17[, 3], maxColorValue = 255)
 expect_equal(e_color_17$edges$color.color,e_i_colors_17)



 #### test filter_zero_edges function
 edges_fze <- edges
 #browser()
 edges_fze[25,1] <- NA
 edges_fze[25,1] <- 1
 edges_fze[25,2] <- 3
 edges_fze[25,3] <- 0

 test_fze <- suppressWarnings(pecanPrepare(nodes = nodes,edges = edges_fze,
                                          edges_width = "width"))

 expect_equal(nrow(test_fze$edges),nrow(edges))
 expect_warning(pecanPrepare(nodes = nodes,edges = edges_fze,
                             edges_width = "width"))
 expect_warning(pecanPrepare(nodes = nodes,edges = edges_fze,
                             edges_width = 0))
 expect_warning(pecanPrepare(nodes = nodes,edges = edges_fze,
                               edges_width = edges_fze$width))
 expect_warning(pecanPrepare(nodes = nodes,edges = edges_fze,
                             edges_width = c(rep(2,23),0,0)))
 expect_no_warning(pecanPrepare(nodes = nodes,edges = edges_fze))

 test_fze_2 <- suppressWarnings(pecanPrepare(nodes = nodes,edges = edges_fze,
                                           edges_width = edges_fze$width))

 test_fze_3 <- pecanPrepare(nodes = nodes,edges = edges_fze)

 expect_equal(nrow(test_fze_2$edges),nrow(edges))
 expect_equal(nrow(test_fze_3$edges),nrow(edges_fze))

 #test edges reg # test also with neg edges whixh should not make any diff

 reg_fze <- suppressWarnings(pecanPrepare(nodes = nodes,edges = edges_fze,edges_width = edges_fze$width,
                      edges_regulation = list(reg_by = "width", reg_type = "nodes", reg_value = 1)))

 expect_equal(nrow(reg_fze$edges),15)
 expect_equal(max(reg_fze$edges$width),7)
 expect_equal(min(reg_fze$edges$width),4)

 reg_1 <- pecanPrepare(nodes = nodes,edges = edges,edges_width = edges$width,
                       edges_regulation = list(reg_by = "width", reg_type = "nodes", reg_value = 1))



 expect_equal(nrow(reg_1$edges),15)
 expect_equal(max(reg_1$edges$width),7)
 expect_equal(min(reg_1$edges$width),4)

 reg_2 <- pecanPrepare(nodes = nodes,edges = edges,edges_width = edges$width, edges_regulation = list(reg_by = "width",
                                                                           reg_type = "nodes",
                                                                           reg_value = 0.5))



 expect_equal(nrow(reg_2$edges),9)
 expect_equal(max(reg_2$edges$width),7)
 expect_equal(min(reg_2$edges$width),5)

 reg_3 <- pecanPrepare(nodes = nodes,edges = edges,edges_width = edges$width, edges_regulation = list(reg_by = "width",
                                                                           reg_type = "nodes",
                                                                           reg_value = 1.5))

 expect_equal(nrow(reg_3$edges),22)
 expect_equal(max(reg_3$edges$width),7)
 expect_equal(min(reg_3$edges$width),3)

 reg_4 <- pecanPrepare(nodes = nodes,edges = edges,edges_width = edges$width, edges_regulation = list(reg_by = "width",
                                                                           reg_type = "number",
                                                                           reg_value = 9))

 expect_equal(nrow(reg_4$edges),9)
 expect_equal(max(reg_4$edges$width),7)
 expect_equal(min(reg_4$edges$width),5)

 reg_5 <- pecanPrepare(nodes = nodes,edges = edges,edges_width = edges$width, edges_regulation = list(reg_by = "width",
                                                                           reg_type = "number",
                                                                           reg_value = 10))

 expect_equal(nrow(reg_5$edges),15)
 expect_equal(max(reg_5$edges$width),7)
 expect_equal(min(reg_5$edges$width),4)


 reg_6 <- pecanPrepare(nodes = nodes,edges = edges,edges_width = edges$width, edges_regulation = list(reg_by = "width",
                                                                           reg_type = "number",
                                                                           reg_value = 23))

 expect_equal(nrow(reg_6$edges),24)
 expect_equal(max(reg_6$edges$width),7)
 expect_equal(min(reg_6$edges$width),2)

 reg_7 <- pecanPrepare(nodes = nodes,edges = edges,edges_width = edges$width, edges_regulation = list(reg_by = "width",
                                                                           reg_type = "value",
                                                                           reg_value = 7))

 expect_equal(nrow(reg_7$edges),2)
 expect_equal(max(reg_7$edges$width),7)
 expect_equal(min(reg_7$edges$width),7)

 #reg_8 <- pecanPrepare(nodes = nodes,edges = edges,edges_regulation = list(reg_by = "width",
  #                                                                         reg_type = "value",
  #                                                                         reg_value = 10))

 #expect_equal(nrow(reg_8$edges),0)
 #expect_equal(max(reg_8$edges$width),NA)
 #expect_equal(min(reg_8$edges$width),NA)

 reg_9 <- pecanPrepare(nodes = nodes,edges = edges,edges_width = edges$width, edges_regulation = list(reg_by = "width",
                                                                           reg_type = "value",
                                                                           reg_value = 0.1))

 expect_equal(nrow(reg_9$edges),24)
 expect_equal(max(reg_9$edges$width),7)
 expect_equal(min(reg_9$edges$width),2)

 reg_10 <- pecanPrepare(nodes = nodes,edges = edges,edges_width = edges$width, edges_regulation = list(reg_by = "width",
                                                                           reg_type = "value",
                                                                           reg_value = 4))

 expect_equal(nrow(reg_10$edges),15)
 expect_equal(max(reg_10$edges$width),7)
 expect_equal(min(reg_10$edges$width),4)

 #test whether all also works with negative edges which schould not make a diff

 reg_11 <- suppressWarnings(pecanPrepare(nodes = nodes,edges = negative_edges,edges_width = edges$width,
                                         edges_regulation = list(reg_by = "width",reg_type = "nodes",reg_value = 1)))



 expect_equal(nrow(reg_11$edges),15)
 expect_equal(max(reg_11$edges$width),7)
 expect_equal(min(reg_11$edges$width),4)

 reg_12 <- suppressWarnings(pecanPrepare(nodes = nodes,edges_width = edges$width,
                                         edges = negative_edges,edges_regulation = list(reg_by = "width",
                                                                           reg_type = "nodes",
                                                                           reg_value = 0.5)))


 expect_equal(nrow(reg_12$edges),9)
 expect_equal(max(reg_12$edges$width),7)
 expect_equal(min(reg_12$edges$width),5)

 reg_13 <- suppressWarnings(pecanPrepare(nodes = nodes,edges_width = edges$width,
                                         edges = negative_edges,edges_regulation = list(reg_by = "width",
                                                                           reg_type = "nodes",
                                                                           reg_value = 1.5)))

 expect_equal(nrow(reg_13$edges),22)
 expect_equal(max(reg_13$edges$width),7)
 expect_equal(min(reg_13$edges$width),3)

 reg_14 <- suppressWarnings(pecanPrepare(nodes = nodes,edges_width = edges$width,
                                         edges = negative_edges,edges_regulation = list(reg_by = "width",
                                                                           reg_type = "number",
                                                                           reg_value = 9)))

 expect_equal(nrow(reg_14$edges),9)
 expect_equal(max(reg_14$edges$width),7)
 expect_equal(min(reg_14$edges$width),5)

 reg_15 <- suppressWarnings(pecanPrepare(nodes = nodes,edges_width = edges$width,
                                         edges = negative_edges,edges_regulation = list(reg_by = "width",
                                                                           reg_type = "number",
                                                                           reg_value = 10)))

 expect_equal(nrow(reg_15$edges),15)
 expect_equal(max(reg_15$edges$width),7)
 expect_equal(min(reg_15$edges$width),4)


 reg_16 <- suppressWarnings(pecanPrepare(nodes = nodes,edges_width = edges$width,
                                         edges = negative_edges,edges_regulation = list(reg_by = "width",
                                                                           reg_type = "number",
                                                                           reg_value = 23)))

 expect_equal(nrow(reg_16$edges),24)
 expect_equal(max(reg_16$edges$width),7)
 expect_equal(min(reg_16$edges$width),2)

 reg_17 <- suppressWarnings(pecanPrepare(nodes = nodes,edges_width = edges$width,
                                         edges = negative_edges,edges_regulation = list(reg_by = "width",
                                                                           reg_type = "value",
                                                                           reg_value = 7)))

 expect_equal(nrow(reg_17$edges),2)
 expect_equal(max(reg_17$edges$width),7)
 expect_equal(min(reg_17$edges$width),7)

 reg_18 <- suppressWarnings(pecanPrepare(nodes = nodes,edges = negative_edges,edges_regulation = list(reg_by = "width",
                                                                          reg_type = "value",
                                                                          reg_value = 10)))


 expect_equal(nrow(reg_18$edges),0)


 reg_19 <- suppressWarnings(pecanPrepare(nodes = nodes,edges_width = edges$width,
                                         edges = negative_edges,edges_regulation = list(reg_by = "width",
                                                                           reg_type = "value",
                                                                           reg_value = 0.1)))

 expect_equal(nrow(reg_19$edges),24)
 expect_equal(max(reg_19$edges$width),7)
 expect_equal(min(reg_19$edges$width),2)

 reg_20 <- suppressWarnings(pecanPrepare(nodes = nodes,edges_width = edges$width,
                                         edges = negative_edges,edges_regulation = list(reg_by = "width",
                                                                            reg_type = "value",
                                                                            reg_value = 4)))

 expect_equal(nrow(reg_20$edges),15)
 expect_equal(max(reg_20$edges$width),7)
 expect_equal(min(reg_20$edges$width),4)

 # now combine with colors and you are almost done

 reg_21 <- pecanPrepare(nodes = nodes,edges_width = edges$width,
                        edges = edges,edges_regulation = list(reg_by = "width", reg_type = "nodes", reg_value = 1),
                        edges_color = list("grey","black"),
                        edges_color_by = "width")

 #browser()

 expect_equal(nrow(reg_21$edges),15)
 expect_equal(max(reg_21$edges$width),7)
 expect_equal(min(reg_21$edges$width),4)


 reg_col_v21 <- edges_width_v[edges_width_v > 3]


 if(max(reg_col_v21) < 10){reg_col_v21 <- reg_col_v21 * (10/(max(reg_col_v21)))}
 if(max(reg_col_v21) > 10){reg_col_v21 <- reg_col_v21 / (max(reg_col_v21)/10)}

 rgb_reg_col_21 <- colorRamp(c("grey","black"))(
   (reg_col_v21 - min(reg_col_v21))/(10 - min(reg_col_v21)))

 reg_col_i_colors_21 <- rgb(rgb_reg_col_21[, 1], rgb_reg_col_21[, 2], rgb_reg_col_21[, 3], maxColorValue = 255)

 #browser()
 expect_equal(reg_21$edges$color.color,reg_col_i_colors_21)


 reg_22 <- pecanPrepare(nodes = nodes,edges = edges,edges_width = edges$width,
                        edges_regulation = list(reg_by = "width", reg_type = "nodes", reg_value = 0.5),
                        edges_color = list("grey","black"),
                        edges_color_by = "width",edges_color_max = 11)


 expect_equal(nrow(reg_22$edges),9)
 expect_equal(max(reg_22$edges$width),7)
 expect_equal(min(reg_22$edges$width),5)


 reg_col_v22 <- edges_width_v[edges_width_v > 4]
 reg_col_v22 <- reg_col_v22 / (11/10)

 rgb_reg_col_22 <- colorRamp(c("grey","black"))(
   (reg_col_v22 - min(reg_col_v22))/(10 - min(reg_col_v22)))

 reg_col_i_colors_22 <- rgb(rgb_reg_col_22[, 1], rgb_reg_col_22[, 2], rgb_reg_col_22[, 3], maxColorValue = 255)

 #browser()
 expect_equal(reg_22$edges$color.color,reg_col_i_colors_22)


 reg_23 <- pecanPrepare(nodes = nodes,edges = edges,
                        edges_regulation = list(reg_by = "width", reg_type = "nodes", reg_value = 0.5),
                        edges_color = list("grey","black"),
                        edges_color_by = "width",edges_color_max = 11,edges_color_scaling = "scaled")


 expect_equal(reg_23$edges$color.color,reg_col_i_colors_22)

# browser()

 ###### Combination of regulation, color and wdith.

 reg_24 <- pecanPrepare(nodes = nodes,edges = edges,edges_width = "width",
                       edges_regulation = list(reg_by = "width", reg_type = "nodes", reg_value = 1),
                       edges_color_by = "width",edges_color = list("grey","black")
                       )


 edges_w_max_24 <- max(edges$width)
 edges_v24 <- edges$width

 if(edges_w_max_24 < 10){edges_v24  <- edges_v24 * (10/edges_w_max_24)}
 if(edges_w_max_24 > 10){edges_v24  <- edges_v24 / (edges_w_max_24/10)}


 edges_v24_1 <- edges_v24[edges$width >= 4]



 rgb_e_24 <- colorRamp(c("grey","black"))(
   (edges_v24_1 - min(edges_v24_1))/(10 - min(edges_v24_1)))

 e_i_colors_24 <- rgb(rgb_e_24[, 1], rgb_e_24[, 2], rgb_e_24[, 3], maxColorValue = 255)

 #browser()

 expect_equal(nrow(reg_24$edges),15)
 expect_equal(reg_24$edges$width,edges_v24_1)
 expect_equal(reg_24$edges$color.color,e_i_colors_24)

 reg_25 <- pecanPrepare(nodes = nodes,edges = edges,edges_width = "width",
                        edges_regulation = list(reg_by = "width", reg_type = "nodes", reg_value = 1),
                        edges_color_by = "width",edges_color = list("grey","black"),
                        edges_color_scaling = "fixed")


 edges_w_max_25 <- max(edges$width)
 edges_v25 <- edges$width

 if(edges_w_max_25 < 10){edges_v25  <- edges_v25 * (10/edges_w_max_25)}
 if(edges_w_max_25 > 10){edges_v25  <- edges_v25 / (edges_w_max_25/10)}


 edges_v25_1 <- edges_v25[edges$width >= 4]



 rgb_e_25 <- colorRamp(c("grey","black"))((edges_v25_1)/10)

 e_i_colors_25 <- rgb(rgb_e_25[, 1], rgb_e_25[, 2], rgb_e_25[, 3], maxColorValue = 255)

 #browser()

 expect_equal(nrow(reg_25$edges),15)
 expect_equal(reg_25$edges$width,edges_v25_1)
 expect_equal(reg_25$edges$color.color,e_i_colors_25)


 reg_26 <- pecanPrepare(nodes = nodes,edges = edges,edges_width = "width",
                        edges_regulation = list(reg_by = "width", reg_type = "nodes", reg_value = 1),
                        edges_color_by = "width",edges_color = list("grey","black"),
                        edges_color_scaling = "scaled",edges_color_max = 11, edges_width_max = 12)


 edges_w_max_26 <- max(edges$width)
 edges_v26 <- edges$width
 edges_v26 <- edges_v26[edges$width >= 4]

 edges_v26_w  <- edges_v26 / (12/10)
 edges_v26_c  <- edges_v26 / (11/10)


 rgb_e_26 <- colorRamp(c("grey","black"))(
   (edges_v26_c - min(edges_v26_c))/(10 - min(edges_v26_c)))

 e_i_colors_26 <- rgb(rgb_e_26[, 1], rgb_e_26[, 2], rgb_e_26[, 3], maxColorValue = 255)
 #browser()


 expect_equal(nrow(reg_26$edges),15)
 expect_equal(reg_26$edges$width,edges_v26_w)
 expect_equal(reg_26$edges$color.color,e_i_colors_26)


 #ein fze noch mit color einbringen!
 reg_27 <- suppressWarnings(pecanPrepare(nodes = nodes,edges = edges_fze,edges_width = "width",
                        edges_regulation = list(reg_by = "width", reg_type = "nodes", reg_value = 1),
                        edges_color_by = "width",edges_color = list("grey","black"),
                        edges_color_scaling = "scaled",edges_color_max = 11, edges_width_max = 12))


 expect_equal(nrow(reg_27$edges),15)
 expect_equal(reg_27$edges$width,edges_v26_w)
 expect_equal(reg_27$edges$color.color,e_i_colors_26)


#### test also when all edges and all nodes have th esame value for color!





})
