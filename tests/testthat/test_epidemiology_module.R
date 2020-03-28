# Epidemiology Module Test

# Test the convert to graph function

context("Epidemiology Tests. Function: Convert to Graph")

# Toy examples
test_that("Epidemiology Tests. Function: Convert to Graph", {

  # Graph with no adjacency
  one_skeleton_result = list()
  one_skeleton_result$adjacency_matrix = matrix(c(0,0,0,0), ncol = 2)
  g = convert_to_graph(one_skeleton_result)
  expect_equal(length(V(g)), 2)
  expect_equal(length(E(g)), 0)


  # Graph with  adjacency
  one_skeleton_result = list()
  one_skeleton_result$adjacency_matrix = matrix(c(0,1,1,0), ncol = 2)
  g = convert_to_graph(one_skeleton_result)
  expect_equal(length(V(g)), 2)
  expect_equal(length(E(g)), 1)

  # Graph with  adjacency should be directed
  one_skeleton_result = list()
  one_skeleton_result$adjacency_matrix = matrix(c(0,1,0,0), ncol = 2)
  g = convert_to_graph(one_skeleton_result)
  expect_equal(length(V(g)), 2)
  expect_equal(length(E(g)), 1)

})




context("Epidemiology Tests. Function: Get One Skeleton Node sizes")

# Toy examples
test_that("Epidemiology Tests. Function: Get One Skeleton Node sizes", {

  # Toy Examples
  one_skeleton_result = list()
  one_skeleton_result$points_in_nodes = list()
  one_skeleton_result$points_in_nodes[[1]] = c(1)
  one_skeleton_result$points_in_nodes[[2]] = c(1)
  one_skeleton_result$points_in_nodes[[3]] = c(1)
  one_skeleton_result$points_in_nodes[[4]] = c(1)

  resp = get_1_skeleton_node_sizes(one_skeleton_result)
  expect_equal(resp, c(1,1,1,1))


  one_skeleton_result = list()
  one_skeleton_result$points_in_nodes = list()
  one_skeleton_result$points_in_nodes[[1]] = c(1)
  one_skeleton_result$points_in_nodes[[2]] = c(1,2)
  one_skeleton_result$points_in_nodes[[3]] = c(1,2,3)
  one_skeleton_result$points_in_nodes[[4]] = c(1,2,3,4)

  resp = get_1_skeleton_node_sizes(one_skeleton_result)
  expect_equal(resp, c(1,2,3,4))

  one_skeleton_result = list()
  one_skeleton_result$points_in_nodes = list()
  one_skeleton_result$points_in_nodes[[1]] = c(1)
  one_skeleton_result$points_in_nodes[[2]] = c(1,1)
  one_skeleton_result$points_in_nodes[[3]] = c(1,1,1)
  one_skeleton_result$points_in_nodes[[4]] = c(1,1,1,1)

  resp = get_1_skeleton_node_sizes(one_skeleton_result)
  expect_equal(resp, c(1,2,3,4))


})



context("Epidemiology Tests. Function: Construct Grid Graph Layout")

# Toy examples
test_that("Epidemiology Tests. Function: Construct Grid Graph Layout", {

  # TODO

})


context("Epidemiology Tests. Function: Get Filter Dimension")

# Toy examples
test_that("Epidemiology Tests. Function:  Get Filter Dimension", {

  # Dimension 1
  one_skeleton_result = list()
  one_skeleton_result$nodes_per_interval = list()
  one_skeleton_result$nodes_per_interval[[1]] = c(1)
  one_skeleton_result$nodes_per_interval[[2]] = c(1,2)

  resp = get_filter_dimension(one_skeleton_result)
  expect_equal(resp, 1)

  # Dimension 2
  one_skeleton_result = list()
  one_skeleton_result$nodes_per_interval = list()
  one_skeleton_result$nodes_per_interval[[1]] = list()
  one_skeleton_result$nodes_per_interval[[1]][[1]] = c(1,2)
  one_skeleton_result$nodes_per_interval[[1]][[2]] = c(1,2)

  one_skeleton_result$nodes_per_interval[[2]] = list()
  one_skeleton_result$nodes_per_interval[[2]][[1]] = c(1,2)
  one_skeleton_result$nodes_per_interval[[2]][[2]] = c(1,2)

  resp = get_filter_dimension(one_skeleton_result)
  expect_equal(resp, 2)

  # Dimension 3
  one_skeleton_result = list()
  one_skeleton_result$nodes_per_interval = list()
  one_skeleton_result$nodes_per_interval[[1]] = list()
  one_skeleton_result$nodes_per_interval[[1]][[1]] = list()
  one_skeleton_result$nodes_per_interval[[1]][[1]][[1]] = c(1,2)

  one_skeleton_result$nodes_per_interval[[2]] = list()
  one_skeleton_result$nodes_per_interval[[2]][[1]] = list()
  one_skeleton_result$nodes_per_interval[[2]][[1]][[1]] = c(1,2)

  resp = get_filter_dimension(one_skeleton_result)
  expect_equal(resp, 3)

})


context("Epidemiology Tests. Function: Extract Intersection Centrality")


test_that("Epidemiology Tests. Function: Extract Intersection Centrality", {

  # Disconnected nodes
  one_skeleton_result = list()
  one_skeleton_result$points_in_nodes = list()
  one_skeleton_result$points_in_nodes[[1]] = c(1)
  one_skeleton_result$points_in_nodes[[2]] = c(2)
  one_skeleton_result$points_in_nodes[[3]] = c(3)

  resp = extract_intersection_centrality(one_skeleton_result)
  expect_equal(resp, c(1,1,1))

  # Connected nodes clique
  one_skeleton_result = list()
  one_skeleton_result$points_in_nodes = list()
  one_skeleton_result$points_in_nodes[[1]] = c(1,2,3)
  one_skeleton_result$points_in_nodes[[2]] = c(1,2,3)
  one_skeleton_result$points_in_nodes[[3]] = c(1,2,3)

  resp = extract_intersection_centrality(one_skeleton_result)
  expect_equal(resp, c(3,3,3))


  # Connected nodes example 1
  one_skeleton_result = list()
  one_skeleton_result$points_in_nodes = list()
  one_skeleton_result$points_in_nodes[[1]] = c(1,2)
  one_skeleton_result$points_in_nodes[[2]] = c(2)
  one_skeleton_result$points_in_nodes[[3]] = c(2,3)

  resp = extract_intersection_centrality(one_skeleton_result)
  expect_equal(resp, c(1,3,1))

  # Connected nodes example 2
  one_skeleton_result = list()
  one_skeleton_result$points_in_nodes = list()
  one_skeleton_result$points_in_nodes[[1]] = c(2,1)
  one_skeleton_result$points_in_nodes[[2]] = c(1)
  one_skeleton_result$points_in_nodes[[3]] = c(3,1)
  one_skeleton_result$points_in_nodes[[4]] = c(4)

  resp = extract_intersection_centrality(one_skeleton_result)
  expect_equal(resp, c(3,1,1,1))

})


context("Epidemiology Tests. Function: Create Point Intersection Adjacency")


test_that("Epidemiology Tests. Function: Create Point Intersection Adjacency", {

  # Completely Disconnected (no edges in one skeleton)
  one_skeleton_result = list()
  one_skeleton_result$num_nodes = 3
  one_skeleton_result$points_in_nodes = list()
  one_skeleton_result$points_in_nodes[[1]] = c(1)
  one_skeleton_result$points_in_nodes[[2]] = c(2)
  one_skeleton_result$points_in_nodes[[3]] = c(3)

  resp = create_point_intersection_adjacency(one_skeleton_result)
  expect_equal(dim(resp), c(3,3))
  expect_equal(sum(resp), 0)

  # Completely Disconnected (full clique in one skeleton)
  one_skeleton_result = list()
  one_skeleton_result$num_nodes = 4
  one_skeleton_result$points_in_nodes = list()
  one_skeleton_result$points_in_nodes[[1]] = c(1,2,3,4)
  one_skeleton_result$points_in_nodes[[2]] = c(1,2,3,4)
  one_skeleton_result$points_in_nodes[[3]] = c(1,2,3,4)
  one_skeleton_result$points_in_nodes[[4]] = c(1,2,3,4)

  resp = create_point_intersection_adjacency(one_skeleton_result)
  expect_equal(dim(resp), c(4,4))
  expect_equal(sum(resp), 0)

  # Network as a star (single central element)
  one_skeleton_result = list()
  one_skeleton_result$num_nodes = 3
  one_skeleton_result$points_in_nodes = list()
  one_skeleton_result$points_in_nodes[[1]] = c(1,2,3)
  one_skeleton_result$points_in_nodes[[2]] = c(2,3,4)
  one_skeleton_result$points_in_nodes[[3]] = c(3,4)

  resp = create_point_intersection_adjacency(one_skeleton_result)
  expect_equal(dim(resp), c(4,4))
  expect_equal(sum(diag(resp)),0)
  expect_equal(resp[1,2], 0)
  expect_equal(resp[1,3], 1)
  expect_equal(resp[1,4], 0)
  expect_equal(resp[2,1], 0)
  expect_equal(resp[2,3], 1)
  expect_equal(resp[2,4], 0)
  expect_equal(resp[3,1], 0)
  expect_equal(resp[3,2], 0)
  expect_equal(resp[3,4], 0)
  expect_equal(resp[4,1], 0)
  expect_equal(resp[4,2], 0)
  expect_equal(resp[4,3], 1)

  # Network as a star (double central element)
  one_skeleton_result = list()
  one_skeleton_result$num_nodes = 3
  one_skeleton_result$points_in_nodes = list()
  one_skeleton_result$points_in_nodes[[1]] = c(1,2,3)
  one_skeleton_result$points_in_nodes[[2]] = c(2,3)
  one_skeleton_result$points_in_nodes[[3]] = c(2,3,4)

  resp = create_point_intersection_adjacency(one_skeleton_result)
  expect_equal(dim(resp), c(4,4))
  expect_equal(sum(diag(resp)),0)
  expect_equal(resp[1,2], 1)
  expect_equal(resp[1,3], 1)
  expect_equal(resp[1,4], 0)
  expect_equal(resp[2,1], 0)
  expect_equal(resp[2,3], 0)
  expect_equal(resp[2,4], 0)
  expect_equal(resp[3,1], 0)
  expect_equal(resp[3,2], 0)
  expect_equal(resp[3,4], 0)
  expect_equal(resp[4,1], 0)
  expect_equal(resp[4,2], 1)
  expect_equal(resp[4,3], 1)


  # Network as a double star (single central element)
  one_skeleton_result = list()
  one_skeleton_result$num_nodes = 4
  one_skeleton_result$points_in_nodes = list()
  one_skeleton_result$points_in_nodes[[1]] = c(1,2,3)
  one_skeleton_result$points_in_nodes[[2]] = c(3)
  one_skeleton_result$points_in_nodes[[3]] = c(4,5,6)
  one_skeleton_result$points_in_nodes[[4]] = c(6)

  resp = create_point_intersection_adjacency(one_skeleton_result)
  expect_equal(dim(resp), c(4,4))
  expect_equal(sum(diag(resp)),0)
  expect_equal(resp[1,2], 0)
  expect_equal(resp[1,3], 1)
  expect_equal(resp[1,4], 0)
  expect_equal(resp[1,5], 0)
  expect_equal(resp[1,6], 0)

  expect_equal(resp[2,1], 0)
  expect_equal(resp[2,3], 1)
  expect_equal(resp[2,4], 0)
  expect_equal(resp[2,5], 0)
  expect_equal(resp[2,6], 0)

  expect_equal(resp[3,1], 0)
  expect_equal(resp[3,2], 0)
  expect_equal(resp[3,4], 0)
  expect_equal(resp[3,5], 0)
  expect_equal(resp[3,6], 0)

  expect_equal(resp[4,1], 0)
  expect_equal(resp[4,2], 0)
  expect_equal(resp[4,3], 0)
  expect_equal(resp[4,5], 0)
  expect_equal(resp[4,6], 1)

  expect_equal(resp[5,1], 0)
  expect_equal(resp[5,2], 0)
  expect_equal(resp[5,3], 0)
  expect_equal(resp[5,4], 0)
  expect_equal(resp[5,6], 1)

  expect_equal(resp[6,1], 0)
  expect_equal(resp[6,2], 0)
  expect_equal(resp[6,3], 0)
  expect_equal(resp[6,4], 0)
  expect_equal(resp[6,5], 0)

})


