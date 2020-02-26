require('igraph')

#' @author Felipe González-Casabianca
#' Mapper Tests

# Size of test
percetange_of_test = 1


# Support Funtion
is_circle = function(one_squeleton)
{
  # Method that checks if the result graph forms a circle or not
  # Checks that all nodes have exactly two neighbors and that there is one connected component

  # Constructs the graph
  g = graph.adjacency(one_squeleton$adjacency_matrix, mode = 'undirected')

  # Checks that all nodes have two neighbors
  if(any(degree(g) != 2))
    return(FALSE)
  # Checks that there is only on connected componnent
  if(components(g)$no != 1)
    return(FALSE)

  # Is Circle
  return(TRUE)
}


context("MapperKD Tests. Toy Examples. 1 Dimension")

# Toy examples
test_that("Toy Examples. 1 Dimension", {

  # In following examples the distance matrix can be mock because the cluster method is: cluster all

  # Multiple overlapping intervals over same point
  # When only one point is included (or multiple with same filter), the output graph must be a clique

  num_ite = 100*percetange_of_test
  #num_ite = 1
  for(i in 1:num_ite)
  {
    num_points = sample(2:10, 1)
    filter = rep(1,num_points)
    intervals = sample(2:10, 1)
    overlap = sample(1:99, 1)

    res = mapperKD(k = 1,
                   distance = matrix(rep(1,length(filter)**2), ncol = length(filter)),
                   filter = filter,
                   intervals = intervals,
                   overlap = overlap,
                   clustering_method = cluster_all)

    expect_equal( res$num_nodes, intervals )
    for(lev in res$points_in_nodes)
      expect_equal( length(lev), num_points )

    expect_equal( sum(res$adjacency_matrix), res$num_nodes**2 - res$num_nodes )

  }

  # Two extree point and random in the middle. With overlap above 95% should be clique
  num_ite = 100*percetange_of_test
  for(i in 1:num_ite)
  {
    num_points = 2 + sample(2:10, 1)
    filter = c(0, rep(1,num_points -2), 2)
    intervals = sample(2:10, 1)
    overlap = sample(95:99, 1)

    res = mapperKD(k = 1,
                   distance = matrix(rep(1,length(filter)**2), ncol = length(filter)),
                   filter = filter,
                   intervals = intervals,
                   overlap = overlap,
                   clustering_method = cluster_all)

    # Every interval has a point
    expect_equal( res$num_nodes, intervals )
    # Is clique
    expect_equal( sum(res$adjacency_matrix), res$num_nodes**2 - res$num_nodes )

  }

  # 3 points. 2 Intervals. Cluster all
  filter = c(1,2,3)
  intervals = 2
  overlap = 20
  res = mapperKD(k = 1,
                 distance = matrix(rep(1,length(filter)**2), ncol = length(filter)),
                 filter = filter,
                 intervals = intervals,
                 overlap = overlap,
                 clustering_method = cluster_all)

  expect_equal( res$num_nodes, 2 )
  expect_equal( length(res$points_in_nodes[[1]]), 2 )
  expect_equal( length(res$points_in_nodes[[2]]), 2 )
  expect_equal( sum(res$adjacency_matrix), 2 )

  # 4 points. 2 Intervals (no overlap). Cluster all
  filter = c(1,2,3,4)
  intervals = 2
  overlap = 20
  res = mapperKD(k = 1,
                 distance = matrix(rep(1,length(filter)**2), ncol = length(filter)),
                 filter = filter,
                 intervals = intervals,
                 overlap = overlap,
                 clustering_method = cluster_all)

  expect_equal( res$num_nodes, 2 )
  expect_equal( length(res$points_in_nodes[[1]]), 2 )
  expect_equal( length(res$points_in_nodes[[2]]), 2 )
  expect_equal( sum(res$adjacency_matrix), 0 )


})


context("MapperKD Tests. Toy Examples. 2 Dimensions. Circle. Filter 2D")

# Toy examples
test_that("Toy Examples. 2 Dimension. Circle 2D", {

  # Simple Circle
  num_points = 100
  points = cbind(cos(1:num_points), sin(1:num_points))
  filter = points
  intervals = c(6,6)
  overlap = c(30,30)
  res = mapperKD(k = 2,
                 distance = as.matrix(dist(points)),
                 filter = filter,
                 intervals = intervals,
                 overlap = overlap,
                 clustering_method = cluster_all)


  expect_true(is_circle(res))

})

context("MapperKD Tests. Toy Examples. 2 Dimensions. Circle. Filter 1D")

# Toy examples
test_that("Toy Examples. 2 Dimension. Circle 1D", {

  # Simple Circle
  # Y coordinate
  num_points = 100
  points = cbind(cos(1:num_points), sin(1:num_points))
  filter = points[,2]
  intervals = c(6)
  overlap = c(40)
  res = mapperKD(k = 1,
                 distance = as.matrix(dist(points)),
                 filter = filter,
                 intervals = intervals,
                 overlap = overlap,
                 clustering_method = hierarchical_clustering)


  expect_true(is_circle(res))


  # x coordinate
  num_points = 100
  points = cbind(cos(1:num_points), sin(1:num_points))
  filter = points[,1]
  intervals = c(6)
  overlap = c(40)
  res = mapperKD(k = 1,
                 distance = as.matrix(dist(points)),
                 filter = filter,
                 intervals = intervals,
                 overlap = overlap,
                 clustering_method = hierarchical_clustering)


  expect_true(is_circle(res))

})

