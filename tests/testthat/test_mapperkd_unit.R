
#' @author Felipe González-Casabianca
#' Equivalent of unit tests for the library. Tests the algorithm on very simple cases that
#' try to isolate different sections of the algorithm (and any support methods)


percetange_of_test = 0.1

# TODO: create scenarios for errors


context("MapperKD Unit Tests. Support Functions")
# Test for function: advance_coordinates
test_that("Unit Tests for: advance_coordinates", {

  # Case 1
  initial_coordinates = c(4,5,5)
  initial_max_values = c(5,5,5)

  response = advance_coordinates(initial_coordinates, initial_max_values)
  expected = c(5,5,5)

  expect_equal( response, expected )

  # Case 2
  initial_coordinates = c(5,4,5)
  initial_max_values = c(5,5,5)

  response = advance_coordinates(initial_coordinates, initial_max_values)
  expected = c(1,5,5)

  expect_equal( response, expected )

  # Case 3
  initial_coordinates = c(1,1,1)
  initial_max_values = c(5,5,5)

  response = advance_coordinates(initial_coordinates, initial_max_values)
  expected = c(2,1,1)

  expect_equal( response, expected )

  # Case 4
  initial_coordinates = c(5,5,5)
  initial_max_values = c(5,5,5)

  response = advance_coordinates(initial_coordinates, initial_max_values)
  expected = c(1,1,1)

  expect_equal( response, expected )

})


context("MapperKD Unit Tests. Toy Examples. 1 Dimension")

# Toy examples
test_that("Toy Examples. 1 Dimension", {

  # In following examples the distance matrix can be mock because the cluster method is: cluster all

  #Multiple overlapping intervals over same point
  # When only one point is included (or multiple with same filter), the output graph must be a clique

  num_ite = 100*percetange_of_test
  num_ite = 1
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
                     clustering_method = cluster_all,
                     low_ram = FALSE,
                     data = NA)

      expect_equal( res$num_nodes, intervals )
      for(lev in res$points_in_nodes)
        expect_equal( length(lev), num_points )

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
           clustering_method = cluster_all,
           low_ram = FALSE,
           data = NA)

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
                 clustering_method = cluster_all,
                 low_ram = FALSE,
                 data = NA)

  expect_equal( res$num_nodes, 2 )
  expect_equal( length(res$points_in_nodes[[1]]), 2 )
  expect_equal( length(res$points_in_nodes[[2]]), 2 )
  expect_equal( sum(res$adjacency_matrix), 0 )


})







