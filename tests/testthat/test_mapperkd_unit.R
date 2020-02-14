
#' @author Felipe González-Casabianca
#' Equivalent of unit tests for the library. Tests the algorithm on very simple cases that
#' try to isolate different sections of the algorithm (and any support methods)

# Size of test
percetange_of_test = 0.1

context("MapperKD Unit Tests. Support Functions. Advance Coordinates")

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


context("MapperKD Unit Tests. Support Functions. Construct Step Grid. Dimension 1")
# Test for function: advance_coordinates
test_that("Unit Tests for: Construct Step Grid. Dimension 1", {


  # The structure of the test will be:
  # Sample points in an 1 dimensinal space. Invokes mapperKD and will only work with the: nodes_per_interval
  # attribute. Will check the intersection by grid search and by brute force. Both should be the same

  # Case 1. n = 1.
  # Toy case
  N = 30
  filter = runif(N, -100, 100)
  filter_min = min(filter)
  filter_max = max(filter)
  intervals = 4
  overlap = 30

  step_grid = construct_step_grid(filter_min, filter_max, intervals, overlap)
  res = mapperKD(k = 1, distance = matrix(rep(1,length(filter)**2), ncol = length(filter)), filter = filter, intervals = intervals, overlap = overlap, clustering_method = cluster_all, local_distance = FALSE, data = NA)

  # Complete grid
  grid = as.matrix(expand.grid(lapply(intervals, function(i){1:i})))

  for(i in 1:nrow(grid))
  {
    current_id = grid[i,]

    # Searches for intersection using brute force. Saves as strings
    brute_results = c()

    for(j in 1:nrow(grid))
    {
      other_id = grid[j,]
      intersection = intersect(res$points_per_interval[[current_id]], res$points_per_interval[[other_id]])
      ahead = all(other_id >= current_id) & i != j
      if(length(intersection) > 0 & ahead)
        brute_results = c(brute_results, paste(other_id, sep = ','))
    }

    # Constructs possible intersections by step_grid
    step_results = sapply(1:nrow(step_grid), function(j){paste(current_id + step_grid[j,], sep = ',')})

    expect_equal(length(setdiff(brute_results, step_results)), 0)

  }

  # Case 2. n = 1.
  # multiple Random cases

  num_ite = 100*percetange_of_test
  #num_ite = 1
  for(k in 1:num_ite)
  {

    N = sample(10:250, 1)
    filter = runif(N, -100, 100)
    filter_min = min(filter)
    filter_max = max(filter)
    intervals = sample(1:20, 1)
    overlap = sample(1:99, 1)

    step_grid = construct_step_grid(filter_min, filter_max, intervals, overlap)
    res = mapperKD(k = 1, distance = matrix(rep(1,length(filter)**2), ncol = length(filter)), filter = filter, intervals = intervals, overlap = overlap, clustering_method = cluster_all, local_distance = FALSE, data = NA)

    # Complete grid
    grid = as.matrix(expand.grid(lapply(intervals, function(i){1:i})))

    for(i in 1:nrow(grid))
    {
      current_id = grid[i,]

      # Searches for intersection using brute force. Saves as strings
      brute_results = c()

      for(j in 1:nrow(grid))
      {
        other_id = grid[j,]
        intersection = intersect(res$points_per_interval[[current_id]], res$points_per_interval[[other_id]])
        ahead = all(other_id >= current_id) & i != j
        if(length(intersection) > 0 & ahead)
          brute_results = c(brute_results, paste(other_id, sep = ','))
      }

      # Constructs possible intersections by step_grid
      step_results = sapply(1:nrow(step_grid), function(j){paste(current_id + step_grid[j,], sep = ',')})

      # Tests
      expect_equal(length(setdiff(brute_results, step_results)), 0)

    }

  }


})



context("MapperKD Unit Tests. Support Functions. Construct Step Grid. Dimension 2")
# Test for function: advance_coordinates
test_that("Unit Tests for: Construct Step Grid. Dimesnion 2", {


  # The structure of the test will be:
  # Sample points in an 2 dimensinal space. Invokes mapperKD and will only work with the: nodes_per_interval
  # attribute. Will check the intersection by grid search and by brute force. Both should be the same

  # Case 1. n = 2.
  # Toy case
  N = 30
  filter = cbind(runif(N, -100, 100), runif(N, -100, 100))
  filter_min = apply(filter,2, min)
  filter_max = apply(filter,2, max)
  intervals = c(4,4)
  overlap = c(30,55)

  step_grid = construct_step_grid(filter_min, filter_max, intervals, overlap)
  res = mapperKD(k = 2, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, intervals = intervals, overlap = overlap, clustering_method = cluster_all, local_distance = FALSE, data = NA)

  # Complete grid
  grid = as.matrix(expand.grid(lapply(intervals, function(i){1:i})))

  for(i in 1:nrow(grid))
  {
    current_id = grid[i,]

    # Searches for intersection using brute force. Saves as strings
    brute_results = c()

    for(j in 1:nrow(grid))
    {
      other_id = grid[j,]
      intersection = intersect(res$points_per_interval[[current_id]], res$points_per_interval[[other_id]])
      ahead = all(other_id >= current_id) & i != j
      if(length(intersection) > 0 & ahead)
        brute_results = c(brute_results, paste(other_id, collapse = ','))
    }

    # Constructs possible intersections by step_grid
    step_results = sapply(1:nrow(step_grid), function(j){paste(current_id + step_grid[j,], collapse = ',')})

    # Tests
    expect_equal(length(setdiff(brute_results, step_results)), 0)

  }

  # Case 2. n = 1.
  # multiple Random cases

  num_ite = 30*percetange_of_test
  #num_ite = 1
  for(k in 1:num_ite)
  {

    N = sample(10:250, 1)
    filter = cbind(runif(N, -100, 100), runif(N, -100, 100))
    filter_min = min(filter)
    filter_max = max(filter)
    filter_min = apply(filter,2, min)
    filter_max = apply(filter,2, max)
    intervals = sample(1:20, 2)
    overlap = sample(1:99, 2)


    step_grid = construct_step_grid(filter_min, filter_max, intervals, overlap)
    res = mapperKD(k = 2, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, intervals = intervals, overlap = overlap, clustering_method = cluster_all, local_distance = FALSE, data = NA)

    # Complete grid
    grid = as.matrix(expand.grid(lapply(intervals, function(i){1:i})))

    for(i in 1:nrow(grid))
    {
      current_id = grid[i,]

      # Searches for intersection using brute force. Saves as strings
      brute_results = c()

      for(j in 1:nrow(grid))
      {
        other_id = grid[j,]
        intersection = intersect(res$points_per_interval[[current_id]], res$points_per_interval[[other_id]])
        ahead = all(other_id >= current_id) & i != j
        if(length(intersection) > 0 & ahead)
          brute_results = c(brute_results, paste(other_id, collapse = ','))
      }

      # Constructs possible intersections by step_grid
      step_results = sapply(1:nrow(step_grid), function(j){paste(current_id + step_grid[j,], collapse = ',')})

      # Tests
      expect_equal(length(setdiff(brute_results, step_results)), 0)

    }
  }

})


context("MapperKD Unit Tests. Support Functions. Construct Step Grid. Dimension n")
# Test for function: advance_coordinates
test_that("Unit Tests for: Construct Step Grid. Dimesnion n", {


  # The structure of the test will be:
  # Sample points in a n dimensinal space. Invokes mapperKD and will only work with the: nodes_per_interval
  # attribute. Will check the intersection by grid search and by brute force. Both should be the same

  # Case 1. n = 3.
  # Toy case
  N = 30
  filter = cbind(runif(N, -100, 100), runif(N, -100, 100), runif(N, -100, 100))
  filter_min = apply(filter,2, min)
  filter_max = apply(filter,2, max)
  intervals = c(4,4,4)
  overlap = c(30,55, 70)

  step_grid = construct_step_grid(filter_min, filter_max, intervals, overlap)
  res = mapperKD(k = 3, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, intervals = intervals, overlap = overlap, clustering_method = cluster_all, local_distance = FALSE, data = NA)

  # Complete grid
  grid = as.matrix(expand.grid(lapply(intervals, function(i){1:i})))

  for(i in 1:nrow(grid))
  {
    current_id = grid[i,]

    # Searches for intersection using brute force. Saves as strings
    brute_results = c()

    for(j in 1:nrow(grid))
    {
      other_id = grid[j,]
      intersection = intersect(res$points_per_interval[[current_id]], res$points_per_interval[[other_id]])
      ahead = all(other_id >= current_id) & i != j
      if(length(intersection) > 0 & ahead)
        brute_results = c(brute_results, paste(other_id, collapse = ','))
    }

    # Constructs possible intersections by step_grid
    step_results = sapply(1:nrow(step_grid), function(j){paste(current_id + step_grid[j,], collapse = ',')})

    # Tests
    expect_equal(length(setdiff(brute_results, step_results)), 0)

  }

  # Case 2. n = random.
  # multiple Random cases

  num_ite = 10*percetange_of_test
  #num_ite = 1
  for(k in 1:num_ite)
  {
    n = sample(3:4, 1) # DImension
    N = sample(10:50, 1)

    filter = c()
    for(i in 1:n)
      filter = cbind(filter, runif(N, -100, 100))

    filter_min = min(filter)
    filter_max = max(filter)
    filter_min = apply(filter,2, min)
    filter_max = apply(filter,2, max)

    intervals = c()
    overlap = c()

    for(i in 1:n)
    {
      intervals = c(intervals, sample(2:4, 1))
      overlap = c(overlap, sample(1:75, 1))
    }


    step_grid = construct_step_grid(filter_min, filter_max, intervals, overlap)
    res = mapperKD(k = n, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, intervals = intervals, overlap = overlap, clustering_method = cluster_all, local_distance = FALSE, data = NA)

    # Complete grid
    grid = as.matrix(expand.grid(lapply(intervals, function(i){1:i})))

    for(i in 1:nrow(grid))
    {
      current_id = grid[i,]

      # Searches for intersection using brute force. Saves as strings
      brute_results = c()

      for(j in 1:nrow(grid))
      {
        other_id = grid[j,]
        intersection = intersect(res$points_per_interval[[current_id]], res$points_per_interval[[other_id]])
        ahead = all(other_id >= current_id) & i != j
        if(length(intersection) > 0 & ahead)
          brute_results = c(brute_results, paste(other_id, collapse = ','))
      }

      # Constructs possible intersections by step_grid
      step_results = sapply(1:nrow(step_grid), function(j){paste(current_id + step_grid[j,], collapse = ',')})

      # Tests
      expect_equal(length(setdiff(brute_results, step_results)), 0)

    }
  }

})


context("MapperKD Unit Tests. Error Scenarios")
# Test scnearios that respond with errors
test_that("MapperKD Unit Tests. Error Scenarios", {

  # General parameters
  intervals = 2
  overlap = 20
  filter = c(1,2,3)

  # Bad filter
  filter = c(1,2,3)
  # --- FIlter One dimensional OK (k = 1)
  expect_true({mapperKD(k = 1, distance = matrix(rep(1,length(filter)**2), ncol = length(filter)), filter = filter, intervals = intervals, overlap = overlap, clustering_method = cluster_all, local_distance = FALSE, data = NA); TRUE})
  # --- FIlter One dimensional not OK (k != 1)
  expect_error(mapperKD(k = 2, distance = matrix(rep(1,length(filter)**2), ncol = length(filter)), filter = filter, intervals = intervals, overlap = overlap, clustering_method = cluster_all, local_distance = FALSE, data = NA))
  # --- FIlter two dimensional OK
  filter = cbind(c(1,2,3),c(4,5,6))
  expect_true({mapperKD(k = 2, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, intervals = c(intervals,intervals), overlap = c(overlap,overlap), clustering_method = cluster_all, local_distance = FALSE, data = NA); TRUE})
  # --- FIlter two dimensional not OK
  expect_error(mapperKD(k = 1, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, intervals = intervals, overlap = overlap, clustering_method = cluster_all, local_distance = FALSE, data = NA))
  expect_error(mapperKD(k = 3, distance = matrix(rep(1,dim(filter)[1]**2), ncol = dim(filter)[1]), filter = filter, intervals = intervals, overlap = overlap, clustering_method = cluster_all, local_distance = FALSE, data = NA))

  # TODO: Finish
})





