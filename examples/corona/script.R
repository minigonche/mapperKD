library('mapperKD')

# Loads the data
meta_df = read.csv('data/meta_data.csv', stringsAsFactors = FALSE)
distance_matrix = read.csv('data/distance_matrix.csv', header = FALSE)

# Applies the Algorithm
one_skeleton_result = mapperKD(k = 1,
               distance = distance_matrix,
               filter = meta_df$day_occurence,
               interval_scheme = "GMM", # Calculates the intervals using Gaussian Mixed Models
               clustering_method = hierarchical_clustering)


# Plots the 1 Skeleton (graph)
plot_1_skeleton(one_skeleton_result, layout = 'auto')

# Plots the point intersection network
groups = sapply(meta_df$country, toString)
plot_intersection_network(one_skeleton_result, groups = groups, max_node_size = 6)


# Plot intersection network -  world
plot_intersection_network_over_map(one_skeleton_result,
                                   lon  = meta_df$lon,
                                   lat = meta_df$lat,
                                   arrow_color = 'DESTINATION',
                                   groups = groups)




# Plot intersection network -  Asia
only_asia =  (1:dim(meta_df)[1])[meta_df$lon > 30]
plot_intersection_network_over_map(one_skeleton_result,
                                   lon  = meta_df$lon,
                                   lat = meta_df$lat,
                                   groups = groups,
                                   arrow_color = 'DESTINATION',
                                   focus_on = only_asia)
