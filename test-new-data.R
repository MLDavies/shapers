# A test using an input data frame in which no column names duplicate arguments
# found in the function (e.g., idx)

df <- tibble::tibble(
 actor_var = c("A", "A", "B"),
 assoc_actors_var = c("B", "B; C", "C; D; E")) |>
   dplyr::mutate(id_val = dplyr::row_number()) |>
   dplyr::select(id_val, dplyr::everything())

print(get_edge_list(df,
                    actor = 'actor_var',
                    assoc_actors = 'assoc_actors_var',
                    idx = 'id_val'))
