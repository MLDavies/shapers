#' get_edge_list
#'
#' @description Creates an edge list when the second column lists any number of nodes/entities for one observation.
#'
#' @details \code{get_edge_list()} takes an edge list with an id column where the second column has multiple values. It then separates the second column and creates a full edge list. As opposed to \code{separate_rows()}, this function only keeps those pairwise combinations that exist in the original data.
#'
#' @param df dataframe
#' @param actor actor or node
#' @param assoc_actor_1 second actor or node with multiple values sep = ';'
#' @param idx index, row number or grouping variable
#' @param sep separator - default sep = ';'
#'
#' @return returns a dataframe or tibble
#'
#' @examples
#' df <- tibble::tibble(
#'  actor_1 = c("A", "A", "B"),
#'  assoc_actor_1 = c("B", "B; C", "C; D; E")) |>
#'    dplyr::mutate(idx = dplyr::row_number()) |>
#'    dplyr::select(idx, dplyr::everything())
#'
# print(get_edge_list(df,
#                     actor = actor_1,
#                     assoc_actors = assoc_actor_1,
#                     idx = idx))
#'
#'
#' @export
get_edge_list <- function(df,
                          actor = actor,
                          assoc_actors = assoc_actors,
                          idx = idx,
                          sep = ';'
                          ){
  df_temp <- df  |>
    dplyr::select(actor_1, assoc_actor_1, idx) |>
    dplyr::group_by(idx) |>
    dplyr::filter(stringr::str_detect(assoc_actor_1, sep)) |>
    tidyr::separate_rows(assoc_actor_1) |>
    dplyr::mutate(assoc_actor_1 = stringr::str_squish(assoc_actor_1)) |>
    tidyr::pivot_longer(actor_1:assoc_actor_1) |>
    dplyr::select(value) |>
    dplyr::distinct(value) |>
    dplyr::mutate(value2 = value) |>
    tidyr::expand(value, value2) |>
    dplyr::filter(value !=value2) |>
    dplyr::mutate(helper = stringr::str_c(value,value2)) |>
    dplyr::rowwise() |>
    dplyr::mutate(helper = stringr::str_c(
      stringr::str_sort(unlist(stringr::str_split(helper, ""))),collapse = "")) |>
    dplyr::distinct(helper,.keep_all = T) |>
    dplyr::select(-helper) |>
    dplyr::rename(actor_1 = value, assoc_actor_1 = value2) |>
    dplyr::ungroup()

  #return(df_temp)

  return <- df |>
    dplyr::filter(!stringr::str_detect(assoc_actor_1, sep)) |>
    dplyr::bind_rows(df_temp)
}
