#' get_edge_list
#'
#' @description Creates an edge list when the second column lists any number of nodes/entities for one observation.
#'
#' @details \code{get_edge_list()} takes an edge list with an id column where the second column has multiple values. It then separates the second column and creates a full edge list. As opposed to \code{separate_rows()}, this function only keeps those pairwise combinations that exist in the original data.
#'
#' @param df A dataframe
#' @param actor Actor or node
#' @param assoc_actors Second actor or node with multiple values
#' @param idx Index, row number or grouping variable
#' @param sep Separator delimiting collapsed values - default sep = ';'
#'
#' @return returns a dataframe or tibble
#'
#' @examples
#' df <- tibble::tibble(
#'  actor_var = c("A", "A", "B"),
#'  assoc_actors_var = c("B", "B; C", "C; D; E")) |>
#'    dplyr::mutate(id_val = dplyr::row_number()) |>
#'    dplyr::select(id_val, dplyr::everything())
#'
#' print(get_edge_list(df,
#'                     actor = 'actor_var',
#'                     assoc_actors = 'assoc_actors_var',
#'                     idx = 'id_val'))
#'
#' @importFrom magrittr %>%
#' @import rlang
#'
#' @export
get_edge_list <- function(df,
                          actor,
                          assoc_actors,
                          idx,
                          sep = ';'
){
  df_temp <- df %>%
    dplyr::select( {{actor}},
                   {{assoc_actors}},
                   {{idx}} ) %>%
    dplyr::group_by(.data[[idx]]) %>%
    dplyr::filter(stringr::str_detect(eval(as.symbol(assoc_actors)), sep)) %>%
    tidyr::separate_rows(dplyr::all_of(assoc_actors)) %>%
    # dplyr::mutate(!!assoc_actors := stringr::str_squish(!!rlang::enquo(assoc_actors))) %>%
    tidyr::pivot_longer({{actor}}:{{assoc_actors}}) %>%
    dplyr::select(value) %>%
    dplyr::distinct(value) %>%
    dplyr::mutate(value2 = value) %>%
    tidyr::expand(value, value2) %>%
    dplyr::filter(value != value2) %>%
    dplyr::mutate(helper = stringr::str_c(value, value2)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(helper = stringr::str_c(
      stringr::str_sort(unlist(stringr::str_split(helper, ""))), collapse = "")) %>%
    dplyr::distinct(helper,.keep_all = T) %>%
    dplyr::select(-helper) %>%
    dplyr::rename({{actor}} := value, {{assoc_actors}} := value2) %>%
    dplyr::ungroup()

  #return(df_temp)

  return <- df %>%
    dplyr::filter(!stringr::str_detect(eval(as.symbol(assoc_actors)), sep)) %>%
    dplyr::bind_rows(df_temp)
}



