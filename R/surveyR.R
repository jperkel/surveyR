
# returns a list of columns that match a text string
# inputs: dataset and text string
# outputs: a list of column indices, and a printout of column names
### ADAPTED FROM 4_3_WOS_ANALYSIS.RMD
column_chunk <- function(mytable, text_string, print_colnames = FALSE) {
  questionIDs <- grep(text_string, names(mytable))
  # print relevant column names to check whether last item is a free text field
  if (print_colnames) print(names(mytable[questionIDs]))

  questionIDs
}

#' Process a standard radio button question (ie, tallies results from a
#' single column with multiple possible answers)
#'
#' @param mytable the dataset
#' @param question question under analysis (use the janitor::clean_names() text)
#' @param mylevels vector of possible answers to the question
#'
#' @returns tibble with cols 'option', 'count', 'pct', where
#' count = count of responses, pct = count as a fraction of all responses (count / sum(count))
#' @export
process_radio_button_q <- function(mytable, question, mylevels) {
  # Ensure the question is treated as a column name
  index <- column_chunk(mytable, question)
  if (length(index) != 1) stop("ERR: 'question' must uniquely identify one column.", call. = FALSE)

  question <- colnames(mytable)[index]
  question <- rlang::sym(question)

  mytable %>%
    dplyr::group_by(!!question) %>%
    dplyr::summarize(count = n(), .groups = 'drop') %>%
    # dplyr::rename(option = !!quo_name(question)) %>%
    # dplyr::rename(option = !!rlang::as_name(question)) %>%
    dplyr::rename(option = question) %>%
    dplyr::mutate(option = factor(option, levels = mylevels, ordered = TRUE)) %>%
    dplyr::filter(!is.na(option)) %>%
    dplyr::mutate(pct = count / sum(count)) %>%
    dplyr::arrange(option)
}


#' Process a standard radio button question, but break down numbers by group
#'
#' @param mytable the dataset
#' @param mygroup the column whose values define a group
#' @param question question under analysis (use the janitor::clean_names() text)
#' @param mylevels vector of possible answers to the question
#'
#' @returns a tibble with cols 'option', <mygroup>, count, pct,
#' where count = count of responses and pct = count as a percentage of all responses PER GROUP (ie, each group will sum to 100%)
#' @export
process_radio_button_q_by_grp <- function(mytable, mygroup, question, mylevels) {

  # Ensure mygroup is treated as a column name
  index <- column_chunk(mytable, question)
  if (length(index) != 1) stop("ERR: 'question' must uniquely identify one column.", call. = FALSE)

  question <- colnames(mytable)[index]
  question <- rlang::sym(question)
  mygroup <- rlang::sym(mygroup)

  # index <- column_chunk(mytable, question)
  # fullname <- colnames(mytable)[index]

  mytable |>
    # dplyr::rename(option = !!fullname) |>
    dplyr::rename(option = !!question) |>
    dplyr::mutate(option = factor(option, mylevels, ordered = TRUE)) |>
    dplyr::filter(!is.na(option),
           !is.na(!!mygroup)) |>
    dplyr::group_by(!!mygroup, option) |>
    dplyr::summarize(count = n()) |>
    dplyr::group_by(!!mygroup) |>
    dplyr::mutate(pct = count / sum(count)) |>
    dplyr::ungroup() |>
    tidyr::complete(option, !!mygroup)

}

#' Process a multi-question radio button question,
#' ie, tallies multiple cols, each of which is a sub-question of a larger
#' question.
#'
#' @param mytable the dataset
#' @param question question under analysis (use the janitor::clean_names() text)
#' @param mylevels vector of possible answers to the question
#'
#' @returns a list of tibbles, each named according to the question,
#' each with cols 'option', 'count', 'pct'
#' @export
process_multiq_radio_button_q <- function(mytable, question, mylevels) {
  pattern <- stringr::str_c(question, '[a-z_]', '*', sep = '')
  indices <- column_chunk(mytable, question)

  ans <- list()
  for (i in seq_along(indices)) {
    cn <- colnames(mytable)[indices[i]]
    if (!is.null(cn)) {
      ans[[cn]] <- process_radio_button_q(mytable, cn, mylevels)
    } else {
      warning("Column name not found for index: ", indices[i])
    }
  }

  names(ans) <- stringr::str_replace(string = names(ans), pattern = pattern, replacement = '') |>
    stringr::str_replace_all('_', ' ') |>
    stringr::str_replace(' $', '') |>
    stringr::str_to_sentence()

  ans
}


#' Processes a multi-question radio button question, but breaks down numbers by group
#'
#' @param mytable the dataset
#' @param mygroup the column whose values define a group
#' @param question question under analysis (use the janitor::clean_names() text)
#' @param mylevels vector of possible answers to the question
#'
#' @returns a list of tibbles, each named for one question in the overall question
#' and containing cols 'option', <mygroup>, 'count', 'pct'
#' @export
process_multiq_radio_button_q_by_grp <- function(mytable, mygroup,
                                                    question, mylevels) {
  # Ensure mygroup is treated as a column name
  # mygroup <- rlang::sym(mygroup)

  pattern <- stringr::str_c(question, '[a-z_]', '*', sep = '')
  indices <- column_chunk(mytable, question)

  ans <- list()
  for (i in seq_along(indices)) {
    cn <- colnames(mytable)[indices[i]]
    ans[[cn]] <- process_radio_button_q_by_grp(mytable, mygroup, cn, mylevels)
  }

  # fix table names. remove everything after question...
  names(ans) <- stringr::str_replace(string = names(ans), pattern = pattern, replacement = '') |>
    # remove underscores...
    stringr::str_replace_all('_', ' ') |>
    # and trailing spaces
    stringr::str_replace(' $', '') |>
    stringr::str_to_sentence()

  ans
}

#' creates a stacked, single-bar chart showing the fraction of
#' responses for each option (ie, the column totals 100% (% of responses))
#'
#' @param mytable the output of process_radio_button_q()
#' @param mytitle Title text for the graph
#'
#' @returns a ggplot object
#' @export
graph_responses <- function(mytable, mytitle) {
  denominator <- mytable |> dplyr::summarize(count = sum(count))

  mycaption <- md(glue::glue("_n = {denominator$count} responses_"))

  p <- mytable |>
    dplyr::mutate(group = 'group') |>
    ggplot2::ggplot(aes(x = group, y = pct, fill = option)) +
    ggplot2::geom_bar(position = 'fill', color = 'black', stat = 'identity') +
    ggrepel::geom_text_repel(aes(label = glue::glue("{count} ({round(pct * 100, 2)}%)")),
              position = position_fill(vjust = 0.5),
              size = 3,
              angle = 45) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = mytitle, x = NULL, y = '% responses', fill = NULL, caption = mycaption) +
    ggplot2::theme(legend.position = 'bottom',
          axis.text.y = element_blank(),
          plot.caption = element_markdown()) +
    ggplot2::guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse=TRUE)) # +
    # paletteer::scale_fill_paletteer_d('colorblindr::OkabeIto')

  p
}

#' creates a stacked, multicolumn bar chart where each bar represents one group
#' in 'mygroup'; each column totals 100% of responses in that group
#'
#' @param mytable the output of process_radio_button_q_by_grp()
#' @param mygroup the column whose values define a group
#' @param mytitle graph title
#'
#' @returns a ggplot object.
#' @export
graph_responses_by_grp <- function(mytable, mygroup, mytitle) {
  mygroup <- rlang::sym(mygroup)

  denominator <- mytable |> dplyr::summarize(count = sum(count, na.rm = T))

  mycaption <- md(glue::glue("_n = {denominator$count} responses_", sum = sum(denominator$count)))

  p <- mytable |>
    ggplot2::ggplot(aes(x = !!mygroup, y = pct, fill = option)) +
    ggplot2::geom_bar(position = 'fill', color = 'black', stat = 'identity') +
    ggrepel::geom_text_repel(aes(label = glue::glue("{count} ({round(pct * 100, 2)}%)")),
              position = position_fill(vjust = 0.5),
              size = 3,
              angle = 45) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = mytitle, x = NULL, y = '% responses', fill = NULL, caption = mycaption) +
    ggplot2::theme(legend.position = 'bottom',
          # axis.text.y = element_blank(),
          plot.caption = element_markdown()) +
    ggplot2::guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse=TRUE)) # +
    # paletteer::scale_fill_paletteer_d('colorblindr::OkabeIto')

  p
}

#' Process a check-all-that-apply question
#'
#' @param mytable the dataset
#' @param question question under analysis (use the janitor::clean_names() text)
#'
#' @returns a tibble: options (rows) x count
#' @export
process_check_all_that_apply_q <- function(mytable, question) {
  index <- column_chunk(mytable, question)
  pattern <- stringr::str_c(question, '[a-z_]', '*', sep = '')

  tmp <- mytable |>
    # convert answers to T/F be/c each column just contains the same text as the question
    dplyr::mutate(across(index, ~ !is.na(.))) |>
    dplyr::select(index) |>
    dplyr::rename_with(~ sub(pattern = pattern, replacement = '', x = colnames(mytable)[index])) |>
    # count answers
    colSums() |>
    data.frame()

  names(tmp) <- 'count'
  tmp <- tmp |>
    tibble::rownames_to_column(var = 'option') |>
    dplyr::mutate(option = str_replace_all(option, '_', ' '),
           option = str_replace(option, ' $', ''))

  tmp
}

#' Process a check-all-that-apply question, but break down numbers by group
#'
#' @param mytable the dataset
#' @param mygroup column whose values defines a group
#' @param question question under analysis (use the janitor::clean_names() text)
#'
#' @returns a tibble of questions (cols) x grps (rows)
process_check_all_that_apply_q_by_grp <- function(mytable, mygroup, question) {
  mygroup <- rlang::sym(mygroup)

  index <- column_chunk(mytable, question)
  pattern <- stringr::str_c(question, '[a-z_]', '*', sep = '')

  tmp <- mytable |>
    # convert answers to T/F be/c each column just contains the same text as the question
    dplyr::mutate(across(all_of(index), ~!is.na(.))) |>
    dplyr::rename_with(~sub(pattern = pattern, replacement = '', x = colnames(mytable)[index]),
                .cols = colnames(mytable)[index]) |>
    dplyr::select(c(!!mygroup, index)) |>
    dplyr::group_by(!!mygroup) |>
    dplyr::summarize(across(is.logical, sum)) |>
    dplyr::rename_with(~ str_replace_all(.x, '_', ' ')) |>
    dplyr::rename_with(~str_replace(.x,' $', '')) |>
    t() |>
    data.frame()

  names(tmp) <- tmp[1,]
  tmp <- tmp[-1,] |>
    tibble::rownames_to_column()

  tmp
}


# get_write_in_colno <- function(mytable, question) {
#   indices <- column_chunk(mytable, question)
#
#   return(colnames(mytable)[indices] |>
#            stringr::str_subset('other_write_in'))
# }

#' Count regex terms in free-text answers. Multiple matches in
#' a single answer are counted once.
#'
#' @param mytable the dataset
#' @param question question under analysis (use the janitor::clean_names() text)
#' @param terms a vector of regexes to search for
#'
#' @returns a tibble tallying the occurrence of each term in 'terms' in the question
#' @export
process_free_text_q <- function(mytable, question, terms) {
  index <- column_chunk(mytable, question)
  if (length(index) != 1) stop("ERR: `question` must uniquely identify one column", call. = F)

  tmp <- mytable |> dplyr::pull(index)

  v <- rep(NA, length(terms))

  for (i in seq_along(terms)) {
    term <- terms[i]
    v[i] <- stringr::str_detect(tmp, regex(term, ignore_case = TRUE)) |>
      sum(na.rm = T)
  }
  names(v) <- terms

  data.frame(v) |>
    tibble::rownames_to_column() |>
    tibble::as_tibble() |>
    dplyr::rename(count = v) |>
    dplyr::arrange(desc(count)) |>
    dplyr::filter(count > 0) # |>
    # gt::gt(rowname_col = "rowname") |>
    # gt::tab_header(question) |>
    # gt::tab_footnote(footnote)
}
