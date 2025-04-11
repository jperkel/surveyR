library(tidyverse)

### ADAPTED FROM 4_3_WOS_ANALYSIS.RMD
# returns a list of columns that match a text string
# inputs: dataset and text string
# outputs: a list of column indices, and a printout of column names
column_chunk <- function(mytable, text_string, print_colnames = FALSE) {
  questionIDs <- grep(text_string, names(mytable))
  # print relevant column names to check whether last item is a free text field
  if (print_colnames) print(names(mytable[questionIDs]))

  questionIDs
}

#' process_radio_button_q: Process a standard radio button question
#' @param mytable the dataset
#' @param question question under analysis (use the janitor::clean_names() text)
#' @param mylevels vector of possible answers to the question
#'
#' @returns tibble with cols 'option', 'count', 'pct', where count = count of responses, pct = count as a fraction of all responses (count / sum(count))
#' @export
process_radio_button_q <- function(mytable, question, mylevels) {
  # Ensure the question is treated as a column name
  question <- sym(question)

  mytable %>%
    group_by(!!question) %>%
    summarize(count = n(), .groups = 'drop') %>%
    rename(option = !!quo_name(question)) %>%
    mutate(option = factor(option, levels = mylevels, ordered = TRUE)) %>%
    filter(!is.na(option)) %>%
    mutate(pct = count / sum(count)) %>%
    arrange(option)
}


#' process_radio_button_q_by_grp: Process a standard radio button, but break down numbers by group
#' @param mytable the dataset
#' @param mygroup the column whose values define a group
#' @param question question under analysis (use the janitor::clean_names() text)
#' @param mylevels vector of possible answers to the question
#'
#' @returns a tibble with cols 'option', <mygroup>, count, pct, where count = count of responses and pct = count as a percentage of all responses PER GROUP (ie, each group will sum to 100%)
#' @export
process_radio_button_q_by_grp <- function(mytable, mygroup, question, mylevels) {

  # Ensure mygroup is treated as a column name
  mygroup <- sym(mygroup)

  index <- column_chunk(mytable, question)
  fullname <- colnames(mytable)[index]

  mytable |>
    rename(option = !!(fullname)) |>
    mutate(option = factor(option, mylevels, ordered = TRUE)) |>
    filter(!is.na(option),
           !is.na({{mygroup}})) |>
    group_by({{mygroup}}, option) |>
    summarize(count = n()) |>
    group_by({{mygroup}}) |>
    mutate(pct = count / sum(count)) |>
    ungroup() |>
    complete(option, {{mygroup}})

}

#' process_multiq_radio_button_q: Processes a multi-question radio button
#' @param mytable the dataset
#' @param question question under analysis (use the janitor::clean_names() text)
#' @param mylevels vector of possible answers to the question
#'
#' @returns a list of tibbles, each named according to the question, each with cols 'option', 'count', 'pct'
#' @export
process_multiq_radio_button_q <- function(mytable, question, mylevels) {
  pattern <- str_c(question, '[a-z_]', '*', sep = '')
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


#' process_multiq_radio_button_q_by_grp: Processes a multi-question radio button, but break down numbers by group
#'
#' @param mytable the dataset
#' @param mygroup the column whose values define a group
#' @param question question under analysis (use the janitor::clean_names() text)
#' @param mylevels vector of possible answers to the question
#'
#' @returns a list of tibbles, each named for one question in the overall question and containing cols 'option', <mygroup>, 'count', 'pct'
#' @export
process_multiq_radio_button_q_by_grp <- function(mytable, mygroup,
                                                    question, mylevels) {
  # Ensure mygroup is treated as a column name
  mygroup <- sym(mygroup)

  pattern <- str_c(question, '[a-z_]', '*', sep = '')
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

#' graph_responses: creates a stacked, single-bar chart showing the fraction of responses for each option (ie, the column totals 100% (% of responses))
#'
#' @param mytable the output of process_radio_button_q()
#' @param mytitle Title text for the graph
#'
#' @returns a ggplot object
#' @export
graph_responses <- function(mytable, mytitle) {
  denominator <- mytable |> summarize(count = sum(count))

  mycaption <- md(glue::glue("_n = {denominator$count} responses_"))

  p <- mytable |>
    mutate(group = 'group') |>
    ggplot(aes(x = group, y = pct, fill = option)) +
    geom_bar(position = 'fill', color = 'black', stat = 'identity') +
    geom_text(aes(label = glue::glue("{count} ({round(pct * 100, 2)}%)")),
              position = position_fill(vjust = 0.5),
              size = 3) +
    coord_flip() +
    theme_minimal() +
    labs(title = mytitle, x = NULL, y = '% responses', fill = NULL, caption = mycaption) +
    theme(legend.position = 'bottom',
          axis.text.y = element_blank(),
          plot.caption = element_markdown()) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse=TRUE)) # +
    # paletteer::scale_fill_paletteer_d('colorblindr::OkabeIto')

  p
}

#' graph_responses_by_grp: creates a stacked, multicolumn bar chart where each bar represents one group in 'mygroup'; each column totals 100% of responses in that group
#'
#' @param mytable the output of process_radio_button_q_by_grp()
#' @param mygroup the column whose values define a group
#' @param mytitle graph title
#'
#' @returns a ggplot object.
#' @export
graph_responses_by_grp <- function(mytable, mygroup, mytitle) {
  mygroup <- sym(mygroup)

  denominator <- mytable |> summarize(count = sum(count, na.rm = T))

  mycaption <- md(glue::glue("_n = {denominator$count} responses_", sum = sum(denominator$count)))

  p <- mytable |>
    ggplot(aes(x = {{mygroup}}, y = pct, fill = option)) +
    geom_bar(position = 'fill', color = 'black', stat = 'identity') +
    geom_text(aes(label = glue::glue("{count} ({round(pct * 100, 2)}%)")),
              position = position_fill(vjust = 0.5),
              size = 3) +
    coord_flip() +
    theme_minimal() +
    labs(title = mytitle, x = NULL, y = '% responses', fill = NULL, caption = mycaption) +
    theme(legend.position = 'bottom',
          # axis.text.y = element_blank(),
          plot.caption = element_markdown()) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse=TRUE)) # +
    # paletteer::scale_fill_paletteer_d('colorblindr::OkabeIto')

  p
}

#' process_check_all_that_apply_q: Process a 'check all answers that apply' question
#'
#' @param mytable the dataset
#' @param question question under analysis (use the janitor::clean_names() text)
#'
#' @returns a tibble: options (rows) x count
#' @export
process_check_all_that_apply_q <- function(mytable, question) {
  index <- column_chunk(mytable, question)
  pattern <- str_c(question, '[a-z_]', '*', sep = '')

  tmp <- mytable |>
    # convert answers to T/F be/c each column just contains the same text as the question
    mutate(across(index, ~ !is.na(.))) |>
    select(index) |>
    rename_with(~ sub(pattern = pattern, replacement = '', x = colnames(mytable)[index])) |>
    # count answers
    colSums() |>
    data.frame()

  names(tmp) <- 'count'
  tmp <- tmp |>
    rownames_to_column(var = 'option') |>
    mutate(option = str_replace_all(option, '_', ' '),
           option = str_replace(option, ' $', ''))

  tmp
}

#' process_check_all_that_apply_q_by_grp
#'
#' @param mytable the dataset
#' @param mygroup column whose values defines a group
#' @param question question under analysis (use the janitor::clean_names() text)
#'
#' @returns a tibble of questions (cols) x grps (rows)
process_check_all_that_apply_q_by_grp <- function(mytable, mygroup, question) {
  mygroup <- sym(mygroup)

  index <- column_chunk(mytable, question)
  pattern <- str_c(question, '[a-z_]', '*', sep = '')

  tmp <- mytable |>
    # convert answers to T/F be/c each column just contains the same text as the question
    mutate(across(all_of(index), ~!is.na(.))) |>
    rename_with(~sub(pattern = pattern, replacement = '', x = colnames(mytable)[index]),
                .cols = colnames(mytable)[index]) |>
    select(c({{mygroup}}, index)) |>
    group_by({{mygroup}}) |>
    summarize(across(is.logical, sum)) |>
    rename_with(~ str_replace_all(.x, '_', ' ')) |>
    rename_with(~str_replace(.x,' $', '')) |>
    t() |>
    data.frame()

  names(tmp) <- tmp[1,]
  tmp <- tmp[-1,] |>
    rownames_to_column()

  tmp
}


# get_write_in_colno <- function(mytable, question) {
#   indices <- column_chunk(mytable, question)
#
#   return(colnames(mytable)[indices] |>
#            stringr::str_subset('other_write_in'))
# }

#' process_free_text_q: Count regex terms in free-text answers
#'
#' @param mytable the dataset
#' @param question question under analysis (use the janitor::clean_names() text)
#' @param terms a vector of regexes to search for
#'
#' @returns a tibble tallying the occurrence of each term in 'terms' in the question
#' @export
process_free_text_q <- function(mytable, question, terms) {
  index <- column_chunk(mytable, question)
  tmp <- mytable |> pull(index)

  v <- rep(NA, length(terms))

  for (i in seq_along(terms)) {
    term <- terms[i]
    v[i] <- stringr::str_detect(tmp, regex(term, ignore_case = TRUE)) |>
      sum(na.rm = T)
  }
  names(v) <- terms

  data.frame(v) |>
    rownames_to_column() |>
    as_tibble() |>
    rename(count = v) |>
    arrange(desc(count)) |>
    filter(count > 0) # |>
    # gt::gt(rowname_col = "rowname") |>
    # gt::tab_header(question) |>
    # gt::tab_footnote(footnote)
}
