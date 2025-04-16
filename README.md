## `surveyR`: an R library package for analyzing Nature survey data

### Installation
```{r}
# install.packages(devtools)
devtools::install_github('jperkel/surveyR')
library(surveyR)
```

### Usage

See `example_survey.qmd` for a fully worked example for using these functions: 

#### `process_radio_button_q`
Process a standard radio button question (ie, tallies results from a single column with multiple possible answers)

```{r}
full_q <- "Q3: What is your overall opinion of the science advice system and practices in your country?"

levels <- c('Very good (consistent practices and a coherent system)',
            'Good (good practices within many areas of government)',
            'Patchy (science advice practiced inconsistently in government)',
            'Poor (practices are inconsistent and/or of low quality)',
            'Very poor (very little science advice practiced or no system exists)',
            'No opinion/Don’t know')

q3 <- surveyR::process_radio_button_q(mydata, 'what_is_your_overall_opinion_of_the_science_advice_system_and_practices_in_your_country', levels)
```

#### `process_radio_button_q_by_grp`
Process a standard radio button question, but break down numbers by group

```{r}
q3_grp <- surveyR::process_radio_button_q_by_grp(mydata, 'analysis_group',  'what_is_your_overall_opinion_of_the_science_advice_system_and_practices_in_your_country', levels)
```

#### `process_multiq_radio_button_q`
Process a multi-question radio button question, ie, tallies multiple cols, each of which is a sub-question of a larger question.

```{r}
levels <- c('Strongly agree', 'Agree', 'Neither agree nor disagree', 'Disagree', 'Strongly disagree')
full_q <- 'Q6: Please indicate how strongly you agree or disagree with the following statements about obstacles to successful science advice to government in your country.'
tmp <- surveyR::process_multiq_radio_button_q(mydata, 'please_indicate_how_strongly_you_agree_or_disagree_with_the_following_statements_about_obstacles_to_successful_science_advice', levels)
tmp2 <- as_tibble(data.table::rbindlist(tmp, idcol = TRUE))
```
  
#### `process_multiq_radio_button_q_by_grp`
Processes a multi-question radio button question, but breaks down numbers by group

```{r}
tmp_grp <- surveyR::process_multiq_radio_button_q_by_grp(mydata, 'analysis_group',  'please_indicate_how_strongly_you_agree_or_disagree_with_the_following_statements_about_obstacles_to_successful_science_advice', levels)
```
  
#### `process_check_all_that_apply_q`
Process a check-all-that-apply question

```{r}
tmp <- surveyR::process_check_all_that_apply_q(mydata, 'which_of_these_best_describes_the_organization_s_you_work_for') |> 
  filter(option != "other write in 78") 
```

#### `process_check_all_that_apply_q_by_grp`
Process a check-all-that-apply question, but break down numbers by group

#### `process_free_text_q`
Count regex terms in free-text answers. Multiple matches in a single answer are counted once.

```{r}
terms <- c(
  '\\bUSA?\\b|United States( of America)?|\\bEEUU\\b|U\\.S\\.(A\\.)?',
  'United Kingdom|\\bUK\\b|Great Britain|England|Scotland|U\\.K\\.',
  # either " EU" or "^EU" but not "EEUU", eg
  '\\bEU\\b|European (Union|Commission)',
  'Afghanistan',
  'Albania',
  'Algeria',
  'Angola',
  'Argentina',
  'Australia',
  'Bangladesh',
  'Belarus',
  'Belgium',
  'Botswana',
  'Bra[zs]il',
  ...
)
surveyR::process_free_text_q(mydata, terms = terms, question = 'looking_internationally_which_country_if_any_do_you_think_is_particularly_successful_at_ensuring_science_is_factored_into_government_policies_and_decisions_i_e_they_have_an_enviable_science_advice_system')
```

#### `graph_responses`
Creates a stacked, single-bar chart showing the fraction of responses for each option (ie, the column totals 100% (% of responses))

```{r}
surveyR::graph_responses(q3, full_q) +
  theme(aspect.ratio = 1/10) +
  paletteer::scale_fill_paletteer_d('colorblindr::OkabeIto',
                                    labels = strsplit(levels, "\\(.*\\)$") |>
                                      stringr::str_trim())
```

#### `graph_responses_by_grp`
Creates a stacked, multicolumn bar chart where each bar represents one group in 'mygroup'; each column totals 100% of responses in that group

```{r}
surveyR::graph_responses_by_grp(q3_grp, 'analysis_group', full_q) +
  theme(aspect.ratio = 3/10) +
  paletteer::scale_fill_paletteer_d('colorblindr::OkabeIto', 
                                    labels = strsplit(levels, "\\(.*\\)$") |>
                                      stringr::str_trim())  
```
