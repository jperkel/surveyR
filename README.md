## `surveyR`: an R library package for analyzing Nature survey data

### Installation
```{r}
# install.packages(devtools)
devtools::install_github('jperkel/surveyR')
library(surveyR)
```

### Usage

See `example_survey.qmd` for a fully worked example for using these functions: 

- `process_radio_button_q`: Process a standard radio button question (ie, tallies results from a single column with multiple possible answers)
- `process_radio_button_q_by_grp`: Process a standard radio button question, but break down numbers by group
- `process_multiq_radio_button_q`: Process a multi-question radio button question, ie, tallies multiple cols, each of which is a sub-question of a larger question.
- `process_multiq_radio_button_q_by_grp`: Processes a multi-question radio button question, but breaks down numbers by group
- `process_check_all_that_apply_q`: process a check-all-that-apply question
- `process_check_all_that_apply_q_by_grp`: Process a check-all-that-apply question, but break down numbers by group
- `process_free_text_q`: Count regex terms in free-text answers. Multiple matches in a single answer are counted once.
- `graph_responses`: creates a stacked, single-bar chart showing the fraction of responses for each option (ie, the column totals 100% (% of responses))
- `graph_responses_by_grp`: creates a stacked, multicolumn bar chart where each bar represents one group in 'mygroup'; each column totals 100% of responses in that group

