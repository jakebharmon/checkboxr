# checkboxr

Clean select-all-that-apply survey questions in R and summarize them in
[gtsummary](https://www.danieldsjoberg.com/gtsummary/) with the right
denominator.

Survey exports store a checkbox question as one 0/1 column per option.
Respondents who ticked "don't know" or "prefer not to answer", or skipped the
question, get 0 everywhere and drag every percentage down. checkboxr sets the
question to `NA` for them, and shows each option as a row under one question
header, next to your other variables. The data stays one row per respondent.

## Why not pivot longer?

The usual workaround is to pivot the checkbox columns to one row per tick.
That repeats respondents, which distorts every other variable in the table:

```r
library(dplyr)
library(tidyr)

long <- cb_example |>
  pivot_longer(starts_with("q5_"), names_to = "option", values_to = "ticked") |>
  filter(ticked == 1) |>
  select(id, age, option)
long
#>       id   age option
#>  1     1    34 q5_car
#>  2     1    34 q5_bus     <- respondent 1 counted twice
#>  3     2    51 q5_bike
#>  4     3    28 q5_none
#>  5     4    45 q5_dk
#>  6     5    62 q5_pna
#>  7     7    57 q5_car
#>  8     7    57 q5_dk      <- respondent 7 counted twice
#>  9     8    23 q5_car
#> 10     8    23 q5_bike    <- respondent 8 counted twice
#>                           (respondent 6 ticked nothing and is gone)

nrow(long)             # 10 rows for 8 respondents
mean(long$age)         # 41.4
mean(cb_example$age)   # 42.4, the true mean
```

A table built from `long` reports N = 10 and the wrong mean age. checkboxr
never reshapes the data, so age and every other variable are summarized once
per respondent.

## Installation

```r
devtools::install_github("jakebharmon/checkboxr")
```

## Usage

```r
library(checkboxr)

# Define the question
sets <- cb_sets(
  cb_define(
    name = "q5", label = "Transport used",
    cols = starts_with("q5_"),
    option_labels = c(q5_car = "Car", q5_bus = "Bus", q5_bike = "Bike",
                      q5_none = "None of the above"),
    none = q5_none, dk = q5_dk, pna = q5_pna
  )
)

cb_check(cb_example, sets)               # counts per rule, changes nothing
cleaned <- cb_clean(cb_example, sets)    # NA where there is no usable answer

cb_tbl_summary(
  cleaned, sets,
  include = c(age, q5),
  type = age ~ "continuous",
  statistic = age ~ "{mean} ({sd})"
)
#> Characteristic              N = 8
#> age                         42 (14)
#> Transport used (n = 5)
#>     Car                     3 (60%)
#>     Bus                     1 (20%)
#>     Bike                    2 (40%)
#>     None of the above       1 (20%)
```

### Several questions

`cb_sets()` takes one `cb_define()` per checkbox question. Each question is
cleaned on its own and gets its own header in the table.

```r
library(gtsummary)

# Add a second question to the toy data
survey <- cb_example
survey$q6_work   <- c(1, 0, 0, 0, 1, 0, 0, 1)
survey$q6_school <- c(0, 0, 1, 0, 1, 0, 0, 0)
survey$q6_other  <- c(0, 1, 0, 0, 0, 0, 0, 0)

sets <- cb_sets(
  cb_define(
    "q5", "Transport used", starts_with("q5_"),
    option_labels = c(q5_car = "Car", q5_bus = "Bus", q5_bike = "Bike",
                      q5_none = "None of the above"),
    none = q5_none, dk = q5_dk, pna = q5_pna
  ),
  cb_define(
    "q6", "Trip purpose", starts_with("q6_"),
    option_labels = c(q6_work = "Work", q6_school = "School", q6_other = "Other")
  )
)

cleaned <- cb_clean(survey, sets)
#> i q5: 3 of 8 respondents set to `NA` (1 dk, 1 pna, 1 skipped); 1 conflict kept as answered.
#> i q6: 3 of 8 respondents set to `NA` (3 skipped).

cb_tbl_summary(
  cleaned, sets,
  include = c(age, q5, q6),
  by = group,
  type = age ~ "continuous"
) |>
  add_overall()
#> Characteristic          Overall, N = 8   A, N = 4      B, N = 4
#> age                     42 (31, 54)      54 (43, 60)   34 (26, 42)
#> Transport used
#>     Car                 3 (60%)          2 (67%)       1 (50%)
#>     Bus                 1 (20%)          1 (33%)       0 (0%)
#>     Bike                2 (40%)          1 (33%)       1 (50%)
#>     None of the above   1 (20%)          0 (0%)        1 (50%)
#> Trip purpose
#>     Work                3 (60%)          2 (67%)       1 (50%)
#>     School              2 (40%)          1 (33%)       1 (50%)
#>     Other               1 (20%)          1 (33%)       0 (0%)
```

Age is summarized over all eight respondents, while each question uses only
the respondents who answered it. The result is an ordinary gtsummary table,
so `add_p()` and `as_gt()` work as usual. See `vignette("checkboxr")` for the
full walkthrough.

## Functions

| Function | Job |
|---|---|
| `cb_define()`, `cb_sets()` | Describe which columns form a question |
| `cb_detect()` | Draft that description from column names |
| `cb_check()` | Report problems before cleaning |
| `cb_clean()` | Apply the `NA` rules and add a status column |
| `cb_tbl_summary()` | Table of questions and other variables |
| `cb_count()`, `cb_collapse()`, `cb_long()` | Helpers |

## Input formats

0/1, `TRUE`/`FALSE`, `"Yes"`/`"No"` and `"Checked"`/`"Unchecked"` are
recognised automatically. Use `checked` and `unchecked` for other codes, and
`blank = "unticked"` when unticked boxes are left empty (Qualtrics,
SurveyMonkey). A question stored as one delimited column must be split first.

## Related work

`tntpr::check_all_recode()` and `SvenR::format_checkbox()` share the core
idea: non-responders become `NA` in every column of the question. They detect
non-response only as an all-blank row. checkboxr adds column roles for "don't
know", "prefer not to answer" and "none of the above", works on 0/1 exports
where a skipped question is all zeros, records the reason in a status column,
and builds the table through gtsummary.
