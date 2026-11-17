# checkboxr

Clean select-all-that-apply survey questions and summarize them correctly in
[gtsummary](https://www.danieldsjoberg.com/gtsummary/).

A checkbox question is one question answered once per respondent, but exports
store it as one 0/1 column per option. That causes two problems:

1. **Zeros that are not real zeros.** A respondent who ticks "don't know" or
   "prefer not to answer", or who skips the question, gets 0 in every
   substantive column. Those zeros land in the denominator and pull every
   percentage down.
2. **Long data breaks `tbl_summary()`.** Pivoting to one row per tick
   duplicates respondents, so the table N is the number of ticks and every
   other variable in the table is distorted.

checkboxr keeps the data wide, sets the whole question to `NA` when there is
no usable answer, and shows each option as a row under one question header.

In the toy data shipped with the package (8 respondents):

| Measure | Naive | Correct |
|---|---|---|
| Car selected | 3 of 8 (37.5%) | 3 of 5 (60%) |
| Table N after pivoting long | 10 | 8 |
| Mean age after pivoting long | 41.4 | 42.4 |

## Installation

```r
# install.packages("devtools")
devtools::install_github("jakebharmon/checkboxr")
```

`cb_tbl_summary()` needs gtsummary 2.1.0 or later; `add_p()` on its result
also needs cardx.

## Usage

The pipeline is define, check, clean, summarize.

```r
library(checkboxr)

# 1. Define: which columns form the question, and what each one means
sets <- cb_sets(
  cb_define(
    name = "q5", label = "Transport used",
    cols = starts_with("q5_"),
    option_labels = c(q5_car = "Car", q5_bus = "Bus", q5_bike = "Bike",
                      q5_none = "None of the above"),
    none = q5_none, dk = q5_dk, pna = q5_pna
  )
)

# 2. Check: counts per rule, nothing is changed
cb_check(cb_example, sets)
#>   set   label              n answered    dk   pna all_unticked conflict not_shown has_none
#> 1 q5    Transport used     8        4     1     1            1        1         0 TRUE

# 3. Clean: NA where there is no usable answer, plus a status column
cleaned <- cb_clean(cb_example, sets)
#> i q5: 3 of 8 respondents set to `NA` (1 dk, 1 pna, 1 skipped); 1 conflict kept as answered.

# 4. Summarize
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

## Functions

| Function | Returns | Job |
|---|---|---|
| `cb_define()` | a set | Declare one set by hand |
| `cb_sets()` | a `cb_sets` | Combine sets |
| `cb_detect()` | a draft `cb_sets` | Guess sets from column names |
| `cb_check()` | a tibble | Report problems before cleaning |
| `cb_clean()` | a data frame | Apply the NA rules, add status columns |
| `cb_tbl_summary()` | a gtsummary table | Grouped table of sets and other variables |
| `cb_count()` | integer vector | Number of substantive options ticked |
| `cb_collapse()` | character vector | Ticked labels pasted per respondent |
| `cb_long()` | a long tibble | One row per tick, for plotting only |

## Input formats

Set columns do not have to be 0/1. These codings are recognised without any
extra arguments, ignoring case, and may be mixed within a set:

| Ticked | Unticked | Typical source |
|---|---|---|
| `1` | `0` | REDCap raw, KoboToolbox/ODK, most CSV exports |
| `TRUE` | `FALSE` | R logicals, or the same as text |
| `"Checked"` | `"Unchecked"` | REDCap label export |
| `"Yes"`, `"Y"` | `"No"`, `"N"` | Hand-coded sheets, some SPSS files |
| `"Selected"` | `"Not selected"` | Labelled SPSS/Stata files |

Two other layouts need one argument in `cb_define()`:

* **Another pair of codes**, such as 1 = yes and 2 = no:
  `checked = 1, unchecked = 2`.
* **Ticked boxes filled, unticked boxes blank**, as in Qualtrics (`1` or
  blank) and SurveyMonkey (the option text or blank): `blank = "unticked"`.
  A row that is blank throughout is then treated as all unticked.

Anything else in a set column is an error, so a miscoded column is never read
silently. A question stored as **one delimited column** (`"Car, Bike"`, as in
Google Forms and Microsoft Forms) is not supported; split it into one column
per option first.

## Cleaning rules

Each column of a set has one role: `substantive` (a real option), `none`
("none of the above", a real answer), `dk` (don't know) or `pna` (prefer not
to answer). `cb_clean()` gives each respondent one status per set; the first
matching rule wins.

| Pattern in the raw row | Status | Substantive and none columns |
|---|---|---|
| Any set column is `NA` | `not_shown` | `NA` |
| Substantive option ticked with dk, pna or none | see below | see below |
| dk ticked, no substantive option | `dk` | `NA` |
| pna ticked, no substantive option | `pna` | `NA` |
| none ticked, no substantive option | `answered` | kept |
| At least one substantive option ticked | `answered` | kept |
| Every box unticked | `skipped`, or `answered` with `empty = "zero"` | `NA`, or kept |

Conflicts are handled by the `conflict` argument: `"keep"` (default) trusts
the substantive ticks and sets the none column to 0, `"na"` gives status
`conflict` and sets the columns to `NA`, and `"error"` stops and lists the
rows.

The dk and pna columns are never changed. `cb_clean()` is idempotent.

## Notes on the table

* The table N always equals `nrow(data)`. Option percentages use the
  respondents with status `answered`; that count is in the header.
* With `by`, the count is left out of the header. Use `show_status = TRUE`
  to show response status, including DK and PNA rates, against the full
  sample.
* The result is a gtsummary object: `add_p()`, `add_overall()`, `add_n()`,
  `as_gt()` and `as_flex_table()` work.
* `add_p()` tests each option separately and ignores the dependence between
  options of one question. Use the MRCV package for a joint test.
* `bold_labels()` does not know about the header row.

## How this differs from tntpr and SvenR

Two existing functions solve part of the cleaning problem, and checkboxr
borrows their central idea: **a respondent who did not answer the question
should be `NA` in every column of it, so they drop out of the denominator,
while everyone else gets an explicit 0 or 1.** Both also turn the option text
into a variable label, which is where the default for `option_labels` comes
from.

`tntpr::check_all_recode()` takes columns where a ticked box holds the option
text and an unticked box is `NA`. A respondent is a non-responder when every
column is `NA`; otherwise text becomes 1 and `NA` becomes 0. A free-text
"Other" column counts as a response. Its partner `check_all_count()`
tabulates the result with janitor.

`SvenR::format_checkbox()` takes columns coded 1/`NA` with haven value
labels. Rows that are `NA` everywhere stay `NA`, other `NA`s become 0, and
the value label is moved to the variable label.

checkboxr differs in these ways:

* **Non-response is more than "all blank".** Both functions recognise a
  non-responder only by an all-`NA` row. checkboxr gives columns roles, so
  "don't know" and "prefer not to answer" ticks also remove the respondent
  from the denominator, and "none of the above" keeps them in it.
* **It works on 0/1 exports.** REDCap, Qualtrics and similar tools write 0
  for every box of a skipped question, so there is no all-`NA` row to find.
  `empty = "na"` treats an all-unticked row as a skip.
* **The reason is recorded.** A status column says why each respondent was
  kept or set to `NA`, so DK and PNA rates can still be reported. The other
  two functions leave no trace.
* **Conflicts are a decision, not an accident.** A real option ticked next
  to "don't know" is handled by an explicit `conflict` policy.
* **The definition is reusable.** The sets are declared once and passed to the
  check, clean, summary and helper functions, instead of re-selecting the
  columns in each call. It is not stored in data frame attributes, which
  dplyr verbs do not reliably keep.
* **Cleaning is idempotent** and the dk/pna columns are left untouched.
* **Tabulation goes through gtsummary**, next to the other variables of the
  table, instead of a separate janitor tabyl.
* **Free text is out of scope.** An "Other (specify)" text column is not
  part of a set; the 0/1 "Other" flag is a substantive option.

## Out of scope

Weighting, inference on multiple-response data (MRCV), reading raw exports
(REDCapR, qualtRics) and a new table engine (gtsummary).
