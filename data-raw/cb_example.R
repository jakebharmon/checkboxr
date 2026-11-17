# Toy data from the package spec. Each row exercises one cleaning rule.
cb_example <- tibble::tribble(
  ~id, ~age, ~group, ~q5_car, ~q5_bus, ~q5_bike, ~q5_none, ~q5_dk, ~q5_pna,
    1,   34,    "A",       1,       1,        0,        0,      0,       0, # answered
    2,   51,    "A",       0,       0,        1,        0,      0,       0, # answered
    3,   28,    "B",       0,       0,        0,        1,      0,       0, # none of the above
    4,   45,    "B",       0,       0,        0,        0,      1,       0, # dk
    5,   62,    "A",       0,       0,        0,        0,      0,       1, # pna
    6,   39,    "B",       0,       0,        0,        0,      0,       0, # all unticked
    7,   57,    "A",       1,       0,        0,        0,      1,       0, # conflict: car + dk
    8,   23,    "B",       1,       0,        1,        0,      0,       0  # answered
)

save(cb_example, file = "data/cb_example.rda", compress = "bzip2", version = 2)
