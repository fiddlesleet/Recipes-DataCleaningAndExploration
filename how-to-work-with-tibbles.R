# using tibbles
library(tibble)
# throws error:
data.frame(x = 1:5, x_squared = x ^ 2)

# works:
tibble(x = 1:5, x_squared = x^2)

#We can use tribble() to write out tibbles row-by-row. 
#  Formulas like ~x are used to denote column names.

tribble(
  ~ Film, ~ Year,
  "A New Hope", 1977,
  "The Empire Strikes Back", 1980,
  "Return of the Jedi", 1983)

# adding rows 
df <- tibble(comment = "original", x = 1:2, x_squared = x ^ 2)

df <- df %>% 
  add_row(comment = "append", x = 3:4, x_squared = x ^ 2) %>% 
  add_row(comment = "prepend", x = 0, x_squared = x ^ 2, .before = 1) %>%
  add_row(comment = "prepend2", x = 0, x_squared = x ^ 2, .before = 3)

df %>% 
  add_row(x = 5, comment = "NA defaults") %>% 
  add_row(x_squared = 36, x = 6, comment = "order doesn't matter")

# insert col w single val
df %>% 
  add_column(comment2 = "inserted column", .after = "comment")

as_tibble(mtcars)
mtcars %>% 
  as_tibble() %>% 
  rownames_to_column("model")

