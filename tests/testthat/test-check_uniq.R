test_that("check_uniq passes when data is unique", {
  df_unique <- data.frame(id = 1:3, name = c("A", "B", "C"))
  expect_no_error(check_uniq(df_unique, vars = c("id")))
})

