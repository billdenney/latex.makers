test_that("escape_latex", {
  expect_equal(
    escape_latex("A\\^~#$%&_{}BCD"),
    "A\\textbackslash{}\\textasciicircum{}\\textasciitilde{}\\#\\$\\%\\&\\_\\{\\}BCD"
  )
  expect_equal(
    escape_latex(c("A\\^~#$%&_{}BCD", "foo")),
    c(
      "A\\textbackslash{}\\textasciicircum{}\\textasciitilde{}\\#\\$\\%\\&\\_\\{\\}BCD",
      "foo"
    )
  )
})
