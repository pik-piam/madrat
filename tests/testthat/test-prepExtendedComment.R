test_that("prepExtendedComment works in functions called via package::fun", {
  expect_identical(madrat:::testPrepExtendedComment()[1:2],
                   c(" description: example",
                     " unit: m"))
})
