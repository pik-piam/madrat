test_that("prepExtendedComment works in functions called via package::fun", {
  # need to run in separate R session to make sure madrat is not attached / loaded via load_all
  expect_identical(callr::r(function() madrat::.testPrepExtendedComment()[1:2]),
                   c(" description: example",
                     " unit: m"))
})
