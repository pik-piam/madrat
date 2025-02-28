# SPDX-FileCopyrightText: 2025 Potsdam Institute for Climate Impact Research (PIK)
# SPDX-License-Identifier: BSD-2-Clause

test_that("toolSplitSubtype works as expected", {
  ref <- list(model = c("mymodel", "notmymodel"), version = c("myversion", "42"), world = "myworld")
  expect_identical(toolSplitSubtype("mymodel:myversion:myworld", ref),
                   list(model = "mymodel", version = "myversion", world = "myworld"))
  expect_error(toolSplitSubtype("mymodel:blub:myworld", ref), "Invalid subtype")
  ref2 <- list(model = c("mymodel", "notmymodel"), version = NULL, world = "myworld")
  expect_identical(toolSplitSubtype("mymodel:blub:myworld", ref2),
                   list(model = "mymodel", version = "blub", world = "myworld"))
  expect_error(toolSplitSubtype("mymodel:myworld", ref2), "does not follow expected structure")
})
