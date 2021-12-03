globalassign <- function(...) {
  for (x in c(...)) assign(x, eval.parent(parse(text = x)), .GlobalEnv)
}

skip("Not yet ready")

test_that("wrapper activity setting and detection works", {
  
  
  
  calcTest <- function() {
    expect_false(isWrapperActive("readSource"))
    expect_true(isWrapperActive("calcOutput"))
    
    readSource("Test2", convert=FALSE)
    
    expect_false(isWrapperActive("readSource"))
    expect_true(isWrapperActive("calcOutput"))
    
  }
  
  readTest2 <- function() {
    
    expect_true(isWrapperActive("readSource"))
    expect_true(isWrapperActive("calcOutput"))
    
    return(as.magpie(12))
  }
  
  downloadTest2 <- function() {
    expect_true(isWrapperActive("readSource"))
    expect_true(isWrapperActive("calcOutput"))
    expect_true(isWrapperActive("downloadSource"))
  }
  
  
})