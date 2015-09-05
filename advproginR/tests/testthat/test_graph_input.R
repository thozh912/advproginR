library(advproginR)

context("Graph data")

test_that("graph has correct input",{ 
  expect_error(dijkstra(data.frame(c(1,1,2,2,3,3),c(3,2,1,3,1,2),c(2,3,3,4,2,4),c("a","b","c","d","e","f")),3))
  #expect_equal(length)
  
  })
  
test_that("graph has correct output",{
  expect_equal(length(dijkstra(wiki_graph,3)),6)
  
})