library(advproginR)

context("Graph data")

test_that("dijsktra can handle simple graphs",{
  expect_equal(dijkstra(data.frame(c(1,2),c(2,1),c(5,5)),1),c(0,5))
  expect_equal(dijkstra(data.frame(c(1,1,2,2,3,3),c(3,2,1,3,1,2),c(2,3,3,4,2,4)),3),c(2,4,0))
  
  #expect_equal(length)
  
  })

test_that("dijkstra produces error on incorrect input",{
  expect_error(dijkstra(list(c(1,1,2,2,3,3),c(3,2,1,3,1,2),c(2,3,3,4,2,4)),3))
  expect_error(dijkstra(list(c(1,1,2,2,3,3),c(3,2,1,3,1,2),c(2,3,3,4,2,4)),-4))
})
test_that("graph has correct output",{
  expect_equal(length(dijkstra(wiki_graph,3)),6)
  expect_equal(dijkstra(wiki_graph,3),c(9,10,0,11,11,2))
  
})