context("remove replace functionality")
test_that("remove", {
  
  p <- ggplot2::ggplot(iris,ggplot2::aes(x =Sepal.Length,y=Sepal.Width)) + ggplot2::geom_line()
  p1 <- p + ggplot2::geom_point(ggplot2::aes(colour=Species))
 
  pnew <- p1%>%remove_geom('point',1)
  
  expect_equal(p,pnew)  

})

test_that("remove and replace", {
  
  p <- ggplot2::ggplot(iris,ggplot2::aes(x =Sepal.Length,y=Sepal.Width)) + ggplot2::geom_line()
  
  p1 <- p + ggplot2::geom_point(ggplot2::aes(colour=Species))
  
  thislayer <- ggplot2::geom_point(ggplot2::aes(colour=Species),shape=4)
  
  p2 <- p + thislayer
  
  pnew <- p1%>%rgg('point',1,thislayer)
  
  expect_equal(p2,pnew)  
  
})

