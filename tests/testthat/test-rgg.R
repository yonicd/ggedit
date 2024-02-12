testthat::context("remove replace functionality")

testthat::test_that("remove", {
  testthat::skip(message = 'skip')
  p <- ggplot2::ggplot(iris,ggplot2::aes(x =Sepal.Length,y=Sepal.Width)) + ggplot2::geom_line()
  p1 <- p + ggplot2::geom_point(ggplot2::aes(colour=Species))
 
  pnew <- p1 |> remove_geom('point',1)
  
  testthat::expect_equal(p,pnew)  

})

testthat::test_that("remove and replace", {
  testthat::skip(message = 'skip')
  p <- ggplot2::ggplot(iris,ggplot2::aes(x =Sepal.Length,y=Sepal.Width)) + ggplot2::geom_line()
  
  p1 <- p + ggplot2::geom_point(ggplot2::aes(colour=Species))
  
  thislayer <- ggplot2::geom_point(ggplot2::aes(colour=Species),shape=4)
  
  p2 <- p + thislayer
  
  pnew <- p1 |> rgg('point', 1, thislayer)
  
  testthat::expect_equal(p2, pnew)  
  
})

