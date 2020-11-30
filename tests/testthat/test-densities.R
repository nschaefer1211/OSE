#Beachte: Aus Vollständigkeitsgründen existieren diese Testfiles, sie sind aber zu nicht viel nütze, da
#unsere Resultate Approximationen sind, die wir mit keinen existenten Datensets abgleichen können.
#Wertvoller zum Verständnis des Pakets und zur Überprüfung der Funktionen sind die Datei 'plots' und die Vignette.

context("Tests für die Funktionen des Pakets, die Dichten Schätzen")
test_that("est_dens", {
  expect_is(est_dens(Trig_Basis$new(100), runif(50), 6), "function") #checking that output is function
  expect_error(est_dens(Trig_Basis$new(100), runif(50), -2)) #error if dimension is negative
  expect_error(est_dens(Trig_Basis$new(100), runif(50), 42.5)) #error if dimension is not integer 
})
test_that("est_dens2", {
  expect_is(est_dens2(Trig_Basis$new(100), data.frame(runif(50), runif(50)), c(6,8)), "function") #checking that output is function
  our_dens <-  function(x) 1
  our_dens <- Vectorize(our_dens)
  our_data <- data.frame(rdens(our_dens, 200), rdens(our_dens, 200))
  expect_equal(est_dens2(Trig_Basis$new(100), our_data, c(5, 5)), est_dens2(Trig_Basis$new(100), our_data, 5)) #checking if dimension vector is reproduced
})
