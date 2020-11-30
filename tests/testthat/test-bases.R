#Beachte: Aus Vollständigkeitsgründen existieren diese Testfiles, sie sind aber zu nicht viel nütze, da
#unsere Resultate Approximationen sind, die wir mit keinen existenten Datensets abgleichen können.
#Wertvoller zum Verständnis des Pakets und zur Überprüfung der Funktionen sind die Datei 'plots' und die Vignette.

context("Test korrekte Berechnung der Basisfunktionen") 
test_that("trigonometrische Basis", {
  expect_equal(Trig_Basis$new(1000)$get_function(4)(1), sqrt(2))
  expect_equal(Trig_Basis$new(1000)$get_function(4)(2), sqrt(2))
  expect_equal(Trig_Basis$new(1000)$get_function(4)(3), sqrt(2))
  expect_equal(Trig_Basis$new(1000)$get_function(4)(4), sqrt(2))
  expect_equal(Trig_Basis$new(1000)$get_function(5)(1), 0)
  expect_equal(Trig_Basis$new(1000)$get_function(5)(2), 0)
  expect_equal(Trig_Basis$new(1000)$get_function(5)(3), 0)
  expect_equal(Trig_Basis$new(1000)$get_function(5)(4), 0)
  expect_is(Trig_Basis$new(1000), "Trig_Basis")
  expect_is(Trig_Basis$new(1000)$get_function(4), "function") 
  expect_identical(Trig_Basis$new(1000)$dimension, 1000)
})
test_that("Histogram Basis", {
  expect_equal(Hist_Basis$new(12)$get_function(1)(0), sqrt(12))
  expect_equal(Hist_Basis$new(12)$get_function(1)(1), 0)
  expect_equal(Hist_Basis$new(12)$get_function(2)(0), 0)
  expect_equal(Hist_Basis$new(12)$get_function(2)(0.1), sqrt(12))
  expect_equal(Hist_Basis$new(12)$get_function(2)(1), 0)
  expect_is(Hist_Basis$new(12), "Hist_Basis")
  expect_is(Hist_Basis$new(12)$get_function(4), "function") 
  expect_identical(Hist_Basis$new(12)$dimension, 12)
})
