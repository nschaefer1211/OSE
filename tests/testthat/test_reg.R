#Beachte: Aus Vollständigkeitsgründen existieren diese Testfiles, sie sind aber zu nicht viel nütze, da
#unsere Resultate Approximationen sind, die wir mit keinen existenten Datensets abgleichen können.
#Wertvoller zum Verständnis des Pakets und zur Überprüfung der Funktionen sind die Datei 'plots' und die Vignette.

context("Test korrekter Ablauf bei der Simulation von Stichprobendaten")
test_that("datsim", {
  expect_is(datsim(Vectorize(function(x) 1), log, 100), "data.frame") #check if datsim returns dataframe
  expect_error(datsim(Vectorize(function(x) 1), log, c(100, 100))) #check if there is an error if argument n has is not of length 1
  expect_error(datsim(Vectorize(function(x) 1), log, "a")) #check if there is an error if argument n has is not of type numeric
  expect_error(datsim(Vectorize(function(x) 1), 100, 100)) #check if argument m has to be of type function
  expect_error(datsim(100, log, c(100, 100))) #check if argument dens has to be of type function
})


context("Test korrekter Ablauf bei Schätzung der Regressionsfunktion") 
test_that("reg_est", {
  expect_is(reg_est(Trig_Basis$new(1000), Trig_Basis$new(1000), datsim(Vectorize(function(x) 1), log, 80)), "function") #check if reg_est outputs a function
  expect_error(reg_est(Trig_Basis$new(100), Hist_Basis$new(100), seq(0, 100)))  #check if there is an error if argument n is not of type data.frame
  expect_error(reg_est(function(x) 1, Hist_Basis$new(100), datsim(Vectorize(function(x) 1), log ,100)))  #check if there is an error if argument basis1 is not of type basis
  expect_error(reg_est(Trig_Basis$new(100), function(x) 1, datsim(Vectorize(function(x) 1), log ,100)))  #check if there is an error if argument basis2 is not of type basis
})
