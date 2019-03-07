
context("soilpsi")

test_that("errors on bad input", {

  # sand-silt-clay
  expect_error(soilpsi(sand = 400, silt = 40, clay = 20,
                       vwc = 0.4, vwcsat = 0.8, organic = 10))
  expect_error(soilpsi(sand = 40, silt = 400, clay = 20,
                       vwc = 0.4, vwcsat = 0.8, organic = 10))
  expect_error(soilpsi(sand = 40, silt = 40, clay = 200,
                       vwc = 0.4, vwcsat = 0.8, organic = 10))
  expect_error(soilpsi(sand = 40, silt = 40, clay = 40,
                       vwc = 0.4, vwcsat = 0.8, organic = 10))
  expect_error(soilpsi(sand = 20, silt = 20, clay = 20,
                       vwc = 0.4, vwcsat = 0.8, organic = 10))

  # vwc
  expect_error(soilpsi(sand = 40, silt = 40, clay = 20,
                       vwc = -0.4, vwcsat = 0.8, organic = 10))
  expect_error(soilpsi(sand = 40, silt = 40, clay = 20,
                       vwc = 0.8, vwcsat = 0.4, organic = 10))

  # organic
  expect_error(soilpsi(sand = 40, silt = 40, clay = 20,
                       vwc = 0.4, vwcsat = 0.8, organic = -10))

})

test_that("returns expected data", {

  capture.output({
    out <- soilpsi(sand = 40, silt = 40, clay = 20,
                   vwc = 0.4, vwcsat = 0.8, organic = 10)
  }, file = NULL)

  expect_is(out, "list")
  expect_identical(sort(names(out)), c("psi", "psisat", "smp_l"))

  # honors quiet flag
  expect_output(soilpsi(sand = 40, silt = 40, clay = 20,
                        vwc = 0.4, vwcsat = 0.8, organic = 10, quiet = FALSE))
  expect_silent(soilpsi(sand = 40, silt = 40, clay = 20,
                        vwc = 0.4, vwcsat = 0.8, organic = 10, quiet = TRUE))
})
