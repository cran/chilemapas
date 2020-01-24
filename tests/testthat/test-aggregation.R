context("testthat.R")

test_that("chilemapas datasets can be aggregated", {
  library(dplyr)
  library(sf)
  r14 <- filter(chilemapas::mapa_comunas, codigo_region == 14)

  r14_p <- chilemapas::generar_provincias(r14)
  r14_r <- chilemapas::generar_regiones(r14)

  r14_c <- chilemapas::generar_circunscripciones(r14)
  r14_d <- chilemapas::generar_distritos(r14)

  r14_s <- chilemapas::generar_servicios_salud(r14)

  expect_is(r14_p, "data.frame")
  expect_is(r14_r, "data.frame")
  expect_is(r14_c, "data.frame")
  expect_is(r14_d, "data.frame")
  expect_is(r14_s, "data.frame")
})