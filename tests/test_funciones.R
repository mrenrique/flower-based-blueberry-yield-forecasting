# ============================================================
# PRUEBAS DE LAS FUNCIONES DE CÁLCULO
# ============================================================
# Uso, desde la raíz del repositorio:
#
#   Rscript tests/test_funciones.R
#
# Qué es esto: cada bloque test_that() corre una función con una entrada
# conocida y comprueba que el resultado sea el esperado. expect_equal()
# compara, expect_error() verifica que falle cuando debe fallar. Si algo
# no coincide, la ejecución se detiene e indica qué prueba falló.
#
# Para qué sirve: si alguien cambia el cálculo de la curva o la regla de
# pasadas de cosecha y rompe algo, estas pruebas lo detectan antes de que
# salga una proyección incorrecta.
#
# Cubren las dos funciones que concentran la lógica del modelo
# (curva de maduración y agrupamiento de pasadas) y dos validaciones.

suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
})

source(file.path("R", "00_utils.R"), encoding = "UTF-8")


# ============================================================
# 1. SEMANA ISO
# ============================================================

test_that("fecha_semana_iso devuelve el lunes correcto de cada semana ISO", {
  # La semana 35 de 2026 arranca el lunes 24 de agosto
  expect_equal(fecha_semana_iso(2026, 35), as.Date("2026-08-24"))

  # Siempre cae lunes ("%u" == 1) y coincide con lubridate
  fechas <- fecha_semana_iso(2026, 1:52)
  expect_true(all(format(fechas, "%u") == "1"))
  expect_equal(lubridate::isoweek(fechas), 1:52)
})


# ============================================================
# 2. CURVA DE MADURACIÓN
# ============================================================

test_that("la curva reparte exactamente el 100 % de las flores", {
  curva <- curva_maduracion_normalizada(periodo = 21, media = 11, desviacion = 4)

  expect_equal(nrow(curva), 21)
  expect_equal(sum(curva$DISTRIBUCION), 1, tolerance = 1e-12)
  expect_true(all(curva$DISTRIBUCION > 0))
})

test_that("MEDIA desplaza el pico y DESVIACION lo aplana", {
  pico <- function(media) {
    curva <- curva_maduracion_normalizada(periodo = 30, media = media, desviacion = 4)
    curva$Dias[which.max(curva$DISTRIBUCION)]
  }

  expect_equal(pico(8), 8)
  expect_equal(pico(15), 15)

  # Más desviación reparte más parejo: el pico baja, pero el total sigue en 1
  estrecha <- curva_maduracion_normalizada(30, 15, 2)
  ancha    <- curva_maduracion_normalizada(30, 15, 8)

  expect_gt(max(estrecha$DISTRIBUCION), max(ancha$DISTRIBUCION))
  expect_equal(sum(ancha$DISTRIBUCION), 1, tolerance = 1e-12)
})

test_that("una desviación inválida detiene la ejecución", {
  # Con desviación 0 la curva no se puede normalizar
  expect_error(
    curva_maduracion_normalizada(periodo = 10, media = 5, desviacion = 0),
    "Curva de maduración inválida"
  )
})


# ============================================================
# 3. PASADAS DE COSECHA
# ============================================================

test_that("las fechas se agrupan en pasadas según su separación", {
  # Días consecutivos: una sola pasada
  expect_equal(
    agrupar_pasadas_cosecha(as.Date(c("2026-07-06", "2026-07-07")), 2),
    c(1, 1)
  )

  # Fechas separadas: una pasada cada una
  expect_equal(
    agrupar_pasadas_cosecha(as.Date(c("2026-07-06", "2026-07-10", "2026-07-14")), 2),
    c(1, 2, 3)
  )
})

test_that("sábado y lunes cuentan como dos pasadas con el umbral de 2 días", {
  # Caso ambiguo del criterio actual: no se cosecha el domingo, así que la
  # diferencia es de 2 días y el lunes abre una pasada nueva. Queda fijado
  # por prueba: si el criterio de negocio cambia, esta falla y obliga a
  # decidirlo explícitamente en lugar de cambiarlo sin darse cuenta.
  sabado_lunes <- as.Date(c("2026-07-11", "2026-07-13"))

  expect_equal(agrupar_pasadas_cosecha(sabado_lunes, 2), c(1, 2))
  expect_equal(agrupar_pasadas_cosecha(sabado_lunes, 3), c(1, 1))
})


# ============================================================
# 4. VALIDACIONES
# ============================================================

test_that("validar_unicidad detecta duplicados en la clave indicada", {
  df <- tibble::tibble(
    SubParcela = c("SP01", "SP01", "SP02"),
    Semana     = c(35, 35, 35),
    Peso       = c(2.0, 2.1, 1.9)
  )

  # La clave completa sí es única: no debe pasar nada
  expect_silent(validar_unicidad(df, c("SubParcela", "Semana", "Peso"), "df"))

  # Pero SubParcela-Semana está duplicada: debe detenerse
  expect_error(
    capture.output(validar_unicidad(df, c("SubParcela", "Semana"), "pesos")),
    "Hay duplicados en pesos"
  )
})

test_that("validar_semana_iso rechaza semanas que no existen", {
  # 2025 tiene 52 semanas: una semana 53 de 2025 es un error de convención
  # en el insumo y pasaría silenciosa sin esta validación.
  expect_error(
    capture.output(
      validar_semana_iso(tibble::tibble(Anio = 2025, Semana = 53),
                         "Anio", "Semana", "viabilidad")
    ),
    "no son semanas ISO válidas"
  )

  # 2026 sí tiene 53 semanas
  expect_silent(
    validar_semana_iso(tibble::tibble(Anio = 2026, Semana = 53),
                       "Anio", "Semana", "viabilidad")
  )
})


message("\n[OK] Las 8 pruebas pasaron.")
