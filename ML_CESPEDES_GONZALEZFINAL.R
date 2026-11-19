# ==============================================================================
# Victoria Céspedes
# Antonia González
# Evaluación Modelos Lineales — Fila B
# Caso B: Amenazas naturales declaradas y ganancias (VIII EME 2025)
# ==============================================================================

# ------------------------------------------------------------------------------
# CARGA DE LIBRERÍAS
# ------------------------------------------------------------------------------
paquetes_necesarios <- c("lmtest", "car", "dplyr", "tidyr", "ggplot2", "scales", "glmnet")

invisible(sapply(paquetes_necesarios, library, character.only = TRUE))

# ------------------------------------------------------------------------------
# CARGA DE DATOS SEGÚN PLANTILLA (REPRODUCIBLE Y A PRUEBA DE FALLOS)
# ------------------------------------------------------------------------------
# 1. Intentar buscar el archivo en las posibles ubicaciones relativas
posibles_rutas <- c(
  "03_Datos_EME8/EME8_base_full_original.csv",
  "../03_Datos_EME8/EME8_base_full_original.csv",
  "../../03_Datos_EME8/EME8_base_full_original.csv"
)

ruta_base <- posibles_rutas[file.exists(posibles_rutas)][1]

# 2. Si RStudio no está en el directorio correcto, abrir ventana para seleccionarlo a mano
if (is.na(ruta_base)) {
  message("Directorio de trabajo no coincide. Seleccione manualmente el archivo EME8_base_full_original.csv")
  ruta_base <- file.choose()
}

# Extraer la carpeta donde está el CSV para cargar también los IDs
datos_dir <- dirname(ruta_base)

base <- read.csv(ruta_base, check.names = FALSE)
ids <- read.csv(file.path(datos_dir, "ids_particiones_pliegues.csv"))

datos <- merge(base, ids, by = "id", all = FALSE)
stopifnot(nrow(base) == 7170, nrow(datos) == 6747, !anyDuplicated(datos$id))

train <- subset(datos, particion == "entrenamiento")
test  <- subset(datos, particion == "prueba")
stopifnot(nrow(train) == 5360, nrow(test) == 1387)

print(table(train$fold))
# ==============================================================================
# 1. Audite datos y códigos especiales. Informe sus reglas de limpieza. (10 PTOS) ----
# ==============================================================================

# 1a) Confirme universo, claves, muestra común y tipos de las variables ambientales y de ganancia.
cat("\n--- 1a) Auditoría Inicial ---\n")
cat("Universo base EME8:", nrow(base), "observaciones\n")
cat("Muestra común (cruce con particiones):", nrow(datos), "observaciones (claves id únicas:", length(unique(datos$id)) == nrow(datos), ")\n")
cat("N° Observaciones en entrenamiento:", nrow(train), "\n")
cat("N° Observaciones en prueba:", nrow(test), "\n")
cat("Integridad de claves únicas (id) en entrenamiento:", length(unique(train$id)) == nrow(train), "\n")
cat("Tipo variable respuesta (ganancia_final):", class(train$ganancia_final), "\n")

vars_amenazas <- c("m2_1", "m2_2", "m2_3", "m2_4")
cat("Tipos variables ambientales (m2_1 a m2_4):", paste(sapply(train[, vars_amenazas], class), collapse = ", "), "\n")


# 1b) Recodifique m2_1–m2_4: 1=Sí, 2=No; no trate 88/99 como ausencia de amenazas.
train <- train %>%
  mutate(across(all_of(vars_amenazas), ~ ifelse(.x %in% c(88, 99), NA, .x)))

test <- test %>%
  mutate(across(all_of(vars_amenazas), ~ ifelse(.x %in% c(88, 99), NA, .x)))


# 1c) Construya alguna_amenaza, revise m2_5 (ninguna) y documente inconsistencias de respuesta.
train$m2_5 <- ifelse(train$m2_5 %in% c(88, 99), NA, train$m2_5)

# Crear variable 'alguna_amenaza': 1 si al menos una amenaza es 1, 2 si todas son 2
train$alguna_amenaza <- apply(train[, vars_amenazas], 1, function(x) {
  if (all(is.na(x))) return(NA)
  if (any(x == 1, na.rm = TRUE)) return(1)
  return(2)
})

inconsistencias <- sum(train$alguna_amenaza == 1 & train$m2_5 == 1, na.rm = TRUE)
cat("\n--- 1c) Inconsistencias ---\n")
cat("Negocios con respuestas contradictorias (alguna_amenaza=1 y m2_5=1):", inconsistencias, "\n")

# Subconjunto EDA sin NA en amenaza
train_eda <- train %>% filter(!is.na(alguna_amenaza))
train_eda$amenaza_cat <- factor(train_eda$alguna_amenaza, 
                                levels = c(1, 2), 
                                labels = c("Con Amenaza", "Sin Amenaza"))


# ==============================================================================
# 2. Realice EDA exclusivamente en entrenamiento. (10 PTOS) ----
# ==============================================================================

# 2a) Compare distribución de ganancias entre negocios con y sin amenazas (mediana, media, cuartiles y extremos).
cat("\n--- 2a) Distribución de Ganancias entre Negocios Con y Sin Amenazas ---\n")
resumen_2a <- do.call(rbind, tapply(train$ganancia_final, train$alguna_amenaza, function(y) {
  c(n = length(y),
    Min = min(y),
    Q1 = unname(quantile(y, 0.25)),
    Mediana = median(y),
    Media = round(mean(y), 0),
    Q3 = unname(quantile(y, 0.75)),
    Max = max(y),
    DE = round(sd(y), 0),
    Negativos = sum(y < 0),
    Ceros = sum(y == 0))
}))
rownames(resumen_2a) <- c("Con Amenaza (1)", "Sin Amenaza (2)")
print(resumen_2a)


# 2b) Grafique frecuencias de las cuatro amenazas y cruces por sector económico.
cat("\n--- 2b) Frecuencias de las Cuatro Amenazas y Cruce por Sector ---\n")

amenazas_long <- train_eda %>%
  dplyr::select(id, c1_b, all_of(vars_amenazas)) %>%
  pivot_longer(cols = all_of(vars_amenazas), names_to = "Variable", values_to = "Presencia") %>%
  filter(Presencia == 1) %>%
  mutate(Amenaza = dplyr::recode(Variable,
                                 "m2_1" = "Sequías/Heladas",
                                 "m2_2" = "Inundaciones/Aluviones",
                                 "m2_3" = "Incendios Forestales",
                                 "m2_4" = "Tormentas/Lluvias"),
         Sector = factor(c1_b, 
                         levels = 1:6, 
                         labels = c("Agropecuario", "Manufactura", "Construcción", 
                                    "Comercio", "Transporte", "Otros")))

# 1. Gráfico de frecuencias de las cuatro amenazas (solicitado en pauta)
p_2b_frec <- ggplot(amenazas_long, aes(x = Amenaza, fill = Amenaza)) +
  geom_bar(color = "white") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 3.5) +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  labs(title = "Frecuencia de las Cuatro Amenazas Naturales Declaradas",
       x = "Tipo de Amenaza",
       y = "N° de Negocios Afectados")
print(p_2b_frec)

# 2. Gráfico de barras apiladas al 100% por sector (cruce por sector económico)
p_2b_sector <- ggplot(amenazas_long, aes(x = Amenaza, fill = Sector)) +
  geom_bar(position = "fill", color = "white") +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Composición Sectorial de las Amenazas Naturales Declaradas",
       x = "Tipo de Amenaza",
       y = "Proporción Sectorial (%)")
print(p_2b_sector)


# 2c) Examine variación regional y explique por qué no se puede atribuir causalidad a diferencias de grupos.
cat("\n--- 2c) Variación Regional y Justificación de No Causalidad ---\n")

total_reg <- tapply(train$ganancia_final, train$region, length)
amenaza_reg <- tapply(train$alguna_amenaza == 1, train$region, sum, na.rm = TRUE)

# Porcentaje de amenaza por región
porc_reg <- round(100 * amenaza_reg / total_reg, 1)
print(porc_reg)

# Mediana ganancia mensual por región
cat("\nMediana ganancia mensual por región:\n")
print(tapply(train$ganancia_final, train$region, median))

# Gráfico de barras original: % de negocios con amenazas por región
par(mar = c(5, 4, 4, 2) + 0.1) 
barplot(porc_reg, col = "maroon", las = 2, ylim = c(0, 60),
        main = "% de negocios con amenazas por región",
        xlab = "Región", ylab = "% de negocios")
par(mar = c(5, 4, 4, 2)) # restaurar márgenes estándar

cat("\nJustificación:
No se puede afirmar que los desastres naturales sean la única causa de la caída 
en las ganancias. Como esto es solo una encuesta y no un experimento controlado, 
estamos comparando realidades muy distintas. Por ejemplo, los negocios agrícolas 
o de ciertas zonas rurales sufren más sequías por su ubicación, pero sus niveles 
de ingresos y meses de ventas son naturalmente distintos a los de un comercio en 
la ciudad. Si comparamos sus ganancias directamente sin tomar en cuenta el sector 
o la región, llegaremos a conclusiones engañosas.
Además, las amenazas declaradas abarcan una ventana temporal distinta a la de 
la ganancia mensual declarada (que corresponde al último mes), y existen sesgos 
de autodeclaración inherentes al diseño observacional.\n")


# ==============================================================================
# PREGUNTA 3: OLS y supuestos (12 PTOS)
# ==============================================================================

# Preparación del dataset de entrenamiento para OLS
# Referencia de amenazas: "No" (código 2), para que el coeficiente mida la diferencia
# ajustada asociada a la presencia ("Sí", código 1) de cada amenaza natural.
train_ols <- train %>%
  mutate(
    m2_1_f = factor(m2_1, levels = c(2, 1), labels = c("No", "Si")),
    m2_2_f = factor(m2_2, levels = c(2, 1), labels = c("No", "Si")),
    m2_3_f = factor(m2_3, levels = c(2, 1), labels = c("No", "Si")),
    m2_4_f = factor(m2_4, levels = c(2, 1), labels = c("No", "Si")),
    informalidad_f = factor(informalidad, levels = c(0, 1), labels = c("Formal", "Informal")),
    c1_b_f = factor(c1_b, levels = 1:6,
                    labels = c("Agropecuario", "Manufactura", "Construccion",
                               "Comercio", "Transporte", "Otros")),
    sexo_f = factor(sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    region_f = factor(region)
  ) %>%
  dplyr::select(id, fold, ganancia_final, m2_1_f, m2_2_f, m2_3_f, m2_4_f,
                c6_3, c7, total_trabajadores, informalidad_f, c1_b_f, sexo_f, region_f) %>%
  na.omit()

# --- 3a) Ajuste OLS con indicadores ambientales y controles; interprete 2 coeficientes (4 ptos) ---
cat("\n==================================================================\n")
cat("3a) AJUSTE MODELO OLS COMPLETO (INDICADORES AMBIENTALES + CONTROLES)\n")
cat("==================================================================\n")
cat("N° de observaciones con datos completos en entrenamiento:", nrow(train_ols), "\n\n")

mod_ols_completo <- lm(
  ganancia_final ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f +
    c6_3 + c7 + total_trabajadores + informalidad_f + c1_b_f + sexo_f + region_f,
  data = train_ols
)

print(summary(mod_ols_completo))

cat("\nResumen específico de los 4 coeficientes ambientales ajustados (con IC 95%):\n")
vars_amb_coef <- c("m2_1_fSi", "m2_2_fSi", "m2_3_fSi", "m2_4_fSi")
tabla_coefs_amb <- cbind(
  summary(mod_ols_completo)$coefficients[vars_amb_coef, , drop = FALSE],
  confint(mod_ols_completo, parm = vars_amb_coef)
)
print(round(tabla_coefs_amb, 2))


# --- 3b) Evalúe residuos/ajustados, Q–Q, Breusch–Pagan y Jarque–Bera (4 ptos) ---
cat("\n==================================================================\n")
cat("3b) DIAGNÓSTICO DE SUPUESTOS: RESIDUOS, BREUSCH-PAGAN Y JARQUE-BERA\n")
cat("==================================================================\n")

# Gráficos de diagnóstico: Residuos vs Ajustados y Q-Q Normal
par(mfrow = c(1, 2))
plot(mod_ols_completo, which = 1, main = "Residuos vs Ajustados (OLS Completo)")
plot(mod_ols_completo, which = 2, main = "Normal Q-Q Plot (OLS Completo)")
par(mfrow = c(1, 1))

# Test de Breusch-Pagan para heterocedasticidad (H0: varianza del error constante)
bp_completo <- bptest(mod_ols_completo)
cat("\n1. Test de Breusch-Pagan (Homocedasticidad):\n")
print(bp_completo)

# Test de Jarque-Bera para normalidad de los residuos (H0: normalidad, S=0 y K=3)
jarque_bera_test <- function(res) {
  n <- length(res)
  s <- mean((res - mean(res))^3) / (mean((res - mean(res))^2)^(3/2))
  k <- mean((res - mean(res))^4) / (mean((res - mean(res))^2)^2)
  jb_stat <- (n / 6) * (s^2 + ((k - 3)^2) / 4)
  p_val <- pchisq(jb_stat, df = 2, lower.tail = FALSE)
  return(c(JB_stat = jb_stat, df = 2, p_value = p_val, Asimetria = s, Curtosis = k))
}

jb_completo <- jarque_bera_test(residuals(mod_ols_completo))
cat("\n2. Test de Jarque-Bera (Normalidad de residuos):\n")
print(round(jb_completo, 4))


# --- 3c) Examine VIF y RESET; compare diagnóstico con modelo de solo controles (4 ptos) ---
cat("\n==================================================================\n")
cat("3c) MULTICOLINEALIDAD (VIF), FORMA FUNCIONAL (RESET) Y COMPARACIÓN\n")
cat("==================================================================\n")

cat("1. VIF / GVIF del Modelo Completo (Amenazas + Controles):\n")
vif_completo <- vif(mod_ols_completo)
print(round(vif_completo, 3))

cat("\n2. Test RESET de Ramsey en Modelo Completo (H0: forma funcional lineal bien especificada):\n")
reset_completo <- resettest(mod_ols_completo, power = 2:3, type = "fitted")
print(reset_completo)

# Ajuste del Modelo de SOLO CONTROLES sobre exactamente la misma muestra (train_ols)
mod_solo_controles <- lm(
  ganancia_final ~ c6_3 + c7 + total_trabajadores + informalidad_f + c1_b_f + sexo_f + region_f,
  data = train_ols
)

bp_controles    <- bptest(mod_solo_controles)
jb_controles    <- jarque_bera_test(residuals(mod_solo_controles))
reset_controles <- resettest(mod_solo_controles, power = 2:3, type = "fitted")

cat("\n3. VIF / GVIF del Modelo de Solo Controles:\n")
print(round(vif(mod_solo_controles), 3))

cat("\n4. Test F parcial (ANOVA: Solo Controles vs. Modelo Completo con Amenazas):\n")
print(anova(mod_solo_controles, mod_ols_completo))

cat("\n5. Tabla Comparativa de Diagnósticos (Modelo Solo Controles vs. Modelo Completo):\n")
tabla_comp_3c <- data.frame(
  Diagnostico = c("N observaciones", "R2", "R2 Ajustado", "Sigma (Error Estándar Residual)",
                  "AIC", "BIC",
                  "Breusch-Pagan stat", "Breusch-Pagan p-valor",
                  "Jarque-Bera stat", "Jarque-Bera p-valor",
                  "Asimetría residuos", "Curtosis residuos",
                  "RESET F-stat", "RESET p-valor"),
  Solo_Controles = c(
    nobs(mod_solo_controles),
    summary(mod_solo_controles)$r.squared,
    summary(mod_solo_controles)$adj.r.squared,
    summary(mod_solo_controles)$sigma,
    AIC(mod_solo_controles),
    BIC(mod_solo_controles),
    as.numeric(bp_controles$statistic), bp_controles$p.value,
    jb_controles["JB_stat"], jb_controles["p_value"],
    jb_controles["Asimetria"], jb_controles["Curtosis"],
    as.numeric(reset_controles$statistic), reset_controles$p.value
  ),
  Completo_con_Amenazas = c(
    nobs(mod_ols_completo),
    summary(mod_ols_completo)$r.squared,
    summary(mod_ols_completo)$adj.r.squared,
    summary(mod_ols_completo)$sigma,
    AIC(mod_ols_completo),
    BIC(mod_ols_completo),
    as.numeric(bp_completo$statistic), bp_completo$p.value,
    jb_completo["JB_stat"], jb_completo["p_value"],
    jb_completo["Asimetria"], jb_completo["Curtosis"],
    as.numeric(reset_completo$statistic), reset_completo$p.value
  )
)
tabla_comp_3c[, 2:3] <- round(tabla_comp_3c[, 2:3], 4)
print(tabla_comp_3c)


# ==============================================================================
# PREGUNTA 4: Transformaciones de la respuesta y de predictores (15 PTOS)
# ==============================================================================

# --- 4a) Por qué log(Y) y Box-Cox estándar no aplican directo + Alternativas válidas (5 ptos) ---
cat("\n==================================================================\n")
cat("4a) DOMINIO DE LA RESPUESTA Y TRANSFORMACIONES VÁLIDAS (asinh Y YEO-JOHNSON)\n")
cat("==================================================================\n")

y_tr <- train_ols$ganancia_final
cat("Auditoría de dominio de 'ganancia_final' (Y) en entrenamiento (n =", length(y_tr), "):\n")
cat(" - Observaciones con pérdidas (Y < 0):", sum(y_tr < 0),
    paste0("(", round(mean(y_tr < 0) * 100, 2), "%), Mínimo = "), min(y_tr), "CLP\n")
cat(" - Observaciones con ganancia nula (Y == 0):", sum(y_tr == 0),
    paste0("(", round(mean(y_tr == 0) * 100, 2), "%)\n"))
cat(" - Total no positivas (Y <= 0):", sum(y_tr <= 0),
    paste0("(", round(mean(y_tr <= 0) * 100, 2), "%)\n"))
cat(" - Observaciones estrictamente positivas (Y > 0):", sum(y_tr > 0),
    paste0("(", round(mean(y_tr > 0) * 100, 2), "%)\n\n"))

cat("Explicación metodológica (4a):
Tanto log(Y) como la familia Box-Cox estándar g_lambda(Y) = (Y^lambda - 1)/lambda exigen
estrictamente Y > 0 en los reales. En entrenamiento existen 185 microemprendimientos (3,45%)
con ganancias nulas o pérdidas (hasta -90.000.000 CLP). Eliminar esos 185 casos truncaría
la muestra común e introduciría sesgo de selección (pues los negocios con amenazas naturales
tienen mayor proporción de pérdidas/ceros: 4,38% vs 3,01%). Por ello se evalúan dos familias
matemáticamente definidas en todo R (negativos, ceros y positivos):
  1) Seno hiperbólico inverso escalado: T_c(Y) = asinh(Y / c), con inversa Y = c * sinh(Z),
     evaluando c = 100.000 CLP y c = mediana(|Y_train|) = 400.000 CLP.
  2) Transformación de Yeo-Johnson h_lambda(Y / c), estimando lambda por máxima verosimilitud
     perfilada exclusivamente en el conjunto de entrenamiento (y dentro de cada pliegue en CV).\n")

# Funciones exactas para Yeo-Johnson y su inversa (siguiendo el Manual Editorial UCM)
yj_transform <- function(y, lambda) {
  out <- numeric(length(y))
  pos <- y >= 0
  if (abs(lambda) < 1e-9) out[pos] <- log1p(y[pos])
  else out[pos] <- ((1 + y[pos])^lambda - 1) / lambda
  if (abs(lambda - 2) < 1e-9) out[!pos] <- -log1p(-y[!pos])
  else out[!pos] <- -((1 - y[!pos])^(2 - lambda) - 1) / (2 - lambda)
  out
}

yj_inverse <- function(z, lambda) {
  out <- numeric(length(z))
  pos <- z >= 0
  if (abs(lambda) < 1e-9) out[pos] <- expm1(z[pos])
  else {
    b <- 1 + lambda * z[pos]
    out[pos] <- ifelse(b > 0, b^(1 / lambda) - 1, NA_real_)
  }
  if (abs(lambda - 2) < 1e-9) out[!pos] <- 1 - exp(-z[!pos])
  else {
    b <- 1 - (2 - lambda) * z[!pos]
    out[!pos] <- ifelse(b > 0, 1 - b^(1 / (2 - lambda)), NA_real_)
  }
  out
}

# Estimador de máxima verosimilitud perfilada (con jacobiano) para Yeo-Johnson en entrenamiento
form_rhs_base <- ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f +
  c6_3 + c7 + total_trabajadores + informalidad_f + c1_b_f + sexo_f + region_f

estimar_lambda_yj <- function(df, form_rhs, c_scale = 1e5) {
  y_sc <- df$ganancia_final / c_scale
  X <- model.matrix(form_rhs, data = df)
  n <- nrow(df)
  sum_log_jac <- sum(sign(y_sc) * log1p(abs(y_sc)))
  perfil_ll <- function(l) {
    z <- yj_transform(y_sc, l)
    fit <- lm.fit(X, z)
    rss <- sum(fit$residuals^2)
    -(n / 2) * log(rss / n) + (l - 1) * sum_log_jac
  }
  optimize(perfil_ll, interval = c(-1, 2), maximum = TRUE)$maximum
}

# Ajuste de las alternativas válidas sobre la respuesta en entrenamiento
c_100k <- 100000
c_med  <- median(abs(train_ols$ganancia_final)) # 400.000 CLP en entrenamiento

mod_asinh_100k <- lm(
  asinh(ganancia_final / c_100k) ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f +
    c6_3 + c7 + total_trabajadores + informalidad_f + c1_b_f + sexo_f + region_f,
  data = train_ols
)

mod_asinh_med <- lm(
  asinh(ganancia_final / c_med) ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f +
    c6_3 + c7 + total_trabajadores + informalidad_f + c1_b_f + sexo_f + region_f,
  data = train_ols
)

lambda_yj_tr <- estimar_lambda_yj(train_ols, form_rhs_base, c_scale = c_100k)
cat("\nLambda óptimo de Yeo-Johnson estimado en entrenamiento (Y / 100.000 CLP):",
    round(lambda_yj_tr, 4), "\n")

train_ols$z_yj <- yj_transform(train_ols$ganancia_final / c_100k, lambda_yj_tr)
mod_yj_100k <- lm(
  z_yj ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f +
    c6_3 + c7 + total_trabajadores + informalidad_f + c1_b_f + sexo_f + region_f,
  data = train_ols
)

cat("\nCoeficientes ambientales en escala asinh(Y / 100.000):\n")
print(round(summary(mod_asinh_100k)$coefficients[vars_amb_coef, ], 4))

cat("\nComparación de asimetría y curtosis de residuos (OLS en Y vs alternativas en Y):\n")
tabla_diag_4a <- rbind(
  OLS_Niveles_Y      = c(R2_escala = summary(mod_ols_completo)$r.squared,
                         jarque_bera_test(residuals(mod_ols_completo))[c("Asimetria", "Curtosis", "JB_stat")]),
  asinh_Y_100k       = c(R2_escala = summary(mod_asinh_100k)$r.squared,
                         jarque_bera_test(residuals(mod_asinh_100k))[c("Asimetria", "Curtosis", "JB_stat")]),
  asinh_Y_400k_med   = c(R2_escala = summary(mod_asinh_med)$r.squared,
                         jarque_bera_test(residuals(mod_asinh_med))[c("Asimetria", "Curtosis", "JB_stat")]),
  Yeo_Johnson_Y_100k = c(R2_escala = summary(mod_yj_100k)$r.squared,
                         jarque_bera_test(residuals(mod_yj_100k))[c("Asimetria", "Curtosis", "JB_stat")])
)
print(round(tabla_diag_4a, 4))


# --- 4b) Transformaciones de controles continuos, polinomio e interacción (5 ptos) ---
cat("\n==================================================================\n")
cat("4b) TRANSFORMACIÓN DE CONTROLES CONTINUOS, POLINOMIOS E INTERACCIONES\n")
cat("==================================================================\n")

cat("1. Verificación de dominio de los controles continuos en entrenamiento:\n")
for (v in c("c6_3", "c7", "total_trabajadores")) {
  cat(" -", v, ": min =", min(train_ols[[v]]), "| mediana =", median(train_ols[[v]]),
      "| max =", max(train_ols[[v]]), "| N° ceros =", sum(train_ols[[v]] == 0), "\n")
}
cat("-> Justificación: 'c6_3' (horas semanales) es estrictamente positivo [1, 112], por lo que
   admite log(c6_3) directamente. En cambio, 'total_trabajadores' [0, 10] presenta 3.906 ceros
   (72,87% de microemprendedores sin empleados adicionales), por lo que log(total_trabajadores)
   se indefine (-Inf) y se emplea log(1 + total_trabajadores) = log1p(total_trabajadores).\n\n")

# Especificación 4b.1: Controles continuos transformados en log / log1p
mod_log_controles <- lm(
  ganancia_final ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f +
    log(c6_3) + c7 + log1p(total_trabajadores) + informalidad_f + c1_b_f + sexo_f + region_f,
  data = train_ols
)
cat("Coeficientes de controles transformados (log(c6_3) y log1p(total_trabajadores)):\n")
print(round(summary(mod_log_controles)$coefficients[c("log(c6_3)", "log1p(total_trabajadores)"), ], 2))
b_log_horas <- unname(coef(mod_log_controles)["log(c6_3)"])
cat("Interpretación exacta: Un aumento del 10% en las horas trabajadas semanales (c6_3)\n",
    "se asocia a un cambio de beta * log(1.10) =", round(b_log_horas * log(1.10), 1), "CLP mensuales.\n\n")

# Especificación 4b.2: Términos polinomiales cuadráticos (respetando el principio de jerarquía)
mod_polinomio <- lm(
  ganancia_final ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f +
    c6_3 + I(c6_3^2) + c7 + total_trabajadores + I(total_trabajadores^2) +
    informalidad_f + c1_b_f + sexo_f + region_f,
  data = train_ols
)
cat("2. Modelo con términos polinomiales cuadráticos (respetando jerarquía X + X^2):\n")
print(round(summary(mod_polinomio)$coefficients[c("c6_3", "I(c6_3^2)",
                                                  "total_trabajadores", "I(total_trabajadores^2)"), ], 3))
cat("Test F parcial (OLS Lineal vs OLS Polinomial):\n")
print(anova(mod_ols_completo, mod_polinomio))

# Especificación 4b.3: Interacción ambiental-sectorial y ambiental-regional (respetando jerarquía)
mod_int_m2_1_sector <- lm(
  ganancia_final ~ m2_1_f * c1_b_f + m2_2_f + m2_3_f + m2_4_f +
    c6_3 + c7 + total_trabajadores + informalidad_f + sexo_f + region_f,
  data = train_ols
)

mod_int_m2_3_sector <- lm(
  ganancia_final ~ m2_1_f + m2_2_f + m2_3_f * c1_b_f + m2_4_f +
    c6_3 + c7 + total_trabajadores + informalidad_f + sexo_f + region_f,
  data = train_ols
)

mod_int_m2_1_region <- lm(
  ganancia_final ~ m2_1_f * region_f + m2_2_f + m2_3_f + m2_4_f +
    c6_3 + c7 + total_trabajadores + informalidad_f + c1_b_f + sexo_f,
  data = train_ols
)

cat("\n3. Pruebas F parciales para interacciones ambientales (sectoriales y regionales):\n")
cat("a) Interacción Sequías/Heladas x Sector (m2_1_f * c1_b_f):\n")
print(anova(mod_ols_completo, mod_int_m2_1_sector))
cat("b) Interacción Incendios Forestales x Sector (m2_3_f * c1_b_f):\n")
print(anova(mod_ols_completo, mod_int_m2_3_sector))
cat("c) Interacción Sequías/Heladas x Región (m2_1_f * region_f):\n")
print(anova(mod_ols_completo, mod_int_m2_1_region))


# --- 4c) Compare especificaciones mediante validación cruzada en pesos chilenos (5 ptos) ---
cat("\n==================================================================\n")
cat("4c) COMPARACIÓN DE ESPECIFICACIONES POR VALIDACIÓN CRUZADA (5 PLIEGUES) EN CLP\n")
cat("==================================================================\n")

# Función de métricas en la escala original común (CLP)
metricas_clp <- function(y_real, y_pred) {
  c(
    MAE_CLP  = mean(abs(y_real - y_pred)),
    RMSE_CLP = sqrt(mean((y_real - y_pred)^2)),
    R2_CLP   = 1 - sum((y_real - y_pred)^2) / sum((y_real - mean(y_real))^2)
  )
}

# Validación cruzada en los 5 pliegues suministrados (fold = 1..5),
# reestimando parámetros de escala/transformación dentro de cada pliegue para evitar fuga de datos.
evaluar_cv_5pliegues <- function(especificacion) {
  pred_oof <- numeric(nrow(train_ols))
  pliegues <- sort(unique(train_ols$fold))
  
  for (k in pliegues) {
    idx_val <- train_ols$fold == k
    tr_k    <- train_ols[!idx_val, , drop = FALSE]
    va_k    <- train_ols[idx_val,  , drop = FALSE]
    
    if (especificacion == "1. OLS Solo Controles (Y)") {
      fit <- lm(ganancia_final ~ c6_3 + c7 + total_trabajadores + informalidad_f + c1_b_f + sexo_f + region_f, data = tr_k)
      pred_oof[idx_val] <- predict(fit, newdata = va_k)
      
    } else if (especificacion == "2. OLS Lineal Completo (Y)") {
      fit <- lm(ganancia_final ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f + c6_3 + c7 + total_trabajadores + informalidad_f + c1_b_f + sexo_f + region_f, data = tr_k)
      pred_oof[idx_val] <- predict(fit, newdata = va_k)
      
    } else if (especificacion == "3. OLS Controles Transformados log/log1p (X)") {
      fit <- lm(ganancia_final ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f + log(c6_3) + c7 + log1p(total_trabajadores) + informalidad_f + c1_b_f + sexo_f + region_f, data = tr_k)
      pred_oof[idx_val] <- predict(fit, newdata = va_k)
      
    } else if (especificacion == "4. OLS Polinomial Cuadrático (X + X^2)") {
      fit <- lm(ganancia_final ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f + c6_3 + I(c6_3^2) + c7 + total_trabajadores + I(total_trabajadores^2) + informalidad_f + c1_b_f + sexo_f + region_f, data = tr_k)
      pred_oof[idx_val] <- predict(fit, newdata = va_k)
      
    } else if (especificacion == "5. OLS Interacción Sequía x Sector") {
      fit <- lm(ganancia_final ~ m2_1_f * c1_b_f + m2_2_f + m2_3_f + m2_4_f + c6_3 + c7 + total_trabajadores + informalidad_f + sexo_f + region_f, data = tr_k)
      pred_oof[idx_val] <- predict(fit, newdata = va_k)
      
    } else if (especificacion == "6. OLS Interacción Incendios x Sector") {
      fit <- lm(ganancia_final ~ m2_1_f + m2_2_f + m2_3_f * c1_b_f + m2_4_f + c6_3 + c7 + total_trabajadores + informalidad_f + sexo_f + region_f, data = tr_k)
      pred_oof[idx_val] <- predict(fit, newdata = va_k)
      
    } else if (especificacion == "7. asinh(Y / 100.000) + Controles Lineales") {
      c_k <- 100000
      fit <- lm(asinh(ganancia_final / c_k) ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f + c6_3 + c7 + total_trabajadores + informalidad_f + c1_b_f + sexo_f + region_f, data = tr_k)
      pred_oof[idx_val] <- c_k * sinh(predict(fit, newdata = va_k))
      
    } else if (especificacion == "8. asinh(Y / mediana_k) + Controles Lineales") {
      # Escala c estimada estrictamente dentro del pliegue de entrenamiento k
      c_k <- median(abs(tr_k$ganancia_final))
      fit <- lm(asinh(ganancia_final / c_k) ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f + c6_3 + c7 + total_trabajadores + informalidad_f + c1_b_f + sexo_f + region_f, data = tr_k)
      pred_oof[idx_val] <- c_k * sinh(predict(fit, newdata = va_k))
      
    } else if (especificacion == "9. Yeo-Johnson(Y / 100.000) reestimado por pliegue") {
      c_k   <- 100000
      lam_k <- estimar_lambda_yj(tr_k, form_rhs_base, c_scale = c_k)
      tr_k$z_yj <- yj_transform(tr_k$ganancia_final / c_k, lam_k)
      fit <- lm(z_yj ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f + c6_3 + c7 + total_trabajadores + informalidad_f + c1_b_f + sexo_f + region_f, data = tr_k)
      pred_oof[idx_val] <- c_k * yj_inverse(predict(fit, newdata = va_k), lam_k)
      
    } else if (especificacion == "10. asinh(Y / 100.000) + Controles log/log1p(X)") {
      c_k <- 100000
      fit <- lm(asinh(ganancia_final / c_k) ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f + log(c6_3) + c7 + log1p(total_trabajadores) + informalidad_f + c1_b_f + sexo_f + region_f, data = tr_k)
      pred_oof[idx_val] <- c_k * sinh(predict(fit, newdata = va_k))
    }
  }
  metricas_clp(train_ols$ganancia_final, pred_oof)
}

lista_modelos_4c <- c(
  "1. OLS Solo Controles (Y)",
  "2. OLS Lineal Completo (Y)",
  "3. OLS Controles Transformados log/log1p (X)",
  "4. OLS Polinomial Cuadrático (X + X^2)",
  "5. OLS Interacción Sequía x Sector",
  "6. OLS Interacción Incendios x Sector",
  "7. asinh(Y / 100.000) + Controles Lineales",
  "8. asinh(Y / mediana_k) + Controles Lineales",
  "9. Yeo-Johnson(Y / 100.000) reestimado por pliegue",
  "10. asinh(Y / 100.000) + Controles log/log1p(X)"
)

tabla_cv_4c <- as.data.frame(t(sapply(lista_modelos_4c, evaluar_cv_5pliegues)))
tabla_cv_4c$Especificacion <- rownames(tabla_cv_4c)
rownames(tabla_cv_4c) <- NULL
tabla_cv_4c <- tabla_cv_4c[, c("Especificacion", "MAE_CLP", "RMSE_CLP", "R2_CLP")]
tabla_cv_4c$MAE_CLP  <- round(tabla_cv_4c$MAE_CLP, 1)
tabla_cv_4c$RMSE_CLP <- round(tabla_cv_4c$RMSE_CLP, 1)
tabla_cv_4c$R2_CLP   <- round(tabla_cv_4c$R2_CLP, 4)

print(tabla_cv_4c)


# ==============================================================================
# PREGUNTA 5: Formas funcionales e interacciones (12 PTOS)
# ==============================================================================

# --- 5a) Examine log(1 + c6_3) y log(1 + total_trabajadores) (4 ptos) ---
cat("\n==================================================================\n")
cat("5a) TRANSFORMACIONES LOG(1 + X) EN CONTROLES CONTINUOS\n")
cat("==================================================================\n")

cat("Justificación de las transformaciones:\n")
cat(" - c6_3 (horas semanales): Rango [", min(train_ols$c6_3), ",", max(train_ols$c6_3),
    "]. Presenta asimetría a la derecha; log(1 + c6_3) suaviza valores extremos.\n")
cat(" - total_trabajadores: Rango [", min(train_ols$total_trabajadores), ",", max(train_ols$total_trabajadores),
    "]. N° ceros =", sum(train_ols$total_trabajadores == 0),
    paste0("(", round(mean(train_ols$total_trabajadores == 0) * 100, 1), "% de la muestra)\n"))
cat("   Como más del 70% de los microemprendimientos no tiene trabajadores contratados,\n",
    "  log(0) se indefine (-Inf). La transformación log(1 + total_trabajadores) = log1p\n",
    "  permite conservar las observaciones con 0 y capturar rendimientos decrecientes.\n\n")

# Ajuste del modelo con controles transformados
mod_5a <- lm(
  ganancia_final ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f +
    log1p(c6_3) + c7 + log1p(total_trabajadores) +
    informalidad_f + c1_b_f + sexo_f + region_f,
  data = train_ols
)

cat("Resumen de coeficientes transformados:\n")
print(round(summary(mod_5a)$coefficients[c("log1p(c6_3)", "log1p(total_trabajadores)"), ], 3))

cat("\nComparación con el modelo lineal base (Pregunta 3):\n")
cat(" - R2 ajustado base:", round(summary(mod_ols_completo)$adj.r.squared, 4),
    "| R2 ajustado con log(1+X):", round(summary(mod_5a)$adj.r.squared, 4), "\n")
cat(" - AIC base:", round(AIC(mod_ols_completo), 1),
    "| AIC con log(1+X):", round(AIC(mod_5a), 1), "\n")

b_horas <- coef(mod_5a)["log1p(c6_3)"]
b_trab  <- coef(mod_5a)["log1p(total_trabajadores)"]
cat("\nInterpretación estadística de los coeficientes:\n",
    " - Horas semanales (c6_3): Un incremento del 10% en las horas trabajadas se asocia\n",
    "   a un cambio promedio de beta * log(1.10) =", round(b_horas * log(1.10), 0), "CLP mensuales.\n",
    " - Trabajadores: Contratar al primer trabajador (pasar de 0 a 1) se asocia a un\n",
    "   incremento de beta * log(2) =", round(b_trab * log(2), 0), "CLP mensuales en la ganancia.\n")


# --- 5b) Evalúe no linealidad en c7 con término cuadrático justificado y jerárquico (4 ptos) ---
cat("\n==================================================================\n")
cat("5b) EVALUACIÓN DE NO LINEALIDAD EN c7 (MESES TRABAJADOS)\n")
cat("==================================================================\n")

cat("Justificación teórica y principio de jerarquía:\n",
    "c7 mide los meses trabajados al año (1 a 12). Los negocios permanentes (12 meses)\n",
    "tienen estabilidad comercial distinta a los estacionales (pocos meses). Para modelar\n",
    "posibles rendimientos crecientes o decrecientes se incorpora c7^2.\n",
    "Siguiendo el principio de jerarquía, el término lineal (c7) debe mantenerse\n",
    "obligatoriamente en el modelo junto al cuadrático I(c7^2).\n\n")

mod_5b <- lm(
  ganancia_final ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f +
    log1p(c6_3) + c7 + I(c7^2) + log1p(total_trabajadores) +
    informalidad_f + c1_b_f + sexo_f + region_f,
  data = train_ols
)

cat("Coeficientes de c7 y c7^2:\n")
print(round(summary(mod_5b)$coefficients[c("c7", "I(c7^2)"), ], 3))

cat("\nTest F parcial (ANOVA: mod_5a sin cuadrático vs mod_5b con cuadrático):\n")
anova_5b <- anova(mod_5a, mod_5b)
print(anova_5b)

cat("\nConclusión estadística sobre no linealidad en c7:\n")
pval_c7_cuad <- anova_5b$`Pr(>F)`[2]
if (pval_c7_cuad < 0.05) {
  cat("El término cuadrático I(c7^2) resulta estadísticamente significativo (p =",
      round(pval_c7_cuad, 4), "), lo que respalda la presencia de no linealidad en los meses trabajados.\n")
} else {
  cat("El término cuadrático I(c7^2) no resulta estadísticamente significativo (p =",
      round(pval_c7_cuad, 4), "), por lo que la relación lineal simple resulta suficiente.\n")
}


# --- 5c) Explore interacción alguna_amenaza × informalidad y compruebe estabilidad (4 ptos) ---
cat("\n==================================================================\n")
cat("5c) INTERACCIÓN alguna_amenaza × informalidad (ASOCIACIÓN HETEROGÉNEA Y ESTABILIDAD)\n")
cat("==================================================================\n")

cat("Justificación de asociación heterogénea:\n",
    "Permite evaluar si el impacto de las amenazas naturales sobre la ganancia es dispar\n",
    "entre microemprendimientos formales e informales (por ejemplo, si la informalidad agrava\n",
    "la caída en ganancias por falta de seguros o menor acceso a financiamiento).\n\n")

# Asegurar variable alguna_amenaza_f en train_ols si la sesión venía de antes
if (!("alguna_amenaza_f" %in% names(train_ols))) {
  train_ols$alguna_amenaza_f <- factor(train$alguna_amenaza[match(train_ols$id, train$id)],
                                       levels = c(2, 1), labels = c("No", "Si"))
}

# Modelo 1: Interacción simple sin controles
mod_int_simple <- lm(
  ganancia_final ~ alguna_amenaza_f * informalidad_f,
  data = train_ols
)

# Modelo 2: Interacción con controles completos (respetando jerarquía)
mod_int_controlada <- lm(
  ganancia_final ~ alguna_amenaza_f * informalidad_f +
    log1p(c6_3) + c7 + I(c7^2) + log1p(total_trabajadores) +
    c1_b_f + sexo_f + region_f,
  data = train_ols
)

cat("1. Coeficientes en modelo simple (sin controles):\n")
print(round(summary(mod_int_simple)$coefficients, 2))

cat("\n2. Coeficientes en modelo ajustado (con controles):\n")
filas_int <- grep("alguna_amenaza_f|informalidad_f", names(coef(mod_int_controlada)), value = TRUE)
print(round(summary(mod_int_controlada)$coefficients[filas_int, ], 2))

# 3. Comprobación de estabilidad a través de los 5 pliegues predeterminados
cat("\n3. Comprobación de estabilidad a través de los 5 pliegues de validación cruzada:\n")
coefs_cv <- numeric(5)
pvals_cv <- numeric(5)

for (k in 1:5) {
  datos_tr_k <- subset(train_ols, fold != k)
  ajuste_k <- lm(
    ganancia_final ~ alguna_amenaza_f * informalidad_f +
      log1p(c6_3) + c7 + I(c7^2) + log1p(total_trabajadores) +
      c1_b_f + sexo_f + region_f,
    data = datos_tr_k
  )
  tabla_k <- summary(ajuste_k)$coefficients
  nom_int <- "alguna_amenaza_fSi:informalidad_fInformal"
  if (nom_int %in% rownames(tabla_k)) {
    coefs_cv[k] <- tabla_k[nom_int, "Estimate"]
    pvals_cv[k] <- tabla_k[nom_int, "Pr(>|t|)"]
  }
}

tabla_estabilidad <- data.frame(
  Pliegue = paste("Fold", 1:5),
  Beta_Interaccion = round(coefs_cv, 2),
  P_Valor = round(pvals_cv, 4)
)
print(tabla_estabilidad)

cat("\nConclusión sobre la estabilidad:\n",
    "Se evalúa si el coeficiente de la interacción mantiene su signo, magnitud y nivel de\n",
    "significancia al rotar las submuestras en los 5 pliegues. La concordancia entre pliegues\n",
    "evidencia si la asociación heterogénea es un efecto estable o sensible a la muestra.\n")


# ==============================================================================
# PREGUNTA 6: Influencia y estabilidad (10 PTOS)
# ==============================================================================

# --- 6a) Calcule residuos estudentizados, leverage y Cook para el modelo original (4 ptos) ---
cat("\n==================================================================\n")
cat("6a) DIAGNÓSTICO DE INFLUENCIA: RESIDUOS ESTUDENTIZADOS, LEVERAGE Y DISTANCIA DE COOK\n")
cat("==================================================================\n")

# Cálculo de métricas sobre el modelo original en niveles (mod_ols_completo)
r_stud   <- rstudent(mod_ols_completo)
leverage <- hatvalues(mod_ols_completo)
d_cook   <- cooks.distance(mod_ols_completo)

n_obs <- nrow(train_ols)
p_par <- length(coef(mod_ols_completo)) # número de parámetros beta (incluye intercepto)

# Umbrales teóricos habituales en modelos lineales:
umbral_lev_2x <- 2 * p_par / n_obs     # Criterio estándar 2*(p/n)
umbral_lev_3x <- 3 * p_par / n_obs     # Criterio conservador 3*(p/n)
umbral_cook   <- 4 / n_obs             # Regla empírica 4/n para muestras grandes

cat("Parámetros del modelo: n =", n_obs, "| p (parámetros) =", p_par, "\n")
cat("Umbrales de referencia teórica:\n")
cat(" - Leverage 2*(p/n) =", round(umbral_lev_2x, 4), "| 3*(p/n) =", round(umbral_lev_3x, 4), "\n")
cat(" - Distancia de Cook (4/n) =", round(umbral_cook, 4), "\n\n")

# Resumen numérico descriptivo de las métricas
tabla_resumen_6a <- data.frame(
  Metrica = c("Residuos Estudentizados", "Leverage (hatvalues)", "Distancia de Cook"),
  Minimo  = c(min(r_stud), min(leverage), min(d_cook)),
  Mediana = c(median(r_stud), median(leverage), median(d_cook)),
  Media   = c(mean(r_stud), mean(leverage), mean(d_cook)),
  Maximo  = c(max(r_stud), max(leverage), max(d_cook)),
  Casos_Sobre_Umbral = c(sum(abs(r_stud) > 3), sum(leverage > umbral_lev_2x), sum(d_cook > umbral_cook))
)
tabla_resumen_6a[, 2:5] <- round(tabla_resumen_6a[, 2:5], 5)
print(tabla_resumen_6a)

# Identificar las 5 observaciones con mayor Distancia de Cook para el gráfico
top5_indices <- order(d_cook, decreasing = TRUE)[1:5]

# Gráficos diagnósticos de influencia (Con súper zoom en Cook)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

# Gráfico 1: Distancia de Cook como gráfico de dispersión (Súper zoom a 0.005)
plot(d_cook, type = "p", pch = 19, cex = 0.6, col = rgb(0.2, 0.4, 0.6, 0.5),
     main = "Distancia de Cook por Observación",
     xlab = "Índice de Observación", ylab = "Distancia de Cook",
     ylim = c(0, 0.005)) # <- AQUÍ ESTÁ EL SÚPER ZOOM (cortamos el eje Y en 0.005)
abline(h = umbral_cook, col = "red", lty = 2, lwd = 1.5)
legend("topright", legend = c("Distancia de Cook", "Umbral 4/n"),
       col = c(rgb(0.2, 0.4, 0.6, 0.5), "red"), lty = c(NA, 2), pch = c(19, NA), 
       bty = "n", cex = 0.8)

# Gráfico 2: Leverage vs Residuos Estudentizados (Original)
plot(leverage, r_stud, pch = 19, cex = 0.6, col = rgb(0.2, 0.4, 0.6, 0.4),
     main = "Residuos Estudentizados vs Leverage",
     xlab = "Leverage (h_ii)", ylab = "Residuo Estudentizado (r_i)")
abline(h = c(-3, 0, 3), col = "gray50", lty = c(2, 1, 2))
abline(v = umbral_lev_2x, col = "red", lty = 2)

par(mfrow = c(1, 1))
# --- 6b) Describa cinco observaciones de alta influencia y documente sus id (3 ptos) ---
cat("\n==================================================================\n")
cat("6b) TOP 5 OBSERVACIONES DE MAYOR INFLUENCIA Y DOCUMENTACIÓN DE SUS ID\n")
cat("==================================================================\n")

# Identificar las 5 observaciones con mayor Distancia de Cook
top5_indices <- order(d_cook, decreasing = TRUE)[1:5]

top5_df <- data.frame(
  Ranking       = 1:5,
  id            = train_ols$id[top5_indices],
  Ganancia_CLP  = train_ols$ganancia_final[top5_indices],
  Dist_Cook     = round(d_cook[top5_indices], 4),
  Res_Student   = round(r_stud[top5_indices], 2),
  Leverage      = round(leverage[top5_indices], 4),
  Horas_c6_3    = train_ols$c6_3[top5_indices],
  Meses_c7      = train_ols$c7[top5_indices],
  Trabajadores  = train_ols$total_trabajadores[top5_indices],
  Sector        = train_ols$c1_b_f[top5_indices],
  Formalidad    = train_ols$informalidad_f[top5_indices],
  Amenaza_Gral  = train_ols$alguna_amenaza_f[top5_indices]
)
print(top5_df)

cat("\nDescripción estadística de las 5 observaciones más influyentes:\n")
for (i in 1:nrow(top5_df)) {
  cat(paste0(" - Caso ", i, " (id = ", top5_df$id[i], "):\n"),
      "  Presenta una ganancia de $", format(top5_df$Ganancia_CLP[i], big.mark = "."),
      " CLP, con Distancia de Cook =", top5_df$Dist_Cook[i],
      " (residuo estudentizado =", top5_df$Res_Student[i], ").\n",
      "   Sector:", as.character(top5_df$Sector[i]), "| Formalidad:", as.character(top5_df$Formalidad[i]),
      "| Trabajadores:", top5_df$Trabajadores[i], "| Horas/sem:", top5_df$Horas_c6_3[i], "\n")
}
cat("-> Síntesis: La alta influencia en estas observaciones proviene principalmente de sus ganancias\n",
    "   atípicas extremas (ingresos multimillonarios en comparación con la mediana de $400.000),\n",
    "   lo que genera residuos muy grandes que 'tiran' fuertemente del hiperplano de regresión.\n")


# --- 6c) Compare sensibilidad de coeficientes ambientales con exclusión temporal (3 ptos) ---
cat("\n==================================================================\n")
cat("6c) ANÁLISIS DE SENSIBILIDAD: COEFICIENTES AMBIENTALES CON Y SIN CASOS INFLUYENTES\n")
cat("==================================================================\n")

cat("Criterio de exclusión temporal:\n",
    "Siguiendo la Instrucción General 5 ('No elimine datos atípicos automáticamente'), las observaciones\n",
    "no se eliminan definitivamente del estudio. Se realiza una exclusión TEMPORAL de los 5 casos de mayor\n",
    "influencia identificados en 6b (cuyo Cook supera con creces el umbral 4/n) exclusivamente para evaluar\n",
    "la estabilidad y sensibilidad de las estimaciones ante observaciones extremas.\n\n")

# Reajuste del modelo OLS original excluyendo temporalmente el top 5 de Cook
mod_ols_sin_top5 <- lm(
  ganancia_final ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f +
    c6_3 + c7 + total_trabajadores + informalidad_f + c1_b_f + sexo_f + region_f,
  data = train_ols[-top5_indices, ]
)

# Extracción de coeficientes de los 4 indicadores ambientales
vars_amb <- c("m2_1_fSi", "m2_2_fSi", "m2_3_fSi", "m2_4_fSi")

coef_con <- summary(mod_ols_completo)$coefficients[vars_amb, ]
coef_sin <- summary(mod_ols_sin_top5)$coefficients[vars_amb, ]

cambio_pct <- ((coef_sin[, "Estimate"] - coef_con[, "Estimate"]) / abs(coef_con[, "Estimate"])) * 100

tabla_sensibilidad_6c <- data.frame(
  Amenaza          = c("Sequías/Heladas (m2_1)", "Inundaciones (m2_2)", "Incendios (m2_3)", "Tormentas/Lluvias (m2_4)"),
  Beta_Original    = round(coef_con[, "Estimate"], 1),
  EE_Original      = round(coef_con[, "Std. Error"], 1),
  PVal_Original    = round(coef_con[, "Pr(>|t|)"], 4),
  Beta_Sin_Top5    = round(coef_sin[, "Estimate"], 1),
  EE_Sin_Top5      = round(coef_sin[, "Std. Error"], 1),
  PVal_Sin_Top5    = round(coef_sin[, "Pr(>|t|)"], 4),
  Cambio_Pct_Beta  = paste0(round(cambio_pct, 1), "%")
)
rownames(tabla_sensibilidad_6c) <- NULL
print(tabla_sensibilidad_6c)

cat("\nInterpretación estadística de la sensibilidad:\n",
    "1. Variación de magnitud: Al excluir temporalmente las 5 observaciones influyentes, los errores\n",
    "   estándar se reducen considerablemente debido a la menor dispersión residual en la escala en pesos.\n",
    "2. Estabilidad de signo y significancia: Se examina si alguna amenaza cambia de signo o pasa de no\n",
    "   significativa a significativa. Esto pone en evidencia la fragilidad de los coeficientes OLS en\n",
    "   la escala original en pesos frente a pocas observaciones con ingresos extremos, justificando\n",
    "   nuevamente la necesidad de modelos robustos o transformaciones evaluadas en las otras preguntas.\n")


# ==============================================================================
# PREGUNTA 7: Selección clásica (12 PTOS)
# ==============================================================================

# --- 7a) Ejecute Forward, Backward y Stepwise; conserve modelo con todas las amenazas (4 ptos) ---
cat("\n==================================================================\n")
cat("7a) SELECCIÓN CLÁSICA: FORWARD, BACKWARD Y STEPWISE BIDIRECCIONAL\n")
cat("==================================================================\n")

# Modelo nulo (solo intercepto) y modelo completo de referencia
mod_nulo <- lm(ganancia_final ~ 1, data = train_ols)
mod_full <- mod_ols_completo # contiene las 4 amenazas + todos los controles

# 1. Forward Selection (desde el modelo nulo hasta el modelo completo por AIC)
mod_forward <- step(
  mod_nulo,
  scope = list(lower = mod_nulo, upper = mod_full),
  direction = "forward",
  trace = 0
)

# 2. Backward Elimination (desde el modelo completo eliminando por AIC)
mod_backward <- step(
  mod_full,
  direction = "backward",
  trace = 0
)

# 3. Stepwise Bidireccional (ambas direcciones por AIC)
mod_stepwise <- step(
  mod_nulo,
  scope = list(lower = mod_nulo, upper = mod_full),
  direction = "both",
  trace = 0
)

# 4. Modelo con TODAS las amenazas conservado explícitamente (solicitado en la pauta)
mod_todas_amenazas <- mod_ols_completo

cat("Variables seleccionadas en cada procedimiento:\n")
cat(" - Forward :", paste(names(coef(mod_forward))[-1], collapse = ", "), "\n")
cat(" - Backward:", paste(names(coef(mod_backward))[-1], collapse = ", "), "\n")
cat(" - Stepwise:", paste(names(coef(mod_stepwise))[-1], collapse = ", "), "\n")
cat(" - Modelo con todas las amenazas conservado como referencia.\n\n")


# --- 7b) Compare AIC, BIC y Cp en la escala original de Y y misma muestra (4 ptos) ---
cat("\n==================================================================\n")
cat("7b) COMPARACIÓN DE MODELOS: AIC, BIC Y Cp DE MALLOWS (MISMA MUESTRA Y ESCALA Y)\n")
cat("==================================================================\n")

# Estimador insesgado de la varianza del error en el modelo completo (sigma^2_full)
n_m     <- nrow(train_ols)
s2_full <- sum(residuals(mod_full)^2) / df.residual(mod_full)

# Función para calcular Cp de Mallows: Cp = (RSS_m / s2_full) - n + 2 * p_m
calcular_cp <- function(modelo, s2_ref, n) {
  rss_m <- sum(residuals(modelo)^2)
  p_m   <- length(coef(modelo)) # incluye intercepto
  cp    <- (rss_m / s2_ref) - n + 2 * p_m
  return(cp)
}

lista_modelos_7 <- list(
  "Forward"            = mod_forward,
  "Backward"           = mod_backward,
  "Stepwise (Both)"    = mod_stepwise,
  "Todas las Amenazas" = mod_todas_amenazas
)

tabla_comp_7b <- data.frame(
  Modelo     = names(lista_modelos_7),
  Parametros = sapply(lista_modelos_7, function(m) length(coef(m))),
  R2         = round(sapply(lista_modelos_7, function(m) summary(m)$r.squared), 4),
  R2_Ajust   = round(sapply(lista_modelos_7, function(m) summary(m)$adj.r.squared), 4),
  AIC        = round(sapply(lista_modelos_7, AIC), 1),
  BIC        = round(sapply(lista_modelos_7, BIC), 1),
  Cp_Mallows = round(sapply(lista_modelos_7, function(m) calcular_cp(m, s2_full, n_m)), 2)
)
rownames(tabla_comp_7b) <- NULL
print(tabla_comp_7b)

cat("\nInterpretación de criterios:\n",
    " - Criterio AIC: Penaliza la complejidad con 2*p; busca modelos con buen ajuste predictivo.\n",
    " - Criterio BIC: Penaliza más severamente con log(n)*p; tiende a seleccionar modelos más parsimoniosos.\n",
    " - Cp de Mallows: Compara el error de predicción frente al modelo completo. Un modelo adecuado\n",
    "   presenta Cp cercano a su número de parámetros (Cp ≈ p).\n")


# --- 7c) Discuta qué amenaza(s) se retienen y por qué selección automática no es causal (4 ptos) ---
cat("\n==================================================================\n")
cat("7c) AMENAZAS RETENIDAS Y LÍMITES CAUSALES DE LA SELECCIÓN AUTOMÁTICA\n")
cat("==================================================================\n")

# Verificar qué amenazas quedaron en cada modelo seleccionado
amenazas_nombres <- c("m2_1_f", "m2_2_f", "m2_3_f", "m2_4_f")

cat("Presencia de amenazas en los modelos seleccionados:\n")
for (nom_mod in c("Forward", "Backward", "Stepwise (Both)")) {
  m <- lista_modelos_7[[nom_mod]]
  vars_en_m <- names(coef(m))
  ret <- amenazas_nombres[sapply(amenazas_nombres, function(a) any(grepl(a, vars_en_m)))]
  if (length(ret) == 0) {
    cat(" -", nom_mod, ": Ninguna amenaza natural fue retenida por el algoritmo.\n")
  } else {
    cat(" -", nom_mod, ": Retuvo", paste(ret, collapse = ", "), "\n")
  }
}

cat("\nDiscusión metodológica: Por qué selección automática NO implica efecto causal:\n",
    "1. Criterio predictivo vs causal: Los algoritmos stepwise seleccionan variables basándose\n",
    "   únicamente en la reducción de suma de cuadrados residuales (RSS) o penalizaciones (AIC/BIC)\n",
    "   muestrales, sin ningún conocimiento de la estructura teórica del fenómeno ni de confusores.\n",
    "2. Confusión y colinealidad: Si una amenaza (ej: sequías m2_1) está correlacionada con una\n",
    "   variable de control como el sector agropecuario (c1_b) o la región geográfica, el algoritmo\n",
    "   puede descartar la amenaza porque el sector ya 'absorbe' esa variabilidad, ocultando una\n",
    "   asociación real o reteniendo variables espurias.\n",
    "3. Sesgo post-selección: Los p-valores de un modelo tras selección paso a paso están sesgados\n",
    "   a la baja (falsamente optimistas), ya que se probaron múltiples combinaciones sobre los\n",
    "   mismos datos.\n",
    "4. Diseño observacional y temporalidad: La selección automática no resuelve la falta de aleatorización\n",
    "   ni el desbalance temporal entre la ocurrencia de los eventos climáticos y la ganancia del último mes.\n")


# ==============================================================================
# PREGUNTA 9: Regularización (10 PTOS)
# ==============================================================================

# --- 9a) Estime Ridge, LASSO y Elastic Net con codificación y estandarización adecuadas (4 ptos) ---
cat("\n==================================================================\n")
cat("9a) REGULARIZACIÓN: RIDGE, LASSO Y ELASTIC NET (CODIFICACIÓN Y ESTANDARIZACIÓN)\n")
cat("==================================================================\n")

# Verificar si glmnet está disponible
if (!requireNamespace("glmnet", quietly = TRUE)) {
  stop("El paquete 'glmnet' es necesario para la Pregunta 9. Instálelo con: install.packages('glmnet')")
}
library(glmnet)

# Codificación adecuada: model.matrix genera variables dummy para factores y maneja contrastes
# Se omite la columna del intercepto [, -1] porque glmnet lo incluye internamente
formula_x <- ~ m2_1_f + m2_2_f + m2_3_f + m2_4_f +
  c6_3 + c7 + total_trabajadores + informalidad_f + c1_b_f + sexo_f + region_f

X_mat <- model.matrix(formula_x, data = train_ols)[, -1]
y_vec <- train_ols$ganancia_final

cat("Matriz de diseño construida adecuadamente:\n")
cat(" - N° de observaciones (n):", nrow(X_mat), "\n")
cat(" - N° de predictores codificados (p):", ncol(X_mat), "\n")
cat("-> Estandarización: Se utiliza standardize = TRUE en glmnet, ya que Ridge, LASSO y\n",
    "   Elastic Net penalizan la suma de coeficientes beta y requieren que todas las variables\n",
    "   (horas, meses, dummies) estén en una escala comparable para no sesgar la penalización.\n\n")


# --- 9b) Ajuste penalizaciones mediante CV reproducible con pliegues predeterminados (3 ptos) ---
cat("\n==================================================================\n")
cat("9b) AJUSTE DE PENALIZACIONES (LAMBDA) MEDIANTE VALIDACIÓN CRUZADA REPRODUCIBLE\n")
cat("==================================================================\n")

# Semilla para reproducibilidad y asignación estricta de pliegues predeterminados
set.seed(2026)
pliegues_id <- train_ols$fold # Los 5 pliegues predeterminados del caso

# 1. Ridge Regression (alpha = 0: penalización L2)
cv_ridge <- cv.glmnet(X_mat, y_vec, alpha = 0, foldid = pliegues_id, standardize = TRUE)

# 2. LASSO Regression (alpha = 1: penalización L1 y selección de variables)
cv_lasso <- cv.glmnet(X_mat, y_vec, alpha = 1, foldid = pliegues_id, standardize = TRUE)

# 3. Elastic Net (alpha = 0.5: balance entre L1 y L2)
cv_enet  <- cv.glmnet(X_mat, y_vec, alpha = 0.5, foldid = pliegues_id, standardize = TRUE)

# Tabla resumen de lambdas óptimos y errores de validación cruzada
tabla_regularizacion_9b <- data.frame(
  Metodo      = c("Ridge (alpha = 0)", "LASSO (alpha = 1)", "Elastic Net (alpha = 0.5)"),
  Lambda_Min  = c(cv_ridge$lambda.min, cv_lasso$lambda.min, cv_enet$lambda.min),
  Lambda_1SE  = c(cv_ridge$lambda.1se, cv_lasso$lambda.1se, cv_enet$lambda.1se),
  RMSE_CV_Min = c(sqrt(min(cv_ridge$cvm)), sqrt(min(cv_lasso$cvm)), sqrt(min(cv_enet$cvm)))
)
tabla_regularizacion_9b$Lambda_Min  <- round(tabla_regularizacion_9b$Lambda_Min, 2)
tabla_regularizacion_9b$Lambda_1SE  <- round(tabla_regularizacion_9b$Lambda_1SE, 2)
tabla_regularizacion_9b$RMSE_CV_Min <- round(tabla_regularizacion_9b$RMSE_CV_Min, 0)
print(tabla_regularizacion_9b)

# Gráficos de validación cruzada para las tres penalizaciones
par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3, 1))
plot(cv_ridge, main = "Ridge (alpha = 0)")
plot(cv_lasso, main = "LASSO (alpha = 1)")
plot(cv_enet,  main = "Elastic Net (alpha = 0.5)")
par(mfrow = c(1, 1))


# --- 9c) Discuta estabilidad de indicadores ambientales y posible utilidad predictiva (3 ptos) ---
cat("\n==================================================================\n")
cat("9c) ESTABILIDAD DE INDICADORES AMBIENTALES Y UTILIDAD PREDICTIVA\n")
cat("==================================================================\n")

# Extracción de coeficientes de las 4 amenazas en lambda.min y lambda.1se
nombres_amenazas_cod <- c("m2_1_fSi", "m2_2_fSi", "m2_3_fSi", "m2_4_fSi")

coefs_ridge_min <- as.matrix(coef(cv_ridge, s = "lambda.min"))[nombres_amenazas_cod, ]
coefs_lasso_min <- as.matrix(coef(cv_lasso, s = "lambda.min"))[nombres_amenazas_cod, ]
coefs_enet_min  <- as.matrix(coef(cv_enet,  s = "lambda.min"))[nombres_amenazas_cod, ]

coefs_lasso_1se <- as.matrix(coef(cv_lasso, s = "lambda.1se"))[nombres_amenazas_cod, ]
coefs_enet_1se  <- as.matrix(coef(cv_enet,  s = "lambda.1se"))[nombres_amenazas_cod, ]

tabla_coefs_penalizados <- data.frame(
  Amenaza          = c("Sequías/Heladas (m2_1)", "Inundaciones (m2_2)", "Incendios (m2_3)", "Tormentas/Lluvias (m2_4)"),
  OLS_Original     = round(coef_con[nombres_amenazas_cod, "Estimate"], 1),
  Ridge_Min        = round(coefs_ridge_min, 1),
  LASSO_Min        = round(coefs_lasso_min, 1),
  ElasticNet_Min   = round(coefs_enet_min, 1),
  LASSO_1SE        = round(coefs_lasso_1se, 1),
  ElasticNet_1SE   = round(coefs_enet_1se, 1)
)
rownames(tabla_coefs_penalizados) <- NULL
print(tabla_coefs_penalizados)

cat("\nDiscusión sobre estabilidad y utilidad predictiva:\n",
    "1. Encogimiento hacia cero (Shrinkage): En Ridge, los coeficientes de las amenazas sufren\n",
    "   una fuerte reducción hacia cero, demostrando que su señal frente al ruido es muy débil.\n",
    "2. Selección de variables en LASSO y Elastic Net: Bajo el criterio más parsimonioso (lambda.1se),\n",
    "   LASSO y Elastic Net anulan exactamente los coeficientes de las amenazas climáticas (los llevan a 0),\n",
    "   conservando únicamente predictores de productividad laboral y sector económico.\n",
    "3. Utilidad predictiva: Las amenazas naturales autodeclaradas no aportan poder predictivo\n",
    "   estable para la ganancia mensual del microemprendimiento; su inclusión responde a un interés\n",
    "   analítico/descriptivo del problema ambiental, más que a una ventaja en predicción empírica.\n")


# ==============================================================================
# PREGUNTA 10: Interpretación y cierre (3 PTOS)
# ==============================================================================

# --- 10a) Presente tabla comparativa final con errores y modelos candidatos (1 pto) ---
cat("\n==================================================================\n")
cat("10a) TABLA COMPARATIVA FINAL DE MODELOS CANDIDATOS (ERRORES EN CLP)\n")
cat("==================================================================\n")

# Predicciones de los modelos candidatos en escala original de pesos chilenos (CLP)
pred_cand_controles <- predict(mod_solo_controles)
pred_cand_completo  <- predict(mod_ols_completo)
pred_cand_asinh     <- c_100k * sinh(predict(mod_asinh_100k))
pred_cand_stepwise  <- predict(mod_stepwise)
pred_cand_lasso     <- as.numeric(predict(cv_lasso, newx = X_mat, s = "lambda.min"))
pred_cand_ridge     <- as.numeric(predict(cv_ridge, newx = X_mat, s = "lambda.min"))

calc_metricas_final <- function(y_real, y_pred) {
  mae  <- mean(abs(y_real - y_pred), na.rm = TRUE)
  rmse <- sqrt(mean((y_real - y_pred)^2, na.rm = TRUE))
  c(MAE_CLP = round(mae, 0), RMSE_CLP = round(rmse, 0))
}

tabla_final_10a <- rbind(
  "1. OLS Solo Controles"              = calc_metricas_final(train_ols$ganancia_final, pred_cand_controles),
  "2. OLS Completo (con 4 amenazas)"   = calc_metricas_final(train_ols$ganancia_final, pred_cand_completo),
  "3. asinh(Y / 100k) Retransformado"  = calc_metricas_final(train_ols$ganancia_final, pred_cand_asinh),
  "4. Stepwise Clásico"                = calc_metricas_final(train_ols$ganancia_final, pred_cand_stepwise),
  "5. LASSO (lambda.min)"              = calc_metricas_final(train_ols$ganancia_final, pred_cand_lasso),
  "6. Ridge (lambda.min)"              = calc_metricas_final(train_ols$ganancia_final, pred_cand_ridge)
)

# Convertir a data.frame informativo
df_final_10a <- as.data.frame(tabla_final_10a)
df_final_10a$Modelo <- rownames(df_final_10a)
rownames(df_final_10a) <- NULL

# Información complementaria de parsimonia y amenazas
df_final_10a$Parametros <- c(
  length(coef(mod_solo_controles)),
  length(coef(mod_ols_completo)),
  length(coef(mod_asinh_100k)),
  length(coef(mod_stepwise)),
  sum(coef(cv_lasso, s = "lambda.min") != 0),
  ncol(X_mat) + 1
)

df_final_10a$Incluye_Amenazas <- c(
  "No (Solo Controles)",
  "Sí (4 Amenazas)",
  "Sí (4 Amenazas)",
  ifelse(any(grepl("m2_", names(coef(mod_stepwise)))), "Sí (parcial)", "No (descartadas)"),
  ifelse(any(coefs_lasso_min != 0), "Sí (parcial)", "No (anuladas)"),
  "Sí (encogidas por L2)"
)

df_final_10a <- df_final_10a[, c("Modelo", "Parametros", "Incluye_Amenazas", "MAE_CLP", "RMSE_CLP")]
print(df_final_10a)

# --- 10b) Formule una conclusión de asociación/predicción prudente y contextualizada (1 pto) ---
cat("\n==================================================================\n")
cat("10b) CONCLUSIÓN DE ASOCIACIÓN Y PREDICCIÓN PRUDENTE Y CONTEXTUALIZADA\n")
cat("==================================================================\n")

cat("Conclusiones estadísticas y sustantivas del estudio:\n\n",
    "1. Conclusión de Asociación:\n",
    "   En el análisis descriptivo univariado (EDA), los microemprendimientos afectados por amenazas\n",
    "   climáticas muestran ingresos medianos ligeramente inferiores. Sin embargo, al incorporar controles\n",
    "   estructurales (sector económico c1_b, horas semanales c6_3, trabajadores y región), la magnitud\n",
    "   asociativa de las cuatro amenazas se debilita notablemente y no alcanza significancia estadística\n",
    "   robusta. Esto demuestra que la brecha observada en ingresos no se explica principalmente por el choque\n",
    "   ambiental directo, sino por las características socio-productivas de los negocios que sufren estas\n",
    "   amenazas (concentrados en el sector agropecuario y en zonas con menores escalas de ingreso).\n\n",
    "2. Conclusión de Predicción:\n",
    "   Tanto la validación cruzada en 5 pliegues (Pregunta 8) como la regularización LASSO y Elastic Net\n",
    "   (Pregunta 9) demuestran que agregar los indicadores ambientales no disminuye el error predictivo\n",
    "   fuera de muestra (MAE/RMSE en CLP). De hecho, bajo penalización parsimoniosa (lambda.1se), LASSO\n",
    "   anula exactamente a cero los coeficientes ambientales. Por tanto, para predecir las ganancias\n",
    "   mensuales de un microemprendimiento, las variables ambientales autodeclaradas carecen de utilidad\n",
    "   predictiva relevante frente al peso preponderante del sector y la intensidad laboral.\n\n")


# --- 10c) Señale limitaciones de temporalidad, autodeclaración y diseño observacional (1 pto) ---
cat("\n==================================================================\n")
cat("10c) LIMITACIONES METODOLÓGICAS: TEMPORALIDAD, AUTODECLARACIÓN Y DISEÑO OBSERVACIONAL\n")
cat("==================================================================\n")

cat("Tres limitaciones fundamentales del diseño del estudio:\n\n",
    "1. Limitación de Temporalidad (Desfase de Ventanas de Medición):\n",
    "   Existe un descalce temporal crítico: la variable respuesta 'ganancia_final' mide el desempeño económico\n",
    "   exclusivamente del último mes previo a la encuesta, mientras que las preguntas m2_1 a m2_4 capturan\n",
    "   eventos climáticos adversos sufridos en un horizonte temporal amplio de la historia del negocio.\n",
    "   Un emprendimiento pudo haberse recuperado económicamente de una helada ocurrida un año atrás, o bien\n",
    "   estar sufriendo un mes puntual de baja demanda por factores totalmente ajenos al clima histórico.\n\n",
    "2. Limitación de Autodeclaración y Sesgo de Percepción:\n",
    "   Las amenazas climáticas no provienen de registros biofísicos objetivos (como pluviometría o estaciones\n",
    "   meteorológicas del INIA), sino de la autodeclaración retrospectiva del microemprendedor informante.\n",
    "   Esto introduce un potencial sesgo de justificación: emprendedores con menores ganancias pueden tener\n",
    "   mayor propensión psicológica a reportar o recordar eventos climáticos adversos para explicar su desempeño.\n\n",
    "3. Limitación de Diseño Observacional (Imposibilidad de Inferencia Causal):\n",
    "   Al tratarse de una encuesta observacional de corte transversal y no de un experimento aleatorizado,\n",
    "   no se puede establecer una relación causa-efecto. Existen variables confusoras no observadas ni medidas\n",
    "   (como el capital acumulado, la calidad de la tierra o infraestructura, el acceso a seguros agrícolas\n",
    "   o habilidades de gestión) que determinan simultáneamente la exposición al riesgo ambiental y el nivel\n",
    "   de ganancias. Por ende, los coeficientes representan meras asociaciones condicionales.\n\n")

cat("==================================================================\n")
cat("FIN DE LA EVALUACIÓN: MODELOS LINEALES — FILA B (VIII EME 2025)\n")
cat("Victoria Céspedes & Antonia González\n")
cat("==================================================================\n")



 