# =============================================================================
# PROJEKT: Analiza i modelowanie cen mieszkań za m² w Polsce
# Źródło danych: GUS - Bank Danych Lokalnych
# Zmienna objaśniana: Cena transakcyjna 1m² mieszkania
# =============================================================================

# ── 0. PAKIETY ────────────────────────────────────────────────────────────────
required_packages <- c(
  "tidyverse",   # manipulacja danych i wizualizacja
  "corrplot",    # macierz korelacji
  "car",         # diagnostyka (vif, ncvTest)
  "lmtest",      # testy diagnostyczne (bptest, dwtest)
  "nortest",     # testy normalności (lillie.test)
  "MASS",        # stepAIC
  "Metrics",     # miary błędu (rmse, mae)
  "ggcorrplot",  # ggplot2-owa macierz korelacji
  "gridExtra",   # łączenie wykresów
  "moments"      # skewness, kurtosis
)

# Instalacja brakujących pakietów
new_pkgs <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
invisible(lapply(required_packages, library, character.only = TRUE))


# =============================================================================
# 1. WCZYTANIE DANYCH
# =============================================================================

# Dane wczytane przez użytkownika (format GUS BDL – read.csv2)
dane_ceny_1m2_mieszkan <- read.csv2("RYNE_3787_CTAB_20260321171501.csv")
dane_wynagrodzenia      <- read.csv2("WYNA_2497_CTAB_20260321172330.csv")
dane_bezrobocie         <- read.csv2("RYNE_2392_CTAB_20260321172526.csv")
dane_ludnosc            <- read.csv2("LUDN_2425_CTAB_20260321173151.csv")
dane_ludn_na_1km        <- read.csv2("LUDN_2425_CTAB_20260321173131.csv")
dane_wsk_urb            <- read.csv2("LUDN_2425_CTAB_20260321173206.csv")
dane_liczba_pozwolen    <- read.csv2("PRZE_4431_CTAB_20260321173635.csv")
dane_migracje           <- read.csv2("LUDN_1350_CTAB_20260321174505.csv")
dane_podm_gosp          <- read.csv2("PODM_3802_CTAB_20260321174825.csv")
dane_liczba_studentow   <- read.csv2("SZKO_3226_CTAB_20260321175636.csv")

# Podgląd struktury (uruchom ręcznie, żeby zobaczyć kolumny):
# str(dane_ceny_1m2_mieszkan)
# head(dane_ceny_1m2_mieszkan)


# =============================================================================
# 2. FUNKCJA POMOCNICZA – CZYSZCZENIE FORMATU GUS BDL
# =============================================================================
# Rzeczywisty format eksportu GUS BDL (CTAB):
#   Kolumna 1: "Kod"   – kod TERYT jednostki
#   Kolumna 2: "Nazwa" – nazwa jednostki
#   Kolumna 3: wartość – nazwa w stylu "ogółem.ogółem.2022..zł." (zależy od zmiennej)
#   Kolumna 4: "X"     – pusta / nieużywana
#
# Funkcja zawsze bierze 3. kolumnę jako wartość liczbową i zwraca
# ramkę z kolumnami: kod | nazwa | <nazwa_zmiennej>

extract_gus <- function(df, nazwa_zmiennej) {
  
  # Kolumny identyfikacyjne: zawsze 1. i 2.
  col_kod   <- names(df)[1]   # "Kod"
  col_nazwa <- names(df)[2]   # "Nazwa"
  col_wartosc <- names(df)[3] # np. "ogółem.ogółem.2022..zł."
  
  cat(sprintf("  [%s] kolumna wartości: %s\n", nazwa_zmiennej, col_wartosc))
  
  wynik <- df[, c(col_kod, col_nazwa, col_wartosc)]
  names(wynik) <- c("kod", "nazwa", nazwa_zmiennej)
  
  # Konwersja na liczbę: usunięcie spacji, zamiana przecinka na kropkę
  wynik[[nazwa_zmiennej]] <- suppressWarnings(
    as.numeric(gsub(",", ".", gsub("[[:space:]]", "", 
                                   as.character(wynik[[nazwa_zmiennej]]))))
  )
  
  # Usunięcie wiersza ogólnopolskiego (kod = "0", "00", "0000000" itp.)
  wynik <- wynik[!grepl("^0+$", trimws(as.character(wynik$kod))), ]
  
  # Usunięcie pustych wierszy (np. stopka GUS)
  wynik <- wynik[!is.na(wynik$kod) & nchar(trimws(as.character(wynik$kod))) > 0, ]
  
  return(wynik)
}


# =============================================================================
# 3. PRZYGOTOWANIE DANYCH
# =============================================================================
# Rok został wybrany podczas eksportu z GUS BDL – kazdy plik zawiera
# dokladnie jedna kolumne z wartoscia (3. kolumna), np. "ogoltem.ogoltem.2022..zl."
# Funkcja extract_gus() zawsze bierze te 3. kolumne.

cat("-- Wczytywanie i parsowanie danych GUS --\n")
ceny          <- extract_gus(dane_ceny_1m2_mieszkan, "cena_m2")
wynagrodzenia <- extract_gus(dane_wynagrodzenia,     "wynagrodzenie")
bezrobocie    <- extract_gus(dane_bezrobocie,        "bezrobocie")
ludnosc       <- extract_gus(dane_ludnosc,           "ludnosc")
gestosc       <- extract_gus(dane_ludn_na_1km,       "gestosc")
urbanizacja   <- extract_gus(dane_wsk_urb,           "urbanizacja")
pozwolenia    <- extract_gus(dane_liczba_pozwolen,   "pozwolenia_na_1000")
migracje      <- extract_gus(dane_migracje,          "saldo_migracji")
studenci      <- extract_gus(dane_liczba_studentow,  "studenci")


podmioty <- extract_gus(dane_podm_gosp, "podmioty_gosp")


# =============================================================================
# 4. SCALANIE DANYCH W JEDNĄ RAMKĘ
# =============================================================================

dane <- ceny %>%
  left_join(wynagrodzenia,  by = c("kod", "nazwa")) %>%
  left_join(bezrobocie,     by = c("kod", "nazwa")) %>%
  left_join(ludnosc,        by = c("kod", "nazwa")) %>%
  left_join(gestosc,        by = c("kod", "nazwa")) %>%
  left_join(urbanizacja,    by = c("kod", "nazwa")) %>%
  left_join(pozwolenia,     by = c("kod", "nazwa")) %>%
  left_join(migracje,       by = c("kod", "nazwa")) %>%
  left_join(podmioty,      by = c("kod", "nazwa")) %>%
  left_join(studenci,       by = c("kod", "nazwa"))

cat("Wymiary ramki po scaleniu:", dim(dane), "\n")
cat("Nazwy kolumn:", paste(names(dane), collapse = ", "), "\n")


# =============================================================================
# 5. CZYSZCZENIE DANYCH
# =============================================================================

# 5a. Sprawdzenie braków danych
cat("\n── Braki danych (NA) per kolumna ──\n")
print(colSums(is.na(dane)))

# 5b. Usunięcie wierszy z brakami w zmiennej objaśnianej
dane <- dane %>% filter(!is.na(cena_m2))

# 5c. Obsługa brakujących wartości w zmiennych objaśniających

# Braki w "studenci" oznaczają brak uczelni w powiecie -> uzupełniamy 0
n_brak_przed <- sum(is.na(dane$studenci))
dane$studenci[is.na(dane$studenci)] <- 0
cat(sprintf("\nImputacja 'studenci': uzupełniono %d braków wartością 0\n", n_brak_przed))

# Pozostałe zmienne - usunięcie wierszy z NA
dane_clean <- dane %>% drop_na()

# Usunięcie wierszy z cena_m2 <= 0 (log-transformacja wymaga wartości dodatnich)
n_niepoprawn <- sum(dane_clean$cena_m2 <= 0, na.rm = TRUE)
if (n_niepoprawn > 0) {
  cat(sprintf("\nUsunięto %d wierszy z cena_m2 <= 0 (niemożliwa log-transformacja):\n",
              n_niepoprawn))
  print(dane_clean[dane_clean$cena_m2 <= 0, c("nazwa", "cena_m2")])
  dane_clean <- dane_clean[dane_clean$cena_m2 > 0, ]
}

# 5d. Normalizacja zmiennych na 1000 mieszkańców
# Uzasadnienie: większy powiat ma więcej studentów/migrantów przez sam rozmiar,
# nie ze względu na strukturę – przeliczenie eliminuje efekt skali populacji.
dane_clean <- dane_clean %>%
  mutate(
    studenci_na_1000       = round(studenci       / (ludnosc), 2),
    saldo_migracji_na_1000 = round(saldo_migracji / (ludnosc), 2)
  ) %>%
  dplyr::select(-studenci, -saldo_migracji)  # zastępujemy oryginalne kolumny

cat("\nPrzeliczono 'studenci' i 'saldo_migracji' na 1000 mieszkańców.\n")


# 5d. Sprawdzenie wartości odstających (IQR)
wykryj_outliers <- function(x, nazwa) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  out <- sum(x < (Q1 - 3*IQR_val) | x > (Q3 + 3*IQR_val), na.rm = TRUE)
  if (out > 0) cat(sprintf("  %s: %d wartości odstających (3×IQR)\n", nazwa, out))
}

cat("\n── Wartości odstające (3×IQR) ──\n")
num_cols <- names(dane_clean)[sapply(dane_clean, is.numeric)]
invisible(lapply(num_cols, function(col) wykryj_outliers(dane_clean[[col]], col)))


# =============================================================================
# 6. ANALIZA ROZKŁADÓW ZMIENNYCH
# =============================================================================

# 6a. Statystyki opisowe
cat("\n── Statystyki opisowe ──\n")
statystyki <- dane_clean %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), list(
    n      = ~sum(!is.na(.)),
    srednia= ~mean(., na.rm = TRUE),
    mediana= ~median(., na.rm = TRUE),
    sd     = ~sd(., na.rm = TRUE),
    skos   = ~moments::skewness(., na.rm = TRUE),
    kurtoza= ~moments::kurtosis(., na.rm = TRUE),
    min    = ~min(., na.rm = TRUE),
    max    = ~max(., na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), names_to = c("zmienna", ".value"),
               names_pattern = "(.+)_(.+)$")
print(as.data.frame(statystyki), digits = 3)

# 6b. Histogramy zmiennych
par(mfrow = c(3, 4), mar = c(3, 3, 2, 1))
for (col in num_cols) {
  hist(dane_clean[[col]], main = col, xlab = "", col = "steelblue",
       border = "white", breaks = 15)
}
par(mfrow = c(1, 1))

# 6c. Wykresy pudełkowe zmiennych objaśniających (normalizacja Z dla porównywalności)
dane_scaled <- dane_clean %>%
  dplyr::select(all_of(num_cols)) %>%
  scale() %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "zmienna", values_to = "wartosc")

ggplot(dane_scaled, aes(x = reorder(zmienna, wartosc, FUN = median),
                         y = wartosc, fill = zmienna)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = 21, outlier.fill = "red") +
  coord_flip() +
  labs(title = "Wykresy pudełkowe (zmienne standaryzowane)",
       x = NULL, y = "Wartość standaryzowana") +
  theme_minimal(base_size = 11)


# =============================================================================
# 7. ANALIZA ZALEŻNOŚCI
# =============================================================================

# 7a. Macierz korelacji
macierz_cor <- cor(dane_clean %>% dplyr::select(all_of(num_cols)), use = "complete.obs")
cat("\n── Korelacje z ceną 1m² ──\n")
print(sort(macierz_cor[,"cena_m2"], decreasing = TRUE), digits = 3)

# 7b. Wizualizacja macierzy korelacji
ggcorrplot(macierz_cor,
           method   = "circle",
           type     = "lower",
           lab      = TRUE,
           lab_size = 2.5,
           colors   = c("#d73027","white","#1a9850"),
           title    = "Macierz korelacji zmiennych") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7c. Wykresy rozrzutu: zmienna objaśniana vs. predyktory
zmienne_objasniane <- setdiff(num_cols, "cena_m2")
p_list <- lapply(zmienne_objasniane, function(var) {
  ggplot(dane_clean, aes_string(x = var, y = "cena_m2")) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
    labs(title = paste("cena_m2 ~", var),
         x = var, y = "cena 1m² [PLN]") +
    theme_minimal(base_size = 9)
})
do.call(grid.arrange, c(p_list, ncol = 3))


# =============================================================================
# 8. PODZIAŁ NA ZBIÓR TRENINGOWY I TESTOWY
# =============================================================================

set.seed(42)  # reprodukowalność wyników

n       <- nrow(dane_clean)
indeksy_train <- sample(seq_len(n), size = floor(0.75 * n))  # 75% trening

zbior_train <- dane_clean[ indeksy_train, ]
zbior_test  <- dane_clean[-indeksy_train, ]

cat(sprintf("\nZbiór treningowy: %d obs. | Zbiór testowy: %d obs.\n",
            nrow(zbior_train), nrow(zbior_test)))

# Sprawdzenie czy rozkłady są zbliżone
cat("Średnia cena_m2 – trening:", mean(zbior_train$cena_m2), "\n")
cat("Średnia cena_m2 – test:   ", mean(zbior_test$cena_m2), "\n")


# =============================================================================
# 9. DOBÓR ZMIENNYCH I ESTYMACJA MODELU
# =============================================================================

# 9a. Model pełny (wszystkie zmienne)
model_pelny <- lm(cena_m2 ~ wynagrodzenie + bezrobocie + ludnosc +
                    gestosc + urbanizacja + pozwolenia_na_1000 +
                    saldo_migracji_na_1000 + podmioty_gosp + studenci_na_1000,
                  data = zbior_train)

cat("\n── Model pełny ──\n")
summary(model_pelny)

# 9b. Selekcja zmiennych metodą krokową (AIC)
model_krokowy <- stepAIC(model_pelny,
                          direction = "both",
                          trace     = FALSE)

cat("\n── Model po selekcji krokowej (AIC) ──\n")
summary(model_krokowy)
cat("Wybrane zmienne:", paste(names(coef(model_krokowy))[-1], collapse = ", "), "\n")

# 9c. Porównanie AIC/BIC
cat(sprintf("\nModel pełny:   AIC = %.2f | BIC = %.2f\n",
            AIC(model_pelny), BIC(model_pelny)))
cat(sprintf("Model krokowy: AIC = %.2f | BIC = %.2f\n",
            AIC(model_krokowy), BIC(model_krokowy)))


# =============================================================================
# 10. DIAGNOSTYKA MODELU
# =============================================================================

cat("\n══ DIAGNOSTYKA ══\n")

# 10a. Czynnik inflacji wariancji (VIF) – wielowspółliniowość
cat("\n── VIF (>5 = problem, >10 = poważny problem) ──\n")
print(vif(model_krokowy))

# 10b. Test Breuscha-Pagana – heteroskedastyczność
bp_test <- bptest(model_krokowy)
cat("\n── Test Breuscha-Pagana (H0: homoskedastyczność) ──\n")
print(bp_test)

# 10c. Test Shapiro-Wilka – normalność reszt
sw_test <- shapiro.test(residuals(model_krokowy))
cat("\n── Test Shapiro-Wilka (H0: normalność reszt) ──\n")
print(sw_test)

# 10d. Test Durbina-Watsona – autokorelacja reszt
dw_test <- dwtest(model_krokowy)
cat("\n── Test Durbina-Watsona (H0: brak autokorelacji) ──\n")
print(dw_test)

# 10e. Wykresy diagnostyczne
par(mfrow = c(2, 2))
plot(model_krokowy, which = 1:4)
par(mfrow = c(1, 1))

# 10f. Identyfikacja obserwacji wpływowych
influencePlot(model_krokowy, main = "Obserwacje wpływowe")

# Dźwignia i reszty Studenta
leverage   <- hatvalues(model_krokowy)
reszty_std <- rstandard(model_krokowy)
cook_d     <- cooks.distance(model_krokowy)

wpływowe <- which(cook_d > 4 / nrow(zbior_train))
cat("\n── Obserwacje o dużym wpływie (Cook D > 4/n) ──\n")
if (length(wpływowe) > 0) {
  print(zbior_train[wpływowe, c("nazwa", "cena_m2")])
} else cat("Brak obserwacji wpływowych.\n")


# =============================================================================
# 11. POPRAWA MODELU
# =============================================================================

# 11a. Transformacja logarytmiczna zmiennej objaśnianej (jeśli skośna)
skewness_y <- moments::skewness(zbior_train$cena_m2)
cat(sprintf("\nSkośność cena_m2: %.3f %s\n", skewness_y,
            ifelse(abs(skewness_y) > 1, "– rozważyć log-transformację", "")))

# Model z log(cena_m2)
model_log <- update(model_krokowy, log(cena_m2) ~ .)
cat("\n── Model z log(cena_m2) ──\n")
summary(model_log)

# Porównanie R² i AIC
cat(sprintf("R² krokowy: %.4f | R² log: %.4f\n",
            summary(model_krokowy)$r.squared,
            summary(model_log)$r.squared))
cat(sprintf("AIC krokowy: %.2f | AIC log: %.2f\n",
            AIC(model_krokowy), AIC(model_log)))

# 11b. Usunięcie obserwacji wpływowych (opcjonalne)
if (length(wpływowe) > 0) {
  zbior_train_clean <- zbior_train[-wpływowe, ]
  model_bez_outlierow <- update(model_krokowy, data = zbior_train_clean)
  cat("\n── Model bez obserwacji wpływowych ──\n")
  summary(model_bez_outlierow)
  cat(sprintf("R²: %.4f | AIC: %.2f\n",
              summary(model_bez_outlierow)$r.squared,
              AIC(model_bez_outlierow)))
}

# 11c. Wybór najlepszego modelu (zmień jeśli log daje lepsze wyniki)
# Domyślnie wybieramy model krokowy; zmień na model_log lub model_bez_outlierow
model_final <- model_log
# model_final <- model_log  # <- odkomentuj, jeśli log-model lepszy

cat("\n★ Wybrany model finalny:\n")
print(formula(model_final))

# Diagnostyka modelu finalnego
par(mfrow = c(2, 2))
plot(model_final, which = 1:4, main = "Diagnostyka – model finalny")
par(mfrow = c(1, 1))

cat("\n── VIF modelu finalnego ──\n")
print(vif(model_final))
cat("\n── Test BP modelu finalnego ──\n")
print(bptest(model_final))


# =============================================================================
# 12. PREDYKCJA NA ZBIORZE TESTOWYM I ANALIZA BŁĘDÓW EX POST
# =============================================================================

# Predykcja
# Jeśli model_final to model_log, pamiętaj o exp() przy odwracaniu transformacji:
if (deparse(formula(model_final)[[2]]) == "log(cena_m2)") {
  pred_test <- exp(predict(model_final, newdata = zbior_test))
  # Korekta Duana (bias correction dla log-transformacji)
  sigma2 <- summary(model_final)$sigma^2
  pred_test <- pred_test * exp(sigma2 / 2)
} else {
  pred_test <- predict(model_final, newdata = zbior_test)
}

rzeczywiste <- zbior_test$cena_m2

# Miary błędów
mae_val  <- Metrics::mae(rzeczywiste, pred_test)
mse_val  <- Metrics::mse(rzeczywiste, pred_test)
rmse_val <- Metrics::rmse(rzeczywiste, pred_test)
mape_val <- mean(abs((rzeczywiste - pred_test) / rzeczywiste)) * 100
r2_test  <- 1 - sum((rzeczywiste - pred_test)^2) /
  sum((rzeczywiste - mean(rzeczywiste))^2)

cat("\n══ ANALIZA BŁĘDÓW EX POST (zbiór testowy) ══\n")
cat(sprintf("MAE:  %.2f PLN\n", mae_val))
cat(sprintf("MSE:  %.2f\n",      mse_val))
cat(sprintf("RMSE: %.2f PLN\n", rmse_val))
cat(sprintf("MAPE: %.2f %%\n",  mape_val))
cat(sprintf("R²:   %.4f\n",      r2_test))

# Porównanie R² train vs test (overfitting?)
cat(sprintf("\nR² train: %.4f | R² test: %.4f | różnica: %.4f\n",
            summary(model_final)$r.squared, r2_test,
            summary(model_final)$r.squared - r2_test))

# Wykres: wartości rzeczywiste vs. przewidywane
wyniki_test <- data.frame(
  nazwa        = zbior_test$nazwa,
  rzeczywista  = rzeczywiste,
  prognoza     = pred_test,
  blad_abs     = abs(rzeczywiste - pred_test),
  blad_wzgl    = abs(rzeczywiste - pred_test) / rzeczywiste * 100
)

ggplot(wyniki_test, aes(x = rzeczywista, y = prognoza)) +
  geom_point(aes(color = blad_abs), size = 3, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "firebrick", linewidth = 1) +
  scale_color_gradient(low = "steelblue", high = "red",
                       name = "|błąd| [PLN]") +
  labs(title = "Wartości rzeczywiste vs. prognozowane (zbiór testowy)",
       x = "Cena rzeczywista [PLN/m²]",
       y = "Cena prognozowana [PLN/m²]") +
  theme_minimal(base_size = 12)

# Wykres reszt predykcji
ggplot(wyniki_test, aes(x = rzeczywista, y = rzeczywista - prognoza)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick") +
  geom_point(color = "steelblue", alpha = 0.7, size = 3) +
  geom_smooth(se = FALSE, color = "darkgreen", linewidth = 0.8) +
  labs(title = "Reszty predykcji (rzeczywista – prognoza)",
       x = "Cena rzeczywista [PLN/m²]",
       y = "Reszta predykcji [PLN]") +
  theme_minimal(base_size = 12)

# Top 5 największych błędów
cat("\n── Top 5 największych błędów predykcji ──\n")
print(wyniki_test %>%
        arrange(desc(blad_abs)) %>%
        dplyr::select(nazwa, rzeczywista, prognoza, blad_abs, blad_wzgl) %>%
        head(5) %>%
        mutate(across(where(is.numeric), ~round(., 2))),
      row.names = FALSE)


# =============================================================================
# 13. PODSUMOWANIE I INTERPRETACJA PARAMETRÓW
# =============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║           PODSUMOWANIE PROJEKTU                             ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

cat("CEL: Wyjaśnienie i prognoza ceny transakcyjnej 1m² mieszkania\n")
cat("DANE: GUS Bank Danych Lokalnych 2022r poziom województw/powiatów\n\n")

cat("── Wybrany model ──\n")
print(summary(model_final)$call)
cat("\n")

# Tabela współczynników z interpretacją
coef_df <- as.data.frame(summary(model_final)$coefficients)
coef_df$istotnosc <- ifelse(coef_df[,4] < 0.001, "***",
                            ifelse(coef_df[,4] < 0.01,  "**",
                                   ifelse(coef_df[,4] < 0.05,  "*",
                                          ifelse(coef_df[,4] < 0.1,   ".",  ""))))
names(coef_df) <- c("Estymata", "Błąd std.", "t-stat", "p-wartość", "Istotność")
cat("── Współczynniki ──\n")
print(cbind(round(coef_df[, 1:4], 4), Istotnosc = coef_df[, 5]))

cat("\n── Interpretacja (model liniowy bez log) ──\n")
cat("Wzrost zmiennej o 1 jednostkę → zmiana ceny 1m² o wartość współczynnika [PLN]\n")
cat("Przykład: wzrost wynagrodzenia o 1 PLN → cena_m2 zmienia się o",
    round(coef(model_final)["wynagrodzenie"], 4), "PLN\n")
cat("(Dla modelu log: wzrost zm. o 1 → zmiana ceny o (exp(β)-1)×100 %)\n\n")

cat("── Miary dopasowania (trening) ──\n")
cat(sprintf("R²:          %.4f\n", summary(model_final)$r.squared))
cat(sprintf("R² adj.:     %.4f\n", summary(model_final)$adj.r.squared))
cat(sprintf("AIC:         %.2f\n",  AIC(model_final)))
cat(sprintf("BIC:         %.2f\n",  BIC(model_final)))

cat("\n── Miary błędu predykcji (test) ──\n")
cat(sprintf("RMSE: %.2f PLN | MAE: %.2f PLN | MAPE: %.2f %%\n",
            rmse_val, mae_val, mape_val))
cat(sprintf("R² test: %.4f\n", r2_test))

cat("\n── Wnioski ──\n")
cat("1. Model wyjaśnia", round(summary(model_final)$r.squared * 100, 1),
    "% zmienności cen mieszkań za m² w zbiorze treningowym.\n")
cat("2. Błąd predykcji MAPE =", round(mape_val, 2),
    "% świadczy o", ifelse(mape_val < 10, "bardzo dobrej",
                           ifelse(mape_val < 20, "dobrej", "umiarkowanej")),
    "trafności modelu.\n")
cat("3. Zmienne istotnie wpływające na cenę m²:\n")
istotne <- rownames(coef_df)[coef_df$`p-wartość` < 0.05 &
                               rownames(coef_df) != "(Intercept)"]
for (zm in istotne) {
  kierunek <- ifelse(coef_df[zm, "Estymata"] > 0, "dodatni ↑", "ujemny ↓")
  cat(sprintf("   • %s: wpływ %s\n", zm, kierunek))
}

cat("\n── Ograniczenia modelu ──\n")
cat("• Analiza przekrojowa (1 rok) – brak efektów dynamicznych\n")
cat("• Możliwa endogeniczność (np. migracje ↔ ceny mieszkań)\n")
cat("• Dane agregowane na poziomie regionalnym – błąd ekologiczny\n")
cat("• Model liniowy nie uchwytuje nieliniowych zależności\n")

cat("\n══ KONIEC SKRYPTU ══\n")
