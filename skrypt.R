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
  "moments",      # skewness, kurtosis
  "randtests",
  "sandwich",
  "tidytext"
)

# Instalacja brakujących pakietów
new_pkgs <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
invisible(lapply(required_packages, library, character.only = TRUE))


# =============================================================================
# 1. WCZYTANIE DANYCH
# =============================================================================

dane_ceny_1m2_mieszkan <- read.csv2("data/RYNE_3787_CTAB_20260321171501.csv")
dane_wynagrodzenia      <- read.csv2("data/WYNA_2497_CTAB_20260321172330.csv")
dane_bezrobocie         <- read.csv2("data/RYNE_2392_CTAB_20260321172526.csv")
dane_ludnosc            <- read.csv2("data/LUDN_2425_CTAB_20260321173151.csv")
dane_ludn_na_1km        <- read.csv2("data/LUDN_2425_CTAB_20260321173131.csv")
dane_wsk_urb            <- read.csv2("data/LUDN_2425_CTAB_20260321173206.csv")
dane_liczba_pozwolen    <- read.csv2("data/PRZE_4431_CTAB_20260321173635.csv")
dane_migracje           <- read.csv2("data/LUDN_1350_CTAB_20260321174505.csv")
dane_podm_gosp          <- read.csv2("data/PODM_3802_CTAB_20260321174825.csv")
dane_liczba_studentow   <- read.csv2("data/SZKO_3226_CTAB_20260321175636.csv")

# =============================================================================
# 2. FUNKCJA POMOCNICZA – CZYSZCZENIE FORMATU GUS BDL
# =============================================================================

extract_gus <- function(df, nazwa_zmiennej) {
  
  col_kod   <- names(df)[1]   # "Kod"
  col_nazwa <- names(df)[2]   # "Nazwa"
  col_wartosc <- names(df)[3] # np. "ogółem.ogółem.2022..zł."
  
  wynik <- df[, c(col_kod, col_nazwa, col_wartosc)]
  names(wynik) <- c("kod", "nazwa", nazwa_zmiennej)
  
  # Konwersja na liczbę
  wynik[[nazwa_zmiennej]] <- suppressWarnings(as.numeric(as.character(wynik[[nazwa_zmiennej]])))
  
  return(wynik)
}

# =============================================================================
# 3. PRZYGOTOWANIE DANYCH
# =============================================================================

ceny          <- extract_gus(dane_ceny_1m2_mieszkan, "cena_m2")
wynagrodzenia <- extract_gus(dane_wynagrodzenia,     "wynagrodzenie")
bezrobocie    <- extract_gus(dane_bezrobocie,        "bezrobocie")
ludnosc       <- extract_gus(dane_ludnosc,           "ludnosc")
gestosc       <- extract_gus(dane_ludn_na_1km,       "gestosc")
urbanizacja   <- extract_gus(dane_wsk_urb,           "urbanizacja")
pozwolenia    <- extract_gus(dane_liczba_pozwolen,   "pozwolenia_na_1000")
migracje      <- extract_gus(dane_migracje,          "saldo_migracji")
studenci      <- extract_gus(dane_liczba_studentow,  "studenci")
podmioty      <- extract_gus(dane_podm_gosp,         "podmioty_gosp")


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
head(dane)

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

# Usunięcie wierszy z cena_m2 <= 0 
n_niepoprawn <- sum(dane_clean$cena_m2 <= 0, na.rm = TRUE)
if (n_niepoprawn > 0) {
  cat(sprintf("\nUsunięto %d wierszy z cena_m2 <= 0:\n", n_niepoprawn))
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
  dplyr::select(-studenci, -saldo_migracji)

# 5d. Sprawdzenie wartości odstających (IQR)
wykryj_outliers <- function(x, nazwa) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- Q3 - Q1
  out <- sum(x < (Q1 - 3*IQR_val) | x > (Q3 + 3*IQR_val))
  if (out > 0) cat(sprintf("  %s: %d wartości odstających (3×IQR)\n", nazwa, out))
}

cat("\n── Wartości odstające (3×IQR) ──\n")
num_cols <- setdiff(names(dane_clean)[sapply(dane_clean, is.numeric)], "kod")
invisible(lapply(num_cols, function(col) wykryj_outliers(dane_clean[[col]], col)))

zm <- "wynagrodzenie"  # zmień na interesującą zmienną żeby zobaczyć outlierów
Q1 <- quantile(dane_clean[[zm]], 0.25)
Q3 <- quantile(dane_clean[[zm]], 0.75)
IQR_val <- Q3 - Q1

dane_clean[dane_clean[[zm]] < Q1 - 3*IQR_val | 
             dane_clean[[zm]] > Q3 + 3*IQR_val, 
           c("nazwa", zm)]
# widać że w powiecie jastrzębskim w wynagrodzeniu coś jest nie tak więc go usuwamy 

dane_clean <- dane_clean[dane_clean$nazwa != "Powiat m. Jastrzębie-Zdrój", ]

# =============================================================================
# 6. ANALIZA ROZKŁADÓW ZMIENNYCH
# =============================================================================

# 6a. Statystyki opisowe
cat("\n── Statystyki opisowe ──\n")
print(summary(dane_clean %>% dplyr::select(where(is.numeric))))

cat("\n── Skośność i kurtoza ──\n")
print(round(sapply(dane_clean %>% dplyr::select(where(is.numeric)),
                   function(x) c(skosnosc = moments::skewness(x, na.rm = TRUE),
                                 kurtoza  = moments::kurtosis(x, na.rm = TRUE))), 3))

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
           colors   = c("red","white","green"),
           title    = "Macierz korelacji zmiennych") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7c. Wykresy rozrzutu: zmienna objaśniana vs. predyktory
zmienne_objasniane <- setdiff(num_cols, "cena_m2")
p_list <- lapply(zmienne_objasniane, function(var) {
  ggplot(dane_clean, aes_string(x = var, y = "cena_m2")) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = paste("cena_m2 ~", var),
         x = var, y = "cena 1m² [PLN]") +
    theme_minimal(base_size = 9)
})
do.call(grid.arrange, c(p_list, ncol = 3))


# =============================================================================
# 8. PODZIAŁ NA ZBIÓR TRENINGOWY I TESTOWY
# =============================================================================

set.seed(123)

n <- nrow(dane_clean)
indeksy_train <- sample(seq_len(n), size = floor(0.70 * n))  # 70% trening

zbior_train <- dane_clean[ indeksy_train, ]
zbior_test  <- dane_clean[-indeksy_train, ]

cat(sprintf("\nZbiór treningowy: %d obs. | Zbiór testowy: %d obs.\n",
            nrow(zbior_train), nrow(zbior_test)))

# Sprawdzenie czy rozkłady są zbliżone
cat("Średnia cena_m2 – trening:", mean(zbior_train$cena_m2), "\n")
cat("Średnia cena_m2 – test:   ", mean(zbior_test$cena_m2), "\n")


# =============================================================================
# 9. DOBÓR ZMIENNYCH I ESTYMACJA MODELI
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

# 9c. Metoda Hellwiga
dane_Hellwig <- zbior_train[-c(1:2)]
cor_mat <- cor(dane_Hellwig)
r0 <-  cor_mat[1,-1]
r <- cor_mat[-1,-1]
comb <- expand.grid(rep(list(c(T,F)),9))
H <- numeric(nrow(comb))

for(i in 1:nrow(comb)){
  
  k <- which(unlist(comb[i,])) 
  
  if(length(k)==0){
    H[i] <- 0
  }else{
    h <- sapply(k,function(j){
      (r0[j]^2)/sum(abs(r[j,k]))
    })
    H[i] <- sum(h)
  }
}

max_H <- max(H)
best_model <- which.max(H)
comb[best_model,]

# Najlepszą kombinację uzyskamy biorąc: wynagrodzenie, bezrobocie, ludnosc, podmioty_gosp, studenci_na_1000 i saldo_migracji_na_1000

model_hellwig <- lm(cena_m2 ~ wynagrodzenie + bezrobocie + ludnosc +
                      saldo_migracji_na_1000 + podmioty_gosp + studenci_na_1000,
                    data = zbior_train)

# =============================================================================
# 10. DIAGNOSTYKA I PORÓWNANIE MODELI
# =============================================================================

cat("\n══ DIAGNOSTYKA ══\n")

diagnostyka <- function(model){
  print(summary(model))
  print(shapiro.test(model$residuals))
  print(bptest(model))
  print(vif(model))
  print(dwtest(model))
  print(reset(model))
  print(runs.test(model$residuals))
}

diagnostyka(model_krokowy)
diagnostyka(model_hellwig)

# 10b. Modele logarytminczne

model_krokowy_log <- update(model_krokowy, log(cena_m2) ~ .)
model_hellwig_log <- update(model_hellwig, log(cena_m2) ~ .)

cat("\n══ DIAGNOSTYKA MODELI LOGARYTMICZNYCH ══\n")

diagnostyka(model_krokowy_log)
diagnostyka(model_hellwig_log)

# =============================================================================
# 11. PREDYKCJA NA ZBIORZE TESTOWYM ORAZ PORÓWNANIE MODELI (POPRAWIONE)
# =============================================================================

p1_log <- predict(model_krokowy_log, newdata = zbior_test)
pred1  <- exp(p1_log)

p2_log <- predict(model_hellwig_log, newdata = zbior_test)
pred2  <- exp(p2_log)

e1 <- zbior_test$cena_m2 - pred1
e2 <- zbior_test$cena_m2 - pred2

MAE1 <- mean(abs(e1))
MAE2 <- mean(abs(e2))

RMSE1 <- sqrt(mean(e1^2))
RMSE2 <- sqrt(mean(e2^2))

MAPE1 <- mean(abs(e1 / zbior_test$cena_m2)) * 100
MAPE2 <- mean(abs(e2 / zbior_test$cena_m2)) * 100

wyniki_porownanie <- data.frame(
  Model = c("AIC (log)", "Hellwig (log)"),
  MAE   = round(c(MAE1, MAE2), 2),
  RMSE  = round(c(RMSE1, RMSE2), 2),
  MAPE  = round(c(MAPE1, MAPE2), 2)
)

# 12a. Analiza wyników i wizualizacja

# Zbieramy wyniki dla najlepszego modelu (przyjmujemy AIC wg Sekcji 11)
wyniki <- data.frame(
  nazwa     = zbior_test$nazwa,
  rzecz     = zbior_test$cena_m2,
  pred      = pred1,
  blad_proc = abs(e1 / zbior_test$cena_m2) * 100,
  blad_pln  = abs(e1)
)

# Wykres 1
ggplot(wyniki, aes(x = rzecz, y = pred)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    x = "Cena rzeczywista [PLN/m²]",
    y = "Cena przewidywana [PLN/m²]") +
  theme_minimal(base_size = 14)

# Wykres 2
dane_skrajne <- bind_rows(
  wyniki %>% 
    arrange(desc(blad_proc)) %>% 
    head(10) %>% 
    mutate(typ = "Największe błędy"),
  
  wyniki %>% 
    arrange(blad_proc) %>% 
    head(10) %>% 
    mutate(typ = "Najmniejsze błędy")
) %>%
  mutate(typ = factor(typ, levels = c("Największe błędy", "Najmniejsze błędy")))

ggplot(
  dane_skrajne %>% 
    mutate(sort_val = ifelse(typ == "Największe błędy", blad_proc, -blad_proc)),
  aes(x = reorder_within(nazwa, sort_val, typ), y = blad_proc, fill = typ)
) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", blad_proc)), 
            hjust = -0.1, size = 3.5, color = "black") +
  coord_flip() +
  facet_wrap(~ typ, scales = "free") +
  scale_x_reordered(labels = function(x) gsub("___.*", "", x)) +
  scale_fill_manual(values = c("Największe błędy" = "tomato", 
                               "Najmniejsze błędy" = "mediumseagreen")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(x = NULL, y = "Błąd procentowy") +
  theme_minimal(base_size = 14)

# Wykres 3
dane_skrajne_pln <- bind_rows(
  wyniki %>% 
    arrange(desc(blad_pln)) %>% 
    head(10) %>% 
    mutate(typ = "Największe błędy (PLN)"),
  
  wyniki %>% 
    arrange(blad_pln) %>% 
    head(10) %>% 
    mutate(typ = "Najmniejsze błędy (PLN)")
) %>%
  mutate(typ = factor(typ, levels = c("Największe błędy (PLN)", "Najmniejsze błędy (PLN)")))

ggplot(
  dane_skrajne_pln %>% 
    mutate(sort_val = ifelse(typ == "Największe błędy (PLN)", blad_pln, -blad_pln)),
  aes(x = reorder_within(nazwa, sort_val, typ), y = blad_pln, fill = typ)
) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(blad_pln), " PLN")), 
            hjust = -0.1, size = 3.5, color = "black") +
  coord_flip() +
  facet_wrap(~ typ, scales = "free") +
  scale_x_reordered(labels = function(x) gsub("___.*", "", x)) +
  scale_fill_manual(values = c("Największe błędy (PLN)" = "tomato", 
                               "Najmniejsze błędy (PLN)" = "mediumseagreen")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(x = NULL, y = "Błąd [PLN/m²]") +
  theme_minimal(base_size = 14)

# Brak normalności reszt nie wpływa istotnie na estymatory MNK
# przy dużej liczbie obserwacji (twierdzenie graniczne)