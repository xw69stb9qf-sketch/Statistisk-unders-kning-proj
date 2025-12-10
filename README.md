# Statistisk-unders-kning-proj

# 8 Bilaga
## 8.1 Bilaga A
# I bilaga A presenteras kod som har med inläsning, filtrering, imputtering och aggregering av data.

utspelare <- read.csv2("/Users/jryzr/Desktop/utespelare.csv")
malvakt <- read.csv2("/Users/jryzr/Desktop/målvakter.csv")

#Extrahera säsong och filtrera bort S1 och S17
malvakt$säsong <- sub(" .*", "", malvakt$matchday)
utspelare$säsong <- sub(". .*", "", utspelare$matchday)

malvakt <- malvakt[!malvakt$säsong %in% c("S1", "S17"), ]
utspelare <- utspelare[!utspelare$säsong %in% c("S1", "S17"), ]

# Välj variabler för analys
valda_variabler <- c("ref", "han", "pun", "tro", "aer", "ecc", "total.saves")

# slå ihop målvakter och attributdata
sammanslagen <- merge(
  malvakt[, c("name", "matchday", "säsong", "total.saves")],
  utspelare[, c("name", "matchday", valda_variabler[valda_variabler != "total.saves"])],
  by = c("name", "matchday")
)

# Konvertera numeriska och kategoriska
sammanslagen$säsong <- as.factor(sammanslagen$säsong)
sammanslagen$matchday <- as.factor(sammanslagen$matchday)

# Konvertera numeriska attribut
for (col in valda_variabler) {
  sammanslagen[[col]] <- as.numeric(gsub(".", ", ", sammanslagen[[col]]))
}

# Förbered imputering
# Inkludera total.saves, säsong, matchday som prediktorer
imputering_data <- sammanslagen[, c("säsong", "matchday", valda_variabler)]

# Skapa predictorMatrix
predMat <- make.predictorMatrix(inputering_data)
predMat[, c("säsong", "matchday")] <- 1
predMat["säsong", ] <- 0
predMat["matchday", ] <- 0

# Kör PMM
imputerad <- mice(inputering_data, method = "pmm", predictorMatrix = predMat, m = 5, seed = 123)

# komplett datamängd
komplett_data <- complete(inputerad, 1)

# Lägg tillbaka name och matchday
sammanslagen_imputerad <- cbind(
  sammanslagen[, c("name", "matchday")],
  komplett_data[, valda_variabler]
)

# Aggregera per målvakt
aggregerad <- sammanslagen_imputerad %>% 
  group_by(name) %>% 
  summarise(across(where(is.numeric), ~ mean(x, na.rm = TRUE)))

## 8.2 Bilaga B
# I bilaga B presenteras kod för multipel linjär regression, stegvis regression och utvärdering

#bygga modell
modell_full <- lm(total.saves ~ ref + han + pun + tro + aer + ecc, data = aggregerad)

modell_full %>%
  summary() %>%
  coef() %>%
  round(3) %>%
  kable(col.names = c("Variable1", "Skattning", "Medelfel", "t-värdet", "p-värdet")) %>%
  kable_styling("striped")

#residualanalysis
residualData <- tibble(
  residuals = residuals(modell_full),
  y = aggregerad$total.saves,
  yHat = fitted(modell_full)
)

p1 <- ggplot(data = residualData) + 
  aes(x = residuals, y = after_stat(density)) +
  geom_histogram(bins = 10, fill = "purple", color = "black") +
  theme_bw()

p2 <- ggplot(residualData) +
  # Använder standardiserade residualer
  aes(sample = scale(residuals)) +
  geom_qq_line() +
  geom_qq(color = "purple") +
  theme_bw() +
  labs(x = "Teoretiska kvantiler", y = "Observerade kvantiler")

p3 <- ggplot(residualData) +
  aes(x = yHat, y = residuals) +
  geom_point(color = "purple") +
  theme_bw() +
  labs(x = "Anpassade vården", y = "Residualer") +
  geom_hline(aes(yintercept = 0)) +
  # Imaginära gränser
  geom_hline(aes(yintercept = -5), color = "#d9230f", linetype = 2) +
  geom_hline(aes(yintercept = 5), color = "#d9230f", linetype = 2)

cowplot::plot_grid(p1, p2, p3, nrow = 2)

#börja stegvis
stegModell <- ols_step_both_aic(modell_full)

#börja framläggning
framåtModell <- ols_step_forward_aic(modell_full)

#börja bakåt
bakåt_modell <- ols_step_backward_aic(modell_full)

#kör fram
framåtModell <- lm(data = aggregerad, total.saves ~ tro)

residualData_tva <- tibble(
  residuals = residuals(framåtModell),
  y = aggregerad$total.saves,
  yHat = fitted(framåtModell)
)

a1 <- ggplot(data = residualData_tva) + 
  aes(x = residuals, y = after_stat(density)) +
  geom_histogram(bins = 10, fill = "purple", color = "black") +
  theme_bw()

a2 <- ggplot(residualData_tva) +
  # Använder standardiserade residualer
  aes(sample = scale(residuals)) +
  geom_qq_line() +
  geom_qq(color = "purple") +
  theme_bw() +
  labs(x = "Teoretiska kvantiler", y = "Observerade kvantiler")

a3 <- ggplot(residualData_tva) +
  aes(x = yHat, y = residuals) +
  geom_point(color = "purple") +
  theme_bw() +
  labs(x = "Anpassade vården", y = "Residualer") +
  geom_hline(aes(yintercept = 0)) +
  # Imaginära gränser
  geom_hline(aes(yintercept = -5), color = "#d9230f", linetype = 2) +
  geom_hline(aes(yintercept = 5), color = "#d9230f", linetype = 2)

cowplot::plot_grid(a1, a2, a3, nrow = 2)

#interaktionsmodell_dubbel
modell_inter <- lm(total.saves ~ (ref + han + pun + tro + aer + ecc)^2, data = aggregerad)
stepAIC(modell_inter, direction = "backward")

final_model1 <- lm(total.saves ~ han + tro + aer + aer:tro + han:aer, data = aggregerad)

final_model1 %>%
  summary() %>%
  coef() %>%
  round(3) %>%
  kable(col.names = c("Variable1", "Skattning", "Medelfel", "t-värdet", "p-värdet")) %>%
  kable_styling("striped")

residualData_tre <- tibble(
  residuals = residuals(final_model1),
  y = aggregerad$total.saves,
  yHat = fitted(final_model1)
)

c1 <- ggplot(data = residualData_tre) + 
  aes(x = residuals, y = after_stat(density)) +
  geom_histogram(bins = 10, fill = "purple", color = "black") +
  theme_bw()

c2 <- ggplot(residualData_tre) +
  # Använder standardiserade residualer
  aes(sample = scale(residuals)) +
  geom_qq_line() +
  geom_qq(color = "purple") +
  theme_bw() +
  labs(x = "Teoretiska kvantiler", y = "Observerade kvantiler")

c3 <- ggplot(residualData_tre) +
  aes(x = yHat, y = residuals) +
  geom_point(color = "purple") +
  theme_bw() +
  labs(x = "Anpassade vården", y = "Residualer") +
  geom_hline(aes(yintercept = 0)) +
  # Imaginära gränser
  geom_hline(aes(yintercept = -5), color = "#d9230f", linetype = 2) +
  geom_hline(aes(yintercept = 5), color = "#d9230f", linetype = 2)

cowplot::plot_grid(c1, c2, c3, nrow = 2)

#interaktionsmodell_enskild
final_model1_tva <- lm(formula = total.saves ~ tro + aer + tro:aer, data = aggregerad)

final_model1_tva %>%
  summary() %>%
  coef() %>%
  round(3) %>%
  kable(col.names = c("Variable1", "Skattning", "Medelfel", "t-värdet", "p-värdet")) %>%
  kable_styling("striped")

residualData_fyra <- tibble(
  residuals = residuals(final_model1_tva),
  y = aggregerad$total.saves,
  yHat = fitted(final_model1_tva)
)

d1 <- ggplot(data = residualData_fyra) + 
  aes(x = residuals, y = after_stat(density)) +
  geom_histogram(bins = 10, fill = "purple", color = "black") +
  theme_bw()

d2 <- ggplot(residualData_fyra) +
  # Använder standardiserade residualer
  aes(sample = scale(residuals)) +
  geom_qq_line() +
  geom_qq(color = "purple") +
  theme_bw() +
  labs(x = "Teoretiska kvantiler", y = "Observerade kvantiler")

d3 <- ggplot(residualData_fyra) +
  aes(x = yHat, y = residuals) +
  geom_point(color = "purple") +
  theme_bw() +
  labs(x = "Anpassade vården", y = "Residualer") +
  geom_hline(aes(yintercept = 0)) +
  # Imaginära gränser
  geom_hline(aes(yintercept = -5), color = "#d9230f", linetype = 2) +
  geom_hline(aes(yintercept = 5), color = "#d9230f", linetype = 2)

cowplot::plot_grid(d1, d2, d3, nrow = 2)

#AIC och R^2
olika_AIC <- data_frame(
  Modell = c("modell_1", "modell_2", "modell_3", "modell_4"),
  AIC = c(214.04, 205.36, 198.27, 196.4),
  BIC = c(229.65, 211.21, 211.9267, 206.14),
  "SR^2_[adj]$" = c(0.039, 0.113, 0.28, 0.28)
)

olika_AIC %>%
  kable() %>%
  kable_styling("striped")

#KT för koefficienter för att kontrollera p värdet
confint(framåtModell) %>%
  as.data.frame() %>%
  round(3) %>%
  mutate(Skillnad = c(3.02, 0.29)) %>%
  select(Skillnad, everything()) %>%
  kable(caption = c("958KT för modell_2")) %>%
  kable_styling("striped")

confint(final_model1_tva) %>%
  as.data.frame() %>%
  round(3) %>%
  mutate(Skillnad = c(15.05, 1.55, 1.15, 0.11)) %>%
  select(Skillnad, everything()) %>%
  kable(caption = c("958KT för modell_4")) %>%
  kable_styling("striped")

confint(final_model1) %>%
  as.data.frame() %>%
  round(3) %>%
  mutate(Skillnad = c(19.79, 1.27, 1.58, 1.49, 0.095, 0.117)) %>%
  select(Skillnad, everything()) %>%
  kable(caption = c("958KT för modell_3")) %>%
  kable_styling("striped")

#F-test för tabell_11
anova(final_model1_tva, final_model1)
anova(bakåt_modell, final_model1)
anova(bakåt_modell, final_model1_tva)

## 8.3 Bilaga C
# I bilaga C presenteras kod som använts för beskrivande statistik.

# Skapa tabell med beskrivande statistik
beskrivande_statistik <- aggregerad %>%
  dplyr::select(-name) %>%
  pivot_longer(everything(), names_to = "Variable1", values_to = "Värde") %>%
  group_by(Variable1) %>%
  summarise(
    Medel = mean(Värde),
    SD = sd(Värde),
    Min = min(Värde),
    Max = max(Värde),
    .groups = "drop"
  )

# Visa som snygg tabell
beskrivande_statistik %>%
  mutate(across(where(is.numeric), ~round(.2))) %>%
  kable(col.names = c("Variable1", "Medel", "SD", "Min", "Max"),
        caption = "Tabell 1. Beskrivande statistik för förklarande variabler och responsvariabeln.",
        align = "lcccc") %>%
  kable_styling("striped", full_width = F)

# Histogram
# Histogram: Fördelning av genomsnittliga räddningar (frekvens)
ggplot(aggregerad, aes(x = total.saves)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", boundary = 0) +
  labs(
    title = "Fördelning av genomsnittliga räddningar per målvakt",
    x = "Genomsnittliga räddningar per match",
    y = "Antal målvakter"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 11)
  )

# Spridningsdiagram
aggregerad %>%
  pivot_longer(
    -c(name, total.saves),
    names_to = "Attribut",
    values_to = "Värde"
  ) %>%
  ggplot(aes(x = Värde, y = total.saves)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ Attribut, scales = "free_x") +
  labs(
    x = "Attribute",
    y = "Antal räddningar",
    title = "Samband mellan attribut och antal räddningar"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 11)
  )

# Korrelationsanalys
korrelation_analys <- cor(aggregerad[, c("ref", "han", "pun", "tro", "aer", "ecc")], method = "pearson") %>%
  kable() %>%
  kable_styling("striped", full_width = F)

# Skapa datamatris för korrelation
kor_matris <- cor(aggregerad[, c("ref", "han", "pun", "tro", "aer", "ecc")], method = "pearson")

# Rita ut grafen
corrplot(kor_matris, method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         mar = c(0,0,1,0))
