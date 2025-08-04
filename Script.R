df <- read.csv("C:\\Users\\user\\Downloads\\macro_data_25yrs.csv")


colnames(df) <- c("Дата", "Денежная масса М2", "Доходность 10 - летних облигаций (%)",
                  "Эффективная ставка по федеральным фондам (%)", "ИПЦ", 'Темп инфляции (%)', 
                  "SOFR")


df <- mutate(df, Дата = as.Date(Дата))
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}



df <- df %>%
  mutate(`Нормализованная денежная масса` = normalize(`Денежная масса М2`),
         `Нормализованный темп инфляции` = normalize(`Темп инфляции (%)`),
         `Нормализованный ИПЦ` = normalize(ИПЦ)
)

correlation <- cor(df$`Нормализованный темп инфляции`, df$`Нормализованный ИПЦ`)


ggplot(df, 
       aes(Дата)) +
  geom_smooth(aes(y = `Нормализованный темп инфляции`,
              color = "Темп инфляции"),
              se = FALSE) +
  geom_smooth(aes(y = `Нормализованный ИПЦ`,
              color = "ИПЦ"),
              se = FALSE) +
  scale_color_manual(
    name = "Показатели",
    values = c(
      "Темп инфляции" = "#2a9d8f",
      "ИПЦ" = "#e76f51"
    ))+
  labs(title = "Корреляция между изменениями ИПЦ и темпом инфляции в США",
       subtitle = "По данным за 2018 - 2025 гг.",
       x = "Год",
       y = "Нормализованные значения",
       caption = glue("Коэффициент корреляции = {round(correlation, 2)}")) +
  theme_modern_rc() +
  theme(
    axis.title.x = element_text(
      size = 13,
      hjust = 0.5,
      margin = margin(t = 15)
    ),
    axis.title.y = element_text(
      size = 13,
      hjust = 0.5,
      margin = margin(r = 15)
    ),
    plot.caption = element_text(size = 13 ,hjust = 1, color = "grey70")
  )
