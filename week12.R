glimpse(tidy_data)

tidy_data <- tidy_data %>%
  mutate(
    tidy_education = factor(
      tidy_education,
      levels = c(
        "國小畢業以下", "國中畢業", "高中畢業", "專科畢業", 
        "大學畢業", "碩士畢業", "博士畢業", "教育程度別"
      )
    ),
    tidy_gender = factor(tidy_gender, levels = c("男", "女")),
    tidy_age_group = factor(
      tidy_age_group,
      levels = c(
        "未滿15歲", "15~19歲", "20~24歲", "25~29歲", 
        "30~34歲", "35~39歲", "40~44歲", "45~49歲", 
        "50~54歲", "55~59歲", "60~64歲", "65歲以上"
      ),
      ordered = TRUE
    )
  )

glimpse(tidy_data)

levels(tidy_data$tidy_gender)
levels(tidy_data$tidy_age_group)
levels(tidy_data$tidy_education)

tidy_data <- tidy_data %>%
  mutate(數值 = as.numeric(數值))

tidy_data %>%
  filter(
    tidy_education == "大學畢業",
    tidy_age_group == "30~34歲"
  ) %>%
  summarise(total_people = sum(數值, na.rm = TRUE))
