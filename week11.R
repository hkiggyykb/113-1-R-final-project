# commit and push

library(tidyverse)

data <- read_csv("./2018_01_10140-01-05-2_臺中市 初婚人數按性別、年齡及教育程度分暨其結婚年齡中位數(按發生日期).csv")

tidy_data <- data %>%
  mutate(
    tidy_education = str_extract(`欄位名稱`, "博士畢業|碩士畢業|大學畢業|專科畢業|高中畢業|國中畢業|國小畢業以下|教育程度別"),
    tidy_gender = case_when(
      str_detect(`欄位名稱`, "_男_") ~ "男",
      str_detect(`欄位名稱`, "_女_") ~ "女",
      TRUE ~ NA_character_
    ),
    tidy_age_group = case_when(
      str_detect(`欄位名稱`, "未滿15歲") ~ "未滿15歲",
      str_detect(`欄位名稱`, "15~19歲") ~ "15~19歲",
      str_detect(`欄位名稱`, "20~24歲") ~ "20~24歲",
      str_detect(`欄位名稱`, "25~29歲") ~ "25~29歲",
      str_detect(`欄位名稱`, "30~34歲") ~ "30~34歲",
      str_detect(`欄位名稱`, "35~39歲") ~ "35~39歲",
      str_detect(`欄位名稱`, "40~44歲") ~ "40~44歲",
      str_detect(`欄位名稱`, "45~49歲") ~ "45~49歲",
      str_detect(`欄位名稱`, "50~54歲") ~ "50~54歲",
      str_detect(`欄位名稱`, "55~59歲") ~ "55~59歲",
      str_detect(`欄位名稱`, "60~64歲") ~ "60~64歲",
      str_detect(`欄位名稱`, "65歲以上") ~ "65歲以上",
      TRUE ~ NA_character_
    )
  )
