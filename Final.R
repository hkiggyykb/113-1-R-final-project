rm(list = ls())

# ---------------------------------------------------------------------

# Import the CSV file
library(readr)

file_path <- "opendata112m112.csv"

data <- read_csv(file_path, skip = 1,
                 col_types = cols(.default = col_character()))

# Display the first few rows
print(head(data))

# ---------------------------------------------------------------------

# Remove unnecessary columns
data <- data |>
  select(-`統計年度`, -`按照別`)

# Display the updated data frame
print(head(data))

# Aggregate data by extracting city-level from 區域別
data <- data |>
  mutate(`區域別` = str_sub(`區域別`, 1, 3)) |>  # Extract the first three characters for city level
  group_by(`區域別`, `婚姻類型`, `性別`, `原屬國籍（地區）`, `教育程度`, `年齡`) |> 
  summarise(`結婚人數` = sum(as.numeric(`結婚人數`), na.rm = TRUE)) |>
  ungroup()

# Display the integrated data
print(head(data))

# ---------------------------------------------------------------------

library(dplyr)

# Integrate data by removing `原屬國籍（地區）`
data <- data |>
  group_by(`區域別`, `婚姻類型`, `性別`, `教育程度`, `年齡`) |> 
  summarise(`結婚人數` = sum(as.numeric(`結婚人數`), na.rm = TRUE)) |>
  ungroup()

# Display the updated data
print(head(data))

# Integrate data by removing `婚姻類型`
data <- data |>
  group_by(`區域別`, `性別`, `教育程度`, `年齡`) |> 
  summarise(`結婚人數` = sum(as.numeric(`結婚人數`), na.rm = TRUE)) |>
  ungroup()

# Display the updated data
print(head(data))

#---------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(forcats)

# Define the correct order for Education Levels and Age Groups
education_order <- c(
  "未滿15歲", "國小畢業以下", "國中畢業", "高中畢業", 
  "專科畢業", "大學畢業", "碩士畢業", "博士畢業"
)

age_order <- c(
  "未滿15歲", "15～19歲", "20～24歲", "25～29歲", "30～34歲", 
  "35～39歲", "40～44歲", "45～49歲", "50～54歲", 
  "55～59歲", "60～64歲", "65歲以上"
)

# Apply the sorting orders
age_edu_summary <- data |>
  group_by(`年齡`, `教育程度`) |>
  summarise(`總結婚人數` = sum(as.numeric(`結婚人數`), na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    `教育程度` = factor(`教育程度`, levels = education_order),
    `年齡` = factor(`年齡`, levels = age_order)
  )

# Plot with sorted levels
ggplot(age_edu_summary, aes(x = `年齡`, y = `教育程度`, fill = `總結婚人數`)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Marriage Trends by Age and Education Level",
    x = "Age Group",
    y = "Education Level",
    fill = "Number of Marriages"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----------------------------------------------------------------------------------------

ggplot(age_edu_summary, aes(x = `年齡`, y = `總結婚人數`, fill = `總結婚人數`)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  facet_wrap(~`教育程度`, scales = "free_y") +
  labs(
    title = "Number of Marriages by Age Across Education Levels",
    x = "Age Group",
    y = "Number of Marriages",
    fill = "Number of Marriages"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 10)
  )

#---------------------------------------------------------------------------------------


# Define six metropolitan municipalities
metro_cities <- c("臺北市", "新北市", "桃園市", "臺中市", "臺南市", "高雄市")

# Filter data for six metropolitan municipalities
metro_data <- data |> 
  dplyr::filter(`區域別` %in% metro_cities)

# Prepare data for age plot
age_summary <- metro_data |>
  group_by(`區域別`, `年齡`) |>
  summarise(`總結婚人數` = sum(as.numeric(`結婚人數`), na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    `年齡` = factor(`年齡`, levels = age_order)
  )

# Plot marriage trends by age
age_plot <- ggplot(age_summary, aes(x = `年齡`, y = `總結婚人數`, fill = `總結婚人數`)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  facet_wrap(~`區域別`, scales = "free_y") +
  labs(
    title = "Marriage Trends by Age in Six Metropolitan Municipalities",
    x = "Age Group",
    y = "Number of Marriages",
    fill = "Number of Marriages"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Prepare data for education plot
edu_summary <- metro_data |>
  group_by(`區域別`, `教育程度`) |>
  summarise(`總結婚人數` = sum(as.numeric(`結婚人數`), na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    `教育程度` = factor(`教育程度`, levels = education_order)
  )

# Plot marriage trends by education level
edu_plot <- ggplot(edu_summary, aes(x = `教育程度`, y = `總結婚人數`, fill = `總結婚人數`)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  facet_wrap(~`區域別`, scales = "free_y") +
  labs(
    title = "Marriage Trends by Education Level in Six Metropolitan Municipalities",
    x = "Education Level",
    y = "Number of Marriages",
    fill = "Number of Marriages"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plots
print(age_plot)
print(edu_plot)
