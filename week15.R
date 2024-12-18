# Regex
filtered_data <- tidy_data %>%
  filter(
    str_detect(tidy_education, "大學畢業"),  # Matches "大學畢業" exactly
    str_detect(tidy_age_group, "\\d+~\\d+歲")  # Matches patterns like "15~19歲"
  )

print(filtered_data)

# Group by tidy_education and tidy_gender, then summarize the total
education_gender_summary <- tidy_data %>%
  group_by(tidy_education, tidy_gender) %>%
  summarize(total_people = sum(數值, na.rm = TRUE))

# Print the summarized result
print(education_gender_summary)

# Pivot table: Education levels as rows, Gender as columns, and total people as values
pivot_table <- tidy_data %>%
  group_by(tidy_education, tidy_gender) %>%       # Summarize first
  summarize(total_people = sum(數值, na.rm = TRUE)) %>%
  pivot_wider(names_from = tidy_gender,           # Pivot genders to columns
              values_from = total_people, 
              values_fill = 0)                    # Fill missing values with 0

# Print the pivot table
print(pivot_table)

