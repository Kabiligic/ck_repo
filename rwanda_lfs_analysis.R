#We are going to do some exploratory data analysis on the Rwanda's 
#Labor Force Survey (LFS) data - 2019 to 2024

#Step 1: import the data

lfs_rwanda <- read.csv("C:/Users/User/Desktop/WS/datasets/ck_repo/rwanda_lfs.csv") |> 
  clean_names() |> #let's divide the numbers by 1 million to reduce the length
  mutate(
    working_age = working_age_population_16_years / 1000000,
    labour_force_m = labour_force / 1000000,
    employed_m = employed / 1000000,
    unemployed_m = unemployed / 1000000,
    out_of_labor = out_of_labour_force / 1000000
  ) |> #let's then transform 'quarter' into a date format
  mutate(quarter = as.Date(paste0("30-", quarter), format = "%d-%b-%y")) |> 
  #selecting only the new columns
  select(quarter, working_age, labour_force_m, employed_m, unemployed_m, out_of_labor)

View(lfs_rwanda) #viewing to check if the changes materialized

#Now, let's see the trends

ggplot(lfs_rwanda, aes(x = quarter))+
  geom_line(aes(y = labour_force_m, color = "labor force"))+
  geom_line(aes(y = employed_m, color = "employed"))+
  geom_line(aes(y = unemployed_m, color = "unemployed"))+
  geom_line(aes(y = out_of_labor, color = "out of labor"))+
  scale_color_manual(values = c("labor force" = "blue", 
                                "employed" = "green", 
                                "unemployed" = "red", 
                                "out of labor" = "brown"))+
  scale_y_continuous(breaks = seq(0, 5.5, by = 0.5))+
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months")+
  labs(
    title = "The rate of employed people picked up and grew after the pandemic",
    subtitle = "The out of labor force is decreasing since June 2022",
    colour = "Category",
    x = "Semester end",
    y = "Total number in millions",
    caption = "Source of data: https://www.statistics.gov.rw/publication/labour-force-survey-2024-q3"
  )

