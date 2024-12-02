library(tidyverse)
library(ggridges)
library(janitor)
library(scales)

View(mpg)

ggplot(mpg, aes(x = displ, y = hwy, color = class))+
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, shape = class))+
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, alpha = class))+
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, size = class))+
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(color = "blue")

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(size = 2, color = "blue")

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(shape = 2, size = 1, color = "purple", stroke = 1.5)

ggplot(mpg, aes(color = displ < 5, x = displ, y = hwy))+
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_smooth(method = "loess")

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_smooth(method = "lm")

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_smooth()+
  geom_point(aes(color = class))

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_smooth(method = "lm")+
  geom_point(aes(color = class))

ggplot(mpg, aes(x = displ, y = hwy, group = drv))+
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_smooth(aes(linetype = drv, color = drv), show.legend = TRUE)

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_smooth(aes(linetype = drv, color = drv), show.legend = FALSE)

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = class))+
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point() +
  geom_point(
    data = mpg |> filter(class == "2seater"),
    color = "red"
  ) + 
  geom_point(
    data = mpg |> filter(class == "2seater"),
    shape = "circle open", size = 3, color = "red"
  )

ggplot(mpg, aes(x = hwy))+
  geom_histogram(binwidth = 2)

ggplot(mpg, aes(x = hwy))+
  geom_density()

ggplot(mpg, aes(x = hwy))+
  geom_boxplot()

ggplot(mpg, aes(x = hwy, y = drv, color = drv, fill = drv))+
  geom_density_ridges(alpha = 0.4)

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point()+
  geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy, group = drv))+
  geom_point()+
  geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy, group = drv, color = drv))+
  geom_point()+
  geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = drv))+
  geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = drv))+
  geom_smooth(aes(linetype = drv), se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = drv))

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point()+
  facet_wrap(~cyl)

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point()+
  facet_grid(~drv~cyl, scales = "free")

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

ggplot(mpg, aes(x = drv, fill = class))+
  geom_bar()

ggplot(mpg, aes(x = drv, fill = class))+
  geom_bar(alpha = 0.5, position = "identity")

ggplot(mpg, aes(x = drv, color = class))+
  geom_bar(fill = NA, position = "identity")


ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(position = "fill")


ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(position = "dodge")


ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(position = "jitter")+
  geom_smooth(method = "loess", se = TRUE, color = "blue")

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point()

ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point(position = "jitter")+
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_jitter()

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_jitter(height = 0.4, width = 0.4)


nz <- map_data("nz")

ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black")

ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

ggplot(diamonds, aes(x = carat))+
  geom_histogram(binwidth = 0.5)

smaller <- diamonds |> 
  filter(carat < 3)

ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(diamonds, aes(x = y)) + 
  geom_histogram(binwidth = 0.5)

ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot(fill = "aquamarine2")

diamonds |> 
  count(color, cut) |>  
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = n))

ggplot(smaller, aes(x = carat, y = price)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

ggplot(smaller, aes(x = carat, y = price)) +
  geom_bin2d()

ggplot(smaller, aes(x = carat, y = price))+
  geom_hex()

## we are off to "communication", the layers are done

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    color = "Car type",
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov"
  )

## ariko uwagerageza akanjye bwite ra?

fr <- read_csv("C:/Users/User/Desktop/WS/datasets/if_amounts.csv") |> 
  clean_names() |> 
  mutate(receipts = receipts / 1000000) |> 
  mutate(expenses = expenses / 1000000) |> 
  mutate(year_ending = as.Date(paste0(year_ending, "-06-30")))
View(fr)

ggplot(fr)+
  geom_line(aes(x = year_ending, y = receipts))+
  geom_line(aes(x = year_ending, y = expenses))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
  labs(
    title = "Our annual receipts have consistenly grown over the last decade",
    subtitle = "Our receipts grew tremendously at the transition to the 2028-2024 strategic plan",
    x = "fiscal year",
    y = "receipts (in Million Rwf)",
    caption = "data from our financial system"
  )

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    color = "Car type",
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov"
  )

library(maps)
world_map <- map("world")

world_data <- map_data("world")
View(world_data)


library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")
plot(world["geometry"])

rwanda <- world_data |> 
  filter(region == "Rwanda")

View(rwanda)

ggplot(rwanda, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "orange", color = "black")


tz <- world_data |> 
  filter(region == "Tanzania")

View(tz)

ggplot(tz, aes(x = long, y = lat, group = group))+
  geom_polygon(fill = "green", color = "red")

#using the geom_text, starting with creating a new data frame

label_info <- mpg |> 
  group_by(drv) |> 
  arrange(desc(displ)) |> 
  slice_head(n = 1) |> 
  mutate(
    drive_type = case_when(
      drv == "f" ~ "front-wheel drive",
      drv == "r" ~ "rear-wheel drive",
      drv == "4" ~ "4-wheel drive"
    )
  ) |> 
  select(displ, hwy, drv, drive_type)

View(label_info)

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_text(
    data = label_info, 
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold", size = 5, hjust = "right", vjust = "bottom"
  ) +
  theme(legend.position = "none")

#using the ggrepel's geom_label_repel function to remove overlaps

ggplot(mpg, aes(x = displ, y = hwy, color = drv))+
  geom_point()+
  geom_smooth(se = FALSE)+
  geom_label_repel(
    data = label_info,
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold", size = 5, nudge_y = 2,
    )+
  theme(legend.position = "none")

#using the geom_text_repel

ggplot(mpg, aes(x = displ, y = hwy, color = drv))+
  geom_point()+
  geom_smooth(se = FALSE)+
  geom_text_repel(
    data = label_info,
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold", size = 5, nudge_y = 5
  )+
  theme(legend.position = "none")


#Let us mark potential outliers

#those with a highway consumption higher than 40 miles per galloon

#those with a highway consumption higher than 20 while their
#displacement is higher than 5 Liters
potential_outliers <- mpg |> 
  filter(hwy > 40 | (hwy > 20 & displ > 5))

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point()+
  geom_text_repel(data = potential_outliers, aes(label = model))+
  geom_point(data = potential_outliers, color = "red")+
  geom_point(
    data = potential_outliers,
    color = "red",size = 3, shape = "circle open"
  )

#trying to use annotate

#create a trend text
trend_text <- "Larger engine sizes tend to have lower fuel economy" |> 
  str_wrap(width = 30)

#then add a label and segment geoms
ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point()+
  annotate(
    geom = "label", x = 3.5, y = 37,
    label = trend_text,
    hjust = "left", color = "blue"
  )+
  annotate(
    geom = "segment", x = 3, y = 35, xend = 5, yend = 25, color = "blue",
    arrow = arrow(type = "closed")
  )+
  geom_text_repel(data = potential_outliers, aes(label = model))+
  geom_point(data = potential_outliers, color = "red")+
  geom_point(
    data = potential_outliers, color = "red",
    shape = "circle open", size = 3
  )


#let us see how to change the scales from the default

ggplot(mpg, aes(x = displ, y = hwy, color = drv))+
  geom_point()+
  scale_y_continuous(breaks = seq(15, 40, by = 5))

# let us set the x and y labels to null, and set the drv labels to 
#their full names

ggplot(mpg, aes(x = displ, y = hwy, color = drv))+
  geom_point()+
  scale_x_continuous(labels = NULL)+
  scale_y_continuous(labels = NULL)+
  scale_color_discrete(labels = c("4" = "four-wheel", "f" = "front-wheel", "r" = "rear-wheel"))

#let us create a plot with the diamonds data. We map the price and cut
#to the axes, and use the currency as labels
#note that this requires to install and load the 'scales' package

ggplot(diamonds, aes(x = price, y = cut))+
  geom_boxplot(alpha = 0.5)+
  scale_x_continuous(labels = label_dollar())

#now, let us add the "K" suffix to the x labels (K as in thousand)

ggplot(diamonds, aes(x = price, y = cut))+
  geom_boxplot(alpha = 0.5)+
  scale_x_continuous(
    labels = label_dollar(scale = 1/1000, suffix = "K"),
    breaks = seq(1000, 20000, by = 5000)
  )

#let us swap the axes

ggplot(diamonds, aes(x = cut, y = price))+
  geom_boxplot(alpha = 0.5)+
  scale_y_continuous(
    labels = label_dollar(scale = 1/1000, suffix = "K"),
    breaks = seq(1000, 20000, by = 5000)
  )


#of course I won't proceed witout trying to make it RWF, can I?
ggplot(diamonds, aes(x = cut, y = price))+
  geom_boxplot(alpha = 0.5)+
  scale_y_continuous(
    labels = label_currency(scale = 1/1000, prefix = "RWF", suffix = "K"),
    breaks = seq(1000, 20000, by = 5000)
  )


#let us place the legend in other positions using the theme

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = class))+
  theme(legend.position = "right") #this is the default position

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = class))+
  theme(legend.position = "left") # positioning the legend on the left

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = class))+
  theme(legend.position = "top")+
  guides(color = guide_legend(nrow = 2)) #positioning the legend on top

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = class))+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(nrow = 2)) #positioning the legend at the bottom

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = class))+
  theme(legend.position = "none") #removing the legend

#note the guides and guide_legend layer we added, to control the number
#of rows in the legend for the top and bottom positions
#that also works for the left and right positions, and we also control
#the number of columns by specifying ncol
#using legend.position = "none" removes the legend altogether

#let us set the legend and override an aesthetic

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = class))+
  geom_smooth(se = FALSE)+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 4)))

#let us practice some prior things we learnt on the graph above

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = class))+
  geom_smooth(se = FALSE)+
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 1))+
  labs(
    title = "The highway efficiency varies according to the displacement",
    subtitle = "The lower the displacement, the higher the highway efficiency",
    caption = "Data from in-built R package",
    x = "displacement (L)",
    y = "highway efficiency ( miles per galoon)"
  )+
  scale_y_continuous(
    labels = label_number(suffix = " mpg"),
    breaks = seq(0, 45, by = 5)
    )+
  scale_x_continuous(
    labels = label_number(suffix = " L")
  )+
  annotate(
    geom = "label", x = 3, y = 35,
    label = "large engine sizes result into lower fuel efficiency" |> 
      str_wrap(width = 40),
    hjust = "left", color = "blue"
  )

#let's look into the idea of plotting values vs plotting 
#their transformed values (such as logs)

ggplot(diamonds, aes(x = carat, y = price))+
  geom_bin2d() #the carat vs price

ggplot(diamonds, aes(x = log10(carat), y = log10(price)))+
  geom_bin2d() #their log10 values

#note that this makes the axes labeled with log10 values
#log10(carat) on x and log10(price) on y. Let's fix it 
#through replacing the scale

ggplot(diamonds, aes(x = carat, y = price))+
  geom_bin2d()+
  scale_x_log10()+
  scale_y_log10()

#let us also see how to replace the default color scales (simple)

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = drv)) # the default scale


ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = drv))+
  scale_color_brewer(
    palette = "Set1",
    labels = c("4" = "four-wheel", "f" = "front-wheel", "r" = "rear-wheel")
  ) #the colorblind scale. Note that I had fun changing the labels too

#the writers remind about the simple techniques earlier 
#learned. setting the 'shape' aes will also ensure that
#the plots remain readable in black & white printouts

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(colour = drv, shape = drv))+
  scale_color_brewer(palette = "Set1")

#let us see how scale_color_manual is used, when you have 
#specific colors you want to use. we'll use the 'presidential'
#dataset 

presidential |> 
  mutate(id = 33 + row_number()) |> 
  ggplot(aes(x = start, y = id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(values = c(Republican = "red", Democratic = "#00AEF3"))

