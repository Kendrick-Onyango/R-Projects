library(tidyverse)

table4a

tidy4a <- table4a %>% 
    gather('1999', '2000', key = "year", value = "cases")
tidy4b <- table4b %>%
  gather('1999', '2000', key = "year", value = "population")
left_join(tidy4a, tidy4b)

people <- tribble(
  ~name, ~key, ~value,
  #-----------------|--------|------
  "Phillip Woods",  "age", 45,
  "Phillip Woods", "height", 186,
  "Phillip Woods", "age", 50,
  "Jessica Cordero", "age", 37,
  "Jessica Cordero", "height", 156
)
glimpse(people)

pivot_wider(people, names_from="name", values_from = "value")


people2 <- people %>%
  group_by(name, key) %>%
  mutate(obs = row_number())
people2

pivot_wider(people2, names_from="name", values_from = "value")


people %>%
  distinct(name, key, .keep_all = TRUE) %>%
  pivot_wider(names_from="name", values_from = "value")


preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes", NA, 10,
  "no", 20, 12
)
preg


preg_tidy <- preg %>%
  pivot_longer(c(male, female), names_to = "sex", values_to = "count")

preg_tidy

preg_tidy2 <- preg %>%
  pivot_longer(c(male, female), names_to = "sex", values_to = "count", values_drop_na = TRUE)


preg_tidy2


preg_tidy3 <- preg_tidy2 %>%
  mutate(
    female = sex == "female",
    pregnant = pregnant == "yes"
  ) %>%
  select(female, pregnant, count)

preg_tidy3


filter(preg_tidy3, female, !pregnant)

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"))


tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"))

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), extra = "merge")


tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), extra = "drop")


tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"))


tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"), fill = "right")

library(ggplot2)
library("tidyverse")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"), fill = "left")

ggplot(mpg, aes(x = displ, y = hwy, size = cty)) +
  geom_point()

q()


table3

table3 %>% 
  separate(rate, into = c("cases", "population"))

separate(table3,year, into = c("century", "year"), sep = 2)


unite(table5, new, century, year)

library("ranger")

install.packages("ranger")

ranger

model_rf <- range(survived ~ ., data = titanic_imputed)

mpg?

ggplot(data = mtcars)+
  geom_point(mapping= aes(x = displ, y = hwy))

install.packages("mtcars")

mtcars

ggplot(data = mpg)


name = readline(prompt="Kendrick ")
age =  readline(prompt="30")
print(paste("My name is",Kendrick, "and I am",30,"years old."))


name = "Python"; 
n1 =  10; 
n2 =  0.5
nums = c(10, 20, 30, 40, 50, 60)
print(ls())
print("Details of the objects in memory:")
print(ls.str())


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

table5 %>%
  unite(new, century, year)

table5 %>%
  unite(new, century, year, sep = "")

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"))


tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"))

df <- data.frame(x = c("a", "a b", "a b c", NA))

df %>% separate(x, c("a", "b"))

df %>% separate(x, c("a", "b"), extra = "drop", fill = "right")

df %>% separate(x, c("a", "b"), extra = "merge", fill = "left")

df %>% separate(x, c("a", "b", "c"))

df <- data.frame(x = c("x: 123", "y: error: 7"))

df %>% separate(x, c("key", "value"), ": ", extra = "merge")

df <- data.frame(x = c(NA, "a?b", "a.d", "b:c"))

df %>% separate(x, c("A","B"), sep = "([\\.\\?\\:])")

df <- data.frame(x = c("a:1", "a:2", "c:4", "d", NA))

df %>% separate(x, c("key","value"), ":") %>% str

df %>% separate(x, c("key","value"), ":", convert = TRUE) %>% str


var <- "x"

df %>% separate(!!var, c("key","value"), ":")


stocks <- tibble(
  year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr = c( 1, 2, 3, 4, 2, 3, 4),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)

stocks %>%
  spread(year, return)

stocks %>%
  spread(year, return) %>%
  gather(year, return, `2015`:`2016`, na.rm = TRUE)


stocks %>%
  complete(year, qtr)


treatment <- tribble(
  ~ person, ~ treatment, ~response,
  "Derrick Whitmore", 1, 7,
  NA, 2, 10,
  NA, 3, 9,
  "Katherine Burke", 1, 4
)

treatment

treatment %>% 
  fill(person)

stocks %>% 
  pivot_wider(names_from = year, values_from = return,
              values_fill = 0)

stocks %>% 
  spread(year, return, fill = 0)

stocks %>% 
  complete(year, qtr, fill=list(return=0))

who

who1 <- who %>%
  gather(
    new_sp_m014:newrel_f65, key = "key",
    value = "cases",
    na.rm = TRUE
  )

who1

who1 %>%
  count(key)

who2 <- who1 %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))

who2

who3 <- who2 %>%
  separate(key, c("new", "type", "sexage"), sep = "_")

who3

who3 %>%
  count(new)

who4 <- who3 %>%
  select(-new, -iso2, -iso3)

who4

who5 <- who4 %>%
  separate(sexage, c("sex", "age"), sep = 1)

who5

tail(who5)

who %>%
  gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  mutate(
    code = stringr::str_replace(code, "newrel", "new_rel")
  ) %>%
  separate(code, c("new", "var", "sexage")) %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1)

who1 %>%
  filter(cases == 0) %>%
  nrow()

who3a <- who1 %>%
  separate(key, c("new", "type", "sexage"), sep = "_")


filter(who3a, new == "newrel") %>% head()


select(who3, country, iso2, iso3) %>%
  distinct() %>%
  group_by(country) %>%
  filter(n() > 1)

who5 %>%
  group_by(country, year, sex) %>%
  filter(year > 1995) %>%
  summarise(cases = sum(cases)) %>%
  unite(country_sex, country, sex, remove = FALSE) %>%
  ggplot(aes(x = year, y = cases, group = country_sex, colour = sex)) +
  geom_line()
 

library(tidyverse)

library(nycflights13)

install.packages("nycflights13")


airline
airlines
airports
planes
weather


flights_latlon <- flights %>%
  inner_join(select(airports, origin = faa, origin_lat = lat, origin_lon = lon),
             by = "origin"
  ) %>%
  inner_join(select(airports, dest = faa, dest_lat = lat, dest_lon = lon),
             by = "dest"
  )

flights_latlon

flights_latlon %>%
  slice(1:100) %>%
  ggplot(aes(
    x = origin_lon, xend = dest_lon,
    y = origin_lat, yend = dest_lat
  )) +
  borders("state") +
  geom_segment(arrow = arrow(length = unit(0.1, "cm"))) +
  coord_quickmap() +
  labs(y = "Latitude", x = "Longitude")

special_days <- tribble(
  ~year, ~month, ~day, ~holiday,
  2013, 01, 01, "New Years Day",
  2013, 07, 04, "Independence Day",
  2013, 11, 29, "Thanksgiving Day",
  2013, 12, 25, "Christmas Day"
)

install.packages("maps")
library(maps)

special_days

planes %>%
  count(tailnum) %>%
  filter(n > 1)

weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n > 1)

flights %>%
  count(year, month, day, flight) %>%
  filter(n > 1)

flights %>%
  count(year, month, day, tailnum) %>%
  filter(n > 1)

flights %>%
  arrange(year, month, day, sched_dep_time, carrier, flight) %>%
  mutate(flight_id = row_number()) %>%
  glimpse()

Lahman::Batting

install.packages("Lahman")

library(Lahman)

Lahman::Batting %>%
  count(playerID, yearID, stint) %>%
  filter(n > 1) %>%
  nrow()

install.packages("babynames")


babynames::babynames

babynames


babynames::babynames %>%
  count(year, sex, name) %>%
  filter(n > 1)

install.packages("nasaweather")

library(nasaweather)

nasaweather::atmos %>%
  count(lat, long, year, month) %>%
  filter(n > 1) %>%
  nrow()
nasaweather::nasaweather
nasaweather::atmos

install.packages("fueleconomy")

fueleconomy::vehicles

diamonds

ggplot2::diamonds

ggplot2::diamonds %>%
  distinct() %>%
  nrow()

diamonds <- mutate(ggplot2::diamonds, id = row_number())

diamonds

library(ggplot2)
library(dplyr)

data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)

data %>%
  filter( price<300 ) %>%
  ggplot( aes(x=price)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

library(hrbrthemes)

install.packages("hrbrthemes")

data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)

data %>%
  filter( price<300 ) %>%
  ggplot( aes(x=price)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Night price distribution of Airbnb appartements") +
  theme_ipsum()


Lahman::Batting %>%
  count(playerID, yearID, stint) %>%
  filter(n > 1) %>%
  nrow()

nasaweather::atmos %>%
  count(lat, long, year, month) %>%
  filter(n > 1) %>%
  nrow()

fueleconomy::vehicles %>%
  count(id) %>%
  filter(n > 1) %>%
  nrow()

ggplot2::diamonds %>%
  distinct() %>%
  nrow()

nrow(ggplot2::diamonds)

diamonds <- mutate(ggplot2::diamonds, id = row_number())

diamonds

install.packages("modelr")

library(modelr)

dm1 <- dm_from_data_frames(list(
  Batting = Lahman::Batting,
  Master = Lahman::Master,
  Salaries = Lahman::Salaries
)) %>%
  dm_set_key("Batting", c("playerID", "yearID", "stint")) %>%
  dm_set_key("Master", "playerID") %>%
  dm_set_key("Salaries", c("yearID", "teamID", "playerID")) %>%
  dm_add_references(
    Batting$playerID == Master$playerID,
    Salaries$playerID == Master$playerID
  )

install.packages("viridis")

remotes::update_packages(c('magrittr', 'Lahman', 'DiagrammeR'), upgrade = TRUE)

install.packages("magrittr")

install.packages("DiagrammeR")

lfshaibrary(Lahman)

list(Batting = Lahman::Batting, Master = Lahman::Master, Salaries = Lahman::Salaries) %>% 
  dm_from_data_frames() %>%
  dm_set_key("Batting", c("playerID", "yearID", "stint")) %>%
  dm_set_key("Master", "playerID") %>%
  dm_set_key("Salaries", c("yearID", "teamID", "playerID")) %>%
  dm_add_references(
    Batting$playerID == Master$playerID, 
    Salaries$playerID == Master$playerID
  ) %>% 
  dm_create_graph(rankdir = "LR", columnArrows = TRUE) %>% 
  dm_render_graph()

 
update.packages(c('magrittr', 'Lahman', 'DiagrammeR'), upgrade = TRUE)

install.packages(c("bergant/datamodelr"))


top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)

top_dest

flights %>% 
  filter(dest %in% top_dest$dest)

flights

library("nycflights13")

find('dest')

flights %>% 
  semi_join(top_dest)
flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)

nrow(anti_join(Lahman::Pitching, Lahman::Batting,
               by = c("playerID", "yearID", "stint")
))

nrow(anti_join(Lahman::Fielding, Lahman::Batting,
               by = c("playerID", "yearID", "stint")
))

install.packages("knitr")

library(knitr)

knitr::include_graphics("diagrams/Lahman3.png")

library("tidyverse")

flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)

flights2 %>%
  select(-origin, -dest) %>%
  left_join(airlines, by = "carrier")
flights2 %>%
  select(-origin, -dest) %>%
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

flights2 %>%
  select(-origin, -dest) %>%
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

x %>%
  inner_join(y, by = "key")

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)  

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)

left_join(x, y, by = "key")

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)

left_join(x, y, by = "key")

flights2 %>%
  left_join(weather)

flights2 %>%
  left_join(planes, by = "tailnum")

flights2 %>%
  left_join(airports, c("dest" = "faa"))

flights2 %>%
  left_join(airports, c("origin" = "faa"))

library(maps)

airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

avg_dest_delays <-
  flights %>%
  group_by(dest) %>%
  # arrival delay NA's are cancelled flights
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c(dest = "faa"))

avg_dest_delays %>%
  ggplot(aes(lon, lat, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

airport_locations <- airports %>%
  select(faa, lat, lon)

flights %>%
  select(year:day, hour, origin, dest) %>%
  left_join(
    airport_locations,
    by = c("origin" = "faa")
  ) %>%
  left_join(
    airport_locations,
    by = c("dest" = "faa")
  )

airport_locations <- airports %>%
  select(faa, lat, lon)

flights %>%
  select(year:day, hour, origin, dest) %>%
  left_join(
    airport_locations,
    by = c("origin" = "faa")
  ) %>%
  left_join(
    airport_locations,
    by = c("dest" = "faa"),
    suffix = c("_origin", "_dest"))

airport_locations <- airports %>%
  select(faa, lat, lon)

flights %>%
  select(year:day, hour, origin, dest) %>%
  left_join(
    airport_locations,
    by = c("origin" = "faa")
  ) %>%
  left_join(
    airport_locations,
    by = c("dest" = "faa")
  )

airport_locations <- airports %>%
  select(faa, lat, lon)

flights %>%
  select(year:day, hour, origin, dest) %>%
  left_join(
    airport_locations,
    by = c("origin" = "faa")
  ) %>%
  left_join(
    airport_locations,
    by = c("dest" = "faa"),
    suffix = c("_origin", "_dest"))  

plane_cohorts <- inner_join(flights,
                            select(planes, tailnum, plane_year = year),
                            by = "tailnum"
) %>%
  mutate(age = year - plane_year) %>%
  filter(!is.na(age)) %>%
  mutate(age = if_else(age > 25, 25L, age)) %>%
  group_by(age) %>%
  summarise(
    dep_delay_mean = mean(dep_delay, na.rm = TRUE),
    dep_delay_sd = sd(dep_delay, na.rm = TRUE),
    arr_delay_mean = mean(arr_delay, na.rm = TRUE),
    arr_delay_sd = sd(arr_delay, na.rm = TRUE),
    n_arr_delay = sum(!is.na(arr_delay)),
    n_dep_delay = sum(!is.na(dep_delay))
  )   


ggplot(plane_cohorts, aes(x = age, y = dep_delay_mean)) +
  geom_point() +
  scale_x_continuous("Age of plane (years)", breaks = seq(0, 30, by = 10)) +
  scale_y_continuous("Mean Departure Delay (minutes)")


ggplot(plane_cohorts, aes(x = age, y = arr_delay_mean)) +
  geom_point() +
  scale_x_continuous("Age of Plane (years)", breaks = seq(0, 30, by = 10)) +
  scale_y_continuous("Mean Arrival Delay (minutes)")

flight_weather <-
  flights %>%
  inner_join(weather, by = c(
    "origin" = "origin",
    "year" = "year",
    "month" = "month",
    "day" = "day",
    "hour" = "hour"
  ))


flight_weather %>%
  group_by(precip) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = precip, y = delay)) +
  geom_line() + geom_point()


flight_weather %>%
  ungroup() %>%
  mutate(visib_cat = cut_interval(visib, n = 10)) %>%
  group_by(visib_cat) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = visib_cat, y = dep_delay)) +
  geom_point()


flights %>%
  filter(year == 2013, month == 6, day == 13) %>%
  group_by(dest) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  ggplot(aes(y = lat, x = lon, size = delay, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap() +
  scale_colour_viridis()

install.packages("viridis")
library(viridis)

top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)

top_dest$dest

flights
view(flights)

flights %>%
  filter(dest %in% top_dest$dest)

flights %>% 
  semi_join(top_dest)


flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)

flights %>%
  filter(is.na(tailnum), !is.na(arr_time)) %>%
  nrow()

library(tidyverse)
library(nycflights13)

flights %>% 
  filter(is.na(tailnum), !is.na(arr_time)) %>%
  nrow()

flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(carrier, sort = TRUE) %>%
  mutate(p = n / sum(n))


flights %>%
  distinct(carrier, tailnum) %>%
  left_join(planes, by = "tailnum") %>%
  group_by(carrier) %>%
  summarise(total_planes = n(),
            not_in_planes = sum(is.na(model))) %>%
  mutate(missing_pct = not_in_planes / total_planes) %>%
  arrange(desc(missing_pct))

planes_gte100 <- flights %>%
  filter(!is.na(tailnum)) %>%
  group_by(tailnum) %>%
  count() %>%
  filter(n >= 100)

flights %>%
  semi_join(planes_gte100, by = "tailnum")

flights %>%
  filter(!is.na(tailnum)) %>%
  group_by(tailnum) %>%
  mutate(n = n()) %>%
  filter(n >= 100)

fueleconomy::vehicles %>%
  semi_join(fueleconomy::common, by = c("make", "model"))


fueleconomy::vehicles %>%
  distinct(model, make) %>%
  group_by(model) %>%
  filter(n() > 1) %>%
  arrange(model)

view(tail)

fueleconomy::common %>%
  distinct(model, make) %>%
  group_by(model) %>%
  filter(n() > 1) %>%
  arrange(model)

worst_hours <- flights %>%
  mutate(hour = sched_dep_time %/% 100) %>%
  group_by(origin, year, month, day, hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(dep_delay)) %>%
  slice(1:48)

weather_most_delayed <- semi_join(weather, worst_hours, 
                                  by = c("origin", "year",
                                         "month", "day", "hour"))

select(weather_most_delayed, temp, wind_speed, precip) %>%
  print(n = 48)


ggplot(weather_most_delayed, aes(x = precip, y = wind_speed, color = temp)) +
  geom_point()

planes_carriers <-
  flights %>%
  filter(!is.na(tailnum)) %>%
  distinct(tailnum, carrier)

planes_carriers %>%
  count(tailnum) %>%
  filter(n > 1) %>%
  nrow()

carrier_transfer_tbl <- planes_carriers %>%
  # keep only planes which have flown for more than one airline
  group_by(tailnum) %>%
  filter(n() > 1) %>%
  # join with airlines to get airline names
  left_join(airlines, by = "carrier") %>%
  arrange(tailnum, carrier)

carrier_transfer_tbl

airports %>% count(alt, lon) %>% filter(n > 1)

df1 <- tribble(
  ~x, ~y,
  1, 1,
  2, 1
)

df2 <- tribble(
  ~x, ~y,
  1, 1,
  1, 2
)

intersect(df1, df2)

union(df1, df2)

setdiff(df1, df2)

setdiff(df2, df1)

install.packages("stringr")
library(stringr)

string1 <- "This is a string"

string1

x <- c("\"", "\\")

x

writeLines(x)

?'"'

x <- "\u00b5"

x

c("one", "two", "three")

str_length(c("a", "R for data science", NA))

str_c("x", "y")

str_c("x", "y", sep = ", ")
=
x <- c("abc", NA)

str_c("|-", x, "-|")

str_c("|-", str_replace_na(x), "-|")

str_c("prefix-", c("a", "b", "c"), "-suffix")

name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)

str_c(c("x", "y", "z"), collapse = ", ")

x <- c("Apple", "Banana", "Pear")

str_sub(x, 1, 1)

str_sub(x, -3, -1)

str_sub("a", 1, 5)

str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))

x

str_to_upper(c("i", "i"))

str_to_upper(c("i", "i"), locale = "tr")

x <- c("apple", "eggplant", "banana")

str_sort(x, locale = "en")

str_sort(x, locale = "haw")

x <- c("a", "abc", "abcd", "abcde", "abcdef")
x

L <- str_length(x)
L

m <- ceiling(L / 2)
m

m <- floor(L/2)
m
str_sub(x, m, m)

?str_wrap

str_trim(" abc ")
str_trim(" abc ", side = "left")
str_trim(" abc ", side = "right")

str_pad("abc", 5, side = "both")
str_pad("abc", 5, side = "right")
str_pad("abc", 5, side = "left")


str_commasep <- function(x, delim = ",") {
  n <- length(x)
  if (n == 0) {
    ""
  } else if (n == 1) {
    x
  } else if (n == 2) {
    # no comma before and when n == 2
    str_c(x[[1]], "and", x[[2]], sep = " ")
  } else {
    # commas after all n - 1 elements
    not_last <- str_c(x[seq_len(n - 1)], delim)
    # prepend "and" to the last element
    last <- str_c("and", x[[n]], sep = " ")
    # combine parts with spaces
    str_c(c(not_last, last), collapse = " ")
  }
}
 str_commasep(x)

 str_commasep("")
 str_commasep("a")
 str_commasep(c("a", "b"))
 str_commasep(c("a", "b", "c", "d"))

 
 library(stringr)
library(tidyverse) 

 x <- c("apple", "banana", "pear") 
str_view(x, "an") 

str_view_all(x, "an")

str_view(x, ".a.")

dot <- "\\."
writeLines(dot)
str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b"
writeLines(x)

str_view(x, "\\\\")

str_view("\"'\\")
str_view("\"'\\", match = TRUE)
str_view("\"'\\", "\"'\\\\", match = TRUE)


str_view(c(".a.b.c", ".a.b", "....."), c("\\..\\..\\.."), match = TRUE)

str_view(c(".a.b.c", ".a.b", "....."), c("\\..\\..\\.."), match = TRUE)


x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")


x <- c("apple pie", "apple", "apple cake")
view(x, "apple")
str_view(x, "apple")
str_view(x, "^apple$")


str_view(stringr::words, "^y", match = TRUE)

str_view(stringr::words, "x$", match = TRUE)

str_view(stringr::words, "^...$", match = TRUE)

str_view(stringr::words, ".......", match = TRUE)

str_view(c("grey", "gray"), "gr(e|a)y")


str_subset(stringr::words, "^[aeiou]")

str_subset(stringr::words, "[aeiou]", negate = TRUE)

str_view(stringr::words, "[aeiou]", match=FALSE)

str_subset(stringr::words, "i(ng|se)$")

library(magrittr)

install.packages("highcharter")
library(highcharter)
library(mtcars)
install.packages("mtcars")
library(ggplot2)
mtcars


hchart(mtcars, "scatter", hcaes(wt, mpg, z = drat, color = hp)) %>%
  hc_title(text = "Scatter chart with size and color")

install.packages("plotly")
library(plotly)


p <- ggplot(data = diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "dodge")

ggplotly(p)

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"

str_view(x, "CC?")

install.packages("threejs")

z <- seq(-10, 10, 0.01)
x <- cos(z)
y <- sin(z)

scatterplot3js(x,y,z, color=rainbow(length(z)))

library(threejs)


str_view(fruit, "(..)\\1", match = TRUE)


install.packages("visNetwork")

library(visNetwork)

nodes <- data.frame(id = 1:6, title = paste("node", 1:6), 
                    shape = c("dot", "square"),
                    size = 10:15, color = c("blue", "red"))
nodes

edges <- data.frame(from = 1:5, to = c(5, 4, 6, 3, 3))

visNetwork(nodes, edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

library(stringr)

str_subset(c("ed", stringr::words), "(^|[^e])ed$")

str_subset(words, "([A-Za-z][A-Za-z]).*\\1")

stri_rand_strings(4, 8)

library(stringi)

install.packages("networkD3")

library(networkD3)

data(MisLinks, MisNodes)

forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.4)

install.packages("forcast")

library(forcats)
library(tidyverse)

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

k <- factor(y2, levels = month_levels)

y1 <- c("Dec", "Apr", "Jan", "Mar")

k

y2 <- c("Dec", "Apr", "Jam", "Mar")

k <- parse_factor(y2, levels = month_levels)

factor(y1)

f1 <- factor(y1, levels = unique(y1))
f1

f2 <- y1 %>% factor() %>% fct_inorder()
f2

levels(f2)

gss_cat

gss_cat %>%
  count(marital)

ggplot(gss_cat, aes(race)) +
  geom_bar()

ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = TRUE)


ggplot(gss_cat, aes(rincome)) +
  geom_line()

rincome_plot <-
  gss_cat %>%
  ggplot(aes(x = rincome)) +
  geom_bar()

rincome_plot

rincome_plot +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

rincome_plot +
  coord_flip()



filter(!denom %in% c(
  "No answer", "Other", "Don't know", "Not applicable",
  "No denomination"
)) %>%
  count(relig)

levels(gss_cat$denom)


gss_cat %>%
  count(relig, denom) %>%
  ggplot(aes(x = relig, y = denom, size = n)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))


summary(gss_cat[["tvhours"]])


gss_cat %>%
  filter(!is.na(tvhours)) %>%
  ggplot(aes(x = tvhours)) +
  geom_histogram(binwidth = 1)


keep(gss_cat, is.factor) %>% names()

levels(gss_cat[["marital"]])

gss_cat %>%
  ggplot(aes(x = marital)) +
  geom_bar()

gss_cat %>%
  ggplot(aes(relig)) +
  geom_bar() +
  coord_flip()

relig <- gss_cat %>%
  group_by(relig) %>%
  summarize(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
ggplot(relig, aes(tvhours, relig), color = relig) + geom_point()+
  scale_color_brewer()




