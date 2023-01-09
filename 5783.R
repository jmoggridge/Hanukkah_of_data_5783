
## 5783

library(tidyverse)
library(lubridate)

## USB stick data
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "usb/noahs.sqlite")
db_ls <- DBI::dbListTables(con)

customers <- tbl(con, 'customers') |> glimpse()
orders <- tbl(con, 'orders') |> glimpse()
orders_items <- tbl(con, 'orders_items') |> glimpse()
products <- tbl(con, 'products') |> glimpse()


## Part 0 ----------------------------------------------------

# password is: 5783 - 6 = 5777

## Part 1 ----------------------------------------------------
# find investigator's phone number that spells their name

# convert name to number spelt out on keypad
name_to_phone <- function(name){
  name_ls <- tolower(name) |> str_split('') |> unlist()
  to_num <-
    c(rep(2L:9L, 3), 7L, 9L) |> sort() |> set_names(c(letters))
  name_ls |>
    map_int(~to_num[.x]) |>
    paste0(collapse = '')
}

# map last name to phone, find matching to actual phone
investigator <-
  collect(customers) |>
  mutate(
    last_name = str_extract(name, '[^ ]+$'),
    namephone = map_chr(last_name, name_to_phone)
  ) |>
  filter(str_remove_all(phone,  '-') == namephone) |>
  glimpse()

# Sam Guttenberg is the investigator

rm(name_to_phone, investigator)

## Puzzle 2 ----------------------------------------------------

#' With your help, Sarah was able to call the private investigator that afternoon, and brought them up to speed. The investigator went to the cleaners directly to see if they could get any more information about the unclaimed rug.

#' While they were out, Sarah said, “I tried cleaning the rug myself, but there was this snail on it that always seemed to leave a trail of slime behind it. I spent a few hours cleaning it, and the next day the slime trail was back.”

#' When the investigator returned, they said, “Apparently, this cleaner had a special projects program, where they outsourced challenging cleaning projects to industrious contractors. As they’re right across the street from Noah’s, they usually talked about the project over coffee and bagels at Noah’s before handing off the item to be cleaned. The contractors would pick up the tab and expense it, along with their cleaning supplies.

#' “So this rug was apparently one of those special projects. The claim ticket said ‘2017 spec JD’. ‘2017’ is the year the item was brought in, and ‘JD’ is the initials of the contractor.

#' “But they stopped outsourcing a few years ago, and don’t have contact information for any of these workers anymore.”

#' Sarah first seemed hopeless, and then looked at the USB drive you had just put back in her hand. She said, “I know it’s a long shot, but is there any chance you could find their phone number?”



# codes to find coffee and bagels in order
coffee_bagel_sku <-
  products |>
  mutate(desc = str_to_lower(desc)) |>
  collect() |>
  filter(str_detect(desc, 'bagel$|^coffee')) |>
  pull(sku)

# any customers with initials JD
jd_customers <-
  collect(customers) |>
  filter(str_detect(name, 'J.*?D')) |>
  mutate(customerid = as.character(customerid))

# any orders by a JD during 2017
jd_orders <-
  collect(orders) |>
  mutate(date = lubridate::ymd_hms(ordered) |> as.Date()) |>
  filter(date > as.Date('2017-01-01'),
         date < as.Date('2018-01-01')) |>
  inner_join(jd_customers, by = 'customerid')

# find coffee & bagel orders, find orders in JD orders
sol2 <-
  orders_items |>
  filter(sku %in% coffee_bagel_sku,
         orderid %in% !!jd_orders$orderid) |>
  collect() |>
  mutate(orderid = as.character(orderid)) |>
  left_join(jd_orders, by = 'orderid') |>
  select(name, phone, orderid) |>
  distinct() |>
  glimpse()

# Jeremy Davis is the contractor

rm(sol2, jd_orders, jd_customers, coffee_bagel_sku)

## Puzzle 3 ----------------------------------------------------

#' Sarah and the investigator were very impressed with your data skills, as you were able to figure out the phone number of the contractor. They called up the cleaning contractor straight away and asked about the rug.

#' “Oh, yeah, I did some special projects for them a few years ago. I remember that rug unfortunately. I managed to clean one section, which revealed a giant spider that startled me whenever I tried to work on it.

#' “I already had a fear of spiders before this, but this spider was so realistic that I had a hard time making any more progress. I kept expecting the cleaners would call for the rug, but they never did. I felt so bad about it, I couldn’t face them, and of course they never gave me another project.

#' “At last I couldn’t deal with the rug taking up my whole bathtub, so I gave it to this guy who lived in my neighborhood. He said that he was naturally assertive because he was a Aries born in the year of the Dog, so maybe he was able to clean it.

#' “I don’t remember his name. Last time I saw him, he was leaving the subway and carrying a bag from Noah’s. I swore I saw a spider on his hat.”

#'Can you find the phone number of the person that the contractor gave the rug to?


###' Clues

#' year of dog
#' Aries
#' same neighbourhood as brent (south ozone park ...)


# year of the dog  2030, 2018, 2006, 1994, 1982, 1970, 1958
years_of_dog <- seq(2030, 1900, by = -12)

# aries March 21 to about April 19
is_aries <- function(month, day){
  (month == 3 & day %in% 21:31) | (month == 4 & day %in% 1:19)
}

# Jeremy's zip should be the same
neighborhood <-
  filter(customers, name == 'Jeremy Davis') |>
  pull(citystatezip)

# brent nyugen
contractor <-
  collect(customers) |>
  filter(
    year(birthdate) %in% !!years_of_dog,
    is_aries(month(birthdate), day(birthdate)),
    citystatezip == neighborhood
  ) |>
  glimpse()

# Brent Nyugen is the contractor's neighbour

rm(years_of_dog, is_aries, neighborhood)



## Puzzle 4 ----------------------------------------------------

#' The investigator called the phone number you found and left a message, and a man soon called back: “Wow, that was years ago! It was quite an elegant tapestry.

#' “It took a lot of patience, but I did manage to get the dirt out of one section, which uncovered a superb owl. I put it up on my wall, and sometimes at night I swear I could hear the owl hooting.

# “A **few weeks later** my bike chain broke on the way home, and I needed to get it fixed before work the next day. Thankfully, this woman I met on Tinder came over at 5am with her bike chain repair kit and some pastries from Noah’s. Apparently she liked to get up before dawn and **claim the first pastries that came out of the oven**.

#' “I didn’t have any money or I would’ve paid her for her trouble. She really liked the tapestry, though, so I wound up giving it to her. “I don’t remember her name or anything else about her.”

# Can you find the bicycle fixer’s phone number?

### Clues
# - came over at 5am with pastries
# - a few weeks later
# - first order of the day?

# neighbor's order was 2017-08-12...
neighbor_order <-
  orders |>
  filter(customerid == !!contractor$customerid) |>
  collect() |>
  mutate(ordered = as_datetime(ordered)) |>
  head(1) |>
  glimpse()

pastry_sku <-
  collect(products) |>
  filter(sku %in% !!c('BKY0403', 'BKY0455')) |>
  pull(sku)

pastry_orders <-
  left_join(orders, orders_items, by = 'orderid') |>
  filter(sku %in% !!pastry_sku) |>
  collect() |>
  mutate(
    date = as_date(ordered),
    hour = hour(ordered),
    hour2 = hour(shipped)
  ) |>
  filter(ordered > as_datetime('2017-08-30'),
        hour > 3, hour < 5,
        hour2 > 3, hour2 < 5
  ) |>
  select(orderid, customerid, ordered, shipped) |>
  glimpse()

bike_woman <-
  pastry_orders |>
  left_join(
    customers |>
      mutate(customerid = as.character(customerid)) |>
      collect(),
    by = "customerid"
  ) |>
  distinct(name, phone) |>
  glimpse()

# bike woman is Christina Booker
rm(contractor, neighbor_order, pastry_orders, pastry_sku)




## Puzzle 5 ----------------------------------------------------

#' “Yes, I did have that tapestry for a little bit. I even cleaned a blotchy section that turned out to be a friendly koala.

#' “But it was still really dirty, so when I was going through a Marie Kondo phase, I decided it wasn’t sparking joy anymore.

#' “I listed it on Freecycle, and a woman in Queens Village came to pick it up. She was wearing a ‘Noah’s Market’ sweatshirt, and it was just covered in cat hair. When I suggested that a clowder of cats might ruin such a fine tapestry, she looked at me funny and said she only had ten or eleven cats and they were getting quite old and had cataracts now so they probably wouldn’t notice some old rug anyway.

#' “It took her 20 minutes to stuff the tapestry into some plastic bags she brought because it was raining. I spent the evening cleaning my apartment.”

#' What’s the phone number of the woman from Freecycle?


#' CLUES
#' - Queens Village
#' - ‘Noah’s Market’ sweatshirt
#' - Cat products (food, 'had cataracts now', ...)
#'  Cats were getting quite old ...


items <- tbl(con, 'orders_items') |>
  distinct(orderid, sku) |>
  mutate(orderid = as.character(orderid)) |>
  collect()

queens <-
  tbl(con, 'customers') |>
  collect() |>
  filter(str_detect(citystatezip, 'Queens Village')) |>
  mutate(customerid = as.character(customerid)) |>
  left_join(tbl(con, 'orders') |> collect(),
            by = 'customerid') |>
  filter(!is.na(orderid)) |>
  left_join(items, by = 'orderid')

queens_and_bought_cat_product <-
  tbl(con, 'products') |>
  collect() |>
  filter(
    str_detect(sku, 'PET'),
    str_detect(desc, 'Cat'),
    str_detect(desc, 'Food'),
    !sku == 'PET8066'
  ) |>
  select(sku) |>
  inner_join(queens, by = 'sku') |>
  glimpse() |>
  select(name, phone) |>
  count(name, phone) |>
  head(1) |>
  glimpse()


# anita koch. 315-492-7411

rm(queens, items)


## Puzzle 6 ----------------------------------------------------

# “It was a nice rug and they were surely going to ruin it, so I gave it to my cousin, who was moving into a new place that had wood floors.

# “She refused to buy a new rug for herself–she said they were way too expensive. She’s always been very frugal, and she clips every coupon and shops every sale at Noah’s Market. In fact I like to tease her that Noah actually loses money whenever she comes in the store.

# “I think she’s been taking it too far lately though. Once the subway fare increased, she stopped coming to visit me. And she’s really slow to respond to my texts. I hope she remembers to invite me to the family reunion next year.”

# Can you find her cousin’s phone number?

# her order total will be less that the total wholesale cost of the order..

neg_profit_orders <-
  left_join(orders_items, products, by = 'sku') |>
  mutate(profit = unit_price - wholesale_cost) |>
  group_by(orderid) |>
  summarise(profit = sum(profit, na.rm = T)) |>
  mutate(orderid = as.character(orderid)) |>
  left_join(orders, by = 'orderid') |>
  left_join(customers, by = 'customerid') |>
  arrange(profit) |>
  glimpse()

cheap_woman <-
  neg_profit_orders |>
  group_by(customerid, name, phone) |>
  summarise(ave_cost = mean(profit, na.rm = TRUE),
            .groups = 'drop') |>
  collect() |>
  arrange(ave_cost) |>
  head(1) |>
  glimpse()

rm(neg_profit_orders)

# cheap woman is Emily Randolph



## Puzzle 7 ----------------------------------------------------

#  “Oh that tapestry, with the colorful toucan on it! I’ll tell you what happened to it.
#
# “One day, I was at Noah’s Market, and I was just about to leave when someone behind me said ‘Miss! You dropped something!’
#
# “Well I turned around and sure enough this cute guy was holding something I had bought. He said ‘I got almost exactly the same thing!’ We laughed about it and wound up swapping items because he had wanted the color I got. We had a moment when our eyes met and my heart stopped for a second. I asked him to get some food with me and we spent the rest of the day together.
#
# “Before long I moved into his place. It didn’t last long though, as I soon discovered this man was not the gentleman I thought he was. I moved out only a few months later, late at night and in quite a hurry.
#
# “I realized the next day that I’d left that tapestry hanging on his wall. But the tapestry had come to represent our relationship, and I wanted nothing more to do with him, so I let it go. For all I know, he still has it.”
#
# Can you figure out her ex-boyfriend’s phone number?
#

# order placed at roughly same time as cheap woman...
# similar garment with different color?

# find all items emily bought
emilys_history <-
  orders |>
  filter(customerid == !!cheap_woman$customerid) |>
  mutate(orderid = as.integer(orderid)) |>
  left_join(orders_items, by = 'orderid') |>
  left_join(products, by = 'sku') |>
  select(-items, -qty, -matches('price|cost')) |>
  collect() |>
  mutate(date = as_date(ordered),
         without_color = str_remove(desc, "\\s\\([a-z]+\\)")) |>
  glimpse()

# items emily bought that have a color in the name
emily_colored_purchases <-
  emilys_history |>
  filter(str_detect(desc, "\\([a-z]+\\)")) |>
  select(date, desc, without_color) |>
  glimpse()

# items names without color that target may have bought
regx <- paste0(emily_colored_purchases$without_color, collapse = '|')

# products that the guy might have bought
candidate_products <-
  products |>
  collect() |>
  filter(str_detect(desc, regx),
         str_detect(desc, '\\(.*?\\)')) |>
  glimpse()

# find all the orders on same day as emilys days
orders |>
  left_join(orders_items, by = "orderid") |>
  collect() |>
  mutate(date = as_date(ordered)) |>
  filter(date %in% emily_colored_purchases$date,
         sku %in% candidate_products$sku) |>
  left_join(products |> collect()) |>
  left_join(customers |>
              mutate(customerid = as.character(customerid)) |>
              collect()
            ) |>
  select(name, phone, ordered, desc) |>
  print(n  = 200)

# johnathan adams
# 315-618-5263




## Puzzle 8 ------------------------------------------------

# gave it to my sister who lives in Manhattan
#
# owns an entire set of Noah’s collectibles!

collectibles <-
  products |>
  collect() |>
  filter(str_detect(desc, 'Noah')) |>
  glimpse()

most_collectibles <-
  orders |>
  left_join(orders_items, by = 'orderid') |>
  filter(sku %in% !!collectibles$sku) |>
  select(customerid, sku) |>
  collect() |>
  left_join(collectibles, by = 'sku') |>
  group_by(customerid) |>
  mutate(n = n()) |>
  ungroup() |>
  arrange(desc(n), customerid) |>
  slice(1) |>
  distinct(customerid) |>
  pull()

customers |>
  filter(customerid == !!as.integer(most_collectibles)) |>
  distinct() |>
  glimpse()
