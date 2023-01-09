# Read in the data
orders <- read.csv("https://raw.githubusercontent.com/sjsimmo2/DataMining-Fall/master/orderData.csv")

# Create a unique order id for each customer (not just order and seat)
orders$id <- paste0(orders$orderNo, orders$seatNo)

# Create a df with just the meats and wines (since that's all we really need)
orders_new <- orders %>%
  filter(as.numeric(rownames(orders)) %% 3 != 0) %>%
  dplyr::select(item, id)

# Create a df that has the meat and wine frequencies
freqs <- orders_new %>%
  group_by(item) %>%
  summarise(n = n())

# Create a df with just the meat frequencies
meats <- c("Duck Breast", "Filet Mignon", "Pork Chop", "Pork Tenderloin", 
           "Roast Chicken", "Salmon", "Sea Bass", "Swordfish")
meat_freqs <- freqs %>%
  filter(item %in% meats)

# Create a df with just the wine frequencies
wine_freqs <- freqs %>%
  filter(!(item %in% meats))

# Visualize the meat and wine frequencies
ggplot(meat_freqs, aes(x = reorder(item, -n), y = n)) + 
  geom_bar(stat = 'identity') + 
  labs(title = 'Frequency of Meat Ordered', 
       x = 'Meat', 
       y = 'Order Frequency')
# This answers the first bullet point - summary information on the entrees

ggplot(wine_freqs, aes(x = reorder(item, -n), y = n)) + 
  geom_bar(stat = 'identity') + 
  labs(title = 'Frequency of Wine Ordered', 
       x = 'Wine', 
       y = 'Order Frequency')

# Create a transaction object
trans <- as(split(orders_new$item, orders_new$id), "transactions")

# Create the object as a data frame as well for readability
trans_df <- DATAFRAME(trans)

# Get the rules
rules <- apriori(trans, 
                 parameter = list(supp = 0, conf = 0, target="rules"))
rules <- sort(rules, by="confidence", decreasing=TRUE)

# Create a df of the rules for readability
rules_df <- DATAFRAME(rules)

# Filter it down so that only meats appear on the LHS
rules_df_meats <- rules_df %>%
  filter(gsub('[^[:alnum:] ]','',LHS) %in% meats)

# Filter it down so that only the first (highest confidence) unique observation 
# of each meat appears on the LHS.
rules_df_meats_distinct <- rules_df %>%
  filter(gsub('[^[:alnum:] ]','',LHS) %in% meats) %>%
  distinct(LHS, .keep_all = T)
# This answers the second bullet point - wine suggestions for each entree

# From this point forward, we are answering the third bullet point - any other
# information of interest in terms of customer order habits

# Potential things to look at - association between entrees and sides, 
# association between types of meat (pork, seafood, etc.) and wines or types of
# wines

# Create a df with just the meats and sides
orders_sides <- orders %>%
  filter(as.numeric(rownames(orders)) %% 3 != 2) %>%
  dplyr::select(item, id)

# Create a df that has the meat and side frequencies
freqs2 <- orders_sides %>%
  group_by(item) %>%
  summarise(n = n())

# Create a df with just the wine frequencies
side_freqs <- freqs2 %>%
  filter(!(item %in% meats))

# Visualize the side frequencies
ggplot(side_freqs, aes(x = reorder(item, -n), y = n)) + 
  geom_bar(stat = 'identity') + 
  labs(title = 'Frequency of Side Ordered', 
       x = 'Side', 
       y = 'Order Frequency')

# Create a transaction object
trans_sides <- as(split(orders_sides$item, orders_sides$id), "transactions")

# Create the object as a data frame as well for readability
trans_sides_df <- DATAFRAME(trans_sides)

# Get the rules
rules2 <- apriori(trans_sides, 
                 parameter = list(supp = 0, conf = 0, target="rules"))
rules2 <- sort(rules2, by="confidence", decreasing=TRUE)

# Create a df of the rules for readability
rules_df2 <- DATAFRAME(rules2)

# Filter it down so that only meats appear on the LHS
rules_df_sides <- rules_df2 %>%
  filter(gsub('[^[:alnum:] ]','',LHS) %in% meats)

# Filter it down so that only the first (highest confidence) unique observation 
# of each meat appears on the LHS.
rules_df_sides_distinct <- rules_df2 %>%
  filter(gsub('[^[:alnum:] ]','',LHS) %in% meats) %>%
  distinct(LHS, .keep_all = T)

# Write out the relevant df's as csv's
write.csv(rules_df_meats_distinct, "wine_recs.csv")
write.csv(rules_df_sides_distinct, "side_recs.csv")

