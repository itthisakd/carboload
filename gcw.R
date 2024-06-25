library(dplyr)
library(ggplot2)
library(tidyr)
library(missForest)
library(data.table)
library(knitr)
library(stringi)
library(gt)
library(scatterplot3d)
library(foreach)
library(doParallel)

pprint = function(txt){
  print(paste(">>>>",deparse(substitute(txt))))
  print(txt)
}

setwd("~/Desktop/ucl/y3-20232024/41MKT/mkt-gcw/")

#--------LOAD DATA---------------

products_raw = read.csv("carboload/product_lookup.csv")
transactions = read.csv("carboload/transactions.csv")
causal = read.csv("carboload/causal_lookup.csv")

#--------CLEAN DATA---------------

# transactions
transactions = transactions %>%
  mutate(unit_price = dollar_sales / units)

# products, product_size requires cleaning
standardise_size = function(original_size){
  size = toupper(original_size)
  size = gsub("\\s+", "", size)
  size = gsub("OUNCE", "OZ", size)
  size = gsub("1/2", ".5", size)
  size = gsub("FLOZ", "OZ", size)
  size = gsub("Z", "OZ", size)
  size = gsub("OOZ", "OZ", size)
  
  if (!grepl("OZ|LB", size, ignore.case = TRUE)) {
    size = NA
  }
  
  pounds_match = regmatches(size, regexpr("\\d*\\.?\\d+(?=LB)", size, perl=TRUE))
  pounds = if(length(pounds_match)> 0) as.numeric(pounds_match) else 0
  ounces_match = regmatches(size, regexpr("\\d*\\.?\\d+(?=OZ)", size, perl=TRUE))
  ounces = if(length(ounces_match)> 0) as.numeric(ounces_match) else 0
  
  total_ounces = pounds*16+ounces
  
  if (total_ounces==0){
    return(NA)
  } else {
    return(total_ounces)
  }
}

# standardise product size to oz
products = products_raw
products$product_size = sapply(products_raw$product_size, standardise_size)

# verify the changes
results = c()
for (i in 1:nrow(products_raw)){
  results=append(results, paste(products_raw$product_size[i], "--->" ,products$product_size[i]))
}
print(unique(results))

# get upc with NA
upc_na <- products %>%
  filter(is.na(product_size)) %>%
  select(upc)

# remove NA from tables
products = products %>%
  filter(!is.na(product_size))

transactions = transactions %>%
  filter(!upc %in% upc_na) %>% 
  filter(dollar_sales > 0)

# remove NA from tables
products = products %>%
  filter(!is.na(product_size))

# merge tables
merged = inner_join(products, transactions, by = "upc")

merged_pasta = merged %>% 
  filter(commodity=="pasta") 
merged_sauce = merged %>% 
  filter(commodity=="pasta sauce") 

# count NAs
print(sum(is.na(products$product_size)))
print(sum(is.na(merged$product_size)))

write.csv(products, "products_cleaned.csv", row.names = FALSE)
write.csv(transactions, "transactions_cleaned.csv", row.names = FALSE)

#--------SIMPLE VISUALISATIONS DATA---------------

baskets = merged %>%
  filter(commodity %in% c("pasta", "pasta sauce")) %>% 
  group_by(basket) %>%
  summarise(pasta_count = sum(commodity == "pasta"),
            sauce_count = sum(commodity == "pasta sauce")) 

baskets 



baskets_labelled = baskets %>%
  mutate(contains = case_when(
    pasta_count == 0 & sauce_count > 0 ~ "sauce only",
    pasta_count > 0 & sauce_count == 0 ~ "pasta only",
    pasta_count > 0 & sauce_count > 0 ~ "both pasta and sauce",
    TRUE ~ NA_character_  # Handle other cases if needed
  )) 

baskets_labelled %>% 
  ggplot(aes(x = contains, fill = contains)) +
  geom_bar() +
  labs(title = "Types of Baskets",
       x = "Contains",
       y = "Count") 



baskets_sauce =  baskets_labelled%>% 
  filter(sauce_count>0) 

baskets_pasta =  baskets_labelled%>% 
  filter(pasta_count>0) 

basket_prop_sauce = baskets_sauce %>%  
  group_by(contains) %>% 
  summarise(prop=n()/nrow(baskets_sauce))

basket_prop_pasta = baskets_pasta %>%  
  group_by(contains) %>% 
  summarise(prop=n()/nrow(baskets_pasta))

colnames(basket_prop_sauce) = c("Sauce Baskets that Contain", "Proportion")
gt(basket_prop_sauce) 

colnames(basket_prop_pasta) = c("Pasta Baskets that Contain", "Proportion")
gt(basket_prop_pasta) 

# Number of products by commodity
# => pasta had the most product offergins
products %>%
  group_by(commodity) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = commodity, y = count, fill = commodity)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Products by Commodity Type", x = "Commodity", y = "Count")

# number of transactions by commodity
# => pasta had the most transactions
merged %>%
  group_by(commodity) %>%
  summarise(total_unit_sales = sum(units)) %>%
  ggplot(aes(x = commodity, y = total_unit_sales, fill = commodity)) +
  geom_bar(stat = "identity") +
  labs(title = "Unit Sold by Commodity Type", x = "Commodity", y = "Unit Sales")

merged %>%
  group_by(commodity) %>%
  summarise(num_trans = n()) %>%
  ggplot(aes(x = commodity, y = num_trans, fill = commodity)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Transactions by Commodity Type", x = "Commodity", y = "Number of Transactions")

# total sales by commodity
# => pasta had the highest total dollar sales
merged %>%
  group_by(commodity) %>%
  summarise(total_dollar_sales = sum(dollar_sales)) %>%
  ggplot(aes(x = commodity, y = total_dollar_sales, fill = commodity)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Dollar Sales by Commodity Type", x = "Commodity", y = "Total Dollar Sales")

# total_unit_sales by brands, for pasta
# => private label/barilla/creamette sold the most units
merged %>%
  filter(commodity == "pasta") %>%
  group_by(brand) %>%
  summarise(total_unit_sales = sum(units)) %>%
  arrange(desc(total_unit_sales)) %>% 
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(brand, -total_unit_sales), y = total_unit_sales, fill = brand)) +
  geom_bar(stat = "identity") +
  labs(title = "total_unit_sales by Brand for Pasta (top 20)", x = "Brand", y = "total_unit_sales")

# total_unit_sales by brands, for pasta sauce
# => ragu/prego/privatelabel sold the most units
merged %>%
  filter(commodity == "pasta sauce") %>%
  group_by(brand) %>%
  summarise(total_unit_sales = sum(units)) %>%
  arrange(desc(total_unit_sales)) %>% 
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(brand, -total_unit_sales), y = total_unit_sales, fill = brand)) +
  geom_bar(stat = "identity") +
  labs(title = "total_unit_sales by Brand for Pasta Sauces (top 20)", x = "Brand", y = "total_unit_sales")

# ------ exploring product sizes for pasta
plot_product_size_offered = function(commodity_type){
  products %>% 
    filter(commodity == commodity_type) %>%
    group_by(product_size) %>% 
    summarise(count=n()) %>%
    mutate(product_size = as.character(product_size)) %>% 
    ggplot(aes(x = product_size, y = count, fill = product_size)) +
    geom_bar(stat = "identity") 
}

plot_product_size_offered("pasta")
# => most common size = 16 oz

plot_product_size_offered("pasta sauce")
# => most common size = 26 oz

plot_product_size_offered("pancake mixes")
# => most common size = 32 oz

plot_product_size_offered("syrups")
# => most common size = 24 & 12 oz

# ----------- SALES ----------------------------------

merged %>% 
  filter(commodity=="pasta") %>% 
  group_by(week) %>% 
  summarise(unit_sales=sum(units)) %>% 
  ggplot(aes(week, unit_sales)) + geom_line()

merged %>% 
  filter(commodity=="pasta sauce") %>% 
  group_by(week) %>% 
  summarise(unit_sales=sum(units)) %>% 
  ggplot(aes(week, unit_sales)) + geom_line()

# -----------MARKET SHARE -------------------------------

get_top10_marketshare = function(commodity_type){
  result = merged %>%
    filter(commodity == commodity_type) %>%
    mutate(total_commodity_unit_sales = sum(units)) %>%
    group_by(brand, total_commodity_unit_sales) %>%
    summarise(brand_unit_sales = sum(units)) %>%
    mutate(brand_unit_sales_prop = brand_unit_sales / total_commodity_unit_sales) %>%
    arrange(desc(brand_unit_sales_prop)) 
  return(result[1:10,])
}

pasta_top10_marketshare = get_top10_marketshare("pasta")

sauce_top10_marketshare = get_top10_marketshare("pasta sauce")

table = data.frame(sauce_top10_marketshare %>% 
             select(brand, brand_unit_sales_prop) %>% 
             rename(unit_sales_prop = brand_unit_sales_prop) %>% 
               mutate(unit_sales_prop=round(unit_sales_prop, 3)))
              
names(table) = c("Brand", "Proportion of Total Unit Sales")
gt(table)

pancake_top10_marketshare = get_top10_marketshare("pancake mixes")
pancake_top10_marketshare

syrup_top10_marketshare = get_top10_marketshare("syrups")
syrup_top10_marketshare

# -----------PRICES for pasta & sauce ----------

pasta_overall_median_price <- merged %>%
  filter(commodity == "pasta", product_size == 16, brand %in% pasta_top10_marketshare$brand) %>%  
  summarise(median_price = median(unit_price))

pasta_prices = merged %>%
  filter(commodity=="pasta", product_size==16, brand %in% pasta_top10_marketshare$brand) %>%  
  group_by(upc, brand) %>% 
  summarise(mean_price = mean(unit_price)) 

pasta_prices%>% 
  ggplot(aes(brand, mean_price)) +
  geom_point() +
  labs(title = "Unit Price of 16oz Pasta Products, by Brand", x = "Brand", y = "Unit Price") + 
  geom_hline(yintercept = pasta_overall_median_price$median_price, linetype = "dashed", color = "red") +
  annotate("text", x = 3, y = pasta_overall_median_price$median_price, 
           label = paste("Median price =", round(pasta_overall_median_price$median_price, 2)), vjust = -1) 

sauce_overall_median_price <- merged %>%
  filter(commodity == "pasta sauce", product_size == 26, brand %in% sauce_top10_marketshare$brand) %>%  
  summarise(median_price = median(unit_price))

sauce_prices = merged %>%
  filter(commodity=="pasta sauce", product_size==26, brand %in% sauce_top10_marketshare$brand) %>%  
  group_by(upc, brand) %>% 
  summarise(mean_price = mean(unit_price)) 

sauce_prices%>% 
  ggplot(aes(brand, mean_price)) +
  geom_point() +
  labs(title = "Unit Price of 26oz Pasta Sauce Products, by Brand", x = "Brand", y = "Unit Price") + 
  geom_hline(yintercept = sauce_overall_median_price$median_price, linetype = "dashed", color = "red") +
  annotate("text", x = 2, y = sauce_overall_median_price$median_price, 
           label = paste("Median price =", round(sauce_overall_median_price$median_price, 2)), vjust = -1) 


sauce_prices%>% 
  ggplot(aes(brand, mean_price)) +
  geom_point() +
  labs(title = "Unit Price of 26oz Pasta Sauce Products, by Brand", x = "Brand", y = "Unit Price") + 
  geom_hline(yintercept = sauce_overall_median_price$median_price, linetype = "dashed", color = "red") +
  annotate("text", x = 2, y = sauce_overall_median_price$median_price, 
           label = paste("Median price =", round(sauce_overall_median_price$median_price, 2)), vjust = -1) 



# ----------- ELASTICITY ----------------------------------

get_elas_matrix = function(agg_df){

  brands = unique(agg_df$brand)
  elasticity_matrix = matrix(NA, nrow=length(brands), ncol=length(brands), dimnames=list(brands, brands))
  
  for (i in seq_along(brands)){
    rest_products_weekly = agg_df %>% 
      group_by(week, brand) %>% 
      summarise(weekly_price=mean(price)) %>% 
      pivot_wider(names_from = brand, values_from = weekly_price) 
    
    this_product_weekly= agg_df %>%
      filter(brand==brands[i]) %>% 
      select(-price)
    
    all_products_weekly_full = full_join(this_product_weekly, rest_products_weekly, by="week")
    
    # find self elastiticites
    
    col_names = colnames(all_products_weekly_full)[4:ncol(all_products_weekly_full)]
    covariates = paste0("log(", col_names, ")")
    
    formula = paste0("log(quantity)", "~", paste0(covariates, collapse=" + "))
    
    reg = lm(formula=formula, data=all_products_weekly_full)
    
    coeffs= reg$coefficients[2:length(reg$coefficients)]
    names(coeffs) = col_names
    
    #column name is the brand that discount is implemented on
    # each column is one set of coeffs
    elasticity_matrix[, brands[i]] = coeffs
  }
  
  return (elasticity_matrix)
  
}

get_self_elas = function(matrix){
  
  self_elasticities = c()
  brands = colnames(matrix)
  for (i in seq_along(brands)){
    self_elasticities=append(self_elasticities, matrix[brands[i], brands[i]])
    
  }
  return (data.frame(
    brand=brands, 
    self_elas=self_elasticities
  ))
}

get_cost = function(df){
  df = df %>% 
    mutate(prop_cost=as.numeric((as.numeric(self_elas)+1)/as.numeric(self_elas)))
  return(df)
}

# ------- AGGREATED ELASTICITY--------------------


# -----STEP 1-----------
sauce_agg = merged_sauce %>%
  group_by(week, brand) %>%
  summarise(
    quantity = sum(units), 
    price = weighted.mean(unit_price, units)  
  ) %>% 
  filter(brand %in% sauce_top10_marketshare$brand[1:4]) %>%
  mutate(brand = gsub(" ", "_", toupper(brand))) %>%
  mutate(brand = gsub("'", "", toupper(brand))) 

table = data.frame(sauce_agg)[1:10,]
colnames(table) = c("Week", "Brand", "Weekly Quantity", "Weekly Price")
gt(table)

# -----STEP 2-----------
# mean of last 10 weeks
sauce_markup=0.2

sauce_test_week = sauce_agg %>%
  filter(week>90) %>% 
  group_by(brand) %>% 
  summarise(baseline_price = mean(price), baseline_quantity=mean(quantity)) %>% 
  arrange(desc(baseline_quantity)) %>% 
  mutate(cost=baseline_price/(1+sauce_markup)) %>% 
  arrange(brand)

table =  sauce_agg %>%
  filter(week>90) %>% 
  group_by(brand) %>% 
  summarise(baseline_price = mean(price), baseline_quantity=mean(quantity)) 
colnames(table) = c("Brand", "Baseline Price", "Baseline Quantity")
gt(table)


table = sauce_test_week %>% select(-baseline_quantity)
colnames(table) = c("Brand", "Baseline Price", "Cost")
gt(table)

sauce_agg %>% 
  ggplot(aes(x = week, y = quantity, color = brand, group = brand)) +
  geom_line() +
  labs(title = "Quantity Sold of Pasta Sauce Over Time, by Brand",
       x = "Week",
       y = "Quantity")

sauce_agg %>% 
  ggplot(aes(x = week, y = price, color = brand, group = brand)) +
  geom_line() +
  labs(title = "Price of Pasta Sauce Over Time, by Brand",
       x = "Week",
       y = "Price")

# -----STEP 3-----------

sauce_elas_matrix = data.frame(get_elas_matrix(sauce_agg))
sauce_elas_matrix$brand = rownames(sauce_elas_matrix)

table = data.frame(sauce_elas_matrix)
colnames(table) = paste0("with respect to ", colnames(table))
table = cbind(data.frame("Price Elasticity Of" = rownames(table)), table)
colnames(table)[1] = c("Price Elasticity Of:")

gt(table[,c(1,3)])

# ---------- STEP 4 ---------------

quantity_change = sauce_elas_matrix %>%  
  select(brand, BERTOLLI) %>% 
  rename(elas_wrt_BERTOLLI=BERTOLLI) %>% 
  inner_join(sauce_test_week, by="brand") %>% 
  select(-baseline_price) %>% 
  mutate(discount_amount = -0.1) %>% 
  mutate(expt_percent_quantity_change=discount_amount*elas_wrt_BERTOLLI) %>% 
  mutate(expt_unit_quantity_change=expt_percent_quantity_change*baseline_quantity) %>% 
  select(brand, elas_wrt_BERTOLLI, discount_amount, expt_percent_quantity_change, baseline_quantity, expt_unit_quantity_change)

table = quantity_change
colnames(table) = c("Brand", "Elasticity wrt BERTOLLI", "Discount Amount", "Expected Percent Change in Quantity ", "Baseline Quantity", "Expected Unit Change in Quantity ")
gt(table)


# ---------- STEP 5---------------
# 
sauce_self_elas = get_self_elas(sauce_elas_matrix %>%  select (-brand))

table = sauce_self_elas
colnames(table) = c("Brand", "Self-Elasticity")
gt(table)

# sauce_info = get_cost(sauce_info)

# cost = sauce_info %>% 
#   rename(prop_of_price_that_is_cost=prop_cost) %>% 
#   inner_join(data.frame(sauce_test_week %>% select(brand, baseline_price)), by="brand") %>% 
#   mutate(unit_cost=prop_of_price_that_is_cost*baseline_price)

# -------------STEP 6 ----------------


discounted_sauce_test_week = sauce_test_week

interest_idx =2

discounted_sauce_test_week[interest_idx, "baseline_price"] = discounted_sauce_test_week[interest_idx, "baseline_price"]*0.9

revenue_change = discounted_sauce_test_week %>% 
  select(brand, baseline_price) %>% 
  rename(test_price=baseline_price) %>% 
  inner_join(data.frame(quantity_change %>% select(brand, expt_unit_quantity_change)), by="brand") %>% 
  mutate(expt_revenue_change = expt_unit_quantity_change*test_price) 

revenue_change%>% 
  gt()

cost_change = sauce_test_week %>% 
  select(brand, cost) %>% 
  inner_join(data.frame(quantity_change %>% select(brand, expt_unit_quantity_change)), by="brand") %>% 
  mutate(expt_cost_change = expt_unit_quantity_change*cost)

changes = revenue_change %>% 
  inner_join(cost_change, by="brand") %>% 
  select(brand, test_price, cost, expt_unit_quantity_change.x, expt_revenue_change, expt_cost_change)
  
table = changes %>% arrange(brand)
colnames(table) = c("Brand", 
                    "Unit Price", 
                    "Unit Cost", 
                    "Expected Unit Quantity Change",
                    "Expected Revenue Change",
                    "Expected Cost Change")
gt(table)


table = revenue_change %>% 
  select(brand, expt_revenue_change) %>% 
  inner_join(data.frame(cost_change %>% select(brand, expt_cost_change))) %>% 
  summarise(
    discounted_brand = "BERTOLLI",
    discount_amount = -0.1,
    total_revenue_change = sum(expt_revenue_change),
    total_cost_change = sum(expt_cost_change),
    net_contribution_change = total_revenue_change-total_cost_change
            )

colnames(table) = c("Discounted Brand",
                    "Discount Amount", 
                    "Total Revenue Change", 
                    "Total Cost Change", 
                    "Net Change in Contribution"
                    )
gt(table)

# -----------------------------

calc_net_cont_change = function(brand_idx, discount_percent, elas_matrix, test_week){
  
  i = brand_idx 
  # pprint(colnames(elas_matrix)[i])
  
  elas_matrix = elas_matrix %>%  select(-brand)
  
  
  percent_change = discount_percent*elas_matrix[,i]
  
  # pprint(percent_change)
  
  changes = data.frame(brand = rownames(elas_matrix), percent_change=percent_change) %>% 
    inner_join(test_week, by="brand") %>% 
    mutate(
      quantity_change = percent_change*baseline_quantity
    ) 
  
  # pprint(changes)
  
  dollar_discount = changes[i, "baseline_price"]*discount_percent
  
  # pprint(dollar_discount)
  
  changes[i, "baseline_price"] = changes[i, "baseline_price"] + dollar_discount
  
  # pprint(changes)
  
  changes = changes %>% 
    mutate(
      revenue_change=baseline_price*quantity_change,
      cost_change=cost*quantity_change
      ) 
  
  pprint(changes)
  
  result = changes %>% 
    summarise(
      total_revenue_change = sum(revenue_change),
      total_cost_change = sum(cost_change),
      net_contribution_change = total_revenue_change-total_cost_change
    )
  
  pprint(result)
  
  return(result[1,3])
}

calc_net_cont_change(
  brand_idx = 2,
  discount_percent = -0.1,
  elas_matrix = sauce_elas_matrix,
  test_week = sauce_test_week
)

sauce_test_week$brand
test_discount_across_brands = function(discount_percent, elas_matrix, test_week){
  
  brands = sauce_test_week$brand
  net_cont_change = c()
  
  for (i in seq_along(brands)){
    
    net_cont_change = append(net_cont_change, 
                             calc_net_cont_change(
                               brand_idx = i,
                               discount_percent = discount_percent,
                               elas_matrix = elas_matrix,
                               test_week = test_week
                             )
                             )

  }
  
  result = data.frame(
    brand=brands, 
    net_contribution_change = net_cont_change,
    discount = rep(discount_percent, length(brands))
  )
  
  return(result)
}

test_discount_across_brands(
  discount_percent = -0.07,
  elas_matrix = sauce_elas_matrix,
  test_week = sauce_test_week
)

find_optimal_discount = function() {
  compiled = data.frame(brand=c(), net_contribution_change=c(), discount=c())
  
  for (n in seq(-1, 0, by = 0.01)){
    results = test_discount_across_brands(
      discount_percent = n,
      elas_matrix = sauce_elas_matrix,
      test_week = sauce_test_week
    )
    compiled = rbind(compiled, results)
  }
  return(compiled)
}

discount_results = find_optimal_discount()

discount_results %>% 
  filter(brand=="BERTOLLI")


gt(
  data.frame(
    discount_results  %>% 
      arrange(desc(net_contribution_change)) %>% 
      head(10) %>% 
      select(brand, discount, net_contribution_change)
  )
)

optimal_disc = discount_results  %>% 
  group_by(brand) %>% 
  filter(net_contribution_change==max(net_contribution_change)) %>% 
  arrange(desc(net_contribution_change)) %>% 
  head(10) %>% 
  select(brand, discount, net_contribution_change) %>% 
  rename(optimal_discount=discount)

table = data.frame(
  optimal_disc
)
colnames(table) = c("Brand", "Optimal Discount", "Net Contribution Change")
gt(
  table
)

discount_results  %>% 
  filter(net_contribution_change>0) %>%
  ggplot(aes(-discount, net_contribution_change, color=brand)) +
  geom_point() +
  labs(title = "Net Increase in Contribtion as a result of a Discount, by Brand",
       x = "Discount Amount",
       y = "Net Contribution Increase")

table = sauce_test_week %>% 
  mutate(dollar_markup = baseline_price-cost) %>%  
  inner_join(sauce_self_elas, by="brand") %>% 
  inner_join(optimal_disc, by="brand") %>% 
  inner_join(sauce_top10_marketshare %>%
               mutate(brand = gsub(" ", "_", toupper(brand))) %>%
               mutate(brand = gsub("'", "", toupper(brand))), by="brand") %>% 
  select(brand, net_contribution_change, brand_unit_sales_prop, dollar_markup, self_elas) %>% 
  arrange(desc(net_contribution_change))

sauce_elas_matrix

colnames(table) = c("Brand", "Net Contribution Change", "Market Share (by units)", "Dollar Markup", "Self-Elasticity")
  
gt(table)

# ---------------------------------------------------

sauce_subset = merged_sauce %>% 
  filter(brand %in% sauce_top10_marketshare$brand[1:5]) %>%
  mutate(brand = gsub(" ", "_", toupper(brand))) %>%
  mutate(brand = gsub("'", "", toupper(brand))) 

sauce_subset %>% 
  group_by(brand, upc) %>% 
  summarise(sales = sum(units)) %>% 
  group_by(brand) %>% 
  mutate(brand_share=sales/sum(sales), market_share =sales/sum(sauce_subset$units)) %>% 
  filter(brand_share==max(brand_share))

best_selling = merged_sauce %>% 
  filter(brand %in% sauce_top10_marketshare$brand[1:5]) %>%
  mutate(brand = gsub(" ", "_", toupper(brand))) %>%
  mutate(brand = gsub("'", "", toupper(brand))) %>% 
  group_by(upc, brand) %>% 
  summarise(sales = sum(units)) %>% 
  group_by(brand) %>% 
  filter(sales == max(sales))
  
sauce_bestsell = merged_sauce %>% 
  filter(brand %in% sauce_top10_marketshare$brand[1:5]) %>%
  mutate(brand = gsub(" ", "_", toupper(brand))) %>%
  mutate(brand = gsub("'", "", toupper(brand))) %>% 
  filter(upc %in% best_selling$upc)

nrow(sauce_bestsell)

nrow(merged_sauce %>% 
       filter(brand %in% sauce_top10_marketshare$brand[1:5]))

sauce_prices = sauce_bestsell %>%
  group_by(day, brand) %>%
  summarise(
    quantity = sum(units), 
    price = weighted.mean(unit_price, units)  
  ) 

sauce_pivot = sauce_prices %>% 
  pivot_wider(id_cols = day, names_from = brand, values_from = price)

sauce_hh = sauce_bestsell %>% 
  select(household, brand, day, units)

# **** SHOULD I REPEAT ROWS IF THERE ARE MANY UNITS ? ********
sauce_choice_df = sauce_hh %>% 
  inner_join(sauce_pivot, by="day") %>% 
  rename(choice=brand, hhid=household) %>% 
  select(-day, -units) %>% 
  mutate(chid = row_number()) %>% 
  drop_na()



hh_sample= sample(unique(sauce_choice_df$hhid) , 10000, replace = FALSE)

sauce_choice_subset = sauce_choice_df %>% 
  filter(hhid %in% hh_sample)

nrow(sauce_choice_subset)

# ------ TRY ESTIMATING INDIVIDUAL PRICE PREFERENCES FOR SAUCE ------

library(rlang) # for expression manipulation
library(cluster) # for pam clustering
library(data.table)
library(ggplot2)
library(knitr)
library(mlogit)
library(stringi)
library(gt)

sauce_choice = as.data.table(sauce_choice_df)
product_names = colnames(sauce_choice)[3:7]

frequent_customers = sauce_choice[, .N, by=hhid][N >= 5, hhid]
sauce_choice = sauce_choice[hhid %in% frequent_customers]
sauce_choice[, chid := seq_len(.N)]
sauce_choice[, t(sort(prop.table(table(choice)), decreasing=TRUE))]


product_sales = sauce_choice[, .N, by=choice]
# first melt the data to long format so that each row is an alternative's feature value
# extract the product id and the feature name from each row
# then cast the melted data where the features are made into columns
setnames(sauce_choice, product_sales$choice, stri_paste("price", product_sales$choice, sep="_"))
sauce_long = melt(sauce_choice, id.vars=c("hhid", "chid", "choice"))
sauce_long[
  ,
  c("feature", "product") := as.data.table(stri_split_fixed(variable, "_", n=2, simplify=TRUE))
][, brand := stri_split_fixed(product, "_", n=2, simplify=TRUE)[, 1]]
sauce_long = dcast(sauce_long, chid + hhid + brand + choice + product ~ feature, value.var="value")
sauce_long[, choice := (choice == product)]
brand_names = product_sales[, stri_split_fixed(choice, "_", n=2, simplify=TRUE)[, 1]]
base_brand = brand_names[1]
brand_dummy_names = brand_names[brand_names != base_brand]
sauce_long[, c(brand_dummy_names) :=
                 lapply(brand_dummy_names, function(x, y) as.integer(x == y), brand)][, brand := NULL]
gt(head(sauce_long)) |>
  tab_options(table.font.size=pct(90)) |>
  opt_horizontal_padding(scale=3)

sauce_long_idx = dfidx(
  as.data.frame(sauce_long),
  idx=list(c("chid", "hhid"), "product"), format="long", opposite="price"
)
model_formula = as.formula(
  stri_paste("choice", "~", stri_paste(c(0, "price", brand_dummy_names), collapse= " + "), sep=" ")
)
model_distributions = rep("n", length(brand_dummy_names) + 1)
names(model_distributions) = c("price", brand_dummy_names)
model_distributions["price"] = "ln"


mixed_logit = mlogit(model_formula, sauce_long_idx, panel=TRUE, R=100, rpar=model_distributions)

mlogit_results = mixed_logit





ggplot(data.table(x=-fitted(mixed_logit, type="parameters")[, "price"]), aes(x)) +
  geom_density() +
  xlab("Price Coefficient")

#' Calculate the choice probability before normalization, that is,
#' the numerator of the choice probability.
#' @param data a data table of product features
#' @param coef_dt a data table of individual preference parameters
#' @param hhid the household id used to retrieve preference parameter
#' @param features a vector of feature names used for column matching
#' @return a vector of un-normalized choice probabilities
mnl_scores = function(data, coef_dt, hhid, features) {
  X = as.matrix(data[, features, with=FALSE])
  beta = unlist(coef_dt[.(hhid), ..features, on="id"])
  return(as.vector(exp(X %*% beta)))
}


# obtain posterior mean individual price coefficient
price_coefs = as.data.table(fitted(mixed_logit, type="parameters"), key="id")
price_coefs[, price := -price]
features = c("price", brand_dummy_names)

#' Calculate the total profit of a product from each consumer
#' @param data a long data table of choice sets and choices
#' @param coef_dt a data table of individual preference parameters
#' @param features a vector of feature names used for column matching
#' @param focus the product id of interest
#' @param mc the marginal cost of the product
#' @param discount the discount rate on the focus product
#' @param profit_name the name of the profit column
#' @return a data table of profits where each row is the focus product's profit from a consumer
get_profits = function(data, coef_dt, features, focus, mc, discount, profit_name="profit") {
  data = copy(data)
  setindex(data, product)
  data[.(focus), price := price * (1 - discount), on="product"]
  result = data[
    ,
    .(chid, product, price, prob=mnl_scores(.SD, coef_dt, hhid[1], features)), # un-normalized choice probability
    by="hhid"][
      ,
      prob := prob / sum(prob),
      by="chid"
    ][
      product == focus,
      .(profit=sum((price - mc) * prob), prob=mean(prob)),
      by="hhid"
    ]
  setnames(result, "profit", profit_name)
  return(result)
}

mc = 0.5

profit = get_profits(
  sauce_long, price_coefs, features, "RAGU", mc, 0.2, "profit_base"
)
profit_coupon = get_profits(
  sauce_long, price_coefs, features, "RAGU", mc, 0.2, "profit_coupon"
)
profit = profit[profit_coupon[, .(hhid, profit_coupon)], on="hhid"][, !c("prob")]

gt(head(profit)) |> tab_options(table.font.size=pct(90)) |>
  fmt_number(columns=c(profit_base, profit_coupon), decimals=2) |>
  opt_horizontal_padding(scale=3)


#--------- RFM --------------

# Function to generate RFM
get_rfm = function(data, time_window=90){
  last_day = max(data$day)
  first_day = last_day-time_window
  
  data_subset = data %>%  
    filter(day>first_day) 
  
  # recency
  recency_df = data_subset %>% 
    group_by(household) %>% 
    summarise(recency=last_day-max(day))
  
  # frequency
  frequency_df = data_subset %>% 
    group_by(household) %>% 
    summarise(frequency=n())
  
  # monetary
  monetary_df = data_subset %>% 
    group_by(household) %>% 
    summarise(monetary=sum(dollar_sales))
  
  rfm_df = recency_df %>%  
    inner_join(frequency_df, by="household") %>% 
    inner_join(monetary_df, by="household")
  
  return(rfm_df)
}

# Function to generate RFM scores
get_rfm_scores = function(rfm_df) {
  
  # Calculate quartiles for Recency, Frequency, and Monetary
  recency_quartiles <- quantile(rfm_df$recency, probs = c(0,0.5, 1))
  frequency_quartiles <- quantile(rfm_df$frequency, probs = c(0, 0.5, 1))
  monetary_quartiles <- quantile(rfm_df$monetary, probs = c(0, 0.5,1))
  recency_quartiles[1]=-1
  frequency_quartiles[1]=-1
  monetary_quartiles[1]=0
  
  # Assign quartile labels to Recency, Frequency, and Monetary
  rfm_df$r_score <- cut(rfm_df$recency, breaks = recency_quartiles, labels = c(2, 1))
  rfm_df$f_score <- cut(rfm_df$frequency, breaks = frequency_quartiles, labels = c(1, 2))
  rfm_df$m_score <- cut(rfm_df$monetary, breaks = monetary_quartiles, labels = c(1, 2))
  
  # Combine RFM scores into a single numeric value
  rfm_df$rfm_score <- as.numeric(paste0(rfm_df$r_score, rfm_df$f_score, rfm_df$m_score))
  
  return(rfm_df)
}

# --------  RFM Scores ---------

sauce_rfm_scores=get_rfm_scores(get_rfm(sauce_subset, time_window = 60))







# create one-hot encodings for each segment
km3_segment_levels = sauce_demos[, sort(unique(k3))]
km3_segment_names = stri_paste("km3", km3_segment_levels, sep=".")
sauce_demos[, (km3_segment_names) := lapply(km3_segment_levels, `==`, k3)]
pam3_segment_levels = sauce_demos[, sort(unique(pam3))]
pam3_segment_names = stri_paste("pam3", pam3_segment_levels, sep=".")
sauce_demos[, (pam3_segment_names) := lapply(pam3_segment_levels, `==`, pam3)]
sauce_demos[, `:=`(low_income = Income < mean(Income), small_fam = Fam_Size < mean(Fam_Size))]

sauce_rfm = sauce_rfm_scores %>% 
  select(household, rfm_score) %>% 
  mutate(has_score = TRUE) %>%
  pivot_wider(
    id_cols = household,
    names_from = rfm_score,
    values_from = has_score,
    values_fill = list(has_score = FALSE)
  ) %>%
  replace(is.na(.), FALSE) %>% 
  rename(hhid=household)


# get profit for each segment
targeting_profit = sauce_rfm[
  profit,
  lapply(.SD, function(x, y, z) sum(ifelse(x, y, z)), profit$profit_coupon, profit$profit_base),
  .SDcols=as.vector(unique(sauce_rfm_scores$rfm_score)),
  on="hhid"
]






library(data.table)

# Assuming sauce_rfm is a data.table and profit is a data.frame or data.table
setDT(sauce_rfm)
setDT(profit)

# Get unique RFM scores
unique_scores <- as.vector(unique(sauce_rfm$rfm_score))

# Initialize a list to store results
profit_per_segment <- list()

# Loop through unique RFM scores and calculate profit for each segment
for (score in unique_scores) {
  temp_df <- sauce_rfm[rfm_score == score, on = "household"]
  joined_data <- merge(temp_df, profit, by.x = "household", by.y = "hhid", all.x = TRUE)
  profit_per_segment[[as.character(score)]] <- sum(with(joined_data, ifelse(rfm_score == score, profit_base + profit_coupon, 0)))
}

# Display profits for each segment
print(profit_per_segment)
