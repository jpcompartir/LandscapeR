ls_plot_variation_matrix <- function(df, var1, var2){
  requireNamespace("viridis")

  plotting_table <- df %>%
    dplyr::add_count({{var1}}, name = "var1_n") %>%
    dplyr::add_count({{var2}}, name = "var2_n") %>%
    dplyr::add_count({{var1}}, {{var2}}, name = "var1_var2_n") %>%
    dplyr::mutate(percentage_plot = var1_var2_n/var1_n * 100) %>%
    dplyr::select({{var1}}, {{var2}}, var1_n, var2_n, var1_var2_n, percentage_plot) %>%
    dplyr::distinct({{var1}}, {{var2}}, var1_var2_n, .keep_all = TRUE)

  browser()
}


# product_topic_summary_table <- df_topics %>%
#   add_count(product, topic, name = "product_topic_n") %>%
#   add_count(product, name = "product_n") %>%
#   add_count(topic, name = "topic_n") %>%
#   select(contains('topic'), contains('product'), - topic_probability) %>%
#   mutate(percentage = product_topic_n/ product_n * 100) %>%
#   distinct(topic, product_topic_n, product, .keep_all = TRUE) %>%
#   relocate(product, topic, product_topic_n, percentage, topic_n, product_n)
#
# product_topic_summary_table %>%
#   write_csv('data/ari_files/kyle_product_topic_summary_percentages.csv')
# library(viridis)
# product_topic_summary_table %>%
#   ggplot(aes(x= product, y = topic, fill = percentage)) +
#   geom_tile(color = "white") +
#   theme_minimal() +
#   labs(x = "Product", y = "Topic",
#        fill = "% of product mentions by topic",
#        title = "Microsoft - 576 Edge Browser - Product & Topic Variation Matrix") +
#   scale_fill_viridis(discrete = FALSE) +
#   theme(legend.position = "bottom")
