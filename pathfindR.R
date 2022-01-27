#To write a table with the significative p-values
is.numeric(GSE84422$p_value)



#Download and load the pathfindr package
install.packages("pathfindR")
library(pathfindR)

pathwaysresults<-run_pathfindR(GSE121497, 
                               gene_sets = "KEGG",
                               min_gset_size = 3,
                               max_gset_size = 300,
                               custom_genes = NULL,
                               custom_descriptions = NULL,
                               pin_name_path = "Biogrid",
                               p_val_threshold = 0.05,
                               visualize_enriched_terms = TRUE,
                               max_to_plot = NULL,
                               convert2alias = TRUE,
                               enrichment_threshold = 0.05,
                               adj_method = "bonferroni",
                               search_method = "GR",
                               plot_enrichment_chart = TRUE,
                               output_dir = "pathfindR_Results",
                               list_active_snw_genes = TRUE,
                               silent_option = TRUE)

input_results<-input_processing(
  GSE121497,
  p_val_threshold = 0.05,
  pin_name_path = "Biogrid",
  convert2alias = TRUE
)

visualize_terms(pathwaysresults,
                input_processed = input_results,
                hsa_KEGG = FALSE,
                pin_name_path = "Biogrid")

# The table must be contain 2 or 3 columns (gene name, p value, and or fold change).
write.csv(pathwaysresults, "kegg_121497.csv")
enrichment_chart(pathwaysresults, top_terms = 20)
term_gene_graph(pathwaysresults, num_terms = 3,use_description = TRUE)
term_gene_heatmap(pathwaysresults, GSE121497, num_terms = 10, use_description = TRUE)
