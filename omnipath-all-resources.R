## ----message=FALSE, warning=FALSE---------------------------------------------
library(dplyr)
library(ggplot2)
library(OmnipathR)
library(igraph)
library(ggraph)

## -----------------------------------------------------------------------------
# Download protein-protein interactions
interactions = import_omnipath_interactions() %>% as_tibble()

# Convert to igraph objects:
OPI_g = interaction_graph(interactions = interactions )

library(dbparser)
library(XML)
#  
#  
parse data from XML and save it to memory
get_xml_db_rows("..path-to-DrugBank/full database.xml")
#  
load drugs data
drugs <- parse_drug() %>% select(primary_key, name)
drugs <- rename(drugs,drug_name = name)
#  load drug target data
drug_targets <- parse_drug_targets() %>%
select(id, name,organism,parent_key) %>%
rename(target_name = name)
  
#load polypeptide data
drug_peptides <- parse_drug_targets_polypeptides()  %>%
 select(id, name, general_function, specific_function,
gene_name, parent_id) %>%
rename(target_name = name, gene_id = id)
 
#  # join the 3 datasets
drug_targets_full <- inner_join(drug_targets, drug_peptides,
                                  by=c("id"="parent_id", "target_name")) %>%
     inner_join(drugs, by=c("parent_key"="primary_key")) %>%
     select(-other_keys)


## drug names list -----------------------------------------------------------------------------
drug_names = c("Valproat"      = "Valproic Acid",
               "Diclofenac"    = "Diclofenac",
               "Paracetamol"   = "Acetaminophen",
               "Ciproflaxin"   = "Ciprofloxacin",
               "Nitrofurantoin"= "Nitrofurantoin",
               "Tolcapone",
               "Azathioprine",
               "Troglitazone",
               "Nefazodone",
               "Ketoconazole",
               "Omeprazole",
               "Phenytoin",
               "Amiodarone",
               "Cisplatin",
               "Cyclosporin A"  = "Cyclosporine",
               "Verapamil",
               "Buspirone",
               "Melatonin",
               "N-Acetylcysteine"= "Acetylcysteine",
               "Vitamin C"       = "Ascorbic acid",
               "Famotidine",
               "Vancomycin")

  
drug_target_data_sample <- drug_targets_full %>%
filter(organism == "Humans",drug_name %in% drug_names)
  

 -----------------------------------------------------------------------------
drug_targets <- OmnipathR:::drug_target_data_sample %>%
  filter(organism == "Humans",drug_name %in% drug_names)

-----------------------------------------------------------------------------
drug_targets <-  drug_targets %>%
  select(-target_name, -organism) %>%
  mutate(in_OP = gene_id %in% c(interactions$source))

# not all drug-targets are in OP.
print(all(drug_targets$in_OP))

# But each drug has at least one target in OP.
drug_targets %>% group_by(drug_name) %>% summarise(any(in_OP))


## -----------------------------------------------------------------------------
POI = tibble(protein = c("NFE2L2","HMOX1","TP53","CDKN1A","BTG2","NFKB1",
                         "ICAM1","HSPA5", "ATF4","DDIT3","XBP1"))

## -----------------------------------------------------------------------------
POI <- POI %>% mutate(in_OP = protein %in% interactions$target_genesymbol)
# all POI is in Omnipath
print(all(POI$in_OP))



## -----------------------------------------------------------------------------

source_nodes <- drug_targets %>%
  filter(in_OP, drug_name=="Cisplatin") %>%
  pull(gene_name)
target_nodes <- POI %>% filter(in_OP) %>% pull(protein)

collected_path_nodes = list()

for(i_source in 1:length(source_nodes)){
  
  paths <- shortest_paths(OPI_g, from = source_nodes[[i_source]],
                          to = target_nodes,
                          output = 'vpath')
  path_nodes <- lapply(paths$vpath,names) %>% unlist() %>% unique()
  collected_path_nodes[[i_source]] <- path_nodes
}
collected_path_nodes <- unlist(collected_path_nodes) %>% unique()

## -----------------------------------------------------------------------------
cisplatin_nodes <- c(source_nodes,target_nodes, collected_path_nodes) %>%
  unique()
cisplatin_network <- induced_subgraph(graph = OPI_g,vids = cisplatin_nodes)

## -----------------------------------------------------------------------------
V(cisplatin_network)$node_type = ifelse(
  V(cisplatin_network)$name %in% source_nodes, "direct drug target",
  ifelse(
    V(cisplatin_network)$name %in% target_nodes,"POI","intermediate node"))

ggraph(
  cisplatin_network,
  layout = "lgl",
  area = vcount(cisplatin_network)^2.3,
  repulserad = vcount(cisplatin_network)^1.2,
  coolexp = 1.1
) +
  geom_edge_link(
    aes(
      start_cap = label_rect(node1.name),
      end_cap = label_rect(node2.name)),
    arrow = arrow(length = unit(4, 'mm')
    ),
    edge_width = .5,
    edge_alpha = .2
  ) +
  geom_node_point() +
  geom_node_label(aes(label = name, color = node_type)) +
  scale_color_discrete(
    guide = guide_legend(title = 'Node type')
  ) +
  theme_bw() +
  xlab("") +
  ylab("") +
  ggtitle("Cisplatin induced network")


## ---- fig1, dpi=300, fig.width=10, fig.height=10, fig.cap="Overview of the resources featured in OmniPath. Causal resources (including activity-flow and enzyme-substrate resources) can provide direction (*) or sign and direction (+) of interactions.", echo=FALSE----
library(knitr)
knitr::include_graphics("../man/figures/page1_1.png")

## ----installation, eval=FALSE-------------------------------------------------
#  if (!requireNamespace("BiocManager", quietly = TRUE))
#      install.packages("BiocManager")
#  
#  BiocManager::install("OmnipathR")

## ----libraries, message=FALSE-------------------------------------------------
library(OmnipathR)
library(tidyr)
library(dnet)
library(gprofiler2)

## ----interactions-------------------------------------------------------------
## We check some of the different interaction databases
get_interaction_resources()

## The interactions are stored into a data frame.
interactions <-
  import_omnipath_interactions(resources=c("SignaLink3","PhosphoSite",
                                           "SIGNOR"))

## We visualize the first interactions in the data frame.
print_interactions(head(interactions))

## ----sp, message=TRUE---------------------------------------------------------
## We transform the interactions data frame into a graph
OPI_g <- interaction_graph(interactions = interactions)

## Find and print shortest paths on the directed network between proteins
## of interest:
print_path_es(shortest_paths(OPI_g,from = "TYRO3",to = "STAT3",
                             output = 'epath')$epath[[1]],OPI_g)

## Find and print all shortest paths between proteins of interest:
print_path_vs(all_shortest_paths(OPI_g,from = "DYRK2",
                                 to = "MAPKAPK2")$res,OPI_g)

## ----clustering, message=FALSE------------------------------------------------
## We apply a clustering algorithm (Louvain) to group proteins in
## our network. We apply here Louvain which is fast but can only run
## on undirected graphs. Other clustering algorithms can deal with
## directed networks but with longer computational times,
## such as cluster_edge_betweenness. These cluster methods are directly
## available in the igraph package.
OPI_g_undirected <- as.undirected(OPI_g, mode=c("mutual"))
OPI_g_undirected <- simplify(OPI_g_undirected)
cl_results <- cluster_fast_greedy(OPI_g_undirected)
## We extract the cluster where a protein of interest is contained
cluster_id <- cl_results$membership[which(cl_results$names == "ERBB2")]
module_graph <- induced_subgraph(OPI_g_undirected,
                                 V(OPI_g)$name[which(cl_results$membership == cluster_id)])

## ----pathwayextra-------------------------------------------------------------
## We query and store the interactions into a dataframe
interactions <-
  import_pathwayextra_interactions(resources=c("BioGRID","STRING"),
                                   organism = 10090)

## We select all the interactions in which Amfr gene is involved
interactions_Amfr <- dplyr::filter(interactions, source_genesymbol == "Amfr" |
                                     target_genesymbol == "Amfr")

## We print these interactions:
print_interactions(interactions_Amfr)

## ----kinaseextra--------------------------------------------------------------
## We query and store the interactions into a dataframe
interactions <-
  import_kinaseextra_interactions(resources=c("PhosphoPoint",
                                              "PhosphoSite"), organism = 10116)

## We select the interactions in which Dpysl2 gene is a target
interactions_TargetDpysl2 <- dplyr::filter(interactions,
                                           target_genesymbol == "Dpysl2")

## We print these interactions:
print_interactions(interactions_TargetDpysl2)

## ----ligrecextra--------------------------------------------------------------
## We query and store the interactions into a dataframe
interactions <- import_ligrecextra_interactions(resources=c("iTALK",
                                                            "Baccin2019"), organism=9606)

## Receptors of the CDH1 ligand.
interactions_ADM2 <- dplyr::filter(interactions, source_genesymbol == "ADM2")

## We transform the interactions data frame into a graph
OPI_g <- interaction_graph(interactions = interactions_ADM2)

## We induce a network with these genes
Induced_Network <-  dNetInduce(g=OPI_g,
                               nodes_query=as.character( V(OPI_g)$name), knn=0,
                               remove.loops=FALSE, largest.comp=FALSE)

## ----dorothea-----------------------------------------------------------------
## We query and store the interactions into a dataframe
interactions <- import_dorothea_interactions(
  resources=c("DoRothEA"),
  dorothea_levels = 'A',
  organism=9606
)

## We select the most confident interactions for a given TF and we print
## the interactions to check the way it regulates its different targets
interactions_A_GLI1  <- dplyr::filter(interactions, dorothea_level=="A",
                                      source_genesymbol == "GLI1")
print_interactions(interactions_A_GLI1)

## ----mirnatarget--------------------------------------------------------------
## We query and store the interactions into a dataframe
interactions <-
  import_mirnatarget_interactions(resources=c("miRTarBase","miRecords"))

## We select the interactions where a miRNA is interacting with the TF
## used in the previous code chunk and we print these interactions.
interactions_miRNA_GLI1 <-
  dplyr::filter(interactions,  target_genesymbol == "GLI1")
print_interactions(interactions_miRNA_GLI1)

## We transform the previous selections to graphs (igraph objects)
OPI_g_1 <-interaction_graph(interactions = interactions_A_GLI1)
OPI_g_2 <-interaction_graph(interactions = interactions_miRNA_GLI1)

## ----PTMs---------------------------------------------------------------------
## We check the different PTMs databases
get_enzsub_resources()

## We query and store the ptms into a dataframe. No filtering by
## databases in this case.
ptms <- import_omnipath_enzsub()

## We can select and print the reactions between a specific kinase and
## a specific substrate
print_interactions(dplyr::filter(ptms,enzyme_genesymbol=="MAP2K1",
                                 substrate_genesymbol=="MAPK3"))

## In the previous results, we can see that ptms does not contain sign
## (activation/inhibition). We can generate this information based on the
## protein-protein OmniPath interaction dataset.
interactions <- import_omnipath_interactions()
ptms <- get_signed_ptms(ptms, interactions)

## We select again the same kinase and substrate. Now we have information
## about inhibition or activation when we print the ptms
print_interactions(dplyr::filter(ptms,enzyme_genesymbol=="MAP2K1",
                                 substrate_genesymbol=="MAPK3"))

## We can also transform the ptms into a graph.
ptms_g <- ptms_graph(ptms = ptms)

## We download PTMs for mouse
ptms <- import_omnipath_enzsub(resources=c("PhosphoSite", "SIGNOR"),
                               organism=10090)

## ----complexes----------------------------------------------------------------
## We check the different complexes databases
get_complex_resources()

## We query and store complexes from some sources into a dataframe.
complexes <- import_omnipath_complexes(resources=c("CORUM", "hu.MAP"))

## We check all the molecular complexes where a set of genes participate
query_genes <- c("WRN","PARP1")

## Complexes where any of the input genes participate
complexes_query_genes_any <- unique(get_complex_genes(complexes,query_genes,
                                                      total_match=FALSE))

## We print the components of the different selected components
head(complexes_query_genes_any$components_genesymbols,6)

## Complexes where all the input genes participate jointly
complexes_query_genes_join <- unique(get_complex_genes(complexes,query_genes,
                                                       total_match=TRUE))

## We print the components of the different selected components
complexes_query_genes_join$components_genesymbols

## ----enrichment---------------------------------------------------------------
genes_complex <-
  unlist(strsplit(complexes_query_genes_join$components_genesymbols, "_"))

## We can perform an enrichment analyses with the genes in the complex
EnrichmentResults <- gost(genes_complex, significant = TRUE,
                          user_threshold = 0.001, correction_method = c("fdr"),
                          sources=c("GO:BP","GO:CC","GO:MF"))

## We show the most significant results
EnrichmentResults$result %>%
  dplyr::select(term_id, source, term_name,p_value) %>%
  dplyr::top_n(5,-p_value)

## ----complex_annotations------------------------------------------------------
## We check the different annotation databases
get_annotation_resources()

## We can further investigate the features of the complex selected
## in the previous section.

## We first get the annotations of the complex itself:
annotations <- import_omnipath_annotations(proteins=paste0("COMPLEX:",
                                                           complexes_query_genes_join$components_genesymbols))

head(dplyr::select(annotations,source,label,value),10)

## ----annotations_components---------------------------------------------------
## Then, we explore some annotations of its individual components

## Pathways where the proteins belong:
annotations <- import_omnipath_annotations(proteins=genes_complex,
                                           resources=c("NetPath"))

dplyr::select(annotations,genesymbol,value)

## Cellular localization of our proteins
annotations <-import_omnipath_annotations(proteins=genes_complex,
                                          resources=c("ComPPI"))

## Since we have same record_id for some results of our query, we spread
## these records across columns
spread(annotations, label, value) %>%
  dplyr::arrange(desc(score)) %>%
  dplyr::top_n(10, score)

## ----intercell----------------------------------------------------------------
## We check some of the different intercell categories
get_intercell_generic_categories()

## We import the intercell data into a dataframe
intercell <- import_omnipath_intercell(scope = 'generic',
                                       aspect = 'locational')

## We check the intercell annotations for the individual components of
## our previous complex. We filter our data to print it in a good format
dplyr::filter(intercell,genesymbol %in% genes_complex) %>%
  dplyr::distinct(genesymbol, parent, .keep_all = TRUE) %>%
  dplyr::select(category, genesymbol, parent) %>%
  dplyr::arrange(genesymbol)

## We close graphical connections
while (!is.null(dev.list()))  dev.off()

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

