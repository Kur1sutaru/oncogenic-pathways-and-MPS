setwd("C:/Users/Mateus/Desktop")

# The second quest about Drug repositioning
#First download of the data: March 4th 2021
# Tutorial: http://bioconductor.org/packages/release/bioc/vignettes/OmnipathR/inst/doc/drug_targets.html
# Selected drugs based on the venn diagram of the databases of drugs
# Selected genes based on MCC method top 10 cytohubba
# Downstream signaling nodes
# We would like to investigate the effect of the drugs on some selected proteins. 
# For example, the activity of these proteins are measured upon the drug perturbation. 
# We'll build a network from the drug targets to these selected nodes.
# The above network represents a way how Cisplatin can influence the POIs. 
# One can for example filter out edges based on the number of resources reporting 
# the edge or based on the number of papers mentioning it. 
# However, this is already covered by previous pypath tutorials.

library(dplyr)
library(ggplot2)
library(OmnipathR)
library(igraph)
library(ggraph)
library(dbparser)
library(XML)
library(dplyr)

# Download protein-protein interactions
interactions = import_omnipath_interactions() %>% as_tibble()
# Convert to igraph objects:
OPI_g = interaction_graph(interactions = interactions )

#This one for read the info
read_drugbank_xml_db("C:/Users/Mateus/Desktop/full_database.xml")

## load drugs data
all-drugbank<-drug_element(
  elements_options = c("all"),
  save_table = FALSE,
  save_csv = FALSE,
  csv_path = ".",
  override_csv = TRUE,
  database_connection = NULL
)
# collect data about Organisms in which the drug may display activity; activity may depend on local susceptibility
# patterns and resistance.

drug_affected_organisms<-drug_affected_organisms(
  save_table = FALSE,
  save_csv = TRUE,
  csv_path = ".",
  override_csv = TRUE,
  database_connection = NULL
)

# general categorizations of the drug, like category name, mesh-ID, drugbamk id
drug_categories<-drug_categories(
  save_table = FALSE,
  save_csv = TRUE,
  csv_path = ".",
  override_csv = TRUE,
  database_connection = NULL
)

# The adverse drug reactions that may occur as a result of the listed single nucleotide polymorphisms
# (SNPs)
drug_snp_adverse_reactions<-drug_snp_adverse_reactions(
  save_table = FALSE,
  save_csv = TRUE,
  csv_path = ".",
  override_csv = TRUE,
  database_connection = NULL
)

# List of the commercially available dosages of the drug.
drug_dosages(
  save_table = FALSE,
  save_csv = TRUE,
  csv_path = ".",
  override_csv = TRUE,
  database_connection = NULL
)


# Groups that this drug belongs to. May include any of: approved, vet_approved, nutraceutical, illicit,
# withdrawn, investigational, and experimental.

drug_groups<-drug_groups(
  save_table = FALSE,
  save_csv = FALSE,
  csv_path = ".",
  override_csv = TRUE,
  database_connection = NULL
)

# Drug-drug interactions detailing drugs that, when administered concomitantly with the drug of interest, will affect its activity or result in adverse effects. These interactions may be synergistic or
# antagonistic depending on the physiological effects and mechanism of action of each drug.

drug_interactions(
  save_table = FALSE,
  save_csv = TRUE,
  csv_path = ".",
  override_csv = TRUE,
  database_connection = NULL
)

# Metabolic, disease, and biological pathways that the drug is involved in, as identified by the Small
# Molecule Protein Database (SMPDB).

drug_pathways<-drug_pathway(
  save_table = FALSE,
  save_csv = TRUE,
  csv_path = ".",
  override_csv = TRUE,
  database_connection = NULL
)

# Drugs involved in this pathway.

drug_pathway_drugs<-drug_pathway_drugs(
  save_table = FALSE,
  save_csv = FALSE,
  csv_path = ".",
  override_csv = TRUE,
  database_connection = NULL
)

# Unit drug prices

drug_prices(
  save_table = FALSE,
  save_csv = FALSE,
  csv_path = ".",
  override_csv = TRUE,
  database_connection = NULL
)

# get_drugbank_version returns uploaded drugbank database version.

get_drugbank_version()

## load drugs data
read_drugbank_xml_db("C:/Users/Mateus/Desktop/full_database.xml")
drugs <- parse_drug() %>% select(primary_key, name)
drugs <- rename(drugs,drug_name = name)

#load omnipathR
library(OmnipathR)

#For a better result, make the analysis with the top degs of each human dataset: GSE111906, GSE23075, and GSE32154

#Let's start to the GSE111906 dataset! And the MCC results for genes!


drug_names = c("Axitinib",
               "Bortezomib",
               "Crizotinib",
               "Dasatinib",
               "Gefitinib",
               "Pemetrexed",
               "Erlotinib",
               "Imatinib",
               "Lapatinib",
               "Lenalidomide",
               "Pazopanib",
               "Rapamycin",
               "Sorafenib",
               "Sunitinib",
               "Temsirolimus",
               "Vinblastine",
               "Vandetanib",
               "Brigatinib",
               "Cabozantinib",
               "Cobimetinib",
               "Copanlisib",
               "Dabrafenib",
               "Dacarbazine",
               "Everolimus",
               "Glasdegib",
               "Idelalisib",
               "Ixazomib",
               "Lorlatinib",
               "Bevacizumab",
               "Aflibercept",
               "Regorafenib",
               "Cetuximab",
               "Panitumumab",
               "Afatinib",
               "Dacomitinib",
               "Neratinib",
               "Crizotinib",
               "Selumetinib",
               "Trametinib",
               "Brivanib",
               "Nintedanib",
               "Ganetespib",
               "Navitoclax",
               "Cilengitide",
               "Ridaforolimus")

drug_target_data_sample <- drug_targets %>%
  filter(organism == "Humans",drug_name %in% drug_names)

drug_targets <- OmnipathR:::drug_target_data_sample %>%
  filter(organism == "Humans",drug_name %in% drug_names)

# Quality control: Check which drug targets are in Omnipath
OPI_g = interaction_graph(interactions = interactions )

#Interaction analysis
drug_targets <-  drug_targets %>%
  select(-target_name, -organism) %>%
  mutate(in_OP = gene_id %in% c(interactions$source))

# not all drug-targets are in OP.
print(all(drug_targets$in_OP))

POI = tibble(protein = c("ZBTB12", "A2M", "ABL2", "ABR", "ACTN1", "ACTN4", "ACTR2", "AMPH",
                         "AP1S1","AP2S1", "APLP2", "ARF6", "ARPC2", "ARPC5",                       "ARRB2",
                         "ATP8A1", "B2M", "B4GALT1", "CALU", "CBLB", "CCNB2",
                         "CD63", "CDC20", "CDCA8","CDH2", "CHRDL1", "CKAP4", "CLU",
                         "COG5", "COL11A1", "COL12A1", "COL13A1", "COL16A1", "COL22A1",
                         "COL2A1", "COL3A1", "COL4A1", "COL4A2", "COL6A3", "COL8A1",
                         "COPS7A", "CRKL", "CRTAP", "CUL7", "CYR61", "DAB2", "DDOST",
                         "DEGS1", "DTX3L", "DYNLL2", "EGFR", "ERLEC1", "FBN1", "FBXL2",
                         "FBXO17", "FBXO7", "FN1", "FTL", "GINS2", "GLIPR1", "GPC3",
                         "GPX8", "HGSNAT", "HIF1A", "HMOX2", "HRC", "HSPA5", "IFNGR2",
                         "IGF2", "IGFBP5", "IQGAP1", "IRS1", "ITGA2", "ITGA8", "ITGB1",
                         "JAK1", "JAK3", "KBTBD6", "KBTBD7", "KBTBD8", "KIAA1549",
                         "KIF15", "KIF2C", "KLHL13", "KNTC1", "LAMB1", "LAMB2",
                         "LAMC1", "LAMP1", "LEPR", "LEPRE1", "LEPREL1", "LGALS1",
                         "LMO7", "LRP2", "LTBP1", "MAD2L1", "MAGED2", "MGAT4A",
                         "MKI67", "MMRN1", "MTTP", "NASP", "NCAPD2", "NDEL1",
                         "NECAP2", "NHLRC2", "P4HA1", "P4HB", "PDIA4", "PDXK",
                         "PJA1", "PLAUR", "PLK1", "PLK2", "PROS1", "PRSS23",
                         "PSAP", "PSMD2", "PTK2", "PTPRJ", "PXN", "QKI", "QSOX1",
                         "RAP1B", "RAP2B", "RNF130", "RNF138", "RNF14", "RNF217",
                         "SCARB2", "SDC2", "SDC4", "SEC13", "SERPINE1", "SH2B3",
                         "SH3D19", "SH3KBP1", "SLC15A4", "SLC2A3", "SMURF2",
                         "SNX9", "SOCS3", "SPRED2", "SPRED3", "SPSB4", "STAG1",
                         "STAT6", "STC2", "STOM", "SYT1", "SYT11", "SYT2", "TGFB2",
                         "TGOLN2", "TIMP2", "TLN1", "TMEM63A", "TMSB4X", "TNC",
                         "TNFRSF1B", "TPD52", "TRIM36", "TRIM71", "TRIP10", "UBA6",
                         "UBE2H", "UBE2Z", "USP44", "VCL", "WNT5A", "WSB2"))

POI <- POI %>% mutate(in_OP = protein %in% interactions$target_genesymbol)
# all POI is in Omnipath
print(all(POI$in_OP))

source_nodes <- drug_targets %>%
  filter(in_OP, drug_name=="Aflibercept") %>%
  pull(gene_name)
target_nodes <- POI %>% filter(in_OP) %>% pull(protein)

# Graph of drugs and related genes
collected_path_nodes = list()

for(i_source in 1:length(source_nodes)){
  
  paths <- shortest_paths(OPI_g, from = source_nodes[[i_source]],
                          to = target_nodes,
                          output = 'vpath')
  path_nodes <- lapply(paths$vpath,names) %>% unlist() %>% unique()
  collected_path_nodes[[i_source]] <- path_nodes
}
collected_path_nodes <- unlist(collected_path_nodes) %>% unique()

Aflibercept_nodes <- c(source_nodes,target_nodes, collected_path_nodes) %>%
  unique()
Aflibercept_network <- induced_subgraph(graph = OPI_g,vids = Aflibercept_nodes)

V(Aflibercept_network)$node_type = ifelse(
  V(Aflibercept_network)$name %in% source_nodes, "direct drug target",
  ifelse(
    V(Aflibercept_network)$name %in% target_nodes,"POI","intermediate node"))

#Plot the graph result
ggraph(
  Aflibercept_network,
  layout = "lgl",
  area = vcount(Aflibercept_network)^2.3,
  repulserad = vcount(Aflibercept_network)^1.2,
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
  ggtitle("Aflibercept induced network")




#Let's try to the GSE23075 dataset!

#load omnipathR
library(OmnipathR)

drug_names = c("Axitinib",
               "Bortezomib",
               "Crizotinib",
               "Dasatinib",
               "Gefitinib",
               "Pemetrexed",
               "Erlotinib",
               "Imatinib",
               "Lapatinib",
               "Lenalidomide",
               "Pazopanib",
               "Rapamycin",
               "Sorafenib",
               "Sunitinib",
               "Temsirolimus",
               "Vinblastine",
               "Vandetanib",
               "Brigatinib",
               "Cabozantinib",
               "Cobimetinib",
               "Copanlisib",
               "Dabrafenib",
               "Dacarbazine",
               "Everolimus",
               "Glasdegib",
               "Idelalisib",
               "Ixazomib",
               "Lorlatinib",
               "Temsirolimus",
               "Bevacizumab",
               "Aflibercept",
               "Regorafenib",
               "Cetuximab",
               "Panitumumab",
               "Afatinib",
               "Dacomitinib",
               "Neratinib",
               "Crizotinib",
               "Selumetinib",
               "Trametinib",
               "Brivanib",
               "Nintedanib",
               "Ganetespib",
               "Navitoclax",
               "Cilengitide",
               "Ridaforolimus")

drug_target_data_sample <- drug_targets %>%
  filter(organism == "Humans",drug_name %in% drug_names)

drug_targets <- OmnipathR:::drug_target_data_sample %>%
  filter(organism == "Humans",drug_name %in% drug_names)

# Quality control: Check which drug targets are in Omnipath
OPI_g = interaction_graph(interactions = interactions )

#Interaction analysis
drug_targets <-  drug_targets %>%
  select(-target_name, -organism) %>%
  mutate(in_OP = gene_id %in% c(interactions$source))

# not all drug-targets are in OP.
print(all(drug_targets$in_OP))

POI = tibble(protein = c("ATG4D", "MRPL37", "SFXN1", "COPS4", "ZBTB10", "EIF4G2", "AGFG1", "FOXJ3", 
                         "RAB7A", "SLC25A23", "ZNF513", "OSBPL8", "GORASP2", "TUBB4B", "LRRC42", "FDFT1", 
                         "UPF1", "GIGYF2", "GPKOW", "NHLRC2", "RELA", 
                         "GRK2", "SDHAF2", "PIAS3", "C11orf24", "FSCN1", "MRPL23", 
                         "POGZ", "ARHGEF7", "MRPS18B", "ANKRD17", "YWHAG", "AP2A1", "PPP2R1A", 
                         "TRIM24", "NSD1", "UBE4A", "YWHAH", "UBC", "CST3", "HDAC3", "MFSD14A",
                         "PPWD1", "TFE3", "NCDN", "TCTN3", "ARAF", "RAD23B", "TMEM184C", "TBC1D17"))

POI <- POI %>% mutate(in_OP = protein %in% interactions$target_genesymbol)
# all POI is in Omnipath
print(all(POI$in_OP))

source_nodes <- drug_targets %>%
  filter(in_OP, drug_name=="Erlotinib") %>%
  pull(gene_name)
target_nodes <- POI %>% filter(in_OP) %>% pull(protein)

# Graph of drugs and related genes
collected_path_nodes = list()

for(i_source in 1:length(source_nodes)){
  
  paths <- shortest_paths(OPI_g, from = source_nodes[[i_source]],
                          to = target_nodes,
                          output = 'vpath')
  path_nodes <- lapply(paths$vpath,names) %>% unlist() %>% unique()
  collected_path_nodes[[i_source]] <- path_nodes
}
collected_path_nodes <- unlist(collected_path_nodes) %>% unique()

Erlotinib_nodes <- c(source_nodes,target_nodes, collected_path_nodes) %>%
  unique()
Erlotinib_network <- induced_subgraph(graph = OPI_g,vids = Erlotinib_nodes)

V(Erlotinib_network)$node_type = ifelse(
  V(Erlotinib_network)$name %in% source_nodes, "direct drug target",
  ifelse(
    V(Erlotinib_network)$name %in% target_nodes,"POI","intermediate node"))

#Plot the graph result
ggraph(
  Erlotinib_network,
  layout = "lgl",
  area = vcount(Axitinib_network)^2.3,
  repulserad = vcount(Axitinib_network)^1.2,
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
  ggtitle("Erlotinib induced network")