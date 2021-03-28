#' @title prepTaxa: reshapes plant taxa lookup table
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @description This function reshapes the plant taxa lookup table to match the tlu_Plants table in the original database
#' to make filtering and summarizing easier. Function is mostly internal and used for quadrat species, seedling and sapling summaries.
#'
#' @return Returns a dataframe with cover class midpoints for each quadrat and includes guild for each species.
#'
#' @examples
#' importData()
#' # reshape the plant taxa lookup to wide
#' taxa_wide <- prepTaxa()
#'
#' @export
#'

prepTaxa <- function(){

  env <- if(exists("VIEWS_MIDN")){VIEWS_MIDN} else {.GlobalEnv}

  tryCatch(taxa <- get("COMN_Taxa", envir = env) %>%
             select(TaxonID, TSN, ScientificName, CommonName, Order, Family,
                    Genus, Species, SubSpecies, IsExotic, InvasiveNETN, IsFernAlly,
                    TaxonGroupLabel, DeerIndicatorTree, DeerIndicatorHerb, FilterMIDN),
           error = function(e){stop("COMN_Taxa view not found. Please import view.")})
  sort(taxa$ScientificName[taxa$InvasiveNETN == 1])

  #+++++++++++++++++++++++++
  # When the canopy exclusion column is added back, need to include in this function
  #+++++++++++++++++++++++++

  # Clean up taxa table so easier to work with
  names(taxa)[names(taxa) == "IsFernAlly"] <- "FernAlly"
  names(taxa)[names(taxa) == "IsExotic"] <- "Exotic"

  cols <- c("Order", "Family", "Genus", "Species", "SubSpecies")
  taxa[, cols] <- invisible(lapply(taxa[, cols], gsub, pattern = "NOT DETERMINED", replacement = NA))
  taxa$CommonName <- sub(",.*", "", taxa$CommonName)
  taxa$CommonName <- sub("/.*", "", taxa$CommonName)
  taxa$guild <- 1
  taxa$guild_text <- gsub("/", "", taxa$TaxonGroupLabel)

  # Change spp. native in MIDN, but exotic in NETN to 0
  taxa$Exotic[taxa$ScientificName == "Robinia pseudoacacia"] <- 0
  taxa$Exotic[taxa$ScientificName == "Robinia hispida"] <- 0
  taxa$InvasiveMIDN <- ifelse(taxa$ScientificName == "Robinia pseudoacacia", 0,
                              taxa$InvasiveNETN)

  # reshape guilds to wide
  taxa_wide <- taxa %>% select(-TaxonGroupLabel) %>%
    pivot_wider(names_from = guild_text,
                values_from = guild,
                values_fill = 0) %>%
    select(TaxonID, TSN, ScientificName, CommonName, Order, Family, Genus, Species, SubSpecies,
           Tree, TreeShrub, Shrub, Vine, Herbaceous, Graminoid, FernAlly, MossLichen, Exotic,
           InvasiveMIDN, DeerIndicatorTree, DeerIndicatorHerb, FilterMIDN)

  return(taxa_wide)
}
