#' A function to clean the data set
#'
#' The function deals with the various taxonomical levels of recorded taxa. Only
#' taxa identified at the genus or species levels are kept. It also deletes
#' misleading records such as 'none', 'total' or 'sum'.
#'
#' The way the function treats taxa not identified at the species level
#' changes depending on whether they are redundant or unique. More precisely,
#' a community has a 'Lucanus sp.' record and there is another record
#' (NOTE actually the community level is the unique sampling dt_ID in
#' Datasource_ID, Plot_ID, year) for 'Lucanus cervus'. We can't say for sure
#' that 'Lucanus sp.' is distinct from 'Lucanus cervus', and 'Lucanus sp.' is
#' considered redundant. If 'Lucanus sp.' is the only record for the Lucanus
#' genus, it is considered unique.
#'
#' @param dt a data.frame or data.table
#' @param mode character. If 'richness', unique taxa are kept and redundant records
#'    are deleted.
#'    If `mode` is 'beta', unique taxa are kept as is. Problematic and redundant
#'    taxa are deleted but their abundance/biomass is assigned to the most
#'    frequent species of this site over the years. If this most frequent species
#'    is already present in the considered sample, the abundance of the
#'    congeneric problematic taxon is added to the most frequent taxon's
#'    abundance. If the most frequent taxon is not present in this sample,
#'    the problematic taxon name is replaced with the name of the mos frequent
#'    taxon.
#'   'beta' method is significantly slower.
#' @param add_unique_sample_ID logical. If TRUE, the default, a column of unique
#'    integer ID per sample is added, it is named dt_ID.
#' @value an object of class `data.table` and `data.frame`
#' @import data.table

cleaning_dataset <- function(dt, mode, add_unique_sample_ID = TRUE, distribution_mode = "all", keep_null_abundances = TRUE) {

  library(data.table)
  data.table::setDT(dt)

  null_abundances <- dt[Number == 0L]
  dt <- dt[Number > 0]

  dt[, dt_ID := .GRP, by = .(Datasource_ID, Plot_ID, Year)]

  dt[, ':='(
    Taxon = as.character(Taxon),
    # Original_number = as.numeric(Original_number),
    # Transformed_number = as.numeric(Transformed_number),
    Rank = as.integer(Rank)
  )]

  ## Cleaning taxa ----
  # delete end '_'
  # gsub('_*$', '', c("NONE___", 'Argyresthia_albistria_', 'Argyresthia_albistria'))
  dt[, Taxon := gsub('_*$', '', Taxon)]
  dt[, TaxonKey := Taxon]

  ## Deleting taxa ----
  ### deleting taxa with "total", "sum", "none" or "NONE" in their name
  dt <- dt[#!grepl(
    #  'total(?=[[:punct:]])|(?<=[[:punct:]])sum(?=[[:punct:]])|NONE|none(?=[[:punct:]])',
    #  Taxon, perl = TRUE
    #) &
    ### deleting taxa identified at a high taxonomical level than genus
    Rank >= 8L #&
    #  Number > 0
  ]

  # Temporary: pooling all duplicated taxa (generation 1 & 2, males and females, etc) ----
  dt[, Number := lapply(.SD, sum, na.rm = TRUE),
     by = .(dt_ID, Taxon),
     .SDcols = "Number"]
  # dt[, Withintaxon_group := paste(Withintaxon_group, collapse = ';'),  by = .(dt_ID, Taxon)]
  dt <- unique(dt)

  # * Check pooling
  if (FALSE) {
    dt[, repeated_taxa_count := .N, by = .(dt_ID, Taxon)][, any_repeated_taxa := any(repeated_taxa_count > 1), by = dt_ID]
    any(dt$any_repeated_taxa)
  }

  # problematic_taxon ----
  dt[, problematic_taxon :=
       Level != 'Species' |
       grepl(
         pattern = 'juv\\.|ad\\.|adult|unidentified|undet|unknown|larvae|immature|adult|winged|nymph(?=[\\)_]|(?<=[[:punct:]])agg)',
         x = Taxon, perl = TRUE, fixed = FALSE)
  ]









  if (any(dt$problematic_taxon)) {

    dt[, c("Family", "Subfamily", 'Genus','Species') := lapply(.SD, function(column) replace(column, column == '', NA)), .SDcols = c("Family", "Subfamily", 'Genus', 'Species')] # replacing all empty cells by 'NA'

    dtTaxon <- unique(dt[, c("Datasource_ID", "Plot_ID", "Family", "Subfamily" , "Genus", "Species")])

    ## counting all taxa belonging to a single genus/subfamily/family in - a single data set - a single plot over the all years
    for (variable in c("Family", "Subfamily", "Genus", "Species")) { # going through all levels does not make sense. "Phylum", "Class", "Subclass", "Suborder", "Order",
      dtTaxon[, paste0('count', variable) := .N, by = .(Datasource_ID, Plot_ID, dtTaxon[, get(variable)])] # dt_ID
    } # when several taxa have a NA value for Species, countGenus count them together.
    dt <- merge(dt, dtTaxon, by = c("Datasource_ID", "Plot_ID", "Family", "Subfamily" , "Genus", "Species"), all.x = TRUE)

    dt[, unique_taxon := data.table::fifelse((Level == 'Family' & countFamily == 1) | (Level == 'Subfamily' & countSubfamily == 1) | (Level == 'Genus' & countGenus == 1) | (Level == 'Species' & countSpecies == 1), TRUE, FALSE)] # there must be a more elegant way to test either countGenus or countSpecies depending on the value in Level.

    data.table::setkey(dt, dt_ID, TaxonKey)

    if (mode == 'richness') dt <- dt[!(problematic_taxon & !unique_taxon)]


    if (mode == 'beta') {

      dt[, total_abundance := sum(Number), by = .(Datasource_ID, Plot_ID, Taxon)]
      dt[, should_be_distributed_for_beta := FALSE][ (problematic_taxon & !unique_taxon), should_be_distributed_for_beta := TRUE] # would fifelse be faster?

      if (distribution_mode == "all") {
        additional_table <- setNames(data.table(matrix(nrow = 0, ncol = 16)), c('Taxon', 'Phylum', 'Class', 'Subclass', 'Order', 'Suborder', 'Family', 'Subfamily', 'Genus', 'Species', 'Level', 'Rank','total_abundance','relative_abundance', 'should_be_distributed_for_beta','Number'))
      }

      for (id_i in unique(dt[(problematic_taxon) & (should_be_distributed_for_beta), dt_ID])) {  # id_i= 266
        # for (id_i in c(258  ,259  ,260,261,  262  , 263, 264,265)) {  # id_i= 266

        # Temporary solution: if there is only one species in this dt_ID, delete it. In the future, once periods have been pooled, there should not be sites with S = 1 anymore.
        if (nrow(dt[.(id_i)]) == 1)     next

        for (problematic_taxon_j in unique(dt[.(id_i)][(problematic_taxon & should_be_distributed_for_beta), Taxon])) {  # problematic_taxon_j = "Conocephalus_undet"

          # dt[.(id_i, problematic_taxon_j), Number]
          # dt[.(id_i, problematic_taxon_j), Level]
          # dt[.(id_i, problematic_taxon_j), countGenus]
          # dt[.(id_i, problematic_taxon_j), Unit]

          # add the number to distribute to the most abundant species

          # find he most abundant taxon from this level in this Plot_ID and over
          # the years (if ties, first is chosen)
          # should be "in this level or the first level with other taxon"
          most_abundant_taxa_over_the_years <- dt[
            Plot_ID == dt[.(id_i), Plot_ID][1] &
              Taxon != problematic_taxon_j &
              get(as.character(dt[.(id_i, problematic_taxon_j), Level])) == dt[.(id_i, problematic_taxon_j), get(as.character(dt[.(id_i, problematic_taxon_j), Level]))]
            # the name in the column matching Level (ie Species, Genus, Subfamily or Family) matches the name of the taxon whose abundance has to be distributed,
            # problem when problematic taxon is the only member at this level, function should go higher to find the next closest most abundant taxon
          ]

          go_up_X_rank <- function(rank, X) {
            ranklist <- c("Species", "Genus", "Subfamily", "Family", "Order", "Suborder", "Subclass", "Class", "Phylum")
            return(ranklist[match(rank, ranklist) + X])
          }

          X <- 1
          while (nrow(most_abundant_taxa_over_the_years) == 0) {
            most_abundant_taxa_over_the_years <- dt[
              Plot_ID == dt[.(id_i), Plot_ID][1] &
                Taxon != problematic_taxon_j &
                get(as.character(go_up_X_rank(dt[.(id_i, problematic_taxon_j), Level], X))) == dt[.(id_i, problematic_taxon_j), get(as.character(go_up_X_rank(dt[.(id_i, problematic_taxon_j), Level], X)))]
            ]
            X <- X + 1
          }

          column_selection <- match(c('Taxon', 'Phylum', 'Class', 'Subclass', 'Order', 'Suborder', 'Family', 'Subfamily', 'Genus', 'Species', 'Level', 'Rank','total_abundance'), colnames(dt))
          most_abundant_taxa_over_the_years <- unique(most_abundant_taxa_over_the_years[, ..column_selection])
          most_abundant_taxa_over_the_years[, relative_abundance := total_abundance / sum(total_abundance)][, should_be_distributed_for_beta := FALSE]

          if (distribution_mode == "most_frequent") {
            most_abundant_taxon <- most_abundant_taxa_over_the_years[which.max(total_abundance)]

            if (most_abundant_taxon$Taxon %in% dt[dt_ID == id_i, Taxon]) {
              # if the most abundant taxon is already present in dt_ID
              number_to_distribute <- dt[.(id_i, problematic_taxon_j), Number]

              dt[dt_ID == id_i & Taxon == most_abundant_taxon$Taxon,
                 Number := Number + number_to_distribute]
            } else {
              # if the most abundant taxon of this plot_ID over the years was not present in this dt_ID
              # replace the problematic taxon with the most abundant taxon name
              # and number and change the problematic taxon and should be distributed columns

              data.table::set( # alternative style for updating dt by reference
                x = dt,
                i = which(dt$dt_ID == id_i & dt$Taxon == problematic_taxon_j),
                j = column_selection,
                value = most_abundant_taxon[, ..column_selection]
              )
              dt[dt_ID == id_i & Taxon == problematic_taxon_j,
                 ':='(
                   problematic_taxon = FALSE,
                   unique_taxon =  TRUE,
                   should_be_distributed_for_beta = FALSE)
              ]
            }
          } # end of distribution mode "most_frequent"


          if (distribution_mode == "all") {

            number_to_distribute <- dt[.(id_i, problematic_taxon_j), Number]
            # if the most abundant taxon is already present in dt_ID
            if (any(most_abundant_taxa_over_the_years$Taxon %in% dt[dt_ID == id_i, Taxon])) {
              for (alternative_taxon_k in intersect(most_abundant_taxa_over_the_years$Taxon, dt[dt_ID == id_i, Taxon])) {

                dt[dt_ID == id_i & Taxon == alternative_taxon_k,
                   Number := round(
                     Number + number_to_distribute * most_abundant_taxa_over_the_years[Taxon == alternative_taxon_k, relative_abundance],
                     0)]
              }
            }
            # if the most abundant taxa of this plot_ID over the years were not present in this dt_ID
            # add the most abundant alternative taxa at the end of the table
            most_abundant_taxa_over_the_years <- most_abundant_taxa_over_the_years[!Taxon %in% intersect(most_abundant_taxa_over_the_years$Taxon, dt[dt_ID == id_i, Taxon])]
            most_abundant_taxa_over_the_years[, Number := round(number_to_distribute * relative_abundance, 0)]
            most_abundant_taxa_over_the_years <- most_abundant_taxa_over_the_years[Number > 0]
            most_abundant_taxa_over_the_years[, c("Datasource_ID", "Plot_ID", "Year", "dt_ID") := dt[.(id_i), .(Datasource_ID, Plot_ID, Year, dt_ID)][1]]
            additional_table <- rbind(additional_table, most_abundant_taxa_over_the_years, fill = TRUE)

          } # end of distribution mode "all"
        } # end of the loop on problematic taxa
      } # end of the loop on samples

      if (nrow(additional_table) > 0) {
        dt <- rbind(dt, additional_table, fill = TRUE)
        final_reordering_needed <- TRUE
      }

      # Now that the abundances of problematic taxa have been distributed, we delete them
      dt <- dt[(!should_be_distributed_for_beta)]
      dt[, should_be_distributed_for_beta := NULL][, total_abundance := NULL][, relative_abundance := NULL]

    } # end of "beta" mode


    dt[, paste0('count', c("Family", "Subfamily", "Genus", "Species")) := NULL   # "Phylum", "Class", "Subclass", "Suborder", "Order",
    ][, unique_taxon := NULL]

  }  #end of if there is any problematic taxon in dt

  if (keep_null_abundances) {
    null_abundances[, TaxonKey := Taxon]
    dt <- rbind(dt, null_abundances, fill = TRUE)
  }

  if (isTRUE("final_reordering_needed") | keep_null_abundances) {
    dt[, dt_ID := .GRP, by = .(Datasource_ID, Plot_ID, Year)]
    data.table::setorder(dt, dt_ID, TaxonKey)
  }

  if (add_unique_sample_ID)  {
    data.table::setcolorder(x = dt, neworder = 'dt_ID')
  } else {
    data.table::set(x = dt, j = 'dt_ID', value = NULL)
  }
  dt[, TaxonKey := NULL]

  dt[, problematic_taxon := NULL]

  return(dt)
}


## Quick checking result ----
if (FALSE) {
  # dt <- readRDS("~/insect-richness-trends/data/dropbox data/all.selectedIns.RDS")
  dt <- readRDS("~/insect-richness-trends/data/dropbox data/rawInsectsForRichnessAggregatedPerYear.RDS")
  dim(dt)
  dt1 <- cleaning_dataset(dt, mode = 'richness') # dt is partially changed by reference
  dim(dt);dim(dt1)
  sum(dt$Number);sum(dt1$Number)
  dt2 <- cleaning_dataset(dt, mode = 'beta') # dt is partially changed by reference
  dim(dt);dim(dt2)
  sum(is.na(dt)) ; sum(is.na(dt2))
  sum(dt$Number);sum(dt2$Number)
  dt1[,sum(Number)] < dt2[,sum(Number)]

}





