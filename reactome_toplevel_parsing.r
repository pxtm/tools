library(AnnotationDbi)
library(rbioapi)
library(reactome.db)
library(tidyverse)


## Get all pathways
pathways <- AnnotationDbi::toTable(reactomedb::reactomePATHNAME2ID)
## Filter to keep only human pathways
pathwaysSelectedSpecies <- pathways[grep("Homo sapiens: ", iconv(pathways$path_name)), ]

## Get ancestor pathway for each entry
top_paths <- lapply(pathwaysSelectedSpecies$DB_ID, function(p){
    tryCatch({
        ancestors <- rbioapi::rba_reactome_event_ancestors(p) ## retrieve ancestors info
        # Sys.sleep(2)
        ancestors <- ancestors[[1]] ## unlist
        df <- cbind(pathwaysSelectedSpecies[which(pathwaysSelectedSpecies$DB_ID == p), ],
                ancestors %>% 
                    dplyr::filter(schemaClass == "TopLevelPathway") %>% 
                    dplyr::select(parent_ID = stId, parent_name = displayName, schemaClass))
        names(df) <- c("path_ID", "name", "parent_ID", "parent_name", "level")
        return(df)
    }, error = function(e){
        message(sprintf("The pathway %s was not found in the Reactome DB", p))
        df <- cbind(pathwaysSelectedSpecies[which(pathwaysSelectedSpecies$DB_ID == p), ], NA, NA, NA)
        names(df) <- c("path_ID", "name", "parent_ID", "parent_name", "level")
        return(df)
    } )
})

top_paths.df <- do.call(rbind.data.frame, top_paths)
top_paths.df$name <- gsub("^Homo sapiens: ", "", top_paths.df$name)
saveRDS(top_paths.df, "HSapiens_paths_reactome.rds")
