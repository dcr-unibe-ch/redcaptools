#' automatically check for NA on a list of variables, account for branching logic using the REDCap metadata

#' iterate the rules up, label them R_na_xxx, where xxx is the rule number to show in the working_list at the very end
#' check for branching logic, also for fields that are embedded elsewhere
#' correctly translate REDCap logic into R logic



#' ----------Translate REDCap Branching Logic into R Syntax Expressions----------
#'
#' Takes a REDCap branching logic string and transforms it into an equivalent 
#' R logical expression compatible with \code{\link[validate]{confront}} or 
#' standard R evaluations.
#'
#' @details The function sequentially applies regex translations for:
#' \itemize{
#'   \item Custom missingness functions (e.g., \code{isblanknotmissingcode()})
#'   \item REDCap event and arm tags (e.g., \code{[event_arm_1]})
#'   \item Date differences (\code{datediff()} translated to R \code{as.Date()} arithmetic)
#'   \item REDCap checkbox fields (e.g., \code{[var(1)]} to \code{var___1})
#'   \item Blank/missing checks (e.g., \code{[var] = ''} to \code{is.na(var)})
#'   \item Logical and equality operators (\code{AND}/\code{OR} to \code{\&}/\code{|}, \code{=} to \code{==}, \code{<>} to \code{!=})
#'   \item Square field brackets (e.g., \code{[var]} to \code{var})
#' }
#'
#' @param logic_str Character scalar. The raw REDCap branching logic string to 
#'   be translated.
#'
#' @return Character scalar containing the translated R logical expression. 
#'   If \code{logic_str} is \code{NA} or an empty string, returns \code{NA_character_}.
#'
#' @examples
#' transform_redcap_branching("[age] >= 18 AND [sex] = '1'")
#' # Returns: "age >= 18 & sex == '1'"
#'
#' transform_redcap_branching("[med_check(1)] = '1' OR [notes] <> ''")
#' # Returns: "med_check___1 == '1' | !is.na(notes)"
#'
#' @export

transform_redcap_branching <- function(logic_str) {
  if (is.na(logic_str) || trimws(logic_str) == "") return(NA_character_)
  
  res <- logic_str
  
  # 1. Translate REDCap missing-code functions
  res <- gsub("!isblanknotmissingcode\\(([^)]+)\\)", "(!is.na(\\1) & \\1 != \"\")", res, ignore.case = TRUE)
  res <- gsub("isblanknotmissingcode\\(([^)]+)\\)", "(is.na(\\1) | \\1 == \"\")", res, ignore.case = TRUE)
  
  # 2. Strip REDCap event prefixes like [12_monate_followup_arm_1][var_name] -> [var_name]
  # Or [12_monate_followup_arm_1fup12_var] -> [fup12_var]
  res <- gsub("\\[[a-zA-Z0-9_]+_arm_[0-9]+\\]\\s*", "", res) # Remove standalone event brackets
  res <- gsub("(?<=\\(|\\s|^)[0-9]+_[a-zA-Z0-9_]*fup", "fup", res, perl = TRUE) # Clean leading digits in event-prefixed names
  
  # 3. Translate datediff (use a placeholder for format assignment)
  res <- gsub(
    "datediff\\s*\\(\\s*([^,]+)\\s*,\\s*([^,]+)\\s*,\\s*['\"]d['\"]\\s*(?:,\\s*[^)]+)?\\)",
    "as.numeric(as.Date(\\1) - as.Date(\\2, __FORMAT_ASSIGNMENT__))",
    res,
    ignore.case = TRUE
  )
  
  # 4. Convert REDCap Checkbox Syntax [var(code)] -> var___code
  res <- gsub("\\[([a-zA-Z0-9_]+)\\(([a-zA-Z0-9_]+)\\)\\]", "\\1___\\2", res)
  
  # 5. Standard REDCap missing/not-missing checks
  res <- gsub("\\[([a-zA-Z0-9_]+)\\]\\s*=\\s*''", "is.na(\\1)", res, ignore.case = TRUE)
  res <- gsub("\\[([a-zA-Z0-9_]+)\\]\\s*=\\s*\"\"", "is.na(\\1)", res, ignore.case = TRUE)
  res <- gsub("\\[([a-zA-Z0-9_]+)\\]\\s*<>\\s*''", "!is.na(\\1)", res, ignore.case = TRUE)
  res <- gsub("\\[([a-zA-Z0-9_]+)\\]\\s*<>\\s*\"\"", "!is.na(\\1)", res, ignore.case = TRUE)
  
  # 6. Convert equality and inequality operators
  res <- gsub("<>", "!=", res)
  res <- gsub("(?<=[^<>!=])=(?=[^=])", "==", res, perl = TRUE)
  
  # 7. Restore the R parameter assignment format = '%d-%m-%Y'
  res <- gsub("__FORMAT_ASSIGNMENT__", "format = '%d-%m-%Y'", res, fixed = TRUE)
  
  # 8. Convert logical connectors
  res <- gsub("\\bAND\\b", "&", res, ignore.case = TRUE)
  res <- gsub("\\bOR\\b", "|", res, ignore.case = TRUE)
  
  # 9. Remove remaining single square brackets
  res <- gsub("\\[([a-zA-Z0-9_]+)\\]", "\\1", res)
  
  return(trimws(res))
}




#' ----------Extract embedded variables from descriptive fields----------
#' Scans REDCap metadata for descriptive fields containing embedded variables 
#' (formatted like \code{\{variable_name\}}) within their field labels. 
#' Maps those embedded variables to the branching logic of their parent descriptive field.
#'
#' @param metadata A data frame or tibble containing REDCap project metadata 
#'   (typically \code{m$metadata}). Must include columns \code{field_type}, 
#'   \code{field_label}, \code{field_name}, and \code{branching_logic}.
#'
#' @return A \code{\link[tibble]{tibble}} with two columns:
#'   \describe{
#'     \item{\code{var}}{Character. The embedded variable name extracted from the curly braces.}
#'     \item{\code{desc_branching}}{Character. The branching logic of the parent descriptive field.}
#'   }
#'   If no descriptive fields with embedded variables exist, returns an empty 
#'   tibble with zero rows and those two character columns.
#'
#' @examples
#' \dontrun{
#' embedded_map <- get_embedded_logic_map(main_meta)
#' }
#' @export


get_embedded_logic_map <- function(metadata) {
  desc_fields <- metadata |> 
    filter(field_type == "descriptive", !is.na(field_label)) |> 
    select(field_name, field_label, branching_logic)
  
  if (nrow(desc_fields) == 0) return(tibble(var = character(), desc_branching = character()))
  
  desc_fields |> 
    mutate(
      embedded_vars = map(field_label, ~ {
        str_extract_all(.x, "(?<=\\{)[^\\}]+(?=\\})")[[1]] |> 
          str_remove(":[a-z_]+$") |> 
          unique()
      })
    ) |> 
    unnest(embedded_vars) |> 
    filter(!is.na(embedded_vars) & embedded_vars != "") |> 
    select(var = embedded_vars, desc_branching = branching_logic) |> 
    filter(!is.na(desc_branching) & desc_branching != "")
}





#' ----------Generate Smart Conditional NA Rules for REDCap Variables----------
#'
#' Evaluates a list of target variables against REDCap metadata to generate 
#' vectorized missingness rules compatible with \code{\link[validate]{confront}}. 
#' Rules automatically adjust for REDCap instrument completion status and 
#' complex branching logic, ignoring incomplete ("gray") forms.
#'
#' @details The function handles several REDCap metadata edge cases:
#' \itemize{
#'   \item Exact variable name matching with fallback to stripped field names 
#'         (e.g., removing suffixes like \code{_factor}, \code{_date}, or index/slot numbers like \code{_7}).
#'   \item Automated detection of form completion status columns (\code{<form_name>_complete}).
#'   \item Mapping embedded branching logic from parent descriptive fields via \code{get_embedded_logic_map()}.
#'   \item Translating REDCap branching logic to R code via \code{transform_redcap_branching()}.
#'   \item Column substitution for factor versions of variables (\code{<var>_factor}) present in the target dataset.
#' }
#'
#' @param vars Character vector. Names of dataset columns for which to generate NA validation rules.
#' @param data Data frame or tbl. The active dataset being validated.
#' @param data_name Character scalar. The string identifier for the dataset (e.g., \code{"krank"}).
#' @param metadata Data frame. The REDCap project metadata/data dictionary (e.g., \code{m$metadata}).
#' @param rule_df Tibble or NULL. An existing rules tibble to append new rules to. If \code{NULL}, a new tibble is initialized.
#' @param dataset Character scalar, optional. A broad category label for the rule dataframe (e.g., \code{"dose"}). Defaults to \code{NA_character_}.
#' @param form_name Character scalar, optional. Explicit REDCap instrument name to override automatic metadata lookup. Defaults to \code{NULL}.
#'
#' @return A \code{\link[tibble]{tibble}} containing the existing rules plus newly appended rules with columns:
#'   \describe{
#'     \item{\code{name}}{Sequential rule identifier (e.g., \code{"R_na_001"}).}
#'     \item{\code{dataset}}{Category identifier specified by the \code{dataset} argument.}
#'     \item{\code{var}}{Name of the validated target variable.}
#'     \item{\code{description}}{Human-readable explanation of the validation condition.}
#'     \item{\code{data}}{Name of the evaluation dataset specified by \code{data_name}.}
#'     \item{\code{rule}}{The generated R logical expression string ready for \code{confront()}.}
#'   }
#'
#' @examples
#' \dontrun{
#' rules <- add_smart_na_rules(
#'   vars = c("bl_gp_ppi_yn_1", "fup9_gp_ppi_yn_7"),
#'   data = krank,
#'   data_name = "krank",
#'   metadata = main_meta,
#'   rule_df = NULL,
#'   dataset = "gp_meds"
#' )
#' }
#'
#' @export

add_smart_na_rules <- function(vars, 
                               data, 
                               data_name, 
                               metadata, 
                               rule_df = NULL, 
                               dataset = NA_character_,
                               form_name = NULL) {
  
  if (is.null(data) || length(vars) == 0) return(rule_df)
  
  dat_cols <- colnames(data)
  vars <- intersect(vars, dat_cols)
  
  if (length(vars) == 0) return(rule_df)

  # --- Data Inspection Warning ---
  comp_cols <- grep("_complete(_factor)?$", colnames(data), value = TRUE)
  
  if (length(comp_cols) > 0) {
    # Check if complete columns have 0s but zero NAs (typical when exportBlankForGrayFormStatus is FALSE/omitted)
    has_zeros <- any(sapply(dat[comp_cols], function(x) any(x == 0, na.rm = TRUE)))
    has_nas   <- any(sapply(dat[comp_cols], function(x) any(is.na(x))))
    
    if (has_zeros && !has_nas) {
      warning(
        "Detected '0' values in instrument completion status columns with zero NAs. ",
        "If unstarted (gray) forms were exported as '0' instead of NA, validation rules will fail. ",
        "Ensure REDCap API call uses `exportBlankForGrayFormStatus = 'true'`.",
        call. = FALSE
      )
    }
  }
  # --------------------------------
  
  # 1. Map exported variables: store both exact name and stripped variant
  mapping_df <- tibble::tibble(dat_var = vars) |> 
    dplyr::mutate(
      stripped_field = stringr::str_remove(dat_var, "_(factor|date|datetime|calc|dmy|ymd)$"),
      stripped_field = stringr::str_remove(stripped_field, "_\\d+$")
    )
  
  # 2. Get Embedded Variables Map
  embedded_map <- get_embedded_logic_map(metadata)
  
  # 3. Match metadata: Try exact dat_var match FIRST, then fall back to stripped_field
  var_meta <- mapping_df |> 
    dplyr::left_join(
      metadata |> dplyr::select(field_name, form_name_meta = form_name, branching_logic), 
      by = c("dat_var" = "field_name")
    ) |> 
    dplyr::left_join(
      metadata |> dplyr::select(field_name, form_name_strip = form_name, branching_logic_strip = branching_logic), 
      by = c("stripped_field" = "field_name")
    ) |> 
    dplyr::mutate(
      # Coalesce metadata fields (exact match > stripped match)
      form_name_meta = dplyr::coalesce(form_name_meta, form_name_strip),
      branching_logic = dplyr::coalesce(branching_logic, branching_logic_strip),
      
      # Use provided form_name override if available, else metadata form_name
      target_form = dplyr::coalesce(form_name, form_name_meta),
      
      # Determine expected completion status column name
      status_col = dplyr::case_when(
        !is.na(target_form) & paste0(target_form, "_complete") %in% dat_cols ~ paste0(target_form, "_complete"),
        !is.na(target_form) & paste0(target_form, "_complete_factor") %in% dat_cols ~ paste0(target_form, "_complete_factor"),
        TRUE ~ NA_character_
      ),
      
      has_status = !is.na(status_col)
    )
  
  # 4. Process embedded branching logic
  var_meta <- var_meta |> 
    dplyr::left_join(embedded_map, by = c("dat_var" = "var")) |> 
    dplyr::mutate(
      combined_redcap_logic = dplyr::case_when(
        !is.na(branching_logic) & branching_logic != "" & !is.na(desc_branching) ~ 
          paste0("(", desc_branching, ") AND (", branching_logic, ")"),
        !is.na(branching_logic) & branching_logic != "" ~ branching_logic,
        !is.na(desc_branching) & desc_branching != "" ~ desc_branching,
        TRUE ~ NA_character_
      )
    )
  
  # 5. Translate REDCap logic and construct vectorized expressions
  translated_rules <- var_meta |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      r_logic = {
        if (is.na(combined_redcap_logic) || trimws(combined_redcap_logic) == "") {
          NA_character_
        } else {
          r_cond <- transform_redcap_branching(combined_redcap_logic)
          tokens <- unique(stringr::str_extract_all(r_cond, "\\b[a-zA-Z0-9_]+\\b")[[1]])
          for (tok in tokens) {
            if (!tok %in% dat_cols && paste0(tok, "_factor") %in% dat_cols) {
              r_cond <- gsub(paste0("\\b", tok, "\\b"), paste0(tok, "_factor"), r_cond)
            }
          }
          r_cond
        }
      },
      
      # Construct final condition
      final_condition = dplyr::case_when(
        # Has status column AND branching logic
        has_status & !is.na(r_logic) & trimws(r_logic) != "" ~ 
          sprintf("!is.na(%s) & (%s)", status_col, r_logic),
        
        # Has status column BUT NO branching logic
        has_status ~ 
          sprintf("!is.na(%s)", status_col),
        
        # No status column BUT has branching logic
        !is.na(r_logic) & trimws(r_logic) != "" ~ 
          r_logic,
        
        TRUE ~ NA_character_
      ),
      
      rule_expr = dplyr::if_else(
        !is.na(final_condition),
        sprintf("!(%s) | !is.na(%s)", final_condition, dat_var),
        sprintf("!is.na(%s)", dat_var)
      ),
      
      description = dplyr::if_else(
        !is.na(final_condition),
        sprintf("Value should not be missing when form is active and condition (%s) is met", final_condition),
        "Value should not be missing"
      )
    ) |> 
    dplyr::ungroup()
  
  # 6. Determine next R_na_xxx index
  if (is.null(rule_df) || nrow(rule_df) == 0) {
    next_num <- 1
    rule_df <- tibble::tibble(
      name = character(), dataset = character(), var = character(), 
      description = character(), data = character(), rule = character()
    )
  } else {
    existing <- rule_df |> dplyr::filter(stringr::str_detect(name, "^R_na_"))
    next_num <- if (nrow(existing) == 0) 1 else max(as.numeric(sub("^R_na_", "", existing$name)), na.rm = TRUE) + 1
  }
  
  # 7. Append new rules
  new_rules <- tibble::tibble(
    name = sprintf("R_na_%03d", seq(next_num, length.out = nrow(translated_rules))),
    dataset = dataset,
    var = translated_rules$dat_var,
    description = translated_rules$description,
    data = data_name,
    rule = translated_rules$rule_expr
  )
  
  dplyr::bind_rows(rule_df, new_rules)
}


