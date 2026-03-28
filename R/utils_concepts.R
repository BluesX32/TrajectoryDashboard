# utils_concepts.R
# OMOP concept ID registries for myositis-relevant labs and medications.
# Used to filter queries and resolve user-facing lab/drug names to concept IDs.

# ---------------------------------------------------------------------------
# Lab concept IDs
# ---------------------------------------------------------------------------

#' OMOP concept IDs for myositis-relevant laboratory tests
#'
#' A named list mapping short lab names to integer vectors of OMOP
#' measurement_concept_id values. Multiple IDs per lab cover synonymous
#' concepts across different OMOP vocabularies (LOINC, SNOMED, local).
#'
#' @format Named list of integer vectors.
#' @export
MYOSITIS_LAB_CONCEPTS <- list(
  # Muscle enzymes
  ck         = c(4013722L, 37397513L, 3013682L),  # Creatine kinase
  aldolase   = c(4013725L, 3015198L),              # Aldolase
  ast        = c(3013682L, 4146380L),              # AST
  alt        = c(3006923L, 4146377L),              # ALT
  ldh        = c(3019550L, 4149519L),              # LDH

  # Inflammatory markers
  esr        = c(3009542L),                        # ESR
  crp        = c(3020460L, 3034963L),              # CRP

  # Myositis-specific antibodies
  anti_jo1   = c(3032688L, 4085799L, 36304841L),  # Anti-Jo-1
  anti_mi2   = c(4265555L, 36304842L),             # Anti-Mi-2
  anti_mda5  = c(36304843L),                       # Anti-MDA5
  anti_tif1  = c(36304844L),                       # Anti-TIF1-gamma
  anti_hmgcr = c(36661370L),                       # Anti-HMGCR
  anti_srs   = c(36659435L),                       # Anti-SRP
  anti_nxp2  = c(36304845L),                       # Anti-NXP2
  anti_pm_scl = c(36304846L),                      # Anti-PM/Scl

  # Pulmonary function (for ILD monitoring)
  fvc        = c(3030501L),                        # FVC
  dlco       = c(3016502L)                         # DLCO
)

# Default upper limits of normal (ULN) for labs without OMOP range_high data
# These are sex-combined defaults; site-specific values override these.
.LAB_DEFAULT_ULN <- list(
  ck         = 200,    # U/L (female-biased; males typically ~300)
  aldolase   = 7.6,    # U/L
  ast        = 40,
  alt        = 56,
  ldh        = 250,
  esr        = 20,     # mm/hr (approximate; sex/age dependent)
  crp        = 10,     # mg/L
  anti_jo1   = 1.0,    # AI (antibody index; positive >= 1.0)
  anti_mi2   = 1.0,
  anti_mda5  = 1.0,
  anti_tif1  = 1.0,
  anti_hmgcr = 1.0,
  anti_srs   = 1.0,
  anti_nxp2  = 1.0,
  anti_pm_scl = 1.0,
  fvc        = NA_real_,
  dlco       = NA_real_
)

# ---------------------------------------------------------------------------
# Drug / medication concept IDs
# ---------------------------------------------------------------------------

#' OMOP concept IDs for myositis-relevant medications
#'
#' A named list mapping drug family names to integer vectors of OMOP
#' drug_concept_id values (ingredient level).
#'
#' @format Named list of integer vectors.
#' @export
MYOSITIS_DRUG_CONCEPTS <- list(
  # Corticosteroids (oral)
  prednisone         = c(1518254L),
  prednisolone       = c(1550557L, 40224172L),
  methylprednisolone = c(1506270L),
  dexamethasone      = c(1518005L),
  hydrocortisone     = c(975125L),
  triamcinolone      = c(903963L),

  # Steroid-sparing agents
  azathioprine       = c(1513103L),
  methotrexate       = c(1305058L),
  mycophenolate      = c(1550557L, 19041542L),  # MMF / MPA
  hydroxychloroquine = c(1777087L),
  leflunomide        = c(1310149L),
  cyclosporine       = c(19011459L),
  cyclophosphamide   = c(1310149L, 19041542L),
  tacrolimus         = c(950637L),

  # Biologics / infusions
  rituximab          = c(1314273L),
  ivig               = c(528323L, 35605670L, 19041569L),
  tocilizumab        = c(40161532L),
  tofacitinib        = c(42873985L),
  baricitinib        = c(1510408L),
  ruxolitinib        = c(42899491L),
  abatacept          = c(40175801L),
  belimumab          = c(40222444L),

  # Pulmonary / supportive
  nintedanib         = c(44818493L),
  pirfenidone        = c(45776887L)
)

# Drug family groupings for UI checkboxes
.DRUG_FAMILY_MAP <- list(
  Corticosteroids = c("prednisone", "prednisolone", "methylprednisolone",
                      "dexamethasone", "hydrocortisone", "triamcinolone"),
  Azathioprine    = c("azathioprine"),
  Methotrexate    = c("methotrexate"),
  Mycophenolate   = c("mycophenolate"),
  IVIG            = c("ivig"),
  Rituximab       = c("rituximab"),
  `JAK inhibitors` = c("tofacitinib", "baricitinib", "ruxolitinib"),
  Other           = c("hydroxychloroquine", "leflunomide", "cyclosporine",
                      "cyclophosphamide", "tacrolimus", "tocilizumab",
                      "abatacept", "belimumab", "nintedanib", "pirfenidone")
)

# ---------------------------------------------------------------------------
# Resolution helpers
# ---------------------------------------------------------------------------

#' Resolve a user-facing lab name to OMOP concept IDs
#'
#' @param lab_name Character(1). One of the names in [MYOSITIS_LAB_CONCEPTS].
#' @return Integer vector of concept IDs, or NULL if not found.
#' @noRd
.resolve_lab_concept <- function(lab_name) {
  key <- tolower(gsub("[- ]", "_", lab_name))
  MYOSITIS_LAB_CONCEPTS[[key]]
}

#' Resolve a drug family name to OMOP concept IDs
#'
#' @param family_name Character(1). One of the names in [.DRUG_FAMILY_MAP].
#' @return Integer vector of concept IDs.
#' @noRd
.resolve_drug_family <- function(family_name) {
  drugs <- .DRUG_FAMILY_MAP[[family_name]]
  if (is.null(drugs)) return(NULL)
  unlist(MYOSITIS_DRUG_CONCEPTS[drugs], use.names = FALSE)
}

#' Get the default ULN for a lab name
#'
#' @param lab_name Character(1). One of the names in [MYOSITIS_LAB_CONCEPTS].
#' @return Numeric(1) default ULN, or NA if unknown.
#' @noRd
.get_default_uln <- function(lab_name) {
  key <- tolower(gsub("[- ]", "_", lab_name))
  uln <- .LAB_DEFAULT_ULN[[key]]
  if (is.null(uln)) NA_real_ else uln
}

#' Map a drug_concept_name to a standardised drug family name
#'
#' @param drug_names Character vector of drug concept names.
#' @return Character vector of standardised family names.
#' @noRd
.standardize_drug_family <- function(drug_names) {
  s <- tolower(drug_names)
  dplyr::case_when(
    grepl("prednisone|prednisolone|methylpred|dexamethasone|hydrocortisone|triamcinolone", s) ~ "Corticosteroids",
    grepl("azathioprine", s) ~ "Azathioprine",
    grepl("methotrexate", s) ~ "Methotrexate",
    grepl("mycophenolate|cellcept|myfortic", s) ~ "Mycophenolate",
    grepl("intravenous immunoglobulin|ivig|immune globulin", s) ~ "IVIG",
    grepl("rituximab", s) ~ "Rituximab",
    grepl("tofacitinib|baricitinib|ruxolitinib|upadacitinib", s) ~ "JAK inhibitors",
    TRUE ~ "Other"
  )
}
