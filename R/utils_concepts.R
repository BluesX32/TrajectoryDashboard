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
  dlco       = c(3016502L),                        # DLCO

  # --- Safety monitoring / cardiac (verify concept IDs against local OMOP) ---
  ferritin   = c(3013272L, 3007461L),              # Ferritin (MAS warning in MDA5+)
  troponin_i = c(3016723L, 4208432L),              # Troponin-I (cardiac myositis)
  bnp        = c(3028437L),                        # BNP (cardiac myositis)
  wbc        = c(3010813L),                        # White blood cell count
  lymphocytes = c(3004327L),                       # Lymphocyte count (cytopenia watch)
  hemoglobin = c(3000963L),                        # Hemoglobin (anemia of inflammation)
  creatinine = c(3051825L, 3016723L)               # Creatinine (renal safety)
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
  dlco       = NA_real_,

  # Safety monitoring
  ferritin   = 200,     # ng/mL (MAS concern threshold is much higher, ~1500; ULN is ~200)
  troponin_i = 0.04,    # ng/mL (99th percentile URL)
  bnp        = 100,     # pg/mL
  wbc        = 10.5,    # K/µL
  lymphocytes = 3.4,    # K/µL (lower reference ~1.0; danger threshold 0.5)
  hemoglobin = NA_real_, # g/dL (sex-dependent; no universal default)
  creatinine = 1.2      # mg/dL (sex-combined approximate)
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
  # Corticosteroids
  prednisone         = c(1518254L, 19014878L),
  prednisolone       = c(1550557L, 40224172L),
  methylprednisolone = c(1506270L, 19068900L),
  dexamethasone      = c(1518005L),
  hydrocortisone     = c(975125L),
  triamcinolone      = c(903963L),
  budesonide         = c(19003999L),

  # csDMARDs
  methotrexate       = c(1305058L),
  leflunomide        = c(1310317L),
  sulfasalazine      = c(1314273L),
  hydroxychloroquine = c(1777087L),
  chloroquine        = c(1777087L),

  # Antimetabolites
  azathioprine       = c(1513103L),
  mycophenolate      = c(1361580L, 1593700L),               # MMF / MPA

  # Calcineurin inhibitors (CNI)
  tacrolimus         = c(950637L),
  voclosporin        = c(45892883L),
  cyclosporine       = c(19011459L, 1101898L),

  # Alkylating
  cyclophosphamide   = c(40171288L, 1594587L),

  # TNF inhibitors
  infliximab         = c(937368L),
  adalimumab         = c(1119119L),
  golimumab          = c(40161534L),                         # verify concept ID
  certolizumab       = c(40174007L),                         # certolizumab pegol; verify
  etanercept         = c(1151789L),

  # IL-6 inhibitors
  tocilizumab        = c(40161532L),
  sarilumab          = c(1594982L),                          # verify concept ID

  # IL-12/23 inhibitors
  ustekinumab        = c(1592188L),                          # verify concept ID
  risankizumab       = c(1510599L),                          # verify concept ID
  guselkumab         = c(1509310L),                          # verify concept ID

  # IL-17 inhibitors
  secukinumab        = c(1186087L),
  ixekizumab         = c(40161530L),                         # verify concept ID
  bimekizumab        = c(40244265L),                         # verify concept ID; very new

  # Type 1 IFN inhibitor
  anifrolumab        = c(1511348L),

  # JAK inhibitors
  tofacitinib        = c(42873985L),
  baricitinib        = c(1510408L),
  ruxolitinib        = c(42899491L),
  upadacitinib       = c(1151789L),
  filgotinib         = c(1777087L),                          # verify concept ID

  # T-cell co-stimulation inhibition
  abatacept          = c(40175801L),

  # BAFF inhibitors
  belimumab          = c(40222444L),

  # CD19/CD20
  rituximab          = c(1119119L),
  obinutuzumab       = c(43025770L),                         # verify concept ID

  # IVIG
  ivig               = c(528323L, 35605670L, 19041569L, 701470L),

  # Pulmonary antifibrotics
  nintedanib         = c(44818493L),
  pirfenidone        = c(45776887L),

  # Other
  sirolimus          = c(1594587L)
)

# Drug family groupings aligned to standard DMARD class taxonomy.
# Used for UI checkboxes, toxicity rule drug_selector matching, and taper detection.
.DRUG_FAMILY_MAP <- list(
  Corticosteroids         = c("prednisone", "prednisolone", "methylprednisolone",
                               "dexamethasone", "hydrocortisone", "triamcinolone",
                               "budesonide"),
  IVIG                    = c("ivig"),

  # csDMARDs
  csDMARD                 = c("methotrexate", "leflunomide", "sulfasalazine",
                               "hydroxychloroquine", "chloroquine"),

  # Antimetabolites
  Antimetabolite          = c("azathioprine", "mycophenolate"),

  # Calcineurin inhibitors
  CNI                     = c("tacrolimus", "voclosporin", "cyclosporine"),

  # Alkylating
  Alkylating              = c("cyclophosphamide"),

  # bDMARDs by target
  `TNF inhibitors`        = c("infliximab", "adalimumab", "golimumab",
                               "certolizumab", "etanercept"),
  `IL-6 inhibitors`       = c("tocilizumab", "sarilumab"),
  `IL-12/23 inhibitors`   = c("ustekinumab", "risankizumab", "guselkumab"),
  `IL-17 inhibitors`      = c("secukinumab", "ixekizumab", "bimekizumab"),
  `Type 1 IFN inhibitors` = c("anifrolumab"),
  `JAK inhibitors`        = c("tofacitinib", "baricitinib", "ruxolitinib",
                               "upadacitinib", "filgotinib"),
  `T-cell inhibitors`     = c("abatacept"),
  `BAFF inhibitors`       = c("belimumab"),
  `CD19/CD20`             = c("rituximab", "obinutuzumab"),

  # Pulmonary antifibrotics
  Nintedanib              = c("nintedanib"),
  Pirfenidone             = c("pirfenidone"),

  # Other
  Sirolimus               = c("sirolimus")
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

#' Build the lab picker choices list for the UI
#'
#' Returns either a grouped list (myositis preset) or a flat named vector
#' derived from `config$lab_concepts` keys for generic configs.
#'
#' @param config A `dashboard_config` object.
#' @return A named list or named character vector suitable for
#'   `shiny::selectInput(choices = ...)`.
#' @noRd
.build_lab_picker_choices <- function(config) {
  if (inherits(config, "myositis_dashboard_config")) {
    # Full grouped picker with human-readable labels (existing behaviour)
    list(
      "Muscle Enzymes" = c(
        "CK (Creatine Kinase)" = "ck",
        "Aldolase"             = "aldolase",
        "AST"                  = "ast",
        "ALT"                  = "alt",
        "LDH"                  = "ldh"
      ),
      "Inflammatory" = c(
        "ESR"                  = "esr",
        "CRP"                  = "crp"
      ),
      "Myositis Antibodies" = c(
        "Anti-Jo-1"            = "anti_jo1",
        "Anti-Mi-2"            = "anti_mi2",
        "Anti-MDA5"            = "anti_mda5",
        "Anti-TIF1-γ"     = "anti_tif1",
        "Anti-HMGCR"           = "anti_hmgcr",
        "Anti-SRP"             = "anti_srs",
        "Anti-NXP2"            = "anti_nxp2",
        "Anti-PM/Scl"          = "anti_pm_scl"
      ),
      "Pulmonary" = c(
        "FVC"                  = "fvc",
        "DLCO"                 = "dlco"
      ),
      "Cardiac & Safety" = c(
        "Ferritin"             = "ferritin",
        "Troponin-I"           = "troponin_i",
        "BNP"                  = "bnp",
        "WBC"                  = "wbc",
        "Lymphocytes"          = "lymphocytes",
        "Hemoglobin"           = "hemoglobin",
        "Creatinine"           = "creatinine"
      )
    )
  } else {
    # Flat list: auto-derive display labels from concept keys
    keys <- names(config$lab_concepts)
    labels <- gsub("_", " ", toupper(keys))
    stats::setNames(keys, labels)
  }
}

#' Map a drug_concept_name to a standardised drug family name
#'
#' @param drug_names Character vector of drug concept names.
#' @return Character vector of standardised family names.
#' @noRd
.standardize_drug_family <- function(drug_names) {
  s <- tolower(drug_names)
  dplyr::case_when(
    grepl("prednisone|prednisolone|methylpred|dexamethasone|hydrocortisone|triamcinolone|budesonide", s) ~ "Corticosteroids",
    grepl("intravenous immunoglobulin|ivig|immune globulin", s) ~ "IVIG",
    grepl("methotrexate|leflunomide|sulfasalazine|hydroxychloroquine|chloroquine", s) ~ "csDMARD",
    grepl("azathioprine|mycophenolate|cellcept|myfortic", s) ~ "Antimetabolite",
    grepl("tacrolimus|voclosporin|cyclosporine|ciclosporin", s) ~ "CNI",
    grepl("cyclophosphamide", s) ~ "Alkylating",
    grepl("infliximab|adalimumab|golimumab|certolizumab|etanercept", s) ~ "TNF inhibitors",
    grepl("tocilizumab|sarilumab", s) ~ "IL-6 inhibitors",
    grepl("ustekinumab|risankizumab|guselkumab", s) ~ "IL-12/23 inhibitors",
    grepl("secukinumab|ixekizumab|bimekizumab", s) ~ "IL-17 inhibitors",
    grepl("anifrolumab", s) ~ "Type 1 IFN inhibitors",
    grepl("tofacitinib|baricitinib|ruxolitinib|upadacitinib|filgotinib", s) ~ "JAK inhibitors",
    grepl("abatacept", s) ~ "T-cell inhibitors",
    grepl("belimumab", s) ~ "BAFF inhibitors",
    grepl("rituximab|obinutuzumab", s) ~ "CD19/CD20",
    grepl("nintedanib", s) ~ "Nintedanib",
    grepl("pirfenidone", s) ~ "Pirfenidone",
    grepl("sirolimus", s) ~ "Sirolimus",
    TRUE ~ NA_character_
  )
}
