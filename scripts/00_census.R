# Definitions follow the SHRUG 2001 Village Directory metadata.
# https://docs.devdatalab.org/SHRUG-Metadata/Population%20Census/Tables/vd01-metadata/
CENSUS_AVAILABILITY <- c(
  "pc01_vd_edu_fac", "pc01_vd_medi_fac", "pc01_vd_power_supl",
  "pc01_vd_bank_fac", "pc01_vd_app_mr"
)

census_availability <- function(x) {
  stopifnot(all(is.na(x) | x %in% 0:1))
  if (any(x == 1, na.rm = TRUE)) return(1L)
  if (anyNA(x)) return(NA_integer_)
  0L
}

CENSUS_BALANCE <- c(
  pc01_vd_t_p = "Population", female_share = "Female (\\%)",
  sc_share = "SC (\\%)", st_share = "ST (\\%)",
  medical_available = "Medical facility (\\%)", m_home_per1k = "Maternity homes/1k",
  mcw_per1k = "Maternal/child centers/1k", p_sch_per1k = "Primary schools/1k",
  m_sch_per1k = "Middle schools/1k", banking_available = "Banking (\\%)",
  power_available = "Power supply (\\%)", road_available = "Mud road (\\%)"
)
CENSUS_BALANCE_PERCENT <- c(
  "female_share", "sc_share", "st_share", "medical_available",
  "banking_available", "power_available", "road_available"
)

census_balance_covariates <- function(data) {
  dplyr::mutate(data,
    female_share = dplyr::if_else(pc01_vd_t_p > 0, pc01_vd_t_f / pc01_vd_t_p, NA_real_),
    sc_share = dplyr::if_else(pc01_vd_t_p > 0, pc01_vd_sc_p / pc01_vd_t_p, NA_real_),
    st_share = dplyr::if_else(pc01_vd_t_p > 0, pc01_vd_st_p / pc01_vd_t_p, NA_real_),
    medical_available = pc01_vd_medi_fac,
    m_home_per1k = dplyr::if_else(pc01_vd_t_p > 0, 1000 * pc01_vd_m_home / pc01_vd_t_p, NA_real_),
    mcw_per1k = dplyr::if_else(pc01_vd_t_p > 0, 1000 * pc01_vd_mcw_cntr / pc01_vd_t_p, NA_real_),
    p_sch_per1k = dplyr::if_else(pc01_vd_t_p > 0, 1000 * pc01_vd_p_sch / pc01_vd_t_p, NA_real_),
    m_sch_per1k = dplyr::if_else(pc01_vd_t_p > 0, 1000 * pc01_vd_m_sch / pc01_vd_t_p, NA_real_),
    banking_available = pc01_vd_bank_fac, power_available = pc01_vd_power_supl,
    road_available = pc01_vd_app_mr
  )
}
