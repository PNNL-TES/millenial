#' Compute soil hydrological properties.
#'
#' @param sand Sand (percent)
#' @param silt Silt (percent)
#' @param clay Clay (percent)
#' @param vwc Volumetric water content (m3/m3)
#' @param vwcsat Volumetric water content at saturation (m3/m3)
#' @param organic Organic matter content (kg/m3)
#' @param quiet Print diagnostics?
#' @importFrom assertthat assert_that
#' @return A list containing \code{psisat} (soil water potential at saturation, MPa);
#'  \code{psi} (soil water potential, MPa); and \code{smp_l} (soil matrix potential; mm).
#' @details The calculations generally follow Clapp & Hornberger (1978),
#' "Empirical equations for some soil hydraulic properties",
#' \url{https://doi.org/10.1029/WR014i004p00601}. Peatland calculations are based on
#' Letts et al. (2000), "Parametrization of peatland hydraulic properties
#' for the Canadian land surface scheme",
#' \url{https://doi.org/10.1080/07055900.2000.9649643}.
#' @export
#' @examples
#' soilpsi(sand = 40, silt = 40, clay = 20, vwc = 0.4, vwcsat = 0.8, organic = 10)
soilpsi <- function(sand, clay, silt, vwc, vwcsat, organic, quiet = FALSE) {
  assert_that(sand >= 0)
  assert_that(silt >= 0)
  assert_that(clay >= 0)
  assert_that(all.equal(sand + silt + clay, 100.0))
  assert_that(vwc >= 0)
  assert_that(vwc <= vwcsat)
  assert_that(organic > 0)

  # The following calculations generally follow Clapp & Hornberger 1978,
  # "Empirical equations for some soil hydraulic properties"
  # https://doi.org/10.1029/WR014i004p00601

  # Peatland calculation after Letts et al. 2000, "Parametrization of
  # peatland hydraulic properties for the Canadian land surface scheme"
  # https://doi.org/10.1080/07055900.2000.9649643

  om_sucsat <- 10.3    	# saturated suction for organic matter (Letts, 2000)
  smpmin <- -1    	    # restriction for min of soil potential line 750 of iniTimeConst.F90 (CLM)
  organic_max <- 130
  assert_that(organic <= organic_max)

  om_b <- 2.7 # Clapp and Hornberger parameter for oragnic soil (Letts, 2000)

  om_frac <- min(organic / organic_max, 1)

  sucsat <- 10 * (10 ^ (1.88 - 0.0131 * sand))
  bsw <- (1 - om_frac) * (2.91 + 0.159 * clay) + om_frac * om_b  # Clapp and Hornberger "b"
  sucsat <- (1 - om_frac) * sucsat + om_sucsat * om_frac # minimum soil suction
  s_node <- min(1.0, max(vwc / vwcsat, 0.01)) # soil wetness

  # Calculate outputs
  smp_l <- max(smpmin, (-sucsat * (s_node ^ -bsw)))
  bsw2 <- -(3.1 + 0.157 * clay - 0.003 * sand)
  psisat = -(exp((1.54 - 0.0095 * sand + 0.0063 * silt) * log(10.0)) * 9.8e-5)
  psi <- psisat * ((vwc / vwcsat) ^ bsw2)

  if(!quiet) {
    cat("psi: ", psi, psisat, bsw2, smp_l, sucsat, bsw, om_frac, organic, om_b, "\n")
  }

  list(psisat = psisat, psi = psi, smp_l = smp_l)
}
