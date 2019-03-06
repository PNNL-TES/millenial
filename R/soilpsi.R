#' Compute soil hydrological properties.
#'
#' @param sand Sand (fraction)
#' @param clay Clay (fraction)
#' @param silt Silt (fraction)
#' @param vwc Volumetric water content (m3/m3)
#' @param vwcsat Volumetric water content at saturation (m3/m3)
#' @param organic Organic matter content (kg/m3)
#' @param quiet Print diagnostics?
#' @importFrom assertthat assert_that
#' @return A list containing \code{psisat} (soil water potential at saturation, MPa);
#'  \code{psi} (soil water potential, MPa); and \code{smp_l} (soil matrix potential; mm).
#' @details Soil water potential at saturation (\code{psisat}) is
#' computed from a long equation with no reference.
#' @export
#' @examples
#' soilpsi(sand = 0.4, silt = 0.4, clay = 0.2, vwc = 0.4, vwcsat = 0.8, organic = 10)
soilpsi <- function(sand, clay, silt, vwc, vwcsat, organic, quiet = FALSE) {
  assert_that(sand >= 0)
  assert_that(silt >= 0)
  assert_that(clay >= 0)
  assert_that(all.equal(sand + silt + clay, 1.0))
  # 825 !	hydrological properties start
  # 826 subroutine soilpsi(sand, clay, silt, vwc, vwcsat, organic, psisat, psi, smp_l)
  # 827 implicit none
  # 828 	integer,parameter 	:: r8 = selected_real_kind(12) 		! 8 byte real
  # 829 	real, intent(in) 		:: sand 		! percentage
  # 830 	real, intent(in) 		:: clay 		! percentage
  # 831 	real, intent(in)		:: silt 		! percentage
  # 832 	real, intent(in) 		:: vwc
  # 833 	real, intent(in) 		:: vwcsat
  # 834 	real, intent(in)		:: organic   	! read-in the organic matter content kg / m3
  # 835 	real, intent(out) 		:: psisat
  # 836 	real, intent(out) 		:: psi
  # 837 	real, intent(out)		:: smp_l 		! soil matric potential (mm)
  # 838 	real					:: bsw 		! Clapp and Hornberger "b"
  # 839 	real					:: bsw2 		! Clapp and Hornberger "b" for CN module
  # 840 	real					:: smp 		! msoil matrix potential  it seems this is exactly same as smp_l
  # 841 	real					:: sucsat  		! minimum soil suction
  # 842 	real					:: s_node 		! soil wetness
  # 843 	real					:: smpmin 	! restriction for min of soil potential
  # 844 	real					:: om_frac 	! organic matter fraction
  # 845 	real					:: om_b 		! Clapp Hornberger parameter for organic soil (letts, 2000) Line 188 of iniTimeConst.F90
  # 846 	real					:: organic_max  ! orgnaic matter hwere oil is assumed to act like peat
  # 847 	real					:: om_sucsat 	! saturated suction for organic matter (Lets, 2000)
  # 848
  # 849 	om_sucsat = 10.3_r8    				! saturated suction for organic matter (Lets, 2000)
  # 850 	smpmin = -1._r8    					! restriction for min of soil potential line 750 of iniTimeConst.F90
  # 851 	organic_max = 130._r8
  # 852 	om_b = 2.7_r8
  om_sucsat <- 10.3    	# saturated suction for organic matter (Letts, 2000)
  smpmin <- -1    	    # restriction for min of soil potential line 750 of iniTimeConst.F90
  organic_max <- 130
  assert_that(organic > 0)
  assert_that(organic <= organic_max)
  om_b <- 2.7
  # 853 !	print * , vwc, vwcsat
  # 854 	if(vwc > vwcsat) then
  # 855 	write(*,*) 'vwcsat is less than vwc'
  # 856 	end if
  assert_that(vwc >= 0)
  assert_that(vwc <= vwcsat)
  # 857
  # 858 	om_frac = min(organic / organic_max, 1._r8)
  om_frac <- min(organic / organic_max, 1)
  # 859 	sucsat = 10. * ( 10.**(1.88-0.0131*sand) )
  sucsat <- 10 * (10 ^ (1.88 - 0.0131 * sand))
  # 860 	bsw = (1.-om_frac)*(2.91 + 0.159*clay) + om_frac*om_b
  bsw <- (1 - om_frac) * (2.91 + 0.159 * clay) + om_frac * om_b
  # 861 	sucsat = (1.-om_frac)*sucsat + om_sucsat*om_frac
  sucsat <- (1 - om_frac) * sucsat + om_sucsat * om_frac
  # 862 	s_node = min(1.0, max(vwc / vwcsat, 0.01))
  s_node <- min(1.0, max(vwc / vwcsat, 0.01))
  # 863 	smp = max(smpmin, (-sucsat * (s_node ** (-bsw))))
  smp <- max(smpmin, (-sucsat * (s_node ^ -bsw)))
  # 864 	smp_l = smp
  smp_l <- smp
  # 865 	bsw2 = -(3.1 + 0.157 * clay - 0.003 * sand)
  bsw2 <- -(3.1 + 0.157 * clay - 0.003 * sand)
  # 866 	psisat = -(exp((1.54 - 0.0095*sand + 0.0063*(100.0-sand-clay))*log(10.0))*9.8e-5_r8)
  psisat = -(exp((1.54 - 0.0095 * sand + 0.0063 * (100.0 - sand - clay)) * log(10.0)) * 9.8e-5)
  # 867 	psi = psisat * ((vwc/vwcsat)**bsw2)
  psi <- psisat * ((vwc / vwcsat) ^ bsw2)
  # 868 !	print *, "psi: ", psi,psisat, bsw2, smp_l, sucsat, bsw, om_frac,organic,om_b
  if(!quiet) cat("psi: ", psi, psisat, bsw2, smp_l, sucsat, bsw, om_frac, organic, om_b, "\n")
  # 869 end subroutine soilpsi
  # 870 !	hydrological properties end
  list(psisat = psisat, psi = psi, smp_l = smp_l)
}
