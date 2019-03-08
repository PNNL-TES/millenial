#' Decomposition - CENTURY
#'
#' @param forc_st Soil temperature (K)
#' @param psi Soil water potential, MPa
#' @param DOC Dissolved organic carbon pool
#' @param ACTIVE Active soil organic carbon pool
#' @param SLOW Slow soil organic carbon pool
#' @param PASSIVE Passive soil organic carbon pool
#' @param timestep Timestep (days)
#'
#' @return A list containing DOC, ACTIVE,
#' SLOW, PASSIVE, f_ACTIVE_ATM, f_PASSIVE_ATM,
#' f_SLOW_ATM, f_ACTIVE_DOC, f_ACTIVE_SLOW, f_SLOW_PASSIVE,
#' f_ACTIVE_PASSIVE, f_PASSIVE_ACTIVE, f_DOC_Leaching, and f_DOC_ATM.
#' @export
#' @examples
#' decomp_century(forc_st = 15, psi = -0.5, DOC = 0.1, ACTIVE = 1, SLOW = 10, PASSIVE = 100, 1)
decomp_century <- function(forc_st, psi, DOC, ACTIVE, SLOW, PASSIVE, timestep) {
  # 873 !	decomposition subroutine CENTURY start
  # 874 !~ subroutine decomp_century(forc_st, psi, forc_npp, forc_roots, &
  # 875 		!~ forc_exoenzyme, clay, DOC, ACTIVE, SLOW, PASSIVE, f_DOC_ATM, f_ACTIVE_ATM,&
  # 876 		!~ f_PASSIVE_ATM, f_SLOW_ATM,f_ACTIVE_DOC, f_ACTIVE_SLOW, f_SLOW_PASSIVE, f_ACTIVE_PASSIVE,&
  # 877 		!~ f_PASSIVE_ACTIVE, f_DOC_Leaching)
  # 878
  # 879 	!~ implicit none
  # 880 	!~ integer,parameter :: r8 = selected_real_kind(12) ! 8 byte real
  # 881 	!~ integer,parameter :: r6 = selected_real_kind(8) ! 8 byte real
  # 882 	!~ real(r8), intent(in) :: forc_st    		! soil temperature (Kelvin)  (-nlevsno+1:nlevgrnd)
  # 883 	!~ real(r8), intent(in) :: psi      			! soil water potential at saturation for CN code (MPa)
  # 884 	!~ real(r8), intent(in) :: forc_npp
  # 885 	!~ real(r8), intent(in) :: forc_roots
  # 886
  # 887 	!~ real(r8),intent(inout) 	:: forc_exoenzyme
  # 888 	!~ real, intent(inout)	:: clay
  # 889 	!~ real(r8),intent(inout) 	:: DOC
  # 890 	!~ real(r8),intent(inout) 	:: ACTIVE
  # 891 	!~ real(r8),intent(inout) 	:: SLOW
  # 892 	!~ real(r8),intent(inout) 	:: PASSIVE
  # 893 	!~ real(r8),intent(inout) 	:: f_DOC_ATM
  # 894 	!~ real(r8),intent(inout) 	:: f_ACTIVE_ATM
  # 895 	!~ real(r8),intent(inout) 	:: f_PASSIVE_ATM
  # 896 	!~ real(r8),intent(inout) 	:: f_SLOW_ATM
  # 897 	!~ real(r8),intent(inout) 	:: f_ACTIVE_DOC
  # 898 	!~ real(r8),intent(inout) 	:: f_ACTIVE_SLOW
  # 899 	!~ real(r8),intent(inout) 	:: f_SLOW_PASSIVE
  # 900 	!~ real(r8),intent(inout) 	:: f_ACTIVE_PASSIVE
  # 901 	!~ real(r8),intent(inout)	:: f_PASSIVE_ACTIVE
  # 902 	!~ real(r8),intent(inout)	:: f_DOC_Leaching
  # 903
  # 904 	!~ ! local pointers to implicit out scalars
  # 905 	!~ !
  # 906 	!~ ! !OTHER LOCAL VARIABLES:
  # 907
  # 908 	!~ real		:: temp         !temporary variables
  # 909 	!~ real		:: k_1          	!temporar variable for k of sorption
  # 910 	!~ real		:: Qmax	!maximum sorption capacity  mg / kg (mayes et al, 2012, SSSAJ)
  # 911 	!~ real(r8)	:: dt           		!decomp timestep (seconds)
  # 912 	!~ real(r8)	:: dtd          		!decomp timestep (days)
  # 913 	!~ real(r8)	:: t_scalar     	!soil temperature scalar for decomp
  # 914 	!~ real(r8)	:: minpsi, maxpsi    !limits for soil water scalar for decomp
  # 915 !~ !	real		:: psi                   	!temporary soilpsi for water scalar
  # 916 	!~ real		:: w_scalar     !soil water scalar for decomp
  # 917 	!~ real		:: rate_scalar  !combined rate scalar for decomp
  # 918 	!~ real		:: pH
  # 919
  # 920 	!-----------------------------------------------------------------------
  # 921 	!~ integer	:: timestep
  # 922
  # 923 	!~ common	/global/ &
  # 924 		!~ timestep
  # 925
  # 926 	!~ ! set time steps
  # 927 	!~ dt = real(timestep, r8 )
  dt <- as.numeric(timestep)
  # 928 !~ !	dtd = dt/86400.0_r8
  dtd <- dt / (60 * 60 * 24)
  # 929
  # 930 	!~ t_scalar = 0._r8
  # 931 	!~ temp = (forc_st - 25._r8)/10._r8
  temp <- (forc_st - 25) / 10
  # 932 	!~ t_scalar = t_scalar + 1.5**(temp)
  t_scalar <- 1.5 ^ temp
  # 933
  # 934 	!~ minpsi = -10.0_r8;
  minpsi <- -10
  # 935 	!~ w_scalar = 0._r8
  w_scalar <- 0
  # 936 	!~ maxpsi = -0.01
  maxpsi <- -0.01
  # 937 	!~ pH = 7.0
  # 938 !~ !	psi = min(psi,maxpsi)
  # 939 !~ !	if (psi > minpsi) then
  # 940 !~ !	w_scalar = w_scalar + (log(minpsi/psi)/log(minpsi/maxpsi))
  # 941 !~ !	end if
  # 942 !~ !	xiaofeng replaced above codes with following
  # 943 	!~ if (psi > minpsi) then
  # 944 	!~ w_scalar = w_scalar + (psi-minpsi)*(psi-maxpsi)/((psi-minpsi)*(psi-maxpsi) - &
  # 945 		!~ (psi-(maxpsi-(maxpsi-minpsi)/3.))*(psi-(maxpsi-(maxpsi-minpsi)/3.)))
  # 946 	!~ end if
  if (psi > minpsi) {
    w_scalar <- (psi - minpsi) * (psi - maxpsi) /
      ((psi - minpsi) * (psi - maxpsi) - (psi - (maxpsi - (maxpsi - minpsi) / 3)) *
         (psi - (maxpsi - (maxpsi - minpsi) / 3)))
  }
  # 947 	!~ w_scalar = w_scalar ** 0.5
  w_scalar <- w_scalar ^ 0.5
  # 948
  # 949 	!~ ! DOC -> leaching
  # 950 	!~ if (DOC > 0._r8) then
  # 951         !~ f_DOC_leaching = DOC * 0.0001 * (DOC/(DOC + 100)) / dt * t_scalar * w_scalar
  # 952 	!~ end if
  f_DOC_leaching <- DOC * 0.0001 * (DOC / (DOC + 100)) / dt * t_scalar * w_scalar
  # 953
  # 954 	!~ ! DOC -> ATM
  # 955 	!~ if (DOC > 0._r8) then
  # 956         !~ f_DOC_leaching = DOC * 0.005 / dt * t_scalar * w_scalar
  # 957 	!~ end if
  f_DOC_ATM <- DOC * 0.005 / dt * t_scalar * w_scalar
  # 958
  # 959 	!~ ! ACTIVE -> ATM
  # 960 	!~ if (ACTIVE > 0._r8) then
  # 961         !~ f_ACTIVE_ATM = ACTIVE * 0.0005 / dt * t_scalar * w_scalar
  # 962 	!~ end if
  f_ACTIVE_ATM <- ACTIVE * 0.0005 / dt * t_scalar * w_scalar
  # 963
  # 964 	!~ ! SLOW -> ATM
  # 965 	!~ if (SLOW > 0._r8) then
  # 966         !~ f_SLOW_ATM = SLOW * 0.0001 / dt * t_scalar * w_scalar
  # 967 	!~ end if
  f_SLOW_ATM <- SLOW * 0.0001 / dt * t_scalar * w_scalar
  # 968
  # 969 	!~ ! PASSIVE -> ATM
  # 970 	!~ if (PASSIVE > 0._r8) then
  # 971         !~ f_PASSIVE_ATM = PASSIVE * 0.00001 / dt * t_scalar * w_scalar
  # 972 	!~ end if
  f_PASSIVE_ATM <- PASSIVE * 0.00001 / dt * t_scalar * w_scalar
  # 973
  # 974 	!~ ! ACTIVE -> DOC
  # 975 	!~ if(ACTIVE > 0._r8) then
  # 976 	!~ f_ACTIVE_DOC = ACTIVE * 0.001 / dt * t_scalar * w_scalar
  # 977 	!~ end if
  f_ACTIVE_DOC <- ACTIVE * 0.001 / dt * t_scalar * w_scalar
  # 978
  # 979 	!~ ! ACTIVE -> SLOW
  # 980 	!~ if (ACTIVE > 0._r8) then
  # 981         !~ f_ACTIVE_SLOW = ACTIVE * 0.006 / dt * t_scalar * w_scalar
  # 982 	!~ end if
  f_ACTIVE_SLOW <- ACTIVE * 0.006 / dt * t_scalar * w_scalar
  # 983
  # 984 	!~ ! SLOW -> PASSIVE
  # 985 	!~ if (SLOW > 0._r8) then
  # 986         !~ f_SLOW_PASSIVE = SLOW * 0.0001 / dt * t_scalar * SLOW / (SLOW + 100)
  # 987 	!~ end if
  f_SLOW_PASSIVE <- SLOW * 0.0001 / dt * t_scalar * SLOW / (SLOW + 100)
  # 988
  # 989 	!~ ! ACTIVE -> PASSIVE
  # 990 	!~ if (ACTIVE > 0._r8) then
  # 991         !~ f_ACTIVE_PASSIVE = ACTIVE / dt * t_scalar * 0.0001
  # 992 	!~ end if
  f_ACTIVE_PASSIVE <- ACTIVE / dt * t_scalar * 0.0001
  # 993
  # 994 	!~ ! PASSIVE -> ACTIVE
  # 995 	!~ if (PASSIVE > 0._r8) then
  # 996         !~ f_PASSIVE_ACTIVE = 0.0001 * (PASSIVE - 10) / (1000 + PASSIVE - 10) / dt * t_scalar * w_scalar
  # 997 	!~ end if
  f_PASSIVE_ACTIVE <- 0.0001 * (PASSIVE - 10) / (1000 + PASSIVE - 10) / dt * t_scalar * w_scalar
  # 998
  # Update state pools
  # 999 	!~ DOC = DOC + (f_ACTIVE_DOC - f_DOC_ATM - f_DOC_Leaching) * dt
  DOC <- DOC + (f_ACTIVE_DOC - f_DOC_ATM - f_DOC_leaching) * dt
  # 1000 	!~ ACTIVE = ACTIVE + (f_PASSIVE_ACTIVE - f_ACTIVE_DOC - f_ACTIVE_SLOW - f_ACTIVE_PASSIVE - f_ACTIVE_ATM) * dt
  ACTIVE <- ACTIVE + (f_PASSIVE_ACTIVE - f_ACTIVE_DOC - f_ACTIVE_SLOW - f_ACTIVE_PASSIVE - f_ACTIVE_ATM) * dt
  # 1001 	!~ SLOW = SLOW + (f_ACTIVE_SLOW - f_SLOW_PASSIVE - f_SLOW_ATM) * dt
  SLOW <- SLOW + (f_ACTIVE_SLOW - f_SLOW_PASSIVE - f_SLOW_ATM) * dt
  # 1002 	!~ PASSIVE = PASSIVE + (f_SLOW_PASSIVE + f_ACTIVE_PASSIVE - f_PASSIVE_ACTIVE - f_PASSIVE_ATM) * dt
  PASSIVE <- PASSIVE + (f_SLOW_PASSIVE + f_ACTIVE_PASSIVE - f_PASSIVE_ACTIVE - f_PASSIVE_ATM) * dt
  # 1003
  # 1004 !~ end subroutine decomp_century
  # 1005 	!~ ! decomposition subroutine of CENTURY end
  list(DOC = DOC, ACTIVE = ACTIVE, SLOW = SLOW, PASSIVE = PASSIVE,
       f_ACTIVE_ATM = f_ACTIVE_ATM, f_PASSIVE_ATM = f_PASSIVE_ATM,
       f_SLOW_ATM = f_SLOW_ATM, f_ACTIVE_DOC = f_ACTIVE_DOC,
       f_ACTIVE_SLOW = f_ACTIVE_SLOW, f_SLOW_PASSIVE = f_SLOW_PASSIVE,
       f_ACTIVE_PASSIVE = f_ACTIVE_PASSIVE, f_PASSIVE_ACTIVE = f_PASSIVE_ACTIVE,
       f_DOC_leaching = f_DOC_leaching, f_DOC_ATM = f_DOC_ATM)
}
