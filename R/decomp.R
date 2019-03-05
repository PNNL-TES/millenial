#' Decomposition
#'
#' @return stuff
#' @export
decomp <- function(forc_st, forc_sw, psi, forc_npp, forc_roots,
                   forc_exoenzyme, clay, LMWC, POM, MB, MINERAL,
                   SOILAGG, f_LM_leaching, f_MI_LM_des, f_LM_MI_sor,
                   f_LM_MB_uptake,f_PO_LM_dep, f_MB_MI_sor,f_PO_SO_agg,
                   f_MI_SO_agg, f_SO_PO_break, f_SO_MI_break, f_MB_atm) {
  #   537 !	decomposition subroutine start
  #   538 subroutine decomp(forc_st, forc_sw, psi, forc_npp, forc_roots, &
  #                           539 		forc_exoenzyme, clay, LMWC, POM, MB, MINERAL, SOILAGG, f_LM_leaching, f_MI_LM_des,&
  #                           540 		f_LM_MI_sor, f_LM_MB_uptake,f_PO_LM_dep, f_MB_MI_sor,f_PO_SO_agg, f_MI_SO_agg,&
  #                           541 		f_SO_PO_break, f_SO_MI_break, f_MB_atm)
  #   542
  #   543 	implicit none
  #   544 	integer,parameter :: r8 = selected_real_kind(12) 	! 8 byte real
  #   545 	integer,parameter :: r6 = selected_real_kind(8) 	! 8 byte real
  #   546 	real(r8), intent(in) :: forc_st    				! soil temperature (Kelvin)  (-nlevsno+1:nlevgrnd)
  #   547 	real(r8), intent(in) :: forc_sw    				! soil moisture (fraction)
  #   548 	real(r8), intent(in) :: psi      					! soil water potential at saturation for CN code (MPa)
  #   549 	real(r8), intent(in) :: forc_npp
  #   550 	real(r8), intent(in) :: forc_roots
  #   551 !	real(r8), intent(in) :: pH
  #   552
  #   553 	real(r8),intent(inout) 	:: forc_exoenzyme
  #   554 	real, intent(inout)	:: clay
  #   555 	real(r8),intent(inout) 	:: LMWC
  #   556 	real(r8),intent(inout) 	:: POM
  #   557 	real(r8),intent(inout) 	:: MB
  #   558 	real(r8),intent(inout) 	:: MINERAL
  #   559 	real(r8),intent(inout) 	:: SOILAGG
  #   560 	real(r8),intent(inout) 	:: f_LM_leaching
  #   561 	real(r8),intent(inout) 	:: f_MI_LM_des
  #   562 	real(r8),intent(inout) 	:: f_LM_MI_sor
  #   563 	real(r8),intent(inout) 	:: f_LM_MB_uptake
  #   564 	real(r8),intent(inout) 	:: f_PO_LM_dep
  #   565 	real(r8),intent(inout) 	:: f_MB_MI_sor
  #   566 	real(r8),intent(inout) 	:: f_PO_SO_agg
  #   567 	real(r8),intent(inout) 	:: f_MI_SO_agg
  #   568 	real(r8),intent(inout) 	:: f_SO_PO_break
  #   569 	real(r8),intent(inout)	:: f_SO_MI_break
  #   570 	real(r8),intent(inout)	:: f_MB_atm
  #   571
  #   572 	real(r8) :: k_leaching
  #   573 	real(r8) :: Vm_l
  #   574 	real(r8) :: km_l
  #   575 	real(r8) :: M_Lmin
  #   576 	real(r8) :: klmc_min
  #   577 	real(r8) :: Qmax
  #   578 	real(r8) :: klmc
  #   579 	real(r8) :: kes
  #   580 	real(r8) :: CUEref
  #   581 	real(r8) :: CUET
  #   582 	real(r8) :: Taeref
  #   583 	real(r8) :: Vpom_lmc
  #   584 	real(r8) :: kpom
  #   585 	real(r8) :: k_POMes
  #   586 	real(r8) :: kmic_min
  #   587 	real(r8) :: kmic
  #   588 	real(r8) :: Vpom_agg
  #   589 	real(r8) :: kpom_agg
  #   590 	real(r8) :: Vmin_agg
  #   591 	real(r8) :: kmin_agg
  #   592 	real(r8) :: AGGmax
  #   593 	real(r8) :: kagg
  #   594
  #   595 	! local pointers to implicit out scalars
  #   596 	!
  #     597 	! !OTHER LOCAL VARIABLES:
  #     598
  #   599 	real		:: temp, temp2, temp3	! temporary variables
  #   600 	real		:: psi_tem1, psi_tem2
  #   601 	real		:: k_sorption          		! temporar variable for k of sorption
  #   602 !	real		:: Qmax				! maximum sorption capacity  mg / kg (mayes et al, 2012, SSSAJ)
  #   603 	real(r8)	:: t_scalar     			! soil temperature scalar for decomp
  #   604 	real(r8)	:: t_scalar_mb  			! soil temperature scalar for decomp
  #   605 	real(r8)	:: minpsi, maxpsi    		! limits for soil water scalar for decomp
  #   606 !	real		:: psi                   			! temporary soilpsi for water scalar
  #   607 	real		:: w_scalar     			! soil water scalar for decomp
  #   608 	real		:: rate_scalar  			! combined rate scalar for decomp
  #   609 	real		:: pH
  #   610 	real 	:: f_SO_break
  #   611 	real(r8)	:: t_scalar_reverse     	! soil temperature scalar for decomp
  #   612 	real(r8)	:: w_scalar_reverse     	! soil temperature scalar for decomp
  #   613
  #   614 	!~ !-----------------------------------------------------------------------
  #     615
  #   616 	common	/global/ &
  #     617 		k_leaching, &
  #     618 		Vm_l, &
  #     619 		km_l, &
  #     620 		M_Lmin, &
  #     621 		klmc_min, &
  #     622 		Qmax, &
  #     623 		klmc, &
  #     624 		kes, &
  #     625 		CUEref, &
  #     626 		CUET, &
  #     627 		Taeref, &
  #     628 		Vpom_lmc, &
  #     629 		kpom, &
  #     630 		k_POMes, &
  #     631 		kmic_min, &
  #     632 		kmic, &
  #     633 		Vpom_agg, &
  #     634 		kpom_agg, &
  #     635 		Vmin_agg, &
  #     636 		kmin_agg, &
  #     637 		AGGmax, &
  #     638 		kagg
  #   639
  #   640 	t_scalar = 0._r8
  #   641 	t_scalar_reverse = 0._r8
  #   642 	temp = (forc_st - 15._r8) / 10._r8
  #   643 	t_scalar = t_scalar + 2. **(temp)
  #   644 	t_scalar_reverse = t_scalar_reverse + 0.5**(temp)
  #   645
  #   646 	t_scalar_mb = 0._r8
  #   647 	temp = (forc_st - 15._r8) / 10._r8
  #   648 	t_scalar_mb = t_scalar_mb + 2.**(temp)
  #   649
  #   650 !	print *, "t_scalar", t_scalar, minpsi, maxpsi, psi
  #   651 	minpsi = -10.0_r8
  #   652 	w_scalar = 0._r8
  #   653 	maxpsi = -0.01_r8
  #   654 	pH = 7.0
  #   655 !~ !	print *, " here ", minpsi / psi, minpsi / maxpsi
  #   656 	!~ psi_tem1 = minpsi / psi
  #   657 	!~ psi_tem2 = minpsi / maxpsi
  #   658 	!~ if (psi > maxpsi) then
  #   659 	!~ w_scalar = 1.
  #   660 	!~ end if
  #   661 	!~ if (psi < minpsi) then
  #   662 	!~ w_scalar = 0.
  #   663 	!~ else
  #     664 	!~ w_scalar = w_scalar + log(psi_tem1) / log(psi_tem2) * 2.3
  #   665 !~ !	w_scalar = w_scalar + (log(1.0 * minpsi/psi))!/log(1.0 * minpsi/maxpsi))
  # 666 	!~ end if
  # 667
  # 668 !	xiaofeng replaced above codes with following
  # 669 	if (psi > minpsi) then
  # 670 	w_scalar = w_scalar + (psi-minpsi)*(psi-maxpsi)/((psi-minpsi)*(psi-maxpsi) - &
  #                                                         671 		(psi-(maxpsi-(maxpsi-minpsi)/3.))*(psi-(maxpsi-(maxpsi-minpsi)/3.)))
  # 672 	end if
  # 673 	w_scalar = w_scalar ** 0.5
  # 674 !	print *, "w_scalar", w_scalar
  # 675
  # 676 	!#century temperature function
  #   677 	!soilTemp <- seq(-20,40,length.out = 100)
  # 678 	!teff <- c(15.4, 11.75, 29.7, 0.031)
  # 679 	!tfunc <- (teff[2] + (teff[3]/pi)* atan(pi*teff[4]*(soilTemp - teff[1]))) / (teff[2] + (teff[3]/pi)* atan(pi*teff[4]*(30 - teff[1])))
  # 680 	t_scalar = (11.75 + (29.7 / 3.1415926) * ATAN(real(3.1415926*0.031*(forc_st - 15.4)))) / &
  #   681 	(11.75 + (29.7 / 3.1415926) * ATAN(real(3.1415926 * 0.031 *(30.0 - 15.4))))
  # 682 	t_scalar_mb = t_scalar
  # 683
  # 684 	!#century water function
  #   685 	!relwc <- seq(0,1,length.out = 100)
  # 686 	!wfunc <- 1/(1 + 30 * exp(-9*relwc))
  # 687 	w_scalar = 1.0 / (1.0 + 30. * EXP(real(-9.0 * forc_sw)))
  # 688
  # 689 	! LMWC -> out of sysem LWMMWC leaching
  # 690 	if (LMWC > 0._r8) then
  # 691         f_LM_leaching = LMWC * k_leaching * t_scalar !* w_scalar ! Xiaofeng removed water impact, after review at GBC June,2017
  # 692 	end if
  # 693
  # 694 	!~ ! MINERAL -> LWMC  desorption Xu found this processes is not imporant as we treat below desorption function as double way, blocked it
  # 695 	!~ if (MINERAL > M_Lmin) then
  # 696         !~ f_MI_LM_des = Vm_l * (MINERAL - M_Lmin) / (km_l + MINERAL - M_Lmin) * t_scalar * w_scalar
  # 697 	!~ else
  #   698 	!~ f_MI_LM_des = 0.
  # 699 	!~ end if
  # 700
  # 701 	! LMWC -> MINERAL: This desorption function is from Mayes 2012, SSAJ
  # 702 	klmc_min = (10.0 ** (-0.186 * pH - 0.216)) / 24.0
  # 703 !	Qmax = 10.0 ** (0.4833 * log(clay * 100.0) + 2.3282) * 1.35 ! 1.35 is bulk density to convert Q from mg/kg to mg/m3
  # 704 	Qmax = 10.0 ** (0.297 * log(clay * 100.0) + 2.355 + 0.50) !* 1.25  ! 1.35 is bulk density to convert Q from mg/kg to g/m2
  # 705 !	write(*,*)"Qmax: ", Qmax
  # 706 	temp = (klmc_min * Qmax * LMWC ) / (2. + klmc_min * LMWC) - MINERAL
  # 707 !	if(temp > 0)then
  # 708 	f_LM_MI_sor = (temp / Qmax + 0.0015) * LMWC / 50. * t_scalar * w_scalar !* t_scalar * w_scalar !* (LMWC / 200) * (LMWC / 200)
  # 709 !	else
  #   710 !	f_LM_MI_sor = 0.
  # 711 !	end if
  # 712 !	f_LM_MI_sor = temp / (Qmax - MINERAL) * LMWC / 50. !* t_scalar * w_scalar !* (LMWC / 200) * (LMWC / 200)
  # 713
  # 714 	if (f_LM_MI_sor < (LMWC * 0.9)) then
  # 715  	f_LM_MI_sor = f_LM_MI_sor
  # 716 	else
  #   717 	f_LM_MI_sor = LMWC * 0.9
  # 718 	end if
  # 719
  # 720 	!~ if(f_LM_MI_sor < 0) then
  # 721 	!~ f_LM_MI_sor = 0.
  # 722 	!~ end if
  # 723
  # 724 !	print *, klmc_min, Qmax, f_LM_MI_sor, LMWC, MINERAL
  # 725
  # 726 	! LMWC -> MB
  # 727 	if (LMWC > 0._r8) then
  # 728 	f_LM_MB_uptake = LMWC * klmc * t_scalar * w_scalar * MB / (MB + kes) * LMWC / (20. + LMWC)
  # 729 	temp2 = f_LM_MB_uptake * (1. - (CUEref + CUET * (forc_st - Taeref)))
  # 730 	if(temp2 < 0._r8) then
  # 731 	temp2 = 0_r8
  # 732 	end if
  # 733 	f_LM_MB_uptake = f_LM_MB_uptake - temp2
  # 734 	end if
  # 735
  # 736 	! POM -> LMWC
  # 737 	if (POM > 0._r8) then
  # 738         f_PO_LM_dep = Vpom_lmc * POM / (POM + kpom) * t_scalar * w_scalar !* (1. - MB / (MB + k_POMes))
  # 739 	end if
  # 740
  # 741 	if(f_PO_LM_dep > (0.9 * POM)) then
  # 742 	f_PO_LM_dep = 0.9 * POM
  # 743 	end if
  # 744
  # 745 !	print *, f_PO_LM_dep, Vpom_lmc, POM, kpom, MB, k_POMes
  # 746
  # 747 	! MB -> MINERAL
  # 748 	!~ if (MB > 0._r8) then
  # 749         !~ temp = kmic_min * Qmax * MB / (1.0 + kmic_min * MB) - MINERAL
  # 750 	!~ f_MB_MI_sor = temp / Qmax * MB / 50. * t_scalar * w_scalar  !* (MB / 200) * (MB / 200)
  # 751 	!~ end if
  # 752
  # 753 	if (MB > 0._r8 .and. MINERAL < Qmax) then
  # 754 	f_MB_MI_sor = MB * kmic * 0.15 * t_scalar_mb * w_scalar  !* (MB / 200) * (MB / 200)
  # 755 	else
  #   756 	f_MB_MI_sor = 0.
  # 757 	end if
  # 758
  # 759 	if(f_MB_MI_sor > 0.9 * MB) then
  # 760 	f_MB_MI_sor = 0.9 * MB
  # 761 	end if
  # 762 	if(f_MB_MI_sor < 0.) then
  # 763 	f_MB_MI_sor = 0.
  # 764 	end if
  # 765
  # 766 	! MB -> ATM
  # 767 	if (MB > 0._r8) then
  # 768         f_MB_atm = temp2 + MB * kmic * t_scalar_mb * w_scalar
  # 769 	end if
  # 770
  # 771 	! POM -> SOILAGG
  # 772 	if (POM > 0._r8) then
  # 773         f_PO_SO_agg = Vpom_agg * POM / (kpom_agg + POM) * (1. - SOILAGG / AGGmax) * t_scalar * w_scalar
  # 774 	end if
  # 775
  # 776 	if(f_PO_SO_agg > 0.9 * POM) then
  # 777 	f_PO_SO_agg = 0.9 * POM
  # 778 	end if
  # 779
  # 780 !	print *, "POM > soilAGG: ",  f_PO_SO_agg, Vpom_agg, POM, kpom_agg, SOILAGG, AGGmax
  # 781
  # 782 	! MINERAL -> SOILAGG
  # 783 	if (MINERAL > 0._r8) then
  # 784         f_MI_SO_agg = Vmin_agg * MINERAL / (kmin_agg + MINERAL) * (1. - SOILAGG / AGGmax) !* t_scalar * w_scalar
  # 785 	end if
  # 786
  # 787 	if(f_MI_SO_agg>0.9 * MINERAL) then
  # 788 	f_MI_SO_agg = 0.9 * MINERAL
  # 789 	end if
  # 790
  # 791 	! SOILAGG -> MINERAL
  # 792 	if (SOILAGG > 0._r8) then
  # 793         f_SO_break = SOILAGG * kagg * t_scalar * w_scalar
  # 794 	f_SO_PO_break = f_SO_break * 1.5 / 3.
  # 795 	f_SO_MI_break = f_SO_break * 1.5 / 3.
  # 796 	end if
  # 797
  # 798 !	print *, "before update:", forc_npp, LMWC,POM,MB,MINERAL,SOILAGG,f_PO_LM_dep,f_MI_LM_des,f_LM_leaching,f_LM_MI_sor,f_LM_MB_uptake,&
  #   799 !	f_SO_PO_break,f_PO_LM_dep,f_PO_SO_agg
  # 800
  # 801 	if((f_PO_LM_dep + f_PO_SO_agg) > POM) then
  # 802 	temp3 = POM / (f_PO_LM_dep + f_PO_SO_agg)
  # 803 	f_PO_LM_dep = f_PO_LM_dep * temp3
  # 804 	f_PO_SO_agg = f_PO_SO_agg * temp3
  # 805 	end if
  # 806
  # 807 	LMWC = LMWC + (f_PO_LM_dep + f_MI_LM_des - f_LM_leaching - f_LM_MI_sor - f_LM_MB_uptake - temp2) + forc_npp / 3.
  # 808
  # 809 	POM = POM + (f_SO_PO_break - f_PO_LM_dep - f_PO_SO_agg) + forc_npp * 2. / 3.
  # 810
  # 811 	MB = MB + (f_LM_MB_uptake - f_MB_MI_sor - f_MB_atm)
  # 812
  # 813 !	print *, "MB, LM_MB, MB_Mi, MB_ATM", MB, f_LM_MB_uptake, f_MB_MI_sor, f_MB_atm
  # 814
  # 815 	MINERAL = MINERAL + (f_LM_MI_sor + f_MB_MI_sor + f_SO_MI_break - f_MI_LM_des - f_MI_SO_agg)
  # 816
  # 817 	SOILAGG = SOILAGG + (f_PO_SO_agg + f_MI_SO_agg - f_SO_PO_break - f_SO_MI_break)
  # 818
  # 819 !	print *, "after update:", LMWC,POM,MB,MINERAL,SOILAGG,f_PO_LM_dep,f_MI_LM_des,f_LM_leaching,f_LM_MI_sor,f_LM_MB_uptake,&
  #   820 !	f_SO_PO_break,f_PO_LM_dep,f_PO_SO_agg
  # 821
  # 822 end subroutine decomp
  # 823 	! decomposition subroutine end
}
