#' Millenial model main loop
#'
#' @param nr Number of simulation steps (days)
#'
#' @return Simulation outputs in a data frame.
#' @export
#'
#' @examples
#' main(365)  # a one-year simulation
main <- function(nr) {
  # 1 !	main program start
  # 2 	PROGRAM Millennial
  # 3 !	History
  # 4 !	Xiaofeng Xu created this code program to play with Millennial model structure (ICOS workshop Mar 14-16, 2016 in Boulder, CO)
  # 5 !	The code is created in May - June 2016, solely by Xiaofeng XU (xxu@mail.sdsu.edu)
  # 6 !	This is a toy verion of the Millennial model (C only version, N P will be added in future updates)
  # 7
  # 8 	implicit none
  # 9 	integer,parameter 	:: r8 = selected_real_kind(12) 	!8 byte real
  # 10 !	integer				:: flag					!century model or millenial model
  # 11 	integer 				:: nr, i, n, flag_output	, flag_annual
  # 12
  # 13  !	for initile file
  # 14 	character(len = 256) :: initialfile
  # 15 	character(len = 256) :: soilparafile
  # 16
  # 17 !	for output file
  # 18 	character(len = 256) :: outputfile
  # 19 	character(len = 256) :: outputfile_century
  # 20 !	end of defining output file
  # 21
  # 22 !	the input data: driving forces
  # 23 	real(r8), dimension(:), allocatable :: forc_st
  # 24 	real(r8), dimension(:), allocatable :: forc_sw
  # 25 !	end of driving forces
  # 26
  # 27 !	key variables to drive this model : semi-driving forces
  # 28 	real(r8), dimension(:), allocatable :: forc_npp
  # 29 	real(r8), dimension(:), allocatable :: forc_roots
  # 30 	real(r8), dimension(:), allocatable :: forc_exoenzyme !if modified this could be calcuated based on biomass and limitation of N or P
  # 31 !	end of key variables to drive this model : semi-driving forces
  # 32
  # 33 !!	key variables to track the system over time
  # 34 !	pools
  # 35 	real(r8), dimension(:), allocatable :: LMWC
  # 36 	real(r8), dimension(:), allocatable :: POM
  # 37 	real(r8), dimension(:), allocatable :: MB
  # 38 	real(r8), dimension(:), allocatable :: MINERAL
  # 39 	real(r8), dimension(:), allocatable :: SOILAGG
  # 40 !	end of pools
  # 41
  # 42 !	flux
  # 43 	real(r8), dimension(:), allocatable :: f_LM_leaching		! LM leaching
  # 44 	real(r8), dimension(:), allocatable :: f_MI_LM_des		! Mineral to LMWC
  # 45 	real(r8), dimension(:), allocatable :: f_LM_MI_sor		! LMWC to Mineral C
  # 46 	real(r8), dimension(:), allocatable :: f_LM_MB_uptake	! LMWC to microbial C
  # 47 	real(r8), dimension(:), allocatable :: f_PO_LM_dep		! PO
  # 48 	real(r8), dimension(:), allocatable :: f_MB_MI_sor
  # 49 	real(r8), dimension(:), allocatable :: f_PO_SO_agg
  # 50 	real(r8), dimension(:), allocatable :: f_MI_SO_agg
  # 51 	real(r8), dimension(:), allocatable :: f_SO_PO_break
  # 52 	real(r8), dimension(:), allocatable :: f_SO_MI_break
  # 53 	real(r8), dimension(:), allocatable :: f_MB_atm
  # 54 !	end of flux
  # 55
  # 56
  # 57 !!	key variables to track the system over time CENTURY
  # 58 !	pools
  # 59 	real(r8), dimension(:), allocatable :: DOC
  # 60 	real(r8), dimension(:), allocatable :: ACTIVE
  # 61 	real(r8), dimension(:), allocatable :: PASSIVE
  # 62 	real(r8), dimension(:), allocatable :: SLOW
  # 63 !	end of pools
  # 64
  # 65 !	flux
  # 66 	real(r8), dimension(:), allocatable :: f_DOC_ATM
  # 67 	real(r8), dimension(:), allocatable :: f_ACTIVE_ATM
  # 68 	real(r8), dimension(:), allocatable :: f_PASSIVE_ATM
  # 69 	real(r8), dimension(:), allocatable :: f_SLOW_ATM
  # 70 	real(r8), dimension(:), allocatable :: f_ACTIVE_DOC
  # 71 	real(r8), dimension(:), allocatable :: f_ACTIVE_SLOW
  # 72 	real(r8), dimension(:), allocatable :: f_SLOW_PASSIVE
  # 73 	real(r8), dimension(:), allocatable :: f_ACTIVE_PASSIVE
  # 74 	real(r8), dimension(:), allocatable :: f_PASSIVE_ACTIVE
  # 75 	real(r8), dimension(:), allocatable :: f_DOC_Leaching
  # 76 !	end of flux CENTURY
  # 77
  # 78
  # 79 !	soil properties over time
  # 80 	real(r8), dimension(:), allocatable :: psi_real
  # 81 !	end of flux
  # 82
  # 83 !	soil properties
  # 84 	real			:: sand
  # 85 	real			:: clay
  # 86 	real			:: silt
  # 87 	real			:: maxpsi
  # 88 	real			:: vwc
  # 89 	real			:: vwcsat
  # 90 	real			:: smp_l
  # 91 	real			:: psisat
  # 92 	real			:: organic
  # 93 	real			:: psi
  # 94
  # 95 	real(r8) 		:: k_leaching
  # 96 	real(r8) 		:: Vm_l
  # 97 	real(r8) 		:: km_l
  # 98 	real(r8) 		:: M_Lmin
  # 99 	real(r8) 		:: klmc_min
  # 100 	real(r8) 		:: Qmax
  # 101 	real(r8) 		:: klmc
  # 102 	real(r8) 		:: kes
  # 103 	real(r8) 		:: CUEref
  # 104 	real(r8) 		:: CUET
  # 105 	real(r8) 		:: Taeref
  # 106 	real(r8) 		:: Vpom_lmc
  # 107 	real(r8) 		:: kpom
  # 108 	real(r8) 		:: k_POMes
  # 109 	real(r8) 		:: kmic_min
  # 110 	real(r8) 		:: kmic
  # 111 	real(r8) 		:: Vpom_agg
  # 112 	real(r8) 		:: kpom_agg
  # 113 	real(r8) 		:: Vmin_agg
  # 114 	real(r8) 		:: kmin_agg
  # 115 	real(r8) 		:: AGGmax
  # 116 	real(r8) 		:: kagg
  # 117 !	end
  # 118
  # 119 	real(r8)			:: initial_pom
  # 120 	real(r8)			:: initial_lmwc
  # 121 	real(r8)			:: initial_mb
  # 122 	real(r8)			:: initial_mineral
  # 123 	real(r8)			:: initial_soilagg
  # 124 !	end of key variables
  # 125
  # 126 	integer, parameter 		:: soil_par_num = 28
  # 127 !	character(len=256) 			:: soil_par_f = './soilpara_in' 	! local file name
  # 128 	integer 					:: ier              				! error code
  # 129 	character(len=40) 		:: soil_par_name(soil_par_num)	! parameter name
  # 130 	real(r8)					:: dummy(soil_par_num)
  # 131
  # 132 	common	/global/ &
  #   133 		k_leaching, &
  #   134 		Vm_l, &
  #   135 		km_l, &
  #   136 		M_Lmin, &
  #   137 		klmc_min, &
  #   138 		Qmax, &
  #   139 		klmc, &
  #   140 		kes, &
  #   141 		CUEref, &
  #   142 		CUET, &
  #   143 		Taeref, &
  #   144 		Vpom_lmc, &
  #   145 		kpom, &
  #   146 		k_POMes, &
  #   147 		kmic_min, &
  #   148 		kmic, &
  #   149 		Vpom_agg, &
  #   150 		kpom_agg, &
  #   151 		Vmin_agg, &
  #   152 		kmin_agg, &
  #   153 		AGGmax, &
  #   154 		kagg
  # 155
  # 156 	write(*,*) "This is the toy version of the millienum model at a daily time step"
  # 157
  # 158 	write(*,*) "Pleae enter the number for total simulation steps"
  # 159 	read(*,*) nr
  # 160
  # 161 	write(*,*) "please enter the name of parameter file:"
  # 162 	read(*,*) soilparafile
  # 163
  # 164 	write(*,*) "Do you want to save model output! 1 for YES, 0 for NO"
  # 165 	read(*,*) flag_output
  # 166 	write(*,*) "annaul output or daily? 1 for annual, 0 for daily"
  # 167 	read(*,*) flag_annual
  # 168 	if(flag_output == 1) then
  # 169 	write(*,*) "please enter the name of file for saving model output!"
  # 170 	read(*,*) outputfile
  # 171 	end if
  # 172
  # 173 !	allocate space for key input data
  # 174 	allocate(forc_st(1:nr))
  # 175 	allocate(forc_sw(1:nr))
  # 176 	allocate(forc_npp(1:nr))
  # 177 	allocate(forc_roots(1:nr))
  # 178 	allocate(forc_exoenzyme(1:nr))
  # 179 	allocate(psi_real(1:nr))
  # 180
  # 181 	allocate(LMWC(1:nr))
  # 182 	allocate(POM(1:nr))
  # 183 	allocate(MB(1:nr))
  # 184 	allocate(MINERAL(1:nr))
  # 185 	allocate(SOILAGG(1:nr))
  # 186
  # 187 	allocate(f_LM_leaching(1:nr))
  # 188 	allocate(f_MI_LM_des(1:nr))
  # 189 	allocate(f_LM_MI_sor(1:nr))
  # 190 	allocate(f_LM_MB_uptake(1:nr))
  # 191 	allocate(f_PO_LM_dep(1:nr))
  # 192 	allocate(f_MB_MI_sor(1:nr))
  # 193 	allocate(f_PO_SO_agg(1:nr))
  # 194 	allocate(f_MI_SO_agg(1:nr))
  # 195 	allocate(f_SO_PO_break(1:nr))
  # 196 	allocate(f_SO_MI_break(1:nr))
  # 197 	allocate(f_MB_atm(1:nr))
  # 198 !	end of the allocation
  # 199
  # 200 !	CENTURY allocation
  # 201 	allocate(DOC(1:nr))
  # 202 	allocate(ACTIVE(1:nr))
  # 203 	allocate(PASSIVE(1:nr))
  # 204 	allocate(SLOW(1:nr))
  # 205 !	end of pools
  # 206
  # 207 !	flux
  # 208 	allocate(f_DOC_ATM(1:nr))
  # 209 	allocate(f_ACTIVE_ATM(1:nr))
  # 210 	allocate(f_PASSIVE_ATM(1:nr))
  # 211 	allocate(f_SLOW_ATM(1:nr))
  # 212 	allocate(f_ACTIVE_DOC(1:nr))
  # 213 	allocate(f_ACTIVE_SLOW(1:nr))
  # 214 	allocate(f_SLOW_PASSIVE(1:nr))
  # 215 	allocate(f_ACTIVE_PASSIVE(1:nr))
  # 216 	allocate(f_PASSIVE_ACTIVE(1:nr))
  # 217 	allocate(f_DOC_Leaching(1:nr))
  # 218 !	end of allocation for CENTURY
  # 219
  # 220 	write(*,*) 'Attempting to read soil parameters .....'
  # 221 	open(unit = 10, file=soilparafile)
  # 222 	do i = 1, soil_par_num
  # 223 	read (10,*,iostat=ier) soil_par_name(i), dummy(i)
  # 224 	print *, dummy(i)
  # 225 	if (ier /= 0) then
  # 226 	write(*,*)'soilpara: error in reading in soilpara_in'
  # 227 	end if
  # 228 	end do
  # 229 	close(10)
  # 230
  # 231 !	Assign values
  # 232 	i = 1
  # 233 	clay				= dummy(i); i = i + 1
  # 234 	sand				= dummy(i); i = i + 1
  # 235 	silt				= dummy(i); i = i + 1
  # 236 	maxpsi			= dummy(i); i = i + 1
  # 237 	vwcsat			= dummy(i); i = i + 1
  # 238 	organic			= dummy(i); i = i + 1
  # 239 	k_leaching			= dummy(i); i = i + 1
  # 240 	Vm_l				= dummy(i); i = i + 1
  # 241 	km_l				= dummy(i); i = i + 1
  # 242 	M_Lmin			= dummy(i); i = i + 1
  # 243 	klmc_min			= dummy(i); i = i + 1
  # 244 	Qmax			= dummy(i); i = i + 1
  # 245 	klmc				= dummy(i); i = i + 1
  # 246 	kes				= dummy(i); i = i + 1
  # 247 	CUEref			= dummy(i); i = i + 1
  # 248 	CUET			= dummy(i); i = i + 1
  # 249 	Taeref			= dummy(i); i = i + 1
  # 250 	Vpom_lmc			= dummy(i); i = i + 1
  # 251 	kpom			= dummy(i); i = i + 1
  # 252 	k_POMes			= dummy(i); i = i + 1
  # 253 	kmic_min			= dummy(i); i = i + 1
  # 254 	kmic				= dummy(i); i = i + 1
  # 255 	Vpom_agg			= dummy(i); i = i + 1
  # 256 	kpom_agg			= dummy(i); i = i + 1
  # 257 	Vmin_agg			= dummy(i); i = i + 1
  # 258 	kmin_agg			= dummy(i); i = i + 1
  # 259 	AGGmax			= dummy(i); i = i +1
  # 260 	kagg				= dummy(i)
  # 261
  # 262 	AGGmax = AGGmax * (0.0265 * clay * 100.0 + 0.1351)
  # 263 !	print *, "vwcsat: ", vwcsat, clay, dummy(5)
  # 264 	write(*,*) "Model inializing! "
  # 265 	write(*,*) "please enter the name of file for initilizing the model"
  # 266 	read(*,*) initialfile
  # 267
  # 268 	open(unit = 11, file=initialfile)
  # 269
  # 270 	read (11,*,iostat=ier) initial_pom, initial_lmwc, initial_mb, initial_mineral, initial_soilagg
  # 271
  # 272 	if (ier /= 0) then
  # 273 	write(*,*)'model inializing failed !'
  # 274 	else
  #   275 	write(*,*) "model inialization finished !"
  # 276 	end if
  # 277 	close(11)
  # 278
  # 279 	print *, 'read data start'
  # 280 	call readdata(nr, forc_st, forc_sw, forc_npp)
  # 281 	print *, 'read data end'
  # 282
  # 283 	LMWC(1)=initial_lmwc
  # 284 	POM(1)=initial_pom
  # 285 	MB(1)=initial_mb
  # 286 	MINERAL(1)=initial_mineral
  # 287 	SOILAGG(1)=initial_soilagg
  # 288
  # 289 	DOC(1:)=LMWC(1)
  # 290 	ACTIVE(1)=MB(1) + POM(1)
  # 291 	PASSIVE(1)=MINERAL(1)
  # 292 	SLOW(1)=SOILAGG(1)
  # 293
  # 294 	do n = 1, nr
  for(n in seq_along(nr)) {  # daily loop
    # 295 	vwc = forc_sw(n)
    # 296 call soilpsi(sand, clay, silt, vwc, vwcsat, organic, psisat, psi, smp_l)
    # 297 !	soilpsi(sand, clay, silt, vwc, vwcsat, organic3d, psisat, psi, smp_l)
    # 298 	psi_real(n) = psi
    # 299
    # 300 call decomp(forc_st(n), forc_sw(n), psi_real(n), forc_npp(n), forc_roots(n), &
    #                   301 		forc_exoenzyme(n), clay, LMWC(n), POM(n), MB(n), MINERAL(n), SOILAGG(n), f_LM_leaching(n), f_MI_LM_des(n),&
    #                   302 		f_LM_MI_sor(n), f_LM_MB_uptake(n),f_PO_LM_dep(n), f_MB_MI_sor(n), f_PO_SO_agg(n), f_MI_SO_agg(n),&
    #                   303 		f_SO_PO_break(n), f_SO_MI_break(n),f_MB_atm(n))
    # 304
    # 305 !	upading the pool after each iteration
    # 306 	if(n < nr) then
    # 307 	LMWC(n+1)=LMWC(n)
    # 308 	POM(n+1)=POM(n)
    # 309 	MB(n+1)=MB(n)
    # 310 	MINERAL(n+1)=MINERAL(n)
    # 311 	SOILAGG(n+1)=SOILAGG(n)
    # 312 	endif
    # 313 !	print *, n, " days millennial simulation finished!"
    # 314 !	end do
    # 315
    # 316 !call decomp_century(forc_st(n), psi_real(n), forc_npp(n), forc_roots(n), &
    #                            317 !		forc_exoenzyme(n), clay, DOC(n), ACTIVE(n), SLOW(n), PASSIVE(n), f_DOC_ATM(n), f_ACTIVE_ATM(n),&
    #                            318 !		f_PASSIVE_ATM(n), f_SLOW_ATM(n),f_ACTIVE_DOC(n), f_ACTIVE_SLOW(n), f_SLOW_PASSIVE(n), f_ACTIVE_PASSIVE(n),&
    #                            319 !		f_PASSIVE_ACTIVE(n), f_DOC_Leaching(n))
    # 320
    # 321 !	upading the pool after each iteration
    # 322 	if(n < nr) then
    # 323 	DOC(n+1)=DOC(n)
    # 324 	ACTIVE(n+1)=ACTIVE(n)
    # 325 	SLOW(n+1)=SLOW(n)
    # 326 	PASSIVE(n+1)=PASSIVE(n)
    # 327 	endif
    # 328 !	print *, n, " days century simulation finished!"
    # 329 	print *, n, "LMWC: ", LMWC(n),  "POMC: ",POM(n), "MBC: ", MB(n),  "MINERALC: ",MINERAL(n),  "AGGC: ",SOILAGG(n)
    # 330 	end do
  }
  # 331
  # 332 	if(flag_output ==1) then
  # 333 call writeoutput(flag_annual, nr, forc_st, forc_sw, forc_npp, forc_roots, &
  #                        334 		forc_exoenzyme, LMWC, POM, MB, MINERAL, SOILAGG, f_LM_leaching, f_MI_LM_des,&
  #                        335 		f_LM_MI_sor, f_LM_MB_uptake, f_PO_LM_dep, f_MB_MI_sor, f_PO_SO_agg, f_MI_SO_agg,&
  #                        336 		f_SO_PO_break, f_SO_MI_break, f_MB_atm, outputfile, DOC, ACTIVE, SLOW, PASSIVE, f_DOC_ATM, f_ACTIVE_ATM,&
  #                        337 		f_PASSIVE_ATM, f_SLOW_ATM,f_ACTIVE_DOC, f_ACTIVE_SLOW, f_SLOW_PASSIVE, f_ACTIVE_PASSIVE,&
  #                        338 		f_PASSIVE_ACTIVE, f_DOC_Leaching)
  # 339 	end if
  # 340
  # 341 	deallocate(forc_st)
  # 342 	deallocate(forc_sw)
  # 343 	deallocate(forc_npp)
  # 344 	deallocate(forc_roots)
  # 345 	deallocate(forc_exoenzyme)
  # 346 	deallocate(psi_real)
  # 347
  # 348 	deallocate(LMWC)
  # 349 	deallocate(POM)
  # 350 	deallocate(MB)
  # 351 	deallocate(MINERAL)
  # 352 	deallocate(SOILAGG)
  # 353
  # 354 	deallocate(f_LM_leaching)
  # 355 	deallocate(f_MI_LM_des)
  # 356 	deallocate(f_LM_MI_sor)
  # 357 	deallocate(f_LM_MB_uptake)
  # 358 	deallocate(f_PO_LM_dep)
  # 359 	deallocate(f_MB_MI_sor)
  # 360 	deallocate(f_PO_SO_agg)
  # 361 	deallocate(f_MI_SO_agg)
  # 362 	deallocate(f_SO_PO_break)
  # 363 	deallocate(f_SO_MI_break)
  # 364 	deallocate(f_MB_atm)
  # 365 !	end of the allocation
  # 366
  # 367 !	CENTURY allocation
  # 368 	deallocate(DOC)
  # 369 	deallocate(ACTIVE)
  # 370 	deallocate(PASSIVE)
  # 371 	deallocate(SLOW)
  # 372 !	end of pools
  # 373
  # 374 !	flux
  # 375 	deallocate(f_DOC_ATM)
  # 376 	deallocate(f_ACTIVE_ATM)
  # 377 	deallocate(f_PASSIVE_ATM)
  # 378 	deallocate(f_SLOW_ATM)
  # 379 	deallocate(f_ACTIVE_DOC)
  # 380 	deallocate(f_ACTIVE_SLOW)
  # 381 	deallocate(f_SLOW_PASSIVE)
  # 382 	deallocate(f_ACTIVE_PASSIVE)
  # 383 	deallocate(f_PASSIVE_ACTIVE)
  # 384 	deallocate(f_DOC_Leaching)
  # 385
  # 386 	stop
  # 387 END PROGRAM Millennial
  # 388 !	main program end
}
