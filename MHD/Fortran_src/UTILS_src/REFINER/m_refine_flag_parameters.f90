!m_refine_flag_parameters.f90
!      module m_refine_flag_parameters
!
      module m_refine_flag_parameters
!
!      Writen by H. Matsui on Sep., 2007
!
      use m_precision
!
      implicit none
!

!      iflag_refine(iele) = 0 ::    nothing to do
!
!      iflag_refine(iele) = 100 ::  convert linear to quad20
!      iflag_refine(iele) = 110 ::  convert linear to quad27
!
!      iflag_refine(iele) = 200 ::  double each element
!
!      iflag_refine(iele) = 211 ::  double on xy-plane
!      iflag_refine(iele) = 212 ::  double on yz-plane
!      iflag_refine(iele) = 213 ::  double on zx-plane
!
!      iflag_refine(iele) = 221 ::  double on x-edge
!      iflag_refine(iele) = 222 ::  double on y-edge
!      iflag_refine(iele) = 223 ::  double on z-edge
!
!   spherical shell special
!
!      iflag_refine(iele) = 400 ::  refine triple in xi-plane
!      iflag_refine(iele) = 410 ::  refine triple in ei-plane
!      iflag_refine(iele) = 420 ::  refine triple in zi-plane
!
!      iflag_refine(iele) = 401 ::  refine triple on surface 1
!      iflag_refine(iele) = 402 ::  refine triple on surface 2
!      iflag_refine(iele) = 411 ::  refine triple on surface 3
!      iflag_refine(iele) = 412 ::  refine triple on surface 4
!      iflag_refine(iele) = 421 ::  refine triple on surface 5
!      iflag_refine(iele) = 422 ::  refine triple on surface 6
!
!      iflag_refine(iele) = 300 ::  refine triple
!
!      iflag_refine(iele) = 311 ::  refine triple on surface 1
!      iflag_refine(iele) = 312 ::  refine triple on surface 2
!      iflag_refine(iele) = 313 ::  refine triple on surface 3
!      iflag_refine(iele) = 314 ::  refine triple on surface 4
!      iflag_refine(iele) = 315 ::  refine triple on surface 5
!      iflag_refine(iele) = 316 ::  refine triple on surface 6
!
!      iflag_refine(iele) = 321 ::  refine triple on edge 1
!      iflag_refine(iele) = 322 ::  refine triple on edge 2
!      iflag_refine(iele) = 323 ::  refine triple on edge 3
!      iflag_refine(iele) = 324 ::  refine triple on edge 4
!      iflag_refine(iele) = 325 ::  refine triple on edge 5
!      iflag_refine(iele) = 326 ::  refine triple on edge 6
!      iflag_refine(iele) = 327 ::  refine triple on edge 7
!      iflag_refine(iele) = 328 ::  refine triple on edge 8
!      iflag_refine(iele) = 329 ::  refine triple on edge 9
!      iflag_refine(iele) = 330 ::  refine triple on edge 10
!      iflag_refine(iele) = 331 ::  refine triple on edge 11
!      iflag_refine(iele) = 332 ::  refine triple on edge 12
!
!      iflag_refine(iele) = 341 ::  refine triple at node 1
!      iflag_refine(iele) = 342 ::  refine triple at node 2
!      iflag_refine(iele) = 343 ::  refine triple at node 3
!      iflag_refine(iele) = 344 ::  refine triple at node 4
!      iflag_refine(iele) = 345 ::  refine triple at node 5
!      iflag_refine(iele) = 346 ::  refine triple at node 6
!      iflag_refine(iele) = 347 ::  refine triple at node 7
!      iflag_refine(iele) = 348 ::  refine triple at node 8
!
!      iflag_refine(iele) = 451 ::  refine small triple on edge 1
!      iflag_refine(iele) = 452 ::  refine small triple on edge 2
!      iflag_refine(iele) = 453 ::  refine small triple on edge 3
!      iflag_refine(iele) = 454 ::  refine small triple on edge 4
!      iflag_refine(iele) = 455 ::  refine small triple on edge 5
!      iflag_refine(iele) = 456 ::  refine small triple on edge 6
!      iflag_refine(iele) = 457 ::  refine small triple on edge 7
!      iflag_refine(iele) = 458 ::  refine small triple on edge 8
!      iflag_refine(iele) = 459 ::  refine small triple on edge 9
!      iflag_refine(iele) = 460 ::  refine small triple on edge 10
!      iflag_refine(iele) = 461 ::  refine small triple on edge 11
!      iflag_refine(iele) = 462 ::  refine small triple on edge 12
!
!    pre-refine for corners
!
!      iflag_refine(iele) = 501 ::  refine to 5 segments in xi-plane
!      iflag_refine(iele) = 502 ::  refine to 5 segments in ei-plane
!      iflag_refine(iele) = 503 ::  refine to 5 segments in zi-plane
!
!      iflag_refine(iele) = 511 ::  refine to 5 segments on surface 1
!      iflag_refine(iele) = 512 ::  refine to 5 segments on surface 2
!      iflag_refine(iele) = 513 ::  refine to 5 segments on surface 3
!      iflag_refine(iele) = 514 ::  refine to 5 segments on surface 4
!      iflag_refine(iele) = 515 ::  refine to 5 segments on surface 5
!      iflag_refine(iele) = 516 ::  refine to 5 segments on surface 6
!
!
      integer(kind = kint), parameter :: iflag_nothing = 0
      integer(kind = kint), parameter :: iflag_8_to_20 = 100
      integer(kind = kint), parameter :: iflag_8_to_27 = 110
!
      integer(kind = kint), parameter :: iflag_double =  200
!
      integer(kind = kint), parameter :: iflag_double_x = 211
      integer(kind = kint), parameter :: iflag_double_y = 212
      integer(kind = kint), parameter :: iflag_double_z = 213
!
      integer(kind = kint), parameter :: iflag_tri_x =   400
      integer(kind = kint), parameter :: iflag_tri_xs1 = 401
      integer(kind = kint), parameter :: iflag_tri_xs2 = 402
      integer(kind = kint), parameter :: iflag_tri_y =   410
      integer(kind = kint), parameter :: iflag_tri_ys3 = 411
      integer(kind = kint), parameter :: iflag_tri_ys4 = 412
      integer(kind = kint), parameter :: iflag_tri_z =   420
      integer(kind = kint), parameter :: iflag_tri_zs5 = 421
      integer(kind = kint), parameter :: iflag_tri_zs6 = 422
!
      integer(kind = kint), parameter :: iflag_tri_full = 300
      integer(kind = kint), parameter :: iflag_tri_full_eq = 301
!
      integer(kind = kint), parameter :: iflag_tri_s1 =   311
      integer(kind = kint), parameter :: iflag_tri_s2 =   312
      integer(kind = kint), parameter :: iflag_tri_s3 =   313
      integer(kind = kint), parameter :: iflag_tri_s4 =   314
      integer(kind = kint), parameter :: iflag_tri_s5 =   315
      integer(kind = kint), parameter :: iflag_tri_s6 =   316
!
      integer(kind = kint), parameter :: iflag_tri_e1 =   321
      integer(kind = kint), parameter :: iflag_tri_e2 =   322
      integer(kind = kint), parameter :: iflag_tri_e3 =   323
      integer(kind = kint), parameter :: iflag_tri_e4 =   324
      integer(kind = kint), parameter :: iflag_tri_e5 =   325
      integer(kind = kint), parameter :: iflag_tri_e6 =   326
      integer(kind = kint), parameter :: iflag_tri_e7 =   327
      integer(kind = kint), parameter :: iflag_tri_e8 =   328
      integer(kind = kint), parameter :: iflag_tri_e9 =   329
      integer(kind = kint), parameter :: iflag_tri_e10 =  330
      integer(kind = kint), parameter :: iflag_tri_e11 =  331
      integer(kind = kint), parameter :: iflag_tri_e12 =  332
!
      integer(kind = kint), parameter :: iflag_tri_n1 =   341
      integer(kind = kint), parameter :: iflag_tri_n2 =   342
      integer(kind = kint), parameter :: iflag_tri_n3 =   343
      integer(kind = kint), parameter :: iflag_tri_n4 =   344
      integer(kind = kint), parameter :: iflag_tri_n5 =   345
      integer(kind = kint), parameter :: iflag_tri_n6 =   346
      integer(kind = kint), parameter :: iflag_tri_n7 =   347
      integer(kind = kint), parameter :: iflag_tri_n8 =   348
!
      integer(kind = kint), parameter :: iflag_five_x =  501
      integer(kind = kint), parameter :: iflag_five_y =  502
      integer(kind = kint), parameter :: iflag_five_z =  503
!
      integer(kind = kint), parameter :: iflag_five_s1 = 511
      integer(kind = kint), parameter :: iflag_five_s2 = 512
      integer(kind = kint), parameter :: iflag_five_s3 = 513
      integer(kind = kint), parameter :: iflag_five_s4 = 514
      integer(kind = kint), parameter :: iflag_five_s5 = 515
      integer(kind = kint), parameter :: iflag_five_s6 = 516
!
!
      integer(kind = kint), parameter :: iflag_stri_e1 =   451
      integer(kind = kint), parameter :: iflag_stri_e2 =   452
      integer(kind = kint), parameter :: iflag_stri_e3 =   453
      integer(kind = kint), parameter :: iflag_stri_e4 =   454
      integer(kind = kint), parameter :: iflag_stri_e5 =   455
      integer(kind = kint), parameter :: iflag_stri_e6 =   456
      integer(kind = kint), parameter :: iflag_stri_e7 =   457
      integer(kind = kint), parameter :: iflag_stri_e8 =   458
      integer(kind = kint), parameter :: iflag_stri_e9 =   459
      integer(kind = kint), parameter :: iflag_stri_e10 =  460
      integer(kind = kint), parameter :: iflag_stri_e11 =  461
      integer(kind = kint), parameter :: iflag_stri_e12 =  462
!
!    refine flag for surface
!
!      iflag_refine_surf(iele) = 0 ::    nothing to do
!
!      iflag_refine_surf(iele) = 100 ::  convert linear to quad8
!      iflag_refine_surf(iele) = 110 ::  convert linear to quad9
!
!      iflag_refine_surf(iele) = 200 ::  double each element
!
!      iflag_refine_surf(iele) = 211 ::  double on x-edge
!      iflag_refine_surf(iele) = 212 ::  double on y-edge
!
!   spherical shell special
!
!      iflag_refine_surf(iele) = 410 ::  refine triple on xi-edge
!      iflag_refine_surf(iele) = 420 ::  refine triple on ei-edge
!      iflag_refine_surf(iele) = 411 ::  refine triple on edge 1
!      iflag_refine_surf(iele) = 412 ::  refine triple on edge 3
!      iflag_refine_surf(iele) = 421 ::  refine triple on edge 2
!      iflag_refine_surf(iele) = 422 ::  refine triple on edge 4
!
!      iflag_refine_surf(iele) = 300 ::  refine triple
!
!      iflag_refine_surf(iele) = 311 ::  refine triple on edge 1
!      iflag_refine_surf(iele) = 312 ::  refine triple on edge 2
!      iflag_refine_surf(iele) = 313 ::  refine triple on edge 3
!      iflag_refine_surf(iele) = 314 ::  refine triple on edge 4
!
!      iflag_refine_surf(iele) = 321 ::  refine triple at node 1
!      iflag_refine_surf(iele) = 322 ::  refine triple at node 2
!      iflag_refine_surf(iele) = 323 ::  refine triple at node 3
!      iflag_refine_surf(iele) = 324 ::  refine triple at node 4
!
!      iflag_refine_surf(iele) = 500 ::  refine to five segments
!
!
      integer(kind = kint), parameter :: iflag_nothing_sf =  0
      integer(kind = kint), parameter :: iflag_4_to_8_sf = 100
      integer(kind = kint), parameter :: iflag_4_to_9_sf = 110
!
      integer(kind = kint), parameter :: iflag_dbl_sf =    200
!
      integer(kind = kint), parameter :: iflag_dbl_x_sf =   211
      integer(kind = kint), parameter :: iflag_dbl_y_sf =   212
!
      integer(kind = kint), parameter :: iflag_tri_full_sf_eq = 301
      integer(kind = kint), parameter :: iflag_tri_full_sf = 300
!
      integer(kind = kint), parameter :: iflag_tri_x_sf =   410
      integer(kind = kint), parameter :: iflag_tri_xe1_sf = 411
      integer(kind = kint), parameter :: iflag_tri_xe3_sf = 412
      integer(kind = kint), parameter :: iflag_tri_y_sf =   420
      integer(kind = kint), parameter :: iflag_tri_ye2_sf = 421
      integer(kind = kint), parameter :: iflag_tri_ye4_sf = 422
!
      integer(kind = kint), parameter :: iflag_tri_e1_sf =   311
      integer(kind = kint), parameter :: iflag_tri_e2_sf =   312
      integer(kind = kint), parameter :: iflag_tri_e3_sf =   313
      integer(kind = kint), parameter :: iflag_tri_e4_sf =   314
!
      integer(kind = kint), parameter :: iflag_tri_n1_sf =   321
      integer(kind = kint), parameter :: iflag_tri_n2_sf =   322
      integer(kind = kint), parameter :: iflag_tri_n3_sf =   323
      integer(kind = kint), parameter :: iflag_tri_n4_sf =   324
!
      integer(kind = kint), parameter :: iflag_five_sf =    500
!
!
!      refinement flag for edge
!
!      iflag_refine_edge(iele) = 0 ::    nothing to do
!
!      iflag_refine_edge(iele) = 100 ::  convert linear to quad3
!
!      iflag_refine_edge(iele) = 200 ::  double each element
!
!   spherical shell special
!
!      iflag_refine_edge(iele) = 300 ::  refine triple
!
!      iflag_refine_edge(iele) = 311 ::  refine triple at node 1 side
!      iflag_refine_edge(iele) = 312 ::  refine triple at node 2 side
!
      integer(kind = kint), parameter :: iflag_nothing_ed = 0
      integer(kind = kint), parameter :: iflag_2_to_3_ed = 100
!
      integer(kind = kint), parameter :: iflag_dbl_ed =    200
!
      integer(kind = kint), parameter :: iflag_tri_full_ed_eq = 301
      integer(kind = kint), parameter :: iflag_tri_full_ed =    300
!
      integer(kind = kint), parameter :: iflag_tri_n1_ed =   311
      integer(kind = kint), parameter :: iflag_tri_n2_ed =   312
!
!
!  --------------------------------------------------------------------
!
!      contains
!
!  --------------------------------------------------------------------
!
      end module m_refine_flag_parameters
