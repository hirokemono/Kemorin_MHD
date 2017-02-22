!>@file   reordering_djds_smp_type.f90
!!@brief  module reordering_djds_smp_type
!!
!!@author H. Matsui
!!@author K. Nakajima and H. Matsui
!!@date        Written by H. Matsui
!!@n      modified by H. Matsui in May. 2007
!!@n      modified by H. Matsui in Jan., 2009
!!@n      modified by H. Matsui in Nov., 2013
!
!>     Construct DJDS ordering table in structure
!!
!!@verbatim
!!      subroutine s_reordering_djds_smp(np_smp, NP, N, ISTACK_N_smp,   &
!!     &          solver_C, tbl_crs, DJDS_param, djds_tbl)
!!        type(mpi_4_solver), intent(in) ::       solver_C
!!        type(CRS_matrix_connect), intent(in) :: tbl_crs
!!        type(DJDS_poarameter), intent(in) :: DJDS_param
!!        type(DJDS_ordering_table), intent(inout) :: djds_tbl
!!@endverbatim
!
      module reordering_djds_smp_type
!
      use m_precision
      use t_colored_connect
      use t_iccg_parameter
      use t_work_DJDS_ordering
!
      implicit none
!
      type(work_4_RCM), private :: WK_MC
      type(work_DJDS_ordering), private :: WK_DJDS
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_reordering_djds_smp(np_smp, NP, N, ISTACK_N_smp,     &
     &          solver_C, tbl_crs, DJDS_param, djds_tbl)
!
      use t_crs_connect
      use t_solver_djds
      use t_vector_for_solver
!
      use ordering_MC_RCM_type
      use DJDS_hyperplane
      use DJDS_ordering
      use DJDS_total_nondiag
      use DJDS_nodiag_item
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: ISTACK_N_smp(0:np_smp)
      type(mpi_4_solver), intent(in) ::       solver_C
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(DJDS_poarameter), intent(in) :: DJDS_param
!
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!C +----------------+
!C | reordering RCM |
!C +----------------+
!C===
!
      call alloc_type_4_RCM(NP, djds_tbl)
!
!      count_rcm
!       (output:: NHYP, OLDtoNEW, NEWtoOLD, NLmax, NUmax)
!
      call count_rcm(NP, N, solver_C, tbl_crs, djds_tbl,                &
     &    DJDS_param, WK_MC, WK_DJDS)
!      call check_mc_connect(my_rank, WK_MC)
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
!
!C-- Off-Diagonal Component Number
!
      call alloc_type_number_4_djds(djds_tbl)
      call alloc_work_num_4_djds(NP, WK_DJDS)
!
!   count_hyperplane
!    ( output :: itotal_l, itotal_u, NLmax, NUmax, npLX1, npUX1
!                IVECT, NLmaxHYP, NUmaxHYP)
!
      call count_hyperplane(np_smp, NP, N,                              &
     &                  WK_MC%ntot_mc_l,   WK_MC%ntot_mc_u,             &
     &                  WK_MC%num_mc_l,    WK_MC%num_mc_u,              &
     &                  WK_MC%istack_mc_l, WK_MC%istack_mc_u,           &
     &                  WK_MC%item_mc_l,   WK_MC%item_mc_u,             &
     &                  djds_tbl%NHYP, djds_tbl%IVECT,                  &
     &                  djds_tbl%npLX1, djds_tbl%npUX1,                 &
     &                  djds_tbl%NLmax, djds_tbl%NUmax,                 &
     &                  djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP,           &
     &                  djds_tbl%itotal_l, djds_tbl%itotal_u,           &
     &                  WK_DJDS%NCOLORtot, WK_DJDS%OLDtoNEWmc,          &
     &                  WK_DJDS%NEWtoOLDmc, WK_DJDS%IVECmc,             &
     &                  WK_DJDS%INLmc, WK_DJDS%INUmc)
!
      call dealloc_4_IVECmc(WK_DJDS)
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
      call alloc_type_lists_4_DJDS(np_smp, NP, djds_tbl)
      call alloc_work_4_djds(NP, djds_tbl%NHYP,                         &
     &    djds_tbl%npLX1, djds_tbl%npUX1, WK_DJDS)
!
!  set_djds_ordering
!    ( output:: STACKmc, PEon, indexDJDS_L, indexDJDS_U,
!               NEWtoOLD_DJDS_L, NEWtoOLD_DJDS_U,
!               OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U)
!
      call set_djds_ordering(np_smp, NP, N, ISTACK_N_smp,               &
     &    djds_tbl%NHYP, djds_tbl%IVECT,                                &
     &    djds_tbl%STACKmcG, djds_tbl%STACKmc, djds_tbl%PEon,           &
     &    djds_tbl%npLX1, djds_tbl%npUX1, djds_tbl%NLmax,               &
     &    djds_tbl%NUmax, djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP,         &
     &    djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                   &
     &    djds_tbl%NEWtoOLD_DJDS_L, djds_tbl%NEWtoOLD_DJDS_U,           &
     &    djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U,           &
     &    WK_DJDS%INLmc, WK_DJDS%INUmc, WK_DJDS%IWKX, WK_DJDS%IW,       &
     &    WK_DJDS%inumDJDS_L, WK_DJDS%inumDJDS_U)
!C
!C== change connectivity
!
      call dealloc_iW_ordering(WK_DJDS)
!
      call alloc_conn_work_4_djds                                       &
     &   (N, djds_tbl%NLmax, djds_tbl%NUmax, WK_DJDS)
!
!  set_itotal_djds
!    ( output:: itotal_l, itotal_u) 
      call set_itotal_djds                                              &
     &   (np_smp, NP, N, WK_MC%ntot_mc_l, WK_MC%ntot_mc_u,              &
     &    WK_MC%istack_mc_l, WK_MC%istack_mc_u,                         &
     &    WK_MC%item_mc_l,   WK_MC%item_mc_u,                           &
     &    djds_tbl%NHYP, djds_tbl%npLX1, djds_tbl%npUX1,                &
     &    djds_tbl%NLmax, djds_tbl%NUmax,                               &
     &    djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP,                         &
     &    djds_tbl%itotal_l, djds_tbl%itotal_u,                         &
     &    djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                   &
     &    WK_DJDS%NEWtoOLDmc, WK_DJDS%IALmc, WK_DJDS%IAUmc)
      call dealloc_mc_connect(WK_MC)
!     call check_istack_and_items_mc(N, my_rank, WK_DJDS)
!
!C
!C== ADDRESS
!
      call alloc_type_address_4_DJDS(djds_tbl)
!
!  set_item_djds
!  (output:: COLORon, itemDJDS_L, itemDJDS_U, OLDtoNEW, NEWtoOLD, LtoU)
!
      call set_item_djds(np_smp, NP, N,                                 &
     &    djds_tbl%NHYP, djds_tbl%IVECT, djds_tbl%COLORon,              &
     &    djds_tbl%STACKmc, djds_tbl%NLmax, djds_tbl%NUmax,             &
     &    djds_tbl%npLX1, djds_tbl%npUX1,                               &
     &    djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP,                         &
     &    djds_tbl%itotal_l, djds_tbl%itotal_u,                         &
     &    djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                   &
     &    djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                     &
     &    djds_tbl%OLDtoNEW, djds_tbl%NEWtoOLD,                         &
     &    djds_tbl%NEWtoOLD_DJDS_L, djds_tbl%NEWtoOLD_DJDS_U,           &
     &    djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U,           &
     &    djds_tbl%LtoU, WK_DJDS%IALmc, WK_DJDS%IAUmc,                  &
     &    WK_DJDS%OLDtoNEWmc, WK_DJDS%inumDJDS_L, WK_DJDS%inumDJDS_U)
!
      call dealloc_work_4_djds(WK_DJDS)
!
!      call check_type_DJDS_ordering_info(my_rank, NP, djds_tbl)
!
      end subroutine s_reordering_djds_smp
!
!  ---------------------------------------------------------------------
!
      end module reordering_djds_smp_type
