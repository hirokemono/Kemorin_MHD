!>@file   reordering_djds_smp_type.f90
!!@brief  module reordering_djds_smp_type
!!
!!@author H. Matsui
!!@author K. Nakajima and H. Matsui
!!@date        Written by H. Matsui
!!@n      modified by H. Matsui on May. 2007
!!@n      modified by H. Matsui on Jan., 2009
!!@n      modified by H. Matsui on Nov., 2013
!
!>     Construct DJDS ordering table in structure
!!
!!@verbatim
!!      subroutine s_reordering_djds_smp_type(np_smp, NP, N,            &
!!     &          ISTACK_N_smp, solver_C, tbl_crs, djds_tbl)
!!@endverbatim
!
      module reordering_djds_smp_type
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_reordering_djds_smp_type(np_smp, NP, N,              &
     &          ISTACK_N_smp, solver_C, tbl_crs, djds_tbl)
!
      use t_crs_connect
      use t_solver_djds
      use t_vector_for_solver
!
      use m_matrix_work
      use m_colored_connect
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
!
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
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
      call count_rcm_type(NP, N, solver_C, tbl_crs, djds_tbl)
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
!
!C-- Off-Diagonal Component Number
!
      call alloc_type_number_4_djds(djds_tbl)
      call allocate_work_num_4_djds(NP)
!
!   count_hyperplane
!    ( output :: itotal_l, itotal_u, NLmax, NUmax, npLX1, npUX1
!                IVECT, NLmaxHYP, NUmaxHYP)
!
      call count_hyperplane_type(np_smp, NP, N, djds_tbl)
!
      call deallocate_4_IVECmc
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
      call alloc_type_lists_4_DJDS(np_smp, NP, djds_tbl)
      call allocate_work_4_djds(NP, djds_tbl%NHYP,                      &
     &    djds_tbl%npLX1, djds_tbl%npUX1)
!
!  set_djds_ordering
!    ( output:: STACKmc, PEon, indexDJDS_L, indexDJDS_U,
!               NEWtoOLD_DJDS_L, NEWtoOLD_DJDS_U,
!               OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U)
!
      call set_djds_ordering_type(np_smp, NP, N, ISTACK_N_smp,          &
    &     djds_tbl)
!C
!C== change connectivity
!
      call deallocate_iW_ordering
!
      call allocate_conn_work_4_djds(N, djds_tbl%NLmax, djds_tbl%NUmax)
!
!  set_itotal_djds
!    ( output:: itotal_l, itotal_u) 
!
      call set_itotal_djds_type(np_smp, N, djds_tbl)
!
      call deallocate_mc_connect
!C
!C== ADDRESS
!
      call alloc_type_address_4_DJDS(djds_tbl)
!
!  set_item_djds
!  (output:: COLORon, itemDJDS_L, itemDJDS_U, OLDtoNEW, NEWtoOLD, LtoU)
!
      call set_item_djds_type(np_smp, NP, N, djds_tbl)
!
      call deallocate_work_4_djds
!
!      call check_type_DJDS_ordering_info(my_rank, NP, djds_tbl)
!
      end subroutine s_reordering_djds_smp_type
!
!  ---------------------------------------------------------------------
!
      end module reordering_djds_smp_type
