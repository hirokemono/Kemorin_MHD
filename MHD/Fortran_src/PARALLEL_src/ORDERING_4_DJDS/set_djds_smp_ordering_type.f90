!>@file   set_djds_smp_ordering_type.f90
!!@brief  module set_djds_smp_ordering_type
!!
!!@author H. Matsui
!!@author K. Nakajima and H. Matsui
!!@date        Written by K. Nakajima in 2001
!!@n      modified by H. Matsui on May. 2002
!!@n      modified by H. Matsui on June. 2006
!!@n      modified by H. Matsui on Jan., 2009
!!@n      modified by H. Matsui on Nov., 2013
!
!>     Subroutines to construct DJDS orderd matrix
!!
!!@verbatim
!!      subroutine count_hyperplane_type(np_smp, N, NP, djds_tbl)
!!        type(DJDS_ordering_table), intent(inout) :: djds_tbl
!!      subroutine set_djds_ordering_type(np_smp, NP, N, ISTACK_N_smp,  &
!!     &          djds_tbl)
!!        type(DJDS_ordering_table), intent(inout) :: djds_tbl
!!      subroutine set_itotal_djds_type(np_smp, N, djds_tbl)
!!        type(DJDS_ordering_table), intent(inout) :: djds_tbl
!!      subroutine set_item_djds_type(np_smp, NP, N, djds_tbl)
!!        type(DJDS_ordering_table), intent(inout) :: djds_tbl
!!      subroutine set_new_comm_table_type(NP, nod_comm, djds_tbl)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(DJDS_ordering_table), intent(inout) :: djds_tbl
!!@endverbatim
!
      module set_djds_smp_ordering_type
!
      use m_precision
!
      use t_solver_djds
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_hyperplane_type(np_smp, N, NP, djds_tbl)
!
      use DJDS_hyperplane
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: NP, N
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      call count_hyperplane(np_smp, N, NP,                              &
     &                  djds_tbl%NHYP, djds_tbl%IVECT,                  &
     &                  djds_tbl%npLX1, djds_tbl%npUX1,                 &
     &                  djds_tbl%NLmax, djds_tbl%NUmax,                 &
     &                  djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP,           &
     &                  djds_tbl%itotal_l, djds_tbl%itotal_u)
!
      end subroutine count_hyperplane_type
!
! ----------------------------------------------------------------------
!
      subroutine set_djds_ordering_type(np_smp, NP, N, ISTACK_N_smp,    &
     &          djds_tbl)
!
      use DJDS_ordering
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: ISTACK_N_smp(0:np_smp)
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      call set_djds_ordering(np_smp, NP, N, ISTACK_N_smp,               &
     &    djds_tbl%NHYP, djds_tbl%IVECT,                                &
     &    djds_tbl%STACKmcG, djds_tbl%STACKmc, djds_tbl%PEon,           &
     &    djds_tbl%npLX1, djds_tbl%npUX1, djds_tbl%NLmax,               &
     &    djds_tbl%NUmax, djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP,         &
     &    djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                   &
     &    djds_tbl%NEWtoOLD_DJDS_L, djds_tbl%NEWtoOLD_DJDS_U,           &
     &    djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U)
!
      end subroutine set_djds_ordering_type
!
! ----------------------------------------------------------------------
!
      subroutine set_itotal_djds_type(np_smp, N, djds_tbl)
!
      use DJDS_total_nondiag
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: N
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      call set_itotal_djds(np_smp, N, djds_tbl%NHYP,                    &
     &    djds_tbl%npLX1, djds_tbl%npUX1,                               &
     &    djds_tbl%NLmax, djds_tbl%NUmax,                               &
     &    djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP,                         &
     &    djds_tbl%itotal_l, djds_tbl%itotal_u,                         &
     &    djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U)
!
      end subroutine set_itotal_djds_type
!
! ----------------------------------------------------------------------
!
      subroutine set_item_djds_type(np_smp, NP, N, djds_tbl)
!
      use DJDS_nodiag_item
!
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: np_smp
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      call set_item_djds(np_smp, NP, N,                                 &
     &      djds_tbl%NHYP, djds_tbl%IVECT, djds_tbl%COLORon,            &
     &      djds_tbl%STACKmc, djds_tbl%NLmax, djds_tbl%NUmax,           &
     &      djds_tbl%npLX1, djds_tbl%npUX1,                             &
     &      djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP,                       &
     &      djds_tbl%itotal_l, djds_tbl%itotal_u,                       &
     &      djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                 &
     &      djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                   &
     &      djds_tbl%OLDtoNEW, djds_tbl%NEWtoOLD,                       &
     &      djds_tbl%NEWtoOLD_DJDS_L, djds_tbl%NEWtoOLD_DJDS_U,         &
     &      djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U,         &
     &      djds_tbl%LtoU)
!
      end subroutine set_item_djds_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_new_comm_table_type(NP, nod_comm, djds_tbl)
!
      use t_comm_table
      use DJDS_new_comm_table
!
      integer(kind = kint), intent(in) :: NP
      type(communication_table), intent(in) :: nod_comm
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      call alloc_type_new_comm_table(nod_comm%ntot_export, djds_tbl)
!
      call set_new_comm_table(NP, djds_tbl%OLDtoNEW,                    &
     &    nod_comm%num_neib, nod_comm%istack_export,                    &
     &    nod_comm%item_export, djds_tbl%NOD_EXPORT_NEW)
!
      end subroutine set_new_comm_table_type
!
!-----------------------------------------------------------------------
!
      end module set_djds_smp_ordering_type
