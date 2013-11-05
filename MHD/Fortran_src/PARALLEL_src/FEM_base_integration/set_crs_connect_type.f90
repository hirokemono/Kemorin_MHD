!>@file   set_crs_connect_type.f90
!!@brief  module set_crs_connect_type
!!
!!@author H. Matsui
!!@date     Written by H. Matsui in Jan., 2009
!!@n        Modified by H. Matsui in Nov., 2013
!
!>      DJDS matrix connection from CRS matrix
!!
!!@verbatim
!!      subroutine s_set_crs_connect_type(np_smp, NP, ISTACK_NP_smp,    &
!!     &          ntot_next_nod_4_node, inod_next_stack_4_node,         &
!!     &          inod_next_4_node, tbl_crs)
!!        type(CRS_matrix_connect), intent(inout) :: tbl_crs
!!@endverbatim
!
      module set_crs_connect_type
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_crs_connect_type(np_smp, NP, ISTACK_NP_smp,      &
     &          ntot_next_nod_4_node, inod_next_stack_4_node,           &
     &          inod_next_4_node, tbl_crs)
!
      use t_crs_connect
!
      use set_crs_connection
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: NP
      integer(kind = kint), intent(in) :: ISTACK_NP_smp(0:np_smp)
!
      integer(kind = kint), intent(in) :: ntot_next_nod_4_node
      integer(kind = kint), intent(in)                                  &
     &      :: inod_next_stack_4_node(0:NP)
      integer(kind = kint), intent(in)                                  &
     &      :: inod_next_4_node(ntot_next_nod_4_node)
!
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
!
!
      call alloc_type_crs_stack(NP, tbl_crs)
!
      call count_item_crs(NP, np_smp, ISTACK_NP_smp,                    &
     &    ntot_next_nod_4_node, inod_next_stack_4_node,                 &
     &    inod_next_4_node, tbl_crs%num_crs_l, tbl_crs%num_crs_u)
!
      call s_cal_minmax_and_stacks(NP, tbl_crs%num_crs_l,               &
     &    izero, tbl_crs%istack_crs_l, tbl_crs%ntot_crs_l,              &
     &    tbl_crs%max_crs_l, tbl_crs%min_crs_l)
      call s_cal_minmax_and_stacks(NP, tbl_crs%num_crs_u,               &
     &    izero, tbl_crs%istack_crs_u, tbl_crs%ntot_crs_u,              &
     &    tbl_crs%max_crs_u, tbl_crs%min_crs_u)
!
      call alloc_type_crs_connect(tbl_crs)
!
      call set_item_crs(NP, np_smp, ISTACK_NP_smp,                      &
     &    ntot_next_nod_4_node, inod_next_stack_4_node,                 &
     &    inod_next_4_node, tbl_crs%ntot_crs_l, tbl_crs%ntot_crs_u,     &
     &    tbl_crs%istack_crs_l, tbl_crs%istack_crs_u,                   &
     &    tbl_crs%item_crs_l, tbl_crs%item_crs_u)
!
      end subroutine s_set_crs_connect_type
!
!-----------------------------------------------------------------------
!
      end module set_crs_connect_type
