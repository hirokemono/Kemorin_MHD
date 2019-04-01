!>@file   t_work_const_itp_table.f90
!!@brief  module t_work_const_itp_table
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in Aug., 2006
!
!> @brief Work array to construct interpolation table
!!
!!@verbatim
!!      subroutine alloc_work_const_itp_tbl(itp_dest, orderd)
!!      subroutine deallocate__work_const_itp_tbl
!!      subroutine dealloc_istack_org_ptype(cst_itp_wk)
!!
!!      subroutine swap_interpolation_table                             &
!!     &         (idest, inod, itp_dest, itp_coef, orderd)
!!        type(interpolate_table_dest), intent(in) :: itp_dest
!!        type(interpolate_coefs_dest), intent(in) :: itp_coef
!!        type(ordered_list), intent(inout) :: orderd
!!      subroutine copy_table_2_order(orderd, itp_dest, itp_coef)
!!        type(ordered_list), intent(in) :: orderd
!!        type(interpolate_table_dest), intent(inout) :: itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: itp_coef
!!@endverbatim
!
      module t_work_const_itp_table
!
      use m_precision
!
     implicit none
!
!
!> Structure for istack_itp_wtype_dest
      type istack_dest_type
!>   end address to receive interpolated data including interpolate type
        integer(kind = kint), allocatable :: istack_itp_wtype_dest(:)
      end type istack_dest_type
!
      type ordered_list
        integer(kind = kint), allocatable :: inod_dest_ordered(:)
        integer(kind = kint), allocatable :: iele_orgin_ordered(:)
        real(kind = kreal), allocatable :: coef_inter_ordered(:,:)
      end type ordered_list
!
      type work_const_itp_table
        integer(kind = kint), allocatable :: iflag_org_domain(:)
        integer(kind = kint), allocatable :: istack_org_para_type(:)
!
        type(ordered_list) :: orderd
      end type work_const_itp_table
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_istack_itp_wtype_dest(num_org_domain,            &
     &          itp_stack_dest)
!
      integer(kind = kint), intent(in) :: num_org_domain
      type(istack_dest_type), intent(inout) :: itp_stack_dest
      integer(kind = kint) :: num
!
!
      num = 4*num_org_domain
      allocate(itp_stack_dest%istack_itp_wtype_dest(0:num))
      itp_stack_dest%istack_itp_wtype_dest = 0
!
      end subroutine alloc_istack_itp_wtype_dest
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_istack_itp_wtype_dest(itp_stack_dest)
!
      type(istack_dest_type), intent(inout) :: itp_stack_dest
!
!
      deallocate(itp_stack_dest%istack_itp_wtype_dest)
!
      end subroutine dealloc_istack_itp_wtype_dest
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_work_const_itp_tbl(itp_dest, orderd)
!
      use t_interpolate_tbl_dest
!
      type(interpolate_table_dest), intent(in) :: itp_dest
      type(ordered_list), intent(inout) :: orderd
!
      allocate(orderd%inod_dest_ordered(itp_dest%ntot_table_dest))
      allocate(orderd%iele_orgin_ordered(itp_dest%ntot_table_dest))
      allocate(orderd%coef_inter_ordered(itp_dest%ntot_table_dest,3))
!
      orderd%coef_inter_ordered = 0.0d0
      orderd%inod_dest_ordered = 0
      orderd%iele_orgin_ordered = 0
!
      end subroutine alloc_work_const_itp_tbl
!
!-----------------------------------------------------------------------
!
      subroutine alloc_iflag_org_domain(numnod, cst_itp_wk)
!
      integer(kind = kint), intent(in) :: numnod
      type(work_const_itp_table), intent(inout) :: cst_itp_wk
!
      allocate(cst_itp_wk%iflag_org_domain(numnod) )
      if(numnod .gt. 0) cst_itp_wk%iflag_org_domain = 0
!
      end subroutine alloc_iflag_org_domain
!
!-----------------------------------------------------------------------
!
      subroutine alloc_istack_org_ptype(num_dest_pe, cst_itp_wk)
!
      integer(kind = kint), intent(in) :: num_dest_pe
      type(work_const_itp_table), intent(inout) :: cst_itp_wk
!
!
      allocate(cst_itp_wk%istack_org_para_type(0:4*num_dest_pe) )
      cst_itp_wk%istack_org_para_type = 0
!
      end subroutine alloc_istack_org_ptype
!
!-----------------------------------------------------------------------
!
      subroutine deallocate__work_const_itp_tbl(orderd)
!
      type(ordered_list), intent(inout) :: orderd
!
      deallocate(orderd%coef_inter_ordered)
      deallocate(orderd%iele_orgin_ordered, orderd%inod_dest_ordered)
!
      end subroutine deallocate__work_const_itp_tbl
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_iflag_org_domain(cst_itp_wk)
!
      type(work_const_itp_table), intent(inout) :: cst_itp_wk
!
      deallocate(cst_itp_wk%iflag_org_domain)
!
      end subroutine dealloc_iflag_org_domain
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_istack_org_ptype(cst_itp_wk)
!
      type(work_const_itp_table), intent(inout) :: cst_itp_wk
!
!
      deallocate(cst_itp_wk%istack_org_para_type)
!
      end subroutine dealloc_istack_org_ptype
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine swap_interpolation_table                               &
     &         (idest, inod, itp_dest, itp_coef, orderd)
!
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      integer(kind = kint), intent(in) :: idest, inod
      type(interpolate_table_dest), intent(in) :: itp_dest
      type(interpolate_coefs_dest), intent(in) :: itp_coef
!
      type(ordered_list), intent(inout) :: orderd
!
!
      orderd%inod_dest_ordered(idest) = itp_dest%inod_dest_4_dest(inod)
      orderd%iele_orgin_ordered(idest) = itp_coef%iele_org_4_dest(inod)
      orderd%coef_inter_ordered(idest,1:3)                              &
     &      = itp_coef%coef_inter_dest(inod,1:3)
!
      end subroutine swap_interpolation_table
!
!-----------------------------------------------------------------------
!
      subroutine copy_table_2_order(orderd, itp_dest, itp_coef)
!
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      type(ordered_list), intent(in) :: orderd
!
      type(interpolate_table_dest), intent(inout) :: itp_dest
      type(interpolate_coefs_dest), intent(inout) :: itp_coef
!
!
      itp_dest%inod_dest_4_dest(1:itp_dest%ntot_table_dest)             &
     &      = orderd%inod_dest_ordered(1:itp_dest%ntot_table_dest)
      itp_coef%iele_org_4_dest(1:itp_dest%ntot_table_dest)              &
     &      = orderd%iele_orgin_ordered(1:itp_dest%ntot_table_dest)
!
      itp_coef%coef_inter_dest(1:itp_dest%ntot_table_dest,1)            &
     &      = orderd%coef_inter_ordered(1:itp_dest%ntot_table_dest,1)
      itp_coef%coef_inter_dest(1:itp_dest%ntot_table_dest,2)            &
     &      = orderd%coef_inter_ordered(1:itp_dest%ntot_table_dest,2)
      itp_coef%coef_inter_dest(1:itp_dest%ntot_table_dest,3)            &
     &      = orderd%coef_inter_ordered(1:itp_dest%ntot_table_dest,3)
!
!
      end subroutine copy_table_2_order
!
!-----------------------------------------------------------------------

      end module t_work_const_itp_table
