!>@file   m_work_const_itp_table.f90
!!@brief  module m_work_const_itp_table
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in Aug., 2006
!
!> @brief Work array to construct interpolation table
!!
!!@verbatim
!!      subroutine allocate_work_const_itp_tbl(numnod)
!!      subroutine deallocate__work_const_itp_tbl
!!
!!      subroutine allocate_itp_work_dest(num_org_pe)
!!      subroutine deallocate_itp_work_dest
!!
!!      subroutine swap_interpolation_table(idest, inod)
!!      subroutine copy_table_2_order
!!
!!      subroutine check_search_ID(id_file)
!!@endverbatim
!
      module m_work_const_itp_table
!
      use m_precision
!
     implicit none
!
!
!> Structure for istack_itp_wtype_dest
      type istack_dest_type
!>   end address to receive interpolated data including interpolate type
        integer(kind = kint), pointer :: istack_itp_wtype_dest(:)
      end type istack_dest_type
!
!
      integer(kind = kint), allocatable :: id_search_area(:,:)
      integer(kind = kint), allocatable :: iflag_org_domain(:)
!
      integer(kind = kint), allocatable :: istack_org_para_type(:)
!
!>   number of node to be interpolated in each original domain
      integer(kind = kint), allocatable :: numnod_dest(:)
!>   number of node to be interpolated in each original domain and type
      integer(kind = kint), allocatable :: nnod_table_wtype_dest(:)
!
      integer(kind = kint), allocatable :: inod_dest_ordered(:)
      integer(kind = kint), allocatable :: iele_orgin_ordered(:)
      real(kind = kreal), allocatable :: coef_inter_ordered(:,:)
      private :: inod_dest_ordered, iele_orgin_ordered
      private :: coef_inter_ordered
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
      subroutine allocate_work_const_itp_tbl(numnod)
!
      use m_interpolate_table_dest
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate( id_search_area(ntot_table_dest,3) )
      allocate( iflag_org_domain(numnod) )
!
      iflag_org_domain = 0
      id_search_area = 0
!
      allocate( inod_dest_ordered(ntot_table_dest) )
      allocate( iele_orgin_ordered(ntot_table_dest) )
      allocate( coef_inter_ordered(ntot_table_dest,3) )
!
     coef_inter_ordered = 0.0d0
     inod_dest_ordered = 0
     iele_orgin_ordered = 0
!
      end subroutine allocate_work_const_itp_tbl
!
!-----------------------------------------------------------------------
!
      subroutine allocate_istack_org_ptype(num_dest_pe)
!
      integer(kind = kint), intent(in) :: num_dest_pe
!
!
      allocate( istack_org_para_type(0:4*num_dest_pe) )
      istack_org_para_type = 0
!
      end subroutine allocate_istack_org_ptype
!
!-----------------------------------------------------------------------
!
      subroutine deallocate__work_const_itp_tbl
!
!
      deallocate( id_search_area )
      deallocate( iflag_org_domain )
!
      deallocate( coef_inter_ordered )
      deallocate( iele_orgin_ordered, inod_dest_ordered )
!
      end subroutine deallocate__work_const_itp_tbl
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_istack_org_ptype
!
!
      deallocate(istack_org_para_type)
!
      end subroutine deallocate_istack_org_ptype
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_work_dest(num_org_pe)
!
      integer(kind = kint), intent(in) :: num_org_pe
!
!
      allocate( numnod_dest(0:num_org_pe) )
      allocate( nnod_table_wtype_dest(4*num_org_pe) )
!
      if(num_org_pe .gt. 0) then
        numnod_dest = 0
        nnod_table_wtype_dest = 0
      end if
!
      end subroutine allocate_itp_work_dest
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_work_dest
!
!
      deallocate( numnod_dest )
      deallocate( nnod_table_wtype_dest )
!
      end subroutine deallocate_itp_work_dest
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine swap_interpolation_table(idest, inod)
!
      use m_interpolate_table_dest
      use m_interpolate_coefs_dest
!
      integer(kind = kint), intent(in) :: idest, inod
!
!
      inod_dest_ordered(idest) =       inod_dest_4_dest(inod)
      iele_orgin_ordered(idest) =      iele_org_4_dest(inod)
      coef_inter_ordered(idest,1:3) = coef_inter_dest(inod,1:3)
!
      end subroutine swap_interpolation_table
!
!-----------------------------------------------------------------------
!
      subroutine copy_table_2_order
!
      use m_interpolate_table_dest
      use m_interpolate_coefs_dest
!
!
      inod_dest_4_dest(1:ntot_table_dest)                               &
     &      = inod_dest_ordered(1:ntot_table_dest)
      iele_org_4_dest(1:ntot_table_dest)                                &
     &         = iele_orgin_ordered(1:ntot_table_dest)
!
      coef_inter_dest(1:ntot_table_dest,1)                              &
     &         = coef_inter_ordered(1:ntot_table_dest,1)
      coef_inter_dest(1:ntot_table_dest,2)                              &
     &         = coef_inter_ordered(1:ntot_table_dest,2)
      coef_inter_dest(1:ntot_table_dest,3)                              &
     &         = coef_inter_ordered(1:ntot_table_dest,3)
!
!
      end subroutine copy_table_2_order
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_search_ID(id_file)
!
      use m_interpolate_table_dest
!
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: inod
!
      write(id_file,*) '#'
      write(id_file,*) '#  sphere block ID for search'
      write(id_file,*) '#'
!
      write(id_file,'(i16)') ntot_table_dest
      do inod = 1, ntot_table_dest
        write(id_file,'(4i16)') inod, id_search_area(inod,1:3)
      end do
!
      end subroutine check_search_ID
!
!-----------------------------------------------------------------------
!
      end module m_work_const_itp_table
