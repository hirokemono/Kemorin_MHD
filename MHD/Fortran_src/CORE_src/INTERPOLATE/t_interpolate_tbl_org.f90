!>@file   t_interpolate_tbl_org.f90
!!@brief  module t_interpolate_tbl_org
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in Nov., 2008
!
!> @brief Structure of interpolation table for source mesh
!!
!!@verbatim
!!      subroutine set_num_dest_domain(num_dest_pe, tbl_org)
!!      subroutine alloc_type_itp_num_org(np_smp, tbl_org)
!!      subroutine alloc_type_itp_table_org(tbl_org)
!!      subroutine alloc_type_zero_itp_tbl_org(np_smp, tbl_org)
!!
!!      subroutine deallocate_itp_num_org(tbl_org)
!!      subroutine dealloc_type_itp_table_org(tbl_org)
!!
!!      subroutine set_stack_tbl_wtype_org_smp(tbl_org)
!!@endverbatim
!
      module t_interpolate_tbl_org
!
      use m_precision
!
      implicit none
!
!
!> Structure of interpolation table for source grid
      type interpolate_table_org
!
!>   number of subdomain to send interpolated data
        integer(kind = kint) :: num_dest_domain
!>   flag if target nodes have same prosess
        integer(kind = kint) :: iflag_self_itp_send
!>   subdomain rank to send interpolated data
        integer(kind = kint), pointer :: id_dest_domain(:)
!>   end address to send interpolated data
        integer(kind = kint), pointer :: istack_nod_tbl_org(:)
!>   end address for interplation modes
        integer(kind = kint), pointer :: istack_itp_type_org(:)
!
!>   total number of node to interpolate in original subdomain
        integer(kind = kint) :: ntot_table_org
!>   export table for interpolation
        integer(kind = kint), pointer :: inod_itp_send(:)
!>   global node ID for target domain
        integer(kind = kint), pointer :: inod_gl_dest_4_org(:)
!>   local element ID to make interpolation
        integer(kind = kint), pointer :: iele_org_4_org(:)
!>   interpolation type ID
        integer(kind = kint), pointer :: itype_inter_org(:)
!>   Coordinate of target node in element coordinate
        real(kind = kreal), pointer :: coef_inter_org(:,:)
!
!>   end address of table to interpolation at original elements
        integer(kind = kint), pointer :: istack_tbl_type_org_smp(:)
!>   maximum number of interpolation at original elements
        integer(kind = kint) :: imax_tbl_wtype_org_smp
!
      end type interpolate_table_org
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_num_dest_domain(num_dest_pe, tbl_org)
!
      integer(kind = kint), intent(in) :: num_dest_pe
      type(interpolate_table_org), intent(inout) :: tbl_org
!
      tbl_org%num_dest_domain = num_dest_pe
!
      end subroutine set_num_dest_domain
!
!-----------------------------------------------------------------------
!
      subroutine alloc_type_itp_num_org(np_smp, tbl_org)
!
      integer(kind = kint), intent(in) :: np_smp
      type(interpolate_table_org), intent(inout) :: tbl_org
!
!
      allocate( tbl_org%id_dest_domain(tbl_org%num_dest_domain) )
      allocate( tbl_org%istack_nod_tbl_org(0:tbl_org%num_dest_domain) )
      allocate( tbl_org%istack_itp_type_org(0:4) )
!
      allocate(tbl_org%istack_tbl_type_org_smp(0:4*np_smp))
!
      if (tbl_org%num_dest_domain .gt. 0) tbl_org%id_dest_domain = 0
      tbl_org%istack_nod_tbl_org =      0
      tbl_org%istack_itp_type_org =     0
      tbl_org%istack_tbl_type_org_smp = 0
!
      end subroutine alloc_type_itp_num_org
!
!-----------------------------------------------------------------------
!
      subroutine alloc_type_itp_table_org(tbl_org)
!
      type(interpolate_table_org), intent(inout) :: tbl_org
!
!
      allocate( tbl_org%inod_itp_send(tbl_org%ntot_table_org) )
      allocate( tbl_org%inod_gl_dest_4_org(tbl_org%ntot_table_org) )
      allocate( tbl_org%iele_org_4_org(tbl_org%ntot_table_org) )
      allocate( tbl_org%itype_inter_org(tbl_org%ntot_table_org) )
      allocate( tbl_org%coef_inter_org(tbl_org%ntot_table_org,3) )
!
      if (tbl_org%ntot_table_org .gt. 0) then
        tbl_org%inod_itp_send =      0
        tbl_org%inod_gl_dest_4_org = 0
        tbl_org%iele_org_4_org =     0
        tbl_org%itype_inter_org =   -1
        tbl_org%coef_inter_org = 0.0d0
      end if
!
      end subroutine alloc_type_itp_table_org
!
!-----------------------------------------------------------------------
!
      subroutine alloc_type_zero_itp_tbl_org(np_smp, tbl_org)
!
      use m_constants
!
      integer(kind = kint), intent(in) :: np_smp
      type(interpolate_table_org), intent(inout) :: tbl_org
!
!
      tbl_org%num_dest_domain = 0
      tbl_org%ntot_table_org =  0
      call alloc_type_itp_num_org(np_smp, tbl_org)
      call alloc_type_itp_table_org(tbl_org)
!
      end subroutine alloc_type_zero_itp_tbl_org
!
!------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_num_org(tbl_org)
!
      type(interpolate_table_org), intent(inout) :: tbl_org
!
      deallocate( tbl_org%id_dest_domain )
      deallocate( tbl_org%istack_nod_tbl_org )
      deallocate( tbl_org%istack_itp_type_org )
      deallocate( tbl_org%istack_tbl_type_org_smp )
!
      end subroutine deallocate_itp_num_org
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_type_itp_table_org(tbl_org)
!
      type(interpolate_table_org), intent(inout) :: tbl_org
!
      deallocate( tbl_org%inod_itp_send )
      deallocate( tbl_org%inod_gl_dest_4_org )
      deallocate( tbl_org%iele_org_4_org )
      deallocate( tbl_org%itype_inter_org )
      deallocate( tbl_org%coef_inter_org )
!
      end subroutine dealloc_type_itp_table_org
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_stack_tbl_wtype_org_smp(tbl_org)
!
      use m_machine_parameter
      use cal_minmax_and_stacks
!
      type(interpolate_table_org), intent(inout) :: tbl_org
!
      integer(kind = kint) :: itype, ist, ied
      integer(kind = kint) :: ist_smp, ied_smp
!
!
      do itype = 1, 4
        ist = tbl_org%istack_itp_type_org(itype-1) + 1
        ied = tbl_org%istack_itp_type_org(itype  )
        ist_smp = np_smp * (itype-1)
        ied_smp = np_smp *  itype
        call count_number_4_smp( np_smp, ist, ied,                      &
     &        tbl_org%istack_tbl_type_org_smp(ist_smp:ied_smp),         &
     &        tbl_org%imax_tbl_wtype_org_smp)
      end do
!
      end subroutine set_stack_tbl_wtype_org_smp
!
!-----------------------------------------------------------------------
!
      end module t_interpolate_tbl_org
