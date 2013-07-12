!>@file   t_ucd_data.f90
!!@brief  module t_ucd_data
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Structure for Field data IO
!!
!!@verbatim
!!      subroutine alloc_ucd_node_t(ucd)
!!      subroutine alloc_ucd_ele_t(ucd)
!!      subroutine alloc_ucd_phys_name_t(ucd)
!!      subroutine alloc_ucd_phys_data_t(ucd)
!!
!!      subroutine dealloc_ucd_node_t(ucd)
!!      subroutine dealloc_ucd_ele_t(ucd)
!!      subroutine dealloc_ucd_phys_data_t(ucd)
!!      subroutine dealloc_ucd_phys_name_t(ucd)
!!      subroutine dealloc_ucd_data_t(ucd)
!!
!!      subroutine disconnect_ucd_node_t(ucd)
!!      subroutine disconnect_ucd_data_t(ucd)
!!
!!      subroutine cal_istack_ucd_comp_type(ucd)
!!
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!
      module t_ucd_data
!
      use m_precision
      use m_field_file_format
      use m_file_format_switch
!
      implicit none
!
!>        Structure for FEM field data IO
      type ucd_data
!>        file name for field data
        character(len=kchara) :: file_name
!
!
!>        file header for field data
        character(len=kchara) :: header_name = "field/out"
!
!>        file type for field data IO
        integer (kind = kint) :: itype_data_file = iflag_fld
!
!>        number of nodes for field data
        integer(kind = kint) :: nnod
!>        number of elements for field data
        integer(kind = kint) :: nele
!>        number of nodes for each element for field data
        integer(kind = kint) :: nnod_4_ele
!
!>        position of nodes
        real (kind=kreal), pointer :: xx(:,:)
!>        global node ID
        integer(kind = kint), pointer :: inod_global(:)
!>        global element ID
        integer(kind = kint), pointer :: iele_global(:)
!>        element connectivity
        integer(kind = kint), pointer :: ie(:,:)
!>        element flag for hexahedral element
        character (len=5) :: hexmark = ' hex '
!>        element flag for triangle element
        character (len=5) :: trimark = ' tri '
!
!
!>        number of field for IO
        integer(kind=kint) :: num_field
!>        total number of component for IO
        integer(kind=kint) :: ntot_comp
!>        number of component for each field
        integer(kind=kint), pointer :: num_comp(:)
!>        end address of component for each field
        integer(kind=kint), pointer :: istack_comp(:)
!>        field name
        character (len=kchara), pointer :: phys_name(:)
!
!>        field data for IO
          real (kind=kreal), pointer :: d_ucd(:,:)
        end type ucd_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_ucd_node_t(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      allocate(ucd%xx(ucd%nnod,3))
      allocate(ucd%inod_global(ucd%nnod))
!
      if(ucd%nnod .gt. 0) then
        ucd%xx = 0.0d0
        ucd%inod_global = 0
      end if
!
      end subroutine alloc_ucd_node_t
!
! -----------------------------------------------------------------------
!
      subroutine alloc_ucd_ele_t(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      allocate(ucd%ie(ucd%nele,ucd%nnod_4_ele))
      allocate(ucd%iele_global(ucd%nele))
!
      if(ucd%nele .gt. 0) then
        ucd%ie = 0
        ucd%iele_global =   0
      end if
!
      end subroutine alloc_ucd_ele_t
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_ucd_phys_name_t(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      allocate( ucd%num_comp(ucd%num_field) )
      allocate( ucd%istack_comp(0:ucd%num_field) )
      allocate( ucd%phys_name(ucd%num_field) )
!
      ucd%istack_comp = 0
      if(ucd%num_field .gt. 0) ucd%num_comp = 0
!
      end subroutine alloc_ucd_phys_name_t
!
! -----------------------------------------------------------------------
!
      subroutine alloc_ucd_phys_data_t(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      allocate(ucd%d_ucd(ucd%nnod,ucd%ntot_comp) )
      if( (ucd%nnod*ucd%ntot_comp) .gt. 0) ucd%d_ucd = 0.0d0
!
      end subroutine alloc_ucd_phys_data_t
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_ucd_node_t(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      deallocate(ucd%xx, ucd%inod_global)
!
      end subroutine dealloc_ucd_node_t
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ucd_ele_t(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      deallocate(ucd%ie, ucd%iele_global)
!
      end subroutine dealloc_ucd_ele_t
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ucd_phys_data_t(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      deallocate(ucd%d_ucd)
!
      end subroutine dealloc_ucd_phys_data_t
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ucd_phys_name_t(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      deallocate(ucd%num_comp, ucd%istack_comp)
      deallocate(ucd%phys_name)
!
      end subroutine dealloc_ucd_phys_name_t
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ucd_data_t(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call dealloc_ucd_phys_name_t(ucd)
      call dealloc_ucd_phys_data_t(ucd)
!
      end subroutine dealloc_ucd_data_t
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine disconnect_ucd_node_t(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      nullify(ucd%xx, ucd%inod_global)
!
      end subroutine disconnect_ucd_node_t
!
! -----------------------------------------------------------------------
!
      subroutine disconnect_ucd_data_t(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      nullify(ucd%num_comp, ucd%istack_comp)
      nullify(ucd%phys_name, ucd%d_ucd)
!
      end subroutine disconnect_ucd_data_t
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_istack_ucd_comp_type(ucd)
!
      use m_constants
      use cal_minmax_and_stacks
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call s_cal_total_and_stacks(ucd%num_field, ucd%num_comp,          &
     &    izero, ucd%istack_comp, ucd%ntot_comp)
!
      end subroutine cal_istack_ucd_comp_type
!
! -------------------------------------------------------------------
!
      end module t_ucd_data
