!>@file   t_ucd_data.f90
!!@brief  module t_ucd_data
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Structure for Field data IO
!!
!!@verbatim
!!      subroutine allocate_ucd_node(ucd)
!!      subroutine allocate_ucd_ele(ucd)
!!      subroutine allocate_ucd_phys_name(ucd)
!!      subroutine allocate_ucd_phys_data(ucd)
!!      subroutine alloc_merged_hdt5_array(my_rank, ucd, m_ucd)
!!
!!      subroutine alloc_merged_ucd_stack(nprocs, m_ucd)
!!
!!      subroutine deallocate_ucd_node(ucd)
!!      subroutine deallocate_ucd_ele(ucd)
!!      subroutine deallocate_ucd_phys_data(ucd)
!!      subroutine deallocate_ucd_phys_name(ucd)
!!      subroutine deallocate_ucd_data(ucd)
!!
!!      subroutine disconnect_ucd_node(ucd)
!!      subroutine disconnect_ucd_data(ucd)
!!
!!      subroutine dealloc_merged_ucd_stack(m_ucd)
!!      subroutine dealloc_merged_hdt5_ele_list(m_ucd)
!!      subroutine dealloc_merged_hdt5_array(m_ucd)
!!
!!      subroutine cal_istack_ucd_component(ucd)
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
        character(len=kchara) :: file_prefix = "field/out"
!
!>        file type for field data IO
        integer (kind = kint) :: ifmt_file = iflag_fld
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
!
!>        number of field for IO
        integer(kind=kint) :: num_field
!>        total number of component for IO
        integer(kind=kint) :: ntot_comp
!>        number of component for each field
        integer(kind=kint), pointer :: num_comp(:)
!>        field name
        character (len=kchara), pointer :: phys_name(:)
!
!>        field data for IO
        real (kind=kreal), pointer :: d_ucd(:,:)
      end type ucd_data
!
!
!>        Structure for numbers of FEM mesh for merged IO
      type merged_ucd_data
!>        end point for number of node for each subdomain
        integer(kind = kint), pointer :: istack_merged_nod(:)
!>        end point for number of element for each subdomain
        integer(kind = kint), pointer :: istack_merged_ele(:)
!>        end point for number of internal node for each subdomain
        integer(kind = kint), pointer :: istack_merged_intnod(:)
!
!>        number of components for HDF data output
        integer(kind = kint) :: ncomp_hdf5
!>        Real work array for HDF data output
        real(kind=kreal), pointer :: fld_hdf5(:)
!>        Integer array for HDF data output
        integer(kind=kint), pointer :: ie_hdf5(:,:)
      end type merged_ucd_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_node(ucd)
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
      end subroutine allocate_ucd_node
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_ele(ucd)
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
      end subroutine allocate_ucd_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_phys_name(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      allocate( ucd%num_comp(ucd%num_field) )
      allocate( ucd%phys_name(ucd%num_field) )
!
      if(ucd%num_field .gt. 0) ucd%num_comp = 0
!
      end subroutine allocate_ucd_phys_name
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_phys_data(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      allocate(ucd%d_ucd(ucd%nnod,ucd%ntot_comp) )
      if( (ucd%nnod*ucd%ntot_comp) .gt. 0) ucd%d_ucd = 0.0d0
!
      end subroutine allocate_ucd_phys_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_merged_ucd_stack(nprocs, m_ucd)
!
      integer(kind = kint), intent(in) :: nprocs
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      allocate(m_ucd%istack_merged_nod(0:nprocs))
      allocate(m_ucd%istack_merged_ele(0:nprocs))
      allocate(m_ucd%istack_merged_intnod(0:nprocs))
!
      m_ucd%istack_merged_nod =    0
      m_ucd%istack_merged_ele =    0
      m_ucd%istack_merged_intnod = 0
!
      end subroutine alloc_merged_ucd_stack
!
! -----------------------------------------------------------------------
!
      subroutine alloc_merged_hdt5_array(my_rank, ucd, m_ucd)
!
      integer(kind = kint), intent(in) :: my_rank
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
      integer(kind = kint) :: nnod
!
!
      nnod = m_ucd%istack_merged_intnod(my_rank+1)                      &
     &      - m_ucd%istack_merged_intnod(my_rank)
      allocate( m_ucd%fld_hdf5(9*nnod) )
      allocate( m_ucd%ie_hdf5(8,ucd%nele) )
!
      end subroutine alloc_merged_hdt5_array
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_node(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      deallocate(ucd%xx, ucd%inod_global)
!
      end subroutine deallocate_ucd_node
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_ele(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      deallocate(ucd%ie, ucd%iele_global)
!
      end subroutine deallocate_ucd_ele
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_phys_data(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      deallocate(ucd%d_ucd)
!
      end subroutine deallocate_ucd_phys_data
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_phys_name(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      deallocate(ucd%num_comp, ucd%phys_name)
!
      end subroutine deallocate_ucd_phys_name
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_data(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call deallocate_ucd_phys_name(ucd)
      call deallocate_ucd_phys_data(ucd)
!
      end subroutine deallocate_ucd_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine disconnect_ucd_node(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      nullify(ucd%xx, ucd%inod_global)
!
      end subroutine disconnect_ucd_node
!
! -----------------------------------------------------------------------
!
      subroutine disconnect_ucd_data(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      nullify(ucd%num_comp)
      nullify(ucd%phys_name, ucd%d_ucd)
!
      end subroutine disconnect_ucd_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_merged_ucd_stack(m_ucd)
!
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      deallocate(m_ucd%istack_merged_nod)
      deallocate(m_ucd%istack_merged_ele)
      deallocate(m_ucd%istack_merged_intnod)
!
      end subroutine dealloc_merged_ucd_stack
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_merged_hdt5_ele_list(m_ucd)
!
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      deallocate(m_ucd%ie_hdf5)
!
      end subroutine dealloc_merged_hdt5_ele_list
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_merged_hdt5_array(m_ucd)
!
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      deallocate(m_ucd%fld_hdf5)
!
      end subroutine dealloc_merged_hdt5_array
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_istack_ucd_component(ucd)
!
      type(ucd_data), intent(inout) :: ucd
      integer(kind = kint) :: inum
!
!
      ucd%ntot_comp = 0
      do inum = 1, ucd%num_field
        ucd%ntot_comp = ucd%ntot_comp + ucd%num_comp(inum)
      end do
!
      end subroutine cal_istack_ucd_component
!
! -------------------------------------------------------------------
!
      end module t_ucd_data
