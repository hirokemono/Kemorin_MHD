!
!     module m_boundary_field_IO
!.......................................................................
!
!     module m_boundary_field_IO
!
!      subroutine allocate_num_bc_values
!      subroutine allocate_boundary_values
!
!      subroutine deallocate_boundary_values
!
      module m_boundary_field_IO
!
      use m_precision
!
      implicit  none
!
      integer(kind=kint) :: num_bc_group_IO
!   number of boundary data from data file
      character(len=kchara), allocatable :: bc_group_type_IO(:)
      character(len=kchara), allocatable :: bc_data_group_IO(:)
!  group name for boundary data
!
      character(len=kchara), allocatable :: bc_field_type_IO(:)
!  data type of boundary data
!
      integer(kind=kint), allocatable ::  istack_bc_data_IO(:)
!  stack ID of boundary data from data file
!
      integer(kind=kint) :: ntot_boundary_field_IO
!   total number of boundary data from data file
      integer(kind=kint), allocatable :: id_local_bc_fld_IO(:)
      real(kind=kreal), allocatable :: boundary_field_IO(:)
!  values of boundary data from data file
!
      character(len=kchara),parameter :: flag_nod_grp=  'Node_group'
      character(len=kchara),parameter :: flag_surf_grp= 'Surface_group'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_num_bc_values
!
      allocate( bc_data_group_IO(num_bc_group_IO) )
      allocate( bc_group_type_IO(num_bc_group_IO) )
      allocate( bc_field_type_IO(num_bc_group_IO) )
      allocate( istack_bc_data_IO(0:num_bc_group_IO) )
!
      istack_bc_data_IO = 0
!
      end subroutine allocate_num_bc_values
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_boundary_values
!
      allocate( id_local_bc_fld_IO(ntot_boundary_field_IO) )
      allocate( boundary_field_IO(ntot_boundary_field_IO) )
!
      id_local_bc_fld_IO = 0
      boundary_field_IO = 0.0d0
!
      end subroutine allocate_boundary_values
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_boundary_values
!
!
      deallocate( boundary_field_IO,  id_local_bc_fld_IO)
!
      deallocate( bc_data_group_IO, bc_group_type_IO)
      deallocate( bc_field_type_IO  )
      deallocate( istack_bc_data_IO )
!
      end subroutine deallocate_boundary_values
!
!  ---------------------------------------------------------------------
!
      end module m_boundary_field_IO
