!m_node_phys_data.f90
!     module m_node_phys_data
!
!> @brief nodal field data for FEM
!
!     Written by H. Matsui
!
!      subroutine initialize_nod_field_data
!
!       subroutine deallocate_phys_name
!       subroutine deallocate_data_arrays
!
      module m_node_phys_data
!
      use m_precision
      use t_phys_data
      use t_phys_address
!
      implicit  none
!
!>       label   for simulation
      character(len=kchara)   :: label_sim
!
!>       Structure for nodal field data
      type(phys_data), save :: nod_fld1
!
!>       address for nodal fields
      type(phys_address), save :: iphys
!
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine initialize_nod_field_data
!
      use m_geometry_data
      use set_field_address
!
!
!  allocation for physical values
!
      call alloc_phys_data_type(node1%numnod, nod_fld1)
!
!   set address of nodal values
!
      call set_field_addresses(ione, nod_fld1%num_phys,                 &
     &    nod_fld1%phys_name, nod_fld1%num_component, iphys)
!
       end subroutine initialize_nod_field_data
!
!  --------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
       subroutine deallocate_phys_name
!
       call dealloc_phys_name_type(nod_fld1)
!
       end subroutine deallocate_phys_name
!
!  --------------------------------------------------------------------
!
       subroutine deallocate_data_arrays
!
       call dealloc_phys_data_type(nod_fld1)
!
       end subroutine deallocate_data_arrays
!
!  --------------------------------------------------------------------
!
      end module m_node_phys_data
