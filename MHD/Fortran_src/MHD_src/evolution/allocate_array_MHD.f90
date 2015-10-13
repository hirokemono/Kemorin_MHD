!
!     module allocate_array_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on July, 2006
!        Modified by H. Matsui on May, 2007
!
!      subroutine allocate_array
!
      module allocate_array_MHD
!
      use m_precision
      use m_machine_parameter
!
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_array
!
      use m_geometry_data
      use m_group_data
      use m_control_parameter
      use m_phys_constants
      use m_node_phys_address
      use m_element_phys_data
      use m_finite_element_matrix
      use m_int_vol_data
      use m_bulk_values
!
!
      label_sim = 'GeoFEM_MHD'
!
      if (iflag_debug.ge.1) write(*,*) 'allocate_finite_elem_mt'
      call allocate_finite_elem_mt
      call alloc_mass_mat_conduct(node1%numnod, mhd_fem1_wk)
!
      if (iflag_debug.ge.1) write(*,*) 'allocate_int_vol_data'
      call allocate_int_vol_data(ele1%numele)
!
!
!  allocation for field values
     if (iflag_debug.ge.1)  write(*,*) 'initialize_nod_field_data'
      call initialize_nod_field_data
     if (iflag_debug.ge.1)  write(*,*) 'initialize_ele_field_data'
      call initialize_ele_field_data
!
!
      if ( iflag_debug.ge.1 ) write(*,*) 'set_bulk_values'
      call count_bulk_values
      call set_bulk_values
!
      end subroutine allocate_array
!
! ----------------------------------------------------------------------
!
      end module allocate_array_MHD
