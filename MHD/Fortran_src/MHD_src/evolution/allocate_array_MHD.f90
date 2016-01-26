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
      use m_node_phys_data
      use m_element_phys_data
      use m_finite_element_matrix
      use m_int_vol_data
      use m_mean_square_values
      use m_SGS_address
      use t_FEM_phys_data
!
!
      label_sim = 'GeoFEM_MHD'
!
      if (iflag_debug.ge.1) write(*,*) 'alloc_finite_elem_mat'
      call alloc_finite_elem_mat                                        &
     &   (node1, ele1, m1_lump, fem1_wk, f1_l, f1_nl)
      call alloc_mass_mat_fluid(node1%numnod, mhd_fem1_wk)
      call alloc_mass_mat_conduct(node1%numnod, mhd_fem1_wk)
!
      if (iflag_debug.ge.1) write(*,*) 'allocate_int_vol_data'
      call allocate_int_vol_data(ele1%numele, node1%max_nod_smp)
      call set_SGS_addresses
!
!  allocation for field values
     if (iflag_debug.ge.1)  write(*,*) 'set_field_address_type'
      call set_field_address_type(node1%numnod, nod_fld1, iphys)
     if (iflag_debug.ge.1)  write(*,*) 'initialize_ele_field_data'
      call initialize_ele_field_data
!
!
      if ( iflag_debug.ge.1 ) write(*,*) 'set_mean_square_values'
      call count_mean_square_values(nod_fld1)
      call set_mean_square_values(nod_fld1)
!
      end subroutine allocate_array
!
! ----------------------------------------------------------------------
!
      end module allocate_array_MHD
