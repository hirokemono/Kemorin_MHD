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
!
      use m_parallel_var_dof
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
      use m_array_for_send_recv
      use m_machine_parameter
      use m_control_parameter
      use m_geometry_parameter
      use m_phys_constants
      use m_finite_element_matrix
      use m_int_vol_data
      use m_bulk_values
      use m_surface_group
      use initialize_phys_data
!
!
      if (iflag_debug.ge.1) write(*,*) 'allocate_finite_elem_mt'
      call allocate_finite_elem_mt
!
      if (iflag_debug.ge.1) write(*,*) 'allocate_int_vol_data'
      call allocate_int_vol_data
!
!
      if (iflag_debug.ge.1) write(*,*) 'allocate_phys_data'
      call allocate_phys_data
!
      if (iflag_debug.ge.1 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(n_sym_tensor, numnod)
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
