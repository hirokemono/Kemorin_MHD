!set_boundary_potentials.f90
!      module set_boundary_potentials
!
!      Written by H. Matsui on July, 2005
!
!      subroutine set_boundary_phi
!      subroutine set_boundary_m_phi
!      subroutine set_boundary_ff
!      subroutine set_boundary_fmag
!
      module set_boundary_potentials
!
      use m_precision
      use m_constants
!
      use m_geometry_data
      use m_finite_element_matrix
!
      use set_fixed_boundaries
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_m_phi
!
      use m_node_phys_data
      use m_node_phys_address
      use m_bc_data_magne
      use set_velocity_boundary
!
!
      call set_boundary_scalar                                          &
     &   (nod_bc1_f, bc_mag_p_id_apt, iphys%i_m_phi, nod_fld1)
!
      end subroutine set_boundary_m_phi
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_ff
!
      use m_bc_data_velo
      use m_surf_data_press
      use m_phys_constants
      use m_finite_element_matrix
!
!
      if (nod_bc1_p%num_bc_nod .le. 0) return
      call set_fixed_bc_scalar_phys                                     &
     &   (nod_bc1_p%num_bc_nod, nod_bc1_p%ibc_id,                       &
     &    bc_p_id_apt, node1%numnod, n_vector, ione, f1_l%ff)
!
      end subroutine set_boundary_ff
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_fmag
!
      use m_bc_data_magne
      use m_phys_constants
      use m_finite_element_matrix
!
!
      if (nod_bc1_f%num_bc_nod .le. 0) return
        call set_fixed_bc_scalar_phys                                   &
     &   (nod_bc1_f%num_bc_nod, nod_bc1_f%ibc_id,                       &
     &    bc_mag_p_id_apt, node1%numnod, n_vector, ione, f1_l%ff)
!
      end subroutine set_boundary_fmag
!
!  ---------------------------------------------------------------------
!
      end module set_boundary_potentials
