!set_vecp_boundary.f90
!      module set_vecp_boundary
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Nov. 2003
!        modified by H.Matsui on July 2005
!
!      subroutine set_boundary_vect_p
!      subroutine set_boundary_vect_p_4_rhs
!      subroutine delete_field_by_fixed_a_bc(i_field)
!      subroutine set_boundary_current
!
      module set_vecp_boundary
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_vect_p
!
      use m_control_parameter
      use m_node_phys_address
      use m_bc_data_vect_p
      use m_surf_data_vector_p
!
      use set_fixed_phys_boundary
!
!
      if (nmax_bc_vp_nod/=0) then
          call set_fixed_bc_vect_phys(nmax_bc_vp_nod, num_bc_vp_nod, &
     &        ibc_vp_id, bc_vp_id_apt, iphys%i_vecp )
      end if
!
      end subroutine set_boundary_vect_p
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_vect_p_4_rhs
!
      use m_bc_data_vect_p
      use m_surf_data_vector_p
!
      use set_fixed_boundaries
!
!
        if (nmax_bc_vp_nod/=0) then
          call set_fixed_bc_zero_ff_vect(nmax_bc_vp_nod,                &
     &        num_bc_vp_nod, ibc_vp_id)
        end if
!
      end subroutine set_boundary_vect_p_4_rhs
!
!  ---------------------------------------------------------------------
!
      subroutine delete_field_by_fixed_a_bc(i_field)
!
      use m_bc_data_vect_p
      use m_surf_data_vector_p
!
      use set_fixed_phys_boundary
!
!
      integer(kind = kint), intent(in) :: i_field
!
!
      if (nmax_bc_vp_nod/=0) then
          call del_vector_phys_on_bc(nmax_bc_vp_nod, num_bc_vp_nod,     &
     &     ibc_vp_id, i_field)
      end if
!
      end subroutine delete_field_by_fixed_a_bc
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_current
!
      use m_node_phys_address
      use m_bc_data_current
      use m_surf_data_current
!
      use set_fixed_phys_boundary
!
!
        if (nmax_bc_j_nod/=0) then
          call set_fixed_bc_vect_phys(nmax_bc_j_nod, num_bc_j_nod,      &
     &        ibc_j_id, bc_j_id_apt, iphys%i_current)
        end if
!
!
      end subroutine set_boundary_current
!
!  ---------------------------------------------------------------------
!
      end module set_vecp_boundary
