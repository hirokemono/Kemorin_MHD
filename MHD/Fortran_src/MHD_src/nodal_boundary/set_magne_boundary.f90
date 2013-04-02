!set_magne_boundary.f90
!      module set_magne_boundary
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Nov. 2003
!        modified by H.Matsui on July 2005
!
!      subroutine set_boundary_magne
!      subroutine set_boundary_magne_4_rhs
!      subroutine delete_field_by_fixed_b_bc(i_field)
!
      module set_magne_boundary
!
      use m_precision
!
      use m_bc_data_magne
      use m_surf_data_magne
!
      use set_fixed_phys_boundary
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_magne
!
!    boundary condition setting
!
      use m_control_parameter
      use m_node_phys_address
!
!
      if (nmax_bc_b_nod/=0) then
          call set_fixed_bc_vect_phys(nmax_bc_b_nod, num_bc_b_nod,      &
     &        ibc_b_id, bc_b_id_apt, iphys%i_magne)
      end if
!
      end subroutine set_boundary_magne
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_magne_4_rhs
!
      use set_fixed_boundaries
!
!
        if (nmax_bc_b_nod/=0) then
          call set_fixed_bc_zero_ff_vect(nmax_bc_b_nod, num_bc_b_nod,   &
     &        ibc_b_id)
        end if
!
      end subroutine set_boundary_magne_4_rhs
!
!  ---------------------------------------------------------------------
!
      subroutine delete_field_by_fixed_b_bc(i_field)
!
      integer(kind = kint), intent(in) :: i_field
!
!
      if (nmax_bc_b_nod/=0) then
          call del_vector_phys_on_bc(nmax_bc_b_nod, num_bc_b_nod,       &
     &     ibc_b_id, i_field)
      end if
!
      end subroutine delete_field_by_fixed_b_bc
!
!  ---------------------------------------------------------------------
!
      end module set_magne_boundary
