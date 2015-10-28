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
      use m_constants
!
      use m_geometry_data
      use m_bc_data_magne
      use m_surf_data_magne
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
      subroutine set_boundary_magne
!
      use m_node_phys_data
      use m_node_phys_address
!
!
      if (nod_bc1_b%nmax_bc .gt. 0) then
          call set_fixed_bc_vect_phys                                   &
     &       (nod_bc1_b%nmax_bc, nod_bc1_b%num_bc_nod,                  &
     &        nod_bc1_b%ibc_id, bc_b_id_apt, node1%numnod,              &
     &        nod_fld1%ntot_phys, iphys%i_magne, nod_fld1%d_fld)
      end if
!
      end subroutine set_boundary_magne
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_magne_4_rhs
!
      use m_phys_constants
      use m_finite_element_matrix
!
!
      if (nod_bc1_b%nmax_bc .le. 0) return
      call del_2vector_phys_on_bc                                       &
     &   (nod_bc1_b%nmax_bc, nod_bc1_b%num_bc_nod,                      &
     &    nod_bc1_b%ibc_id, node1%numnod, n_vector, ione,               &
     &    f1_l%ff, f1_nl%ff)
!
      end subroutine set_boundary_magne_4_rhs
!
!  ---------------------------------------------------------------------
!
      subroutine delete_field_by_fixed_b_bc(i_field)
!
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: i_field
!
!
      if (nod_bc1_b%nmax_bc/=0) then
        call del_vector_phys_on_bc                                      &
     &     (nod_bc1_b%nmax_bc, nod_bc1_b%num_bc_nod,                    &
     &      nod_bc1_b%ibc_id, node1%numnod,                             &
     &      nod_fld1%ntot_phys, i_field, nod_fld1%d_fld)
      end if
!
      end subroutine delete_field_by_fixed_b_bc
!
!  ---------------------------------------------------------------------
!
      end module set_magne_boundary
