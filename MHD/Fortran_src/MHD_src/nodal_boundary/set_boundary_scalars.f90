!
!      module set_boundary_scalars
!
!      Written by H. Matsui on July, 2005
!
!      subroutine set_boundary_part_temp
!      subroutine set_boundary_ene_4_rhs
!      subroutine delete_field_by_fixed_t_bc(i_field)
!      subroutine delete_vector_by_fixed_t_bc(i_field)
!      subroutine set_boundary_composition
!      subroutine set_boundary_composition_4_rhs
!
      module set_boundary_scalars
!
      use m_precision
      use m_constants
!
      use m_geometry_data
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
      subroutine set_boundary_ene_4_rhs
!
      use m_phys_constants
      use m_finite_element_matrix
      use m_bc_data_ene
      use m_surf_data_temp
!
!
      if (nod_bc1_t%num_bc_nod .le. 0) return
      call del_2scalar_phys_on_bc                                       &
     &   (nod_bc1_t%num_bc_nod, nod_bc1_t%ibc_id,                       &
     &    node1%numnod, n_vector, ione, f1_l%ff, f1_nl%ff)
!
      end subroutine set_boundary_ene_4_rhs
!
!  ---------------------------------------------------------------------
!
      subroutine delete_field_by_fixed_t_bc(i_field)
!
      use m_geometry_data
      use m_node_phys_data
      use m_bc_data_ene
      use m_surf_data_temp
!
      integer(kind = kint), intent(in) :: i_field
!
!
      if (nod_bc1_t%num_bc_nod .gt. 0) then
       call del_scalar_phys_on_bc                                       &
     &    (nod_bc1_t%num_bc_nod, nod_bc1_t%ibc_id,                      &
     &     node1%numnod, nod_fld1%ntot_phys, i_field, nod_fld1%d_fld)
      end if
!
      end subroutine delete_field_by_fixed_t_bc
!
!  ---------------------------------------------------------------------
!
      subroutine delete_vector_by_fixed_t_bc(i_field)
!
      use m_node_phys_data
      use m_bc_data_ene
      use m_surf_data_temp
!
!
      integer(kind = kint), intent(in) :: i_field
!
!
      if (nod_bc1_t%num_bc_nod .gt. 0) then
       call del_vector_phys_on_1bc                                      &
     &    (nod_bc1_t%num_bc_nod, nod_bc1_t%ibc_id,                      &
     &     node1%numnod, nod_fld1%ntot_phys, i_field, nod_fld1%d_fld)
      end if
!
      end subroutine delete_vector_by_fixed_t_bc
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_composition_4_rhs
!
      use m_phys_constants
      use m_finite_element_matrix
      use m_bc_data_ene
      use m_surf_data_composition
!
!
      if (nod_bc1_c%num_bc_nod .le. 0) return
      call del_2scalar_phys_on_bc                                       &
     &   (nod_bc1_c%num_bc_nod, nod_bc1_c%ibc_id,                       &
     &    node1%numnod, n_vector, ione, f1_l%ff, f1_nl%ff)
!
      end subroutine set_boundary_composition_4_rhs
!
!  ---------------------------------------------------------------------
!
      end module set_boundary_scalars
