!
!      module set_boundary_scalars
!
!      Written by H. Matsui on July, 2005
!
!      subroutine set_boundary_ene
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
      subroutine set_boundary_ene
!
      use m_node_phys_data
      use m_node_phys_address
      use m_bc_data_ene
      use m_surf_data_temp
!
!
      if (num_bc_e_nod .gt. 0) then
       call set_fixed_bc_scalar_phys(num_bc_e_nod, ibc_e_id,            &
     &     bc_e_id_apt, node1%numnod, nod_fld1%ntot_phys, iphys%i_temp, &
     &     nod_fld1%d_fld)
      end if
!
      end subroutine set_boundary_ene
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_part_temp
!
      use m_node_phys_data
      use m_node_phys_address
      use m_bc_data_ene
      use m_surf_data_temp
!
!
      if (num_bc_e_nod .gt. 0) then
       call set_fixed_bc_scalar_phys(num_bc_e_nod, ibc_e_id,            &
     &     bc_e_id_apt, node1%numnod, nod_fld1%ntot_phys,               &
     &     iphys%i_par_temp, nod_fld1%d_fld)
      end if
!
      end subroutine set_boundary_part_temp
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
      if (num_bc_e_nod .le. 0) return
      call del_2scalar_phys_on_bc(num_bc_e_nod, ibc_e_id,               &
     &    node1%numnod, n_vector, ione, f1_l%ff, ff_nl)
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
      if (num_bc_e_nod .gt. 0) then
       call del_scalar_phys_on_bc(num_bc_e_nod, ibc_e_id,               &
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
      if (num_bc_e_nod .gt. 0) then
       call del_vector_phys_on_1bc(num_bc_e_nod, ibc_e_id,              &
     &     node1%numnod, nod_fld1%ntot_phys, i_field, nod_fld1%d_fld)
      end if
!
      end subroutine delete_vector_by_fixed_t_bc
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_composition
!
      use m_geometry_data
      use m_node_phys_data
      use m_node_phys_address
      use m_bc_data_composition
      use m_surf_data_composition
!
!
      if (num_bc_composition_nod .gt. 0) then
        call set_fixed_bc_scalar_phys(num_bc_composition_nod,           &
     &     ibc_composit_id, bc_composit_id_apt, node1%numnod,           &
     &     nod_fld1%ntot_phys, iphys%i_light, nod_fld1%d_fld)
      end if
!
      end subroutine set_boundary_composition
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_composition_4_rhs
!
      use m_phys_constants
      use m_finite_element_matrix
      use m_bc_data_composition
      use m_surf_data_composition
!
!
      if (num_bc_composition_nod .le. 0) return
      call del_2scalar_phys_on_bc                                       &
     &   (num_bc_composition_nod, ibc_composit_id,                      &
     &    node1%numnod, n_vector, ione, f1_l%ff, ff_nl)
!
      end subroutine set_boundary_composition_4_rhs
!
!  ---------------------------------------------------------------------
!
      end module set_boundary_scalars
