!set_velocity_boundary.f90
!      module set_velocity_boundary
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Nov. 2003
!        modified by H.Matsui on Jan. 2004
!        modified by H.Matsui on July 2005
!
!      subroutine set_boundary_velo
!      subroutine set_boundary_velo_4_rhs
!      subroutine delete_field_by_fixed_v_bc(i_field)
!
!      subroutine set_boundary_scalar(scalar_bc, i_field, nod_fld)
!      subroutine set_boundary_vect(vector_bc, i_field, nod_fld)
!      subroutine delete_vector_on_bc(vector_bc, i_field, nod_fld)
!
      module set_velocity_boundary
!
      use m_precision
      use m_constants
!
      use m_geometry_data
!
      use m_bc_data_velo
      use m_surf_data_torque
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
      subroutine set_boundary_velo
!
      use m_control_parameter
      use m_group_data
      use m_node_phys_address
      use m_node_phys_data
!
      use set_normal_field
      use set_nodal_bc_4_velo
      use set_boundary_scalars
!
!     set normal velocity
!
      call set_normal_velocity(sf_grp1, sf_grp_nod1)
!
!     set fixed velocity
!
      call set_boundary_vect(nod_bc1_v, iphys%i_velo, nod_fld1)
!
!   set rotation boundary
      call set_boundary_rot_vect                                        &
     &   (node1, nod_bc1_rot, iphys%i_velo, nod_fld1)
!
!   boundary condition for special case
!     ( please write every time!!)
      call set_boundary_specific_vect                             &
     &         (node1, nod_bc1_vsp, iphys%i_velo, nod_fld1)
!
!
      call delete_radial_vector_on_bc                                   &
     &   (node1, nod_bc1_vr0, iphys%i_velo, nod_fld1)
!
      end subroutine set_boundary_velo
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_velo_4_rhs
!
      use m_phys_constants
      use m_finite_element_matrix
      use set_boundary_scalars
!
!
      call delete_vector_ffs_on_bc(node1, nod_bc1_v, f1_l, f1_nl)
      call delete_vector_ffs_rot_bc(node1, nod_bc1_rot, f1_l, f1_nl)
      call set_vector_ffs_special_bc(node1, nod_bc1_vsp, f1_l)
!
      end subroutine set_boundary_velo_4_rhs
!
!  ---------------------------------------------------------------------
!
      subroutine delete_field_by_fixed_v_bc(i_field)
!
      use m_node_phys_data
      use set_boundary_scalars
!
      integer(kind = kint), intent(in) :: i_field
!
!
      call delete_vector_on_bc(nod_bc1_v, i_field, nod_fld1)
      call delete_vector_by_rot_v_bc(nod_bc1_rot, i_field, nod_fld1)
      call delete_vector_by_fixed_t_bc(nod_bc1_vsp, i_field, nod_fld1)
!
      end subroutine delete_field_by_fixed_v_bc
!
!  ---------------------------------------------------------------------
!
      end module set_velocity_boundary
