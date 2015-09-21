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
      module set_velocity_boundary
!
      use m_precision
!
      use m_bc_data_velo
      use m_bc_data_rotate
!      use m_bc_data_vfree
      use m_bc_data_vr0
      use m_bc_data_vsp
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
      use m_geometry_data
      use m_group_data
      use m_node_phys_address
      use m_node_phys_data
!
      use set_normal_field
      use set_fixed_phys_boundary
      use set_nodal_bc_4_velo
      use delete_radial_velocity
!
!     set normal velocity
!
      call set_normal_velocity(sf_grp1, sf_grp_nod1)
!
!     set fixed velocity
!
      if (nmax_bc_v_nod .gt. 0) then
        call set_fixed_bc_vect_phys(nmax_bc_v_nod, num_bc_v_nod,        &
     &      ibc_v_id, bc_v_id_apt, node1%numnod, nod_fld1%ntot_phys,    &
     &      iphys%i_velo, d_nod)
      end if
!
!   set rotation boundary
!
      if (num_bc_v10_nod .gt. 0) then
       call set_rotation_boundary(node1%numnod, node1%xx,               &
     &     num_bc_v10_nod, ibc_v10_id, bc_v10_id_apt)
      end if
!
!   boundary condition for special case
!     ( please write every time!!)
!
      if (num_bc_vsp_nod .gt. 0) then
        call set_specific_boundary_velo(node1%numnod, node1%xx,         &
     &      num_bc_vsp_nod, ibc_vsp_id, bc_vsp_id_apt)
      end if
!
!
      if (num_bc_vr0_nod .gt. 0) then
        call del_radial_velocity(node1%numnod, node1%xx, node1%a_r,     &
     &      nod_fld1%ntot_phys, iphys%i_velo, d_nod)
      end if
!
      end subroutine set_boundary_velo
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_velo_4_rhs
!
      use set_nodal_bc_4_velo
!
      if (nmax_bc_v_nod/=0) then
       call set_fixed_bc_zero_ff_vect(nmax_bc_v_nod, num_bc_v_nod,      &
     &     ibc_v_id)
      end if
!
      if (num_bc_v10_nod/=0) then
        call set_fixed_bc_zero_ff_rot(num_bc_v10_nod, ibc_v10_id)
      end if
!
      if (num_bc_vsp_nod/=0) then
        call set_specific_boundary_velo_rhs(num_bc_vsp_nod,             &
     &    ibc_vsp_id)
      end if
!
!
      end subroutine set_boundary_velo_4_rhs
!
!  ---------------------------------------------------------------------
!
      subroutine delete_field_by_fixed_v_bc(i_field)
!
      use m_geometry_data
      use m_node_phys_data
      use set_fixed_phys_boundary
!
      integer(kind = kint), intent(in) :: i_field
!
!
      if (nmax_bc_v_nod/=0) then
       call del_vector_phys_on_bc(nmax_bc_v_nod, num_bc_v_nod,          &
     &     ibc_v_id, node1%numnod, nod_fld1%ntot_phys, i_field,         &
     &     d_nod)
      end if
!
      if (num_bc_v10_nod/=0) then
       call del_vector_phys_on_1bc(num_bc_v10_nod, ibc_v10_id,          &
     &     node1%numnod, nod_fld1%ntot_phys, i_field, d_nod)
      end if
!
      if (num_bc_vsp_nod/=0) then
       call del_vector_phys_on_1bc(num_bc_vsp_nod, ibc_vsp_id,          &
     &     node1%numnod, nod_fld1%ntot_phys, i_field, d_nod)
      end if
!
!
      end subroutine delete_field_by_fixed_v_bc
!
!  ---------------------------------------------------------------------
!
      end module set_velocity_boundary
