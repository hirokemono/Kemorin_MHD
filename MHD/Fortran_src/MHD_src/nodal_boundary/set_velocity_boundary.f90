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
!      subroutine set_boundary_scalar(scalar_bc, bc_id_apt,             &
!     &          i_field, nod_fld)
!      subroutine set_boundary_vect(vector_bc, bc_id_apt,               &
!     &          i_field, nod_fld)
!      subroutine delete_vector_on_bc(vector_bc, i_field, nod_fld)
!      subroutine delete_vector_ffs_on_bc(node, vector_bc, f_l, f_nl)
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
!
!     set normal velocity
!
      call set_normal_velocity(sf_grp1, sf_grp_nod1)
!
!     set fixed velocity
!
      call set_boundary_vect                                            &
     &   (nod_bc1_v, bc_v_id_apt, iphys%i_velo, nod_fld1)
!
!   set rotation boundary
!
      if (nod_bc1_rot%num_bc_nod .gt. 0) then
       call set_rotation_boundary(node1%numnod, node1%xx,               &
     &     nod_bc1_rot%num_bc_nod, nod_bc1_rot%ibc_id,                  &
     &     nod_bc1_rot%bc_rot_apt, nod_fld1%ntot_phys, iphys%i_velo,    &
     &     nod_fld1%d_fld)
      end if
!
!   boundary condition for special case
!     ( please write every time!!)
!
      if (nod_bc1_vsp%num_bc_nod .gt. 0) then
        call set_specific_boundary_velo(node1%numnod, node1%xx,         &
     &      nod_bc1_vsp%num_bc_nod, nod_bc1_vsp%ibc_id, bc_vsp_id_apt,  &
     &      nod_fld1%ntot_phys, iphys%i_velo, nod_fld1%d_fld)
      end if
!
!
      if (nod_bc1_vr0%num_bc_nod .gt. 0) then
        call del_radial_velocity(node1%numnod, node1%xx, node1%a_r,     &
     &      nod_bc1_vr0%num_bc_nod, nod_bc1_vr0%ibc_id,                 &
     &      nod_fld1%ntot_phys, iphys%i_velo, nod_fld1%d_fld)
      end if
!
      end subroutine set_boundary_velo
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_velo_4_rhs
!
      use set_nodal_bc_4_velo
      use m_phys_constants
      use m_finite_element_matrix
!
!
      call delete_vector_ffs_on_bc(node1, nod_bc1_v, f1_l, f1_nl)
!
      if (nod_bc1_rot%num_bc_nod .gt. 0) then
        call set_fixed_bc_zero_ff_rot(node1%numnod,                     &
     &      nod_bc1_rot%num_bc_nod, nod_bc1_rot%ibc_id,   &
     &      f1_l%ff, f1_nl%ff)
      end if
!
      if (nod_bc1_vsp%num_bc_nod .gt. 0) then
        call set_specific_boundary_velo_rhs(node1%numnod,               &
     &      nod_bc1_vsp%num_bc_nod, nod_bc1_vsp%ibc_id, f1_l%ff)
      end if
!
!
      end subroutine set_boundary_velo_4_rhs
!
!  ---------------------------------------------------------------------
!
      subroutine delete_field_by_fixed_v_bc(i_field)
!
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: i_field
!
!
      call delete_vector_on_bc(nod_bc1_v, i_field, nod_fld1)
!
      if (nod_bc1_rot%num_bc_nod/=0) then
       call del_vector_phys_on_1bc     &
     &    (nod_bc1_rot%num_bc_nod, nod_bc1_rot%ibc_id,  &
     &     node1%numnod, nod_fld1%ntot_phys, i_field, nod_fld1%d_fld)
      end if
!
      if (nod_bc1_vsp%num_bc_nod/=0) then
       call del_vector_phys_on_1bc     &
     &    (nod_bc1_vsp%num_bc_nod, nod_bc1_vsp%ibc_id,  &
     &     node1%numnod, nod_fld1%ntot_phys, i_field, nod_fld1%d_fld)
      end if
!
!
      end subroutine delete_field_by_fixed_v_bc
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_scalar(scalar_bc, bc_id_apt,              &
     &          i_field, nod_fld)
!
      use t_phys_data
!
      type(scaler_fixed_nod_bc_type), intent(in) :: scalar_bc
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: bc_id_apt(scalar_bc%num_bc_nod)
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (scalar_bc%num_bc_nod .le. 0) return
      call set_fixed_bc_scalar_phys                                     &
     &    (scalar_bc%num_bc_nod, scalar_bc%ibc_id, bc_id_apt,           &
     &     nod_fld%n_point, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine set_boundary_scalar
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_vect(vector_bc, bc_id_apt,                &
     &          i_field, nod_fld)
!
      use t_phys_data
!
      type(vect_fixed_nod_bc_type), intent(in) :: vector_bc
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: bc_id_apt(vector_bc%nmax_bc,3)
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (vector_bc%nmax_bc .le. 0) return
      call set_fixed_bc_vect_phys                                       &
     &   (vector_bc%nmax_bc, vector_bc%num_bc_nod,                      &
     &    vector_bc%ibc_id, bc_id_apt,                                  &
     &    nod_fld%n_point, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine set_boundary_vect
!
!  ---------------------------------------------------------------------
!
      subroutine delete_vector_on_bc(vector_bc, i_field, nod_fld)
!
      use t_phys_data
!
      type(vect_fixed_nod_bc_type), intent(in) :: vector_bc
!
      integer(kind = kint), intent(in) :: i_field
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (vector_bc%nmax_bc .le. 0) return
      call del_vector_phys_on_bc                                        &
     &   (vector_bc%nmax_bc, vector_bc%num_bc_nod, vector_bc%ibc_id,    &
     &    nod_fld%n_point, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine delete_vector_on_bc
!
!  ---------------------------------------------------------------------
!
      subroutine delete_vector_ffs_on_bc(node, vector_bc, f_l, f_nl)
!
      use t_finite_element_mat
!
      type(node_data), intent(in) :: node
      type(vect_fixed_nod_bc_type), intent(in) :: vector_bc
!
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      if (vector_bc%nmax_bc .le. 0) return
      call del_2vector_phys_on_bc                                       &
     &   (vector_bc%nmax_bc, vector_bc%num_bc_nod, vector_bc%ibc_id,    &
     &    node%numnod, n_vector, ione, f_l%ff, f_nl%ff)
!
      end subroutine delete_vector_ffs_on_bc
!
!  ---------------------------------------------------------------------
!
      end module set_velocity_boundary
