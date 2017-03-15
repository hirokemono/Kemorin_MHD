!
!      module set_boundary_scalars
!
!      Written by H. Matsui on July, 2005
!
!!      subroutine set_boundary_ff(node, scalar_bc, f_l)
!!      subroutine set_boundary_rhs_scalar(node, scalar_bc, f_l, f_nl)
!!      subroutine delete_vector_ffs_on_bc(node, vector_bc, f_l, f_nl)
!!      subroutine delete_vector_ffs_rot_bc(node, rot_bc, f_l, f_nl)
!!      subroutine set_vector_ffs_special_bc(node, vsp_bc, f_l)
!!
!!      subroutine delete_vector_on_bc(vector_bc, i_field, nod_fld)
!!      subroutine delete_radial_vector_on_bc                           &
!!     &         (node, vr0_bc, i_field, nod_fld)
!!      subroutine set_boundary_scalar(scalar_bc, i_field, nod_fld)
!!      subroutine set_boundary_vect(vector_bc, i_field, nod_fld)
!!      subroutine set_boundary_rot_vect(node, rot_bc, i_field, nod_fld)
!!      subroutine set_boundary_specific_vect                           &
!!     &         (time, node, vsp_bc, i_field, nod_fld)
!!
!!      subroutine delete_field_by_fixed_s_bc                           &
!!     &         (scalar_bc, i_field, nod_fld)
!!      subroutine delete_vector_by_fixed_t_bc                          &
!!     &         (scalar_bc, i_field, nod_fld)
!!      subroutine delete_vector_by_rot_v_bc(rot_bc, i_field, nod_fld)
!
      module set_boundary_scalars
!
      use m_precision
      use m_constants
!
      use t_nodal_bc_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_ff(node, scalar_bc, f_l)
!
      use m_phys_constants
      use t_geometry_data
      use t_finite_element_mat
!
      use set_fixed_boundaries
!
      type(node_data), intent(in) :: node
      type(scaler_fixed_nod_bc_type), intent(in) :: scalar_bc
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (scalar_bc%num_bc_nod .le. 0) return
      call set_fixed_bc_scalar_phys                                     &
     &   (scalar_bc%num_bc_nod, scalar_bc%ibc_id,                       &
     &    scalar_bc%bc_apt, node%numnod, n_vector, ione, f_l%ff)
!
      end subroutine set_boundary_ff
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_rhs_scalar(node, scalar_bc, f_l, f_nl)
!
      use m_phys_constants
      use t_geometry_data
      use t_finite_element_mat
!
      use set_fixed_boundaries
!
      type(node_data), intent(in) :: node
      type(scaler_fixed_nod_bc_type), intent(in) :: scalar_bc
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      if (scalar_bc%num_bc_nod .le. 0) return
      call del_2scalar_phys_on_bc                                       &
     &   (scalar_bc%num_bc_nod, scalar_bc%ibc_id,                       &
     &    node%numnod, n_vector, ione, f_l%ff, f_nl%ff)
!
      end subroutine set_boundary_rhs_scalar
!
!  ---------------------------------------------------------------------
!
      subroutine delete_vector_ffs_on_bc(node, vector_bc, f_l, f_nl)
!
      use t_geometry_data
      use t_finite_element_mat
!
      use set_fixed_boundaries
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
      subroutine delete_vector_ffs_rot_bc(node, rot_bc, f_l, f_nl)
!
      use t_geometry_data
      use t_finite_element_mat
!
      use set_nodal_bc_4_velo
!
      type(node_data), intent(in) :: node
      type(scaler_rotaion_nod_bc_type), intent(in) :: rot_bc
!
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
!
      if (rot_bc%num_bc_nod .le. 0) return
      call set_fixed_bc_zero_ff_rot(rot_bc%num_bc_nod, rot_bc%ibc_id,   &
     &    node%numnod, f_l%ff, f_nl%ff)
!
      end subroutine delete_vector_ffs_rot_bc
!
!  ---------------------------------------------------------------------
!
      subroutine set_vector_ffs_special_bc(node, vsp_bc, f_l)
!
      use t_geometry_data
      use t_finite_element_mat
!
      use set_nodal_bc_4_velo
!
      type(node_data), intent(in) :: node
      type(scaler_fixed_nod_bc_type), intent(in) :: vsp_bc
!
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (vsp_bc%num_bc_nod .le. 0) return
      call set_specific_boundary_velo_rhs                               &
     &   (vsp_bc%num_bc_nod, vsp_bc%ibc_id, node%numnod, f_l%ff)
!
      end subroutine set_vector_ffs_special_bc
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine delete_vector_on_bc(vector_bc, i_field, nod_fld)
!
      use t_phys_data
      use set_fixed_boundaries
!
      integer(kind = kint), intent(in) :: i_field
      type(vect_fixed_nod_bc_type), intent(in) :: vector_bc
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
      subroutine delete_radial_vector_on_bc                             &
     &         (node, vr0_bc, i_field, nod_fld)
!
      use t_geometry_data
      use t_phys_data
      use set_nodal_bc_4_velo
!
      integer(kind = kint), intent(in) :: i_field
      type(node_data), intent(in) :: node
      type(scaler_fixed_nod_bc_type), intent(in) :: vr0_bc
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (vr0_bc%num_bc_nod .le. 0) return
      call del_radial_velocity(node%numnod, node%xx, node%a_r,          &
     &    vr0_bc%num_bc_nod, vr0_bc%ibc_id,                             &
     &    nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine delete_radial_vector_on_bc
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_scalar(scalar_bc, i_field, nod_fld)
!
      use t_phys_data
      use set_fixed_boundaries
!
      type(scaler_fixed_nod_bc_type), intent(in) :: scalar_bc
!
      integer(kind = kint), intent(in) :: i_field
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (scalar_bc%num_bc_nod .le. 0) return
      call set_fixed_bc_scalar_phys                                     &
     &    (scalar_bc%num_bc_nod, scalar_bc%ibc_id, scalar_bc%bc_apt,    &
     &     nod_fld%n_point, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine set_boundary_scalar
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_vect(vector_bc, i_field, nod_fld)
!
      use t_phys_data
      use set_fixed_boundaries
!
      integer(kind = kint), intent(in) :: i_field
      type(vect_fixed_nod_bc_type), intent(in) :: vector_bc
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (vector_bc%nmax_bc .le. 0) return
      call set_fixed_bc_vect_phys                                       &
     &   (vector_bc%nmax_bc, vector_bc%num_bc_nod,                      &
     &    vector_bc%ibc_id, vector_bc%bc_apt,                           &
     &    nod_fld%n_point, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine set_boundary_vect
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_rot_vect(node, rot_bc, i_field, nod_fld)
!
      use t_geometry_data
      use t_phys_data
      use set_nodal_bc_4_velo
!
      integer(kind = kint), intent(in) :: i_field
      type(node_data), intent(in) :: node
      type(scaler_rotaion_nod_bc_type), intent(in) :: rot_bc
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (rot_bc%num_bc_nod .le. 0) return
      call set_rotation_boundary(node%numnod, node%xx,                  &
     &    rot_bc%num_bc_nod, rot_bc%ibc_id, rot_bc%bc_rot_apt,          &
     &    nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine set_boundary_rot_vect
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_specific_vect                             &
     &         (time, node, vsp_bc, i_field, nod_fld)
!
      use t_geometry_data
      use t_phys_data
      use set_nodal_bc_4_velo
!
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: i_field
      type(node_data), intent(in) :: node
      type(scaler_fixed_nod_bc_type), intent(in) :: vsp_bc
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (vsp_bc%num_bc_nod .le. 0) return
      call set_specific_boundary_velo(time, node%numnod, node%xx,       &
     &    vsp_bc%num_bc_nod, vsp_bc%ibc_id, vsp_bc%bc_apt,              &
     &    nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine set_boundary_specific_vect
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine delete_field_by_fixed_s_bc                             &
     &         (scalar_bc, i_field, nod_fld)
!
      use t_phys_data
!
      use set_fixed_boundaries
!
      integer(kind = kint), intent(in) :: i_field
      type(scaler_fixed_nod_bc_type), intent(in) :: scalar_bc
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (scalar_bc%num_bc_nod .le. 0) return
      call del_scalar_phys_on_bc                                        &
     &    (scalar_bc%num_bc_nod, scalar_bc%ibc_id,                      &
     &     nod_fld%n_point, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine delete_field_by_fixed_s_bc
!
!  ---------------------------------------------------------------------
!
      subroutine delete_vector_by_fixed_t_bc                            &
     &         (scalar_bc, i_field, nod_fld)
!
      use t_phys_data
!
      use set_fixed_boundaries
!
      integer(kind = kint), intent(in) :: i_field
      type(scaler_fixed_nod_bc_type), intent(in) :: scalar_bc
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (scalar_bc%num_bc_nod .le. 0) return
      call del_vector_phys_on_1bc                                       &
     &   (scalar_bc%num_bc_nod, scalar_bc%ibc_id,                       &
     &    nod_fld%n_point, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine delete_vector_by_fixed_t_bc
!
!  ---------------------------------------------------------------------
!
      subroutine delete_vector_by_rot_v_bc(rot_bc, i_field, nod_fld)
!
      use t_phys_data
!
      use set_fixed_boundaries
!
      integer(kind = kint), intent(in) :: i_field
      type(scaler_rotaion_nod_bc_type), intent(in) :: rot_bc
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (rot_bc%num_bc_nod .le. 0) return
      call del_vector_phys_on_1bc(rot_bc%num_bc_nod, rot_bc%ibc_id,     &
     &    nod_fld%n_point, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine delete_vector_by_rot_v_bc
!
!  ---------------------------------------------------------------------
!
      end module set_boundary_scalars
