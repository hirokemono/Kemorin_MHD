!set_bc_scalars.f90
!      module set_bc_scalars
!
!      Written by H. Matsui nad H. Okuda
!      Modified by H. Matsui on Oct., 2005
!
!!      subroutine set_bc_velo_4_sphere_id(node, nod_grp, bc_list,      &
!!     &          nod_bc_vfree, nod_bc_vr0, nod_bc_vsp)
!!      subroutine set_bc_fixed_temp_id(node, nod_grp, bc_list,         &
!!     &          nod_bc_t, sgs_bc_t)
!!      subroutine set_bc_fixed_m_potential_id(node, nod_grp, bc_list,  &
!!     &          nod_bc_f, sgs_bc_f)
!
      module set_bc_scalars
!
      use m_precision
!
      use m_constants
      use m_phys_labels
      use m_boundary_condition_IDs
      use m_bc_data_list
      use t_geometry_data
      use t_group_data
      use t_nodal_bc_data
      use set_nod_bc_scalar_id
!
      implicit none
!
      private :: set_bc_fixed_scalar_id, set_bc_sgs_scalar_id
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_velo_4_sphere_id(node, nod_grp, bc_list,        &
     &          nod_bc_vfree, nod_bc_vr0, nod_bc_vsp)
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
!
      type(scaler_fixed_nod_bc_type), intent(inout) :: nod_bc_vfree
      type(scaler_fixed_nod_bc_type), intent(inout) :: nod_bc_vr0
      type(scaler_fixed_nod_bc_type), intent(inout) :: nod_bc_vsp
!
      integer (kind = kint) :: l_fr, l_r0, l_vsp
!
!
      l_fr = 0
      call set_bc_scalar_id                                             &
     &   (node, nod_grp, bc_list%num_bc, bc_list%bc_name,               &
     &    bc_list%ibc_type, bc_list%bc_magnitude,                       &
     &    nod_bc_vfree%ibc, nod_bc_vfree%ibc2, nod_bc_vfree%num_bc_nod, &
     &    nod_bc_vfree%ibc_id, nod_bc_vfree%bc_apt,                     &
     &    iflag_free_sph, l_fr)
!
      l_r0 = 0
      call set_bc_scalar_id                                             &
     &   (node, nod_grp, bc_list%num_bc, bc_list%bc_name,               &
     &    bc_list%ibc_type, bc_list%bc_magnitude,                       &
     &    nod_bc_vr0%ibc, nod_bc_vr0%ibc2, nod_bc_vr0%num_bc_nod,       &
     &    nod_bc_vr0%ibc_id, nod_bc_vr0%bc_apt, iflag_no_vr, l_r0)
!
      l_vsp = 0
      call set_bc_scalar_id                                             &
     &   (node, nod_grp, bc_list%num_bc, bc_list%bc_name,               &
     &    bc_list%ibc_type, bc_list%bc_magnitude,                       &
     &    nod_bc_vsp%ibc, nod_bc_vsp%ibc2, nod_bc_vsp%num_bc_nod,       &
     &    nod_bc_vsp%ibc_id, nod_bc_vsp%bc_apt,                         &
     &    iflag_bc_special, l_vsp)
!
      end subroutine set_bc_velo_4_sphere_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_temp_id(node, nod_grp, bc_list,           &
     &          nod_bc_t, sgs_bc_t)
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
!
      type(scaler_fixed_nod_bc_type), intent(inout) :: nod_bc_t
      type(scaler_fixed_nod_bc_type), intent(inout) :: sgs_bc_t
!
      integer (kind = kint) :: ii, i0
!
!
      ii = 0
      call set_bc_fixed_scalar_id(node, nod_grp, bc_list, nod_bc_t, ii)
!
      i0 = 0
      call set_bc_sgs_scalar_id(node, nod_grp, bc_list, sgs_bc_t, i0)
!
      end subroutine set_bc_fixed_temp_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_m_potential_id(node, nod_grp, bc_list,    &
     &          nod_bc_f, sgs_bc_f)
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
!
      type(scaler_fixed_nod_bc_type), intent(inout) :: nod_bc_f
      type(scaler_fixed_nod_bc_type), intent(inout) :: sgs_bc_f
!
      integer (kind = kint) :: ii, i0
!
!
      ii = 0
      call set_bc_fixed_scalar_id(node, nod_grp, bc_list, nod_bc_f, ii)
!
      call set_bc_sph_magne_p_id(node, nod_grp, bc_list%num_bc,         &
     &    bc_list%bc_name, bc_list%ibc_type, ii, nod_bc_f)
!
      i0 = 0
      call set_bc_sgs_scalar_id(node, nod_grp, bc_list, sgs_bc_f, i0)
!
      end subroutine set_bc_fixed_m_potential_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_scalar_id                                 &
     &         (node, nod_grp, bc_list, scalar_bc, ii)
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
!
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
      integer (kind = kint), intent(inout) :: ii
!
!
      call set_fixed_bc_scalar_id                                       &
     &   (node, nod_grp, bc_list%num_bc, bc_list%bc_name,               &
     &    bc_list%ibc_type,  bc_list%bc_magnitude, scalar_bc%ibc,       &
     &    scalar_bc%ibc2, scalar_bc%num_bc_nod, scalar_bc%ibc_id,       &
     &    scalar_bc%bc_apt, scalar_bc%scalar_bc_name, ii)
!
      end subroutine set_bc_fixed_scalar_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_sgs_scalar_id                                   &
     &         (node, nod_grp, bc_list, scalar_bc, ii)
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
!
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
      integer (kind = kint), intent(inout) :: ii
!
!
      call set_bc_scalar_id(node, nod_grp, bc_list%num_bc,              &
     &    bc_list%bc_name, bc_list%ibc_type, bc_list%bc_magnitude,      &
     &    scalar_bc%ibc, scalar_bc%ibc2, scalar_bc%num_bc_nod,          &
     &    scalar_bc%ibc_id, scalar_bc%bc_apt, iflag_bc_sgs_s, ii)
!
      end subroutine set_bc_sgs_scalar_id
!
!  ---------------------------------------------------------------------
!
      end module set_bc_scalars
