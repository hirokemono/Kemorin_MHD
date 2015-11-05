!set_ele_nod_bc_potential.f90
!      module set_ele_nod_bc_potential
!
!      Written by H. Matsui nad H. Okuda
!      Modified by H. Matsui on Oct., 2005
!
!      subroutine set_ele_nodal_bc_4_press
!      subroutine set_ele_nodal_bc_4_press_sgs
!
!      subroutine set_ele_nodal_bc_4_magne_p
!      subroutine set_ele_nodal_bc_4_mag_p_sgs
!      subroutine set_ele_nodal_bc_4_mag_p_ins
!      subroutine set_ele_nodal_bc_4_mag_p_cd
!
      module set_ele_nod_bc_potential
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use set_bc_element
      use count_bc_element
      use ordering_ele_4_fix_bd
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_press
!
      use m_bc_data_velo
!
!
      call count_bc_element_fl(nod_bc1_p%num_idx_ibc, nod_bc1_p%ibc)
      call count_bc_element_fl(nod_bc1_p%num_idx_ibc2, nod_bc1_p%ibc2)
!
      call alloc_nod_bc_scalar_ele_type                                 &
     &   (np_smp, num_t_linear, nod_bc1_p)
!
      call set_bc_element_fl(nod_bc1_p%num_idx_ibc, nod_bc1_p%ibc,      &
     &    nod_bc1_p%ele_bc_id, nod_bc1_p%nod_bc_id, num_t_linear)
      call set_bc_element_fl(nod_bc1_p%num_idx_ibc2, nod_bc1_p%ibc2,    &
     &    nod_bc1_p%ele_bc2_id, nod_bc1_p%nod_bc2_id, num_t_linear)
!
      call reordering_ele_4_fix_bd(np_smp, nod_bc1_p%num_idx_ibc,       &
     &    nod_bc1_p%num_idx_ibc, nod_bc1_p%ele_bc_id,    &
     &    nod_bc1_p%nod_bc_id, nod_bc1_p%ibc_end, nod_bc1_p%ibc_shape,  &
     &    nod_bc1_p%ibc_stack, nod_bc1_p%ibc_stack_smp, num_t_linear)
!
      call dealloc_scalar_ibc_type(nod_bc1_p)
!
      end subroutine set_ele_nodal_bc_4_press
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_press_sgs
!
      use m_bc_data_velo
!
!
      call count_bc_element_fl(sgs_bc1_p%num_idx_ibc, sgs_bc1_p%ibc)
      call count_bc_element_fl(sgs_bc1_p%num_idx_ibc2, sgs_bc1_p%ibc2)
!
      call alloc_nod_bc_scalar_ele_type                                 &
     &   (np_smp, num_t_linear, sgs_bc1_p)
!
      call set_bc_element_fl(sgs_bc1_p%num_idx_ibc, sgs_bc1_p%ibc,      &
     &    sgs_bc1_p%ele_bc_id, sgs_bc1_p%nod_bc_id, num_t_linear)
      call set_bc_element_fl(sgs_bc1_p%num_idx_ibc2, sgs_bc1_p%ibc2,    &
     &    sgs_bc1_p%ele_bc2_id, sgs_bc1_p%nod_bc2_id, num_t_linear)
!
      call reordering_ele_4_fix_bd(np_smp, sgs_bc1_p%num_idx_ibc,       &
     &    sgs_bc1_p%num_idx_ibc, sgs_bc1_p%ele_bc_id,                   &
     &    sgs_bc1_p%nod_bc_id,  sgs_bc1_p%ibc_end, sgs_bc1_p%ibc_shape, &
     &    sgs_bc1_p%ibc_stack, sgs_bc1_p%ibc_stack_smp, num_t_linear)
!
      call dealloc_scalar_ibc_type(sgs_bc1_p)
!
      end subroutine set_ele_nodal_bc_4_press_sgs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_magne_p
!
      use m_bc_data_magne
!
!
      call count_bc_element_whole     &
     &   (nod_bc1_f%num_idx_ibc, nod_bc1_f%ibc)
      call count_bc_element_whole     &
     &   (nod_bc1_f%num_idx_ibc2, nod_bc1_f%ibc2)
!
      call alloc_nod_bc_scalar_ele_type                                 &
     &   (np_smp, num_t_linear, nod_bc1_f)
!
      call set_bc_element_whole(nod_bc1_f%num_idx_ibc, nod_bc1_f%ibc,   &
     &    nod_bc1_f%ele_bc_id, nod_bc1_f%nod_bc_id, num_t_linear)
      call set_bc_element_whole(nod_bc1_f%num_idx_ibc2, nod_bc1_f%ibc2, &
     &    nod_bc1_f%ele_bc2_id, nod_bc1_f%nod_bc2_id, num_t_linear)
!
      call reordering_ele_4_fix_bd(np_smp, nod_bc1_f%num_idx_ibc,       &
     &     nod_bc1_f%num_idx_ibc, nod_bc1_f%ele_bc_id,                  &
     &     nod_bc1_f%nod_bc_id, nod_bc1_f%ibc_end,                      &
     &     nod_bc1_f%ibc_shape, nod_bc1_f%ibc_stack,                    &
     &     nod_bc1_f%ibc_stack_smp, num_t_linear)
!
      end subroutine set_ele_nodal_bc_4_magne_p
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_mag_p_sgs
!
      use m_bc_data_magne
!
!
      call count_bc_element_whole(sgs_bc1_f%num_idx_ibc, sgs_bc1_f%ibc)
      call count_bc_element_whole       &
     &   (sgs_bc1_f%num_idx_ibc2, sgs_bc1_f%ibc2)
!
      call alloc_nod_bc_scalar_ele_type                                 &
     &   (np_smp, num_t_linear, sgs_bc1_f)
!
      call set_bc_element_whole                      &
     &   (sgs_bc1_f%num_idx_ibc, sgs_bc1_f%ibc,       &
     &    sgs_bc1_f%ele_bc_id, sgs_bc1_f%nod_bc_id, num_t_linear)
      call set_bc_element_whole                     &
     &   (sgs_bc1_f%num_idx_ibc2, sgs_bc1_f%ibc2,  &
     &    sgs_bc1_f%ele_bc2_id, sgs_bc1_f%nod_bc2_id, num_t_linear)
!
      call reordering_ele_4_fix_bd(np_smp, sgs_bc1_f%num_idx_ibc,       &
     &    sgs_bc1_f%num_idx_ibc, sgs_bc1_f%ele_bc_id,                 &
     &    sgs_bc1_f%nod_bc_id, sgs_bc1_f%ibc_end, sgs_bc1_f%ibc_shape,  &
     &    sgs_bc1_f%ibc_stack, sgs_bc1_f%ibc_stack_smp, num_t_linear)
!
      call dealloc_scalar_ibc_type(sgs_bc1_f)
!
      end subroutine set_ele_nodal_bc_4_mag_p_sgs
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_mag_p_ins
!
      use m_bc_data_magne
      use m_bc_data_mag_p_ins
!
!
      call count_bc_element_ins(num_index_ibc_mag_pi, nod_bc1_f%ibc)
      call count_bc_element_ins(num_index_ibc2_mag_pi, nod_bc1_f%ibc2)
!
      call allocate_bc_mag_p_ins_4_ele
!
      call set_bc_element_ins(num_index_ibc_mag_pi, nod_bc1_f%ibc,      &
     &    ele_bc_mag_pi_id, nod_bc_mag_pi_id, num_t_linear)
      call set_bc_element_ins(num_index_ibc2_mag_pi, nod_bc1_f%ibc2,    &
     &    ele_bc2_mag_pi_id, nod_bc2_mag_pi_id, num_t_linear)
!
      call reordering_ele_4_fix_bd(np_smp, num_index_ibc_mag_pi,        &
     &      num_index_ibc_mag_pi, ele_bc_mag_pi_id, nod_bc_mag_pi_id,   &
     &      ibc_mag_pi_end, ibc_mag_pi_shape, ibc_mag_pi_stack,         &
     &      ibc_mag_pi_stack_smp, num_t_linear)
!
      end subroutine set_ele_nodal_bc_4_mag_p_ins
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_mag_p_cd
!
      use m_bc_data_magne
      use m_bc_data_mag_p_cd
!
!
      call count_bc_element_cd(num_index_ibc_mag_pc, nod_bc1_f%ibc)
      call count_bc_element_cd(num_index_ibc2_mag_pc, nod_bc1_f%ibc2)
!
      call allocate_bc_mag_p_cd_4_ele
!
      call set_bc_element_cd(num_index_ibc_mag_pc, nod_bc1_f%ibc,       &
     &    ele_bc_mag_pc_id, nod_bc_mag_pc_id, num_t_linear)
      call set_bc_element_cd(num_index_ibc2_mag_pc, nod_bc1_f%ibc2,     &
     &    ele_bc2_mag_pc_id, nod_bc2_mag_pc_id, num_t_linear)
!
      call reordering_ele_4_fix_bd(np_smp, num_index_ibc_mag_pc,        &
     &      num_index_ibc_mag_pc, ele_bc_mag_pc_id, nod_bc_mag_pc_id,   &
     &      ibc_mag_pc_end, ibc_mag_pc_shape, ibc_mag_pc_stack,         &
     &      ibc_mag_pc_stack_smp, num_t_linear)
!
      call dealloc_scalar_ibc_type(nod_bc1_f)
!
      end subroutine set_ele_nodal_bc_4_mag_p_cd
!
!  ---------------------------------------------------------------------
!
      end module set_ele_nod_bc_potential
