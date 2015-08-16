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
      use m_bc_data_press
!
!
      call count_bc_element_fl(num_index_ibc_press, ibc_press)
      call count_bc_element_fl(num_index_ibc2_press, ibc2_press)
!
      call allocate_bc_press_4_element
!
      call set_bc_element_fl(num_index_ibc_press, ibc_press,            &
     &    ele_bc_p_id, nod_bc_p_id, num_t_linear)
      call set_bc_element_fl(num_index_ibc2_press, ibc2_press,          &
     &    ele_bc2_p_id, nod_bc2_p_id, num_t_linear)
!
      call reordering_ele_4_fix_bd(np_smp, num_index_ibc_press,         &
     &    num_index_ibc_press, ele_bc_p_id, nod_bc_p_id, ibc_p_end,     &
     &    ibc_p_shape, ibc_p_stack, ibc_p_stack_smp, num_t_linear)
!
      call deallocate_ibc_4_press
!
      end subroutine set_ele_nodal_bc_4_press
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_press_sgs
!
      use m_bc_press_sgs
!
!
      call count_bc_element_fl(num_index_ibc_p_sgs, ibc_press_sgs)
      call count_bc_element_fl(num_index_ibc2_p_sgs, ibc2_press_sgs)
!
      call allocate_bc_p_sgs_4_ele
!
      call set_bc_element_fl(num_index_ibc_p_sgs, ibc_press_sgs,        &
     &    ele_bc_ps_id, nod_bc_ps_id, num_t_linear)
      call set_bc_element_fl(num_index_ibc2_p_sgs, ibc2_press_sgs,      &
     &    ele_bc2_ps_id, nod_bc2_ps_id, num_t_linear)
!
      call reordering_ele_4_fix_bd(np_smp, num_index_ibc_p_sgs,         &
     &    num_index_ibc_p_sgs, ele_bc_ps_id, nod_bc_ps_id, ibc_ps_end,  &
     &    ibc_ps_shape, ibc_ps_stack, ibc_ps_stack_smp, num_t_linear)
!
      call deallocate_ibc_4_p_sgs
!
      end subroutine set_ele_nodal_bc_4_press_sgs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_magne_p
!
      use m_bc_data_magne_p
!
!
      call count_bc_element_whole(num_index_ibc_mag_p, ibc_mag_p)
      call count_bc_element_whole(num_index_ibc2_mag_p, ibc2_mag_p)
!
      call allocate_bc_magne_p_4_element
!
      call set_bc_element_whole(num_index_ibc_mag_p, ibc_mag_p,         &
     & ele_bc_mag_p_id, nod_bc_mag_p_id, num_t_linear)
      call set_bc_element_whole(num_index_ibc2_mag_p, ibc2_mag_p,       &
     & ele_bc2_mag_p_id, nod_bc2_mag_p_id, num_t_linear)
!
      call reordering_ele_4_fix_bd(np_smp, num_index_ibc_mag_p,         &
     &     num_index_ibc_mag_p, ele_bc_mag_p_id, nod_bc_mag_p_id,       &
     &     ibc_mag_p_end, ibc_mag_p_shape, ibc_mag_p_stack,             &
     &     ibc_mag_p_stack_smp, num_t_linear)
!
      end subroutine set_ele_nodal_bc_4_magne_p
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_mag_p_sgs
!
      use m_bc_mag_p_sgs
!
!
      call count_bc_element_whole(num_index_ibc_mp_sgs, ibc_mp_sgs)
      call count_bc_element_whole(num_index_ibc2_mp_sgs, ibc2_mp_sgs)
!
      call allocate_bc_magp_sgs_4_ele
!
      call set_bc_element_whole(num_index_ibc_mp_sgs, ibc_mp_sgs,       &
     &    ele_bc_mp_sgs_id, nod_bc_mp_sgs_id, num_t_linear)
      call set_bc_element_whole(num_index_ibc2_mp_sgs, ibc2_mp_sgs,     &
     &    ele_bc2_mp_sgs_id, nod_bc2_mp_sgs_id, num_t_linear)
!
      call reordering_ele_4_fix_bd(np_smp, num_index_ibc_mp_sgs,        &
     &      num_index_ibc_mp_sgs, ele_bc_mp_sgs_id, nod_bc_mp_sgs_id,   &
     &      ibc_mp_sgs_end, ibc_mp_sgs_shape, ibc_mp_sgs_stack,         &
     &      ibc_mp_sgs_stack_smp, num_t_linear)
!
      call deallocate_ibc_4_magp_sgs
!
      end subroutine set_ele_nodal_bc_4_mag_p_sgs
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_mag_p_ins
!
      use m_bc_data_magne_p
      use m_bc_data_mag_p_ins
!
!
      call count_bc_element_ins(num_index_ibc_mag_pi, ibc_mag_p)
      call count_bc_element_ins(num_index_ibc2_mag_pi, ibc2_mag_p)
!
      call allocate_bc_mag_p_ins_4_ele
!
      call set_bc_element_ins(num_index_ibc_mag_pi, ibc_mag_p,          &
     &    ele_bc_mag_pi_id, nod_bc_mag_pi_id, num_t_linear)
      call set_bc_element_ins(num_index_ibc2_mag_pi, ibc2_mag_p,        &
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
      use m_bc_data_magne_p
      use m_bc_data_mag_p_cd
!
!
      call count_bc_element_cd(num_index_ibc_mag_pc, ibc_mag_p)
      call count_bc_element_cd(num_index_ibc2_mag_pc, ibc2_mag_p)
!
      call allocate_bc_mag_p_cd_4_ele
!
      call set_bc_element_cd(num_index_ibc_mag_pc, ibc_mag_p,           &
     &    ele_bc_mag_pc_id, nod_bc_mag_pc_id, num_t_linear)
      call set_bc_element_cd(num_index_ibc2_mag_pc, ibc2_mag_p,         &
     &    ele_bc2_mag_pc_id, nod_bc2_mag_pc_id, num_t_linear)
!
      call reordering_ele_4_fix_bd(np_smp, num_index_ibc_mag_pc,        &
     &      num_index_ibc_mag_pc, ele_bc_mag_pc_id, nod_bc_mag_pc_id,   &
     &      ibc_mag_pc_end, ibc_mag_pc_shape, ibc_mag_pc_stack,         &
     &      ibc_mag_pc_stack_smp, num_t_linear)
!
      call deallocate_ibc_4_magne_p
!
      end subroutine set_ele_nodal_bc_4_mag_p_cd
!
!  ---------------------------------------------------------------------
!
      end module set_ele_nod_bc_potential
