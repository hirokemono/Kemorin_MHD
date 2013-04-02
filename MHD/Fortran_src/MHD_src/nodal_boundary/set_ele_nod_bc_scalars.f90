!
!      module set_ele_nod_bc_scalars
!
!      Written by H. Matsui nad H. Okuda
!      Modified by H. Matsui on Oct., 2005
!
!      subroutine set_ele_nodal_bc_4_rotate
!      subroutine set_ele_nodal_bc_4_vfree
!      subroutine set_ele_nodal_bc_4_vr0
!      subroutine set_ele_nodal_bc_4_velo_sph
!
!      subroutine set_ele_nodal_bc_4_temp
!      subroutine set_ele_nodal_bc_4_temp_sgs
!
!      subroutine set_ele_nodal_bc_4_composition
!
      module set_ele_nod_bc_scalars
!
      use m_precision
!
      use count_bc_element
      use set_bc_element
      use set_ele_4_nodal_bc
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_rotate
!
      use m_bc_data_rotate
!
!
      call count_bc_element_fl(num_index_ibc_vrot, ibc_velo_rot)
      call count_bc_element_fl(num_index_ibc2_vrot, ibc2_velo_rot)
!
      call allocate_bc_rot_4_ele
!
      call set_ele_4_scalar_nodal_bc_fl(ibc_velo_rot, ibc2_velo_rot,    &
     &    num_index_ibc_vrot, ele_bc_vrot_id, nod_bc_vrot_id,           &
     &    num_index_ibc2_vrot, ele_bc2_vrot_id, nod_bc2_vrot_id,        &
     &    ibc_vrot_end, ibc_vrot_shape, ibc_vrot_stack,                 &
     &    ibc_vrot_stack_smp)
!
      call deallocate_ibc_4_rot
!
      end subroutine set_ele_nodal_bc_4_rotate
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_vfree
!
      use m_bc_data_vfree
!
!
      call count_bc_element_fl(num_index_ibc_vfr, ibc_velo_fr)
      call count_bc_element_fl(num_index_ibc2_vfr, ibc2_velo_fr)
!
      call allocate_bc_vfr_4_ele
!
      call set_ele_4_scalar_nodal_bc_fl(ibc_velo_fr, ibc2_velo_fr,      &
     &    num_index_ibc_vfr, ele_bc_vfr_id, nod_bc_vfr_id,              &
     &    num_index_ibc2_vfr, ele_bc2_vfr_id, nod_bc2_vfr_id,           &
     &    ibc_vfr_end, ibc_vfr_shape, ibc_vfr_stack, ibc_vfr_stack_smp)
!
      call deallocate_ibc_4_vfr
!
      end subroutine set_ele_nodal_bc_4_vfree
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_vr0
!
      use m_bc_data_vr0
!
!
      call count_bc_element_fl(num_index_ibc_vr0, ibc_velo_r0)
      call count_bc_element_fl(num_index_ibc2_vr0, ibc2_velo_r0)
!
      call allocate_bc_vr0_4_ele
!
      call set_ele_4_scalar_nodal_bc_fl(ibc_velo_r0, ibc2_velo_r0,      &
     &    num_index_ibc_vr0, ele_bc_vr0_id, nod_bc_vr0_id,              &
     &    num_index_ibc2_vr0, ele_bc2_vr0_id, nod_bc2_vr0_id,           &
     &    ibc_vr0_end, ibc_vr0_shape, ibc_vr0_stack, ibc_vr0_stack_smp)
!
      call deallocate_ibc_4_vr0
!
      end subroutine set_ele_nodal_bc_4_vr0
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_velo_sph
!
      use m_bc_data_vsp
!
!
      call count_bc_element_fl(num_index_ibc_vsp, ibc_velo_vsp)
      call count_bc_element_fl(num_index_ibc2_vsp, ibc2_velo_vsp)
!
      call allocate_bc_vsp_4_ele
!
      call set_ele_4_scalar_nodal_bc_fl(ibc_velo_vsp, ibc2_velo_vsp,    &
     &    num_index_ibc_vsp, ele_bc_vsp_id, nod_bc_vsp_id,              &
     &    num_index_ibc2_vsp, ele_bc2_vsp_id, nod_bc2_vsp_id,           &
     &    ibc_vsp_end, ibc_vsp_shape, ibc_vsp_stack, ibc_vsp_stack_smp)
!
      call deallocate_ibc_4_vsp
!
      end subroutine set_ele_nodal_bc_4_velo_sph
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_temp
!
      use m_bc_data_ene
!
!
      call count_bc_element_fl(num_index_ibc_temp, ibc_temp)
      call count_bc_element_fl(num_index_ibc2_temp, ibc2_temp)
!
      call allocate_bc_temp_4_element
!
      call set_ele_4_scalar_nodal_bc_fl(ibc_temp, ibc2_temp,            &
     &    num_index_ibc_temp, ele_bc_temp_id, nod_bc_temp_id,           &
     &    num_index_ibc2_temp, ele_bc2_temp_id, nod_bc2_temp_id,        &
     &    ibc_temp_end, ibc_temp_shape, ibc_temp_stack,                 &
     &    ibc_temp_stack_smp)
!
      call deallocate_ibc_4_temp
!
      end subroutine set_ele_nodal_bc_4_temp
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_temp_sgs
!
      use m_bc_temp_sgs
!
!
      call count_bc_element_fl(num_index_ibc_t_sgs, ibc_t_sgs)
      call count_bc_element_fl(num_index_ibc2_t_sgs, ibc2_t_sgs)
!
      call allocate_bc_t_sgs_4_ele
!
      call set_ele_4_scalar_nodal_bc_fl(ibc_t_sgs, ibc2_t_sgs,          &
     &    num_index_ibc_t_sgs, ele_bc_t_sgs_id, nod_bc_t_sgs_id,        &
     &    num_index_ibc2_t_sgs, ele_bc2_t_sgs_id, nod_bc2_t_sgs_id,     &
     &    ibc_t_sgs_end, ibc_t_sgs_shape, ibc_t_sgs_stack,              &
     &    ibc_t_sgs_stack_smp)
!
      call deallocate_ibc_4_t_sgs
!
      end subroutine set_ele_nodal_bc_4_temp_sgs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_composition
!
      use m_bc_data_composition
!
      call count_bc_element_fl(num_index_ibc_compsition, ibc_composit)
      call count_bc_element_fl(num_index_ibc2_compsition,               &
     &                         ibc2_composit)
!
      call allocate_bc_composit_4_element
!
      call set_ele_4_scalar_nodal_bc_fl(ibc_composit, ibc2_composit,    &
     &    num_index_ibc_compsition, ele_bc_composit_id,                 &
     &    nod_bc_composit_id, num_index_ibc2_compsition,                &
     &    ele_bc2_composit_id, nod_bc2_composit_id,                     &
     &    ibc_composition_end, ibc_composit_shape,                      &
     &    ibc_composit_stack, ibc_composit_stack_smp)
!
      call deallocate_ibc_4_composit
!
      end subroutine set_ele_nodal_bc_4_composition
!
!  ---------------------------------------------------------------------
!
      end module set_ele_nod_bc_scalars
