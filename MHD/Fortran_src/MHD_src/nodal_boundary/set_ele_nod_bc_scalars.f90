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
      use m_geometry_data
      use m_geometry_data_MHD
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
      use m_bc_data_velo
!
!
      call count_bc_element_layer                                       &
     &   (node1, ele1, iele_fl_start, iele_fl_end,                      &
     &    nod_bc1_rot%num_idx_ibc, nod_bc1_rot%ibc)
      call count_bc_element_layer                                       &
     &   (node1, ele1, iele_fl_start, iele_fl_end,                      &
     &    nod_bc1_rot%num_idx_ibc2, nod_bc1_rot%ibc2)
!
      call alloc_nod_bc_rotate_ele_type                                 &
      &  (np_smp, ele1%nnod_4_ele, nod_bc1_rot)
!
      call set_ele_4_scalar_nodal_bc_fl(node1, ele1,                    &
     &    nod_bc1_rot%ibc, nod_bc1_rot%ibc2,   &
     &    nod_bc1_rot%num_idx_ibc, nod_bc1_rot%ele_bc_id,  &
     &    nod_bc1_rot%nod_bc_id, nod_bc1_rot%num_idx_ibc2,      &
     &    nod_bc1_rot%ele_bc2_id, nod_bc1_rot%nod_bc2_id,        &
     &    nod_bc1_rot%ibc_end, nod_bc1_rot%ibc_shape,   &
     &    nod_bc1_rot%ibc_stack, nod_bc1_rot%ibc_stack_smp)
!
      call dealloc_rotate_ibc_type(nod_bc1_rot)
!
      end subroutine set_ele_nodal_bc_4_rotate
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_vfree
!
      use m_bc_data_velo
!
!
      call count_bc_element_layer                                       &
     &   (node1, ele1, iele_fl_start, iele_fl_end,                      &
     &    nod_bc1_vfree%num_idx_ibc, nod_bc1_vfree%ibc)
      call count_bc_element_layer                                       &
     &   (node1, ele1, iele_fl_start, iele_fl_end,                      &
     &    nod_bc1_vfree%num_idx_ibc2, nod_bc1_vfree%ibc2)
!
      call alloc_nod_bc_scalar_ele_type                    &
      &  (np_smp, ele1%nnod_4_ele, nod_bc1_vfree)
!
      call set_ele_4_scalar_nodal_bc_fl(node1, ele1,     &
     &    nod_bc1_vfree%ibc, nod_bc1_vfree%ibc2,     &
     &    nod_bc1_vfree%num_idx_ibc, nod_bc1_vfree%ele_bc_id,     &
     &    nod_bc1_vfree%nod_bc_id, nod_bc1_vfree%num_idx_ibc2,      &
     &    nod_bc1_vfree%ele_bc2_id, nod_bc1_vfree%nod_bc2_id,           &
     &    nod_bc1_vfree%ibc_end, nod_bc1_vfree%ibc_shape,       &
     &    nod_bc1_vfree%ibc_stack, nod_bc1_vfree%ibc_stack_smp)
!
      call dealloc_scalar_ibc_type(nod_bc1_vfree)
!
      end subroutine set_ele_nodal_bc_4_vfree
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_vr0
!
      use m_bc_data_velo
!
!
      call count_bc_element_layer                                       &
     &   (node1, ele1, iele_fl_start, iele_fl_end,                      &
     &   nod_bc1_vr0%num_idx_ibc, nod_bc1_vr0%ibc)
      call count_bc_element_layer                                       &
     &   (node1, ele1, iele_fl_start, iele_fl_end,                      &
     &    nod_bc1_vr0%num_idx_ibc2, nod_bc1_vr0%ibc2)
!
      call alloc_nod_bc_scalar_ele_type                                 &
      &  (np_smp, ele1%nnod_4_ele, nod_bc1_vr0)
!
      call set_ele_4_scalar_nodal_bc_fl(node1, ele1,                    &
     &    nod_bc1_vr0%ibc, nod_bc1_vr0%ibc2,                            &
     &    nod_bc1_vr0%num_idx_ibc, nod_bc1_vr0%ele_bc_id,       &
     &    nod_bc1_vr0%nod_bc_id, nod_bc1_vr0%num_idx_ibc2,        &
     &    nod_bc1_vr0%ele_bc2_id, nod_bc1_vr0%nod_bc2_id,           &
     &    nod_bc1_vr0%ibc_end, nod_bc1_vr0%ibc_shape,            &
     &    nod_bc1_vr0%ibc_stack, nod_bc1_vr0%ibc_stack_smp)
!
      call dealloc_scalar_ibc_type(nod_bc1_vr0)
!
      end subroutine set_ele_nodal_bc_4_vr0
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_velo_sph
!
      use m_bc_data_velo
!
!
      call count_bc_element_layer                                       &
     &   (node1, ele1, iele_fl_start, iele_fl_end,                      &
     &    nod_bc1_vsp%num_idx_ibc, nod_bc1_vsp%ibc)
      call count_bc_element_layer                                       &
     &   (node1, ele1, iele_fl_start, iele_fl_end,                      &
     &    nod_bc1_vsp%num_idx_ibc2, nod_bc1_vsp%ibc2)
!
      call alloc_nod_bc_scalar_ele_type                                 &
      &  (np_smp, ele1%nnod_4_ele, nod_bc1_vsp)
!
      call set_ele_4_scalar_nodal_bc_fl(node1, ele1,            &
     &    nod_bc1_vsp%ibc, nod_bc1_vsp%ibc2,   &
     &    nod_bc1_vsp%num_idx_ibc, nod_bc1_vsp%ele_bc_id,    &
     &    nod_bc1_vsp%nod_bc_id, nod_bc1_vsp%num_idx_ibc2,        &
     &    nod_bc1_vsp%ele_bc2_id, nod_bc1_vsp%nod_bc2_id,           &
     &    nod_bc1_vsp%ibc_end, nod_bc1_vsp%ibc_shape,          &
     &    nod_bc1_vsp%ibc_stack, nod_bc1_vsp%ibc_stack_smp)
!
      call dealloc_scalar_ibc_type(nod_bc1_vsp)
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
      call count_bc_element_layer                                       &
     &   (node1, ele1, iele_fl_start, iele_fl_end,                      &
     &    nod_bc1_t%num_idx_ibc, nod_bc1_t%ibc)
      call count_bc_element_layer                                       &
     &   (node1, ele1, iele_fl_start, iele_fl_end,                      &
     &    nod_bc1_t%num_idx_ibc2, nod_bc1_t%ibc2)
!
      call alloc_nod_bc_scalar_ele_type                                 &
      &  (np_smp, ele1%nnod_4_ele, nod_bc1_t)
!
      call set_ele_4_scalar_nodal_bc_fl(node1, ele1,                    &
     &    nod_bc1_t%ibc, nod_bc1_t%ibc2,                                &
     &    nod_bc1_t%num_idx_ibc, nod_bc1_t%ele_bc_id,                   &
     &    nod_bc1_t%nod_bc_id, nod_bc1_t%num_idx_ibc2,                  &
     &    nod_bc1_t%ele_bc2_id, nod_bc1_t%nod_bc2_id,                   &
     &    nod_bc1_t%ibc_end, nod_bc1_t%ibc_shape, nod_bc1_t%ibc_stack,  &
     &    nod_bc1_t%ibc_stack_smp)
!
      call dealloc_scalar_ibc_type(nod_bc1_t)
!
      end subroutine set_ele_nodal_bc_4_temp
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_temp_sgs
!
      use m_bc_data_ene
!
!
      call count_bc_element_layer                                       &
     &   (node1, ele1, iele_fl_start, iele_fl_end,                      &
     &    sgs_bc1_t%num_idx_ibc, sgs_bc1_t%ibc)
      call count_bc_element_layer                                       &
     &   (node1, ele1, iele_fl_start, iele_fl_end,                      &
     &    sgs_bc1_t%num_idx_ibc2, sgs_bc1_t%ibc2)
!
      call alloc_nod_bc_scalar_ele_type                                 &
      &  (np_smp, ele1%nnod_4_ele, sgs_bc1_t)
!
      call set_ele_4_scalar_nodal_bc_fl(node1, ele1,                    &
     &    sgs_bc1_t%ibc, sgs_bc1_t%ibc2, &
     &    sgs_bc1_t%num_idx_ibc, sgs_bc1_t%ele_bc_id,         &
     &    sgs_bc1_t%nod_bc_id, sgs_bc1_t%num_idx_ibc2,  &
     &    sgs_bc1_t%ele_bc2_id, sgs_bc1_t%nod_bc2_id,   &
     &    sgs_bc1_t%ibc_end, sgs_bc1_t%ibc_shape, sgs_bc1_t%ibc_stack,  &
     &    sgs_bc1_t%ibc_stack_smp)
!
      call dealloc_scalar_ibc_type(sgs_bc1_t)
!
      end subroutine set_ele_nodal_bc_4_temp_sgs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_composition
!
      use m_bc_data_ene
!
      call count_bc_element_layer                                       &
     &   (node1, ele1, iele_fl_start, iele_fl_end,                      &
     &    nod_bc1_c%num_idx_ibc, nod_bc1_c%ibc)
      call count_bc_element_layer                                       &
     &   (node1, ele1, iele_fl_start, iele_fl_end,                      &
     &    nod_bc1_c%num_idx_ibc2, nod_bc1_c%ibc2)
!
      call alloc_nod_bc_scalar_ele_type                                 &
     &   (np_smp, ele1%nnod_4_ele, nod_bc1_c)
!
      call set_ele_4_scalar_nodal_bc_fl(node1, ele1,                    &
     &    nod_bc1_c%ibc, nod_bc1_c%ibc2,                                &
     &    nod_bc1_c%num_idx_ibc, nod_bc1_c%ele_bc_id,                   &
     &    nod_bc1_c%nod_bc_id, nod_bc1_c%num_idx_ibc2,                &
     &    nod_bc1_c%ele_bc2_id, nod_bc1_c%nod_bc2_id,                   &
     &    nod_bc1_c%ibc_end, nod_bc1_c%ibc_shape,                      &
     &    nod_bc1_c%ibc_stack, nod_bc1_c%ibc_stack_smp)
!
      call dealloc_scalar_ibc_type(nod_bc1_c)
!
      end subroutine set_ele_nodal_bc_4_composition
!
!  ---------------------------------------------------------------------
!
      end module set_ele_nod_bc_scalars
