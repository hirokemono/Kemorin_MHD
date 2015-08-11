!
!      module set_ele_nod_bc_vectors
!
!      Written by H. Matsui nad H. Okuda
!      Modified by H. Matsui on Oct., 2005
!
!      subroutine set_ele_nodal_bc_4_velo
!      subroutine set_ele_nodal_bc_4_velo_sgs
!      subroutine set_ele_nodal_bc_4_vect_p
!      subroutine set_ele_nodal_bc_4_vecp_sgs
!      subroutine set_ele_nodal_bc_4_magne
!      subroutine set_ele_nodal_bc_4_mag_sgs
!      subroutine set_ele_nodal_bc_4_current
!
      module set_ele_nod_bc_vectors
!
      use m_precision
!
      use m_geometry_data
      use count_bc_element
      use count_num_nodal_fields
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
      subroutine set_ele_nodal_bc_4_velo
!
      use m_bc_data_velo
!
!
      call count_bc_element_4_vect_fl(num_idx_ibc_v, ibc_velo)
      call count_bc_element_4_vect_fl(num_idx_ibc2_v, ibc2_velo)
!
      call cal_max_int_4_vector(nmax_idx_ibc_v,  num_idx_ibc_v)
      call cal_max_int_4_vector(nmax_idx_ibc2_v, num_idx_ibc2_v)
!
      call allocate_bc_velo_4_element
!
      call set_ele_4_vector_nodal_bc_fl                                 &
     &   (node1%numnod, nnod_4_ele, ibc_velo, ibc2_velo,                &
     &    nmax_idx_ibc_v, num_idx_ibc_v, ele_bc_v_id, nod_bc_v_id,      &
     &    nmax_idx_ibc2_v, ele_bc2_v_id, nod_bc2_v_id, ibc_v_end,       &
     &    ibc_v_shape, ibc_v_stack, ibc_v_stack_smp)
!
      call deallocate_ibc_4_velo
!
      end subroutine set_ele_nodal_bc_4_velo
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_velo_sgs
!
      use m_bc_velo_sgs
!
!
      call count_bc_element_4_vect_fl(num_idx_ibc_v_sgs, ibc_v_sgs)
      call count_bc_element_4_vect_fl(num_idx_ibc2_v_sgs, ibc2_v_sgs)
!
      call cal_max_int_4_vector(nmax_idx_ibc_v_sgs,  num_idx_ibc_v_sgs)
      call cal_max_int_4_vector(nmax_idx_ibc2_v_sgs, num_idx_ibc2_v_sgs)
!
      call allocate_bc_vsgs_4_ele
!
      call set_ele_4_vector_nodal_bc_fl                                 &
     &   (node1%numnod, nnod_4_ele, ibc_v_sgs, ibc2_v_sgs,              &
     &    nmax_idx_ibc_v_sgs, num_idx_ibc_v_sgs, ele_bc_v_sgs_id,       &
     &    nod_bc_v_sgs_id, nmax_idx_ibc2_v_sgs, ele_bc2_v_sgs_id,       &
     &    nod_bc2_v_sgs_id, ibc_v_sgs_end, ibc_v_sgs_shape,             &
     &    ibc_v_sgs_stack, ibc_v_sgs_stack_smp)
!
      call deallocate_ibc_4_vsgs
!
      end subroutine set_ele_nodal_bc_4_velo_sgs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_vect_p
!
      use m_bc_data_vect_p
!
!
      call count_bc_element_4_vect(num_idx_ibc_vp, ibc_vp)
      call count_bc_element_4_vect(num_idx_ibc2_vp, ibc2_vp)
!
      call cal_max_int_4_vector(nmax_idx_ibc_vp,  num_idx_ibc_vp)
      call cal_max_int_4_vector(nmax_idx_ibc2_vp, num_idx_ibc2_vp)
!
      call allocate_bc_vect_p_4_element
!
      call set_ele_4_vector_nodal_bc                                    &
     &   (node1%numnod, nnod_4_ele, ibc_vp, ibc2_vp, nmax_idx_ibc_vp,   &
     &    num_idx_ibc_vp, ele_bc_vp_id, nod_bc_vp_id, nmax_idx_ibc2_vp, &
     &    ele_bc2_vp_id, nod_bc2_vp_id, ibc_vp_end, ibc_vp_shape,       &
     &    ibc_vp_stack, ibc_vp_stack_smp)
!
      call deallocate_ibc_4_vect_p
!
      end subroutine set_ele_nodal_bc_4_vect_p
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_vecp_sgs
!
      use m_bc_vecp_sgs
!
!
      call count_bc_element_4_vect(num_idx_ibc_a_sgs, ibc_a_sgs)
      call count_bc_element_4_vect(num_idx_ibc2_a_sgs, ibc2_a_sgs)
!
      call cal_max_int_4_vector(nmax_idx_ibc_a_sgs,  num_idx_ibc_a_sgs)
      call cal_max_int_4_vector(nmax_idx_ibc2_a_sgs, num_idx_ibc2_a_sgs)
!
      call allocate_bc_vecp_sgs_4_ele
!
      call set_ele_4_vector_nodal_bc                                    &
     &   (node1%numnod, nnod_4_ele, ibc_a_sgs, ibc2_a_sgs,              &
     &    nmax_idx_ibc_a_sgs, num_idx_ibc_a_sgs, ele_bc_a_sgs_id,       &
     &    nod_bc_a_sgs_id, nmax_idx_ibc2_a_sgs, ele_bc2_a_sgs_id,       &
     &    nod_bc2_a_sgs_id, ibc_a_sgs_end, ibc_a_sgs_shape,             &
     &    ibc_a_sgs_stack, ibc_a_sgs_stack_smp)
!
      call deallocate_ibc_4_vecp_sgs
!
      end subroutine set_ele_nodal_bc_4_vecp_sgs
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_magne
!
      use m_bc_data_magne
!
!
      call count_bc_element_4_vect(num_idx_ibc_b, ibc_magne)
      call count_bc_element_4_vect(num_idx_ibc2_b, ibc2_magne)
!
      call cal_max_int_4_vector(nmax_idx_ibc_b,  num_idx_ibc_b)
      call cal_max_int_4_vector(nmax_idx_ibc2_b, num_idx_ibc2_b)
!
      call allocate_bc_magne_4_element
!
      call set_ele_4_vector_nodal_bc                                    &
     &   (node1%numnod, nnod_4_ele, ibc_magne, ibc2_magne,              &
     &    nmax_idx_ibc_b, num_idx_ibc_b, ele_bc_b_id, nod_bc_b_id,      &
     &    nmax_idx_ibc2_b, ele_bc2_b_id, nod_bc2_b_id, ibc_b_end,       &
     &    ibc_b_shape, ibc_b_stack, ibc_b_stack_smp)
!
      call deallocate_ibc_4_magne
!
      end subroutine set_ele_nodal_bc_4_magne
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_mag_sgs
!
      use m_bc_magne_sgs
!
!
      call count_bc_element_4_vect(num_idx_ibc_b_sgs, ibc_b_sgs)
      call count_bc_element_4_vect(num_idx_ibc2_b_sgs, ibc2_b_sgs)
!
      call cal_max_int_4_vector(nmax_idx_ibc_b_sgs,  num_idx_ibc_b_sgs)
      call cal_max_int_4_vector(nmax_idx_ibc2_b_sgs, num_idx_ibc2_b_sgs)
!
      call allocate_bc_b_sgs_ele
!
      call set_ele_4_vector_nodal_bc                                    &
     &   (node1%numnod, nnod_4_ele, ibc_b_sgs, ibc2_b_sgs,              &
     &    nmax_idx_ibc_b_sgs, num_idx_ibc_b_sgs, ele_bc_b_sgs_id,       &
     &    nod_bc_b_sgs_id, nmax_idx_ibc2_b_sgs, ele_bc2_b_sgs_id,       &
     &    nod_bc2_b_sgs_id, ibc_b_sgs_end, ibc_b_sgs_shape,             &
     &    ibc_b_sgs_stack, ibc_b_sgs_stack_smp)
!
      call deallocate_ibc_4_b_sgs
!
      end subroutine set_ele_nodal_bc_4_mag_sgs
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_current
!
      use m_bc_data_current
!
!
      call deallocate_ibc_4_current
!
      end subroutine set_ele_nodal_bc_4_current
!
!  ---------------------------------------------------------------------
!
      end module set_ele_nod_bc_vectors
