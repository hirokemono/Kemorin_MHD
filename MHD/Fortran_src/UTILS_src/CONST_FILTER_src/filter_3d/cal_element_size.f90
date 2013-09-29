!
!      module cal_element_size
!
      module cal_element_size
!
!      Written by H.Matsui on Nov., 2006
!      Modified by H. Matsui on Mar., 2008
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
!      subroutine s_cal_element_size
!      subroutine s_const_filter_mom_ele(ifil)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_element_size
!
      use m_finite_element_matrix
      use m_ctl_params_4_gen_filter
      use m_element_id_4_node
      use m_next_node_id_4_node
      use m_filter_elength
      use m_filter_dxdxi
      use m_reference_moments
      use m_dxi_dxes_3d_node
      use m_filter_dxdxi
      use m_crs_consist_mass_mat
!
      use set_element_id_4_node
      use ordering_4_rhs_assemble
      use cal_diff_elesize_on_ele
      use cal_filter_moms_ele_by_elen
      use int_mass_matrix_gen_filter
      use int_vol_elesize_on_node
      use cal_filter_moms_by_element
      use filter_moments_send_recv
      use cal_dxidx_ele
      use cal_deltax_and_prods_4_nod
      use cal_1st_diff_deltax_4_nod
!
!  ---------------------------------------------------
!      set RHS assemble table
!  ---------------------------------------------------
!
      if (iflag_debug.eq.1)  write(*,*) 'set_ele_id_4_node'
       call set_ele_id_4_node
!
      if (iflag_debug.eq.1)  write(*,*) 'const_next_nod_id_4_node'
       call const_next_nod_id_4_node
!
      if (iflag_debug.eq.1)  write(*,*) 'sort_node_index'
      call sort_node_index
!
!  ---------------------------------------------------
!        cal element size for each node
!  ---------------------------------------------------
!
!
      if (iflag_debug.eq.1)  write(*,*) 'allocate_nodal_ele_length'
      call allocate_nodal_ele_length
      call allocate_jacobians_on_node
      call allocate_dxi_dx_ele
      call allocate_dxi_dx_nod
!
      call allocate_fem_mat_base
      call allocate_scalar_ele_4_int
!
      if (itype_mass_matrix .eq. 1) then
        if (iflag_debug.eq.1) write(*,*) 'set_mass_matrix_for_consist'
        call set_mass_matrix_for_consist
      end if
!
      if (iflag_debug.eq.1)  write(*,*) 'int_mass_matrix_4_filter'
      call int_mass_matrix_4_filter
!
      if (iflag_debug.eq.1)  write(*,*) 's_cal_dxidx_ele'
      call s_cal_dxidx_ele
!
!  ---------------------------------------------------
!        cal element size for each node
!  ---------------------------------------------------
!
      call cal_dx2_on_node(itype_mass_matrix)
      call cal_dxi_dxes_node(itype_mass_matrix)
!
      call elength_nod_send_recv
      call dxidx_nod_send_recv
!
!  ---------------------------------------------------
!        cal products of element size for each node
!  ---------------------------------------------------
!
      if (itype_mass_matrix .eq. 1) then
        if (iflag_debug.eq.1) write(*,*) 'cal_1st_diffs_dx_by_consist'
        call cal_1st_diffs_dx_by_consist
      else
        if (iflag_debug.eq.1) write(*,*) 'cal_1st_diffs_dx_by_lump'
        call cal_1st_diffs_dx_by_lump
      end if
!
      if (iflag_debug.eq.1)  write(*,*) 'diff_elen_nod_send_recv'
      call diff_elen_nod_send_recv
!
!  ---------------------------------------------------
!        filter moments on each node
!  ---------------------------------------------------
!
      call allocate_reference_moments
      call allocate_seed_moms_ele
      call allocate_seed_moms_nod
!
      if (iflag_debug.eq.1) write(*,*) 'cal_filter_moments_on_ele'
      call cal_filter_moments_on_ele
!
      if (iflag_debug.eq.1) write(*,*) 'cal_filter_moments_on_node'
      call cal_filter_moments_on_node
!
!  ---------------------------------------------------
!        differences of element size for each element
!  ---------------------------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'cal_diffs_delta_on_element'
      call cal_diffs_delta_on_element
!
      if (iflag_debug.eq.1) write(*,*) 'cal_2nd_diffs_delta_on_element'
      call cal_2nd_diffs_delta_on_element
!
      if (iflag_momentum_type .eq. 1) then
        call delete_cross_products_of_elen
      end if
!
      call deallocate_seed_moms_ele
!
      call deallocate_jacobians_for_ele
      call deallocate_iele_belonged
      call deallocate_inod_next_node
!
      end subroutine s_cal_element_size
!
!-----------------------------------------------------------------------
!
      subroutine s_const_filter_mom_ele(ifil)
!
      use m_ctl_params_4_gen_filter
      use cal_diff_elesize_on_ele
      use cal_1st_diff_deltax_4_nod
      use filter_moments_send_recv
!
      integer(kind = kint), intent(in) :: ifil
!
      call filter_mom_nod_send_recv(ifil)
!
      if (itype_mass_matrix .eq. 1) then
        call cal_diffs_filter_nod_consist(ifil)
      else
        call cal_diffs_filter_nod_lump(ifil)
      end if
!
      call diff_filter_mom_nod_send_recv(ifil)
!
      call cal_filter_moms_ele_by_nod(ifil)
      call cal_1st_diffs_filter_ele(ifil)
      call cal_2nd_diffs_filter_ele(ifil)
!
      end subroutine s_const_filter_mom_ele
!
!-----------------------------------------------------------------------
!
      subroutine set_mass_matrix_for_consist
!
      use m_crs_connect
      use m_crs_consist_mass_mat
      use set_crs_connection
      use set_index_list_4_crs
      use set_consist_mass_connect
      use int_consist_mass_mat_filter
!
!  ---------------------------------------------------
!       set CRS matrix connectivity for whole domain
!  ---------------------------------------------------
!
      if (iflag_debug.eq.1)  write(*,*) 's_set_crs_connection'
      call s_set_crs_connection
!
      if (iflag_debug.eq.1)  write(*,*) 'set_idx_list_4_whole_crs'
      call set_idx_list_4_whole_crs
!
!  ---------------------------------------------------
!        cal consist mass matrix
!  ---------------------------------------------------
!
      if (iflag_debug.eq.1)  write(*,*) 's_set_consist_mass_connect'
      call s_set_consist_mass_connect
!
      if (iflag_debug.eq.1)  write(*,*) 'deallocate_crs_connect'
      call deallocate_crs_connect
!
      if (iflag_debug.eq.1)  write(*,*) 'int_vol_consist_mass_matrix'
      call int_vol_consist_mass_matrix
!
      end subroutine set_mass_matrix_for_consist
!
!-----------------------------------------------------------------------
!
      subroutine release_mass_mat_for_consist
!
      use m_finite_element_matrix
      use m_crs_consist_mass_mat
      use int_vol_elesize_on_node
!
      call deallocate_scalar_ele_4_int
      call deallocate_fem_mat_base
      call deallocate_aiccg_mass
      call deallocate_mass_connect
!
      end subroutine release_mass_mat_for_consist
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      end module cal_element_size
