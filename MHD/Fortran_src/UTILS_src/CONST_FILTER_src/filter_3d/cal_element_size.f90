!
!      module cal_element_size
!
!      Written by H.Matsui on Nov., 2006
!      Modified by H. Matsui on Mar., 2008
!
!!      subroutine s_cal_element_size(nod_comm, node, ele, jac_3d_q,    &
!!     &          rhs_tbl, tbl_crs, mat_tbl, rhs_mat, FEM_elen,         &
!!     &          filter_dxi, dxidxs)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(dxdxi_data_type), intent(inout) :: filter_dxi
!!        type(dxidx_data_type), intent(inout) :: dxidxs
!!
!!      subroutine s_const_filter_mom_ele(nod_comm, node, ele,          &
!!     &          jac_3d_q, rhs_tbl, tbl_crs, rhs_mat, mom_nod, mom_ele)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(jacobians_3d), intent(in) :: jac_3d_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(nod_mom_diffs_type), intent(inout) :: mom_nod
!!        type(ele_mom_diffs_type), intent(inout) :: mom_ele
!!
!!      subroutine release_mass_mat_for_consist(rhs_mat)
!
      module cal_element_size
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_comm_table
      use t_geometry_data
      use t_next_node_ele_4_node
      use t_table_FEM_const
      use t_finite_element_mat
      use t_crs_connect
      use t_crs_matrix
      use t_jacobian_3d
!
      implicit none
!
      type(next_nod_ele_table), save, private :: next_tbl_f
      type(CRS_matrix), save, private :: mass1
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_element_size(nod_comm, node, ele, jac_3d_q,      &
     &          rhs_tbl, tbl_crs, mat_tbl, rhs_mat, FEM_elen,           &
     &          filter_dxi, dxidxs)
!
      use m_ctl_params_4_gen_filter
      use m_reference_moments
!
      use t_filter_elength
      use t_filter_dxdxi
!
      use set_table_type_RHS_assemble
      use cal_diff_elesize_on_ele
      use cal_filter_moms_ele_by_elen
      use int_consist_mass_mat_filter
      use int_mass_matrix_gen_filter
      use int_vol_elesize_on_node
      use cal_filter_moms_by_element
      use filter_moments_send_recv
      use cal_dxidx_ele
      use cal_deltax_and_prods_4_nod
      use cal_1st_diff_deltax_4_nod
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_q
!
      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(table_mat_const), intent(inout) :: mat_tbl
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(gradient_model_data_type), intent(inout) :: FEM_elen
      type(dxdxi_data_type), intent(inout) :: filter_dxi
      type(dxidx_data_type), intent(inout) :: dxidxs
!
!  ---------------------------------------------------
!      set RHS assemble table
!  ---------------------------------------------------
!
      if (iflag_debug.eq.1)  write(*,*) 's_set_table_type_RHS_assemble'
      call s_set_table_type_RHS_assemble                                &
     &   (node, ele, next_tbl_f, rhs_tbl)
!
!  ---------------------------------------------------
!        cal element size for each node
!  ---------------------------------------------------
!
!
      if (iflag_debug.eq.1)  write(*,*) 'alloc_nodal_elen_type'
      call alloc_nodal_elen_type                                        &
     &   (FEM_elen%nnod_filter_mom, FEM_elen%elen_nod)
      call alloc_jacobians_node(FEM_elen%nnod_filter_mom, filter_dxi)
      call alloc_dxidxs_ele(ele%numele, dxidxs)
      call alloc_dxidxs_node(node%numnod, dxidxs)
!
      call alloc_fem_mat_base_type(node, ele, rhs_mat)
      call allocate_scalar_ele_4_int(ele%numele)
!
      if (itype_mass_matrix .eq. 1) then
        if (iflag_debug.eq.1) write(*,*) 'set_consist_mass_matrix'
        call set_consist_mass_matrix(node, ele, jac_3d_q,               &
     &      next_tbl_f%neib_nod, rhs_tbl, tbl_crs, mat_tbl,             &
     &      rhs_mat%fem_wk, mass1)
      end if
!
      if (iflag_debug.eq.1)  write(*,*) 'int_mass_matrix_4_filter'
      call int_mass_matrix_4_filter(node, ele, jac_3d_q,                &
     &   rhs_tbl, rhs_mat%fem_wk, rhs_mat%fem_rhs%f_l, rhs_mat%m_lump)
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_dxidx_ele_type'
      call cal_dxidx_ele_type(ele, jac_3d_q, dxidxs%dx_ele)
!
!  ---------------------------------------------------
!        cal element size for each node
!  ---------------------------------------------------
!
      call cal_dx2_on_node(nod_comm, node, ele, jac_3d_q,               &
     &    rhs_tbl, tbl_crs, rhs_mat%m_lump, itype_mass_matrix,          &
     &    mass1, FEM_elen, rhs_mat%fem_wk, rhs_mat%fem_rhs%f_l)
      call cal_dxi_dxes_node(nod_comm, node, ele, jac_3d_q,             &
     &    rhs_tbl, tbl_crs, rhs_mat%m_lump, itype_mass_matrix,          &
     &    mass1, dxidxs, rhs_mat%fem_wk, rhs_mat%fem_rhs%f_l)
!
      call elength_nod_send_recv(node, nod_comm, FEM_elen%elen_nod)
      call dxidx_nod_send_recv(node, nod_comm, dxidxs%dx_nod)
!
!  ---------------------------------------------------
!        cal products of element size for each node
!  ---------------------------------------------------
!
      if (itype_mass_matrix .eq. 1) then
        if (iflag_debug.eq.1) write(*,*) 'cal_1st_diffs_dx_by_consist'
        call cal_1st_diffs_dx_by_consist(nod_comm, node, ele,           &
     &     jac_3d_q, rhs_tbl, tbl_crs, mass1,                           &
     &     FEM_elen, rhs_mat%fem_wk, rhs_mat%fem_rhs%f_nl)
      else
        if (iflag_debug.eq.1) write(*,*) 'cal_1st_diffs_dx_by_lump'
        call cal_1st_diffs_dx_by_lump(node, ele, jac_3d_q,              &
     &      rhs_tbl, rhs_mat%m_lump, FEM_elen,                          &
     &      rhs_mat%fem_wk, rhs_mat%fem_rhs%f_nl)
      end if
!
      if (iflag_debug.eq.1)  write(*,*) 'diff_elen_nod_send_recv'
      call diff_elen_nod_send_recv(node, nod_comm, FEM_elen%elen_nod)
!
!  ---------------------------------------------------
!        filter moments on each node
!  ---------------------------------------------------
!
      call allocate_reference_moments
      call allocate_seed_moms_ele(FEM_elen%nele_filter_mom)
      call allocate_seed_moms_nod(FEM_elen%nnod_filter_mom)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_filter_moments_on_ele'
      call cal_filter_moments_on_ele(filter_dxi%dxi_ele, FEM_elen)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_filter_moments_on_node_1st'
      call cal_filter_moments_on_node_1st                               &
     &   (nod_comm, node, ele, jac_3d_q,                                &
     &    rhs_tbl, tbl_crs, rhs_mat%m_lump, FEM_elen, mass1,            &
     &    rhs_mat%fem_wk, rhs_mat%fem_rhs%f_l)
!
!  ---------------------------------------------------
!        differences of element size for each element
!  ---------------------------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'cal_diffs_delta_on_element'
      call cal_diffs_delta_on_element(node, ele, jac_3d_q, FEM_elen)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_2nd_diffs_delta_on_element'
      call cal_2nd_diffs_delta_on_element                               &
     &   (node, ele, jac_3d_q, FEM_elen)
!
      if (iflag_momentum_type .eq. 1) then
        call delete_x_products_of_elen(FEM_elen)
      end if
!
      call deallocate_seed_moms_ele
!
      call dealloc_iele_belonged(next_tbl_f%neib_ele)
      call dealloc_inod_next_node(next_tbl_f%neib_nod)
!
      end subroutine s_cal_element_size
!
!-----------------------------------------------------------------------
!
      subroutine s_const_filter_mom_ele(nod_comm, node, ele,            &
     &          jac_3d_q, rhs_tbl, tbl_crs, rhs_mat, mom_nod, mom_ele)
!
      use t_filter_moments
      use m_ctl_params_4_gen_filter
      use cal_diff_elesize_on_ele
      use cal_1st_diff_deltax_4_nod
      use filter_moments_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(CRS_matrix_connect), intent(in) :: tbl_crs
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
      type(ele_mom_diffs_type), intent(inout) :: mom_ele
!
!
      call filter_mom_nod_send_recv(node, nod_comm, mom_nod)
!
      if (itype_mass_matrix .eq. 1) then
        call cal_diffs_filter_nod_consist(nod_comm, node, ele,          &
     &      jac_3d_q, rhs_tbl, tbl_crs, mass1,                          &
     &      rhs_mat%fem_wk, rhs_mat%fem_rhs%f_nl, mom_nod)
      else
        call cal_diffs_filter_nod_lump(node, ele, jac_3d_q,             &
     &     rhs_tbl, rhs_mat%m_lump, rhs_mat%fem_wk,                     &
     &     rhs_mat%fem_rhs%f_nl, mom_nod)
      end if
!
      call diff_filter_mom_nod_send_recv(node, nod_comm, mom_nod)
!
      call cal_filter_moms_ele_by_nod                                   &
     &   (node, ele, jac_3d_q, mom_nod, mom_ele)
      call cal_1st_diffs_filter_ele                                     &
     &   (node, ele, jac_3d_q, mom_nod, mom_ele)
      call cal_2nd_diffs_filter_ele                                     &
     &   (node, ele, jac_3d_q, mom_nod, mom_ele)
!
      end subroutine s_const_filter_mom_ele
!
!-----------------------------------------------------------------------
!
      subroutine release_mass_mat_for_consist(tbl_crs, rhs_mat)
!
      use int_vol_elesize_on_node
!
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!
!
      call deallocate_scalar_ele_4_int
      call dealloc_fem_mat_base_type(rhs_mat)
      call dealloc_crs_mat_data(mass1)
      call dealloc_crs_connect(tbl_crs)
!
      end subroutine release_mass_mat_for_consist
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      end module cal_element_size
