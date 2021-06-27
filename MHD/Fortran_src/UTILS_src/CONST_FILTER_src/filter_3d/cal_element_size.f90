!
!>@file   calypso_SR_type.f90
!!@brief  module calypso_SR_type
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2006
!!      Modified in  Mar., 2008
!
!>@brief  Select communication routines for spherical harmonics transform
!!
!!@verbatim
!!      subroutine s_cal_element_size(mesh, group, fil_elist,           &
!!     &          gfil_p, tbl_crs, mat_tbl, rhs_mat, fem_int, FEM_elen, &
!!     &          ref_m, filter_dxi, dxidxs, v_sol, SR_sig, SR_r)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(element_list_4_filter), intent(in) :: fil_elist
!!        type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(dxdxi_data_type), intent(inout) :: filter_dxi
!!        type(dxidx_data_type), intent(inout) :: dxidxs
!!        type(reference_moments), intent(inout) :: ref_m
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!      subroutine s_const_filter_mom_ele(nod_comm, node, ele,          &
!!     &          g_FEM, jac_3d_q, rhs_tbl, tbl_crs, m_lump, rhs_mat,   &
!!     &          gfil_p, mom_nod, mom_ele, v_sol, SR_sig, SR_r)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(nod_mom_diffs_type), intent(inout) :: mom_nod
!!        type(ele_mom_diffs_type), intent(inout) :: mom_ele
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!      subroutine release_mass_mat_for_consist(rhs_mat)
!!        type(CRS_matrix_connect), intent(inout) :: tbl_crs
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!@endverbatim
!
      module cal_element_size
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_next_node_ele_4_node
      use t_table_FEM_const
      use t_work_FEM_integration
      use t_finite_element_mat
      use t_crs_connect
      use t_crs_matrix
      use t_jacobian_3d
      use t_vector_for_solver
      use t_solver_SR
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
      subroutine s_cal_element_size(mesh, group, fil_elist,             &
     &          gfil_p, tbl_crs, mat_tbl, rhs_mat, fem_int, FEM_elen,   &
     &          ref_m, filter_dxi, dxidxs, v_sol, SR_sig, SR_r)
!
!
      use t_filter_elength
      use t_reference_moments
      use t_filter_dxdxi
      use t_element_list_4_filter
      use t_ctl_params_4_gen_filter
!
      use set_table_4_RHS_assemble
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
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_list_4_filter), intent(in) :: fil_elist
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(table_mat_const), intent(inout) :: mat_tbl
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(finite_element_integration), intent(inout) :: fem_int
      type(gradient_model_data_type), intent(inout) :: FEM_elen
      type(reference_moments), intent(inout) :: ref_m
      type(dxdxi_data_type), intent(inout) :: filter_dxi
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!  ---------------------------------------------------
!      set RHS assemble table
!  ---------------------------------------------------
!
      if (iflag_debug.eq.1)  write(*,*) 's_set_RHS_assemble_table'
      call s_set_RHS_assemble_table(mesh, next_tbl_f, fem_int%rhs_tbl)
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
      call alloc_dxidxs_ele(mesh%ele%numele, dxidxs)
      call alloc_dxidxs_node(mesh%node%numnod, dxidxs)
!
      call alloc_finite_elem_mat(mesh, rhs_mat)
      call alloc_int_surf_data                                          &
     &   (group%surf_grp%num_item, mesh%surf%nnod_4_surf,               &
     &    rhs_mat%surf_wk)
      call alloc_fem_int_base_type(mesh, fem_int)
      call allocate_scalar_ele_4_int(mesh%ele%numele)
!
      if(gfil_p%itype_mass_matrix .eq. 1) then
        if (iflag_debug.eq.1) write(*,*) 'set_consist_mass_matrix'
        call set_consist_mass_matrix(gfil_p, mesh%node, mesh%ele,       &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, next_tbl_f%neib_nod, &
     &      fem_int%rhs_tbl, tbl_crs, mat_tbl, rhs_mat%fem_wk, mass1)
      end if
!
      if (iflag_debug.eq.1)  write(*,*) 'int_mass_matrix_4_filter'
      call int_mass_matrix_4_filter(gfil_p, mesh%node, mesh%ele,        &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,       &
     &    rhs_mat%fem_wk, rhs_mat%f_l, fem_int%m_lump)
!
      if (iflag_debug.eq.1)  write(*,*) 'cal_dxidx_ele_type'
      call cal_dxidx_ele_type(mesh%ele,                                 &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, dxidxs%dx_ele)
!
!  ---------------------------------------------------
!        cal element size for each node
!  ---------------------------------------------------
!
      call cal_dx2_on_node(mesh%nod_comm, mesh%node, mesh%ele,          &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,       &
     &    tbl_crs, fem_int%m_lump, fil_elist, gfil_p, mass1, FEM_elen,  &
     &    rhs_mat%fem_wk, rhs_mat%f_l, v_sol, SR_sig, SR_r)
      call cal_dxi_dxes_node(mesh%nod_comm, mesh%node, mesh%ele,        &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,       &
     &    tbl_crs, fem_int%m_lump, fil_elist, gfil_p, mass1, dxidxs,    &
     &    rhs_mat%fem_wk, rhs_mat%f_l, v_sol, SR_sig, SR_r)
!
      call elength_nod_send_recv(mesh%node%numnod, mesh%nod_comm,       &
     &    FEM_elen%elen_nod, v_sol, SR_sig, SR_r)
      call dxidx_nod_send_recv(mesh%node%numnod, mesh%nod_comm,         &
     &    dxidxs%dx_nod, v_sol, SR_sig, SR_r)
!
!  ---------------------------------------------------
!        cal products of element size for each node
!  ---------------------------------------------------
!
      if(gfil_p%itype_mass_matrix .eq. 1) then
        if (iflag_debug.eq.1) write(*,*) 'cal_1st_diffs_dx_by_consist'
        call cal_1st_diffs_dx_by_consist                                &
     &     (mesh%nod_comm, mesh%node, mesh%ele, fem_int%jcs%g_FEM,      &
     &      fem_int%jcs%jac_3d, fem_int%rhs_tbl, tbl_crs, gfil_p,       &
     &      mass1, FEM_elen, rhs_mat%fem_wk, rhs_mat%f_nl,              &
     &      v_sol, SR_sig, SR_r)
      else
        if (iflag_debug.eq.1) write(*,*) 'cal_1st_diffs_dx_by_lump'
        call cal_1st_diffs_dx_by_lump(gfil_p%num_int_points,            &
     &      mesh%node, mesh%ele, fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, &
     &      fem_int%rhs_tbl, fem_int%m_lump, FEM_elen,                  &
     &      rhs_mat%fem_wk, rhs_mat%f_nl)
      end if
!
      if (iflag_debug.eq.1)  write(*,*) 'diff_elen_nod_send_recv'
      call diff_elen_nod_send_recv(mesh%node%numnod, mesh%nod_comm,     &
     &    FEM_elen%elen_nod, v_sol, SR_sig, SR_r)
!
!  ---------------------------------------------------
!        filter moments on each node
!  ---------------------------------------------------
!
      call alloc_reference_moments(ref_m)
      call alloc_seed_moms_ele(FEM_elen%nele_filter_mom, ref_m)
      call alloc_seed_moms_nod(FEM_elen%nnod_filter_mom, ref_m)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_filter_moments_on_ele'
      call cal_filter_moments_on_ele                                    &
     &   (gfil_p, filter_dxi%dxi_ele, FEM_elen, ref_m)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_filter_moments_on_node_1st'
      call cal_filter_moments_on_node_1st                               &
     &   (mesh%nod_comm, mesh%node, mesh%ele,                           &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_3d,                        &
     &    fem_int%rhs_tbl, tbl_crs, fem_int%m_lump, fil_elist,          &
     &    FEM_elen, gfil_p, mass1, rhs_mat%fem_wk,                      &
     &    rhs_mat%f_l, ref_m, v_sol, SR_sig, SR_r)
!
!  ---------------------------------------------------
!        differences of element size for each element
!  ---------------------------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'cal_diffs_delta_on_element'
      call cal_diffs_delta_on_element                                   &
     &   (gfil_p%num_int_points, mesh%node, mesh%ele,                   &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, FEM_elen)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_2nd_diffs_delta_on_element'
      call cal_2nd_diffs_delta_on_element                               &
     &   (gfil_p%num_int_points, mesh%node, mesh%ele,                   &
     &    fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, FEM_elen)
!
      if (gfil_p%iflag_momentum_type .eq. 1) then
        call delete_x_products_of_elen(FEM_elen)
      end if
!
      call dealloc_seed_moms_ele(ref_m)
!
      call dealloc_next_nod_ele_table(next_tbl_f)
!
      end subroutine s_cal_element_size
!
!-----------------------------------------------------------------------
!
      subroutine s_const_filter_mom_ele(nod_comm, node, ele,            &
     &          g_FEM, jac_3d_q, rhs_tbl, tbl_crs, m_lump, rhs_mat,     &
     &          gfil_p, mom_nod, mom_ele, v_sol, SR_sig, SR_r)
!
      use t_filter_moments
      use t_fem_gauss_int_coefs
      use t_fem_gauss_int_coefs
      use t_ctl_params_4_gen_filter
      use cal_diff_elesize_on_ele
      use cal_1st_diff_deltax_4_nod
      use filter_moments_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
      type(ele_mom_diffs_type), intent(inout) :: mom_ele
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call filter_mom_nod_send_recv                                     &
     &   (node%numnod, nod_comm, mom_nod, v_sol, SR_sig, SR_r)
!
      if(gfil_p%itype_mass_matrix .eq. 1) then
        call cal_diffs_filter_nod_consist                               &
     &     (nod_comm, node, ele, g_FEM, jac_3d_q, rhs_tbl, tbl_crs,     &
     &      gfil_p, mass1, rhs_mat%fem_wk, rhs_mat%f_nl,                &
     &      mom_nod, v_sol, SR_sig, SR_r)
      else
        call cal_diffs_filter_nod_lump                                  &
     &     (gfil_p%num_int_points, node, ele, g_FEM, jac_3d_q,          &
     &      rhs_tbl, m_lump, rhs_mat%fem_wk, rhs_mat%f_nl, mom_nod)
      end if
!
      call diff_filter_mom_nod_send_recv                                &
     &   (node%numnod, nod_comm, mom_nod, v_sol, SR_sig, SR_r)
!
      call cal_filter_moms_ele_by_nod(gfil_p%num_int_points,            &
     &    node, ele, g_FEM, jac_3d_q, mom_nod, mom_ele)
      call cal_1st_diffs_filter_ele(gfil_p%num_int_points,              &
     &    node, ele, g_FEM, jac_3d_q, mom_nod, mom_ele)
      call cal_2nd_diffs_filter_ele(gfil_p%num_int_points,              &
     &    node, ele, g_FEM, jac_3d_q, mom_nod, mom_ele)
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
      call dealloc_int_surf_data(rhs_mat%surf_wk)
      call dealloc_finite_elem_mat(rhs_mat)
      call dealloc_crs_mat_data(mass1)
      call dealloc_crs_connect(tbl_crs)
!
      end subroutine release_mass_mat_for_consist
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      end module cal_element_size
