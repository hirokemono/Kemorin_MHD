!initialization_4_MHD.f90
!     module initialization_4_MHD
!
!      Written by H. Matsui
!
!!      subroutine init_analyzer_fl(IO_bc, FEM_prm, SGS_par, MHD_step,  &
!!     &          mesh, group, ele_mesh, MHD_mesh, layer_tbl,           &
!!     &          iphys, nod_fld, label_sim)
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(mesh_data_MHD), intent(inout) :: MHD_mesh
!!        type(layering_tbl), intent(inout) :: layer_tbl
!!        type(layering_tbl), intent(inout) :: layer_tbl
!!        type(phys_address), intent(inout) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!
      module initialization_4_MHD
!
      use m_precision
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_MHD_step_parameter
      use t_phys_data
      use t_phys_address
!
      use t_mesh_data
      use t_geometry_data_MHD
      use t_layering_ele_list
      use t_work_layer_correlate
      use t_boundary_field_IO
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer_fl(IO_bc, FEM_prm, SGS_par, MHD_step,    &
     &          time_d, mesh, group, ele_mesh, MHD_mesh, layer_tbl,     &
     &          iphys, nod_fld, label_sim)
!
      use calypso_mpi
      use m_machine_parameter
      use m_physical_property
      use m_flexible_time_step
!
      use m_finite_element_matrix
      use m_ele_material_property
      use m_mean_square_values
      use m_work_4_dynamic_model
      use m_boundary_condition_IDs
      use m_flags_4_solvers
      use m_array_for_send_recv
      use m_solver_djds_MHD
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
      use m_3d_filter_coef_MHD
      use m_SGS_model_coefs
!
      use m_cal_max_indices
      use m_surf_data_list
      use m_bc_data_velo
!
      use count_whole_num_element
!
      use init_iccg_matrices
      use copy_nodal_fields
      use cal_volume_node_MHD
      use int_MHD_mass_matrices
      use int_surface_params_MHD
      use set_dynamo_initial_field
      use set_nodal_bc_id_data
      use allocate_array_MHD
      use ordering_line_filter_smp
      use const_ele_layering_table
      use estimate_stabilities
      use const_comm_table_fluid
      use const_bc_infty_surf_type
      use set_reference_value
      use material_property
      use reordering_by_layers
      use set_layers_4_MHD
      use set_istart_3d_filtering
      use count_sgs_components
      use init_sgs_diff_coefs
      use set_normal_vectors
      use set_layer_list_by_table
      use reordering_MG_ele_by_layers
      use initialize_4_MHD_AMG
      use const_jacobians_sf_grp
      use const_mesh_information
      use const_element_comm_tables
      use init_check_delta_t_data
      use precond_djds_MHD
!
      use nod_phys_send_recv
      use solver_MGCG_MHD
!
      type(IO_boundary), intent(in) :: IO_bc
!
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(MHD_step_param), intent(inout) :: MHD_step
      type(time_data), intent(inout) :: time_d
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
      type(layering_tbl), intent(inout) :: layer_tbl
      type(phys_address), intent(inout) :: iphys
      type(phys_data), intent(inout) :: nod_fld
      character(len=kchara), intent(inout)   :: label_sim
!
      integer(kind = kint) :: iflag
!
!     --------------------------------
!       set up for mesh information
!     --------------------------------
!
!  -----   ordering by regions ---------------------------------------
!
      call reordering_by_layers_MHD                                     &
     &   (SGS_par, MGCG_WK1, MGCG_FEM1, MGCG_MHD_FEM1, FEM_prm,         &
     &    mesh%ele, group, MHD_mesh, MHD1_matrices%MG_interpolate)
!
      call set_layers                                                   &
     &   (FEM_prm, mesh%node, mesh%ele, group%ele_grp, MHD_mesh)
!
      if (SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        call const_layers_4_dynamic(group%ele_grp, layer_tbl)
        call alloc_work_4_dynamic(layer_tbl%e_grp%num_grp, wk_lsq1)
        call alloc_work_layer_correlate                                 &
     &     (layer_tbl%e_grp%num_grp, inine, wk_cor1)
      end if
!
!     ---------------------
!
      if (iflag_debug.ge.1 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(n_sym_tensor, mesh%node%numnod)
!
      call init_send_recv(mesh%nod_comm)
!
!  -----    construct geometry informations
!
      if (iflag_debug .gt. 0) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank, mesh, group, ele_mesh)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbls'
      call const_element_comm_tbls(mesh, ele_mesh)
!
      if(i_debug .eq. iflag_full_msg) then
        call check_whole_num_of_elements(mesh%ele)
      end if
!
      call deallocate_surface_geom_type(ele_mesh%surf)
      call deallocate_edge_geom_type(ele_mesh%edge)
!
!     ---------------------
!
      iflag = SGS_par%filter_p%iflag_SGS_filter
      if(    (SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF     &
     &   .or. SGS_par%model_p%iflag_SGS.eq.id_SGS_similarity)) then
!
        if    (iflag .eq. id_SGS_3D_FILTERING                           &
     &    .or. iflag .eq. id_SGS_3D_EZ_FILTERING) then
          if (iflag_debug .gt. 0)                                       &
     &      write(*,*) ' s_set_istart_3d_filtering'
          call s_set_istart_3d_filtering(filtering1%filter)
!
        else if (iflag .eq. id_SGS_3D_SMP_FILTERING                     &
     &      .or. iflag .eq. id_SGS_3D_EZ_SMP_FILTERING) then
          if (iflag_debug .gt. 0)                                       &
     &      write(*,*) ' const_tbl_3d_filtering_smp'
          call const_tbl_3d_filtering_smp(filtering1)
!
        else if (iflag .eq. id_SGS_LINE_FILTERING) then
          if (iflag_debug.gt.0) write(*,*)' ordering_l_filter_smp'
          call ordering_l_filter_smp(mesh%node%istack_nod_smp,          &
     &        filtering1%fil_l, filtering1%fil_l_smp)
        end if
      end if
!
      if(    (SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF     &
     &  .and. SGS_par%model_p%iflag_SGS.eq.id_SGS_similarity) ) then
        if    (iflag .eq. id_SGS_3D_FILTERING                           &
     &    .or. iflag .eq. id_SGS_3D_EZ_FILTERING) then
          if (iflag_debug .gt. 0) write(*,*)' s_set_istart_w_filtering'
          call s_set_istart_3d_filtering(wide_filtering%filter)
!
        else if (iflag.eq.id_SGS_3D_SMP_FILTERING                       &
     &     .or. iflag.eq.id_SGS_3D_EZ_SMP_FILTERING) then
!
          if (iflag_debug .gt. 0)                                       &
     &      write(*,*)' s_const_tbl_w_filtering_smp'
          call const_tbl_3d_filtering_smp(wide_filtering)
        end if
      end if
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*)' allocate_array'
      call allocate_array                                               &
     &   (SGS_par, mesh, MHD_prop1%fl_prop, MHD_prop1%cd_prop, MHD_prop1%ht_prop, MHD_prop1%cp_prop,        &
     &    iphys, nod_fld, iphys_elediff, mhd_fem1_wk, rhs_mat1,         &
     &    fem_int1, label_sim)
!
      if ( iflag_debug.ge.1 ) write(*,*) 'init_check_delta_t_data'
      call s_init_check_delta_t_data                                    &
     &  (MHD_prop1%cd_prop, iphys, flex_data)
!
      if (iflag_debug.eq.1) write(*,*)' set_reference_temp'
      call set_reference_temp                                           &
     &   (MHD_prop1%ref_param_T, MHD_prop1%takepito_T, mesh%node,       &
     &    MHD_mesh%fluid, iphys%i_ref_t, iphys%i_gref_t, nod_fld)
      call set_reference_temp                                           &
     &   (MHD_prop1%ref_param_C, MHD_prop1%takepito_C, mesh%node,       &
      &   MHD_mesh%fluid, iphys%i_ref_c, iphys%i_gref_c, nod_fld)
!
      if (iflag_debug.eq.1) write(*,*)' set_material_property'
      call set_material_property                                        &
     &   (iphys, MHD_prop1%ref_param_T%depth_top, MHD_prop1%ref_param_T%depth_bottom,     &
     &    MHD_prop1%fl_prop, MHD_prop1%cd_prop, MHD_prop1%ht_prop, MHD_prop1%cp_prop)
      call init_ele_material_property(mesh%ele%numele,                  &
     &    MHD_prop1%fl_prop, MHD_prop1%cd_prop, MHD_prop1%ht_prop, MHD_prop1%cp_prop)
      call define_sgs_components                                        &
     &   (mesh%node%numnod, mesh%ele%numele, SGS_par%model_p,           &
     &    layer_tbl, MHD_prop1%fl_prop, MHD_prop1%cd_prop, MHD_prop1%ht_prop, MHD_prop1%cp_prop,            &
     &    ifld_sgs, icomp_sgs, wk_sgs1, sgs_coefs, sgs_coefs_nod)
      call define_sgs_diff_coefs                                        &
     &   (mesh%ele%numele, SGS_par%model_p, SGS_par%commute_p,          &
     &    layer_tbl, MHD_prop1%fl_prop, MHD_prop1%cd_prop, MHD_prop1%ht_prop, MHD_prop1%cp_prop,            &
     &    ifld_diff, icomp_diff, wk_diff1, diff_coefs)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'copy_communicator_4_MHD'
      call copy_communicator_4_solver(solver_C)
!
      if (iflag_debug.eq.1) write(*,*) 'make comm. table for fluid'
      call s_const_comm_table_fluid                                     &
     &   (nprocs, MHD_mesh%fluid%istack_ele_fld_smp,                    &
     &    mesh%node, mesh%ele, mesh%nod_comm, DJDS_comm_fl)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'init_MGCG_MHD'
      call init_MGCG_MHD                                                &
     &   (FEM_prm, mesh%node, MHD_prop1%fl_prop, MHD_prop1%cd_prop)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*)' initial_data_control'
      call initial_data_control                                         &
     &   (MHD_step%rst_step, MHD_prop1%ref_param_T,                     &
     &    mesh%node, mesh%ele, MHD_mesh%fluid, MHD_prop1%cd_prop,       &
     &    iphys, layer_tbl, SGS_par, wk_sgs1, wk_diff1,                 &
     &    sgs_coefs, diff_coefs, nod_fld, flex_p1,                      &
     &    MHD_step%init_d, time_d)
      MHD_step%iflag_initial_step = 0
!
!  -------------------------------
!
      if(MHD_prop1%ref_param_T%iflag_reference                          &
     & .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1) write(*,*)' set_2_perturbation_temp'
        call subtract_2_nod_scalars(nod_fld,                            &
     &      iphys%i_temp, iphys%i_ref_t, iphys%i_par_temp)
      end if
      if (MHD_prop1%ref_param_C%iflag_reference                         &
     & .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1) write(*,*)' set_2_perturbation_comp'
        call subtract_2_nod_scalars(nod_fld,                            &
     &      iphys%i_light, iphys%i_ref_c, iphys%i_par_light)
      end if
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*)  'const_bc_infinity_surf_grp'
      call const_bc_infinity_surf_grp                                   &
     &   (iflag_surf_infty, group%surf_grp, group%infty_grp)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'const_MHD_jacobian_and_volumes'
      call const_MHD_jacobian_and_volumes(SGS_par%model_p,              &
     &    ele_mesh, group, mesh, layer_tbl, fem_int1%jacobians,         &
     &    MHD_mesh)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_MHD_layerd_connectivity'
      call set_MHD_connectivities                                       &
     &   (FEM_prm%DJDS_param, mesh, MHD_mesh%fluid,                     &
     &    fem_int1%next_tbl, fem_int1%rhs_tbl)
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*)  'const_normal_vector'
      call const_normal_vector(my_rank, nprocs,                         &
     &    mesh%node, ele_mesh%surf, fem_int1%jacobians)
!
      if (iflag_debug.eq.1) write(*,*)  'int_surface_parameters'
      call int_surface_parameters                                       &
     &   (mesh, ele_mesh%surf, group, rhs_mat1%surf_wk)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_data'
      call set_boundary_data                                            &
     &   (time_d, IO_bc, mesh, ele_mesh, MHD_mesh, group,               &
     &    MHD_prop1%fl_prop, MHD_prop1%cd_prop, MHD_prop1%ht_prop, MHD_prop1%cp_prop,                       &
     &    MHD_prop1%ref_param_T, MHD_prop1%ref_param_C, iphys, nod_fld)
!
!     ---------------------
!
      call int_RHS_mass_matrices(FEM_prm%npoint_t_evo_int,              &
     &    mesh, MHD_mesh, fem_int1%jacobians, fem_int1%rhs_tbl,         &
     &    mhd_fem1_wk, rhs_mat1%fem_wk, rhs_mat1%f_l, fem_int1%m_lump)
!
!     ---------------------
!
      if (iflag_debug.eq.1 ) write(*,*) 'allocate_aiccg_matrices'
      call allocate_aiccg_matrices(time_d%dt, mesh%node,                &
     &    MHD_prop1%fl_prop, MHD_prop1%cd_prop, MHD_prop1%ht_prop, MHD_prop1%cp_prop, FEM_prm)
!      call reset_aiccg_matrices(mesh%node, mesh%ele, MHD_mesh%fluid)
!
      if(solver_iflag(FEM_PRM%CG11_param%METHOD) .eq. iflag_mgcg) then
        call s_initialize_4_MHD_AMG(time_d%dt, FEM_prm,                 &
     &      mesh%node, mesh%ele, ifld_diff, diff_coefs,                 &
     &      MHD_prop1%fl_prop, MHD_prop1%cd_prop, MHD_prop1%ht_prop, MHD_prop1%cp_prop, FEM_prm%DJDS_param, &
     &      MGCG_WK1, MGCG_FEM1, MGCG_MHD_FEM1, MHD1_matrices)
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'cal_stability_4_diffuse'
      call cal_stability_4_diffuse                                      &
     &   (time_d%dt, mesh%ele, MHD_prop1%fl_prop, MHD_prop1%cd_prop, MHD_prop1%ht_prop, MHD_prop1%cp_prop)
! 
      call deallocate_surf_bc_lists                                     &
     &   (MHD_prop1%fl_prop, MHD_prop1%cd_prop, MHD_prop1%ht_prop, MHD_prop1%cp_prop)
!
      end subroutine init_analyzer_fl
!
! ----------------------------------------------------------------------
!
      end module initialization_4_MHD
