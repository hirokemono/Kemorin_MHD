!initialization_4_MHD.f90
!     module initialization_4_MHD
!
!      Written by H. Matsui
!
!!      subroutine init_analyzer_fl(MHD_files, IO_bc, FEM_prm, SGS_par, &
!!     &          flex_p, flex_data, MHD_step, mesh, group, ele_mesh,   &
!!     &          MHD_mesh, layer_tbl, MHD_prop, ak_MHD, Csims_FEM_MHD, &
!!     &          iphys, nod_fld, MHD_CG, fem_sq, label_sim)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(flexible_stepping_parameter), intent(inout) :: flex_p
!!        type(flexible_stepping_data), intent(inout) :: flex_data
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(mesh_data_MHD), intent(inout) :: MHD_mesh
!!        type(layering_tbl), intent(inout) :: layer_tbl
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!!        type(phys_address), intent(inout) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(FEM_MHD_solvers), intent(inout) :: MHD_CG
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      module initialization_4_MHD
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_parameter
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
      use t_MHD_file_parameter
      use t_boundary_field_IO
      use t_FEM_SGS_model_coefs
      use t_material_property
      use t_flex_delta_t_data
      use t_FEM_MHD_mean_square
      use t_FEM_MHD_solvers
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer_fl(MHD_files, IO_bc, FEM_prm, SGS_par,   &
     &          flex_p, flex_data, MHD_step, mesh, group, ele_mesh,     &
     &          MHD_mesh, layer_tbl, MHD_prop, ak_MHD, Csims_FEM_MHD,   &
     &          iphys, nod_fld, MHD_CG, fem_sq, label_sim)
!
      use m_flexible_time_step
!
      use m_finite_element_matrix
      use m_work_4_dynamic_model
      use m_boundary_condition_IDs
      use m_flags_4_solvers
      use m_array_for_send_recv
      use m_3d_filter_coef_MHD
      use t_work_4_MHD_layering
!
      use m_bc_data_velo
      use m_bc_data_list
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
      use init_ele_material_property
      use precond_djds_MHD
      use reordering_by_layers
!
      use nod_phys_send_recv
      use solver_MGCG_MHD
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(IO_boundary), intent(in) :: IO_bc
!
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(flexible_stepping_parameter), intent(inout) :: flex_p
      type(flexible_stepping_data), intent(inout) :: flex_data
      type(MHD_step_param), intent(inout) :: MHD_step
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
      type(layering_tbl), intent(inout) :: layer_tbl
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(coefs_4_MHD_type), intent(inout) :: ak_MHD
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(phys_address), intent(inout) :: iphys
      type(phys_data), intent(inout) :: nod_fld
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
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
      call reordering_by_layers_MHD(SGS_par, MHD_CG%MGCG_WK,            &
     &    MHD_CG%MGCG_FEM, MHD_CG%MGCG_MHD_FEM, FEM_prm,                &
     &    mesh%ele, group, MHD_mesh, MHD_CG%MHD_mat%MG_interpolate)
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
      call init_nod_send_recv(mesh)
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
      call allocate_array(SGS_par, mesh, MHD_prop, iphys,               &
     &    nod_fld, Csims_FEM_MHD%iphys_elediff, mk_MHD1,                &
     &    mhd_fem1_wk, rhs_mat1, fem_int1, fem_sq, label_sim)
!
      if ( iflag_debug.ge.1 ) write(*,*) 'init_check_delta_t_data'
      call s_init_check_delta_t_data                                    &
     &  (MHD_prop%cd_prop, iphys, flex_data)
!
      if (iflag_debug.eq.1) write(*,*)' set_reference_temp'
      call set_reference_temp                                           &
     &   (MHD_prop%ref_param_T, MHD_prop%takepito_T, mesh%node,         &
     &    MHD_mesh%fluid, iphys%i_ref_t, iphys%i_gref_t, nod_fld)
      call set_reference_temp                                           &
     &   (MHD_prop%ref_param_C, MHD_prop%takepito_C, mesh%node,         &
      &   MHD_mesh%fluid, iphys%i_ref_c, iphys%i_gref_c, nod_fld)
!
      if (iflag_debug.eq.1) write(*,*)' set_material_property'
      call set_material_property                                        &
     &   (iphys, MHD_prop%ref_param_T%depth_top,                        &
     &    MHD_prop%ref_param_T%depth_bottom, MHD_prop)
      call s_init_ele_material_property                                 &
     &   (mesh%ele%numele, MHD_prop, ak_MHD)
!
      call define_sgs_components                                        &
     &   (mesh%node%numnod, mesh%ele%numele,                            &
     &    SGS_par%model_p, layer_tbl, MHD_prop,                         &
     &    Csims_FEM_MHD%ifld_sgs, Csims_FEM_MHD%icomp_sgs, wk_sgs1,     &
     &    Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod)
      call define_sgs_diff_coefs(mesh%ele%numele,                       &
     &    SGS_par%model_p, SGS_par%commute_p, layer_tbl, MHD_prop,      &
     &    Csims_FEM_MHD%ifld_diff, Csims_FEM_MHD%icomp_diff,            &
     &    wk_diff1, Csims_FEM_MHD%diff_coefs)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'copy_communicator_4_MHD'
      call copy_communicator_4_solver(MHD_CG%solver_C)
!
      if (iflag_debug.eq.1) write(*,*) 'make comm. table for fluid'
      call s_const_comm_table_fluid                                     &
     &   (nprocs, MHD_mesh%fluid%istack_ele_fld_smp,                    &
     &    mesh%node, mesh%ele, mesh%nod_comm, MHD_CG%DJDS_comm_fl)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'init_MGCG_MHD'
      call init_MGCG_MHD                                                &
     &   (FEM_prm, mesh%node, MHD_prop%fl_prop, MHD_prop%cd_prop)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*)' initial_data_control'
      call initial_data_control                                         &
     &   (MHD_files, MHD_step%rst_step, MHD_prop%ref_param_T,           &
     &    mesh%node, mesh%ele, MHD_mesh%fluid, MHD_prop%cd_prop,        &
     &    iphys, layer_tbl, SGS_par, wk_sgs1, wk_diff1,                 &
     &    Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%diff_coefs,            &
     &    nod_fld, flex_p, MHD_step%init_d, MHD_step%time_d)
      MHD_step%iflag_initial_step = 0
!
!  -------------------------------
!
      if(MHD_prop%ref_param_T%iflag_reference                           &
     & .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1) write(*,*)' set_2_perturbation_temp'
        call subtract_2_nod_scalars(nod_fld,                            &
     &      iphys%i_temp, iphys%i_ref_t, iphys%i_par_temp)
      end if
      if (MHD_prop%ref_param_C%iflag_reference                          &
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
      call const_MHD_jacobian_and_volumes(SGS_par%model_p, ele_mesh,    &
     &    group, fem_sq%i_msq, mesh, layer_tbl, fem_int1%jcs,           &
     &    MHD_mesh, fem_sq%msq)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_MHD_layerd_connectivity'
      call set_MHD_connectivities                                       &
     &   (FEM_prm%DJDS_param, mesh, MHD_mesh%fluid, MHD_CG%solver_C,    &
     &    fem_int1%next_tbl, fem_int1%rhs_tbl,                          &
     &    MHD_CG%MHD_mat, MHD_CG%DJDS_comm_fl)
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*)  'const_normal_vector'
      call const_normal_vector(my_rank, nprocs,                         &
     &    mesh%node, ele_mesh%surf, fem_int1%jcs)
!
      if (iflag_debug.eq.1) write(*,*)  'int_surface_parameters'
      call int_surface_parameters                                       &
     &   (mesh, ele_mesh%surf, group, rhs_mat1%surf_wk)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_data'
      call set_boundary_data                                            &
     &   (MHD_step%time_d, IO_bc, mesh, ele_mesh, MHD_mesh, group,      &
     &    MHD_prop, MHD_BC1, iphys, nod_fld)
!
!     ---------------------
!
      call int_RHS_mass_matrices(FEM_prm%npoint_t_evo_int,              &
     &    mesh, MHD_mesh, fem_int1%jcs, fem_int1%rhs_tbl,               &
     &    rhs_mat1%fem_wk, rhs_mat1%f_l, fem_int1%m_lump, mk_MHD1)
!
!     ---------------------
!
      call set_residual_4_crank(MHD_step%time_d%dt,                     &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, FEM_prm)
      if (iflag_debug.eq.1 ) write(*,*) 'alloc_MHD_MGCG_matrices'
      call alloc_MHD_MGCG_matrices                                      &
     &   (izero, mesh%node, MHD_prop, MHD_CG%MHD_mat)
!      call reset_aiccg_matrices(mesh%node, mesh%ele, MHD_mesh%fluid)
!
      if(solver_iflag(FEM_PRM%CG11_param%METHOD) .eq. iflag_mgcg) then
        call s_initialize_4_MHD_AMG(MHD_step%time_d%dt, FEM_prm, mesh,  &
     &      Csims_FEM_MHD%ifld_diff, Csims_FEM_MHD%diff_coefs,          &
     &      MHD_prop, MHD_BC1, FEM_prm%DJDS_param, MHD_CG%MGCG_WK,      &
     &      MHD_CG%MGCG_FEM, MHD_CG%MGCG_MHD_FEM, MHD_CG%MHD_mat)
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'cal_stability_4_diffuse'
      call cal_stability_4_diffuse                                      &
     &   (MHD_step%time_d%dt, mesh%ele, MHD_prop)
      call deallocate_surf_bc_lists(MHD_prop, MHD_BC1)
!
      end subroutine init_analyzer_fl
!
! ----------------------------------------------------------------------
!
      end module initialization_4_MHD
