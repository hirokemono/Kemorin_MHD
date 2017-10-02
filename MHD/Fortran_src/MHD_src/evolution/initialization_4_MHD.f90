!initialization_4_MHD.f90
!     module initialization_4_MHD
!
!      Written by H. Matsui
!
!!      subroutine init_analyzer_fl                                     &
!!     &        (MHD_files, IO_bc, FEM_prm, SGS_par, flex_MHD,          &
!!     &         MHD_step, mesh, group, ele_mesh, MHD_mesh, FEM_filters,&
!!     &         MHD_prop, MHD_BC, FEM_MHD_BCs, Csims_FEM_MHD,          &
!!     &         iphys, nod_fld, MHD_CG, SGS_MHD_wk, fem_sq, fem_fst_IO,&
!!     &         label_sim)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(FEM_MHD_time_stepping), intent(inout) :: flex_MHD
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(mesh_data_MHD), intent(inout) :: MHD_mesh
!!        type(filters_on_FEM), intent(inout) :: FEM_filters
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(MHD_BC_lists), intent(inout) :: MHD_BC
!!        type(FEM_MHD_BC_data), intent(inout) :: FEM_MHD_BCs
!!        type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!!        type(phys_address), intent(inout) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!        type(FEM_MHD_solvers), intent(inout) :: MHD_CG
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(field_IO), intent(inout) :: fem_fst_IO
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
      use t_MHD_file_parameter
      use t_boundary_field_IO
      use t_FEM_SGS_model_coefs
      use t_material_property
      use t_FEM_MHD_time_stepping
      use t_FEM_MHD_mean_square
      use t_FEM_MHD_solvers
      use t_work_FEM_dynamic_SGS
      use t_work_FEM_SGS_MHD
      use t_MHD_mass_matrices
      use t_FEM_MHD_filter_data
      use t_FEM_MHD_boundary_data
      use t_work_4_MHD_layering
      use t_bc_data_list
      use t_field_data_IO
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer_fl                                       &
     &        (MHD_files, IO_bc, FEM_prm, SGS_par, flex_MHD,            &
     &         MHD_step, mesh, group, ele_mesh, MHD_mesh, FEM_filters,  &
     &         MHD_prop, MHD_BC, FEM_MHD_BCs, Csims_FEM_MHD,            &
     &         iphys, nod_fld, MHD_CG, SGS_MHD_wk, fem_sq, fem_fst_IO,  &
     &         label_sim)
!
      use m_boundary_condition_IDs
      use m_flags_4_solvers
      use m_array_for_send_recv
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
      use parallel_FEM_mesh_init
      use init_check_delta_t_data
      use init_ele_material_property
      use precond_djds_MHD
      use reordering_by_layers
      use FEM_MHD_evolution
!
      use nod_phys_send_recv
      use solver_MGCG_MHD
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(IO_boundary), intent(in) :: IO_bc
!
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(FEM_MHD_time_stepping), intent(inout) :: flex_MHD
      type(MHD_step_param), intent(inout) :: MHD_step
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
      type(filters_on_FEM), intent(inout) :: FEM_filters
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(FEM_MHD_BC_data), intent(inout) :: FEM_MHD_BCs
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(phys_address), intent(inout) :: iphys
      type(phys_data), intent(inout) :: nod_fld
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(field_IO), intent(inout) :: fem_fst_IO
      character(len=kchara), intent(inout)   :: label_sim
!
      type(shape_finctions_at_points), save :: spfs_1
!
!     --------------------------------
!       set up for mesh information
!     --------------------------------
!
!  -----   ordering by regions ---------------------------------------
!
      call reordering_by_layers_MHD(SGS_par, MHD_CG%MGCG_WK,            &
     &    MHD_CG%MGCG_FEM, MHD_CG%MGCG_MHD_FEM, FEM_prm,                &
     &    mesh%ele, group, MHD_mesh, MHD_CG%MHD_mat%MG_interpolate,     &
     &    FEM_filters%FEM_elens)
!
      call set_layers                                                   &
     &   (FEM_prm, mesh%node, mesh%ele, group%ele_grp, MHD_mesh)
!
      if (SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        call const_layers_4_dynamic                                     &
     &     (group%ele_grp, FEM_filters%layer_tbl)
        call alloc_work_FEM_dynamic                                     &
     &     (FEM_filters%layer_tbl, SGS_MHD_wk%FEM_SGS_wk)
      end if
!
!     ---------------------
!
      call FEM_mesh_initialization(mesh, group, ele_mesh)
!
      call deallocate_surface_geom_type(ele_mesh%surf)
      call dealloc_edge_geometory(ele_mesh%edge)
!
!     ---------------------
!
      call const_FEM_3d_filtering_tables(SGS_par, mesh,                 &
     &    FEM_filters%filtering, FEM_filters%wide_filtering)
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*)' allocate_array'
      call allocate_array(SGS_par, mesh, MHD_prop,                      &
     &    iphys, nod_fld, SGS_MHD_wk%iphys_ele, SGS_MHD_wk%ele_fld,     &
     &    Csims_FEM_MHD%iphys_elediff, SGS_MHD_wk%mk_MHD,               &
     &    SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat,                    &
     &    SGS_MHD_wk%fem_int, fem_sq, label_sim)
!
      if ( iflag_debug.ge.1 ) write(*,*) 'init_check_delta_t_data'
      call s_init_check_delta_t_data                                    &
     &  (MHD_prop%cd_prop, iphys, flex_MHD%flex_data)
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
     &   (mesh%ele%numele, MHD_prop, MHD_CG%ak_MHD)
!
      call def_sgs_commute_component                                    &
     &   (SGS_par, mesh, FEM_filters%layer_tbl, MHD_prop,               &
     &    Csims_FEM_MHD, SGS_MHD_wk%FEM_SGS_wk)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'copy_communicator_4_MHD'
      call copy_communicator_4_solver(MHD_CG%solver_C)
!
      if (iflag_debug.eq.1) write(*,*) 'make comm. table for fluid'
      call set_const_comm_table_fluid                                   &
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
      call initial_data_control(MHD_files, MHD_step%rst_step,           &
     &    MHD_prop%ref_param_T, mesh%node, mesh%ele,                    &
     &    MHD_mesh%fluid, MHD_prop%cd_prop, iphys,                      &
     &    FEM_filters%layer_tbl, SGS_par, SGS_MHD_wk%FEM_SGS_wk,        &
     &    Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%diff_coefs,            &
     &    nod_fld, MHD_step%flex_p, MHD_step%init_d, MHD_step%time_d,   &
     &    fem_fst_IO)
      MHD_step%iflag_initial_step = 0
!
!  -------------------------------
!
      call set_perturbation_to_scalar(MHD_prop, iphys, nod_fld)
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
     &    group, fem_sq%i_msq, mesh, FEM_filters%layer_tbl,             &
     &    spfs_1, SGS_MHD_wk%fem_int%jcs, MHD_mesh, fem_sq%msq)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_MHD_layerd_connectivity'
      call set_MHD_connectivities                                       &
     &   (FEM_prm%DJDS_param, mesh, MHD_mesh%fluid, MHD_CG%solver_C,    &
     &    SGS_MHD_wk%fem_int, MHD_CG%MHD_mat, MHD_CG%DJDS_comm_fl)
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*)  'const_normal_vector'
      call const_normal_vector                                          &
     &   (my_rank, nprocs, mesh%node, ele_mesh%surf,                    &
     &    spfs_1%spf_2d, SGS_MHD_wk%fem_int%jcs)
      call dealloc_surf_shape_func(spfs_1%spf_2d)
!
      if (iflag_debug.eq.1) write(*,*)  'int_surface_parameters'
      call int_surface_parameters                                       &
     &   (mesh, ele_mesh%surf, group, SGS_MHD_wk%rhs_mat%surf_wk)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_boundary_data'
      call set_boundary_data                                            &
     &   (MHD_step%time_d, IO_bc, mesh, ele_mesh, MHD_mesh, group,      &
     &    MHD_prop, MHD_BC, iphys, nod_fld, FEM_MHD_BCs)
!
!     ---------------------
!
      call int_RHS_mass_matrices                                        &
     &   (FEM_prm%npoint_t_evo_int, mesh, MHD_mesh,                     &
     &    SGS_MHD_wk%rhs_mat%fem_wk, SGS_MHD_wk%rhs_mat%f_l,            &
     &    SGS_MHD_wk%fem_int, SGS_MHD_wk%mk_MHD)
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
        call s_initialize_4_MHD_AMG                                     &
     &     (MHD_step%time_d%dt, FEM_prm, mesh, SGS_MHD_wk%fem_int%jcs,  &
     &      Csims_FEM_MHD%ifld_diff, Csims_FEM_MHD%diff_coefs,          &
     &      MHD_prop, MHD_BC, FEM_prm%DJDS_param, spfs_1,               &
     &      MHD_CG%MGCG_WK, MHD_CG%MGCG_FEM,                            &
     &      MHD_CG%MGCG_MHD_FEM, MHD_CG%MHD_mat)
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'cal_stability_4_diffuse'
      call cal_stability_4_diffuse                                      &
     &   (MHD_step%time_d%dt, mesh%ele, MHD_prop)
      call deallocate_surf_bc_lists(MHD_prop, MHD_BC)
!
      end subroutine init_analyzer_fl
!
! ----------------------------------------------------------------------
!
      end module initialization_4_MHD
