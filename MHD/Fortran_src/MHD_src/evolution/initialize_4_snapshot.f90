!initialize_4_snapshot.f90
!     module initialize_4_snapshot
!
!      Written by H. Matsui
!
!!      subroutine init_analyzer_snap(MHD_files, FEM_prm, SGS_par,      &
!!     &          IO_bc, MHD_step, mesh, group, ele_mesh, MHD_mesh,     &
!!     &&          FEM_filters, MHD_prop, ak_MHD, MHD_BC, FEM_MHD_BCs,  &
!!     &          Csims_FEM_MHD, iphys, nod_fld, iphys_ele, ele_fld,    &
!!     &          t_IO, rst_step, fem_int, mk_MHD, SGS_MHD_wk,          &
!!     &          fem_sq, label_sim)
!!        type(field_IO_params), intent(in) :: fst_file_IO
!!        type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(IO_boundary), intent(in) :: IO_bc
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
!!        type(phys_address), intent(inout) :: iphys, iphys_ele
!!        type(phys_data), intent(inout) :: nod_fld, ele_fld
!!        type(time_data), intent(inout) :: t_IO
!!        type(IO_step_param), intent(inout) :: rst_step
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!        type(finite_element_integration), intent(inout) :: fem_int
!!        type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!
      module initialize_4_snapshot
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_parameter
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_MHD_step_parameter
      use t_time_data
      use t_phys_data
      use t_phys_address
!
      use t_mesh_data
      use t_geometry_data_MHD
      use t_work_layer_correlate
      use t_time_data
      use t_boundary_field_IO
      use t_IO_step_parameter
      use t_MHD_file_parameter
      use t_bc_data_list
      use t_FEM_MHD_boundary_data
      use t_FEM_SGS_model_coefs
      use t_material_property
      use t_FEM_MHD_mean_square
      use t_MHD_finite_element_mat
      use t_work_FEM_SGS_MHD
      use t_MHD_mass_matricxes
      use t_FEM_MHD_filter_data
      use t_work_4_MHD_layering
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer_snap(MHD_files, FEM_prm, SGS_par,        &
     &          IO_bc, MHD_step, mesh, group, ele_mesh, MHD_mesh,       &
     &          FEM_filters, MHD_prop, ak_MHD, MHD_BC, FEM_MHD_BCs,     &
     &          Csims_FEM_MHD, iphys, nod_fld, iphys_ele, ele_fld,      &
     &          t_IO, rst_step, fem_int, mk_MHD, SGS_MHD_wk,            &
     &          fem_sq, label_sim)
!
      use m_boundary_condition_IDs
      use m_array_for_send_recv
      use m_fem_mhd_restart
      use m_fem_gauss_int_coefs
!
      use cal_volume_node_MHD
      use int_MHD_mass_matrices
      use int_surface_params_MHD
      use set_nodal_bc_id_data
      use allocate_array_MHD
      use ordering_line_filter_smp
      use const_ele_layering_table
      use const_comm_table_fluid
      use const_bc_infty_surf_type
      use set_reference_value
      use material_property
      use set_layers_4_MHD
      use set_istart_3d_filtering
      use count_sgs_components
      use init_sgs_diff_coefs
      use set_layer_list_by_table
      use set_normal_vectors
      use set_table_4_RHS_assemble
      use const_jacobians_sf_grp
      use parallel_FEM_mesh_init
      use init_ele_material_property
      use reordering_by_layers
!
      use nod_phys_send_recv
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(SGS_paremeters), intent(in) :: SGS_par
      type(IO_boundary), intent(in) :: IO_bc
      type(MHD_step_param), intent(in) :: MHD_step
!
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
      type(filters_on_FEM), intent(inout) :: FEM_filters
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(coefs_4_MHD_type), intent(inout) :: ak_MHD
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(FEM_MHD_BC_data), intent(inout) :: FEM_MHD_BCs
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(phys_address), intent(inout) :: iphys, iphys_ele
      type(phys_data), intent(inout) :: nod_fld, ele_fld
      type(time_data), intent(inout) :: t_IO
      type(IO_step_param), intent(inout) :: rst_step
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(finite_element_integration), intent(inout) :: fem_int
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      character(len=kchara), intent(inout)   :: label_sim
!
      type(shape_finctions_at_points), save :: spfs_1
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*)' reordering_by_layers_snap'
      call reordering_by_layers_snap(FEM_prm, SGS_par,                  &
     &    mesh%ele, group, MHD_mesh, FEM_filters%FEM_elens)
!
      if (iflag_debug.eq.1) write(*,*)' set_layers'
      call set_layers                                                   &
     &   (FEM_prm, mesh%node, mesh%ele, group%ele_grp, MHD_mesh)
!
      if (SGS_par%model_p%iflag_dynamic  .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*)' const_layers_4_dynamic'
        call const_layers_4_dynamic                                     &
      &    (group%ele_grp, FEM_filters%layer_tbl)
        call alloc_work_FEM_dynamic                                     &
      &    (FEM_filters%layer_tbl, SGS_MHD_wk%FEM_SGS_wk)
      end if
!
!
!     ---------------------
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(mesh, group, ele_mesh)
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
     &    iphys, nod_fld, iphys_ele, ele_fld,                           &
     &    Csims_FEM_MHD%iphys_elediff, mk_MHD,                          &
     &    SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, fem_int,           &
     &    fem_sq, label_sim)
!
      if (iflag_debug.eq.1) write(*,*)' set_reference_temp'
      call set_reference_temp                                           &
     &   (MHD_prop%ref_param_T, MHD_prop%takepito_T, mesh%node,         &
     &    MHD_mesh%fluid, iphys%i_ref_t, iphys%i_gref_t, nod_fld)
      call set_reference_temp                                           &
     &   (MHD_prop%ref_param_C, MHD_prop%takepito_C, mesh%node,         &
     &    MHD_mesh%fluid, iphys%i_ref_c, iphys%i_gref_c, nod_fld)
!
      if (iflag_debug.eq.1) write(*,*)' set_material_property'
      call set_material_property                                        &
     &   (iphys, MHD_prop%ref_param_T%depth_top,                        &
     &    MHD_prop%ref_param_T%depth_bottom, MHD_prop)
      call s_init_ele_material_property                                 &
     &   (mesh%ele%numele, MHD_prop, ak_MHD)
!
      call def_sgs_commute_component                                    &
     &   (SGS_par, mesh, FEM_filters%layer_tbl,                         &
     &    MHD_prop, Csims_FEM_MHD, SGS_MHD_wk%FEM_SGS_wk)
!
      call deallocate_surface_geom_type(ele_mesh%surf)
      call dealloc_edge_geometory(ele_mesh%edge)
!
!     --------------------- 
!
      if (rst_step%increment .gt. 0) then
        if (iflag_debug.eq.1) write(*,*)' init_restart_4_snapshot'
        call init_restart_4_snapshot(MHD_step%init_d%i_time_step,       &
     &      MHD_files%fst_file_IO, mesh%node, t_IO, rst_step)
      end if
!
!     ---------------------
!
      call const_bc_infinity_surf_grp                                   &
     &   (iflag_surf_infty, group%surf_grp, group%infty_grp)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*)' const_MHD_jacobian_and_volumes'
      call const_MHD_jacobian_and_volumes(SGS_par%model_p, ele_mesh,    &
     &    group, fem_sq%i_msq, mesh, FEM_filters%layer_tbl,             &
     &    spfs_1, fem_int%jcs, MHD_mesh, fem_sq%msq)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*)' set_connect_RHS_assemble'
      call s_set_RHS_assemble_table                                     &
     &   (mesh, fem_int%next_tbl, fem_int%rhs_tbl)
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*)  'const_normal_vector'
      call const_normal_vector                                          &
     &   (my_rank, nprocs, mesh%node, g_FEM1, ele_mesh%surf,            &
     &    spfs_1%spf_2d, fem_int%jcs)
      call dealloc_surf_shape_func(spfs_1%spf_2d)
!
      if (iflag_debug.eq.1) write(*,*)' int_surface_parameters'
      call int_surface_parameters                                       &
     &   (mesh, ele_mesh%surf, group, SGS_MHD_wk%rhs_mat%surf_wk)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*)' set_boundary_data'
      call set_boundary_data                                            &
     &   (MHD_step%time_d, IO_bc, mesh, ele_mesh, MHD_mesh, group,      &
     &    MHD_prop, MHD_BC, iphys, nod_fld, FEM_MHD_BCs)
!
!     ---------------------
!
      call int_RHS_mass_matrices(FEM_prm%npoint_t_evo_int,              &
     &     mesh, MHD_mesh, fem_int%jcs, fem_int%rhs_tbl,                &
     &     SGS_MHD_wk%rhs_mat%fem_wk, SGS_MHD_wk%rhs_mat%f_l,           &
     &     fem_int%m_lump, mk_MHD)
!
!     ---------------------
!
      call deallocate_surf_bc_lists(MHD_prop, MHD_BC)
!
      end subroutine init_analyzer_snap
!
! ----------------------------------------------------------------------
!
      end module initialize_4_snapshot
