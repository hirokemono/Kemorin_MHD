!initialize_4_snapshot.f90
!     module initialize_4_snapshot
!
!      Written by H. Matsui
!
!!      subroutine init_analyzer_snap(MHD_files, FEM_prm, SGS_par,      &
!!     &          IO_bc, MHD_step, geofem, MHD_mesh, FEM_filters,       &
!!     &          MHD_prop, ak_MHD, MHD_BC, FEM_MHD_BCs, Csims_FEM_MHD, &
!!     &          iref_base, iref_grad, ref_fld, iphys, iphys_LES,      &
!!     &          nod_fld, t_IO, rst_step, SGS_MHD_wk, fem_sq,          &
!!     &          fem_fst_IO, m_SR, label_sim)
!!        type(field_IO_params), intent(in) :: fst_file_IO
!!        type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(mesh_data), intent(inout) :: geofem
!!        type(mesh_data_MHD), intent(inout) :: MHD_mesh
!!        type(filters_on_FEM), intent(inout) :: FEM_filters
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(MHD_BC_lists), intent(inout) :: MHD_BC
!!        type(FEM_MHD_BC_data), intent(inout) :: FEM_MHD_BCs
!!        type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!!        type(base_field_address), intent(inout) :: iref_base
!!        type(gradient_field_address), intent(inout) :: iref_grad
!!        type(phys_address), intent(inout) :: iphys
!!        type(SGS_model_addresses), intent(inout) :: iphys_LES
!!        type(phys_data), intent(inout) :: ref_fld
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(time_data), intent(inout) :: t_IO
!!        type(IO_step_param), intent(inout) :: rst_step
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(field_IO), intent(inout) :: fem_fst_IO
!!        type(mesh_SR), intent(inout) :: m_SR
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
      use t_base_force_labels
      use t_grad_field_labels
      use t_SGS_model_addresses
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
      use t_MHD_mass_matrices
      use t_FEM_MHD_filter_data
      use t_work_4_MHD_layering
      use t_field_data_IO
      use t_mesh_SR
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
     &          IO_bc, MHD_step, geofem, MHD_mesh, FEM_filters,         &
     &          MHD_prop, ak_MHD, MHD_BC, FEM_MHD_BCs, Csims_FEM_MHD,   &
     &          iref_base, iref_grad, ref_fld, iphys, iphys_LES,        &
     &          nod_fld, t_IO, rst_step, SGS_MHD_wk, fem_sq,            &
     &          fem_fst_IO, m_SR, label_sim)
!
      use m_boundary_condition_IDs
      use m_fem_mhd_restart
!
      use cal_volume_node_MHD
      use int_MHD_mass_matrices
      use int_surface_params_MHD
      use set_nodal_bc_id_data
      use allocate_array_MHD
      use ordering_line_filter_smp
      use const_ele_layering_table
      use const_comm_table_fluid
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
      type(mesh_data), intent(inout) :: geofem
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
      type(filters_on_FEM), intent(inout) :: FEM_filters
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(coefs_4_MHD_type), intent(inout) :: ak_MHD
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(FEM_MHD_BC_data), intent(inout) :: FEM_MHD_BCs
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(base_field_address), intent(inout) :: iref_base
      type(gradient_field_address), intent(inout) :: iref_grad
      type(phys_address), intent(inout) :: iphys
      type(SGS_model_addresses), intent(inout) :: iphys_LES
      type(phys_data), intent(inout) :: ref_fld
      type(phys_data), intent(inout) :: nod_fld
      type(time_data), intent(inout) :: t_IO
      type(IO_step_param), intent(inout) :: rst_step
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(field_IO), intent(inout) :: fem_fst_IO
      type(mesh_SR), intent(inout) :: m_SR
      character(len=kchara), intent(inout)   :: label_sim
!
      type(shape_finctions_at_points), save :: spfs_1
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*)' reordering_by_layers_snap'
      call reordering_by_layers_snap                                    &
     &   (FEM_prm, SGS_par, geofem%mesh%ele, geofem%group, MHD_mesh,    &
     &    FEM_filters%FEM_elens)
!
      if (iflag_debug.eq.1) write(*,*)' set_layers'
      call set_layers(FEM_prm, geofem%mesh%node, geofem%mesh%ele,       &
     &                geofem%group%ele_grp, MHD_mesh)
!
      if (SGS_par%model_p%iflag_dynamic  .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*)' const_layers_4_dynamic'
        call const_layers_4_dynamic                                     &
      &    (geofem%group%ele_grp, FEM_filters%layer_tbl)
        call alloc_work_FEM_dynamic                                     &
      &    (FEM_filters%layer_tbl, SGS_MHD_wk%FEM_SGS_wk)
      end if
!
!     ---------------------
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_comm_initialization(geofem%mesh, m_SR)
      call FEM_mesh_initialization(geofem%mesh, geofem%group,           &
     &                             m_SR%SR_sig, m_SR%SR_i)
!
!     ---------------------
!
      call const_FEM_3d_filtering_tables(SGS_par, geofem%mesh,          &
     &    FEM_filters%filtering, FEM_filters%wide_filtering)
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*)' allocate_array_FEM_MHD'
      call allocate_array_FEM_MHD(SGS_par, geofem%mesh, MHD_prop,       &
     &    iphys, iphys_LES, nod_fld, iref_base, iref_grad, ref_fld,     &
     &    Csims_FEM_MHD, SGS_MHD_wk, fem_sq, label_sim)
!
      if (iflag_debug.eq.1) write(*,*)' set_reference_temp'
      call set_reference_temp                                           &
     &   (MHD_prop%ref_param_T, MHD_prop%takepito_T,                    &
     &    geofem%mesh%node, MHD_mesh%fluid,                             &
     &    iref_base%i_temp, iref_grad%i_grad_temp, ref_fld)
      call set_reference_temp                                           &
     &   (MHD_prop%ref_param_C, MHD_prop%takepito_C,                    &
     &    geofem%mesh%node, MHD_mesh%fluid, iref_base%i_light,          &
     &    iref_grad%i_grad_composit, ref_fld)
!
      if (iflag_debug.eq.1) write(*,*)' set_material_property'
      call set_material_property                                        &
     &   (MHD_prop%ref_param_T%depth_top,                               &
     &    MHD_prop%ref_param_T%depth_bottom, iphys, MHD_prop)
      call s_init_ele_material_property                                 &
     &   (geofem%mesh%ele%numele, MHD_prop, ak_MHD)
!
      call def_sgs_commute_component                                    &
     &   (SGS_par, geofem%mesh, FEM_filters%layer_tbl,                  &
     &    MHD_prop, Csims_FEM_MHD, SGS_MHD_wk%FEM_SGS_wk)
!
!     --------------------- 
!
      if (rst_step%increment .gt. 0) then
        if (iflag_debug.eq.1) write(*,*)' init_restart_4_snapshot'
        call init_restart_4_snapshot(MHD_step%init_d%i_time_step,       &
     &      MHD_files%fst_file_IO, geofem%mesh%node,                    &
     &      t_IO, rst_step, fem_fst_IO)
      end if
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*)' set_connect_RHS_assemble'
      call s_set_RHS_assemble_table                                     &
     &   (geofem%mesh, SGS_MHD_wk%fem_int%next_tbl,                     &
     &    SGS_MHD_wk%fem_int%rhs_tbl)
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*)' const_MHD_jacobian_and_volumes'
      call const_MHD_jacobian_and_volumes(SGS_par%model_p,              &
     &    fem_sq%i_msq, MHD_BC, geofem%mesh, geofem%group,              &
     &    FEM_filters%layer_tbl, spfs_1, SGS_MHD_wk%fem_int%jcs,        &
     &    MHD_mesh, fem_sq%msq)
!
      if (iflag_debug.eq.1) write(*,*)  'const_jacobian_sf_grp'
      call alloc_surf_shape_func                                        &
     &   (geofem%mesh%surf%nnod_4_surf, SGS_MHD_wk%fem_int%jcs%g_FEM,   &
     &    spfs_1%spf_2d)
      call const_jacobians_surf_group(my_rank, nprocs,                  &
     &    geofem%mesh%node, geofem%mesh%ele, geofem%mesh%surf,          &
     &    geofem%group%surf_grp, spfs_1%spf_2d, SGS_MHD_wk%fem_int%jcs)
      call dealloc_surf_shape_func(spfs_1%spf_2d)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &           'surf_jacobian_sf_grp_normal'
      call surf_jacobian_sf_grp_normal(my_rank, nprocs,                 &
     &    geofem%mesh, geofem%group, spfs_1, SGS_MHD_wk%fem_int%jcs)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*)' int_surface_parameters'
      call int_surface_parameters                                       &
     &   (geofem%mesh, geofem%group, SGS_MHD_wk%rhs_mat%surf_wk)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*)' set_boundary_data'
      call set_boundary_data                                            &
     &   (MHD_step%time_d, IO_bc, geofem%mesh, MHD_mesh, geofem%group,  &
     &    MHD_prop, MHD_BC, iref_base, ref_fld, iphys, nod_fld,         &
     &    FEM_MHD_BCs)
!
!     ---------------------
!
      call int_RHS_mass_matrices                                        &
     &    (FEM_prm%npoint_t_evo_int, geofem%mesh, MHD_mesh,             &
     &     SGS_MHD_wk%rhs_mat%fem_wk, SGS_MHD_wk%rhs_mat%f_l,           &
     &     SGS_MHD_wk%fem_int, SGS_MHD_wk%mk_MHD)
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
