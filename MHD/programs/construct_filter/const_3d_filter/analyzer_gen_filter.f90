!analyzer_gen_filter.f90
!      module analyzer_gen_filter
!..................................................
!
!      modified by H. Matsui on Mar., 2008 
!
!      subroutine generate_filter_init
!      subroutine generate_filter_analyze
!
      module analyzer_gen_filter
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_ctl_params_4_gen_filter
      use t_ctl_param_newdom_filter
      use t_file_IO_parameter
      use t_mesh_data
      use t_shape_functions
      use t_jacobians
      use t_table_FEM_const
      use t_crs_connect
      use t_work_FEM_integration
      use t_filter_elength
      use t_filter_dxdxi
      use t_filter_moments
      use t_filtering_data
      use t_filter_file_data
      use t_ctl_data_gen_3d_filter
      use t_reference_moments
      use t_element_list_4_filter
      use t_matrix_4_filter
!
      implicit none
!
!
      type(ctl_data_gen_3d_filter), save :: filter3d_ctl1
      type(field_IO_params), save ::  mesh_filter_file
      type(ctl_params_4_gen_filter), save :: gfil_p1
      type(ctl_param_newdom_filter), save :: newfil_p1
!
      type(mesh_data), save :: fem_f
      type(shape_finctions_at_points), save :: spfs_f
!>      Stracture for FEM assembling
      type(finite_element_integration), save :: fem_int_f
!
      type(CRS_matrix_connect), save :: tbl_crs_f
      type(table_mat_const), save :: mat_tbl_f
      type(arrays_finite_element_mat), save :: rhs_mat_f
      type(gradient_model_data_type), save :: FEM_elen_f
!
      type(dxdxi_data_type), save :: filter_dxi1
      type(dxidx_data_type), save :: dxidxs1
      type(gradient_filter_mom_type), save :: FEM_momenet1
      type(reference_moments), save :: ref_m1
      type(element_list_4_filter), save :: fil_elist1
!
      type(matrices_4_filter), save :: f_matrices1
      type(filtering_data_type), save :: filtering_gen
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine generate_filter_init
!
      use m_filter_file_names
!
      use const_mesh_information
      use cal_1d_moments_4_fliter
!
      use set_element_data_4_IO
      use set_surface_data_4_IO
      use set_edge_data_4_IO
      use check_jacobians
      use int_volume_of_domain
      use int_element_length
      use set_surf_grp_vectors
      use check_surface_groups
      use set_normal_vectors
      use set_edge_vectors
      use sum_normal_4_surf_group
      use const_jacobians_3d
      use set_ctl_gen_filter
      use mpi_load_mesh_data
!
!
      use calypso_mpi
!
      if (my_rank.eq.0) then
        write(*,*) 'Construct commutation filter'
        write(*,*) 'Input file: mesh data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_gen_filter'
      call read_control_4_gen_filter(filter3d_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'set_controls_gen_3dfilter'
      call set_controls_gen_3dfilter(filter3d_ctl1, FEM_elen_f,         &
     &    mesh_filter_file, gfil_p1, newfil_p1, ref_m1, f_matrices1)
      call dealloc_const_filter_ctl_data(filter3d_ctl1)
!
!  --  read geometry
!
      if (iflag_debug.eq.1) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(mesh_filter_file, nprocs, fem_f)
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_1d_moments'
      call s_cal_1d_moments                                             &
     &   (gfil_p1%num_ref_filter, gfil_p1%iref_filter_type, FEM_elen_f)
!
      call s_set_element_list_4_filter                                  &
     &   (fem_f%mesh%ele, fem_f%group%ele_grp, gfil_p1, fil_elist1)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank, fem_f%mesh, fem_f%group)
!
!  -------------------------------
!
      if (iflag_debug.eq.1 .and. my_rank.eq.0 )                         &
     &   write(*,*) 'alloc_vectors_surf_group'
      call alloc_vectors_surf_group                                     &
     &  (fem_f%group%surf_grp%num_grp, fem_f%group%surf_grp%num_item,   &
     &   fem_f%group%surf_grp_geom)
!
      if (iflag_debug.eq.1 .and. my_rank.eq.0 )                         &
     &   write(*,*) 'pick_surface_group_geometry'
      call pick_surface_group_geometry                                  &
     &   (fem_f%mesh%surf, fem_f%group%surf_grp,                        &
     &    fem_f%group%tbls_surf_grp, fem_f%group%surf_grp_geom)
!
!  -------------------------------
!  -------------------------------
!
      if (iflag_debug.eq.1)  write(*,*)  'const_jacobian_and_volume'
      allocate(fem_int_f%jcs%g_FEM)
      call set_max_integration_points                                   &
     &   (gfil_p1%num_int_points, fem_int_f%jcs%g_FEM)
      call initialize_FEM_integration(fem_int_f%jcs%g_FEM,              &
     &    spfs_f%spf_3d, spfs_f%spf_2d, spfs_f%spf_1d)
!
      call const_jacobian_and_volume(my_rank, nprocs,                   &
     &    fem_f%mesh%node, fem_f%group%surf_grp,                        &
     &    fem_f%group%infty_grp, fem_f%mesh%ele,                        &
     &    spfs_f%spf_3d, fem_int_f%jcs)
!
!      call check_jacobians_trilinear                                   &
!     &   (my_rank, fem_f%mesh%ele, fem_int_f%jcs%jac_3d_l)
!
!  -------------------------------
!
      if (iflag_debug.eq.1)  write(*,*)  'int_element_length_1st'
      FEM_elen_f%nnod_filter_mom = fem_f%mesh%node%numnod
      FEM_elen_f%nele_filter_mom = fem_f%mesh%ele%numele
      FEM_momenet1%num_filter_moms = 2
      call alloc_jacobians_ele(FEM_elen_f%nele_filter_mom, filter_dxi1)
      call alloc_elen_ele_type                                          &
     &   (FEM_elen_f%nele_filter_mom, FEM_elen_f%elen_ele)
!
      call s_int_element_length(FEM_elen_f%nele_filter_mom,             &
     &    fem_f%mesh%node, fem_f%mesh%ele,                              &
     &    fem_int_f%jcs%g_FEM, spfs_f%spf_3d,                           &
     &    filter_dxi1%dxi_ele, FEM_elen_f%elen_ele%moms)
      call dealloc_vol_shape_func(spfs_f%spf_3d)
!
       end subroutine generate_filter_init
!
! ----------------------------------------------------------------------
!
      subroutine generate_filter_analyze
!
      use m_array_for_send_recv
      use m_nod_filter_comm_table
      use m_filter_file_names
      use m_file_format_switch
!
      use t_filter_coefs
!
      use cal_element_size
      use set_parallel_file_name
      use filter_moment_IO_select
      use construct_filters
      use copy_mesh_structures
      use set_filter_geometry_4_IO
      use filter_moment_IO_select
      use filter_coefs_file_IO
      use filter_coefs_file_IO_b
      use check_num_fail_nod_commute
      use nod_phys_send_recv
!
      use cal_filter_func_node
!
      character(len=kchara) :: file_name
      type(filter_file_data) :: filter_IO
      type(const_filter_coefs) :: fil_gen1
!
!  ---------------------------------------------------
!       set element size for each node
!  ---------------------------------------------------
!
      if(iflag_debug.eq.1)  write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(ithree, fem_f%mesh%node%numnod)
!
      call init_nod_send_recv(fem_f%mesh)
!
      if(iflag_debug.eq.1)  write(*,*) 's_cal_element_size'
      call s_cal_element_size(fem_f%mesh, fem_f%group,                  &
     &    fil_elist1, gfil_p1, tbl_crs_f, mat_tbl_f, rhs_mat_f,         &
     &    fem_int_f, FEM_elen_f, ref_m1, filter_dxi1, dxidxs1)
      call dealloc_jacobians_ele(filter_dxi1)
!
!  ---------------------------------------------------
!       output filter length and coefs
!  ---------------------------------------------------
!
      ifmt_filter_file = ifmt_filter_elen
      filter_file_head = filter_elen_head
      call sel_write_filter_elen_file(my_rank, FEM_elen_f)
!
!  ---------------------------------------------------
!       copy node and communication table
!  ---------------------------------------------------
!
      if(gfil_p1%iflag_tgt_filter_type .gt. iflag_no_filter)  then
        call copy_node_data_to_filter(fem_f%mesh%node)
        call copy_comm_tbl_types                                        &
     &     (fem_f%mesh%nod_comm, filtering_gen%comm)
!
        call copy_filtering_geometry_to_IO(filter_IO%node)
        call copy_comm_tbl_type(filtering_gen%comm, filter_IO%nod_comm)
!
        filter_file_head = filter_coef_head
        call sel_write_filter_geometry_file(my_rank, filter_IO)
!
        call select_const_filter                                        &
     &      (file_name, newfil_p1, fem_f%mesh, fem_int_f, tbl_crs_f,    &
     &       rhs_mat_f, FEM_elen_f, fil_elist1, gfil_p1, ref_m1,        &
     &       dxidxs1, FEM_momenet1, fil_gen1, f_matrices1)
        call dealloc_jacobians_node(filter_dxi1)
!
!
        call s_check_num_fail_nod_commute                               &
     &     (fil_gen1%whole_area, fil_gen1%fluid_area)
!
!  ---------------------------------------------------
!       output filter moments
!  ---------------------------------------------------
!
        ifmt_filter_file = ifmt_filter_moms
        filter_file_head = filter_moms_head
        call sel_write_filter_moms_file                                 &
     &     (my_rank, FEM_elen_f, FEM_momenet1)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
      end subroutine generate_filter_analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_gen_filter
