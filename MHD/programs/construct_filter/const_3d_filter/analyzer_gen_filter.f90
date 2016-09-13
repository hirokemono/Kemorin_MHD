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
      use m_ctl_params_4_gen_filter
!
      use t_mesh_data
      use t_jacobian_3d
      use t_table_FEM_const
      use t_crs_connect
      use t_work_FEM_integration
      use t_filter_elength
      use t_filter_dxdxi
      use t_filter_moments
      use t_filtering_data
      use t_filter_file_data
!
      implicit none
!
!
      type(mesh_geometry), save :: mesh_filter
      type(mesh_groups), save :: group_filter
      type(element_geometry), save :: ele_filter
!
      type(jacobians_3d), save :: jac_3d_l
      type(jacobians_3d), save :: jac_3d_q
!
      type(tables_4_FEM_assembles), save :: rhs_tbl_f
      type(CRS_matrix_connect), save :: tbl_crs_f
      type(table_mat_const), save :: mat_tbl_f
      type(arrays_finite_element_mat), save :: rhs_mat_f
      type(gradient_model_data_type), save :: FEM_elen_f
!
      type(dxdxi_data_type), save :: filter_dxi1
      type(dxidx_data_type), save :: dxidxs1
      type(gradient_filter_mom_type), save :: FEM_momenet1
!
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
      use m_fem_gauss_int_coefs
      use m_filter_file_names
      use m_ctl_data_gen_3d_filter
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
      use m_read_mesh_data
      use set_element_list_4_filter
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
      call read_control_4_gen_filter
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_gen_filter'
      call set_ctl_params_gen_filter(FEM_elen_f)
!
!  --  read geometry
!
      if (iflag_debug.eq.1) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(mesh_filter, group_filter,                    &
     &    ele_filter%surf%nnod_4_surf, ele_filter%edge%nnod_4_edge)
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_1d_moments'
      call s_cal_1d_moments(FEM_elen_f)
!
      call s_set_element_list_4_filter                                  &
     &   (mesh_filter%ele, group_filter%ele_grp)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
      call const_mesh_infos                                             &
     &   (my_rank, mesh_filter, group_filter, ele_filter)
!
!  -------------------------------
!
      if (iflag_debug.eq.1 .and. my_rank.eq.0 )                         &
     &   write(*,*) 'alloc_vectors_surf_group'
      call alloc_vectors_surf_group                                     &
     &  (group_filter%surf_grp%num_grp, group_filter%surf_grp%num_item, &
     &   group_filter%surf_grp_geom)
!
      if (iflag_debug.eq.1 .and. my_rank.eq.0 )                         &
     &   write(*,*) 'pick_surface_group_geometry'
      call pick_surface_group_geometry                                  &
     &   (ele_filter%surf, group_filter%surf_grp,                       &
     &    group_filter%tbls_surf_grp, group_filter%surf_grp_geom)
!
!  -------------------------------
!  -------------------------------
!
      if (iflag_debug.eq.1)  write(*,*)  'cal_jacobian_element'
      call maximum_integration_points(num_int_points)
      call const_jacobian_and_volume(mesh_filter%node,                  &
     &    group_filter%surf_grp, group_filter%infty_grp,                &
     &    mesh_filter%ele, jac_3d_l, jac_3d_q)
!
!      call check_jacobians_trilinear                                   &
!     &   (my_rank, mesh_filter%ele, jac_3d_l)
!
!  -------------------------------
!
      if (iflag_debug.eq.1)  write(*,*)  'int_element_length_1st'
      FEM_elen_f%nnod_filter_mom = mesh_filter%node%numnod
      FEM_elen_f%nele_filter_mom = mesh_filter%ele%numele
      FEM_momenet1%num_filter_moms = 2
      call alloc_jacobians_ele(FEM_elen_f%nele_filter_mom, filter_dxi1)
      call alloc_elen_ele_type                                          &
     &   (FEM_elen_f%nele_filter_mom, FEM_elen_f%elen_ele)
!
      call s_int_element_length(FEM_elen_f%nele_filter_mom,             &
     &    mesh_filter%node, mesh_filter%ele,                            &
     &    filter_dxi1%dxi_ele, FEM_elen_f%elen_ele%moms)
!
       end subroutine generate_filter_init
!
! ----------------------------------------------------------------------
!
      subroutine generate_filter_analyze
!
      use m_array_for_send_recv
      use m_matrix_4_filter
      use m_crs_matrix_4_filter
      use m_filter_coefs
      use m_nod_filter_comm_table
      use m_filter_file_names
      use m_file_format_switch
!
      use cal_element_size
      use set_parallel_file_name
      use filter_moment_IO_select
      use construct_filters
      use copy_mesh_structures
      use set_comm_table_4_IO
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
!
!  ---------------------------------------------------
!       set element size for each node
!  ---------------------------------------------------
!
      if(iflag_debug.eq.1)  write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(ithree, mesh_filter%node%numnod)
!
      call init_send_recv(mesh_filter%nod_comm)
!
      if(iflag_debug.eq.1)  write(*,*) 's_cal_element_size'
      call s_cal_element_size                                           &
     &   (mesh_filter%nod_comm, mesh_filter%node, mesh_filter%ele,      &
     &    jac_3d_q, rhs_tbl_f, tbl_crs_f, mat_tbl_f, rhs_mat_f,         &
     &    FEM_elen_f, filter_dxi1, dxidxs1)
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
      if (iflag_tgt_filter_type .gt. -10)  then
        call copy_node_data_to_filter(mesh_filter%node)
        call copy_comm_tbl_types                                        &
     &     (mesh_filter%nod_comm, filtering_gen%comm)
!
        call copy_filtering_geometry_to_IO(filter_IO%node)
        call copy_comm_tbl_type(filtering_gen%comm, filter_IO%nod_comm)
!
        filter_file_head = filter_coef_head
        call sel_write_filter_geometry_file(my_rank, filter_IO)
!
        num_failed_whole = 0
        num_failed_fluid = 0
!
        call select_const_filter(file_name,                             &
    &       mesh_filter%nod_comm, mesh_filter%node, mesh_filter%ele,    &
    &       jac_3d_q, rhs_tbl_f, tbl_crs_f, rhs_mat_f, FEM_elen_f,      &
    &       dxidxs1, FEM_momenet1)
        call dealloc_jacobians_node(filter_dxi1)
!
!
        call s_check_num_fail_nod_commute
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
