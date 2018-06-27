!analyzer_gen_table.f90
!      module analyzer_gen_table
!..................................................
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine init_make_interpolate_table
!      subroutine analyze_make_interpolate_table
!
      module analyzer_gen_table
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_machine_parameter
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_shape_functions
      use t_jacobians
      use t_interpolate_table
      use t_interpolate_coefs_dest
!
      implicit none
!
      type(mesh_data), save :: org_femmesh
      type(element_geometry), save :: org_ele_mesh
!
      type(next_nod_ele_table), save :: next_tbl_i
!
      type(shape_finctions_at_points), save :: spfs_I
      type(jacobians_type), save :: jacobians_I
!
      type(interpolate_table), save :: itp_nod
      type(interpolate_coefs_dest), save :: itp_n_coef
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_make_interpolate_table
!
      use m_ctl_params_4_gen_table
      use m_2nd_pallalel_vector
      use t_shape_functions
!
      use input_control_gen_table
      use const_mesh_information
      use set_table_4_RHS_assemble
      use set_2nd_geometry_4_table
      use const_jacobians_3d
      use mpi_load_mesh_data
!
!
      if (my_rank.eq.0) then
        write(*,*) 'Construct commutation filter'
        write(*,*) 'Input file: mesh data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_generate_table'
      call s_input_control_generate_table
!
!     ----- construct mesh informations for target mesh
!
      call mpi_input_mesh                                               &
     &   (itp_dest_mesh_file, nprocs, org_femmesh, org_ele_mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
      call const_mesh_infos                                             &
     &   (my_rank, org_femmesh%mesh, org_femmesh%group, org_ele_mesh)
!
!     ----- construct mesh informations for original mesh
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'set_2nd_geometry_type_itp_tbl', nprocs_2nd
      call set_2nd_geometry_type_itp_tbl(itp_org_mesh_file, nprocs_2nd)
!
!  -------------------------------
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (org_femmesh%mesh, next_tbl_i%neib_ele, next_tbl_i%neib_nod)
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobians_element'
      allocate(jacobians_I%g_FEM)
      call sel_max_int_point_by_etype                                   &
     &   (org_femmesh%mesh%ele%nnod_4_ele, jacobians_I%g_FEM)
      call initialize_FEM_integration(jacobians_I%g_FEM,                &
     &    spfs_I%spf_3d, spfs_I%spf_2d, spfs_I%spf_1d)
!
      call alloc_vol_shape_func(org_femmesh%mesh%ele%nnod_4_ele,        &
     &    jacobians_I%g_FEM, spfs_I%spf_3d)
      call const_jacobians_element(my_rank, nprocs,                     &
     &    org_femmesh%mesh%node, org_femmesh%mesh%ele,                  &
     &    org_femmesh%group%surf_grp, org_femmesh%group%infty_grp,      &
     &    spfs_I%spf_3d, jacobians_I)
      call dealloc_vol_shape_func(spfs_I%spf_3d)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 's_set_serach_data_4_dest'
      call s_set_serach_data_4_dest                                     &
     &   (org_femmesh%mesh%node, itp_nod%tbl_dest, itp_n_coef)
!
      end subroutine init_make_interpolate_table
!
! ----------------------------------------------------------------------
!
      subroutine analyze_make_interpolate_table
!
      use calypso_mpi
!
      use m_interpolate_table_IO
      use construct_interpolate_table
      use const_interpolate_4_org
      use order_dest_table_by_domain
      use order_dest_table_by_type
      use itp_table_IO_select_4_zlib
      use copy_interpolate_types
      use delete_data_files
!
      integer(kind = kint) :: ierr_missing
!
!
      if (iflag_debug.eq.1) write(*,*) 's_construct_interpolate_table'
      call s_construct_interpolate_table                                &
     &   (org_femmesh%mesh%node, next_tbl_i%neib_nod,                   &
     &    itp_n_coef, ierr_missing)
!
!   ordering destination table by domain
!
      if (iflag_debug.eq.1) write(*,*) 's_order_dest_table_by_domain'
      call s_order_dest_table_by_domain                                 &
     &   (org_femmesh%mesh%node%internal_node,                          &
     &    ierr_missing, itp_nod%tbl_dest, itp_n_coef)
!
!      call check_table_in_org_2(13, itp_nod%tbl_dest, itp_n_coef)
!
!   ordering destination table by interpolation type
!
      if (iflag_debug.eq.1) write(*,*) 's_order_dest_table_by_type'
      call s_order_dest_table_by_type                                   &
     &   (org_femmesh%mesh%node, org_femmesh%mesh%ele,                  &
     &    itp_nod%tbl_dest, itp_n_coef)
!
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_coefs_dest'
      call copy_itp_coefs_dest(my_rank,                                 &
     &    itp_nod%tbl_dest, itp_n_coef, IO_itp_dest, IO_itp_c_dest)
!
      table_file_header = work_header
      call sel_write_itp_coefs_dest                                     &
     &   (my_rank, IO_itp_dest, IO_itp_c_dest)
!
!   construct table for originate domain
!
      if (iflag_debug.eq.1)                                             &
     &   write(*,*) 'const_interpolate_table_4_orgin'
      call const_interpolate_table_4_orgin
!
!
      if (my_rank .eq. 0) then
        call delete_parallel_files(ione, nprocs, work_header)
      end if
!
      if (iflag_debug.eq.1)                                             &
     &       write(*,*) 'exit analyze_make_interpolate_table'
!
      end subroutine analyze_make_interpolate_table
!
! ----------------------------------------------------------------------
!
      end module analyzer_gen_table
