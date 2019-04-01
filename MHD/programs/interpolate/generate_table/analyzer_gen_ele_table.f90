!analyzer_gen_ele_table.f90
!      module analyzer_gen_ele_table
!..................................................
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_gen_ele_table
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
      use t_ctl_data_gen_table
      use t_work_const_itp_table
      use t_search_block_4_itp
      use t_ctl_params_4_gen_table
!
      implicit none
!
      type(ctl_data_gen_table), save :: gtbl_ctl1
      type(ctl_params_4_gen_table), save :: gen_itp_p1
!
      type(mesh_data), save :: org_femmesh
      type(element_geometry), save :: org_ele_mesh
!
      type(next_nod_ele_table), save :: next_tbl_i
!
      type(shape_finctions_at_points), save :: spfs_I
      type(jacobians_type), save :: jacobians_I
!
      type(interpolate_table), save :: itp_ele
      type(interpolate_coefs_dest), save :: itp_e_coef
      type(para_block_4_interpolate) :: itp_blks1
      type(work_const_itp_table), save :: cst_itp_wke
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use t_shape_functions
!
      use const_mesh_information
      use set_table_4_RHS_assemble
      use element_posi_2_nodal_array
      use set_2nd_geometry_4_table
      use const_jacobians_3d
      use mpi_load_mesh_data
!
      if (my_rank.eq.0) then
        write(*,*) 'Construct commutation filter'
        write(*,*) 'Input file: mesh data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_gen_itp_table'
      call read_control_4_gen_itp_table(gtbl_ctl1)
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_4_gen_table'
      call set_ctl_params_4_gen_table(gtbl_ctl1, gen_itp_p1, itp_blks1)
      
call dealloc_ctl_data_gen_table(gtbl_ctl1)
!
!  --  read geometry
!
      if (iflag_debug.eq.1) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(gen_itp_p1%itp_dest_mesh_file,                &
     &    nprocs, org_femmesh, org_ele_mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
      call set_nod_and_ele_infos                                        &
     &   (org_femmesh%mesh%node, org_femmesh%mesh%ele)
!
!     ----- construct mesh informations for original mesh
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'set_2nd_geometry_type_itp_tbl', nprocs_2nd
      call set_2nd_geometry_type_itp_tbl                                &
     &   (gen_itp_p1%itp_org_mesh_file, nprocs_2nd)
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
      call s_element_posi_2_nodal_array                                 &
     &   (org_femmesh%mesh%ele, org_femmesh%mesh%node)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 's_set_serach_data_4_dest'
      call s_set_serach_data_4_dest(org_femmesh%mesh%node,              &
     &    itp_ele%tbl_dest, itp_e_coef, cst_itp_wke, itp_blks1)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use calypso_mpi
      use m_interpolate_table_IO
!
      use construct_interpolate_table
      use const_interpolate_4_org
      use const_rev_ele_itp_table
      use order_dest_table_by_domain
      use order_dest_table_by_type
      use itp_table_IO_select_4_zlib
      use copy_interpolate_types
      use delete_data_files
!
      integer(kind = kint) :: ierr_missing, num_pe
!
!
      if (iflag_debug.eq.1) write(*,*) 's_construct_interpolate_table'
      call s_construct_interpolate_table                                &
     &   (gen_itp_p1, org_femmesh%mesh%node, next_tbl_i%neib_nod,       &
     &    itp_blks1%org_blocks, itp_e_coef,                             &
     &    cst_itp_wke%iflag_org_domain, ierr_missing)
!
!   ordering destination table by domain
!
      if (iflag_debug.eq.1) write(*,*) 's_order_dest_table_by_domain'
      call s_order_dest_table_by_domain(org_femmesh%mesh%node,          &
     &    cst_itp_wke%iflag_org_domain, ierr_missing,                   &
     &    itp_ele%tbl_dest, itp_e_coef, cst_itp_wke%orderd)
!
!      call check_table_in_org_2(13, itp_ele%tbl_dest, itp_e_coef)
!
!   ordering destination table by interpolation type
!
      if (iflag_debug.eq.1) write(*,*) 's_order_dest_table_by_type'
      call s_order_dest_table_by_type                                   &
     &   (org_femmesh%mesh%node, org_femmesh%mesh%ele,                  &
     &    itp_ele%tbl_dest, itp_e_coef, cst_itp_wke%orderd)
!
      call copy_itp_coefs_dest(my_rank,                                 &
     &    itp_ele%tbl_dest, itp_e_coef, IO_itp_dest, IO_itp_c_dest)
!
      table_file_header = work_header
      call sel_write_itp_coefs_dest                                     &
     &   (my_rank, IO_itp_dest, IO_itp_c_dest)
!
!   construct table for originate domain
!
      if(gen_itp_p1%iflag_reverse_itp_tbl .eq. 1) then
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'const_rev_ele_interpolate_table'
        call const_rev_ele_interpolate_table(gen_itp_p1, cst_itp_wke)
      else
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'const_interpolate_table_4_orgin'
        call const_interpolate_table_4_orgin(gen_itp_p1, cst_itp_wke)
      end if
!
!
      if (my_rank .eq. 0) then
        num_pe = int(nprocs, KIND(num_pe))
        call delete_parallel_files(ione, num_pe, work_header)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_gen_ele_table
