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
!
      implicit none
!
      type(mesh_data), save :: org_femmesh
      type(element_geometry), save :: org_ele_mesh
!
      type(next_nod_ele_table), save :: next_tbl_i
!
      type(volume_shape_function), save :: spf_3d_I
      type(jacobians_type), save :: jacobians_I
!
      type(interpolate_table), save :: itp_ele
      type(interpolate_coefs_dest), save :: itp_e_coef
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use m_ctl_params_4_gen_table
      use t_shape_functions
!
      use input_control_gen_table
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
      if (iflag_debug.eq.1) write(*,*) 's_input_control_generate_table'
      call s_input_control_generate_table
!
!  --  read geometry
!
      if (iflag_debug.eq.1) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(itp_dest_mesh_file, nprocs,                   &
     &    org_femmesh%mesh, org_femmesh%group,                          &
     &    org_ele_mesh%surf%nnod_4_surf, org_ele_mesh%edge%nnod_4_edge)
!
      if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
      call set_nod_and_ele_infos                                        &
     &   (org_femmesh%mesh%node, org_femmesh%mesh%ele)
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
      call max_int_point_by_etype(org_femmesh%mesh%ele%nnod_4_ele)
      call initialize_FEM_integration
      call alloc_vol_shape_func                                         &
     &   (org_femmesh%mesh%ele%nnod_4_ele, maxtot_int_3d, spf_3d_I)
      call const_jacobians_element(my_rank, nprocs,                     &
     &    org_femmesh%mesh%node, org_femmesh%mesh%ele,                  &
     &    org_femmesh%group%surf_grp, org_femmesh%group%infty_grp,      &
     &    spf_3d_I, jacobians_I)
      call dealloc_vol_shape_func(spf_3d_I)
!
!  -------------------------------
!
      call s_element_posi_2_nodal_array                                 &
     &   (org_femmesh%mesh%ele, org_femmesh%mesh%node)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 's_set_serach_data_4_dest'
      call s_set_serach_data_4_dest                                     &
     &   (org_femmesh%mesh%node, itp_ele%tbl_dest, itp_e_coef)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use calypso_mpi
      use m_interpolate_table_IO
      use m_ctl_params_4_gen_table
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
      integer(kind = kint) :: ierr_missing
!
!
      if (iflag_debug.eq.1) write(*,*) 's_construct_interpolate_table'
      call s_construct_interpolate_table                                &
     &   (org_femmesh%mesh%node, next_tbl_i%neib_nod,                   &
     &    itp_e_coef, ierr_missing)
!
!   ordering destination table by domain
!
      if (iflag_debug.eq.1) write(*,*) 's_order_dest_table_by_domain'
      call s_order_dest_table_by_domain                                 &
     &   (org_femmesh%mesh%node%internal_node, ierr_missing,            &
     &    itp_ele%tbl_dest, itp_e_coef)
!
!      call check_table_in_org_2(13, itp_ele%tbl_dest, itp_e_coef)
!
!   ordering destination table by interpolation type
!
      if (iflag_debug.eq.1) write(*,*) 's_order_dest_table_by_type'
      call s_order_dest_table_by_type                                   &
     &   (org_femmesh%mesh%node, org_femmesh%mesh%ele,                  &
     &    itp_ele%tbl_dest, itp_e_coef)
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
      if(iflag_reverse_itp_tbl .eq. 1) then
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'const_rev_ele_interpolate_table'
        call const_rev_ele_interpolate_table
      else
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'const_interpolate_table_4_orgin'
        call const_interpolate_table_4_orgin
      end if
!
!
      if (my_rank .eq. 0) then
        call delete_parallel_files(ione, nprocs, work_header)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_gen_ele_table
