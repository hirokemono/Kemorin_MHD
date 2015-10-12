!analyzer_mesh_test.f90
!
!      module analyzer_mesh_test
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine initialize_mesh_test
!      subroutine analyze_mesh_test
!
!..................................................
!
      module analyzer_mesh_test
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_mesh_test
!
      use const_mesh_info
!
      use m_geometry_data
      use m_group_data
      use m_jacobians
      use m_jacobians_4_surface
      use m_jacobians_4_edge
      use m_jacobian_sf_grp
      use m_ele_sf_eg_comm_tables
      use m_array_for_send_recv
      use set_node_data_4_IO
      use set_element_data_4_IO
      use set_surface_data_4_IO
      use set_edge_data_4_IO
      use element_IO_select
      use surface_IO_select
      use edge_IO_select
      use check_jacobians
      use int_volume_of_domain
      use set_surf_grp_vectors
      use check_surface_groups
      use set_normal_vectors
      use set_edge_vectors
      use m_read_mesh_data
      use m_comm_data_IO
      use mesh_data_IO
      use nodal_vector_send_recv
      use sum_normal_4_surf_group
      use set_parallel_file_name
      use set_comm_table_4_IO
!
      use m_ctl_data_test_mesh
      use set_control_test_mesh
      use load_mesh_data
!
!
      if (my_rank.eq.0) then
        write(*,*) 'Construct commutation filter'
        write(*,*) 'Input file: mesh data'
      end if
!
!     ----- read control data
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_4_mesh_test'
      call read_control_4_mesh_test
!
      if (iflag_debug.gt.0) write(*,*) 'set_ctl_params_4_test_mesh'
      call set_ctl_params_4_test_mesh
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
!  -------------------------------
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(isix, node1%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_send_recv'
      call init_send_recv
!
!  -----    construct geometry informations
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tables_1st'
      call const_element_comm_tables_1st
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'pick_surface_group_geometry'
      call pick_surface_group_geometry                                  &
     &   (surf1, sf_grp1, sf_grp_tbl1, sf_grp_v1)
!
!  -------------------------------
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'cal_jacobian_element'
      call set_max_int_point_by_etype
      call cal_jacobian_element
!
      if (iflag_debug.gt.0) write(*,*) 'cal_jacobian_surface'
      call cal_jacobian_surface
!
      if (iflag_debug.gt.0) write(*,*) 'cal_jacobian_edge'
      call cal_jacobian_edge
!
!      call check_jacobians_trilinear(my_rank, ele1, jac_3d_l)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_jacobian_surf_grp'
      call cal_jacobian_surf_grp(sf_grp1)
!
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 's_int_whole_volume_only'
      call s_int_whole_volume_only
      if (my_rank.eq.0) write(*,*)  'Volume of Domain: ', ele1%volume
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_normal_vector'
      call s_cal_normal_vector
      if (iflag_debug.gt.0) write(*,*) 's_cal_normal_vector_spherical'
      call s_cal_normal_vector_spherical
      if (iflag_debug.gt.0) write(*,*) 's_cal_normal_vector_cylindrical'
      call s_cal_normal_vector_cylindrical
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_edge_vector'
      call s_cal_edge_vector
      if (iflag_debug.gt.0) write(*,*) 's_cal_edge_vector_spherical'
      call s_cal_edge_vector_spherical
      if (iflag_debug.gt.0) write(*,*) 's_cal_edge_vector_cylindrical'
      call s_cal_edge_vector_cylindrical
!
!  -------------------------------
!
       if (iflag_debug.gt.0)  write(*,*) 'pick_normal_of_surf_group'
       call pick_normal_of_surf_group                                   &
     &    (surf1, sf_grp1, sf_grp_tbl1, sf_grp_v1)
!
       if (iflag_debug.gt.0)  write(*,*) 's_sum_normal_4_surf_group'
       call s_sum_normal_4_surf_group(sf_grp1, sf_grp_v1)
!
!  ---------------------------------------------
!     output node data
!      spherical and cylindrical coordinate
!  ---------------------------------------------
!
      num_neib_domain_IO = 0
      my_rank_IO = my_rank
!
      call add_int_suffix(my_rank, mesh_sph_file_head, mesh_file_name)
      write(*,*) 'ascii mesh file: ', trim(mesh_file_name)
      open (input_file_code, file = mesh_file_name,                     &
     &      form = 'formatted')
!
      num_neib_domain_IO = 0
      call allocate_neib_domain_IO
!
      call copy_node_sph_to_IO(node1)
!
      call output_node_sph_geometry
      close(input_file_code)
!
!
      call add_int_suffix(my_rank, mesh_cyl_file_head, mesh_file_name)
      write(*,*) 'ascii mesh file: ', trim(mesh_file_name)
      open (input_file_code, file = mesh_file_name,                     &
     &      form = 'formatted')
!
      num_neib_domain_IO = 0
      call allocate_neib_domain_IO
      call copy_node_cyl_to_IO(node1)
!
      call output_node_cyl_geometry
      close(input_file_code)
!
!  -------------------------------
!     output element data
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'copy_ele_geometry_to_IO'
      mesh_ele_file_head = mesh_ele_def_head
      call copy_comm_tbl_type_to_IO(my_rank, ele_comm)
      call copy_ele_geometry_to_IO(ele1)
      call sel_output_element_file(my_rank)
!
!  -------------------------------
!     output surface data
!  -------------------------------
!
      mesh_surf_file_head = mesh_def_surf_head
      if (iflag_debug.gt.0) write(*,*) 'copy_surf_geometry_to_IO'
      call copy_comm_tbl_type_to_IO(my_rank, surf_comm)
      call copy_surf_connect_to_IO(surf1, ele1%numele)
      call copy_surf_geometry_to_IO(surf1)
!
      if (iflag_debug.gt.0) write(*,*) 'sel_output_surface_file'
      call sel_output_surface_file(my_rank)
!
!  -------------------------------
!     output edge data
!  -------------------------------
!
      mesh_edge_file_head = mesh_def_edge_head
      if (iflag_debug.gt.0) write(*,*) 'copy_edge_geometry_to_IO'
      call copy_comm_tbl_type_to_IO(my_rank, edge_comm)
      call copy_edge_connect_to_IO(edge1, ele1%numele, surf1%numsurf)
      call copy_edge_geometry_to_IO(edge1)
!
      if (iflag_debug.gt.0) write(*,*) 'sel_output_edge_geometries'
      call sel_output_edge_geometries(my_rank)
!
       end subroutine initialize_mesh_test
!
! ----------------------------------------------------------------------
!
        subroutine analyze_mesh_test
!
!
      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
        end subroutine analyze_mesh_test
!
! ----------------------------------------------------------------------
!
      end module analyzer_mesh_test
