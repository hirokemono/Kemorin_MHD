!analyzer_mesh_test.f90
!
!      module analyzer_mesh_test
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine init_analyzer
!      subroutine analyze
!
!..................................................
!
      module analyzer_mesh_test
!
      use m_precision
!
      use m_machine_parameter
      use m_parallel_var_dof
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use const_mesh_info
      use cal_jacobian
!
      use set_element_geometry_4_IO
      use set_surface_geometry_4_IO
      use set_edge_geometry_4_IO
      use element_IO_select
      use surface_IO_select
      use edge_IO_select
      use m_geometry_data
      use m_surface_group
      use check_jacobians
      use int_volume_of_domain
      use set_surf_grp_vectors
      use check_surface_groups
      use set_normal_vectors
      use set_edge_vectors
      use m_read_mesh_data
      use m_comm_data_IO
      use mesh_data_IO
      use sum_normal_4_surf_group
      use set_parallel_file_name
      use set_node_geometry_4_IO
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
      iflag_mesh_file_fmt = ifile_type
      if (iflag_debug.gt.0) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
!
!     ---------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'pick_surface_group_geometry'
      call pick_surface_group_geometry
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
!      call check_jacobians_trilinear(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_jacobian_surf_grp'
      call cal_jacobian_surf_grp
!
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 's_int_whole_volume_only'
      call s_int_whole_volume_only
      if (my_rank.eq.0) write(*,*)  'Volume of Domain: ', volume
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
       call pick_normal_of_surf_group
!
       if (iflag_debug.gt.0)  write(*,*) 's_sum_normal_4_surf_group'
       call s_sum_normal_4_surf_group
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
      call copy_node_geom_sph_to_IO
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
      call copy_node_geom_cyl_to_IO
!
      call output_node_cyl_geometry
      close(input_file_code)
!
!  -------------------------------
!     output element data
!  -------------------------------
!
      iflag_mesh_file_fmt = ifile_type
!
      if (iflag_debug.gt.0) write(*,*) 'copy_ele_geometry_to_IO'
      mesh_ele_file_head = mesh_ele_def_head
      call copy_ele_geometry_to_IO
      call sel_output_element_file(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_ele_sph_geom_to_IO'
      write(mesh_ele_file_head,'(a,a4)') mesh_ele_def_head, '_sph'
      call copy_ele_sph_geom_to_IO
      call sel_output_element_sph_file(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_ele_cyl_geom_to_IO'
      write(mesh_ele_file_head,'(a,a4)') mesh_ele_def_head, '_cyl'
      call copy_ele_cyl_geom_to_IO
      call sel_output_element_cyl_file(my_rank)
!
!  -------------------------------
!     output surface data
!  -------------------------------
!
      iflag_mesh_file_fmt = ifile_type
      mesh_surf_file_head = mesh_def_surf_head
      if (iflag_debug.gt.0) write(*,*) 'copy_surf_geometry_to_IO'
      call copy_surf_connect_to_IO
      call copy_surf_geometry_to_IO
!
      if (iflag_debug.gt.0) write(*,*) 'sel_output_surface_file'
      call sel_output_surface_file(my_rank)
!
      write(mesh_surf_file_head,'(a,a4)') mesh_def_surf_head, '_sph'
      if (iflag_debug.gt.0) write(*,*) 'copy_surf_geometry_to_IO_sph'
      call copy_surf_connect_to_IO
      call copy_surf_geometry_to_IO_sph
!
      if (iflag_debug.gt.0) write(*,*) 'sel_output_surface_sph_file'
      call sel_output_surface_sph_file(my_rank)
!
      write(mesh_surf_file_head,'(a,a4)') mesh_def_surf_head, '_cyl'
      if (iflag_debug.gt.0) write(*,*) 'copy_surf_geometry_to_IO_cyl'
      call copy_surf_connect_to_IO
      call copy_surf_geometry_to_IO_cyl
!
      if (iflag_debug.gt.0) write(*,*) 'sel_output_surface_cyl_file'
      call sel_output_surface_cyl_file(my_rank)
!
!  -------------------------------
!     output edge data
!  -------------------------------
!
      mesh_edge_file_head = mesh_def_edge_head
      if (iflag_debug.gt.0) write(*,*) 'copy_edge_geometry_to_IO'
      call copy_edge_connect_to_IO
      call copy_edge_geometry_to_IO
!
      if (iflag_debug.gt.0) write(*,*) 'sel_output_edge_geometries'
      call sel_output_edge_geometries(my_rank)
!
      write(mesh_edge_file_head,'(a,a4)') mesh_def_edge_head, '_sph'
      if (iflag_debug.gt.0) write(*,*) 'copy_edge_geometry_to_IO'
      call copy_edge_connect_to_IO
      call copy_edge_geometry_to_IO_sph
!
      if (iflag_debug.gt.0) write(*,*) 'sel_output_edge_geometries_sph'
      call sel_output_edge_geometries_sph(my_rank)
!
      write(mesh_edge_file_head,'(a,a4)') mesh_def_edge_head, '_cyl'
      if (iflag_debug.gt.0) write(*,*) 'copy_edge_connect_to_IO'
      call copy_edge_connect_to_IO
      call copy_edge_geometry_to_IO_cyl
      if (iflag_debug.gt.0) write(*,*) 'sel_output_edge_geometries_cyl'
      call sel_output_edge_geometries_cyl(my_rank)
!
       end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
        subroutine analyze
!
!
      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
        end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_mesh_test
