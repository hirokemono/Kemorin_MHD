!
      program mesh_core_kemo
!
!
      use m_precision
!
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
      use m_cubed_sph_radius
      use m_cubed_sph_grp_param
      use m_numref_cubed_sph
      use t_control_data_cubed_sph
!
      use count_coarse_parameters
      use const_cube_sphere_surface
      use const_cube_sphere_data
      use set_surf_connect_cubed_sph
!
      use set_cubed_sph_control
!
      use count_shell_configration
      use check_coarsing_level
      use count_coarse_shell_config
!
      implicit none
!
      type(control_data_cubed_sph), save :: cubed_sph_c1
!
      write(*,*) 'Mesh generation is starting. Press return key'
      read(5,*)
!
      call read_control_4_shell(cubed_sph_c1)
!
      call set_shell_paramteres(cubed_sph_c1)
      call dealloc_control_data_cubed_sph(cubed_sph_c1)
!
!    count number of node & element
!
      write(*,*) 'count_cubed_shell_size'
      call count_cubed_shell_size
!
      write(*,*) 'allocate_surface_geometries', c_sphere1%numnod_sf20
      call allocate_surface_geometries
      call allocate_1d_position
!
!  count avaiable coarsing level
!
      write(*,*) 'check_cube_coarsing_level'
      call check_cube_coarsing_level
!
      write(*,*) 'allocate_coarse_mesh_stack'
      call allocate_coarse_mesh_stack(max_coarse_level)
      call alloc_coarsing_stack(max_coarse_level, c_sphere1)
!
      write(*,*) 'count_coarse_cubed_shell'
      call count_coarse_cubed_shell
!
      write(*,*) 'alloc_surface_connect(c_sphere1)'
      call set_ntot_ele_sf20(c_sphere1)
      call alloc_surface_connect(c_sphere1)
      call alloc_coarse_surf_connect(c_sphere1)
!
! set sphere cube data
!
      write(*,*) 'const_cube_surface_data'
      call const_cube_surface_data
!
      write(*,*) 'const_coarse_cube_surf_data'
      call const_coarse_cube_surf_data
!
!   construct whole grid
!
      call construct_sphere_mesh
!
!   construct coarse grid
!
      call construct_coarse_mesh
!
       stop
       end
!
