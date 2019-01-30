!
      program periodic_cube
!
!
      use m_precision
      use m_constants
!
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
      use m_cubed_sph_radius
      use m_cubed_sph_grp_param
      use m_numref_cubed_sph
      use t_control_data_cubed_sph
!
      use count_coarse_parameters
      use const_peri_cube_surface
      use const_peri_cube_data
      use set_surf_connect_cubed_sph
!
      use set_peri_cube_control
!
      use count_shell_configration
      use check_coarsing_level
      use count_coarse_shell_config
!
      implicit none
!
      real(kind = kreal) :: rad_edge
      type(control_data_cubed_sph), save :: cubed_sph_c1
!
!
!      write(*,*) 'Mesh generation is starting. Press return key'
!      read(5,*)
!
      call read_control_4_shell(cubed_sph_c1)
!
      call set_peri_cube_paramteres(cubed_sph_c1)
      call dealloc_control_data_cubed_sph(cubed_sph_c1)
!
!    count number of node & element
!
      write(*,*) 'count_rectangle_shell_size'
      call count_rectangle_shell_size(c_sphere1)
!
      write(*,*) 'alloc_surface_geometries', c_sphere1%numnod_sf20
      call alloc_surface_geometries(c_sphere1)
      call allocate_1d_position
!
!  count avaiable coarsing level
!
      write(*,*) 'check_rect_coarsing_level'
      call check_rect_coarsing_level(c_sphere1)
!
      write(*,*) 'allocate_coarse_mesh_stack'
      call allocate_coarse_mesh_stack(max_coarse_level)
      call alloc_coarsing_stack(max_coarse_level, c_sphere1)
!
      write(*,*) 'count_coarse_rect_shell'
      call count_coarse_rect_shell(c_sphere1)
!
       write(*,*) 'alloc_surface_connect(c_sphere1)'
      call set_ntot_ele_sf20(c_sphere1)
      call alloc_surface_connect(c_sphere1)
      call alloc_coarse_surf_connect(c_sphere1)
!
! set sphere cube data
!
       write(*,*) 'const_rect_sphere_surf_data'
      rad_edge = atan(one / sqrt(two))
      call const_rect_sphere_surf_node(rad_edge)
      call const_rect_sphere_surf_data
!
       write(*,*) 'const_coarse_rect_surf_data'
      call const_coarse_rect_surf_data
!
!   construct whole grid
!
      call construct_rect_sphere_mesh
!
!   construct coarse grid
!
      call const_coarse_rect_tri_peri
!
      end program periodic_cube
