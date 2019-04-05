!
      program periodic_cube
!
!
      use m_precision
      use m_constants
!
      use t_cubed_sph_mesh
      use t_cubed_sph_surf_mesh
      use t_cubed_sph_radius
      use t_numref_cubed_sph
      use t_control_data_cubed_sph
      use m_cubed_sph_grp_param
!
      use count_coarse_parameters
      use const_rect_sphere_surface
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
      type(cubed_sph_radius), save :: rprm_csph1
      type(numref_cubed_sph), save :: csph_p1
      type(coarse_cubed_sph), save :: course_p1
      type(cubed_sph_surf_mesh), save :: c_sphere1
      type(cubed_sph_mesh), save :: csph_mesh1
!
!
!      write(*,*) 'Mesh generation is starting. Press return key'
!      read(5,*)
!
      call read_control_4_shell(cubed_sph_c1)
!
      call set_peri_cube_paramteres                                     &
     &   (cubed_sph_c1, rprm_csph1, csph_p1, course_p1)
      call dealloc_control_data_cubed_sph(cubed_sph_c1)
!
!    count number of node & element
!
      write(*,*) 'count_rectangle_shell_size'
      call count_rectangle_shell_size                                   &
     &   (csph_p1, rprm_csph1, c_sphere1, csph_mesh1)
!
      write(*,*) 'alloc_surface_geometries', c_sphere1%numnod_sf20
      call alloc_surface_geometries(c_sphere1)
      call alloc_1d_position(csph_p1)
!
!  count avaiable coarsing level
!
      write(*,*) 'check_rect_coarsing_level'
      call check_rect_coarsing_level                                    &
     &   (rprm_csph1, csph_p1, course_p1, c_sphere1)
!
      write(*,*) 'alloc_coarse_mesh_stack'
      call alloc_coarse_mesh_stack                                      &
     &   (course_p1%max_coarse_level, csph_mesh1)
      call alloc_coarsing_stack                                         &
     &   (course_p1%max_coarse_level, c_sphere1)
!
      write(*,*) 'count_coarse_rect_shell'
      call count_coarse_rect_shell                                      &
     &   (csph_p1, course_p1, c_sphere1, csph_mesh1)
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
      call const_rect_sphere_surf_node(rad_edge, csph_p1, c_sphere1)
      call const_rect_sphere_surf_data(csph_p1, c_sphere1)
!
       write(*,*) 'const_coarse_rect_surf_data'
      call const_coarse_rect_surf_data                                  &
     &   (csph_mesh1, csph_p1, course_p1, c_sphere1)
!
!   construct whole grid
!
      call construct_rect_peri_mesh(csph_mesh1, csph_p1, c_sphere1)
!
!   construct coarse grid
!
      call const_coarse_rect_tri_peri                                   &
     &   (csph_mesh1, csph_p1, course_p1, c_sphere1)
!
      end program periodic_cube
