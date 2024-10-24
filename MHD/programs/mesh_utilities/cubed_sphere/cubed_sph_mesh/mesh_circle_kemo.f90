!
      program mesh_circle_kemo
!
!
      use m_precision
!
      use t_cubed_sph_mesh
      use t_cubed_sph_surf_mesh
      use t_cubed_sph_radius
      use t_numref_cubed_sph
      use t_cubed_sph_grp_param
      use t_control_data_cubed_sph
!
      use count_coarse_parameters
      use const_squre_circle_edge
      use construct_circle_data
      use set_surf_connect_cubed_sph
!
      use set_cubed_sph_control
!
      use count_square_circle_config
!
      implicit none
!
      type(control_data_cubed_sph), save :: cubed_sph_c1
      type(cubed_sph_radius), save :: rprm_csph1
      type(numref_cubed_sph), save :: csph_p1
      type(coarse_cubed_sph), save :: course_p1
      type(cubed_sph_surf_mesh), save :: c_sphere1
      type(cubed_sph_mesh), save :: csph_mesh1
      type(cubed_sph_group), save :: csph_grp1
!
!      write(*,*) 'Mesh generation is starting. Press return key'
!      read(5,*)
!
      call read_control_4_shell(cubed_sph_c1)
!
      call set_shell_paramteres                                         &
     &   (cubed_sph_c1, rprm_csph1, csph_p1, course_p1, csph_grp1)
      call dealloc_control_data_cubed_sph(cubed_sph_c1)
!
!    count number of node & element
!
      write(*,*) 'count_square_circle_size'
      call count_square_circle_size                                     &
     &   (csph_p1, rprm_csph1, csph_grp1, c_sphere1, csph_mesh1)
!
!      write(*,*) 'alloc_surface_geometries', c_sphere1%numnod_sf20
      call alloc_surface_geometries(c_sphere1)
      call alloc_1d_position(csph_p1)
!
!       write(*,*) 'alloc_surface_connect(c_sphere1)'
      call set_ntot_ele_sf20(c_sphere1)
      call alloc_surface_connect(c_sphere1)
!
! set sphere cube data
!
       write(*,*) 'const_squre_circle_edge_data'
      call const_squre_circle_edge_data(csph_grp1, csph_p1, c_sphere1)
!
!   construct whole grid
!
      call construct_circle_mesh                                        &
     &   (rprm_csph1, csph_mesh1, csph_grp1, csph_p1, c_sphere1)
!
      end program mesh_circle_kemo
