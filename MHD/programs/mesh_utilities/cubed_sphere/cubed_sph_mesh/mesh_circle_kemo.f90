!
      program mesh_circle_kemo
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
      use const_squre_circle_edge
      use construct_circle_data
!
      use set_cubed_sph_control
!
      use count_square_circle_config
!
      implicit none
!
      type(control_data_cubed_sph), save :: cubed_sph_c1
!
!      write(*,*) 'Mesh generation is starting. Press return key'
!      read(5,*)
!
      call read_control_4_shell(cubed_sph_c1)
!
      call set_shell_paramteres(cubed_sph_c1)
      call dealloc_control_data_cubed_sph(cubed_sph_c1)
!
!    count number of node & element
!
      write(*,*) 'count_square_circle_size'
      call count_square_circle_size
!
!      write(*,*) 'allocate_surface_geometries', c_sphere1%numnod_sf20
      call allocate_surface_geometries
      call allocate_1d_position
!
!       write(*,*) 'allocate_surface_connect'
      call allocate_surface_connect
!
! set sphere cube data
!
       write(*,*) 'const_squre_circle_edge_data'
      call const_squre_circle_edge_data
!
!   construct whole grid
!
      call construct_circle_mesh
!
      end program mesh_circle_kemo
