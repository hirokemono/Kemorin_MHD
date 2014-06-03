!
!      module set_peri_cube_control
!
!        programmed by H.Matsui on Apr., 2006
!
!      subroutine set_peri_cube_paramteres
!
      module set_peri_cube_control
!
      use m_precision
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_peri_cube_paramteres
!
      use m_numref_cubed_sph
      use m_cubed_sph_radius
      use m_cubed_sph_grp_param
      use m_control_data_cubed_sph
!
      integer(kind = kint) :: i, j, jst, jed
!
!
!      if     (domain_shape_ctl .eq. 'sphere'                           &
!     &   .or. domain_shape_ctl .eq. 'Sphere'                           &
!     &   .or. domain_shape_ctl .eq. 'SPHERE') then
!        iflag_domain_shell = 1
!      else if(domain_shape_ctl .eq. 'spherical_shell'                  &
!     &   .or. domain_shape_ctl .eq. 'Spherical_shell'                  &
!     &   .or. domain_shape_ctl .eq. 'Spherical_Shell'                  &
!     &   .or. domain_shape_ctl .eq. 'SPHERICAL_SHELL') then
!        iflag_domain_shell = 2
!      else
        iflag_domain_shell = 1
!      end if
!      write(*,*) 'domain type', iflag_domain_shell,                    &
!     &            trim(domain_shape_ctl)
!
      if     (divide_type_ctl .eq. 'cube'                               &
     &   .or. divide_type_ctl .eq. 'Cube'                               &
     &   .or. divide_type_ctl .eq. 'CUBE') then
        iflag_mesh = 1
      else if(divide_type_ctl .eq. 'sphere'                             &
     &   .or. divide_type_ctl .eq. 'Sphere'                             &
     &   .or. divide_type_ctl .eq. 'SPHERE') then
        iflag_mesh = 2
      else
        iflag_mesh = 2
      end if
      write(*,*) 'divide_type_ctl', iflag_mesh, trim(divide_type_ctl)
!
!
      if     (high_ele_type_ctl .eq. 'quad'                             &
     &   .or. high_ele_type_ctl .eq. 'Quad'                             &
     &   .or. high_ele_type_ctl .eq. 'QUAD'                             &
     &   .or. high_ele_type_ctl .eq. 'quadrature'                       &
     &   .or. high_ele_type_ctl .eq. 'Quadrature'                       &
     &   .or. high_ele_type_ctl .eq. 'QUADRATURE') then
        iflag_quad = 1
      else if(high_ele_type_ctl .eq. 'linear'                           &
     &   .or. high_ele_type_ctl .eq. 'Linear'                           &
     &   .or. high_ele_type_ctl .eq. 'LINEAR') then
        iflag_quad = 0
      else
        iflag_quad = 1
      end if
      write(*,*) iflag_quad, trim(high_ele_type_ctl)
!
!
      num_hemi =       numele_4_90deg
      ncube_vertical = num_hemi
!
      n_shell = radial_pnt_ctl%num
      nr_adj =  nend_adjust_ctl
      if(i_nstart_cube .gt. 0) then
         nr_back = nstart_cube_ctl
      else
         nr_back = n_shell
      end if
!
      if(i_numele_4_vert .gt. 0) then
        ncube_vertical = numele_4_vertical_ctl
      end if
!
      write(*,*) 'n_shell', n_shell
      write(*,*) 'nr_adj', nr_adj, nend_adjust_ctl
      write(*,*) 'nr_back', nr_back, nstart_cube_ctl
!
      call allocate_shell_radius
!
      do i = 1, n_shell
        j = radial_pnt_ctl%ivec(i)
        r_nod(j) = radial_pnt_ctl%vect(i)
      end do
      call dealloc_control_array_i_r(radial_pnt_ctl)
!
!   set ICB and CMB address
!
      if (i_nlayer_ICB .gt. 0) then
        nlayer_ICB = nlayer_ICB_ctl
      else
        nlayer_ICB = 1
      end if
!
      if (i_nlayer_CMB .gt. 0) then
        nlayer_CMB = nlayer_CMB_ctl
      else
        nlayer_CMB = n_shell
      end if
!
      nlayer_EXT = n_shell
!
!   set node group table
!
      num_node_grp_csp =  0
      num_nod_layer_csp = 0
      call allocate_nod_grp_name_csp
      call allocate_nod_grp_layer_csp
!
      do j = 1, num_node_grp_csp
        jst = istack_nod_grp_layer_csp(j-1) + 1
        jed = istack_nod_grp_layer_csp(j)
        write(*,*) j, istack_nod_grp_layer_csp(j),                      &
     &      trim(nod_grp_name_csp(j))
        write(*,*) id_nod_grp_layer_csp(jst:jed)
      end do
!
!   set element group table
!
      num_ele_grp_csp =   0
      num_ele_layer_csp = 0
      call allocate_ele_grp_name_csp
      call allocate_ele_grp_layer_csp
!
!   set surface group table
!
      num_surf_grp_csp =   0
      num_surf_layer_csp = 0
      call allocate_surf_grp_name_csp
      call allocate_surf_grp_layer_csp
!
!
!
      max_coarse_level = num_level_coarse
      call allocate_coarsing_parameter
!
      icoarse_level(1:max_coarse_level,1)                               &
     &      = sp_r_coarse_ratio(1:max_coarse_level,1)
      icoarse_level(1:max_coarse_level,2)                               &
     &      = sp_r_coarse_ratio(1:max_coarse_level,2)
!
      end subroutine set_peri_cube_paramteres
!
!   --------------------------------------------------------------------
!
      end module set_peri_cube_control
