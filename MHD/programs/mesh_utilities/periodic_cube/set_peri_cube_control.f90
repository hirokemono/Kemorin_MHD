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
      character(len=kchara) :: tmpchara
!
!
      iflag_domain_shell = 1
!      if(domain_shape_ctl%iflag .gt. 0) then
!        tmpchara = domain_shape_ctl%charavalue
!        if     (cmp_no_case(tmpchara, 'sphere')) then
!          iflag_domain_shell = 1
!        else if(cmp_no_case(tmpchara, 'spherical_shell')) then
!          iflag_domain_shell = 2
!        end if
!      end if
!      write(*,*) 'domain type', iflag_domain_shell,                    &
!     &            trim(tmpchara)
!
      iflag_mesh = 2
      if(divide_type_ctl%iflag .gt. 0) then
        tmpchara = divide_type_ctl%charavalue
        if     (cmp_no_case(tmpchara, 'cube')) then
          iflag_mesh = 1
        else if(cmp_no_case(tmpchara, 'sphere' )) then
          iflag_mesh = 2
        end if
      end if
      write(*,*) 'divide_type_ctl', iflag_mesh, trim(tmpchara)
!
!
      iflag_quad = 1
      if(high_ele_type_ctl%iflag .gt. 0) then
        tmpchara = high_ele_type_ctl%charavalue
        if     (cmp_no_case(tmpchara, 'quad')                           &
     &     .or. cmp_no_case(tmpchara, 'quadrature')) then
          iflag_quad = 1
        else if(cmp_no_case(tmpchara, 'linear')) then
          iflag_quad = 0
        end if
      end if
      write(*,*) iflag_quad, trim(tmpchara)
!
!
      num_hemi = 4
      if(numele_4_90deg%iflag .gt. 0) then
        num_hemi =       numele_4_90deg%intvalue
      end if
!
      n_shell = radial_pnt_ctl%num
!
      nr_adj = 1
      if(nend_adjust_ctl%iflag .gt. 0) then
        nr_adj =  nend_adjust_ctl%intvalue
      end if

      if(nstart_cube_ctl%iflag .gt. 0) then
         nr_back = nstart_cube_ctl%intvalue
      else
         nr_back = n_shell
      end if
!
      ncube_vertical = num_hemi
      if(numele_4_vertical_ctl%iflag .gt. 0) then
        ncube_vertical = numele_4_vertical_ctl%iflag
      end if
!
      write(*,*) 'n_shell', n_shell
      write(*,*) 'nr_adj', nr_adj
      write(*,*) 'nr_back', nr_back
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
      if (nlayer_ICB_ctl%iflag .gt. 0) then
        nlayer_ICB = nlayer_ICB_ctl%intvalue
      else
        nlayer_ICB = 1
      end if
!
      if (nlayer_CMB_ctl%iflag .gt. 0) then
        nlayer_CMB = nlayer_CMB_ctl%intvalue
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
      max_coarse_level = sph_coarsing_ctl%num
      call allocate_coarsing_parameter
!
      icoarse_level(1:max_coarse_level,1)                               &
     &      = sph_coarsing_ctl%int1(1:max_coarse_level)
      icoarse_level(1:max_coarse_level,2)                               &
     &      = sph_coarsing_ctl%int2(1:max_coarse_level)
!
      call dealloc_control_array_i2(sph_coarsing_ctl)
!
      end subroutine set_peri_cube_paramteres
!
!   --------------------------------------------------------------------
!
      end module set_peri_cube_control
