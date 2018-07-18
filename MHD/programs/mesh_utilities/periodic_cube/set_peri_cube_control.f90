!
!      module set_peri_cube_control
!
!        programmed by H.Matsui on Apr., 2006
!
!!      subroutine set_peri_cube_paramteres(cubed_sph_c)
!!        type(control_data_cubed_sph), intent(in) :: cubed_sph_c
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
      subroutine set_peri_cube_paramteres(cubed_sph_c)
!
      use m_numref_cubed_sph
      use m_cubed_sph_radius
      use m_cubed_sph_grp_param
      use t_control_data_cubed_sph
      use set_cubed_sph_control
!
      type(control_data_cubed_sph), intent(in) :: cubed_sph_c
!
      integer(kind = kint) :: i, j, jst, jed
      character(len=kchara) :: tmpchara
!
!
      iflag_domain_shell = 1
!      if(cubed_sph_c%domain_shape_ctl%iflag .gt. 0) then
!        tmpchara = cubed_sph_c%domain_shape_ctl%charavalue
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
      if(cubed_sph_c%divide_type_ctl%iflag .gt. 0) then
        tmpchara = cubed_sph_c%divide_type_ctl%charavalue
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
      if(cubed_sph_c%high_ele_type_ctl%iflag .gt. 0) then
        tmpchara = cubed_sph_c%high_ele_type_ctl%charavalue
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
      call set_cubed_sph_grid_ctl(cubed_sph_c)
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
      max_coarse_level = cubed_sph_c%sph_coarsing_ctl%num
      call allocate_coarsing_parameter
!
      icoarse_level(1:max_coarse_level,1)                               &
     &      = cubed_sph_c%sph_coarsing_ctl%int1(1:max_coarse_level)
      icoarse_level(1:max_coarse_level,2)                               &
     &      = cubed_sph_c%sph_coarsing_ctl%int2(1:max_coarse_level)
!
      end subroutine set_peri_cube_paramteres
!
!   --------------------------------------------------------------------
!
      end module set_peri_cube_control
