!
!      module set_cubed_sph_control
!
!        programmed by H.Matsui on Apr., 2006
!
!!      subroutine set_shell_paramteres                                 &
!!     &         (cubed_sph_c, rprm_csph, csph_p, course_p)
!!        type(control_data_cubed_sph), intent(in) :: cubed_sph_c
!!        type(cubed_sph_radius), intent(inout) :: rprm_csph
!!        type(numref_cubed_sph), intent(inout) :: csph_p
!!        type(coarse_cubed_sph), intent(inout) :: course_p
!!      subroutine set_cubed_sph_grid_ctl                               &
!!     &         (cubed_sph_c, rprm_csph, csph_p)
!!        type(control_data_cubed_sph), intent(in) :: cubed_sph_c
!!        type(cubed_sph_radius), intent(in) :: rprm_csph
!!        type(numref_cubed_sph), intent(inout) :: csph_p
!
      module set_cubed_sph_control
!
      use m_precision
!
      implicit  none
!
      private :: set_cubed_sph_coarsing_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_shell_paramteres                                   &
     &         (cubed_sph_c, rprm_csph, csph_p, course_p)
!
      use m_cubed_sph_grp_param
      use t_numref_cubed_sph
      use t_control_data_cubed_sph
      use t_cubed_sph_radius
      use skip_comment_f
      use set_cubed_sph_group_ctl
!
      type(control_data_cubed_sph), intent(in) :: cubed_sph_c
      type(cubed_sph_radius), intent(inout) :: rprm_csph
      type(numref_cubed_sph), intent(inout) :: csph_p
      type(coarse_cubed_sph), intent(inout) :: course_p
!
      integer(kind = kint) :: j, jst, jed
      character(len=kchara) :: tmpchara
!
!
      csph_p%iflag_domain_shell = 1
!      if(cubed_sph_c%domain_shape_ctl%iflag .gt. 0) then
!        tmpchara = cubed_sph_c%domain_shape_ctl%charavalue
!        if     (cmp_no_case(tmpchara, 'sphere')) then
!          csph_p%iflag_domain_shell = 1
!        else if(cmp_no_case(tmpchara, 'spherical_shell')) then
!          csph_p%iflag_domain_shell = 2
!        end if
!      end if
!      write(*,*) 'domain type', csph_p%iflag_domain_shell,                    &
!     &            trim(tmpchara)
!
      csph_p%iflag_mesh = 2
      if(cubed_sph_c%divide_type_ctl%iflag .gt. 0) then
        tmpchara = cubed_sph_c%divide_type_ctl%charavalue
        if     (cmp_no_case(tmpchara, 'cube')) then
          csph_p%iflag_mesh = 1
        else if(cmp_no_case(tmpchara, 'sphere' )) then
          csph_p%iflag_mesh = 2
        end if
      end if
      write(*,*) 'divide_type_ctl', csph_p%iflag_mesh, trim(tmpchara)
!
!
      csph_p%iflag_quad = 1
      if(cubed_sph_c%high_ele_type_ctl%iflag .gt. 0) then
        tmpchara = cubed_sph_c%high_ele_type_ctl%charavalue
        if     (cmp_no_case(tmpchara, 'quad')                           &
     &     .or. cmp_no_case(tmpchara, 'quadrature')) then
          csph_p%iflag_quad = 1
        else if(cmp_no_case(tmpchara, 'linear')) then
          csph_p%iflag_quad = 0
        end if
      end if
      write(*,*) csph_p%iflag_quad, trim(tmpchara)
!
!
!   set cubed sphere dimension
      call set_cubed_sph_radius_ctl(cubed_sph_c, rprm_csph)
      call set_cubed_sph_grid_ctl(cubed_sph_c, rprm_csph, csph_p)
!
!   set node group table
      call set_cubed_sph_node_grp_ctl                                   &
     &  (cubed_sph_c%node_grp_name_ctl, cubed_sph_c%node_grp_layer_ctl)
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
      call set_cubed_sph_element_grp_ctl                                &
     &  (cubed_sph_c%elem_grp_name_ctl, cubed_sph_c%elem_grp_layer_ctl)
!
      do j = 1, num_ele_grp_csp
        jst = istack_ele_grp_layer_csp(j-1) + 1
        jed = istack_ele_grp_layer_csp(j)
        write(*,*) j, istack_ele_grp_layer_csp(j),                      &
     &      trim(ele_grp_name_csp(j))
        write(*,*) id_ele_grp_layer_csp(jst:jed)
      end do
!
!   set surface group table
      call set_cubed_sph_surface_grp_ctl                                &
     &  (cubed_sph_c%surf_grp_name_ctl, cubed_sph_c%surf_grp_layer_ctl)
!
      do j = 1, num_surf_grp_csp
        jst = istack_surf_grp_layer_csp(j-1) + 1
        jed = istack_surf_grp_layer_csp(j)
        write(*,*) j, istack_surf_grp_layer_csp(j),                     &
     &      trim(surf_grp_name_csp(j))
        write(*,*) id_surf_grp_layer_csp(1,jst:jed)
        write(*,*) id_surf_grp_layer_csp(2,jst:jed)
      end do
!
!
      call set_cubed_sph_coarsing_ctl                                   &
     &   (cubed_sph_c%sph_coarsing_ctl, course_p)
      call set_cubed_rect_adjusting_ctl                                 &
     &   (cubed_sph_c%edge_latitude_ctl, rprm_csph)
!
      end subroutine set_shell_paramteres
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_grid_ctl                                 &
     &         (cubed_sph_c, rprm_csph, csph_p)
!
      use m_cubed_sph_grp_param
      use t_numref_cubed_sph
      use t_control_data_cubed_sph
      use t_cubed_sph_radius
      use skip_comment_f
      use set_cubed_sph_group_ctl
!
      type(control_data_cubed_sph), intent(in) :: cubed_sph_c
      type(cubed_sph_radius), intent(in) :: rprm_csph
      type(numref_cubed_sph), intent(inout) :: csph_p
!
!
      csph_p%num_hemi = 4
      if(cubed_sph_c%numele_4_90deg%iflag .gt. 0) then
        csph_p%num_hemi = cubed_sph_c%numele_4_90deg%intvalue
      end if
!
      csph_p%ncube_vertical = csph_p%num_hemi
      if(cubed_sph_c%numele_4_vertical_ctl%iflag .gt. 0) then
        csph_p%ncube_vertical                                           &
     &          = cubed_sph_c%numele_4_vertical_ctl%intvalue
      end if
!
!   set ICB and CMB address
!
      if (cubed_sph_c%nlayer_ICB_ctl%iflag .gt. 0) then
        nlayer_ICB = cubed_sph_c%nlayer_ICB_ctl%intvalue
      else
        nlayer_ICB = 1
      end if
!
      if (cubed_sph_c%nlayer_CMB_ctl%iflag .gt. 0) then
        nlayer_CMB = cubed_sph_c%nlayer_CMB_ctl%intvalue
      else
        nlayer_CMB = rprm_csph%n_shell
      end if
!
      nlayer_EXT = rprm_csph%n_shell
!
      end subroutine set_cubed_sph_grid_ctl
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_coarsing_ctl                             &
     &         (sph_coarsing_ctl, course_p)
!
      use m_cubed_sph_grp_param
      use t_numref_cubed_sph
      use t_read_control_arrays
      use skip_comment_f
!
      type(ctl_array_i2), intent(in) :: sph_coarsing_ctl
      type(coarse_cubed_sph), intent(inout) :: course_p
!
!
      course_p%max_coarse_level = sph_coarsing_ctl%num
      write(*,*) 'max_coarse_level', course_p%max_coarse_level
      call alloc_coarsing_parameter(course_p)
!
      course_p%icoarse_level(1:course_p%max_coarse_level,1)             &
     &      = sph_coarsing_ctl%int1(1:course_p%max_coarse_level)
      course_p%icoarse_level(1:course_p%max_coarse_level,2)             &
     &      = sph_coarsing_ctl%int2(1:course_p%max_coarse_level)
!
      end subroutine set_cubed_sph_coarsing_ctl
!
!   --------------------------------------------------------------------
!
      end module set_cubed_sph_control
