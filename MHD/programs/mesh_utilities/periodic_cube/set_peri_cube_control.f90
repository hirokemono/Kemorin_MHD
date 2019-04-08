!
!      module set_peri_cube_control
!
!        programmed by H.Matsui on Apr., 2006
!
!!      subroutine set_peri_cube_paramteres                             &
!!     &         (cubed_sph_c, rprm_csph, csph_grp, csph_p, course_p)
!!        type(control_data_cubed_sph), intent(in) :: cubed_sph_c
!!        type(cubed_sph_radius), intent(inout) :: rprm_csph
!!        type(cubed_sph_group), intent(inout) :: csph_grp
!!        type(numref_cubed_sph), intent(inout) :: csph_p
!!        type(coarse_cubed_sph), intent(inout) :: course_p
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
      subroutine set_peri_cube_paramteres                               &
     &         (cubed_sph_c, rprm_csph, csph_grp, csph_p, course_p)
!
      use t_numref_cubed_sph
      use t_control_data_cubed_sph
      use t_cubed_sph_radius
      use t_cubed_sph_grp_param
      use set_cubed_sph_control
!
      type(control_data_cubed_sph), intent(in) :: cubed_sph_c
      type(cubed_sph_radius), intent(inout) :: rprm_csph
      type(cubed_sph_group), intent(inout) :: csph_grp
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
!      write(*,*) 'domain type', csph_p%iflag_domain_shell,             &
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
      call set_cubed_sph_radius_ctl(cubed_sph_c, rprm_csph)
      call set_cubed_sph_grid_ctl                                       &
     &   (cubed_sph_c, rprm_csph, csph_p, csph_grp)
!
!   set empty group table
!
      call set_empty_cubed_sph_group(csph_grp)
!
      course_p%max_coarse_level = cubed_sph_c%sph_coarsing_ctl%num
      call alloc_coarsing_parameter(course_p)
!
      do j = 1, course_p%max_coarse_level
        course_p%icoarse_level(j,1)                                     &
     &     = cubed_sph_c%sph_coarsing_ctl%int1(j)
        course_p%icoarse_level(j,2)                                     &
     &     = cubed_sph_c%sph_coarsing_ctl%int2(j)
      end do
!
      end subroutine set_peri_cube_paramteres
!
!   --------------------------------------------------------------------
!
      end module set_peri_cube_control
