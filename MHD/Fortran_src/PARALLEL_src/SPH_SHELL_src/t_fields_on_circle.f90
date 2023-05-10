!>@file   t_fields_on_circle.f90
!!@brief  module t_fields_on_circle
!!
!!@author H. Matsui
!!@date Programmed on June., 2013
!
!>@brief  spherical transform at a specific circle at @f$(r, theta)@f$
!!
!!@verbatim
!!      subroutine set_control_circle_def(meq_ctl, circle)
!!         type(mid_equator_control), intent(in) :: meq_ctl
!!         type(fields_on_circle), intent(inout) :: circle
!!      subroutine set_ctl_circle_for_dbench(dbench_ctl, circle)
!!         type(dynamobench_control), intent(in) :: dbench_ctl
!!        type(fields_on_circle), intent(inout) :: circle
!!
!!      subroutine alloc_circle_field                                   &
!!     &         (my_rank, mphi_rtp, nidx_global_jmax, circle, d_circle)
!!      subroutine dealloc_circle_field(my_rank, circle, d_circle)
!!@endverbatim
!!
!!@n @param  ltr      Truncation of spherical harmonics
!!@n @param  jmax     Number of modes for harmonincs except for 0 degree
!!@n @param  numdir   Number of components of field
!!@n @param v_rtp_circle(mphi_circle,numdir)  Field along circle
!
      module t_fields_on_circle
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_phys_data
!
      implicit none
!
!
!>      Use spherical coordinate for circle data
      integer(kind = kint), parameter :: iflag_circle_sph = 1
!>      Use Cylindrical coordinate for circle data
      integer(kind = kint), parameter :: iflag_circle_cyl = 2
!
!>      Structure to make fields on circle
      type fields_on_circle
!>        Flag for coordinate system for circle data
        integer(kind = kint) :: iflag_circle_coord = iflag_circle_sph
!
!>        file name for field data on a circle
        character(len=kchara) :: fname_circle_fld = 'circle_field.dat'
!>        file name for spectr power data on a circle
        character(len=kchara) :: fname_circle_mag                       &
     &                        = 'circle_spec_mag.dat'
!>        file name for spectr phase data on a circle
        character(len=kchara) :: fname_circle_phs                       &
     &                        = 'circle_spec_phase.dat'
!
!>        Number of gird points for a circle
        integer(kind = kint) :: mphi_circle
!>        cylindrical radius for a circle to pick
        real(kind = kreal) :: s_circle
!>        vartical position for a circle to pick
        real(kind = kreal) :: z_circle
!>        radius for a circle to pick
        real(kind = kreal) :: r_circle
!>        colatitude for a circle to pick
        real(kind = kreal) :: colat_circle
!
!>        Inner closest point of circle point of fluid shell
        integer(kind = kint) :: kr_gl_rcirc_in
!>        Outer closest point of circle point of fluid shell
        integer(kind = kint) :: kr_gl_rcirc_out
!>        Inner closest radius of circle point of fluid shell
        real(kind = kreal) :: coef_gl_rcirc_in
!>        Outer closest radius of circle point of fluid shell
        real(kind = kreal) :: coef_gl_rcirc_out
      end type fields_on_circle
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_circle_def(meq_ctl, circle)
!
      use t_ctl_data_mid_equator
      use t_control_array_character3
      use skip_comment_f
!
      type(mid_equator_control), intent(in) :: meq_ctl
      type(fields_on_circle), intent(inout) :: circle
!
      character(len = kchara) :: tmpchara
!
!
      circle%iflag_circle_coord = iflag_circle_sph
      if (meq_ctl%pick_circle_coord_ctl%iflag .ne. 0) then
        tmpchara = meq_ctl%pick_circle_coord_ctl%charavalue
        if(    cmp_no_case(tmpchara,'spherical')                        &
     &    .or. cmp_no_case(tmpchara,'rtp')) then
          circle%iflag_circle_coord = iflag_circle_sph
        else if(cmp_no_case(tmpchara,'cyrindrical')                     &
      &    .or. cmp_no_case(tmpchara,'spz')) then
          circle%iflag_circle_coord = iflag_circle_cyl
        end if
      end if
!
      circle%mphi_circle = -1
      if(meq_ctl%nphi_mid_eq_ctl%iflag .gt. 0) then
        circle%mphi_circle = meq_ctl%nphi_mid_eq_ctl%intvalue
      end if
!
      circle%s_circle = 7.0d0/13.0d0 + 0.5d0
      if(meq_ctl%pick_s_ctl%iflag .gt. 0) then
        circle%s_circle = meq_ctl%pick_s_ctl%realvalue
      end if
!
      circle%z_circle = 0.0d0
      if(meq_ctl%pick_z_ctl%iflag .gt. 0) then
        circle%z_circle = meq_ctl%pick_z_ctl%realvalue
      end if
!
      circle%r_circle = sqrt(circle%s_circle + circle%z_circle**2)
      circle%colat_circle = acos(circle%z_circle / circle%r_circle)
!
      end subroutine set_control_circle_def
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_circle_for_dbench(dbench_ctl, circle)
!
      use t_ctl_data_dynamobench
      use t_control_array_character3
      use skip_comment_f
!
      type(dynamobench_control), intent(in) :: dbench_ctl
      type(fields_on_circle), intent(inout) :: circle
!
!
      circle%iflag_circle_coord = iflag_circle_sph
      circle%mphi_circle = -1
      if(dbench_ctl%nphi_mid_eq_ctl%iflag .gt. 0) then
        circle%mphi_circle = dbench_ctl%nphi_mid_eq_ctl%intvalue
      end if
!
      circle%s_circle = 7.0d0/13.0d0 + 0.5d0
      circle%z_circle = 0.0d0
      circle%r_circle = circle%s_circle
      circle%colat_circle = acos(0.0d0)
!
      end subroutine set_ctl_circle_for_dbench
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_circle_field                                     &
     &         (my_rank, mphi_rtp, nidx_global_jmax, circle, d_circle)
!
      integer, intent(in) :: my_rank
      integer(kind = kint), intent(in) :: mphi_rtp, nidx_global_jmax
      type(fields_on_circle), intent(inout) :: circle
      type(phys_data), intent(inout) :: d_circle
!
      integer(kind = kint) :: jmax_gl, ntot
!
!
      jmax_gl = nidx_global_jmax
      ntot = d_circle%ntot_phys
!
      if(circle%mphi_circle .le. izero) then
        circle%mphi_circle = mphi_rtp
      end if
      call alloc_phys_data(circle%mphi_circle, d_circle)
!
      end subroutine alloc_circle_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_circle_field(my_rank, circle, d_circle)
!
      integer, intent(in) :: my_rank
      type(fields_on_circle), intent(inout) :: circle
      type(phys_data), intent(inout) :: d_circle
!
!
      call dealloc_phys_data(d_circle)
      call dealloc_phys_name(d_circle)
!
      end subroutine dealloc_circle_field
!
! ----------------------------------------------------------------------
!
     end module t_fields_on_circle
