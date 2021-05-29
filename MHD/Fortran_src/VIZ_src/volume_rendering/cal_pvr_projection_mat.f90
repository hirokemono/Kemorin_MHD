!>@file  cal_pvr_projection_mat.f90
!!       module cal_pvr_projection_mat
!!
!!@author H. Matsui
!!@date   Programmed in May. 2009
!
!> @brief Evaluate projection matirx
!!
!!@verbatim
!!      subroutine set_pvr_orthogonal_params(i_pvr, view_param)
!!
!!      subroutine set_pvr_projection_matrix                            &
!!     &         (i_pvr, view_param, projection_mat)
!!      subroutine set_pvr_step_projection_mat(i_img, num_img,          &
!!     &          view_param, stereo_def, projection_step)
!!      subroutine set_pvr_projection_left_mat                          &
!!     &          (i_pvr, view_param, stereo_def, projection_left)
!!      subroutine set_pvr_projection_right_mat                         &
!!     &          (i_pvr, view_param, stereo_def, projection_right)
!!        type(pvr_view_parameter), intent(in) :: view_param
!!        type(pvr_stereo_parameter), intent(in) :: stereo_def
!!@endverbatim
!
      module cal_pvr_projection_mat
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_control_params_4_pvr
!
      implicit none
!
      private :: each_eye_from_middle
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_pvr_orthogonal_params(i_pvr, view_param)
!
      integer(kind = kint), intent(in) :: i_pvr
      type(pvr_view_parameter), intent(inout) :: view_param
!
!
        if (view_param%iflag_perspective .eq. 0) then
          view_param%ortho_pvr(1)                                       &
     &        = max(view_param%ortho_pvr(1), view_param%ortho_pvr(2))
          view_param%ortho_pvr(2) = view_param%ortho_pvr(1)
        end if
!
        if (iflag_debug .gt. 0) then
          write(*,*) 'Orthogonal parameter for PVR ', i_pvr
          write(*,*) 'perspective_angle',                               &
     &              view_param%perspective_angle
          write(*,*) 'perspective_xy_ratio',                            &
     &               view_param%perspective_xy_ratio
          write(*,*) 'perspective_near',  view_param%perspective_near
          write(*,*) 'perspective_far',  view_param%perspective_far
          write(*,*) 'ortho_pvr',  view_param%ortho_pvr(1:2)
        end if
!
      end subroutine set_pvr_orthogonal_params
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_projection_matrix                              &
     &         (i_pvr, view_param, projection_mat)
!
      use set_projection_matrix
!
      integer(kind = kint), intent(in) :: i_pvr
      type(pvr_view_parameter), intent(in) :: view_param
      real(kind = kreal), intent(inout) :: projection_mat(4,4)
!
      integer(kind = kint) :: i
!
!
        call set_perspective_mat_by_angle(view_param%perspective_angle, &
     &    view_param%perspective_xy_ratio, view_param%perspective_near, &
     &    view_param%perspective_far, projection_mat)
!
        if (iflag_debug .gt. 0) then
          write(*,*) 'projection_mat for PVR ', i_pvr
          do i = 1, 4
            write(*,'(1p4e16.7)') projection_mat(i,1:4)
          end do
        end if
!
      end subroutine set_pvr_projection_matrix
!
! -----------------------------------------------------------------------
!
      subroutine set_pvr_step_projection_mat(i_img, num_img,            &
     &          view_param, stereo_def, projection_step)
!
      use set_projection_matrix
!
      integer(kind = kint), intent(in) :: i_img, num_img
      type(pvr_view_parameter), intent(in) :: view_param
      type(pvr_stereo_parameter), intent(in) :: stereo_def
      real(kind = kreal), intent(inout) :: projection_step(4,4)
!
      integer(kind = kint) :: i
!
      real(kind = kreal) :: pi_180, wd2, ndfl, each_eye
      real(kind = kreal) :: view_right, view_left
      real(kind = kreal) :: view_top, view_bottom
      real(kind = kreal) :: view_far, view_near
!
!
      view_near = view_param%perspective_near
      view_far =  view_param%perspective_far
!
      pi_180 = four * atan(one) / 180.0d0
      wd2 =  view_near * tan(view_param%perspective_angle*pi_180*half)
      ndfl = view_near / stereo_def%focalLength
!
      each_eye                                                          &
     &     = each_eye_from_middle(i_img, num_img, pi_180, stereo_def)
!
      view_bottom = - wd2
      view_top =      wd2
      view_left  = - view_param%perspective_xy_ratio * wd2              &
     &              + half * each_eye * ndfl
      view_right =   view_param%perspective_xy_ratio * wd2              &
     &              + half * each_eye * ndfl
!
      call set_perspective_mat_by_area(view_left, view_right,           &
     &    view_bottom, view_top, view_near, view_far,                   &
     &    projection_step)
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'projection_step for PVR ', i_img
        do i = 1, 4
          write(*,'(1p4e16.7)') projection_step(i,1:4)
        end do
      end if
!
      end subroutine set_pvr_step_projection_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_pvr_projection_left_mat                            &
     &          (i_pvr, view_param, stereo_def, projection_left)
!
      use set_projection_matrix
!
      integer(kind = kint), intent(in) :: i_pvr
      type(pvr_view_parameter), intent(in) :: view_param
      type(pvr_stereo_parameter), intent(in) :: stereo_def
      real(kind = kreal), intent(inout) :: projection_left(4,4)
!
      integer(kind = kint) :: i
!
      real(kind = kreal) :: pi_180, wd2, ndfl, each_eye
      real(kind = kreal) :: view_right, view_left
      real(kind = kreal) :: view_top, view_bottom
      real(kind = kreal) :: view_far, view_near
!
!
      view_near = view_param%perspective_near
      view_far =  view_param%perspective_far
!
      pi_180 = four * atan(one) / 180.0d0
      wd2 =  view_near * tan(view_param%perspective_angle*pi_180*half)
      ndfl = view_near / stereo_def%focalLength
!
      each_eye                                                          &
     &     = each_eye_from_middle(ione, ione, pi_180, stereo_def)
!
        view_bottom = - wd2
        view_top =      wd2
        view_left  = - view_param%perspective_xy_ratio * wd2            &
     &                + half * each_eye * ndfl
        view_right =   view_param%perspective_xy_ratio * wd2            &
     &                + half * each_eye * ndfl
!
        call set_perspective_mat_by_area(view_left, view_right,         &
     &      view_bottom, view_top, view_near, view_far,                 &
     &      projection_left)
!
        if (iflag_debug .gt. 0) then
          write(*,*) 'projection_left for PVR ', i_pvr
          do i = 1, 4
            write(*,'(1p4e16.7)') projection_left(i,1:4)
          end do
        end if
!
      end subroutine set_pvr_projection_left_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_pvr_projection_right_mat                           &
     &          (i_pvr, view_param, stereo_def, projection_right)
!
      use set_projection_matrix
!
      integer(kind = kint), intent(in) :: i_pvr
      type(pvr_view_parameter), intent(in) :: view_param
      type(pvr_stereo_parameter), intent(in) :: stereo_def
      real(kind = kreal), intent(inout) :: projection_right(4,4)
!
      integer(kind = kint) :: i
!
      real(kind = kreal) :: pi_180, wd2, ndfl, each_eye
      real(kind = kreal) :: view_right, view_left
      real(kind = kreal) :: view_top, view_bottom
      real(kind = kreal) :: view_far, view_near
!
!
      view_near = view_param%perspective_near
      view_far =  view_param%perspective_far
!
      pi_180 = four * atan(one) / 180.0d0
      wd2 =  view_near * tan(view_param%perspective_angle*pi_180*half)
      ndfl = view_near / stereo_def%focalLength
!
      each_eye                                                          &
     &     = each_eye_from_middle(ione, ione, pi_180, stereo_def)
!
        view_bottom = - wd2
        view_top =      wd2
        view_left  = - view_param%perspective_xy_ratio * wd2            &
     &                - half * each_eye * ndfl
        view_right =   view_param%perspective_xy_ratio * wd2            &
     &                - half * each_eye * ndfl
!
        call set_perspective_mat_by_area(view_left, view_right,         &
     &      view_bottom, view_top, view_near, view_far,                 &
     &      projection_right)
!
        if (iflag_debug .gt. 0) then
          write(*,*) 'projection_right for PVR ', i_pvr
          do i = 1, 4
            write(*,'(1p4e16.7)') projection_right(i,1:4)
          end do
        end if
!
      end subroutine set_pvr_projection_right_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      real(kind = kreal) function each_eye_from_middle                  &
     &                 (i_img, num_img, pi_180, stereo_def)
!
      integer(kind = kint), intent(in) :: i_img, num_img
      real(kind = kreal), intent(in) :: pi_180
      type(pvr_stereo_parameter), intent(in) :: stereo_def
!
      real(kind = kreal) :: range, rstep, each_eye, separation
!
!
      rstep = half - dble(i_img-1) / dble(num_img-1)
      if(stereo_def%flag_setp_eye_separation_angle) then
        if(stereo_def%flag_eye_separation_angle) then
          each_eye = stereo_def%focalLength                             &
     &              * tan(pi_180 * rstep * stereo_def%eye_sep_angle)
        else
          range = two * atan(half*stereo_def%eye_separation             &
     &                       / stereo_def%focalLength)
          each_eye = stereo_def%focalLength * tan(rstep * range)
        end if
      else
        if(stereo_def%flag_eye_separation_angle) then
          separation = stereo_def%focalLength                           &
     &                * tan(half * pi_180 * stereo_def%eye_sep_angle)
        else
          separation = stereo_def%eye_separation
        end if
        each_eye = separation * rstep
      end if
      each_eye_from_middle = each_eye
!
      end function each_eye_from_middle
!
! -----------------------------------------------------------------------
!
      end module cal_pvr_projection_mat
