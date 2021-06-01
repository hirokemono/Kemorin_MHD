!>@file  t_control_params_stereo_pvr.f90
!!       module t_control_params_stereo_pvr
!!
!!@author H. Matsui
!!@date   Programmed in May. 2006
!
!> @brief Structures for parameteres for stereo volume rendering
!!
!!@verbatim
!!      real(kind = kreal) function each_eye_from_middle                &
!!     &                 (i_img, num_img, pi_180, stereo_def)
!!        integer(kind = kint), intent(in) :: i_img, num_img
!!        real(kind = kreal), intent(in) :: pi_180
!!        type(pvr_stereo_parameter), intent(in) :: stereo_def
!!      subroutine set_pvr_stereo_control(pvr_ctl, stereo_def)
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!!        type(pvr_stereo_parameter), intent(inout) :: stereo_def
!!@endverbatim
!
      module t_control_params_stereo_pvr
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
!>  Stereo view parameters
      type pvr_stereo_parameter
!>    Defined flag for stereo view
        logical :: flag_stereo_pvr = .FALSE.
!>    Flag to make quilt images with fixed view
        logical :: flag_quilt =      .FALSE.
!
!>    Number of row and column of image array (horizontal, vertical)
        integer(kind = kint) :: n_row_column_view(2) = 0
!
!>    Flag to defeine eye separation by angle
        logical :: flag_eye_separation_angle =  .FALSE.
!>    Flag to stepping eye position by angle
        logical :: flag_setp_eye_separation_angle =  .FALSE.
!
!>    Focal length for streo view
        real(kind = kreal) :: focalLength = one
!>    Eye separation for streo view
        real(kind = kreal) :: eye_separation = zero
!>    Eye separation angle for streo view
        real(kind = kreal) :: eye_sep_angle = zero
      end type pvr_stereo_parameter
!
!  ---------------------------------------------------------------------
!
      contains
!
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
      subroutine set_pvr_stereo_control(pvr_ctl, stereo_def)
!
      use t_control_data_4_pvr
      use set_area_4_viz
      use skip_comment_f
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
      type(pvr_stereo_parameter), intent(inout) :: stereo_def
!
!
      stereo_def%flag_stereo_pvr = .FALSE.
      stereo_def%flag_quilt =      .FALSE.
      if(yes_flag(pvr_ctl%streo_ctl%charavalue)) then
        stereo_def%flag_stereo_pvr = .TRUE.
      else if(yes_flag(pvr_ctl%quilt_ctl%charavalue)) then
        stereo_def%flag_quilt =      .TRUE.
      end if
!
      if(stereo_def%flag_quilt) then
        if(pvr_ctl%quilt_c%i_quilt_image .eq. 0) then
          stereo_def%flag_quilt =      .FALSE.
        else
          stereo_def%n_row_column_view(1:2)                             &
     &        =    pvr_ctl%quilt_c%num_row_column_ctl%intvalue(1:2)
        end if
      end if
!
      end subroutine set_pvr_stereo_control
!
!  ---------------------------------------------------------------------
!
      end module t_control_params_stereo_pvr
