!>@file  t_surf_grp_4_pvr_domain.f90
!!       module t_surf_grp_4_pvr_domain
!!
!!@author H. Matsui
!!@date   Programmed in May. 2006
!
!> @brief Structures for parameteres for volume rendering
!!
!!@verbatim
!!      subroutine reset_pvr_view_parameteres(view_param)
!!@endverbatim
!
      module t_control_params_4_pvr
!
      use m_precision
!
      implicit  none
!
!>  Structure for view parameteres
      type pvr_view_parameter
!>    Number of pixels for image
        integer(kind = kint) :: n_pvr_pixel(2)
!
!>    Defined flag for orthogonal view
        integer(kind = kint) :: iflag_ortho_mat
!>    Orthogonal projection parameter
        real(kind = kreal) :: ortho_mat(3)
!>    Orthogonal projection matrix
        real(kind = kreal) :: ortho_pvr(2)
!
!>    Defined flag for perspective view
        integer(kind = kint) :: iflag_perspective
!>    Apature of perspective view
        real(kind = kreal) :: perspective_angle
!>    Aspect ratio between horiaontal and vertical
        real(kind = kreal) :: perspective_xy_ratio
!>    Near distance for perspective view
        real(kind = kreal) :: perspective_near
!>    Farther distance for perspective view
        real(kind = kreal) :: perspective_far
!>    perspective projection matrix
        real(kind = kreal) :: projection_mat(16)
!
!
!>    Defined flag for modelview matrix
        integer(kind = kint) :: iflag_modelview_mat
!>    Modelview matrix
        real(kind = kreal) :: modelview_mat(16)
!>    Inverse of modelview matrix
        real(kind = kreal) :: modelview_inv(16)
!
!
!>    Defined flag for view rotation
        integer(kind = kint) :: iflag_rotation
!>    View rotatin
        real(kind = kreal) :: rotation_pvr(4)
!
!>    Defined flag for scale factor
        integer(kind = kint) :: iflag_scale_fact
!>    Scale factor
        real(kind = kreal) :: scale_factor_pvr(3)
!
!>    Defined flag for eye point in viewer coordinate
        integer(kind = kint) :: iflag_viewpt_in_view
!>    Position of eye point in viewer coordinate
        real(kind = kreal) :: viewpt_in_viewer_pvr(4)
!
!>    Defined flag for lookatpoint
        integer(kind = kint) :: iflag_lookpoint
!>    Position to look at
        real(kind = kreal) :: lookat_vec(3)
!
!>    Defined flag for viewpoint
        integer(kind = kint) :: iflag_viewpoint
!>    Position of viewpoint
        real(kind = kreal) :: viewpoint_vec(3)
!
!>    Defined flag for up-direction
        integer(kind = kint) :: iflag_updir
!>    Vector for up-direction
        real(kind = kreal) :: up_direction_vec(3)
!
!>    Defined flag for stereo view
        integer(kind = kint) :: iflag_stereo_pvr
!>    Perspective projection matrix for left eye
        real(kind = kreal) :: projection_left(16)
!>    Perspective projection matrix for right eye
        real(kind = kreal) :: projection_right(16)
!
!>    Focal length for streo view
        real(kind = kreal) :: focalLength
!>    Eye separation for streo view
        real(kind = kreal) :: eye_separation
!
!>    Rotation flag
        integer(kind = kint) :: iflag_rotate_snap = 0
!>    Prametere for rotation
!!@n        rotatin axis:    iprm_pvr_rot(1)
!!@n        number of frame: iprm_pvr_rot(2)
        integer(kind = kint) :: iprm_pvr_rot(2)
      end type pvr_view_parameter
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_view_parameteres(view_param)
!
      type(pvr_view_parameter), intent(inout) :: view_param
!
!
        view_param%n_pvr_pixel(1:2) = 0
!
        view_param%iflag_perspective = 0
        view_param%iflag_ortho_mat =   0
!
        view_param%iflag_modelview_mat =  0
!
        view_param%iflag_rotation = 0
        view_param%iflag_scale_fact = 0
        view_param%iflag_viewpt_in_view = 0
        view_param%iflag_lookpoint =  0
        view_param%iflag_updir =      0
        view_param%iflag_viewpoint =  0
!
        view_param%iflag_stereo_pvr = 0
!
        view_param%iflag_rotate_snap = 0
        view_param%iprm_pvr_rot =      0
!
        view_param%perspective_angle =    0.0d0
        view_param%perspective_xy_ratio = 0.0d0
        view_param%perspective_near =     0.0d0
        view_param%perspective_far =      0.0d0
        view_param%projection_mat(1:16) =   0.0d0
!
!
        view_param%ortho_pvr(1:2) = 0.0d0
        view_param%ortho_mat(1:3) = 0.0d0
!
        view_param%modelview_mat(1:16) =   0.0d0
        view_param%modelview_inv(1:16) =   0.0d0
!
        view_param%rotation_pvr(1:4) =     0.0d0
        view_param%scale_factor_pvr(1:3) = 1.0d0
        view_param%viewpt_in_viewer_pvr(1:4) = 0.0d0
        view_param%lookat_vec(1:3) =       0.0d0
        view_param%viewpoint_vec(1:3) =    0.0d0
        view_param%up_direction_vec(1:3) = 0.0d0
!
        view_param%projection_left(1:16) =  0.0d0
        view_param%projection_right(1:16) = 0.0d0
        view_param%focalLength =          1.0d0
        view_param%eye_separation =       0.0d0
!
      end subroutine reset_pvr_view_parameteres
!
!  ---------------------------------------------------------------------
!
      end module t_control_params_4_pvr
