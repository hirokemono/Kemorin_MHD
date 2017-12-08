!>@file  t_control_params_4_pvr.f90
!!       module t_control_params_4_pvr
!!
!!@author H. Matsui
!!@date   Programmed in May. 2006
!
!> @brief Structures for parameteres for volume rendering
!!
!!@verbatim
!!      subroutine reset_pvr_view_parameteres(view_param)
!!      subroutine alloc_pvr_element_group(fld_prm)
!!      subroutine dealloc_pvr_element_group(fld_prm)
!!
!!      subroutine alloc_pvr_color_parameteres(color)
!!      subroutine alloc_pvr_opacity_list(color)
!!      subroutine alloc_light_posi_in_view(color)
!!      subroutine dealloc_pvr_color_parameteres(color)
!!@endverbatim
!
      module t_control_params_4_pvr
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
      integer(kind = kint), parameter :: IFLAG_NORMAL = 0
      integer(kind = kint), parameter :: IFLAG_LEFT =  -1
      integer(kind = kint), parameter :: IFLAG_RIGHT =  1
!
      real(kind = kint), parameter :: TINY =  1.0D-9
      real(kind = kreal), parameter :: SMALL_RAY_TRACE = 0.1d0
      real(kind = kreal), parameter :: SMALL_NORM = 0.01d0
!
!>  Structure for field parameter for PVR
      type pvr_output_parameter
!>    File prefix for image file
        character(len = kchara) :: pvr_prefix
!>    File format for image file
        integer(kind = kint) :: id_pvr_file_type
!>    Transparent image flag
        integer(kind = kint) :: id_pvr_transparent = 0
!>    Monitoring mode flag
        integer(kind = kint) :: iflag_monitoring = 0
!>    Flag to make an anaglyph
        integer(kind = kint) :: iflag_anaglyph = 0
      end type pvr_output_parameter
!
!>  Structure for field parameter for PVR
      type pvr_field_parameter
!>     Field type for PVR data
        integer(kind = kint) :: id_pvr_output =    0
!>     Component flag for PVR data
        integer(kind = kint) :: icomp_pvr_output = 0
!>     Number of component of data for Rendering
        integer(kind = kint) :: ncomp_pvr_org =    0
!>     Field name of data for Rendering
        character(len = kchara) :: name_pvr_output
!
!>     Number of Element group for volume rendering
        integer(kind = kint) :: nele_grp_area_pvr = 0
!>     Element group list for volume rendering
        integer(kind = kint), allocatable :: id_ele_grp_area_pvr(:)
      end type pvr_field_parameter
!
!
!>  Structure for view parameteres
      type pvr_view_parameter
!>    Number of pixels for image
        integer(kind = kint) :: n_pvr_pixel(2) = (/0,0/)
!
!>    Defined flag for orthogonal view
        integer(kind = kint) :: iflag_ortho_mat = 0
!>    Orthogonal projection parameter
        real(kind = kreal) :: ortho_mat(3) = (/zero,zero,zero/)
!>    Orthogonal projection matrix
        real(kind = kreal) :: ortho_pvr(2) = (/zero,zero/)
!
!>    Defined flag for perspective view
        integer(kind = kint) :: iflag_perspective = 0
!>    Apature of perspective view
        real(kind = kreal) :: perspective_angle = zero
!>    Aspect ratio between horiaontal and vertical
        real(kind = kreal) :: perspective_xy_ratio = zero
!>    Near distance for perspective view
        real(kind = kreal) :: perspective_near = zero
!>    Farther distance for perspective view
        real(kind = kreal) :: perspective_far = zero
!>    perspective projection matrix
        real(kind = kreal) :: projection_mat(4,4)
!
!
!>    Defined flag for modelview matrix
        integer(kind = kint) :: iflag_modelview_mat = 0
!>    Modelview matrix
        real(kind = kreal) :: modelview_mat(4,4)
!>    Inverse of modelview matrix
        real(kind = kreal) :: modelview_inv(4,4)
!
!
!>    Defined flag for view rotation
        integer(kind = kint) :: iflag_rotation = 0
!>    View rotatin
        real(kind = kreal) :: rotation_pvr(4) = (/zero,zero,zero,zero/)
!
!>    Defined flag for scale factor
        integer(kind = kint) :: iflag_scale_fact = 0
!>    Scale factor
        real(kind = kreal) :: scale_factor_pvr(3) = (/zero,zero,zero/)
!
!>    Defined flag for eye point in viewer coordinate
        integer(kind = kint) :: iflag_viewpt_in_view = 0
!>    Position of eye point in viewer coordinate
        real(kind = kreal) :: viewpt_in_viewer_pvr(4)                   &
     &                       = (/zero,zero,zero,zero/)
!
!>    Defined flag for lookatpoint
        integer(kind = kint) :: iflag_lookpoint = 0
!>    Position to look at
        real(kind = kreal) :: lookat_vec(3) = (/zero,zero,zero/)
!
!>    Defined flag for up-direction
        integer(kind = kint) :: iflag_updir = 0
!>    Vector for up-direction
        real(kind = kreal) :: up_direction_vec(3) = (/zero,zero,zero/)
!
!>    Defined flag for stereo view
        integer(kind = kint) :: iflag_stereo_pvr = 0
!>    Perspective projection matrix for left eye
        real(kind = kreal) :: projection_left(4,4)
!>    Perspective projection matrix for right eye
        real(kind = kreal) :: projection_right(4,4)
!>    Original projection matrix for backup
        real(kind = kreal) :: projection_saved(4,4)
!
!>    Focal length for streo view
        real(kind = kreal) :: focalLength = one
!>    Eye separation for streo view
        real(kind = kreal) :: eye_separation = zero
!
!>    Rotation flag
        integer(kind = kint) :: iflag_rotate_snap = 0
!>    Prametere for rotation
!!@n        rotatin axis:    iprm_pvr_rot(1)
!!@n        number of frame: iprm_pvr_rot(2)
        integer(kind = kint) :: iprm_pvr_rot(2) = (/0,0/)
!>     Rotation start step
        integer(kind = kint) :: istart_rot = 0
!>     Rotation end step
        integer(kind = kint) :: iend_rot =   0
      end type pvr_view_parameter
!
!
!>  Structure for PVR colormap parameters
      type pvr_colormap_parameter
!>    Colormap IDs
!!@n        pvr_colormap(:) =        id_pvr_color(1)
!!@n        pvr_data_mapping(:) =    id_pvr_color(2)
!!@n        opacity_style(:) =       find_dis_minmax(3)
        integer(kind = kint) :: id_pvr_color(3) = (/0,0,0/)
!
!>    Number of data points to define color
        integer(kind = kint) :: num_pvr_datamap_pnt = 0
!>    Data and corresponding color value
!!@n        Field data:  pvr_datamap_param(1,:)
!!@n        Color data:  pvr_datamap_param(2,:)
        real(kind = kreal), allocatable :: pvr_datamap_param(:,:)
!
!>    Number of data points to define color
        integer(kind = kint) :: num_opacity_pnt = 0
!>     Maximum opacity for colorbar
        real(kind = kreal) :: pvr_max_opacity = zero
!>     Opacity data table
!!@n        pvr_opacity_dat_low(:) =  pvr_opacity_param(1,:)
!!@n        pvr_opacity_dat_high(:) = pvr_opacity_param(2,:)
!!@n        pvr_opacity_opacity(:) =  pvr_opacity_param(3,:)
!!@n        ambient_opacity:  pvr_opacity_param(3,(num_opacity_pnt(:)+1))
        real(kind = kreal), allocatable :: pvr_opacity_param(:,:)
!
!>    Defined flag for lights
        integer(kind = kint) :: iflag_pvr_lights = 0
!>        Number of lights
        integer(kind = kint) :: num_pvr_lights = 0
!!@n        ambient_coef(:) =  pvr_lighting_real(1,:)
!!@n        diffuse_coef(:) =  pvr_lighting_real(2,:)
!!@n        specular_coef(:) = pvr_lighting_real(3,:)
        real(kind = kreal) :: pvr_lighting_real(3) = (/zero,zero,zero/)
!>    Position of lights
        real(kind = kreal), allocatable :: xyz_pvr_lights(:,:)
!>    Position of lights in viewer coordinates
        real(kind = kreal), allocatable :: view_pvr_lights(:,:)
      end type pvr_colormap_parameter
!
!>  Structure for PVR colorbar parameters
      type pvr_colorbar_parameter
!>    Draw flag for color bar
        integer(kind = kint) :: iflag_pvr_colorbar =  0
!>    Draw flag for color bar numbers
        integer(kind = kint) :: iflag_pvr_cbar_nums = 0
!>    Draw flag for zero line in color bar
        integer(kind = kint) :: iflag_pvr_zero_mark = 0
!>    Scaling for number font
        integer(kind = kint) :: iscale_font = 1
!>    Thicknsess of colorbar
        integer(kind = kint) :: ntick_pvr_colorbar =  3
!>    Range of colorbar
        real(kind = kreal) :: cbar_range(2) = (/zero,one/)
!
!>    Draw flag for axis label
        integer(kind = kint) :: iflag_pvr_axis =  0
      end type pvr_colorbar_parameter
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
        view_param%projection_mat(1:4,1:4) =   0.0d0
!
        view_param%modelview_mat(1:4,1:4) =   0.0d0
        view_param%modelview_inv(1:4,1:4) =   0.0d0
!
        view_param%projection_left(1:4,1:4) =  0.0d0
        view_param%projection_right(1:4,1:4) = 0.0d0
!
      end subroutine reset_pvr_view_parameteres
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_element_group(fld_prm)
!
      type(pvr_field_parameter), intent(inout) :: fld_prm
!
      allocate(fld_prm%id_ele_grp_area_pvr(fld_prm%nele_grp_area_pvr))
!
      if(fld_prm%nele_grp_area_pvr .le. 0) return
      fld_prm%id_ele_grp_area_pvr = 0
!
      end subroutine alloc_pvr_element_group
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_element_group(fld_prm)
!
      type(pvr_field_parameter), intent(inout) :: fld_prm
!
      deallocate(fld_prm%id_ele_grp_area_pvr)
!
      end subroutine dealloc_pvr_element_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_color_parameteres(color)
!
      type(pvr_colormap_parameter), intent(inout) :: color
!
!
      allocate(color%pvr_datamap_param(2,color%num_pvr_datamap_pnt) )
      if(color%num_pvr_datamap_pnt .gt. 0) then
        color%pvr_datamap_param = 0.0d0
      end if
!
      end subroutine alloc_pvr_color_parameteres
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_opacity_list(color)
!
      type(pvr_colormap_parameter), intent(inout) :: color
!
!
      allocate(color%pvr_opacity_param(3,color%num_opacity_pnt+1) )
      if(color%num_opacity_pnt .gt. 0) then
        color%pvr_opacity_param = 0.0d0
      end if
!
      end subroutine alloc_pvr_opacity_list
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_light_posi_in_view(color)
!
      type(pvr_colormap_parameter), intent(inout) :: color
!
!
      allocate(color%xyz_pvr_lights(3,color%num_pvr_lights) )
      allocate(color%view_pvr_lights(3,color%num_pvr_lights) )
      if (color%num_pvr_lights .le. 0) return
      color%xyz_pvr_lights =    0.0d0
      color%view_pvr_lights =   0.0d0
!
      end subroutine alloc_light_posi_in_view
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_color_parameteres(color)
!
      type(pvr_colormap_parameter), intent(inout) :: color
!
!
      deallocate(color%pvr_datamap_param)
      deallocate(color%pvr_opacity_param)
      deallocate(color%xyz_pvr_lights)
      deallocate(color%view_pvr_lights)
!
      end subroutine dealloc_pvr_color_parameteres
!
!  ---------------------------------------------------------------------
!
      end module t_control_params_4_pvr
