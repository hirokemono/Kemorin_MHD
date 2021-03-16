!>@file  t_geometries_in_pvr_screen.f90
!!       module t_geometries_in_pvr_screen
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine alloc_projected_position(node, surf, pvr_screen)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!      subroutine dealloc_projected_position(pvr_screen)
!!
!!      subroutine allocate_nod_data_4_pvr                              &
!!     &         (numnod, numele, num_sf_grp, draw_param)
!!      subroutine dealloc_nod_data_4_pvr(draw_param)
!!
!!      subroutine alloc_pvr_sections(draw_param)
!!      subroutine alloc_pvr_isosurfaces(draw_param)
!!
!!      subroutine allocate_pixel_position_pvr(n_pvr_pixel, pixel_xy)
!!      subroutine dealloc_data_4_pvr(draw_param)
!!
!!      subroutine deallocate_projected_data_pvr                        &
!!      &        (num_pvr, proj, draw_param)
!!      subroutine deallocate_pixel_position_pvr(pixel_xy)
!!@endverbatim
!
      module t_geometries_in_pvr_screen
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>  Structure for start points of ray tracing
      type pvr_projected_position
!>    Total number of node in screen coordinate
        integer(kind = kint) :: nnod_screen = 0
!>    Position in modelview coordinate and screen coordinate
!!@n    (Overwritten)
        real(kind = kreal), allocatable :: x_nod_model(:,:)
!
!>    Total number of surface in screen coordinate
        integer(kind = kint) :: nsurf_screen = 0
!>    Opacity value for surface boundaries
        real(kind = kreal), allocatable :: arccos_sf(:)
!
!>    Direction of three axis in screen coordinate
        real(kind = kreal) :: axis_view(3,4)
!>    Order of three axis in screen coordinate
        integer(kind = kint) :: axis_order(3)
      end type pvr_projected_position
!
!
!>  Structure for field data on projected coordinate
      type rendering_parameter
!>    Data for rendering
        real(kind = kreal), allocatable :: d_pvr(:)
!>    Gradient for rendering
        real(kind = kreal), allocatable :: grad_ele(:,:)
!
!>    flag for rendering element
        integer(kind = kint), allocatable :: iflag_used_ele(:)
!
!>    integer flag for surface boundaries
        integer(kind = kint), allocatable :: iflag_enhanse(:)
!>    Opacity value for surface boundaries
        real(kind = kreal), allocatable :: enhansed_opacity(:)
!
!>    Number of sections
        integer(kind = kint) :: num_sections
!>    fiale value for isosurfaces
        real(kind = kreal), allocatable :: coefs(:,:)
!>    Opacity value for isosurfaces
        real(kind = kreal), allocatable :: sect_opacity(:)
!
!>    Number of isosurfaces
        integer(kind = kint) :: num_isosurf
!>    Number of isosurfaces
        integer(kind = kint), allocatable :: itype_isosurf(:)
!>    fiale value for isosurfaces
        real(kind = kreal), allocatable :: iso_value(:)
!>    Opacity value for isosurfaces
        real(kind = kreal), allocatable :: iso_opacity(:)
      end type rendering_parameter
!
!>  Structure for pixel position
      type pvr_pixel_position_type
!>    Number of horizontal pixels
        integer(kind = kint) :: num_pixel_x
!>    Number of vertical pixels
        integer(kind = kint) :: num_pixel_y
!>    Position of horizontal pixels
        real(kind = kreal), allocatable :: pixel_point_x(:)
!>    Position of vertical pixels
        real(kind = kreal), allocatable :: pixel_point_y(:)
      end type pvr_pixel_position_type
!
      private :: alloc_nod_data_4_pvr, alloc_iflag_pvr_used_ele
      private :: alloc_iflag_pvr_boundaries
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_projected_position(node, surf, pvr_screen)
!
      use t_geometry_data
      use t_surface_data
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(pvr_projected_position), intent(inout) :: pvr_screen
!
!
      pvr_screen%nnod_screen = node%numnod
      allocate(pvr_screen%x_nod_model(pvr_screen%nnod_screen,4))
      if(pvr_screen%nnod_screen .gt. 0) pvr_screen%x_nod_model =  0.0d0
!
      pvr_screen%nsurf_screen = surf%numsurf
      allocate(pvr_screen%arccos_sf(pvr_screen%nsurf_screen))
      if(pvr_screen%nsurf_screen .gt. 0) pvr_screen%arccos_sf = 0.0d0
!
      end subroutine alloc_projected_position
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_projected_position(pvr_screen)
!
      type(pvr_projected_position), intent(inout) :: pvr_screen
!
!
      deallocate(pvr_screen%arccos_sf)
      deallocate(pvr_screen%x_nod_model)
!
      end subroutine dealloc_projected_position
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_data_4_pvr                                &
     &         (numnod, numele, num_sf_grp, draw_param)
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: num_sf_grp
      type(rendering_parameter), intent(inout) :: draw_param
!
!
        call alloc_nod_data_4_pvr                                       &
     &     (numnod, numele, draw_param)
        call alloc_iflag_pvr_used_ele(numele, draw_param)
        call alloc_iflag_pvr_boundaries                                 &
     &     (num_sf_grp, draw_param)
!
      end subroutine allocate_nod_data_4_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_nod_data_4_pvr(draw_param)
!
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      deallocate(draw_param%iflag_enhanse, draw_param%enhansed_opacity)
      deallocate(draw_param%iflag_used_ele)
      deallocate(draw_param%d_pvr, draw_param%grad_ele)
!
      end subroutine dealloc_nod_data_4_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_nod_data_4_pvr(nnod, nele, draw_param)
!
      integer(kind = kint), intent(in) :: nnod, nele
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      allocate(draw_param%d_pvr(nnod))
      allocate(draw_param%grad_ele(nele,3))
!
      if(nnod .gt. 0) draw_param%d_pvr =    0.0d0
      if(nele .gt. 0) draw_param%grad_ele = 0.0d0
!
      end subroutine alloc_nod_data_4_pvr
!
! -----------------------------------------------------------------------
!
      subroutine alloc_iflag_pvr_used_ele(nele, draw_param)
!
      integer(kind = kint), intent(in) :: nele
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      allocate(draw_param%iflag_used_ele(nele))
      if(nele .gt. 0) draw_param%iflag_used_ele = 0
!
      end subroutine alloc_iflag_pvr_used_ele
!
! -----------------------------------------------------------------------
!
      subroutine alloc_iflag_pvr_boundaries(num_sf_grp, draw_param)
!
      integer(kind = kint), intent(in) :: num_sf_grp
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      allocate(draw_param%iflag_enhanse(num_sf_grp))
      allocate(draw_param%enhansed_opacity(num_sf_grp))
!
      if(num_sf_grp .gt. 0) draw_param%iflag_enhanse = 0
      if(num_sf_grp .gt. 0) draw_param%enhansed_opacity = 0
!
      end subroutine alloc_iflag_pvr_boundaries
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_pvr_sections(draw_param)
!
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      allocate(draw_param%coefs(10,draw_param%num_sections))
      allocate(draw_param%sect_opacity(draw_param%num_sections))
!
      if(draw_param%num_sections .gt. 0) draw_param%coefs =        zero
      if(draw_param%num_sections .gt. 0) draw_param%sect_opacity = zero
!
      end subroutine alloc_pvr_sections
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pvr_isosurfaces(draw_param)
!
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      allocate(draw_param%itype_isosurf(draw_param%num_isosurf))
      allocate(draw_param%iso_value(draw_param%num_isosurf))
      allocate(draw_param%iso_opacity(draw_param%num_isosurf))
!
      if(draw_param%num_isosurf .gt. 0) draw_param%itype_isosurf = 0
      if(draw_param%num_isosurf .gt. 0) draw_param%iso_value = zero
      if(draw_param%num_isosurf .gt. 0) draw_param%iso_opacity = zero
!
      end subroutine alloc_pvr_isosurfaces
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_sections(draw_param)
!
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      deallocate(draw_param%coefs, draw_param%sect_opacity)
!
      end subroutine dealloc_pvr_sections
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_isosurfaces(draw_param)
!
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      deallocate(draw_param%itype_isosurf)
      deallocate(draw_param%iso_value, draw_param%iso_opacity)
!
      end subroutine dealloc_pvr_isosurfaces
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_pixel_position_pvr(n_pvr_pixel, pixel_xy)
!
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!
!
      pixel_xy%num_pixel_x = n_pvr_pixel(1)
      pixel_xy%num_pixel_y = n_pvr_pixel(2)
      allocate(pixel_xy%pixel_point_x(pixel_xy%num_pixel_x))
      allocate(pixel_xy%pixel_point_y(pixel_xy%num_pixel_y))
!
      pixel_xy%pixel_point_x =  0.0d0
      pixel_xy%pixel_point_y =  0.0d0
!
      end subroutine allocate_pixel_position_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_pixel_position_pvr(pixel_xy)
!
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!
!
      deallocate(pixel_xy%pixel_point_x, pixel_xy%pixel_point_y)
!
      end subroutine deallocate_pixel_position_pvr
!
! -----------------------------------------------------------------------
!
      end module t_geometries_in_pvr_screen
