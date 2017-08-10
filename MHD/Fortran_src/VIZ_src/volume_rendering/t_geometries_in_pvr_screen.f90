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
!!      subroutine copy_projected_position(pvr_scr_org, pvr_screen)
!!
!!      subroutine allocate_nod_data_4_pvr                              &
!!     &         (numnod, numele, num_sf_grp, field_pvr)
!!      subroutine dealloc_nod_data_4_pvr(fld)
!!
!!      subroutine g(fld)
!!      subroutine alloc_pvr_isosurfaces(fld)
!!
!!      subroutine allocate_pixel_position_pvr(n_pvr_pixel, pixel_xy)
!!      subroutine dealloc_data_4_pvr(fld)
!!
!!      subroutine deallocate_projected_data_pvr                        &
!!      &        (num_pvr, proj, field_pvr)
!!      subroutine deallocate_pixel_position_pvr(pixel_xy)
!!@endverbatim
!
      module t_geometries_in_pvr_screen
!
      use m_precision
!
      use m_constants
!
      implicit  none
!
!
!
!>  Structure for start points of ray tracing
      type pvr_projected_data
!>    Defined flag for viewpoint
        integer(kind = kint) :: iflag_viewpoint = 0
!>    Position of viewpoint
        real(kind = kreal) :: viewpoint_vec(3) = (/zero,zero,zero/)
!
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
      end type pvr_projected_data
!
!
!>  Structure for field data on projected coordinate
      type pvr_projected_field
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
      end type pvr_projected_field
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
      type(pvr_projected_data), intent(inout) :: pvr_screen
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
      type(pvr_projected_data), intent(inout) :: pvr_screen
!
!
      deallocate(pvr_screen%arccos_sf)
      deallocate(pvr_screen%x_nod_model)
!
      end subroutine dealloc_projected_position
!
! -----------------------------------------------------------------------
!
      subroutine copy_projected_position(pvr_scr_org, pvr_screen)
!
      type(pvr_projected_data), intent(in) :: pvr_scr_org
      type(pvr_projected_data), intent(inout) :: pvr_screen
!
!
      pvr_screen%viewpoint_vec(:) = pvr_scr_org%viewpoint_vec(:)
!      pvr_screen%x_nod_model(:,:) =  pvr_scr_org%x_nod_model(:,:)
!      pvr_screen%arccos_sf(:) =      pvr_scr_org%arccos_sf(:)
!
      end subroutine copy_projected_position
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_data_4_pvr                                &
     &         (numnod, numele, num_sf_grp, field_pvr)
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: num_sf_grp
      type(pvr_projected_field), intent(inout) :: field_pvr
!
!
        call alloc_nod_data_4_pvr                                       &
     &     (numnod, numele, field_pvr)
        call alloc_iflag_pvr_used_ele(numele, field_pvr)
        call alloc_iflag_pvr_boundaries                                 &
     &     (num_sf_grp, field_pvr)
!
      end subroutine allocate_nod_data_4_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_nod_data_4_pvr(field_pvr)
!
      type(pvr_projected_field), intent(inout) :: field_pvr
!
!
      deallocate(field_pvr%iflag_enhanse, field_pvr%enhansed_opacity)
      deallocate(field_pvr%iflag_used_ele)
      deallocate(field_pvr%d_pvr, field_pvr%grad_ele)
!
      end subroutine dealloc_nod_data_4_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_nod_data_4_pvr(nnod, nele, fld)
!
      integer(kind = kint), intent(in) :: nnod, nele
      type(pvr_projected_field), intent(inout) :: fld
!
!
      allocate(fld%d_pvr(nnod))
      allocate(fld%grad_ele(nele,3))
!
      if(nnod .gt. 0) fld%d_pvr =    0.0d0
      if(nele .gt. 0) fld%grad_ele = 0.0d0
!
      end subroutine alloc_nod_data_4_pvr
!
! -----------------------------------------------------------------------
!
      subroutine alloc_iflag_pvr_used_ele(nele, fld)
!
      integer(kind = kint), intent(in) :: nele
      type(pvr_projected_field), intent(inout) :: fld
!
!
      allocate(fld%iflag_used_ele(nele))
      if(nele .gt. 0) fld%iflag_used_ele = 0
!
      end subroutine alloc_iflag_pvr_used_ele
!
! -----------------------------------------------------------------------
!
      subroutine alloc_iflag_pvr_boundaries(num_sf_grp, fld)
!
      integer(kind = kint), intent(in) :: num_sf_grp
      type(pvr_projected_field), intent(inout) :: fld
!
!
      allocate(fld%iflag_enhanse(num_sf_grp))
      allocate(fld%enhansed_opacity(num_sf_grp))
!
      if(num_sf_grp .gt. 0) fld%iflag_enhanse = 0
      if(num_sf_grp .gt. 0) fld%enhansed_opacity = 0
!
      end subroutine alloc_iflag_pvr_boundaries
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_pvr_sections(fld)
!
      type(pvr_projected_field), intent(inout) :: fld
!
!
      allocate(fld%coefs(10,fld%num_sections))
      allocate(fld%sect_opacity(fld%num_sections))
!
      if(fld%num_sections .gt. 0) fld%coefs =        0.0d0
      if(fld%num_sections .gt. 0) fld%sect_opacity = 0.0d0
!
      end subroutine alloc_pvr_sections
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pvr_isosurfaces(fld)
!
      type(pvr_projected_field), intent(inout) :: fld
!
!
      allocate(fld%itype_isosurf(fld%num_isosurf))
      allocate(fld%iso_value(fld%num_isosurf))
      allocate(fld%iso_opacity(fld%num_isosurf))
!
      if(fld%num_isosurf .gt. 0) fld%itype_isosurf = 0
      if(fld%num_isosurf .gt. 0) fld%iso_value = 0.0d0
      if(fld%num_isosurf .gt. 0) fld%iso_opacity = 0.0d0
!
      end subroutine alloc_pvr_isosurfaces
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_sections(fld)
!
      type(pvr_projected_field), intent(inout) :: fld
!
!
      deallocate(fld%coefs, fld%sect_opacity)
!
      end subroutine dealloc_pvr_sections
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_isosurfaces(fld)
!
      type(pvr_projected_field), intent(inout) :: fld
!
!
      deallocate(fld%itype_isosurf)
      deallocate(fld%iso_value, fld%iso_opacity)
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
