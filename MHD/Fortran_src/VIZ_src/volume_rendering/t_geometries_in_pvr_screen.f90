!>@file  t_geometries_in_pvr_screen.f90
!!       module t_geometries_in_pvr_screen
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine allocate_nod_data_4_pvr                              &
!!     &         (numnod, numele, numsurf, num_sf_grp, field_pvr)
!!      subroutine allocate_pixel_position_pvr(pixel_xy)
!!      subroutine alloc_pvr_isosurfaces(fld)
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
!>  Structure for field data on projected coordinate
      type pvr_projected_field
!>    Data for rendering
        real(kind = kreal), pointer :: d_pvr(:)
!>    Gradient for rendering
        real(kind = kreal), pointer :: grad_ele(:,:)
!
!>    flag for rendering element
        integer(kind = kint), pointer :: iflag_used_ele(:)
!
!>    integer flag for surface boundaries
        integer(kind = kint), pointer :: iflag_enhanse(:)
!>    Opacity value for surface boundaries
        real(kind = kreal), pointer :: enhansed_opacity(:)
!>    Opacity value for surface boundaries
        real(kind = kreal), pointer :: arccos_sf(:)
!
!>    Number of sections
        integer(kind = kint) :: num_sections
!>    fiale value for isosurfaces
        real(kind = kreal), pointer :: coefs(:,:)
!>    Opacity value for isosurfaces
        real(kind = kreal), pointer :: sect_opacity(:)
!
!>    Number of isosurfaces
        integer(kind = kint) :: num_isosurf
!>    Number of isosurfaces
        integer(kind = kint), pointer :: itype_isosurf(:)
!>    fiale value for isosurfaces
        real(kind = kreal), pointer :: iso_value(:)
!>    Opacity value for isosurfaces
        real(kind = kreal), pointer :: iso_opacity(:)
      end type pvr_projected_field
!
!>  Structure for pixel position
      type pvr_pixel_position_type
!>    Number of horizontal pixels
        integer(kind = kint) :: num_pixel_x
!>    Number of vertical pixels
        integer(kind = kint) :: num_pixel_y
!>    Position of horizontal pixels
        real(kind = kreal), pointer :: pixel_point_x(:)
!>    Position of vertical pixels
        real(kind = kreal), pointer :: pixel_point_y(:)
      end type pvr_pixel_position_type
!
      private :: alloc_nod_data_4_pvr, alloc_iflag_pvr_used_ele
      private :: alloc_iflag_pvr_boundaries
      private :: dealloc_iflag_pvr_boundaries
      private :: dealloc_pvr_sections, dealloc_pvr_isosurfaces
!
! -----------------------------------------------------------------------
!
      contains
!
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
      subroutine alloc_iflag_pvr_boundaries                             &
     &         (nsurf, num_sf_grp, fld)
!
      integer(kind = kint), intent(in) :: nsurf, num_sf_grp
      type(pvr_projected_field), intent(inout) :: fld
!
!
      allocate(fld%arccos_sf(nsurf))
      allocate(fld%iflag_enhanse(num_sf_grp))
      allocate(fld%enhansed_opacity(num_sf_grp))
!
      if(nsurf .gt. 0)      fld%arccos_sf = 0.0d0
      if(num_sf_grp .gt. 0) fld%iflag_enhanse = 0
      if(num_sf_grp .gt. 0) fld%enhansed_opacity = 0
!
      end subroutine alloc_iflag_pvr_boundaries
!
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
      subroutine dealloc_data_4_pvr(fld)
!
      type(pvr_projected_field), intent(inout) :: fld
!
!
      deallocate(fld%iflag_used_ele)
      deallocate(fld%d_pvr, fld%grad_ele)
!
      end subroutine dealloc_data_4_pvr
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_iflag_pvr_boundaries(fld)
!
      type(pvr_projected_field), intent(inout) :: fld
!
!
      deallocate(fld%iflag_enhanse, fld%enhansed_opacity)
      deallocate(fld%arccos_sf)
!
      end subroutine dealloc_iflag_pvr_boundaries
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_sections(fld)
!
      type(pvr_projected_field), intent(inout) :: fld
!
!
      deallocate(fld%itype_isosurf, fld%iso_value)
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
      subroutine allocate_nod_data_4_pvr                                &
     &         (numnod, numele, numsurf, num_sf_grp, field_pvr)
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: num_sf_grp
      type(pvr_projected_field), intent(inout) :: field_pvr
!
!
        call alloc_nod_data_4_pvr                                       &
     &     (numnod, numele, field_pvr)
        call alloc_iflag_pvr_used_ele(numele, field_pvr)
        call alloc_iflag_pvr_boundaries                                 &
     &     (numsurf, num_sf_grp, field_pvr)
!
      end subroutine allocate_nod_data_4_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_pixel_position_pvr(pixel_xy)
!
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!
!
      allocate(pixel_xy%pixel_point_x(pixel_xy%num_pixel_x))
      allocate(pixel_xy%pixel_point_y(pixel_xy%num_pixel_y))
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
