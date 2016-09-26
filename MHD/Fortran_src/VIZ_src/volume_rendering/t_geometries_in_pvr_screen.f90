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
!!     &       (num_pvr, numnod, numele, numsurf, num_sf_grp, field_pvr)
!!      subroutine allocate_pixel_position_pvr(pixel_xy)
!!      subroutine alloc_pvr_isosurfaces(fld_pvr)
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
!>    Position in modelview coordinate and screen coordinate
!!@n    (Overwritten)
        real(kind = kreal), pointer :: x_nod_model(:,:)
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
      private :: dealloc_data_4_pvr,  dealloc_iflag_pvr_boundaries
      private :: dealloc_pvr_sections, dealloc_pvr_isosurfaces
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_nod_data_4_pvr(nnod, nele, fld_pvr)
!
      integer(kind = kint), intent(in) :: nnod, nele
      type(pvr_projected_field), intent(inout) :: fld_pvr
!
!
      allocate(fld_pvr%x_nod_model(nnod,4))
!
      allocate(fld_pvr%d_pvr(nnod))
      allocate(fld_pvr%grad_ele(nele,3))
!
      if(nnod .gt. 0) then
        fld_pvr%x_nod_model =  0.0d0
        fld_pvr%d_pvr =        0.0d0
      end if
      if(nele .gt. 0) fld_pvr%grad_ele = 0.0d0
!
      end subroutine alloc_nod_data_4_pvr
!
! -----------------------------------------------------------------------
!
      subroutine alloc_iflag_pvr_used_ele(nele, fld_pvr)
!
      integer(kind = kint), intent(in) :: nele
      type(pvr_projected_field), intent(inout) :: fld_pvr
!
!
      allocate(fld_pvr%iflag_used_ele(nele))
      if(nele .gt. 0) fld_pvr%iflag_used_ele = 0
!
      end subroutine alloc_iflag_pvr_used_ele
!
! -----------------------------------------------------------------------
!
      subroutine alloc_iflag_pvr_boundaries                             &
     &         (nsurf, num_sf_grp, fld_pvr)
!
      integer(kind = kint), intent(in) :: nsurf, num_sf_grp
      type(pvr_projected_field), intent(inout) :: fld_pvr
!
!
      allocate(fld_pvr%arccos_sf(nsurf))
      allocate(fld_pvr%iflag_enhanse(num_sf_grp))
      allocate(fld_pvr%enhansed_opacity(num_sf_grp))
!
      if(nsurf .gt. 0)      fld_pvr%arccos_sf = 0.0d0
      if(num_sf_grp .gt. 0) fld_pvr%iflag_enhanse = 0
      if(num_sf_grp .gt. 0) fld_pvr%enhansed_opacity = 0
!
      end subroutine alloc_iflag_pvr_boundaries
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pvr_sections(fld_pvr)
!
      type(pvr_projected_field), intent(inout) :: fld_pvr
!
!
      allocate(fld_pvr%coefs(10,fld_pvr%num_sections))
      allocate(fld_pvr%sect_opacity(fld_pvr%num_sections))
!
      if(fld_pvr%num_sections .gt. 0) fld_pvr%coefs =        0.0d0
      if(fld_pvr%num_sections .gt. 0) fld_pvr%sect_opacity = 0.0d0
!
      end subroutine alloc_pvr_sections
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pvr_isosurfaces(fld_pvr)
!
      type(pvr_projected_field), intent(inout) :: fld_pvr
!
!
      allocate(fld_pvr%itype_isosurf(fld_pvr%num_isosurf))
      allocate(fld_pvr%iso_value(fld_pvr%num_isosurf))
      allocate(fld_pvr%iso_opacity(fld_pvr%num_isosurf))
!
      if(fld_pvr%num_isosurf .gt. 0) fld_pvr%itype_isosurf = 0
      if(fld_pvr%num_isosurf .gt. 0) fld_pvr%iso_value = 0.0d0
      if(fld_pvr%num_isosurf .gt. 0) fld_pvr%iso_opacity = 0.0d0
!
      end subroutine alloc_pvr_isosurfaces
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_data_4_pvr(fld_pvr)
!
      type(pvr_projected_field), intent(inout) :: fld_pvr
!
!
      deallocate(fld_pvr%iflag_used_ele)
      deallocate(fld_pvr%d_pvr, fld_pvr%grad_ele)
      deallocate(fld_pvr%x_nod_model)
!
      end subroutine dealloc_data_4_pvr
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_iflag_pvr_boundaries(fld_pvr)
!
      type(pvr_projected_field), intent(inout) :: fld_pvr
!
!
      deallocate(fld_pvr%iflag_enhanse, fld_pvr%enhansed_opacity)
      deallocate(fld_pvr%arccos_sf)
!
      end subroutine dealloc_iflag_pvr_boundaries
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_sections(fld_pvr)
!
      type(pvr_projected_field), intent(inout) :: fld_pvr
!
!
      deallocate(fld_pvr%itype_isosurf, fld_pvr%iso_value)
!
      end subroutine dealloc_pvr_sections
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_isosurfaces(fld_pvr)
!
      type(pvr_projected_field), intent(inout) :: fld_pvr
!
!
      deallocate(fld_pvr%itype_isosurf)
      deallocate(fld_pvr%iso_value, fld_pvr%iso_opacity)
!
      end subroutine dealloc_pvr_isosurfaces
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_data_4_pvr                                &
     &       (num_pvr, numnod, numele, numsurf, num_sf_grp, field_pvr)
!
      integer(kind = kint), intent(in) :: num_pvr
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: num_sf_grp
      type(pvr_projected_field), intent(inout) :: field_pvr(num_pvr)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_pvr
        call alloc_nod_data_4_pvr                                       &
     &     (numnod, numele, field_pvr(i))
        call alloc_iflag_pvr_used_ele(numele, field_pvr(i))
        call alloc_iflag_pvr_boundaries                                 &
     &     (numsurf, num_sf_grp, field_pvr(i))
      end do
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
      subroutine deallocate_projected_data_pvr(num_pvr, field_pvr)
!
      integer(kind = kint), intent(in) :: num_pvr
      type(pvr_projected_field), intent(inout) :: field_pvr(num_pvr)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_pvr
        call dealloc_data_4_pvr(field_pvr(i))
        call dealloc_iflag_pvr_boundaries(field_pvr(i))
        call dealloc_pvr_sections(field_pvr(i))
        call dealloc_pvr_isosurfaces(field_pvr(i))
      end do
!
      end subroutine deallocate_projected_data_pvr
!
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
