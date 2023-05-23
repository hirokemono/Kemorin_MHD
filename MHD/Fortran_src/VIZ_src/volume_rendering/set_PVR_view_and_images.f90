!>@file   set_PVR_view_and_images.f90
!!@brief  module set_PVR_view_and_images
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module for each volume rendering
!!
!!@verbatim
!!      subroutine s_set_PVR_view_and_images                            &
!!     &         (num_pvr, num_pvr_images, istack_pvr_images,           &
!!     &          mesh, PVR_sort, pvr_rgb, pvr_param,                   &
!!     &          pvr_bound, pvr_proj, m_SR)
!!        integer(kind = kint), intent(in) :: num_pvr, num_pvr_images
!!        integer(kind = kint), intent(in)                              &
!!     &                       :: istack_pvr_images(0:num_pvr)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(sort_PVRs_by_type), intent(in) :: PVR_sort
!!        type(pvr_image_type), intent(in) :: pvr_rgb(num_pvr_images)
!!        type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound(num_pvr)
!!        type(PVR_projection_data), intent(inout)                      &
!!     &                            :: pvr_proj(num_pvr_images)
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module set_PVR_view_and_images
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
!
      use t_mesh_data
      use t_sort_PVRs_by_type
      use t_pvr_image_array
      use t_rendering_vr_image
      use t_surf_grp_4_pvr_domain
      use t_mesh_SR
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_PVR_view_and_images                              &
     &         (num_pvr, num_pvr_images, istack_pvr_images,             &
     &          mesh, PVR_sort, pvr_rgb, pvr_param,                     &
     &          pvr_bound, pvr_proj, m_SR)
!
      use set_PVR_view_and_image
!
      integer(kind = kint), intent(in) :: num_pvr, num_pvr_images
      integer(kind = kint), intent(in)                                  &
     &                       :: istack_pvr_images(0:num_pvr)
      type(mesh_geometry), intent(in) :: mesh
      type(sort_PVRs_by_type), intent(in) :: PVR_sort
      type(pvr_image_type), intent(in) :: pvr_rgb(num_pvr_images)
      type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound(num_pvr)
      type(PVR_projection_data), intent(inout)                          &
     &                            :: pvr_proj(num_pvr_images)
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_pvr, ist_pvr, ied_pvr
      integer(kind = kint) :: ist_img, num_img
!
!
!       single image
      ist_pvr = PVR_sort%istack_PVR_modes(0) + 1
      ied_pvr = PVR_sort%istack_PVR_modes(1)
      do i_pvr = ist_pvr, ied_pvr
        ist_img = istack_pvr_images(i_pvr-1)
        call single_PVR_view_matrices                                   &
     &     (mesh, pvr_rgb(ist_img+1), pvr_param(i_pvr),                 &
     &      pvr_bound(i_pvr), pvr_proj(ist_img+1), m_SR)
      end do
!
!       stereo rendering
      ist_pvr = PVR_sort%istack_PVR_modes(1) + 1
      ied_pvr = PVR_sort%istack_PVR_modes(2)
      do i_pvr = ist_pvr, ied_pvr
        ist_img = istack_pvr_images(i_pvr-1)
        num_img = istack_pvr_images(i_pvr  ) - ist_img
        call quilt_PVR_view_matrices                                    &
     &     (num_img, mesh, pvr_rgb(ist_img+1), pvr_param(i_pvr),        &
     &      pvr_bound(i_pvr), pvr_proj(ist_img+1), m_SR)
      end do
!
!      Anaglyph with fixed view
      ist_pvr = PVR_sort%istack_PVR_modes(4) + 1
      ied_pvr = PVR_sort%istack_PVR_modes(5)
      do i_pvr = ist_pvr, ied_pvr
        ist_img = istack_pvr_images(i_pvr-1)
        num_img = istack_pvr_images(i_pvr  ) - ist_img
        call anaglyph_PVR_view_matrices                                 &
     &     (mesh, pvr_rgb(ist_img+1), pvr_param(i_pvr),                 &
     &      pvr_bound(i_pvr), pvr_proj(ist_img+1), m_SR)
      end do
!
      end subroutine s_set_PVR_view_and_images
!
!  ---------------------------------------------------------------------
!
      end module set_PVR_view_and_images
