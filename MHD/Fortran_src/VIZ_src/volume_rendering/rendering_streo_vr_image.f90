!>@file  rendering_streo_vr_image.f90
!!       module rendering_streo_vr_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine streo_rendering_fixed_view(istep_pvr, irank_tgt,     &
!!     &          node, ele, surf, group, pvr_param, file_param,        &
!!     &          projection_left, projection_right, pvr_data, pvr_rgb)
!!      subroutine anaglyph_rendering_fixed_view(istep_pvr, irank_tgt,  &
!!     &          node, ele, surf, group, pvr_param, file_param,        &
!!     &          projection_left, projection_right, pvr_data, pvr_rgb)
!!
!!      subroutine rendering_with_rotation                              &
!!     &         (isel_projection, istep_pvr, irank_tgt,                &
!!     &          node, ele, surf, group, pvr_param, file_param,        &
!!     &          projection_mat, pvr_data, pvr_rgb)
!!      subroutine anaglyph_rendering_w_rotation(istep_pvr, irank_tgt,  &
!!     &          node, ele, surf, group, pvr_param, file_param,        &
!!     &          projection_left, projection_right, pvr_data, pvr_rgb)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(mesh_groups), intent(in) :: group
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(pvr_output_parameter), intent(in) :: file_param
!!        type(PVR_image_generator), intent(inout) :: pvr_data
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!@endverbatim
!
      module rendering_streo_vr_image
!
      use m_precision
      use m_machine_parameter
      use m_constants
      use m_work_time
!
      use calypso_mpi
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use generate_vr_image
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine streo_rendering_fixed_view(istep_pvr, irank_tgt,       &
     &          node, ele, surf, group, pvr_param, file_param,          &
     &          projection_left, projection_right, pvr_data, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use composite_pvr_images
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      integer(kind = kint), intent(in) :: irank_tgt
      real(kind = kreal), intent(in) :: projection_left(4,4)
      real(kind = kreal), intent(in) :: projection_right(4,4)
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_output_parameter), intent(in) :: file_param
!
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      call cal_pvr_modelview_matrix                                     &
     &   (izero, pvr_param%outline, pvr_data%view, pvr_data%color,      &
     &    pvr_data%screen)
!
!   Left eye
!
      call transfer_to_screen                                           &
     &   (node, ele, surf, group%surf_grp, group%surf_grp_geom,         &
     &    pvr_param%field, pvr_data%view, projection_left,              &
     &    pvr_param%pixel,  pvr_data%bound, pvr_data%screen,            &
     &    pvr_data%start_pt)
      call set_subimages(pvr_rgb%num_pixel_xy,                          &
     &    pvr_data%start_pt, pvr_data%image)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image'
      call rendering_image(istep_pvr, irank_tgt, file_param,            &
     &    node, ele, surf, pvr_data%color, pvr_param%colorbar,          &
     &    pvr_param%field, pvr_data%screen, pvr_data%start_pt,          &
     &    pvr_data%image, pvr_rgb)
!
      call dealloc_pvr_local_subimage(pvr_data%image)
      call deallocate_pvr_ray_start(pvr_data%start_pt)
!
      call end_elapsed_time(71)
      call start_elapsed_time(72)
      call sel_write_pvr_image_file(file_param, iminus, istep_pvr,      &
     &    irank_tgt, IFLAG_LEFT, pvr_rgb)
      call calypso_mpi_barrier
      call end_elapsed_time(72)
      call start_elapsed_time(71)
!
!   Right eye
!
      call transfer_to_screen                                           &
     &   (node, ele, surf, group%surf_grp, group%surf_grp_geom,         &
     &    pvr_param%field, pvr_data%view, projection_right,             &
     &    pvr_param%pixel,  pvr_data%bound, pvr_data%screen,            &
     &    pvr_data%start_pt)
      call set_subimages(pvr_rgb%num_pixel_xy,                          &
     &    pvr_data%start_pt, pvr_data%image)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image'
      call rendering_image(istep_pvr, irank_tgt, file_param,            &
     &    node, ele, surf, pvr_data%color, pvr_param%colorbar,          &
     &    pvr_param%field, pvr_data%screen, pvr_data%start_pt,          &
     &    pvr_data%image, pvr_rgb)
!
      call dealloc_pvr_local_subimage(pvr_data%image)
      call deallocate_pvr_ray_start(pvr_data%start_pt)
!
      call end_elapsed_time(71)
      call start_elapsed_time(72)
      call sel_write_pvr_image_file(file_param, iminus, istep_pvr,      &
     &    irank_tgt, IFLAG_RIGHT, pvr_rgb)
      call calypso_mpi_barrier
      call end_elapsed_time(72)
      call start_elapsed_time(71)
!
      end subroutine streo_rendering_fixed_view
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_rendering_fixed_view(istep_pvr, irank_tgt,    &
     &          node, ele, surf, group, pvr_param, file_param,          &
     &          projection_left, projection_right, pvr_data, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use composite_pvr_images
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      integer(kind = kint), intent(in) :: irank_tgt
      real(kind = kreal), intent(in) :: projection_left(4,4)
      real(kind = kreal), intent(in) :: projection_right(4,4)
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_output_parameter), intent(in) :: file_param
!
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      call cal_pvr_modelview_matrix                                     &
     &   (izero, pvr_param%outline, pvr_data%view, pvr_data%color,      &
     &    pvr_data%screen)
!
!   Left eye
!
      call transfer_to_screen                                           &
     &   (node, ele, surf, group%surf_grp, group%surf_grp_geom,         &
     &    pvr_param%field, pvr_data%view, projection_left,              &
     &    pvr_param%pixel,  pvr_data%bound, pvr_data%screen,            &
     &    pvr_data%start_pt)
      call set_subimages(pvr_rgb%num_pixel_xy,                          &
     &    pvr_data%start_pt, pvr_data%image)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image'
      call rendering_image(istep_pvr, irank_tgt, file_param,            &
     &    node, ele, surf, pvr_data%color, pvr_param%colorbar,          &
     &    pvr_param%field, pvr_data%screen, pvr_data%start_pt,          &
     &    pvr_data%image, pvr_rgb)
      call dealloc_pvr_local_subimage(pvr_data%image)
      call deallocate_pvr_ray_start(pvr_data%start_pt)
!
      call store_left_eye_image(irank_tgt, pvr_rgb)
!
!   Right eye
!
      call transfer_to_screen                                           &
     &   (node, ele, surf, group%surf_grp, group%surf_grp_geom,         &
     &    pvr_param%field, pvr_data%view, projection_right,             &
     &    pvr_param%pixel,  pvr_data%bound, pvr_data%screen,            &
     &    pvr_data%start_pt)
      call set_subimages(pvr_rgb%num_pixel_xy,                          &
     &    pvr_data%start_pt, pvr_data%image)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image'
      call rendering_image(istep_pvr, irank_tgt, file_param,            &
     &    node, ele, surf, pvr_data%color, pvr_param%colorbar,          &
     &    pvr_param%field, pvr_data%screen, pvr_data%start_pt,          &
     &    pvr_data%image, pvr_rgb)
      call dealloc_pvr_local_subimage(pvr_data%image)
      call deallocate_pvr_ray_start(pvr_data%start_pt)
!
      call add_left_eye_image(irank_tgt, pvr_rgb)
!
      call end_elapsed_time(71)
      call start_elapsed_time(72)
      call sel_write_pvr_image_file(file_param,                         &
     &    iminus, istep_pvr, irank_tgt, IFLAG_NORMAL, pvr_rgb)
      call calypso_mpi_barrier
      call end_elapsed_time(72)
      call start_elapsed_time(71)
!
      end subroutine anaglyph_rendering_fixed_view
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine rendering_with_rotation                                &
     &         (isel_projection, istep_pvr, irank_tgt,                  &
     &          node, ele, surf, group, pvr_param, file_param,          &
     &          projection_mat, pvr_data, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use composite_pvr_images
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: isel_projection
      integer(kind = kint), intent(in) :: istep_pvr
      integer(kind = kint), intent(in) :: irank_tgt
      real(kind = kreal), intent(in) :: projection_mat(4,4)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_output_parameter), intent(in) :: file_param
!
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      integer(kind = kint) :: i_rot, ist_rot, ied_rot
!
      ist_rot = pvr_data%view%istart_rot
      ied_rot = pvr_data%view%iend_rot
      do i_rot = ist_rot, ied_rot
        call cal_pvr_modelview_matrix                                   &
     &     (i_rot, pvr_param%outline, pvr_data%view, pvr_data%color,    &
     &      pvr_data%screen)
!
        call rendering_at_once(istep_pvr, irank_tgt,                    &
     &      node, ele, surf, group, pvr_param, file_param,              &
     &      projection_mat, pvr_data, pvr_rgb)
!
        call end_elapsed_time(71)
        call start_elapsed_time(72)
        if(iflag_debug .gt. 0) write(*,*) 'sel_write_pvr_image_file'
        call sel_write_pvr_image_file                                   &
     &   (file_param, i_rot, istep_pvr, irank_tgt,                      &
     &    isel_projection, pvr_rgb)
        call calypso_mpi_barrier
        call end_elapsed_time(72)
        call start_elapsed_time(71)
!
        call dealloc_pvr_local_subimage(pvr_data%image)
        call deallocate_pvr_ray_start(pvr_data%start_pt)
      end do
!
      end subroutine rendering_with_rotation
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_rendering_w_rotation(istep_pvr, irank_tgt,    &
     &          node, ele, surf, group, pvr_param, file_param,          &
     &          projection_left, projection_right, pvr_data, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      integer(kind = kint), intent(in) :: irank_tgt
      real(kind = kreal), intent(in) :: projection_left(4,4)
      real(kind = kreal), intent(in) :: projection_right(4,4)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_output_parameter), intent(in) :: file_param
!
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      integer(kind = kint) :: i_rot, ist_rot, ied_rot
!
!
      ist_rot = pvr_data%view%istart_rot
      ied_rot = pvr_data%view%iend_rot
      do i_rot = ist_rot, ied_rot
        call cal_pvr_modelview_matrix                                   &
     &     (i_rot, pvr_param%outline, pvr_data%view, pvr_data%color,    &
     &      pvr_data%screen)
!
!    Left eye
        call rendering_at_once(istep_pvr, irank_tgt,                    &
     &      node, ele, surf, group, pvr_param, file_param,              &
     &      projection_left, pvr_data, pvr_rgb)
        call store_left_eye_image(irank_tgt, pvr_rgb)
!
        call dealloc_pvr_local_subimage(pvr_data%image)
        call deallocate_pvr_ray_start(pvr_data%start_pt)
!
!    Right eye
        call rendering_at_once(istep_pvr, irank_tgt,                    &
     &      node, ele, surf, group, pvr_param, file_param,              &
     &      projection_right, pvr_data, pvr_rgb)
        call add_left_eye_image(irank_tgt, pvr_rgb)
!
        call end_elapsed_time(71)
        call start_elapsed_time(72)
        call sel_write_pvr_image_file(file_param,                       &
     &      i_rot, istep_pvr, irank_tgt, IFLAG_NORMAL, pvr_rgb)
        call calypso_mpi_barrier
        call end_elapsed_time(72)
        call start_elapsed_time(71)
!
        call dealloc_pvr_local_subimage(pvr_data%image)
        call deallocate_pvr_ray_start(pvr_data%start_pt)
      end do
!
      end subroutine anaglyph_rendering_w_rotation
!
!  ---------------------------------------------------------------------
!
      end module rendering_streo_vr_image
