!>@file  rendering_streo_LIC_image.f90
!!       module rendering_streo_LIC_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine streo_lic_rendering_fix_view(istep_pvr, irank_tgt,   &
!!     &          node, ele, surf, group, lic_p, pvr_param, file_param, &
!!     &          projection_mat, start_pt, image, pvr_data, pvr_rgb)
!!
!!      subroutine lic_rendering_with_rotation                          &
!!     &         (isel_projection, istep_pvr, irank_tgt,                &
!!     &          node, ele, surf, group, lic_p, pvr_param, file_param, &
!!     &          projection_mat, pvr_data, pvr_rgb)
!!      subroutine anaglyph_lic_rendering_w_rot(istep_pvr, irank_tgt,   &
!!     &          node, ele, surf, group, lic_p, pvr_param, file_param, &
!!     &          projection_left, projection_right, pvr_data, pvr_rgb)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(mesh_groups), intent(in) :: group
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(pvr_output_parameter), intent(in) :: file_param
!!        type(PVR_image_generator), intent(inout) :: pvr_data
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!@endverbatim
!
      module rendering_streo_LIC_image
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
      use t_control_param_LIC
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
      subroutine streo_lic_rendering_fix_view(istep_pvr, irank_tgt,     &
     &          node, ele, surf, group, lic_p, pvr_param, file_param,   &
     &          projection_mat, start_pt, image, pvr_data, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use composite_pvr_images
      use rendering_LIC_image
      use write_LIC_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      integer(kind = kint), intent(in) :: irank_tgt
      real(kind = kreal), intent(in) :: projection_mat(4,4)
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(lic_parameters), intent(in) :: lic_p
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_output_parameter), intent(in) :: file_param
!
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_ray_start_type), intent(inout) :: start_pt
      type(pvr_segmented_img), intent(inout)  :: image
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      call cal_pvr_modelview_matrix                                     &
     &   (izero, pvr_param%outline, pvr_data%view, pvr_data%color,      &
     &    pvr_data%screen)
!
      call transfer_to_screen                                           &
     &   (node, ele, surf, group%surf_grp, group%surf_grp_geom,         &
     &    pvr_param%field, pvr_data%view, projection_mat,               &
     &    pvr_param%pixel, pvr_data%bound, pvr_data%screen, start_pt)
      call set_subimages(pvr_rgb%num_pixel_xy, start_pt, image)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image_4_lic'
      call rendering_image_4_lic(istep_pvr, irank_tgt, file_param,      &
     &    node, ele, surf, lic_p, pvr_data%color, pvr_param%colorbar,   &
     &    pvr_param%field, pvr_data%screen, start_pt, image, pvr_rgb)
!
      call dealloc_pvr_local_subimage(image)
      call deallocate_pvr_ray_start(start_pt)
!
      end subroutine streo_lic_rendering_fix_view
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine lic_rendering_with_rotation                            &
     &         (isel_projection, istep_pvr, irank_tgt,                  &
     &          node, ele, surf, group, lic_p, pvr_param, file_param,   &
     &          projection_mat, pvr_data, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use composite_pvr_images
      use rendering_LIC_image
      use write_LIC_image
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
      type(lic_parameters), intent(in) :: lic_p
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_output_parameter), intent(in) :: file_param
!
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      type(pvr_ray_start_type) :: start_pt
      type(pvr_segmented_img)  :: image
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
        call rendering_lic_at_once(istep_pvr, irank_tgt,                &
     &      node, ele, surf, group, lic_p, pvr_param, file_param,       &
     &      projection_mat, start_pt, image, pvr_data, pvr_rgb)
!
        call end_elapsed_time(76)
        call start_elapsed_time(77)
        if(iflag_debug .gt. 0) write(*,*) 'sel_write_pvr_image_file'
        call sel_write_pvr_image_file                                   &
     &     (file_param, i_rot, istep_pvr, irank_tgt,                    &
     &      isel_projection, pvr_rgb)
        call calypso_mpi_barrier
        call end_elapsed_time(77)
        call start_elapsed_time(76)
!
        call dealloc_pvr_local_subimage(image)
        call deallocate_pvr_ray_start(start_pt)
      end do
!
      end subroutine lic_rendering_with_rotation
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_lic_rendering_w_rot(istep_pvr, irank_tgt,     &
     &          node, ele, surf, group, lic_p, pvr_param, file_param,   &
     &          projection_left, projection_right, pvr_data, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use rendering_LIC_image
      use write_LIC_image
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
      type(lic_parameters), intent(in) :: lic_p
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_output_parameter), intent(in) :: file_param
!
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      type(pvr_ray_start_type) :: start_pt
      type(pvr_segmented_img)  :: image
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
        call rendering_lic_at_once(istep_pvr, irank_tgt,                &
     &      node, ele, surf, group, lic_p, pvr_param, file_param,       &
     &      projection_left, start_pt, image, pvr_data, pvr_rgb)
        call store_left_eye_image(irank_tgt, pvr_rgb)
!
        call dealloc_pvr_local_subimage(image)
        call deallocate_pvr_ray_start(start_pt)
!
!    Right eye
        call rendering_lic_at_once(istep_pvr, irank_tgt,                &
     &      node, ele, surf, group, lic_p, pvr_param, file_param,       &
     &      projection_right, start_pt, image, pvr_data, pvr_rgb)
        call add_left_eye_image(irank_tgt, pvr_rgb)
!
        call end_elapsed_time(76)
        call start_elapsed_time(77)
        call sel_write_pvr_image_file(file_param,                       &
     &      i_rot, istep_pvr, irank_tgt, IFLAG_NORMAL, pvr_rgb)
!
        call calypso_mpi_barrier
        call end_elapsed_time(77)
        call start_elapsed_time(76)
!
        call dealloc_pvr_local_subimage(image)
        call deallocate_pvr_ray_start(start_pt)
      end do
!
      end subroutine anaglyph_lic_rendering_w_rot
!
!  ---------------------------------------------------------------------
!
      end module rendering_streo_LIC_image
