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
!!     &          node, ele, surf, group, lic_p, pvr_param, pvr_data)
!!      subroutine streo_lic_rendering_with_rot(istep_pvr, irank_tgt,   &
!!     &          node, ele, surf, group, lic_p, pvr_param, pvr_data)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(mesh_groups), intent(in) :: group
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(PVR_image_generator), intent(inout) :: pvr_data
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
     &          node, ele, surf, group, lic_p, pvr_param, pvr_data)
!
      use cal_pvr_modelview_mat
      use composite_pvr_images
      use write_PVR_image
      use rendering_LIC_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      integer(kind = kint), intent(in) :: irank_tgt
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(lic_parameters), intent(in) :: lic_p
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(PVR_image_generator), intent(inout) :: pvr_data
!
!
      call cal_pvr_modelview_matrix                                     &
     &   (izero, pvr_param%outline, pvr_data%view, pvr_data%color,      &
     &    pvr_data%screen)
!
!   Left eye
!
      call rendering_lic_at_once(IFLAG_LEFT, istep_pvr, irank_tgt,      &
     &    node, ele, surf, group, lic_p, pvr_param, pvr_data)
!
      if(pvr_data%view%iflag_anaglyph .gt. 0) then
        call store_left_eye_image(irank_tgt, pvr_data%rgb)
      else
        call end_elapsed_time(76)
        call start_elapsed_time(77)
!
        call sel_write_pvr_image_file(pvr_param%file,                   &
     &     iminus, istep_pvr, irank_tgt, IFLAG_LEFT, pvr_data%rgb)
!
        call calypso_mpi_barrier
        call end_elapsed_time(77)
        call start_elapsed_time(76)
      end if
!
      call dealloc_pvr_local_subimage(pvr_data%image)
      call deallocate_pvr_ray_start(pvr_data%start_pt)
!
!   Right eye
!
      call rendering_lic_at_once(IFLAG_RIGHT, istep_pvr, irank_tgt,     &
     &    node, ele, surf, group, lic_p, pvr_param, pvr_data)
!
      if(pvr_data%view%iflag_anaglyph .gt. 0) then
        call add_left_eye_image(irank_tgt, pvr_data%rgb)
!
        call end_elapsed_time(76)
        call start_elapsed_time(77)
!
        call sel_write_pvr_image_file(pvr_param%file,                   &
     &      iminus, istep_pvr, irank_tgt, IFLAG_NORMAL, pvr_data%rgb)
      else
        call end_elapsed_time(76)
        call start_elapsed_time(77)
!
        call sel_write_pvr_image_file(pvr_param%file,                   &
     &      iminus, istep_pvr, irank_tgt, IFLAG_RIGHT, pvr_data%rgb)
      end if
      call calypso_mpi_barrier
      call end_elapsed_time(77)
      call start_elapsed_time(76)
!
      call dealloc_pvr_local_subimage(pvr_data%image)
      call deallocate_pvr_ray_start(pvr_data%start_pt)
!
      end subroutine streo_lic_rendering_fix_view
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine streo_lic_rendering_with_rot(istep_pvr, irank_tgt,     &
     &          node, ele, surf, group, lic_p, pvr_param, pvr_data)
!
      use cal_pvr_modelview_mat
      use rendering_LIC_image
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      integer(kind = kint), intent(in) :: irank_tgt
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(lic_parameters), intent(in) :: lic_p
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(PVR_image_generator), intent(inout) :: pvr_data
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
!    Left eye
        call rendering_lic_at_once(IFLAG_LEFT, istep_pvr, irank_tgt,    &
     &      node, ele, surf, group, lic_p, pvr_param, pvr_data)
!
        if(pvr_data%view%iflag_anaglyph .gt. 0) then
          call store_left_eye_image(irank_tgt, pvr_data%rgb)
        else
          call end_elapsed_time(76)
          call start_elapsed_time(77)
!
          call sel_write_pvr_image_file(pvr_param%file,                 &
     &        i_rot, istep_pvr, irank_tgt, IFLAG_LEFT, pvr_data%rgb)
!
          call calypso_mpi_barrier
          call end_elapsed_time(77)
          call start_elapsed_time(76)
        end if
!
        call dealloc_pvr_local_subimage(pvr_data%image)
        call deallocate_pvr_ray_start(pvr_data%start_pt)
!
!    Right eye
        call rendering_lic_at_once(IFLAG_RIGHT, istep_pvr, irank_tgt,   &
     &      node, ele, surf, group, lic_p, pvr_param, pvr_data)
!
        if(pvr_data%view%iflag_anaglyph .gt. 0) then
          call add_left_eye_image(irank_tgt, pvr_data%rgb)
!
          call end_elapsed_time(76)
          call start_elapsed_time(77)
          call sel_write_pvr_image_file(pvr_param%file,                 &
     &        i_rot, istep_pvr, irank_tgt, IFLAG_NORMAL, pvr_data%rgb)
        else
          call end_elapsed_time(76)
          call start_elapsed_time(77)
!
          call sel_write_pvr_image_file(pvr_param%file,                 &
     &        i_rot, istep_pvr, irank_tgt, IFLAG_RIGHT, pvr_data%rgb)
        end if
        call calypso_mpi_barrier
        call end_elapsed_time(77)
        call start_elapsed_time(76)
!
        call dealloc_pvr_local_subimage(pvr_data%image)
        call deallocate_pvr_ray_start(pvr_data%start_pt)
      end do
!
      end subroutine streo_lic_rendering_with_rot
!
!  ---------------------------------------------------------------------
!
      end module rendering_streo_LIC_image
