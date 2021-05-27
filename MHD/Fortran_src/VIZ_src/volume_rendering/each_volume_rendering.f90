!>@file   each_volume_rendering.f90
!!@brief  module each_volume_rendering
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module for each volume rendering
!!
!!@verbatim
!!      subroutine init_each_PVR_image(pvr_param, pvr_rgb)
!!      subroutine each_PVR_initialize(i_pvr, mesh, group, pvr_rgb,     &
!!     &                               pvr_param, pvr_proj)
!!      subroutine dealloc_PVR_initialize(pvr_param, pvr_proj)
!!
!!      subroutine each_PVR_rendering                                   &
!!     &         (istep_pvr, time, geofem, jacs, nod_fld,               &
!!     &          field_pvr, pvr_param, pvr_proj, pvr_rgb)
!!      subroutine each_PVR_rendering_w_rot                             &
!!     &         (istep_pvr, time, geofem, jacs, nod_fld,               &
!!     &          field_pvr, pvr_param, pvr_proj, pvr_rgb)
!!        type(mesh_data), intent(in) :: geofem
!!        type(viz_area_parameter), intent(in) :: area_def
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(pvr_field_data), intent(inout) :: field_pvr
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(PVR_projection_data), intent(inout) :: pvr_proj(2)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!!@endverbatim
!
      module each_volume_rendering
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
!
      use t_mesh_data
      use t_phys_data
      use t_jacobians
!
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_geometries_in_pvr_screen
      use t_pvr_field_data
!
      use set_default_pvr_params
      use set_position_pvr_screen
      use mesh_outline_4_pvr
      use generate_vr_image
      use rendering_streo_vr_image
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_each_PVR_image(pvr_param, pvr_rgb)
!
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!
!
      call alloc_pvr_image_array                                        &
     &   (pvr_param%view%n_pvr_pixel, pvr_rgb(1))
      if(pvr_param%view%iflag_stereo_pvr .gt. 0                         &
     &     .and. pvr_param%view%iflag_anaglyph .eq. 0) then
        call alloc_pvr_image_array                                      &
     &     (pvr_param%view%n_pvr_pixel, pvr_rgb(2))
      end if
!
      end subroutine init_each_PVR_image
!
!  ---------------------------------------------------------------------
!
      subroutine each_PVR_initialize(i_pvr, mesh, group, pvr_rgb,       &
     &                               pvr_param, pvr_proj)
!
      use t_control_data_pvr_sections
      use set_pvr_control
      use cal_pvr_modelview_mat
      use cal_pvr_projection_mat
      use find_pvr_surf_domain
      use set_iflag_for_used_ele
!
      integer(kind = kint), intent(in) :: i_pvr
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(pvr_image_type), intent(in) :: pvr_rgb(2)
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
!
!
      call alloc_iflag_pvr_used_ele(mesh%ele, pvr_param%draw_param)
      call s_set_iflag_for_used_ele(mesh%ele, group%ele_grp,            &
     &    pvr_param%area_def%nele_grp_area_pvr,                         &
     &    pvr_param%area_def%id_ele_grp_area_pvr,                       &
     &    pvr_param%draw_param%iflag_used_ele)
!
      call find_each_pvr_surf_domain                                    &
     &   (mesh%ele, mesh%surf, group%ele_grp, pvr_param%area_def,       &
     &    pvr_param%draw_param, pvr_proj(1)%bound)
      if(pvr_param%view%iflag_stereo_pvr .gt. 0) then
        call find_each_pvr_surf_domain                                  &
     &     (mesh%ele, mesh%surf, group%ele_grp, pvr_param%area_def,     &
     &      pvr_param%draw_param, pvr_proj(2)%bound)
      end if
!
      call pvr_mesh_outline(mesh%node, pvr_param%outline)
      call check_pvr_parameters                                         &
     &   (pvr_param%outline, pvr_param%view, pvr_param%color)
!
      call set_pixel_on_pvr_screen(pvr_param%view, pvr_param%pixel)
!
!
      if(pvr_param%view%iflag_stereo_pvr .gt. 0) then
        pvr_proj(1)%start_fix%irank_composit_ref = mod(i_pvr-1,nprocs)
        pvr_proj(2)%start_fix%irank_composit_ref = mod(i_pvr-1,nprocs)
!
        if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_left'
        call set_pvr_projection_left_mat                                &
     &     (i_pvr, pvr_param%view, pvr_proj(1)%projection_mat)
        if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_right'
        call set_pvr_projection_right_mat                               &
     &     (i_pvr, pvr_param%view, pvr_proj(2)%projection_mat)
      else
        pvr_proj(1)%start_fix%irank_composit_ref = mod(i_pvr-1,nprocs)
!
        if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_matrix'
        call set_pvr_projection_matrix                                  &
     &     (i_pvr, pvr_param%view, pvr_proj(1)%projection_mat)
!        call set_pvr_orthogonal_params(i_pvr, pvr_param%view)
      end if
!
      call alloc_projected_position                                     &
     &   (mesh%node, mesh%surf, pvr_proj(1)%screen)
      if(pvr_param%view%iflag_stereo_pvr .gt. 0) then
        call alloc_projected_position                                   &
     &     (mesh%node, mesh%surf, pvr_proj(2)%screen)
      end if
!
      if(iflag_debug.gt.0) write(*,*) 'set_fixed_view_and_image'
      call cal_pvr_modelview_matrix                                     &
     &   (izero, pvr_param%outline, pvr_param%view, pvr_param%color)
      call set_fixed_view_and_image                                     &
     &   (mesh, group, pvr_param, pvr_rgb(1), pvr_proj(1))
      if(pvr_param%view%iflag_stereo_pvr .gt. 0) then
        if(pvr_param%view%iflag_anaglyph .gt. 0) then
          call set_fixed_view_and_image                                 &
     &     (mesh, group, pvr_param, pvr_rgb(1), pvr_proj(2))
        else
          call set_fixed_view_and_image                                 &
     &     (mesh, group, pvr_param, pvr_rgb(2), pvr_proj(2))
        end if
      end if
!
      end subroutine each_PVR_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_PVR_initialize(pvr_param, pvr_proj)
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
!
!
      if(pvr_param%view%iflag_stereo_pvr .gt. 0) then
        call deallocate_item_pvr_ray_start(pvr_proj(2)%start_save)
        call deallocate_pvr_ray_start(pvr_proj(2)%start_fix)
        call dealloc_pvr_stencil_buffer(pvr_proj(2)%stencil)
        call dealloc_projected_position(pvr_proj(2)%screen)
        call dealloc_pvr_surf_domain_item(pvr_proj(2)%bound)
      end if
!
      call deallocate_item_pvr_ray_start(pvr_proj(1)%start_save)
      call deallocate_pvr_ray_start(pvr_proj(1)%start_fix)
      call dealloc_pvr_stencil_buffer(pvr_proj(1)%stencil)
      call dealloc_projected_position(pvr_proj(1)%screen)
      call dealloc_pvr_surf_domain_item(pvr_proj(1)%bound)
!
      call dealloc_pixel_position_pvr(pvr_param%pixel)
      call dealloc_iflag_pvr_used_ele(pvr_param%draw_param)
!
      end subroutine dealloc_PVR_initialize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine each_PVR_rendering                                     &
     &         (istep_pvr, time, geofem, jacs, nod_fld,                 &
     &          field_pvr, pvr_param, pvr_proj, pvr_rgb)
!
      use cal_pvr_modelview_mat
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(pvr_field_data), intent(inout) :: field_pvr
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
      type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call cal_field_4_each_pvr(geofem%mesh%node, geofem%mesh%ele,      &
     &    jacs%g_FEM, jacs%jac_3d, nod_fld,                             &
     &    pvr_param%field_def, field_pvr)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_param%color)
!
      if(pvr_param%view%iflag_stereo_pvr .gt. 0) then
        if(pvr_param%view%iflag_anaglyph .gt. 0) then
!
!   Left eye
          call rendering_with_fixed_view(istep_pvr, time, geofem%mesh,  &
     &        field_pvr, pvr_param, pvr_proj(1), pvr_rgb(1))
          call store_left_eye_image(pvr_rgb(1))
!
!   right eye
          call rendering_with_fixed_view(istep_pvr, time, geofem%mesh,  &
     &        field_pvr, pvr_param, pvr_proj(2), pvr_rgb(1))
          call add_left_eye_image(pvr_rgb(1))
        else
!
!   Left eye
          call rendering_with_fixed_view(istep_pvr, time, geofem%mesh,  &
     &        field_pvr, pvr_param, pvr_proj(1), pvr_rgb(1))
!
!   right eye
          call rendering_with_fixed_view(istep_pvr, time, geofem%mesh,  &
     &        field_pvr, pvr_param, pvr_proj(2), pvr_rgb(2))
        end if
      else
        call rendering_with_fixed_view(istep_pvr, time, geofem%mesh,    &
     &      field_pvr, pvr_param, pvr_proj(1), pvr_rgb(1))
      end if
!
      end subroutine each_PVR_rendering
!
!  ---------------------------------------------------------------------
!
      subroutine each_PVR_rendering_w_rot                               &
     &         (istep_pvr, time, geofem, jacs, nod_fld,                 &
     &          field_pvr, pvr_param, pvr_proj, pvr_rgb)
!
      use cal_pvr_modelview_mat
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(pvr_field_data), intent(inout) :: field_pvr
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
      type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call cal_field_4_each_pvr(geofem%mesh%node, geofem%mesh%ele,      &
     &    jacs%g_FEM, jacs%jac_3d, nod_fld,                             &
     &    pvr_param%field_def, field_pvr)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_param%color)
!
!
      if(pvr_param%view%iflag_stereo_pvr .gt. 0) then
        if(pvr_param%view%iflag_anaglyph .gt. 0) then
          call anaglyph_rendering_w_rotation                            &
     &       (istep_pvr, time, geofem%mesh, geofem%group, field_pvr,    &
     &        pvr_rgb(1), pvr_param, pvr_proj)
        else
          call rendering_with_rotation                                  &
     &       (istep_pvr, time, geofem%mesh, geofem%group, field_pvr,    &
     &        pvr_rgb(1), pvr_param, pvr_proj(1))
          call rendering_with_rotation                                  &
     &       (istep_pvr, time, geofem%mesh, geofem%group, field_pvr,    &
     &        pvr_rgb(2), pvr_param, pvr_proj(2))
        end if
      else
        call rendering_with_rotation                                    &
     &     (istep_pvr, time, geofem%mesh, geofem%group, field_pvr,      &
     &      pvr_rgb(1), pvr_param, pvr_proj(1))
      end if
!
      end subroutine each_PVR_rendering_w_rot
!
!  ---------------------------------------------------------------------
!
      end module each_volume_rendering
