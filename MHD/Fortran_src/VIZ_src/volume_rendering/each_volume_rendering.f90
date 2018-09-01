!
!      module each_volume_rendering
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine each_PVR_initialize(i_pvr, mesh, group, ele_mesh,    &
!!     &          area_def, file_param, pvr_param, pvr_data,            &
!!     &          pvr_proj, pvr_rgb)
!!      subroutine each_PVR_rendering                                   &
!!     &         (istep_pvr, mesh, group, ele_mesh, jacs, nod_fld,      &
!!     &          pvr_fld, file_param, pvr_param, pvr_data,             &
!!     &          pvr_proj, pvr_rgb)
!!      subroutine each_PVR_rendering_w_rot                             &
!!     &         (istep_pvr, mesh, group, ele_mesh, jacs, nod_fld,      &
!!     &          pvr_fld, file_param, pvr_param, pvr_data,             &
!!     &          pvr_proj, pvr_rgb)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(viz_area_parameter), intent(in) :: area_def
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(PVR_image_generator), intent(inout) :: pvr_data
!!        type(pvr_projection_data), intent(inout) :: pvr_proj(2)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!!      subroutine dealloc_each_pvr_data(pvr_fld, pvr_param, pvr_data)
!!        type(PVR_field_params), intent(inout) :: pvr_fld
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(PVR_image_generator), intent(inout) :: pvr_data
!
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
!
      use field_data_4_pvr
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
      subroutine each_PVR_initialize(i_pvr, mesh, group, ele_mesh,      &
     &          area_def, file_param, pvr_param, pvr_data,              &
     &          pvr_proj, pvr_rgb)
!
      use t_control_data_pvr_misc
      use set_pvr_control
      use cal_pvr_modelview_mat
      use cal_pvr_projection_mat
      use find_pvr_surf_domain
!
      integer(kind = kint), intent(in) :: i_pvr
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(element_geometry), intent(in) :: ele_mesh
      type(viz_area_parameter), intent(in) :: area_def
      type(pvr_output_parameter), intent(in) :: file_param(2)
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_projection_data), intent(inout) :: pvr_proj(2)
      type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!
!
      call find_each_pvr_surf_domain                                    &
     &   (mesh%ele, ele_mesh%surf, group%ele_grp, area_def,             &
     &    pvr_param%field, pvr_data%bound)
!
      call pvr_mesh_outline(mesh%node, pvr_param%outline)
      call check_pvr_parameters(pvr_param%outline,                      &
     &    pvr_data%view, pvr_data%color, pvr_data%screen)
!
      call set_pixel_on_pvr_screen(pvr_data%view, pvr_param%pixel)
!
!
      if(pvr_data%view%iflag_stereo_pvr .gt. 0) then
        if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_left'
        call set_pvr_projection_left_mat                                &
     &     (i_pvr, pvr_data%view, pvr_proj(1)%projection_mat)
        if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_right'
        call set_pvr_projection_right_mat                               &
     &     (i_pvr, pvr_data%view, pvr_proj(2)%projection_mat)
      else
        if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_matrix'
        call set_pvr_projection_matrix                                  &
     &     (i_pvr, pvr_data%view, pvr_proj(1)%projection_mat)
!        call set_pvr_orthogonal_params(i_pvr, pvr_data%view)
      end if
!
      call alloc_projected_position                                     &
     &   (mesh%node, ele_mesh%surf, pvr_data%screen)
!
      call alloc_pvr_image_array_type                                   &
     &   (file_param(1)%irank_image_file, pvr_data%view%n_pvr_pixel,    &
     &    pvr_rgb(1))
      if(pvr_data%view%iflag_stereo_pvr .gt. 0                          &
     &     .and. pvr_data%view%iflag_anaglyph .eq. 0) then
        call alloc_pvr_image_array_type                                 &
     &     (file_param(2)%irank_image_file, pvr_data%view%n_pvr_pixel,  &
     &      pvr_rgb(2))
      end if
!
      if(pvr_data%view%iflag_rotate_snap .eq. 0) then
        if(pvr_data%view%iflag_stereo_pvr .eq. 0) then
          if(iflag_debug.gt.0) write(*,*) 'set_fixed_view_and_image'
          call set_fixed_view_and_image                                 &
     &       (mesh%node, mesh%ele, ele_mesh%surf, group, pvr_param,     &
     &        pvr_rgb(1), pvr_proj(1)%projection_mat,                   &
     &        pvr_data%start_pt, pvr_data%image, pvr_data)
        end if
      end if
!
      end subroutine each_PVR_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine each_PVR_rendering                                     &
     &         (istep_pvr, mesh, group, ele_mesh, jacs, nod_fld,        &
     &          pvr_fld, file_param, pvr_param, pvr_data,               &
     &          pvr_proj, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
      type(PVR_field_params), intent(in) :: pvr_fld
      type(pvr_output_parameter), intent(in) :: file_param(2)
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_projection_data), intent(inout) :: pvr_proj(2)
      type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call cal_field_4_each_pvr                                         &
     &   (mesh%node, mesh%ele, jacs%g_FEM, jacs%jac_3d, nod_fld,        &
     &    pvr_fld%field_def, pvr_param%field)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_data%color)
!
      if(pvr_data%view%iflag_stereo_pvr .gt. 0) then
        if(pvr_data%view%iflag_anaglyph .gt. 0) then
!
!   Left eye
          call streo_rendering_fixed_view                               &
     &       (istep_pvr, mesh%node, mesh%ele, ele_mesh%surf, group,     &
     &        pvr_param, file_param(1), pvr_proj(1)%projection_mat,     &
     &        pvr_data%start_pt, pvr_data%image, pvr_data, pvr_rgb(1))
          call store_left_eye_image                                     &
     &       (file_param(1)%irank_image_file, pvr_rgb(1))
!
!   right eye
          call streo_rendering_fixed_view                               &
     &       (istep_pvr, mesh%node, mesh%ele, ele_mesh%surf, group,     &
     &        pvr_param, file_param(1), pvr_proj(2)%projection_mat,     &
     &        pvr_data%start_pt, pvr_data%image, pvr_data, pvr_rgb(1))
          call add_left_eye_image                                       &
     &       (file_param(1)%irank_image_file, pvr_rgb(1))
!
          call end_elapsed_time(71)
          call start_elapsed_time(72)
          call sel_write_pvr_image_file                                 &
     &       (file_param(1), iminus, istep_pvr,  pvr_rgb(1))
          call calypso_mpi_barrier
          call end_elapsed_time(72)
          call start_elapsed_time(71)
        else
!
!   Left eye
          call streo_rendering_fixed_view                               &
     &       (istep_pvr, mesh%node, mesh%ele, ele_mesh%surf, group,     &
     &        pvr_param, file_param(1), pvr_proj(1)%projection_mat,     &
     &        pvr_data%start_pt, pvr_data%image, pvr_data, pvr_rgb(1))
!
          call end_elapsed_time(71)
          call start_elapsed_time(72)
          call sel_write_pvr_image_file                                 &
     &       (file_param(1), iminus, istep_pvr, pvr_rgb(1))
          call calypso_mpi_barrier
          call end_elapsed_time(72)
          call start_elapsed_time(71)
!
!   right eye
          call streo_rendering_fixed_view                               &
     &       (istep_pvr, mesh%node, mesh%ele, ele_mesh%surf, group,     &
     &        pvr_param, file_param(2), pvr_proj(2)%projection_mat,     &
     &        pvr_data%start_pt, pvr_data%image, pvr_data, pvr_rgb(2))
!
          call end_elapsed_time(71)
          call start_elapsed_time(72)
          call sel_write_pvr_image_file                                 &
     &       (file_param(2), iminus, istep_pvr, pvr_rgb(2))
          call calypso_mpi_barrier
          call end_elapsed_time(72)
          call start_elapsed_time(71)
        end if
      else
        call rendering_with_fixed_view                                  &
     &     (istep_pvr, mesh%node, mesh%ele, ele_mesh%surf,              &
     &      pvr_param, file_param(1),                                   &
     &      pvr_data%start_pt, pvr_data%image, pvr_data, pvr_rgb(1))
!
        call end_elapsed_time(71)
        call start_elapsed_time(72)
        call sel_write_pvr_image_file                                   &
     &     (file_param(1), iminus, istep_pvr, pvr_rgb(1))
!
        if(file_param(1)%iflag_monitoring .gt. 0) then
          call sel_write_pvr_image_file                                 &
     &       (file_param(1), iminus, iminus, pvr_rgb(1))
        end if
        call calypso_mpi_barrier
        call end_elapsed_time(72)
        call start_elapsed_time(71)
      end if
!
      end subroutine each_PVR_rendering
!
!  ---------------------------------------------------------------------
!
      subroutine each_PVR_rendering_w_rot                               &
     &         (istep_pvr, mesh, group, ele_mesh, jacs, nod_fld,        &
     &          pvr_fld, file_param, pvr_param, pvr_data,               &
     &          pvr_proj, pvr_rgb)
!
      use cal_pvr_modelview_mat
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
      type(PVR_field_params), intent(in) :: pvr_fld
      type(pvr_output_parameter), intent(in) :: file_param(2)
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_projection_data), intent(inout) :: pvr_proj(2)
      type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call cal_field_4_each_pvr                                         &
     &   (mesh%node, mesh%ele, jacs%g_FEM, jacs%jac_3d, nod_fld,        &
     &    pvr_fld%field_def, pvr_param%field)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_data%color)
!
      if(pvr_data%view%iflag_stereo_pvr .gt. 0) then
        if(pvr_data%view%iflag_anaglyph .gt. 0) then
          call anaglyph_rendering_w_rotation                            &
     &       (istep_pvr, mesh%node, mesh%ele, ele_mesh%surf, group,     &
     &        pvr_param, file_param(1), pvr_proj(1)%projection_mat,     &
     &        pvr_proj(2)%projection_mat, pvr_data, pvr_rgb(1))
        else
          call rendering_with_rotation                                  &
     &       (istep_pvr, mesh%node, mesh%ele, ele_mesh%surf, group,     &
     &        pvr_param, file_param(1), pvr_proj(1)%projection_mat,     &
     &        pvr_data, pvr_rgb(1))
          call rendering_with_rotation                                  &
     &       (istep_pvr, mesh%node, mesh%ele, ele_mesh%surf, group,     &
     &        pvr_param, file_param(2), pvr_proj(2)%projection_mat,     &
     &        pvr_data, pvr_rgb(2))
        end if
      else
        call rendering_with_rotation                                    &
     &     (istep_pvr, mesh%node, mesh%ele, ele_mesh%surf, group,       &
     &      pvr_param, file_param(1), pvr_proj(1)%projection_mat,       &
     &      pvr_data, pvr_rgb(1))
      end if
!
      end subroutine each_PVR_rendering_w_rot
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_each_pvr_data(pvr_fld, pvr_param, pvr_data)
!
      use set_pvr_control
!
      type(PVR_field_params), intent(inout) :: pvr_fld
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_image_generator), intent(inout) :: pvr_data
!
!
      if(pvr_data%view%iflag_rotate_snap .eq. 0                         &
     &    .and. pvr_data%view%iflag_stereo_pvr .eq. 0) then
          call flush_rendering_4_fixed_view(pvr_data)
      end if
      call deallocate_pixel_position_pvr(pvr_param%pixel)
!
      call dealloc_projected_position(pvr_data%screen)
!
      call dealloc_pvr_surf_domain_item(pvr_data%bound)
      call dealloc_nod_data_4_pvr(pvr_param%field)
      call flush_each_pvr_control(pvr_fld, pvr_data, pvr_param)
!
      end subroutine dealloc_each_pvr_data
!
!  ---------------------------------------------------------------------
!
      end module each_volume_rendering
