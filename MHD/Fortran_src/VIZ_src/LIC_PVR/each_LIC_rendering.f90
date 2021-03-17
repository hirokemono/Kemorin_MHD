!>@file  each_LIC_rendering.f90
!!       module each_LIC_rendering
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine s_each_LIC_rendering(istep_pvr, time, mesh, nod_fld, &
!!     &          lic_fld_pm, pvr_param, pvr_proj, pvr_rgb)
!!      subroutine s_each_LIC_rendering_w_rot                           &
!!     &         (istep_pvr, time, mesh, group, nod_fld,                &
!!     &          lic_fld_pm, pvr_param, pvr_proj, pvr_rgb)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(LIC_field_params), intent(inout) :: lic_fld_pm
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!      subroutine dealloc_each_lic_data(lic_fld_pm)
!!        type(LIC_field_params), intent(inout) :: lic_fld_pm
!!@endverbatim
!
!
      module each_LIC_rendering
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
!
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_control_param_LIC_PVR
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_geometries_in_pvr_screen
      use t_lic_field_data
!
      use set_default_pvr_params
      use set_position_pvr_screen
      use mesh_outline_4_pvr
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
      subroutine s_each_LIC_rendering(istep_pvr, time, mesh, nod_fld,   &
     &          lic_fld_pm, pvr_param, pvr_proj, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use rendering_LIC_image
      use rendering_streo_LIC_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
!
      type(LIC_field_params), intent(inout) :: lic_fld_pm
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
      type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call cal_field_4_each_lic(mesh%node, nod_fld,                     &
     &    lic_fld_pm%lic_param, lic_fld_pm%field_lic)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_param%color)
!
      if(pvr_param%view%iflag_stereo_pvr .gt. 0) then
        if(pvr_param%view%iflag_anaglyph .gt. 0) then
!
!   Left eye
          call lic_rendering_with_fixed_view                            &
     &       (istep_pvr, time, mesh%node, mesh%ele, mesh%surf,          &
     &        lic_fld_pm%lic_param, lic_fld_pm%field_lic,               &
     &        pvr_param, pvr_proj(1), pvr_rgb(1))
          call store_left_eye_image(pvr_rgb(1))
!
!   Right eye
          call lic_rendering_with_fixed_view                            &
     &       (istep_pvr, time, mesh%node, mesh%ele, mesh%surf,          &
     &        lic_fld_pm%lic_param, lic_fld_pm%field_lic,               &
     &        pvr_param, pvr_proj(2), pvr_rgb(1))
          call add_left_eye_image(pvr_rgb(1))
        else
!
!   Left eye
          call lic_rendering_with_fixed_view                            &
     &       (istep_pvr, time, mesh%node, mesh%ele, mesh%surf,          &
     &        lic_fld_pm%lic_param, lic_fld_pm%field_lic,               &
     &        pvr_param, pvr_proj(1), pvr_rgb(1))
!
!   Right eye
          call lic_rendering_with_fixed_view                            &
     &       (istep_pvr, time, mesh%node, mesh%ele, mesh%surf,          &
     &        lic_fld_pm%lic_param, lic_fld_pm%field_lic,               &
     &        pvr_param, pvr_proj(2), pvr_rgb(2))
        end if
      else
        call lic_rendering_with_fixed_view                              &
     &     (istep_pvr, time, mesh%node, mesh%ele, mesh%surf,            &
     &      lic_fld_pm%lic_param, lic_fld_pm%field_lic,                 &
     &      pvr_param,  pvr_proj(1), pvr_rgb(1))
      end if
!
      end subroutine s_each_LIC_rendering
!
!  ---------------------------------------------------------------------
!
      subroutine s_each_LIC_rendering_w_rot                             &
     &         (istep_pvr, time, mesh, group, nod_fld,                  &
     &          lic_fld_pm, pvr_param, pvr_proj, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use rendering_LIC_image
      use rendering_streo_LIC_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
!
      type(LIC_field_params), intent(inout) :: lic_fld_pm
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
      type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call cal_field_4_each_lic(mesh%node, nod_fld,                     &
     &    lic_fld_pm%lic_param, lic_fld_pm%field_lic)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_param%color)
!
      if(pvr_param%view%iflag_stereo_pvr .gt. 0) then
        if(pvr_param%view%iflag_anaglyph .gt. 0) then
          call anaglyph_lic_rendering_w_rot                             &
     &       (istep_pvr, time, mesh%node, mesh%ele, mesh%surf, group,   &
     &        lic_fld_pm%lic_param, lic_fld_pm%field_lic,               &
     &        pvr_param, pvr_proj, pvr_rgb(1))
        else
          call lic_rendering_with_rotation                              &
     &       (istep_pvr, time, mesh%node, mesh%ele, mesh%surf, group,   &
     &        lic_fld_pm%lic_param, lic_fld_pm%field_lic,               &
     &        pvr_param, pvr_proj(1), pvr_rgb(1))
          call lic_rendering_with_rotation                              &
     &       (istep_pvr, time, mesh%node, mesh%ele, mesh%surf, group,   &
     &        lic_fld_pm%lic_param, lic_fld_pm%field_lic,               &
     &        pvr_param, pvr_proj(2), pvr_rgb(2))
        end if
      else
        call lic_rendering_with_rotation                                &
     &     (istep_pvr, time, mesh%node, mesh%ele, mesh%surf, group,     &
     &      lic_fld_pm%lic_param, lic_fld_pm%field_lic,                 &
     &      pvr_param, pvr_proj(1), pvr_rgb(1))
      end if
!
      end subroutine s_each_LIC_rendering_w_rot
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_each_lic_data(lic_fld_pm)
!
      use set_pvr_control
!
      type(LIC_field_params), intent(inout) :: lic_fld_pm
!
!
      call dealloc_nod_data_4_lic(lic_fld_pm%field_lic)
      call flush_each_lic_control(lic_fld_pm)
!
      end subroutine dealloc_each_lic_data
!
!  ---------------------------------------------------------------------
!
      end module each_LIC_rendering
