!>@file  each_LIC_rendering.f90
!!       module each_LIC_rendering
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine find_lic_surf_domain(num_pvr, mesh, group, ele_mesh, &
!!     &          lic_fld, pvr_param, pvr_data)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(LIC_field_params), intent(in) :: lic_fld(num_pvr)
!!        type(PVR_control_params), intent(inout) :: pvr_param(num_pvr)
!!        type(PVR_image_generator), intent(inout) :: pvr_data(num_pvr)
!!      subroutine s_each_LIC_rendering                                 &
!!     &         (istep_pvr, mesh, group, ele_mesh, jacs, nod_fld,      &
!!     &          lic_fld, file_param, pvr_param, pvr_data, pvr_rgb)
!!      subroutine s_each_LIC_rendering_w_rot                           &
!!     &         (istep_pvr, mesh, group, ele_mesh, jacs, nod_fld,      &
!!     &          lic_fld, file_param, pvr_param, pvr_data, pvr_rgb)
!
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(LIC_field_params), intent(in) :: lic_fld
!!        type(pvr_output_parameter), intent(in) :: file_param
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(PVR_image_generator), intent(inout) :: pvr_data
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!      subroutine dealloc_each_lic_data(lic_fld, pvr_param, pvr_data)
!!        type(PVR_field_params), intent(inout) :: pvr_fld
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(PVR_image_generator), intent(inout) :: pvr_data
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
      use t_jacobians
!
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_control_param_LIC_PVR
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_geometries_in_pvr_screen
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
      subroutine find_lic_surf_domain(num_pvr, mesh, group, ele_mesh,   &
     &          lic_fld, pvr_param, pvr_data)
!
      use t_mesh_data
      use t_rendering_vr_image
      use find_selected_domain_bd
      use find_pvr_surf_domain
!
      integer(kind = kint), intent(in) :: num_pvr
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(element_geometry), intent(in) :: ele_mesh
      type(LIC_field_params), intent(in) :: lic_fld(num_pvr)
!
      type(PVR_control_params), intent(inout) :: pvr_param(num_pvr)
      type(PVR_image_generator), intent(inout) :: pvr_data(num_pvr)
!
      integer(kind = kint) :: i_pvr
!
!
      call allocate_imark_4_surface(ele_mesh%surf%numsurf)
      do i_pvr = 1, num_pvr
        call find_each_pvr_surf_domain                                  &
     &     (mesh%ele, ele_mesh%surf, group%ele_grp,                     &
     &      lic_fld(i_pvr)%area_def, pvr_data(i_pvr)%bound,             &
     &      pvr_param(i_pvr)%field)
      end do
      call deallocate_imark_4_surface
!
      end subroutine find_lic_surf_domain
!
!  ---------------------------------------------------------------------
!
      subroutine s_each_LIC_rendering                                   &
     &         (istep_pvr, mesh, group, ele_mesh, jacs, nod_fld,        &
     &          lic_fld, file_param, pvr_param, pvr_data, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use field_data_4_LIC
      use rendering_LIC_image
      use rendering_streo_LIC_image
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
      type(LIC_field_params), intent(in) :: lic_fld
      type(pvr_output_parameter), intent(in) :: file_param(2)
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call cal_field_4_each_lic                                         &
     &   (mesh%node, mesh%ele, jacs%g_FEM, jacs%jac_3d, nod_fld,        &
     &    lic_fld%lic_param, pvr_param%field)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_data%color)
!
      if(pvr_data%view%iflag_stereo_pvr .gt. 0) then
        if(pvr_data%view%iflag_anaglyph .gt. 0) then
!
!   Left eye
          call streo_lic_rendering_fix_view                             &
     &       (istep_pvr, file_param(1)%irank_image_file,                &
     &        mesh%node, mesh%ele, ele_mesh%surf, group,                &
     &        lic_fld%lic_param, pvr_param, file_param(1),              &
     &        pvr_data%view%projection_left, pvr_data%start_pt,         &
     &        pvr_data%image, pvr_data, pvr_rgb(1))
          call store_left_eye_image                                     &
     &      (file_param(1)%irank_image_file, pvr_rgb(1))
!
!   Right eye
          call streo_lic_rendering_fix_view                             &
     &       (istep_pvr, file_param(1)%irank_image_file,                &
     &        mesh%node, mesh%ele, ele_mesh%surf, group,                &
     &        lic_fld%lic_param, pvr_param, file_param(1),              &
     &        pvr_data%view%projection_right, pvr_data%start_pt,        &
     &        pvr_data%image, pvr_data, pvr_rgb(1))
          call add_left_eye_image                                       &
     &       (file_param(1)%irank_image_file, pvr_rgb(1))
!
          call end_elapsed_time(76)
          call start_elapsed_time(77)
          call sel_write_pvr_image_file(file_param(1),                  &
     &        iminus, istep_pvr, file_param(1)%irank_image_file,        &
     &        pvr_rgb(1))
          call calypso_mpi_barrier
          call end_elapsed_time(77)
          call start_elapsed_time(76)
        else
!
!   Left eye
          call streo_lic_rendering_fix_view                             &
     &       (istep_pvr, file_param(1)%irank_image_file,                &
     &        mesh%node, mesh%ele, ele_mesh%surf, group,                &
     &        lic_fld%lic_param, pvr_param, file_param(1),              &
     &        pvr_data%view%projection_left, pvr_data%start_pt,         &
     &        pvr_data%image, pvr_data, pvr_rgb(1))
!
          call end_elapsed_time(76)
          call start_elapsed_time(77)
          call sel_write_pvr_image_file(file_param(1),                  &
     &        iminus, istep_pvr, file_param(1)%irank_image_file,        &
     &        pvr_rgb(1))
          call calypso_mpi_barrier
          call end_elapsed_time(77)
          call start_elapsed_time(76)
!
!   Right eye
          call streo_lic_rendering_fix_view                             &
     &       (istep_pvr, file_param(2)%irank_image_file,                &
     &        mesh%node, mesh%ele, ele_mesh%surf, group,                &
     &        lic_fld%lic_param, pvr_param, file_param(2),              &
     &        pvr_data%view%projection_right, pvr_data%start_pt,        &
     &        pvr_data%image, pvr_data, pvr_rgb(2))
!
          call end_elapsed_time(76)
          call start_elapsed_time(77)
          call sel_write_pvr_image_file(file_param(2),                  &
     &        iminus, istep_pvr, file_param(2)%irank_image_file,        &
     &        pvr_rgb(2))
          call calypso_mpi_barrier
          call end_elapsed_time(77)
          call start_elapsed_time(76)
        end if
      else
        call lic_rendering_with_fixed_view                              &
     &     (istep_pvr, file_param(1)%irank_image_file,                  &
     &      mesh%node, mesh%ele, ele_mesh%surf,                         &
     &      lic_fld%lic_param, pvr_param, file_param(1),                &
     &      pvr_data%start_pt, pvr_data%image, pvr_data, pvr_rgb(1))
!
        call end_elapsed_time(76)
        call start_elapsed_time(77)
        call sel_write_pvr_image_file(file_param(1), iminus,            &
     &      istep_pvr, file_param(1)%irank_image_file, pvr_rgb(1))
!
        if(file_param(1)%iflag_monitoring .gt. 0) then
          call sel_write_pvr_image_file(file_param(1), iminus,          &
     &        iminus, file_param(1)%irank_image_file, pvr_rgb(1))
        end if
        call calypso_mpi_barrier
        call end_elapsed_time(77)
        call start_elapsed_time(76)
      end if
!
      end subroutine s_each_LIC_rendering
!
!  ---------------------------------------------------------------------
!
      subroutine s_each_LIC_rendering_w_rot                             &
     &         (istep_pvr, mesh, group, ele_mesh, jacs, nod_fld,        &
     &          lic_fld, file_param, pvr_param, pvr_data, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use field_data_4_LIC
      use rendering_LIC_image
      use rendering_streo_LIC_image
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
      type(LIC_field_params), intent(in) :: lic_fld
      type(pvr_output_parameter), intent(in) :: file_param(2)
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_image_type), intent(inout) :: pvr_rgb(2)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call cal_field_4_each_lic                                         &
     &   (mesh%node, mesh%ele, jacs%g_FEM, jacs%jac_3d, nod_fld,        &
     &    lic_fld%lic_param, pvr_param%field)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_data%color)
!
      if(pvr_data%view%iflag_stereo_pvr .gt. 0) then
        if(pvr_data%view%iflag_anaglyph .gt. 0) then
          call anaglyph_lic_rendering_w_rot                             &
     &       (istep_pvr, file_param(1)%irank_image_file,                &
     &        mesh%node, mesh%ele, ele_mesh%surf, group,                &
     &        lic_fld%lic_param, pvr_param, file_param(1),              &
     &        pvr_data%view%projection_left,                            &
     &        pvr_data%view%projection_right, pvr_data, pvr_rgb(1))
        else
          call lic_rendering_with_rotation                              &
     &       (istep_pvr, file_param(1)%irank_image_file,                &
     &        mesh%node, mesh%ele, ele_mesh%surf, group,                &
     &        lic_fld%lic_param, pvr_param, file_param(1),              &
     &        pvr_data%view%projection_left, pvr_data, pvr_rgb(1))
          call lic_rendering_with_rotation                              &
     &       (istep_pvr, file_param(2)%irank_image_file,                &
     &        mesh%node, mesh%ele, ele_mesh%surf, group,                &
     &        lic_fld%lic_param, pvr_param, file_param(2),              &
     &        pvr_data%view%projection_right, pvr_data, pvr_rgb(2))
        end if
      else
        call lic_rendering_with_rotation                                &
     &     (istep_pvr, file_param(1)%irank_image_file,                  &
     &      mesh%node, mesh%ele, ele_mesh%surf, group,                  &
     &      lic_fld%lic_param, pvr_param, file_param(1),                &
     &      pvr_data%view%projection_mat, pvr_data, pvr_rgb(1))
      end if
!
      end subroutine s_each_LIC_rendering_w_rot
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_each_lic_data(lic_fld, pvr_param, pvr_data)
!
      use set_pvr_control
      use field_data_4_pvr
!
      type(LIC_field_params), intent(inout) :: lic_fld
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
      call dealloc_nod_data_4_lic(pvr_param%field)
      call dealloc_nod_data_4_pvr(pvr_param%field)
      call flush_each_lic_control(lic_fld, pvr_data, pvr_param)
!
      end subroutine dealloc_each_lic_data
!
!  ---------------------------------------------------------------------
!
      end module each_LIC_rendering
