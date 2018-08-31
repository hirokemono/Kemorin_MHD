!
!      module each_volume_rendering
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine s_find_pvr_surf_domain(num_pvr, mesh, group,         &
!!     &          ele_mesh, pvr_fld, pvr_param, pvr_data)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(PVR_field_params), intent(in) :: pvr_fld(num_pvr)
!!        type(PVR_control_params), intent(inout) :: pvr_param(num_pvr)
!!        type(PVR_image_generator), intent(inout) :: pvr_data(num_pvr)
!!
!!      subroutine each_PVR_initialize(i_pvr, irank_tgt,                &
!!     &          mesh, group, ele_mesh, pvr_param, pvr_data, pvr_rgb)
!!      subroutine each_PVR_rendering(istep_pvr, irank_tgt,             &
!!     &          mesh, group, ele_mesh, jacs, nod_fld,                 &
!!     &          pvr_fld, pvr_param, pvr_data, pvr_rgb)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(PVR_image_generator), intent(inout) :: pvr_data
!!      subroutine dealloc_each_pvr_data                                &
!!     &         (pvr_fld, pvr_param, pvr_data, pvr_rgb)
!!        type(PVR_field_params), intent(inout) :: pvr_fld
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(PVR_image_generator), intent(inout) :: pvr_data
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
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
      subroutine s_find_pvr_surf_domain(num_pvr, mesh, group,           &
     &          ele_mesh, pvr_fld, pvr_param, pvr_data)
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
      type(PVR_field_params), intent(in) :: pvr_fld(num_pvr)
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
     &      pvr_fld(i_pvr)%area_def, pvr_data(i_pvr)%bound,             &
     &      pvr_param(i_pvr)%field)
      end do
      call deallocate_imark_4_surface
!
      end subroutine s_find_pvr_surf_domain
!
!  ---------------------------------------------------------------------
!
      subroutine each_PVR_initialize(i_pvr, irank_tgt,                  &
     &          mesh, group, ele_mesh, pvr_param, pvr_data, pvr_rgb)
!
      use t_control_data_pvr_misc
      use set_pvr_control
      use cal_pvr_modelview_mat
      use cal_pvr_projection_mat
      use find_selected_domain_bd
!
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint), intent(in) :: irank_tgt
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(element_geometry), intent(in) :: ele_mesh
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      call pvr_mesh_outline(mesh%node, pvr_param%outline)
      call check_pvr_parameters(pvr_param%outline,                      &
     &    pvr_data%view, pvr_data%color, pvr_data%screen)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_pixel_on_pvr_screen'
      call set_pixel_on_pvr_screen                                      &
     &   (irank_tgt, pvr_data%view, pvr_param%pixel, pvr_rgb)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_matrix'
      call set_pvr_projection_matrix(i_pvr, pvr_data%view)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_left'
      call set_pvr_projection_left_mat(i_pvr, pvr_data%view)
      if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_right'
      call set_pvr_projection_right_mat(i_pvr, pvr_data%view)
!        call set_pvr_orthogonal_params(i_pvr, pvr_data%view)
!
      call alloc_projected_position                                     &
     &   (mesh%node, ele_mesh%surf, pvr_data%screen)
!
      if(pvr_data%view%iflag_rotate_snap .eq. 0) then
        if(pvr_data%view%iflag_stereo_pvr .eq. 0) then
          if(iflag_debug.gt.0) write(*,*) 'set_fixed_view_and_image'
          call set_fixed_view_and_image                                 &
     &       (mesh%node, mesh%ele, ele_mesh%surf, group,                &
     &        pvr_param, pvr_rgb, pvr_data)
        end if
      end if
!
      end subroutine each_PVR_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine each_PVR_rendering(istep_pvr, irank_tgt,               &
     &          mesh, group, ele_mesh, jacs, nod_fld,                   &
     &          pvr_fld, pvr_param, pvr_data, pvr_rgb)
!
      use cal_pvr_modelview_mat
!
      integer(kind = kint), intent(in) :: istep_pvr
      integer(kind = kint), intent(in) :: irank_tgt
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
      type(PVR_field_params), intent(in) :: pvr_fld
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
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
      if(pvr_data%view%iflag_rotate_snap .gt. 0) then
        if(pvr_data%view%iflag_stereo_pvr .gt. 0) then
          call streo_rendering_with_rotation(istep_pvr, irank_tgt,      &
     &        mesh%node, mesh%ele, ele_mesh%surf, group,                &
     &        pvr_param, pvr_data, pvr_rgb)
        else
          call rendering_with_rotation(istep_pvr, irank_tgt,            &
     &        mesh%node, mesh%ele, ele_mesh%surf, group,                &
     &        pvr_param, pvr_data, pvr_rgb)
        end if
      else
        if(pvr_data%view%iflag_stereo_pvr .gt. 0) then
          call streo_rendering_fixed_view                               &
     &       (istep_pvr, irank_tgt, mesh%node, mesh%ele, ele_mesh%surf, &
     &        group, pvr_param, pvr_data, pvr_rgb)
        else
          call rendering_with_fixed_view                                &
     &       (istep_pvr, irank_tgt, mesh%node, mesh%ele, ele_mesh%surf, &
     &        pvr_param, pvr_data, pvr_rgb)
        end if
      end if
!
      end subroutine each_PVR_rendering
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_each_pvr_data                                  &
     &         (pvr_fld, pvr_param, pvr_data, pvr_rgb)
!
      use set_pvr_control
!
      type(PVR_field_params), intent(inout) :: pvr_fld
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      if(pvr_data%view%iflag_rotate_snap .eq. 0                         &
     &    .and. pvr_data%view%iflag_stereo_pvr .eq. 0) then
          call flush_rendering_4_fixed_view(pvr_data)
      end if
      call flush_pixel_on_pvr_screen(pvr_param%pixel, pvr_rgb)
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
