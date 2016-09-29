!
!      module volume_rendering
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine PVR_initialize(node, ele, surf, group, nod_fld)
!!      subroutine PVR_visualize                                        &
!!     &         (istep_pvr, node, ele, surf, group, jac_3d, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!
!      subroutine deallocate_pvr_data
!
!
      module volume_rendering
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_jacobian_3d
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
      use find_pvr_surf_domain
      use set_pvr_ray_start_point
      use mesh_outline_4_pvr
      use generate_vr_image
!
      implicit  none
!
      integer(kind = kint) :: num_pvr = 0
!
!>      Structure of PVR control parameters
      type(PVR_control_params), allocatable, save :: pvr_param(:)
!>      Structure of PVR image generation
      type(PVR_image_generator), allocatable, save :: pvr_data(:)
!
      private :: pvr_param, pvr_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine PVR_initialize(node, ele, surf, group, nod_fld)
!
      use m_control_data_pvrs
      use set_pvr_control
      use cal_pvr_modelview_mat
      use cal_pvr_projection_mat
      use find_selected_domain_bd
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint) :: i_pvr
!
!
      num_pvr = num_pvr_ctl
      if (num_pvr .le. 0) return
!
      if(iflag_debug .gt. 0) write(*,*) 'allocate_components_4_pvr',    &
     &         num_pvr
      call allocate_components_4_pvr
!
      do i_pvr = 1, num_pvr
        call allocate_nod_data_4_pvr(node%numnod, ele%numele,           &
     &      group%surf_grp%num_grp, pvr_param(i_pvr)%field)
      end do
!
      ctl_file_code = pvr_ctl_file_code
      call allocate_pvr_ctl_struct
      if(iflag_debug .gt. 0) write(*,*) 's_set_pvr_control', num_pvr
      do i_pvr = 1, num_pvr
        call read_control_pvr(i_pvr)
        call read_control_modelview(i_pvr)
        call read_control_colormap(i_pvr)
!
        call set_each_pvr_control(group%ele_grp, group%surf_grp,        &
     &      nod_fld%num_phys, nod_fld%phys_name,                        &
     &      pvr_ctl_struct(i_pvr), pvr_param(i_pvr)%file,               &
     &      pvr_param(i_pvr)%field_def, pvr_data(i_pvr)%view,           &
     &      pvr_param(i_pvr)%field, pvr_data(i_pvr)%screen,             &
     &      pvr_data(i_pvr)%color, pvr_param(i_pvr)%colorbar)
!
        call deallocate_cont_dat_pvr(pvr_ctl_struct(i_pvr))
        call calypso_mpi_barrier
      end do
      call deallocate_pvr_file_header_ctl
!
!
      call allocate_imark_4_surface(surf%numsurf)
      do i_pvr = 1, num_pvr
        call find_each_pvr_surf_domain(ele, surf, group%ele_grp,        &
     &      pvr_param(i_pvr)%field_def, pvr_data(i_pvr)%bound,          &
     &      pvr_param(i_pvr)%field)
      end do
      call deallocate_imark_4_surface
!
      do i_pvr = 1, num_pvr
        call cal_mesh_outline_pvr                                       &
     &     (node%numnod, node%xx, pvr_param(i_pvr)%outline)
        call check_pvr_parameters(pvr_param(i_pvr)%outline,             &
     &      pvr_data(i_pvr)%view, pvr_data(i_pvr)%color,                &
     &      pvr_data(i_pvr)%screen)
!
        if(iflag_debug .gt. 0) write(*,*) 'set_pixel_on_pvr_screen'
        call set_pixel_on_pvr_screen                                    &
     &    (pvr_data(i_pvr)%view, pvr_param(i_pvr)%pixel)
!
        call alloc_pvr_image_array_type                                 &
     &     (pvr_data(i_pvr)%view, pvr_data(i_pvr)%image)
!
        if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_matrix'
        call set_pvr_projection_matrix(i_pvr, pvr_data(i_pvr)%view)
!
        if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_left'
        call set_pvr_projection_left_mat(i_pvr, pvr_data(i_pvr)%view)
        if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_right'
        call set_pvr_projection_right_mat(i_pvr, pvr_data(i_pvr)%view)
!        call set_pvr_orthogonal_params(i_pvr, pvr_data(i_pvr)%view)
!
        call alloc_projected_position                                   &
     &     (node%numnod, surf%numsurf, pvr_data(i_pvr)%screen)
!
        if(pvr_data(i_pvr)%view%iflag_rotate_snap .eq. 0) then
          if(iflag_debug .gt. 0) write(*,*) 'set_fixed_view_and_image'
          call set_fixed_view_and_image(node, ele, surf, group,         &
     &        pvr_param(i_pvr), pvr_data(i_pvr))
        end if
      end do
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
!
      end subroutine PVR_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine PVR_visualize                                          &
     &         (istep_pvr, node, ele, surf, group, jac_3d, nod_fld)
!
      use cal_pvr_modelview_mat
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind = kint) :: i_pvr
!
!
      if(num_pvr.le.0 .or. istep_pvr.le.0) return
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      do i_pvr = 1, num_pvr
        call cal_field_4_each_pvr(node, ele, jac_3d,                    &
     &      nod_fld%n_point, nod_fld%num_phys, nod_fld%ntot_phys,       &
     &      nod_fld%istack_component, nod_fld%d_fld,                    &
     &      pvr_param(i_pvr)%field_def, pvr_param(i_pvr)%field)
      end do
!
      do i_pvr = 1, num_pvr
        if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
        call set_default_pvr_data_params                                &
     &     (pvr_param(i_pvr)%outline, pvr_data(i_pvr)%color)
!
        if(pvr_data(i_pvr)%view%iflag_rotate_snap .gt. 0) then
          call rendering_with_rotation                                  &
     &       (istep_pvr, node, ele, surf, group,                        &
     &        pvr_param(i_pvr), pvr_data(i_pvr))
        else
          call rendering_with_fixed_view(istep_pvr, node, ele, surf,    &
     &        pvr_param(i_pvr), pvr_data(i_pvr))
        end if
      end do
!
      end subroutine PVR_visualize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_components_4_pvr
!
      integer(kind = kint) :: i_pvr
!
!
      allocate(pvr_param(num_pvr))
      allocate(pvr_data(num_pvr))
      do i_pvr = 1, num_pvr
        call reset_pvr_view_parameteres(pvr_data(i_pvr)%view)
      end do
!
!
      end subroutine allocate_components_4_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_pvr_data
!
      integer(kind = kint) :: i_pvr
!
!
      do i_pvr = 1, num_pvr
        call dealloc_pvr_element_group(pvr_param(i_pvr)%field_def)
        call dealloc_pvr_color_parameteres(pvr_data(i_pvr)%color)
      end do
      deallocate(pvr_param, pvr_data)
!
      end subroutine deallocate_pvr_data
!
!  ---------------------------------------------------------------------
!
      end module volume_rendering
