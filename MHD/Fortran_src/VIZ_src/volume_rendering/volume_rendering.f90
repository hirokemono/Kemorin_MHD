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
!>  Structure for field parameter for PVR
!    file_params(i_pvr)%
      type(pvr_output_parameter), allocatable, save :: file_params(:)
!
!>  Structure for field parameter for PVR
      type(pvr_field_parameter), allocatable, save :: fld_params(:)
!
!>  Structure for view parameteres
      type(pvr_view_parameter), allocatable, save :: view_params(:)
!
!>  Structure for PVR colormap
      type(pvr_colormap_parameter), allocatable, save :: color_params(:)
!>  Structure for PVR colormap
      type(pvr_colorbar_parameter), allocatable, save :: cbar_params(:)
!
!
      type(pvr_bounds_surf_ctl), allocatable, save :: pvr_bound(:)
!
      type(pvr_domain_outline), allocatable, save :: outlines(:)
!
!>    Data for rendering
      type(pvr_projected_field), allocatable, save :: field_pvr(:)
!
      type(pvr_pixel_position_type), allocatable, save :: pixel_xy(:)
!
      type(pvr_ray_start_type), allocatable, save :: pvr_start(:)
      type(pvr_image_type), allocatable, save :: pvr_img(:)
!
      private :: file_params, fld_params, view_params
      private :: color_params, cbar_params
      private :: pvr_bound, outlines, field_pvr, pixel_xy
      private :: pvr_start, pvr_img
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
      call allocate_nod_data_4_pvr(num_pvr, node%numnod,                &
     &    ele%numele, surf%numsurf, group%surf_grp%num_grp, field_pvr)
      if(iflag_debug .gt. 0) write(*,*) 's_set_pvr_control', num_pvr
      call s_set_pvr_control(num_pvr, group%ele_grp, group%surf_grp,    &
     &    nod_fld%num_phys, nod_fld%phys_name, file_params, fld_params, &
     &    view_params, field_pvr, color_params, cbar_params)
      call calypso_mpi_barrier
!
      call s_find_pvr_surf_domain                                       &
     &   (num_pvr, ele%numele, surf%numsurf, ele%interior_ele,          &
     &    surf%isf_4_ele, surf%iele_4_surf,                             &
     &    group%ele_grp, fld_params, pvr_bound, field_pvr)
!
      do i_pvr = 1, num_pvr
        call cal_mesh_outline_pvr                                       &
     &     (node%numnod, node%xx, outlines(i_pvr))
        call check_pvr_parameters(outlines(i_pvr), view_params(i_pvr),  &
     &      color_params(i_pvr))
!
        if(iflag_debug .gt. 0) write(*,*) 'set_pixel_on_pvr_screen'
        call set_pixel_on_pvr_screen                                    &
     &     (view_params(i_pvr)%n_pvr_pixel, pixel_xy(i_pvr))
!
        call alloc_pvr_image_array_type                                 &
     &     (view_params(i_pvr)%n_pvr_pixel, pvr_img(i_pvr))
!
        if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_matrix'
        call set_pvr_projection_matrix(i_pvr, view_params(i_pvr))
!        call set_pvr_orthogonal_params(i_pvr, view_params(i_pvr))
!
        if(view_params(i_pvr)%iflag_rotate_snap .eq. 0) then
          if(iflag_debug .gt. 0) write(*,*) 'cal_pvr_modelview_matrix'
          call cal_pvr_modelview_matrix(izero, outlines(i_pvr),         &
     &        view_params(i_pvr), color_params(i_pvr))
          call transfer_to_screen                                       &
     &       (node, ele, surf, group%surf_grp, group%surf_grp_geom,     &
     &        field_pvr(i_pvr), view_params(i_pvr), pvr_bound(i_pvr),   &
     &        pixel_xy(i_pvr), pvr_start(i_pvr))
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
      integer(kind = kint) :: i_rot, ist_rot, ied_rot
!
!
      if(num_pvr.le.0 .or. istep_pvr.le.0) return
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call cal_field_4_pvr                                              &
     &  (num_pvr, node%numnod, ele%numele, ele%nnod_4_ele,              &
     &   node%istack_nod_smp, ele%istack_ele_smp, node%xx, node%rr,     &
     &   node%a_r, node%ss, node%a_s, ele%ie, ele%a_vol_ele,            &
     &   jac_3d%ntot_int, jac_3d%dnx, jac_3d%xjac,                      &
     &   nod_fld%num_phys, nod_fld%ntot_phys, nod_fld%istack_component, &
     &   nod_fld%d_fld, fld_params, field_pvr)
!
      do i_pvr = 1, num_pvr
        if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
        call set_default_pvr_data_params                                &
     &     (outlines(i_pvr)%d_minmax_pvr, color_params(i_pvr))
!
        ist_rot = view_params(i_pvr)%istart_rot
        ied_rot = view_params(i_pvr)%iend_rot
        do i_rot = ist_rot, ied_rot
          if(view_params(i_pvr)%iflag_rotate_snap .gt. 0) then
            call cal_pvr_modelview_matrix(i_rot, outlines(i_pvr),       &
     &          view_params(i_pvr), color_params(i_pvr))
            call transfer_to_screen                                     &
     &         (node, ele, surf, group%surf_grp, group%surf_grp_geom,   &
     &          field_pvr(i_pvr), view_params(i_pvr), pvr_bound(i_pvr), &
     &          pixel_xy(i_pvr), pvr_start(i_pvr))
          end if
!
          call rendering_image(i_rot, istep_pvr, node, ele, surf,       &
     &       file_params(i_pvr), color_params(i_pvr),                   &
     &       cbar_params(i_pvr), view_params(i_pvr),                    &
     &       pvr_bound(i_pvr), field_pvr(i_pvr), pixel_xy(i_pvr),       &
     &       pvr_start(i_pvr), pvr_img(i_pvr))
!
          if(view_params(i_pvr)%iflag_rotate_snap .gt. 0) then
            call deallocate_pvr_ray_start(pvr_start(i_pvr))
          end if
        end do
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
      allocate(file_params(num_pvr))
      allocate(fld_params(num_pvr))
      allocate(view_params(num_pvr))
      allocate(color_params(num_pvr))
      allocate(cbar_params(num_pvr))
      do i_pvr = 1, num_pvr
        call reset_pvr_view_parameteres(view_params(i_pvr))
      end do
!
      allocate(pvr_bound(num_pvr))
      allocate(pixel_xy(num_pvr))
      allocate(outlines(num_pvr))
      allocate(field_pvr(num_pvr))
      allocate(pvr_start(num_pvr))
      allocate(pvr_img(num_pvr))
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
        call dealloc_pvr_element_group(fld_params(i_pvr))
        call dealloc_pvr_color_parameteres(color_params(i_pvr))
      end do
      deallocate(file_params, fld_params, view_params)
      deallocate(color_params, cbar_params)
!
      end subroutine deallocate_pvr_data
!
!  ---------------------------------------------------------------------
!
      end module volume_rendering
