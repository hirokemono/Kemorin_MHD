!>@file   lic_rendering_test.f90
!!@brief  module lic_rendering_test
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      subroutine LIC_initialize_test(increment_lic, geofem, next_tbl, &
!!     &                          nod_fld, lic_ctls, repart_ctl, lic)
!!      subroutine LIC_visualize_test                                   &
!!     &         (istep_lic, time, geofem, nod_fld, lic, v_sol)
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(lic_rendering_controls), intent(inout) :: lic_ctls
!!        type(viz_repartition_ctl), intent(inout) :: repart_ctl
!!        type(lic_volume_rendering_module), intent(inout) :: lic
!!@endverbatim
!
      module lic_rendering_test
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_work_time
!
      use t_mesh_data
      use t_phys_data
!
      use t_lic_rendering
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_control_param_LIC_PVR
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_geometries_in_pvr_screen
      use t_control_data_LIC_pvrs
      use t_ctl_data_volume_repart
      use t_volume_rendering
      use t_calypso_comm_table
      use t_control_param_vol_grping
      use t_vector_for_solver
!
      use each_volume_rendering
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_initialize_test(increment_lic, geofem, next_tbl,   &
     &                          nod_fld, lic_ctls, repart_ctl, lic)
!
      use t_control_data_pvr_sections
      use set_pvr_control
      use each_LIC_rendering
      use rendering_and_image_nums
      use LIC_re_partition
!
      integer(kind = kint), intent(in) :: increment_lic
      type(mesh_data), intent(in), target :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(lic_rendering_controls), intent(inout) :: lic_ctls
      type(viz_repartition_ctl), intent(inout) :: repart_ctl
      type(lic_volume_rendering_module), intent(inout) :: lic
!
      integer(kind = kint) :: i_lic, ist_rdr, ist_img
!
!
      lic%pvr%num_pvr = lic_ctls%num_lic_ctl
      if(increment_lic .le. 0) lic%pvr%num_pvr = 0
!
      if(lic%pvr%num_pvr .le. 0) then
        lic%pvr%num_pvr = 0
        return
      end if
!
      call bcast_lic_controls(lic%pvr%num_pvr,                          &
     &    lic_ctls%pvr_ctl_type, lic_ctls%lic_ctl_type,                 &
     &    lic%pvr%cflag_update)
!
      call set_ctl_param_vol_repart(repart_ctl, lic%repart_p)
      call dealloc_control_vol_repart(repart_ctl)
!
      call count_num_rendering_and_images                               &
     &   (lic%pvr%num_pvr, lic_ctls%pvr_ctl_type,                       &
     &    lic%pvr%num_pvr_rendering, lic%pvr%num_pvr_images)
      call alloc_LIC_data(lic)
!
      call s_num_rendering_and_images                                   &
     &   (nprocs, lic%pvr%num_pvr, lic_ctls%pvr_ctl_type,               &
     &    lic%pvr%num_pvr_rendering, lic%pvr%num_pvr_images,            &
     &    lic%pvr%istack_pvr_render, lic%pvr%istack_pvr_images,         &
     &    lic%pvr%pvr_rgb)
!
      do i_lic = 1, lic%pvr%num_pvr
        call alloc_iflag_pvr_boundaries(geofem%group%surf_grp,          &
     &      lic%pvr%pvr_param(i_lic)%draw_param)
        call reset_pvr_view_parameteres(lic%pvr%pvr_param(i_lic)%view)
      end do
!
      call s_set_lic_controls(geofem%group, nod_fld, lic%pvr%num_pvr,   &
     &    lic_ctls%pvr_ctl_type, lic_ctls%lic_ctl_type,                 &
     &    lic%lic_fld_pm, lic%pvr%pvr_param, lic%flag_each_repart)
!
      do i_lic = 1, lic%pvr%num_pvr
        if(lic_ctls%fname_lic_ctl(i_lic) .ne. 'NO_FILE'                 &
     &      .or. my_rank .ne. 0) then
          call dealloc_lic_count_data(lic_ctls%pvr_ctl_type(i_lic),     &
     &        lic_ctls%lic_ctl_type(i_lic))
        end if
      end do
!
!
      if(lic%repart_p%flag_repartition) then
!  -----  Repartition
        allocate(lic%viz_fem)
        call s_LIC_re_partition(lic%repart_p, geofem,                   &
     &      next_tbl, lic%viz_fem, lic%mesh_to_viz_tbl)
       else
        lic%viz_fem => geofem
      end if
!
      do i_lic = 1, lic%pvr%num_pvr
        allocate(lic%lic_fld_pm(i_lic)%nod_fld_lic)
        call alloc_nod_vector_4_lic(geofem%mesh%node,                   &
     &      lic%lic_fld_pm(i_lic)%lic_param%num_masking,                &
     &      lic%lic_fld_pm(i_lic)%nod_fld_lic)
!
        if(lic%repart_p%flag_repartition) then
          allocate(lic%lic_fld_pm(i_lic)%field_lic)
          call alloc_nod_vector_4_lic(lic%viz_fem%mesh%node,            &
     &        lic%lic_fld_pm(i_lic)%lic_param%num_masking,              &
     &        lic%lic_fld_pm(i_lic)%field_lic)
        else
          lic%lic_fld_pm(i_lic)%field_lic                               &
     &           => lic%lic_fld_pm(i_lic)%nod_fld_lic
        end if
      end do
!
      do i_lic = 1, lic%pvr%num_pvr
        ist_rdr = lic%pvr%istack_pvr_render(i_lic-1) + 1
        ist_img = lic%pvr%istack_pvr_images(i_lic-1) + 1
        call each_PVR_initialize                                        &
     &    (i_lic, lic%viz_fem%mesh, lic%viz_fem%group,                  &
     &     lic%pvr%pvr_param(i_lic)%area_def, lic%pvr%pvr_param(i_lic), &
     &     lic%pvr%pvr_proj(ist_rdr), lic%pvr%pvr_rgb(ist_img))
      end do
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
!
      end subroutine LIC_initialize_test
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_visualize_test                                     &
     &         (istep_lic, time, geofem, nod_fld, lic, v_sol)
!
      use m_elapsed_labels_4_VIZ
      use cal_pvr_modelview_mat
      use each_LIC_rendering
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_lic
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
!
      type(lic_volume_rendering_module), intent(inout) :: lic
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer(kind = kint) :: i_lic, ist_rdr, ist_img
!
!
      if(lic%pvr%num_pvr.le.0 .or. istep_lic.le.0) return
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      do i_lic = 1, lic%pvr%num_pvr
        ist_rdr = lic%pvr%istack_pvr_render(i_lic-1) + 1
        ist_img = lic%pvr%istack_pvr_images(i_lic-1) + 1
        call s_each_LIC_rendering(istep_lic, time, lic%repart_p,        &
     &      lic%viz_fem, lic%mesh_to_viz_tbl, geofem%mesh, nod_fld,     &
     &      lic%lic_fld_pm(i_lic), lic%pvr%pvr_param(i_lic),            &
     &      lic%pvr%pvr_proj(ist_rdr), lic%pvr%pvr_rgb(ist_img), v_sol)
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+2)
      do i_lic = 1, lic%pvr%num_pvr
        ist_img = lic%pvr%istack_pvr_images(i_lic-1) + 1
        if(lic%pvr%pvr_rgb(ist_img)%iflag_monitoring .gt. 0) then
          call sel_write_pvr_image_file                                 &
     &       (iminus, iminus, lic%pvr%pvr_rgb(ist_img))
        end if
      end do
      do i_lic = 1, lic%pvr%num_pvr_images
        call sel_write_pvr_image_file                                   &
     &     (iminus, istep_lic, lic%pvr%pvr_rgb(i_lic))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      do i_lic = 1, lic%pvr%num_pvr
        if(lic%pvr%pvr_param(i_lic)%view%iflag_rotate_snap .gt. 0) then
          ist_rdr = lic%pvr%istack_pvr_render(i_lic-1) + 1
          ist_img = lic%pvr%istack_pvr_images(i_lic-1) + 1
          call s_each_LIC_rendering_w_rot                               &
     &       (istep_lic, time, lic%repart_p, lic%viz_fem,               &
     &        lic%mesh_to_viz_tbl, geofem%mesh, nod_fld,                &
     &        lic%lic_fld_pm(i_lic), lic%pvr%pvr_param(i_lic),          &
     &        lic%pvr%pvr_proj(ist_rdr), lic%pvr%pvr_rgb(ist_img),      &
     &        v_sol)
        end if
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      end subroutine LIC_visualize_test
!
!  ---------------------------------------------------------------------
!
      end module lic_rendering_test
