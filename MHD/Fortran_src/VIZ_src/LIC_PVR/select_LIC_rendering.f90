!>@file   select_LIC_rendering.f90
!!@brief  module select_LIC_rendering
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      subroutine LIC_initialize_w_shared_mesh(geofem, ele_comm,       &
!!     &          next_tbl, repart_p, repart_data, pvr, m_SR)
!!        type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: ele_comm
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!        type(volume_rendering_module), intent(inout) :: pvr
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine c(istep_lic, time, geofem, &
!!     &          nod_fld, repart_p, repart_data, pvr, lic_param, m_SR)
!!        integer(kind = kint), intent(in) :: istep_lic
!!        real(kind = kreal), intent(in) :: time
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!        type(volume_rendering_module), intent(inout) :: pvr
!!        type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine LIC_visualize_w_each_repart                          &
!!     &         (istep_lic, time, geofem, ele_comm, next_tbl, nod_fld, &
!!     &          repart_p, repart_data, pvr, lic_param, m_SR)
!!        integer(kind = kint), intent(in) :: istep_lic
!!        real(kind = kreal), intent(in) :: time
!!        type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: ele_comm
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(phys_data), intent(in) :: nod_fld
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!        type(volume_rendering_module), intent(inout) :: pvr
!!        type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module select_LIC_rendering
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
      use t_comm_table
      use t_phys_data
      use t_next_node_ele_4_node
!
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_geometries_in_pvr_screen
      use t_control_data_LIC_pvrs
      use t_ctl_data_volume_repart
      use t_volume_rendering
      use t_calypso_comm_table
      use t_control_param_vol_grping
      use t_LIC_re_partition
      use t_control_param_LIC
      use t_mesh_SR
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_initialize_w_shared_mesh(geofem, ele_comm,         &
     &          next_tbl, repart_p, repart_data, pvr, m_SR)
!
      use each_LIC_rendering
      use each_volume_rendering
!
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: ele_comm
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(volume_partioning_param), intent(in) :: repart_p
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(volume_rendering_module), intent(inout) :: pvr
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_lic, ist_img, num_img
!
!
      call LIC_init_shared_mesh(geofem, ele_comm, next_tbl,             &
     &                          repart_p, repart_data, m_SR)
      call init_sf_grp_list_each_surf                                   &
     &   (repart_data%viz_fem%mesh%surf,                                &
     &    repart_data%viz_fem%group%surf_grp, pvr%sf_grp_4_sf)
!
      do i_lic = 1, pvr%num_pvr
        ist_img = pvr%istack_pvr_images(i_lic-1)
        num_img = pvr%istack_pvr_images(i_lic) - ist_img
        call each_PVR_initialize(i_lic, num_img,                        &
     &      repart_data%viz_fem%mesh, repart_data%viz_fem%group,        &
     &      pvr%pvr_rgb(ist_img+1), pvr%pvr_param(i_lic),               &
     &      pvr%pvr_bound(i_lic), pvr%pvr_proj(ist_img+1),              &
     &      m_SR%SR_sig, m_SR%SR_r, m_SR%SR_i)
      end do
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
!
      end subroutine LIC_initialize_w_shared_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine LIC_visualize_w_shared_mesh(istep_lic, time, geofem,   &
     &          nod_fld, repart_p, repart_data, pvr, lic_param, m_SR)
!
      use m_elapsed_labels_4_VIZ
      use cal_pvr_modelview_mat
      use each_LIC_rendering
      use write_PVR_image
      use each_volume_rendering
!
      integer(kind = kint), intent(in) :: istep_lic
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(volume_partioning_param), intent(in) :: repart_p
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(volume_rendering_module), intent(inout) :: pvr
      type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_lic
      integer(kind = kint) :: i_img, ist_img, ied_img, num_img
!
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      do i_lic = 1, pvr%num_pvr
        if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
        call cal_field_4_each_lic(geofem%mesh%node, nod_fld,            &
     &      lic_param(i_lic), repart_data%nod_fld_lic)
        if(iflag_debug .gt. 0) write(*,*) 'set_LIC_each_field'
        call set_LIC_each_field(geofem, nod_fld, repart_p,              &
     &                          lic_param(i_lic), repart_data, m_SR)
!
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                  .ne. IFLAG_NO_MOVIE) cycle
        ist_img = pvr%istack_pvr_images(i_lic-1)
        num_img = pvr%istack_pvr_images(i_lic  ) - ist_img
        if(my_rank .eq. 0) write(*,*) 's_each_LIC_rendering', i_lic
        call s_each_LIC_rendering                                       &
     &     (istep_lic, time, num_img, repart_data%viz_fem,              &
     &      repart_data%field_lic, pvr%sf_grp_4_sf, lic_param(i_lic),   &
     &      pvr%pvr_param(i_lic), pvr%pvr_proj(ist_img+1),              &
     &      pvr%pvr_rgb(ist_img+1), m_SR%SR_sig, m_SR%SR_r,             &
     &      lic_param(i_lic)%elapse_ray_trace)
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+2)
      do i_lic = 1, pvr%num_pvr
        ist_img = pvr%istack_pvr_images(i_lic-1) + 1
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                  .ne. IFLAG_NO_MOVIE) cycle
        if(pvr%pvr_param(i_lic)%stereo_def%flag_quilt) cycle
!
        ied_img = pvr%istack_pvr_images(i_lic  )
        do i_img = ist_img, ied_img
          call sel_write_pvr_image_file(istep_lic, pvr%pvr_rgb(i_img))
        end do
      end do
      do i_lic = 1, pvr%num_pvr
        ist_img = pvr%istack_pvr_images(i_lic-1)
        num_img = pvr%istack_pvr_images(i_lic  ) - ist_img
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                  .ne. IFLAG_NO_MOVIE) cycle
        if(pvr%pvr_param(i_lic)%stereo_def%flag_quilt) then
          call set_output_rot_sequence_image(istep_lic,                 &
     &        pvr%pvr_rgb(ist_img+1)%id_pvr_file_type,                  &
     &        pvr%pvr_rgb(ist_img+1)%pvr_prefix, num_img,               &
     &        pvr%pvr_param(i_lic)%stereo_def%n_column_row_view,        &
     &        pvr%pvr_rgb(ist_img+1))
        end if
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      do i_lic = 1, pvr%num_pvr
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                    .eq. IFLAG_NO_MOVIE) cycle
!
        if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
        call cal_field_4_each_lic(geofem%mesh%node, nod_fld,            &
     &      lic_param(i_lic), repart_data%nod_fld_lic)
        if(iflag_debug .gt. 0) write(*,*) 'set_LIC_each_field'
        call set_LIC_each_field(geofem, nod_fld, repart_p,              &
     &                          lic_param(i_lic), repart_data, m_SR)
!
        ist_img = pvr%istack_pvr_images(i_lic-1)
        num_img = pvr%istack_pvr_images(i_lic) - ist_img
        write(*,*) 's_each_LIC_rendering_w_rot once', i_lic
        call s_each_LIC_rendering_w_rot                                 &
     &     (istep_lic, time, num_img, repart_data%viz_fem,              &
     &      repart_data%field_lic, pvr%sf_grp_4_sf, lic_param(i_lic),   &
     &      pvr%pvr_param(i_lic), pvr%pvr_bound(i_lic),                 &
     &      pvr%pvr_proj(ist_img+1), pvr%pvr_rgb(ist_img+1),            &
     &      m_SR%SR_sig, m_SR%SR_r, m_SR%SR_i,                          &
     &      lic_param(i_lic)%elapse_ray_trace)
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      end subroutine LIC_visualize_w_shared_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine LIC_visualize_w_each_repart                            &
     &         (istep_lic, time, geofem, ele_comm, next_tbl, nod_fld,   &
     &          repart_p, repart_data, pvr, lic_param, m_SR)
!
      use m_elapsed_labels_4_VIZ
      use t_surf_grp_list_each_surf
      use t_lic_field_data
      use cal_pvr_modelview_mat
      use each_LIC_rendering
      use write_PVR_image
      use each_volume_rendering
!
      integer(kind = kint), intent(in) :: istep_lic
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: ele_comm
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(phys_data), intent(in) :: nod_fld
      type(volume_partioning_param), intent(in) :: repart_p
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(volume_rendering_module), intent(inout) :: pvr
      type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_lic
      integer(kind = kint) :: i_img, ist_img, ied_img, num_img
!
!
      do i_lic = 1, pvr%num_pvr
        ist_img = pvr%istack_pvr_images(i_lic-1)
        num_img = pvr%istack_pvr_images(i_lic) - ist_img
        if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
        call cal_field_4_each_lic(geofem%mesh%node, nod_fld,            &
     &      lic_param(i_lic), repart_data%nod_fld_lic)
        if(my_rank .eq. 0) write(*,*) 'LIC_init_each_mesh'
        call LIC_init_each_mesh(geofem, ele_comm, next_tbl, repart_p,   &
     &                          lic_param(i_lic), repart_data, m_SR)
        if(iflag_debug .gt. 0) write(*,*) 'init_sf_grp_list_each_surf'
        call init_sf_grp_list_each_surf                                 &
     &     (repart_data%viz_fem%mesh%surf,                              &
     &      repart_data%viz_fem%group%surf_grp, pvr%sf_grp_4_sf)
        if(iflag_debug .gt. 0) write(*,*) 'set_LIC_each_field'
        call set_LIC_each_field(geofem, nod_fld, repart_p,              &
     &                          lic_param(i_lic), repart_data, m_SR)
!
        if(my_rank .eq. 0) write(*,*) 'each_PVR_initialize'
        call each_PVR_initialize(i_lic, num_img,                        &
     &      repart_data%viz_fem%mesh, repart_data%viz_fem%group,        &
     &      pvr%pvr_rgb(ist_img+1), pvr%pvr_param(i_lic),               &
     &      pvr%pvr_bound(i_lic), pvr%pvr_proj(ist_img+1),              &
     &      m_SR%SR_sig, m_SR%SR_r, m_SR%SR_i)
!
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                  .eq. IFLAG_NO_MOVIE) then
          if(my_rank .eq. 0) write(*,*)                                 &
     &                     's_each_LIC_rendering each', i_lic
          if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
          call s_each_LIC_rendering                                     &
     &       (istep_lic, time, num_img, repart_data%viz_fem,            &
     &        repart_data%field_lic, pvr%sf_grp_4_sf, lic_param(i_lic), &
     &        pvr%pvr_param(i_lic), pvr%pvr_proj(ist_img+1),            &
     &        pvr%pvr_rgb(ist_img+1), m_SR%SR_sig, m_SR%SR_r,           &
     &        lic_param(i_lic)%elapse_ray_trace)
          call dealloc_PVR_initialize(num_img, pvr%pvr_param(i_lic),    &
     &        pvr%pvr_bound(i_lic), pvr%pvr_proj(ist_img+1))
          if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
        else
          call s_each_LIC_rendering_w_rot                               &
     &       (istep_lic, time, num_img, repart_data%viz_fem,            &
     &        repart_data%field_lic, pvr%sf_grp_4_sf, lic_param(i_lic), &
     &        pvr%pvr_param(i_lic), pvr%pvr_bound(i_lic),               &
     &        pvr%pvr_proj(ist_img+1), pvr%pvr_rgb(ist_img+1),          &
     &        m_SR%SR_sig, m_SR%SR_r, m_SR%SR_i,                        &
     &        lic_param(i_lic)%elapse_ray_trace)
         call dealloc_pvr_surf_domain_item(pvr%pvr_bound(i_lic))
         call dealloc_pixel_position_pvr(pvr%pvr_param(i_lic)%pixel)
         call dealloc_iflag_pvr_used_ele                                &
     &      (pvr%pvr_param(i_lic)%draw_param)
        end if
!
        if(lic_param(i_lic)%each_part_p%iflag_repart_ref                &
     &                                   .eq. i_TIME_BASED) then
          call bring_back_rendering_time                                &
     &       (lic_param(i_lic)%each_part_p%weight_prev,                 &
     &        lic_param(i_lic)%elapse_ray_trace,                        &
     &        repart_data%mesh_to_viz_tbl, repart_data%viz_fem%mesh,    &
     &        geofem%mesh, repart_data%field_lic,                       &
     &        repart_data%nod_fld_lic, m_SR)
        end if

!
        call dealloc_num_sf_grp_each_surf(pvr%sf_grp_4_sf)
        call dealloc_LIC_each_mesh                                      &
     &     (repart_p, lic_param(i_lic)%each_part_p, repart_data)
      end do
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+2)
      do i_lic = 1, pvr%num_pvr
        ist_img = pvr%istack_pvr_images(i_lic-1) + 1
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                  .ne. IFLAG_NO_MOVIE) cycle
        if(pvr%pvr_param(i_lic)%stereo_def%flag_quilt) cycle
!
        ied_img = pvr%istack_pvr_images(i_lic  )
        do i_img = ist_img, ied_img
          call sel_write_pvr_image_file(istep_lic, pvr%pvr_rgb(i_img))
        end do
      end do
!
      do i_lic = 1, pvr%num_pvr
        ist_img = pvr%istack_pvr_images(i_lic-1) + 1
        num_img = pvr%istack_pvr_images(i_lic  ) - ist_img
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                  .ne. IFLAG_NO_MOVIE) cycle
        if(pvr%pvr_param(i_lic)%stereo_def%flag_quilt) then
          call set_output_rot_sequence_image(istep_lic,                 &
     &        pvr%pvr_rgb(ist_img+1)%id_pvr_file_type,                  &
     &        pvr%pvr_rgb(ist_img+1)%pvr_prefix, num_img,               &
     &        pvr%pvr_param(i_lic)%stereo_def%n_column_row_view,        &
     &        pvr%pvr_rgb(ist_img+1))
        end if
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
!
      end subroutine LIC_visualize_w_each_repart
!
!  ---------------------------------------------------------------------
!
      end module select_LIC_rendering
