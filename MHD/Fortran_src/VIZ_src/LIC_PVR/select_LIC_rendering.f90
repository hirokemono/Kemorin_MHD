!>@file   select_LIC_rendering.f90
!!@brief  module select_LIC_rendering
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      subroutine LIC_initialize_w_shared_mesh(geofem, next_tbl,       &
!!     &          repart_p, repart_data, pvr)
!!      subroutine LIC_anaglyph_init_shared_mesh(geofem, next_tbl,      &
!!     &          repart_p, repart_data, pvr)
!!        type(mesh_data), intent(in) :: geofem
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!        type(volume_rendering_module), intent(inout) :: pvr
!!      subroutine LIC_visualize_w_shared_mesh                          &
!!     &         (istep_lic, time, geofem, nod_fld,                     &
!!     &          repart_p, repart_data, pvr, lic_param, v_sol)
!!      subroutine LIC_anaglyph_w_shared_mesh                           &
!!     &         (istep_lic, time, geofem, nod_fld,                     &
!!     &          repart_p, repart_data, pvr, lic_param, v_sol)
!!        integer(kind = kint), intent(in) :: istep_lic
!!        real(kind = kreal), intent(in) :: time
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!        type(volume_rendering_module), intent(inout) :: pvr
!!        type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!
!!      subroutine LIC_visualize_w_each_repart                          &
!!     &         (istep_lic, time, geofem, next_tbl, nod_fld,           &
!!     &          repart_p, repart_data, pvr, lic_param, v_sol)
!!      subroutine LIC_anaglyph_w_each_repart                           &
!!     &         (istep_lic, time, geofem, next_tbl, nod_fld,           &
!!     &          repart_p, repart_data, pvr, lic_param, v_sol)
!!        integer(kind = kint), intent(in) :: istep_lic
!!        real(kind = kreal), intent(in) :: time
!!        type(mesh_data), intent(in) :: geofem
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(phys_data), intent(in) :: nod_fld
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!        type(volume_rendering_module), intent(inout) :: pvr
!!        type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
!!        type(vectors_4_solver), intent(inout) :: v_sol
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
      use t_vector_for_solver
      use t_LIC_re_partition
      use t_control_param_LIC
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
      subroutine LIC_initialize_w_shared_mesh(geofem, next_tbl,         &
     &          repart_p, repart_data, pvr)
!
      use each_LIC_rendering
!
      type(mesh_data), intent(in) :: geofem
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(volume_partioning_param), intent(in) :: repart_p
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(volume_rendering_module), intent(inout) :: pvr
!
      integer(kind = kint) :: i_lic, ist_img, num_img
!
!
      call LIC_init_shared_mesh(geofem, next_tbl, repart_p,             &
     &                          repart_data)
!
      do i_lic = 1, pvr%num_pvr
        ist_img = pvr%istack_pvr_images(i_lic-1)
        num_img = pvr%istack_pvr_images(i_lic) - ist_img
        call each_PVR_initialize(i_lic, num_img,                        &
     &      repart_data%viz_fem%mesh, repart_data%viz_fem%group,        &
     &      pvr%pvr_rgb(ist_img+1), pvr%pvr_param(i_lic),               &
     &      pvr%pvr_proj(ist_img+1))
      end do
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
!
      end subroutine LIC_initialize_w_shared_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_anaglyph_init_shared_mesh(geofem, next_tbl,        &
     &          repart_p, repart_data, pvr)
!
      use each_LIC_rendering
!
      type(mesh_data), intent(in) :: geofem
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(volume_partioning_param), intent(in) :: repart_p
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(volume_rendering_module), intent(inout) :: pvr
!
      integer(kind = kint) :: i_lic
!
!
      call LIC_init_shared_mesh(geofem, next_tbl, repart_p,             &
     &                          repart_data)
!
      do i_lic = 1, pvr%num_pvr
        call each_anaglyph_PVR_init(i_lic,                              &
     &      repart_data%viz_fem%mesh, repart_data%viz_fem%group,        &
     &      pvr%pvr_rgb(i_lic), pvr%pvr_param(i_lic),                   &
     &      pvr%pvr_proj(2*i_lic-1))
      end do
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
!
      end subroutine LIC_anaglyph_init_shared_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine LIC_visualize_w_shared_mesh                            &
     &         (istep_lic, time, geofem, nod_fld,                       &
     &          repart_p, repart_data, pvr, lic_param, v_sol)
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
      type(volume_partioning_param), intent(in) :: repart_p
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(volume_rendering_module), intent(inout) :: pvr
      type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer(kind = kint) :: i_lic
      integer(kind = kint) :: i_img, ist_img, ied_img, num_img
!
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      do i_lic = 1, pvr%num_pvr
        if(iflag_debug .gt. 0) write(*,*) 'set_LIC_each_field'
        call set_LIC_each_field(geofem, nod_fld,                        &
     &      repart_p, lic_param(i_lic), repart_data, v_sol)
!
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                  .ne. IFLAG_NO_MOVIE) cycle
        ist_img = pvr%istack_pvr_images(i_lic-1)
        num_img = pvr%istack_pvr_images(i_lic  ) - ist_img
        if(my_rank .eq. 0) write(*,*) 's_each_LIC_rendering', i_lic
        call s_each_LIC_rendering(istep_lic, time, num_img,             &
     &      repart_data%viz_fem, repart_data%field_lic,                 &
     &      lic_param(i_lic), pvr%pvr_param(i_lic),                     &
     &      pvr%pvr_proj(ist_img+1), pvr%pvr_rgb(ist_img+1))
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
     &        pvr%pvr_param(i_lic)%stereo_def%n_row_column_view,        &
     &        pvr%pvr_param(i_lic)%view, pvr%pvr_rgb(ist_img+1))
        end if
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      do i_lic = 1, pvr%num_pvr
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                    .eq. IFLAG_NO_MOVIE) cycle
!
        if(iflag_debug .gt. 0) write(*,*) 'set_LIC_each_field'
        call set_LIC_each_field(geofem, nod_fld,                        &
     &      repart_p, lic_param(i_lic), repart_data, v_sol)
!
        ist_img = pvr%istack_pvr_images(i_lic-1)
        num_img = pvr%istack_pvr_images(i_lic) - ist_img
        write(*,*) 's_each_LIC_rendering_w_rot once', i_lic
        call s_each_LIC_rendering_w_rot(istep_lic, time, num_img,       &
     &      repart_data%viz_fem, repart_data%field_lic,                 &
     &      lic_param(i_lic), pvr%pvr_param(i_lic),                     &
     &      pvr%pvr_proj(ist_img+1), pvr%pvr_rgb(ist_img+1))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      end subroutine LIC_visualize_w_shared_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_anaglyph_w_shared_mesh                             &
     &         (istep_lic, time, geofem, nod_fld,                       &
     &          repart_p, repart_data, pvr, lic_param, v_sol)
!
      use m_elapsed_labels_4_VIZ
      use cal_pvr_modelview_mat
      use each_LIC_rendering
      use rendering_streo_LIC_image
      use write_PVR_image
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
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer(kind = kint) :: i_lic
!
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      do i_lic = 1, pvr%num_pvr
        if(iflag_debug .gt. 0) write(*,*) 'set_LIC_each_field'
        call set_LIC_each_field(geofem, nod_fld,                        &
     &      repart_p, lic_param(i_lic), repart_data, v_sol)
!
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                  .ne. IFLAG_NO_MOVIE) cycle
!
        if(my_rank .eq. 0) write(*,*) 's_each_LIC_anaglyph at once'
        call s_each_LIC_anaglyph(istep_lic, time, repart_data%viz_fem,  &
     &      repart_data%field_lic, lic_param(i_lic),                    &
     &      pvr%pvr_param(i_lic), pvr%pvr_proj(2*i_lic-1),              &
     &      pvr%pvr_rgb(i_lic))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+2)
      do i_lic = 1, pvr%num_pvr
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                  .ne. IFLAG_NO_MOVIE) cycle
!
        call sel_write_pvr_image_file(istep_lic, pvr%pvr_rgb(i_lic))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      do i_lic = 1, pvr%num_pvr
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                    .eq. IFLAG_NO_MOVIE) cycle
!
        if(iflag_debug .gt. 0) write(*,*) 'set_LIC_each_field'
        call set_LIC_each_field(geofem, nod_fld,                        &
     &      repart_p, lic_param(i_lic), repart_data, v_sol)
!
        write(*,*) 'anaglyph_lic_rendering_w_rot once', i_lic
        call anaglyph_lic_rendering_w_rot(istep_lic, time,              &
     &      repart_data%viz_fem, repart_data%field_lic,                 &
     &      lic_param(i_lic), pvr%pvr_rgb(i_lic),                       &
     &      pvr%pvr_param(i_lic), pvr%pvr_proj(2*i_lic-1))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      end subroutine LIC_anaglyph_w_shared_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine LIC_visualize_w_each_repart                            &
     &         (istep_lic, time, geofem, next_tbl, nod_fld,             &
     &          repart_p, repart_data, pvr, lic_param, v_sol)
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
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(phys_data), intent(in) :: nod_fld
      type(volume_partioning_param), intent(in) :: repart_p
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(volume_rendering_module), intent(inout) :: pvr
      type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer(kind = kint) :: i_lic
      integer(kind = kint) :: i_img, ist_img, ied_img, num_img
!
!
      do i_lic = 1, pvr%num_pvr
        ist_img = pvr%istack_pvr_images(i_lic-1)
        num_img = pvr%istack_pvr_images(i_lic) - ist_img
        if(my_rank .eq. 0) write(*,*) 'LIC_init_each_mesh'
        call LIC_init_each_mesh(geofem, next_tbl, repart_p,             &
     &                          lic_param(i_lic), repart_data)
!
        if(my_rank .eq. 0) write(*,*) 'each_PVR_initialize'
        call each_PVR_initialize(i_lic, num_img,                        &
     &      repart_data%viz_fem%mesh, repart_data%viz_fem%group,        &
     &      pvr%pvr_rgb(ist_img+1), pvr%pvr_param(i_lic),               &
     &      pvr%pvr_proj(ist_img+1))
!
!
        if(iflag_debug .gt. 0) write(*,*) 'set_LIC_each_field'
        call set_LIC_each_field(geofem, nod_fld,                        &
     &      repart_p, lic_param(i_lic), repart_data, v_sol)
!
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                  .eq. IFLAG_NO_MOVIE) then
          if(my_rank .eq. 0) write(*,*)                                 &
     &                     's_each_LIC_rendering each', i_lic
          if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
          call s_each_LIC_rendering                                     &
     &       (istep_lic, time, num_img, repart_data%viz_fem,            &
     &        repart_data%field_lic, lic_param(i_lic),                  &
     &        pvr%pvr_param(i_lic), pvr%pvr_proj(ist_img+1),            &
     &        pvr%pvr_rgb(ist_img+1))
          if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
        else
          call s_each_LIC_rendering_w_rot                               &
     &     (istep_lic, time, num_img, repart_data%viz_fem,              &
     &      repart_data%field_lic, lic_param(i_lic),                    &
     &      pvr%pvr_param(i_lic), pvr%pvr_proj(ist_img+1),              &
     &      pvr%pvr_rgb(ist_img+1))
        end if
!
        call dealloc_PVR_initialize(num_img, pvr%pvr_param(i_lic),      &
     &                                       pvr%pvr_proj(ist_img+1))
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
     &        pvr%pvr_param(i_lic)%stereo_def%n_row_column_view,        &
     &        pvr%pvr_param(i_lic)%view, pvr%pvr_rgb(ist_img+1))
        end if
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
!
      end subroutine LIC_visualize_w_each_repart
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_anaglyph_w_each_repart                             &
     &         (istep_lic, time, geofem, next_tbl, nod_fld,             &
     &          repart_p, repart_data, pvr, lic_param, v_sol)
!
      use m_elapsed_labels_4_VIZ
      use cal_pvr_modelview_mat
      use each_LIC_rendering
      use rendering_streo_LIC_image
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_lic
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: geofem
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(phys_data), intent(in) :: nod_fld
      type(volume_partioning_param), intent(in) :: repart_p
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(volume_rendering_module), intent(inout) :: pvr
      type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer(kind = kint) :: i_lic
!
!
      do i_lic = 1, pvr%num_pvr
        if(my_rank .eq. 0) write(*,*) 'LIC_init_each_mesh'
        call LIC_init_each_mesh(geofem, next_tbl, repart_p,             &
     &                          lic_param(i_lic), repart_data)
!
        if(my_rank .eq. 0) write(*,*) 'each_anaglyph_PVR_init'
        call each_anaglyph_PVR_init(i_lic,                              &
     &      repart_data%viz_fem%mesh, repart_data%viz_fem%group,        &
     &      pvr%pvr_rgb(i_lic), pvr%pvr_param(i_lic),                   &
     &      pvr%pvr_proj(2*i_lic-1))
!
!
        if(iflag_debug .gt. 0) write(*,*) 'set_LIC_each_field'
        call set_LIC_each_field(geofem, nod_fld,                        &
     &      repart_p, lic_param(i_lic), repart_data, v_sol)
!
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                  .eq. IFLAG_NO_MOVIE) then
          if(my_rank .eq. 0) write(*,*)                                 &
     &                     's_each_LIC_anaglyph each', i_lic
          if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
          call s_each_LIC_anaglyph                                      &
     &       (istep_lic, time, repart_data%viz_fem,                     &
     &        repart_data%field_lic, lic_param(i_lic),                  &
     &        pvr%pvr_param(i_lic), pvr%pvr_proj(2*i_lic-1),            &
     &        pvr%pvr_rgb(i_lic))
          if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
        else
          call anaglyph_lic_rendering_w_rot(istep_lic, time,            &
     &        repart_data%viz_fem, repart_data%field_lic,               &
     &        lic_param(i_lic), pvr%pvr_rgb(i_lic),                     &
     &        pvr%pvr_param(i_lic), pvr%pvr_proj(2*i_lic-1))
        end if
!
        call dealloc_PVR_initialize(itwo, pvr%pvr_param(i_lic),         &
     &                              pvr%pvr_proj(2*i_lic-1))
        call dealloc_LIC_each_mesh                                      &
     &     (repart_p, lic_param(i_lic)%each_part_p, repart_data)
      end do
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+2)
      do i_lic = 1, pvr%num_pvr
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                  .ne. IFLAG_NO_MOVIE) cycle
!
        call sel_write_pvr_image_file(istep_lic, pvr%pvr_rgb(i_lic))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
!
      end subroutine LIC_anaglyph_w_each_repart
!
!  ---------------------------------------------------------------------
!
      end module select_LIC_rendering
