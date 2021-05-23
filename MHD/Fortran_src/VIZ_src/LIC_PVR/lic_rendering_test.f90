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
      use t_next_node_ele_4_node
!
      use t_lic_rendering
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
      subroutine LIC_initialize_test(increment_lic, geofem, next_tbl,   &
     &                          nod_fld, lic_ctls, repart_ctl, lic)
!
      use t_control_data_pvr_sections
      use set_pvr_control
      use each_LIC_rendering
      use rendering_and_image_nums
      use set_lic_controls
!
      integer(kind = kint), intent(in) :: increment_lic
      type(mesh_data), intent(in), target :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(lic_rendering_controls), intent(inout) :: lic_ctls
      type(viz_repartition_ctl), intent(inout) :: repart_ctl
      type(lic_volume_rendering_module), intent(inout) :: lic
!
      integer(kind = kint) :: i_lic, ist_img, ist_rdr
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
      call alloc_pvr_data(lic%pvr)
!
      do i_lic = 1, lic%pvr%num_pvr
        call alloc_iflag_pvr_boundaries(geofem%group%surf_grp,          &
     &      lic%pvr%pvr_param(i_lic)%draw_param)
        call reset_pvr_view_parameteres(lic%pvr%pvr_param(i_lic)%view)
      end do
!
      allocate(lic%lic_param(lic%pvr%num_pvr))
      call s_set_lic_controls(geofem%group, nod_fld, lic%pvr%num_pvr,   &
     &    lic_ctls%pvr_ctl_type, lic_ctls%lic_ctl_type,                 &
     &    lic%lic_param, lic%pvr%pvr_param, lic%flag_each_repart)
!
      call count_num_rendering_and_images                               &
     &   (lic%pvr%num_pvr, lic%pvr%pvr_param,                           &
     &    lic%pvr%num_pvr_rendering, lic%pvr%num_pvr_images)
      call alloc_pvr_images(lic%pvr)
!
      call s_num_rendering_and_images(nprocs,                           &
     &    lic%pvr%num_pvr, lic%pvr%pvr_param, lic_ctls%pvr_ctl_type,    &
     &    lic%pvr%num_pvr_rendering, lic%pvr%num_pvr_images,            &
     &    lic%pvr%istack_pvr_render, lic%pvr%istack_pvr_images,         &
     &    lic%pvr%pvr_rgb)
!
      do i_lic = 1, lic%pvr%num_pvr
        ist_img = lic%pvr%istack_pvr_images(i_lic-1) + 1
        call init_each_PVR_image(lic%pvr%pvr_param(i_lic),              &
     &                           lic%pvr%pvr_rgb(ist_img))
      end do
!
      do i_lic = 1, lic%pvr%num_pvr
        if(lic_ctls%fname_lic_ctl(i_lic) .ne. 'NO_FILE'                 &
     &      .or. my_rank .ne. 0) then
          call dealloc_lic_count_data(lic_ctls%pvr_ctl_type(i_lic),     &
     &        lic_ctls%lic_ctl_type(i_lic))
        end if
      end do
!
      if(lic%flag_each_repart) return
      call LIC_init_shared_mesh(lic%pvr%num_pvr, lic%repart_p,          &
     &    geofem, next_tbl, nod_fld, lic%repart_data, lic%lic_param)
!
      do i_lic = 1, lic%pvr%num_pvr
        ist_rdr = lic%pvr%istack_pvr_render(i_lic-1) + 1
        ist_img = lic%pvr%istack_pvr_images(i_lic-1) + 1
        call each_PVR_initialize(i_lic,                                 &
     &     lic%repart_data%viz_fem%mesh, lic%repart_data%viz_fem%group, &
     &     lic%pvr%pvr_rgb(ist_img), lic%pvr%pvr_param(i_lic),          &
     &     lic%pvr%pvr_proj(ist_rdr))
      end do
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
!
      end subroutine LIC_initialize_test
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_visualize_test(istep_lic, time, geofem, next_tbl,  &
     &                              nod_fld, lic, v_sol)
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
!
      type(lic_volume_rendering_module), intent(inout) :: lic
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer(kind = kint) :: i_lic, ist_rdr, ist_img
!
!
      if(lic%pvr%num_pvr.le.0 .or. istep_lic.le.0) return
!
      if(lic%flag_each_repart) then
        call LIC_visualize_w_each_repart                                &
     &     (istep_lic, time, geofem, next_tbl, nod_fld, lic%repart_p,   &
     &      lic%repart_data, lic%pvr, lic%lic_param, v_sol)
      else
        call LIC_visualize_w_shared_mesh                                &
     &     (istep_lic, time, geofem, next_tbl, nod_fld, lic%repart_p,   &
     &      lic%repart_data, lic%pvr, lic%lic_param, v_sol)
      end if
!
      end subroutine LIC_visualize_test
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_init_each_mesh(i_lic, geofem, next_tbl, nod_fld,   &
     &                              repart_p, repart_data, lic_param)
!
      use set_pvr_control
      use each_LIC_rendering
      use rendering_and_image_nums
!
      integer(kind = kint), intent(in) :: i_lic
      type(mesh_data), intent(in), target :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(volume_partioning_param), intent(in) :: repart_p
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(lic_parameters), intent(inout) :: lic_param
!
!
      allocate(repart_data%nod_fld_lic)
      call alloc_nod_vector_4_lic(geofem%mesh%node,                     &
     &    lic_param%num_masking, repart_data%nod_fld_lic)
!
!  -----  Repartition
      if(lic_param%each_part_p%flag_repartition) then
        call s_LIC_re_partition                                         &
     &     (lic_param%each_part_p, geofem, next_tbl, repart_data)
!
        allocate(repart_data%field_lic)
        call alloc_nod_vector_4_lic(repart_data%viz_fem%mesh%node,      &
     &      lic_param%num_masking, repart_data%field_lic)
      else if(repart_p%flag_repartition) then
        call s_LIC_re_partition(repart_p, geofem, next_tbl,             &
     &                          repart_data)
!
        allocate(repart_data%field_lic)
        call alloc_nod_vector_4_lic(repart_data%viz_fem%mesh%node,      &
     &      lic_param%num_masking, repart_data%field_lic)
      else
        repart_data%viz_fem =>  geofem
        repart_data%field_lic => repart_data%nod_fld_lic
      end if
!
      end subroutine LIC_init_each_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_init_shared_mesh(num_lic, repart_p,                &
     &          geofem, next_tbl, nod_fld, repart_data, lic_param)
!
      use set_pvr_control
      use rendering_and_image_nums
!
      integer(kind = kint), intent(in) :: num_lic
      type(mesh_data), intent(in), target :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(volume_partioning_param), intent(in) :: repart_p
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(lic_parameters), intent(inout) :: lic_param(num_lic)
!
      integer(kind = kint) :: i_lic, nmax_masking
!
!
      nmax_masking = 0
      do i_lic = 1, num_lic
        nmax_masking = max(nmax_masking, lic_param(i_lic)%num_masking)
      end do
!
      allocate(repart_data%nod_fld_lic)
      call alloc_nod_vector_4_lic(geofem%mesh%node, nmax_masking,       &
     &    repart_data%nod_fld_lic)
!
      if(repart_p%flag_repartition) then
!  -----  Repartition
        call s_LIC_re_partition                                         &
     &     (repart_p, geofem, next_tbl, repart_data)
!
        allocate(repart_data%field_lic)
          call alloc_nod_vector_4_lic                                   &
     &       (repart_data%viz_fem%mesh%node, nmax_masking,              &
     &        repart_data%field_lic)
      else
        repart_data%viz_fem => geofem
        repart_data%field_lic => repart_data%nod_fld_lic
      end if
!
      end subroutine LIC_init_shared_mesh
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
!
      subroutine dealloc_LIC_each_mesh                                  &
     &         (repart_p, repart_data, lic_param)
!
      type(volume_partioning_param), intent(in) :: repart_p
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(lic_parameters), intent(inout) :: lic_param
!
!
      if(lic_param%each_part_p%flag_repartition) then
        call dealloc_LIC_re_partition(repart_data)
        call dealloc_nod_data_4_lic(repart_data%field_lic)
        deallocate(repart_data%field_lic)
      else if(repart_p%flag_repartition) then
        call dealloc_LIC_re_partition(repart_data)
        call dealloc_nod_data_4_lic(repart_data%field_lic)
        deallocate(repart_data%field_lic)
      else
        nullify(repart_data%viz_fem)
        nullify(repart_data%field_lic)
      end if
!
      call dealloc_nod_data_4_lic(repart_data%nod_fld_lic)
      deallocate(repart_data%nod_fld_lic)
!
      end subroutine dealloc_LIC_each_mesh
!
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
      integer(kind = kint) :: i_lic, ist_rdr, ist_img
!
!
      do i_lic = 1, pvr%num_pvr
        ist_rdr = pvr%istack_pvr_render(i_lic-1) + 1
        ist_img = pvr%istack_pvr_images(i_lic-1) + 1
        if(my_rank .eq. 0) write(*,*) 'LIC_init_each_mesh'
        call LIC_init_each_mesh(i_lic, geofem, next_tbl, nod_fld,       &
     &      repart_p, repart_data, lic_param(i_lic))
!
        if(my_rank .eq. 0) write(*,*) 'each_PVR_initialize'
        call each_PVR_initialize(i_lic,                                 &
     &      repart_data%viz_fem%mesh, repart_data%viz_fem%group,        &
     &      pvr%pvr_rgb(ist_img), pvr%pvr_param(i_lic),                 &
     &      pvr%pvr_proj(ist_rdr))
!
!
        if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
        call cal_field_4_each_lic(geofem%mesh%node, nod_fld,            &
     &      lic_param(i_lic), repart_data%nod_fld_lic)
!
        if(lic_param(i_lic)%each_part_p%flag_repartition                &
     &                           .or. repart_p%flag_repartition) then
          call repartition_lic_field                                    &
     &       (geofem%mesh%node, repart_data%viz_fem%mesh,               &
     &        repart_data%mesh_to_viz_tbl, repart_data%nod_fld_lic,     &
     &        repart_data%field_lic, v_sol)
        end if
!
        if(my_rank .eq. 0) write(*,*) 's_each_LIC_rendering each', i_lic
        if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
        call s_each_LIC_rendering(istep_lic, time, repart_data%viz_fem, &
     &      repart_data%field_lic, lic_param(i_lic),                    &
     &      pvr%pvr_param(i_lic), pvr%pvr_proj(ist_rdr),                &
     &      pvr%pvr_rgb(ist_img))
        if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
        write(*,*) 'iflag_movie_mode each', i_lic, &
     &            pvr%pvr_param(i_lic)%view%iflag_movie_mode 
        if(pvr%pvr_param(i_lic)%view%iflag_movie_mode                   &
     &                                  .ne. IFLAG_NO_MOVIE) then
          write(*,*) 's_each_LIC_rendering_w_rot each', i_lic
          call s_each_LIC_rendering_w_rot                               &
     &     (istep_lic, time, repart_data%viz_fem,                       &
     &      repart_data%field_lic, lic_param(i_lic),                    &
     &      pvr%pvr_param(i_lic), pvr%pvr_proj(ist_rdr),                &
     &      pvr%pvr_rgb(ist_img))
        end if
!
        call dealloc_PVR_initialize(pvr%pvr_param(i_lic),               &
     &                              pvr%pvr_proj(ist_rdr))
        call dealloc_LIC_each_mesh(repart_p, repart_data,               &
     &                             lic_param(i_lic))
      end do
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+2)
      do i_lic = 1, pvr%num_pvr
        ist_img = pvr%istack_pvr_images(i_lic-1) + 1
        if(pvr%pvr_rgb(ist_img)%iflag_monitoring .gt. 0) then
          call sel_write_pvr_image_file                                 &
     &       ((-i_lic), iminus, pvr%pvr_rgb(ist_img))
        end if
      end do
      do i_lic = 1, pvr%num_pvr_images
        call sel_write_pvr_image_file                                   &
     &     ((-i_lic), istep_lic, pvr%pvr_rgb(i_lic))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
!
      end subroutine LIC_visualize_w_each_repart
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_visualize_w_shared_mesh                            &
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
      integer(kind = kint) :: i_lic, ist_rdr, ist_img
!
!
      if(my_rank .eq. 0) write(*,*) 's_each_LIC_rendering at once'
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      do i_lic = 1, pvr%num_pvr
        if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
        call cal_field_4_each_lic(geofem%mesh%node, nod_fld,            &
     &      lic_param(i_lic), repart_data%nod_fld_lic)
!
        if(repart_p%flag_repartition) then
          call repartition_lic_field                                    &
     &       (geofem%mesh%node, repart_data%viz_fem%mesh,               &
     &        repart_data%mesh_to_viz_tbl, repart_data%nod_fld_lic,     &
     &        repart_data%field_lic, v_sol)
        end if
!
        ist_rdr = pvr%istack_pvr_render(i_lic-1) + 1
        ist_img = pvr%istack_pvr_images(i_lic-1) + 1
        call s_each_LIC_rendering(istep_lic, time, repart_data%viz_fem, &
     &      repart_data%field_lic, lic_param(i_lic),                    &
     &      pvr%pvr_param(i_lic), pvr%pvr_proj(ist_rdr),                &
     &      pvr%pvr_rgb(ist_img))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+2)
      do i_lic = 1, pvr%num_pvr
        ist_img = pvr%istack_pvr_images(i_lic-1) + 1
        if(pvr%pvr_rgb(ist_img)%iflag_monitoring .gt. 0) then
          call sel_write_pvr_image_file                                 &
     &       ((-i_lic), iminus, pvr%pvr_rgb(ist_img))
        end if
      end do
      do i_lic = 1, pvr%num_pvr_images
        call sel_write_pvr_image_file                                   &
     &     ((-i_lic), istep_lic, pvr%pvr_rgb(i_lic))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      do i_lic = 1, pvr%num_pvr
        write(*,*) 's_each_LIC_rendering_w_rot once', i_lic
        if(pvr%pvr_param(i_lic)%view%iflag_movie_mode                   &
     &                                    .ne. IFLAG_NO_MOVIE) then
          if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
          call cal_field_4_each_lic(geofem%mesh%node, nod_fld,          &
     &      lic_param(i_lic), repart_data%nod_fld_lic)
!
          if(repart_p%flag_repartition) then
            call repartition_lic_field                                  &
     &        (geofem%mesh%node, repart_data%viz_fem%mesh,              &
     &         repart_data%mesh_to_viz_tbl, repart_data%nod_fld_lic,    &
     &         repart_data%field_lic, v_sol)
          end if
!
          ist_rdr = pvr%istack_pvr_render(i_lic-1) + 1
          ist_img = pvr%istack_pvr_images(i_lic-1) + 1
          call s_each_LIC_rendering_w_rot                               &
     &       (istep_lic, time, repart_data%viz_fem,                     &
     &        repart_data%field_lic,                                    &
     &        lic_param(i_lic), pvr%pvr_param(i_lic),                   &
     &        pvr%pvr_proj(ist_rdr), pvr%pvr_rgb(ist_img))
        end if
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      end subroutine LIC_visualize_w_shared_mesh
!
!  ---------------------------------------------------------------------
!
      end module lic_rendering_test
