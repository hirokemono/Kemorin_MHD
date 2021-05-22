!>@file   select_LIC_rendering.f90
!!@brief  module select_LIC_rendering
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
     &          repart_p, repart_data, pvr, lic_param)
!
      use each_LIC_rendering
!
      type(mesh_data), intent(in) :: geofem
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(volume_partioning_param), intent(in) :: repart_p
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(volume_rendering_module), intent(inout) :: pvr
      type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
!
      integer(kind = kint) :: i_lic, ist_rdr, ist_img
!
!
      call LIC_init_shared_mesh(pvr%num_pvr, repart_p,                  &
     &    geofem, next_tbl, repart_data, lic_param)
!
      do i_lic = 1, pvr%num_pvr
        ist_rdr = pvr%istack_pvr_render(i_lic-1) + 1
        ist_img = pvr%istack_pvr_images(i_lic-1) + 1
        call each_PVR_initialize(i_lic,                                 &
     &      repart_data%viz_fem%mesh, repart_data%viz_fem%group,        &
     &      pvr%pvr_rgb(ist_img), pvr%pvr_param(i_lic),                 &
     &      pvr%pvr_proj(ist_rdr))
      end do
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
!
      end subroutine LIC_initialize_w_shared_mesh
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
      call set_LIC_shared_mesh_field(pvr%num_pvr, geofem, nod_fld,      &
     &    repart_p, lic_param, repart_data, v_sol)
!
      if(my_rank .eq. 0) write(*,*) 's_each_LIC_rendering at once'
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      do i_lic = 1, pvr%num_pvr
        ist_rdr = pvr%istack_pvr_render(i_lic-1) + 1
        ist_img = pvr%istack_pvr_images(i_lic-1) + 1
        call s_each_LIC_rendering(istep_lic, time, repart_data%viz_fem, &
     &      repart_data%field_lic(i_lic), lic_param(i_lic),             &
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
     &      lic_param(i_lic), repart_data%nod_fld_lic(i_lic))
!
          if(repart_p%flag_repartition) then
            call repartition_lic_field(geofem%mesh%node,                &
     &          repart_data%viz_fem%mesh,                               &
     &          repart_data%mesh_to_viz_tbl,                            &
     &          repart_data%nod_fld_lic(i_lic),                         &
     &          repart_data%field_lic(i_lic), v_sol)
          end if
!
          ist_rdr = pvr%istack_pvr_render(i_lic-1) + 1
          ist_img = pvr%istack_pvr_images(i_lic-1) + 1
          call s_each_LIC_rendering_w_rot                               &
     &       (istep_lic, time, repart_data%viz_fem,                     &
     &        repart_data%field_lic(i_lic), lic_param(i_lic),           &
     &        pvr%pvr_param(i_lic), pvr%pvr_proj(ist_rdr),              &
     &        pvr%pvr_rgb(ist_img))
        end if
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      end subroutine LIC_visualize_w_shared_mesh
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
      integer(kind = kint) :: i_lic, ist_rdr, ist_img
!
!
      do i_lic = 1, pvr%num_pvr
        ist_rdr = pvr%istack_pvr_render(i_lic-1) + 1
        ist_img = pvr%istack_pvr_images(i_lic-1) + 1
        if(iflag_debug .gt. 0) write(*,*) 'set_LIC_each_mesh_field'
        call set_LIC_each_mesh_field(geofem, next_tbl, nod_fld,         &
     &      repart_p, lic_param(i_lic), repart_data, v_sol)
!
        if(my_rank .eq. 0) write(*,*) 'each_PVR_initialize'
        call each_PVR_initialize(i_lic,                                 &
     &      repart_data%viz_fem%mesh, repart_data%viz_fem%group,        &
     &      pvr%pvr_rgb(ist_img), pvr%pvr_param(i_lic),                 &
     &      pvr%pvr_proj(ist_rdr))
!
        if(my_rank .eq. 0) write(*,*) 's_each_LIC_rendering each'
        if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
        call s_each_LIC_rendering(istep_lic, time, repart_data%viz_fem, &
     &      repart_data%field_lic(1), lic_param(i_lic),                 &
     &      pvr%pvr_param(i_lic), pvr%pvr_proj(ist_rdr),                &
     &      pvr%pvr_rgb(ist_img))
        if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
        write(*,*) 'iflag_movie_mode each', i_lic, &
     &            pvr%pvr_param(i_lic)%view%iflag_movie_mode 
        if(pvr%pvr_param(i_lic)%view%iflag_movie_mode                   &
     &                                  .ne. IFLAG_NO_MOVIE) then
          write(*,*) 's_each_LIC_rendering_w_rot each', i_lic
          call s_each_LIC_rendering_w_rot(istep_lic, time,              &
     &        repart_data%viz_fem, repart_data%field_lic(1),            &
     &        lic_param(i_lic), pvr%pvr_param(i_lic),                   &
     &         pvr%pvr_proj(ist_rdr), pvr%pvr_rgb(ist_img))
        end if
!
        call dealloc_PVR_initialize(pvr%pvr_param(i_lic),               &
     &                              pvr%pvr_proj(ist_rdr))
        call dealloc_LIC_each_mesh                                      &
     &     (repart_p, lic_param(i_lic)%each_part_p, repart_data)
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
      end module select_LIC_rendering
