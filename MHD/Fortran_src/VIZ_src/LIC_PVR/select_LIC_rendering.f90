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
!!     &          next_tbl, repart_p, rep_ref_m, repart_data, pvr, m_SR)
!!        type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: ele_comm
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(lic_repart_reference), intent(in) :: rep_ref_m
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!        type(volume_rendering_module), intent(inout) :: pvr
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine LIC_visualize_w_shared_mesh(istep_lic, time, geofem, &
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
!!     &          repart_p, rep_ref_m, repart_data, pvr, lic_param,     &
!!     &          rep_ref, m_SR)
!!        integer(kind = kint), intent(in) :: istep_lic
!!        real(kind = kreal), intent(in) :: time
!!        type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: ele_comm
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(phys_data), intent(in) :: nod_fld
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(lic_repart_reference), intent(in) :: rep_ref_m
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!        type(volume_rendering_module), intent(inout) :: pvr
!!        type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
!!        type(lic_repart_reference), intent(inout)                     &
!!     &                             :: rep_ref(pvr%num_pvr)
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
      use t_lic_repart_reference
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
     &          next_tbl, repart_p, rep_ref_m, repart_data, pvr, m_SR)
!
      use each_LIC_rendering
      use each_volume_rendering
      use each_anaglyph_PVR
      use set_PVR_view_and_images
!
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: ele_comm
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(volume_partioning_param), intent(in) :: repart_p
      type(lic_repart_reference), intent(in) :: rep_ref_m
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(volume_rendering_module), intent(inout) :: pvr
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_lic
      integer(kind = kint) :: ist_img, num_img
!
!
      call LIC_init_shared_mesh(geofem, ele_comm, next_tbl,             &
     &    repart_p, rep_ref_m, repart_data, m_SR)
      call init_sf_grp_list_each_surf                                   &
     &   (repart_data%viz_fem%mesh%surf,                                &
     &    repart_data%viz_fem%group%surf_grp, pvr%sf_grp_4_sf)
!
      do i_lic = 1, pvr%num_pvr
        call each_PVR_initialize                                        &
     &     (repart_data%viz_fem%mesh, repart_data%viz_fem%group,        &
     &      pvr%pvr_param(i_lic), pvr%pvr_bound(i_lic))
      end do
!
      call s_set_PVR_view_and_images                                    &
     &   (pvr%num_pvr, pvr%num_pvr_images, pvr%istack_pvr_images,       &
     &    repart_data%viz_fem%mesh, pvr%PVR_sort, pvr%pvr_rgb,          &
     &    pvr%pvr_param, pvr%pvr_bound, pvr%pvr_proj, m_SR)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+7)
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
      use LIC_anaglyph_w_shared_mesh
      use LIC_visualize_shared_mesh
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
      type(lic_repart_reference), save :: rep_ref_viz
      integer(kind = kint) :: i_lic, ist_lic, ied_lic
      integer(kind = kint) :: i_img, ist_img, ied_img, num_img
!
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      call alloc_lic_repart_ref(repart_data%viz_fem%mesh%node,          &
     &                          rep_ref_viz)
!
      call LIC_fixview_viz_shared_mesh                                  &
     &   (istep_lic, time, geofem, nod_fld, repart_p,                   &
     &    repart_data, pvr, lic_param, rep_ref_viz, m_SR)
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+2)
      call output_PVR_images                                            &
     &   (istep_lic, pvr%num_pvr, pvr%num_pvr_images,                   &
     &    pvr%istack_pvr_images, pvr%PVR_sort,                          &
     &    pvr%pvr_param, pvr%pvr_rgb)
      call output_quilt_PVR_images                                      &
     &   (istep_lic, pvr%num_pvr, pvr%num_pvr_images,                   &
     &    pvr%istack_pvr_images, pvr%PVR_sort,                          &
     &    pvr%pvr_param, pvr%pvr_rgb)
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
!
!
      call LIC_movie_visualize_shared_mesh                              &
     &   (istep_lic, time, geofem, nod_fld, repart_p,                   &
     &    repart_data, pvr, lic_param, rep_ref_viz, m_SR)
!
      call LIC_movie_quilt_shared_mesh                                  &
     &   (istep_lic, time, geofem, nod_fld, repart_p,                   &
     &    repart_data, pvr, lic_param, rep_ref_viz, m_SR)
!
      call s_LIC_anaglyph_w_shared_mesh                                 &
     &   (istep_lic, time, geofem, nod_fld, repart_p,                   &
     &    repart_data, pvr, lic_param, rep_ref_viz, m_SR)
!
      call LIC_movie_anaglyph_shared_mesh                               &
     &   (istep_lic, time, geofem, nod_fld, repart_p,                   &
     &    repart_data, pvr, lic_param, rep_ref_viz, m_SR)
      call dealloc_lic_repart_ref(rep_ref_viz)
!
      end subroutine LIC_visualize_w_shared_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine LIC_visualize_w_each_repart                            &
     &         (istep_lic, time, geofem, ele_comm, next_tbl, nod_fld,   &
     &          repart_p, rep_ref_m, repart_data, pvr, lic_param,       &
     &          rep_ref, m_SR)
!
      use m_elapsed_labels_4_VIZ
      use t_surf_grp_list_each_surf
      use t_lic_field_data
      use cal_pvr_modelview_mat
      use each_LIC_rendering
      use write_PVR_image
      use each_volume_rendering
      use rendering_streo_LIC_image
      use each_anaglyph_PVR
      use set_PVR_view_and_image
      use set_PVR_view_and_images
      use calypso_reverse_send_recv
      use bring_back_rendering_counts
      use LIC_visualize_each_repart
      use LIC_movie_w_each_repart
      use LIC_anaglyph_w_each_repart
!
      integer(kind = kint), intent(in) :: istep_lic
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: ele_comm
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(phys_data), intent(in) :: nod_fld
      type(volume_partioning_param), intent(in) :: repart_p
      type(lic_repart_reference), intent(in) :: rep_ref_m
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(volume_rendering_module), intent(inout) :: pvr
      type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
      type(lic_repart_reference), intent(inout) :: rep_ref(pvr%num_pvr)
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call LIC_fixview_render_each_repart                               &
     &   (istep_lic, time, geofem, ele_comm, next_tbl, nod_fld,         &
     &    repart_p, rep_ref_m, repart_data, pvr, lic_param,             &
     &    rep_ref, m_SR)
      call LIC_quilt_render_each_repart                                 &
     &   (istep_lic, time, geofem, ele_comm, next_tbl, nod_fld,         &
     &    repart_p, rep_ref_m, repart_data, pvr, lic_param,             &
     &    rep_ref, m_SR)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+2)
      call output_PVR_images                                            &
     &   (istep_lic, pvr%num_pvr, pvr%num_pvr_images,                   &
     &    pvr%istack_pvr_images, pvr%PVR_sort,                          &
     &    pvr%pvr_param, pvr%pvr_rgb)
      call output_quilt_PVR_images                                      &
     &   (istep_lic, pvr%num_pvr, pvr%num_pvr_images,                   &
     &    pvr%istack_pvr_images, pvr%PVR_sort,                          &
     &    pvr%pvr_param, pvr%pvr_rgb)
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
!
!
      call LIC_movie_visualize_each_repart                              &
     &   (istep_lic, time, geofem, ele_comm, next_tbl, nod_fld,         &
     &    repart_p, rep_ref_m, repart_data, pvr, lic_param,             &
     &    rep_ref, m_SR)
      call LIC_movie_quilt_each_repart                                  &
     &   (istep_lic, time, geofem, ele_comm, next_tbl, nod_fld,         &
     &    repart_p, rep_ref_m, repart_data, pvr, lic_param,             &
     &    rep_ref, m_SR)
!
      call s_LIC_anaglyph_w_each_repart                                 &
     &   (istep_lic, time, geofem, ele_comm, next_tbl, nod_fld,         &
     &    repart_p, rep_ref_m, repart_data, pvr, lic_param,             &
     &    rep_ref, m_SR)
      call LIC_movie_anaglyph_each_repart                               &
     &   (istep_lic, time, geofem, ele_comm, next_tbl, nod_fld,         &
     &    repart_p, rep_ref_m, repart_data, pvr, lic_param,             &
     &    rep_ref, m_SR)
!
      end subroutine LIC_visualize_w_each_repart
!
!  ---------------------------------------------------------------------
!
      subroutine output_PVR_images(istep_lic, num_pvr, num_pvr_images,  &
     &          istack_pvr_images, PVR_sort, pvr_param, pvr_rgb)
!
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_lic
      integer(kind = kint), intent(in) :: num_pvr, num_pvr_images
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_pvr_images(0:num_pvr)
!
      type(sort_PVRs_by_type), intent(in) :: PVR_sort
      type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!
      integer(kind = kint) :: i_pvr, ist_pvr, ied_pvr
      integer(kind = kint) :: i_img, ist_img, num_img
!
!
      ist_pvr = PVR_sort%istack_PVR_modes(0) + 1
      ied_pvr = PVR_sort%istack_PVR_modes(1)
      do i_pvr = ist_pvr, ied_pvr
        ist_img = istack_pvr_images(i_pvr-1)
        num_img = istack_pvr_images(i_pvr  ) - ist_img
        do i_img = 1, num_img
          call sel_write_pvr_image_file(istep_lic, -1,                  &
     &                                  pvr_rgb(i_img+ist_img))
        end do
      end do
!
      end subroutine output_PVR_images
!
!  ---------------------------------------------------------------------
!
      subroutine output_quilt_PVR_images                                &
     &         (istep_lic, num_pvr, num_pvr_images,                     &
     &          istack_pvr_images, PVR_sort, pvr_param, pvr_rgb)
!
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_lic
      integer(kind = kint), intent(in) :: num_pvr, num_pvr_images
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_pvr_images(0:num_pvr)
!
      type(sort_PVRs_by_type), intent(in) :: PVR_sort
      type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!
      integer(kind = kint) :: i_pvr, ist_pvr, ied_pvr
      integer(kind = kint) :: ist_img, num_img
!
!
      ist_pvr = PVR_sort%istack_PVR_modes(1) + 1
      ied_pvr = PVR_sort%istack_PVR_modes(2)
      do i_pvr = ist_pvr, ied_pvr
        ist_img = istack_pvr_images(i_pvr-1)
        num_img = istack_pvr_images(i_pvr  ) - ist_img
        call set_output_rot_sequence_image(istep_lic, -1,               &
     &      pvr_rgb(ist_img+1)%id_pvr_file_type,                        &
     &      pvr_rgb(ist_img+1)%pvr_prefix, num_img,                     &
     &      pvr_param(i_pvr)%stereo_def%n_column_row_view,              &
     &      pvr_rgb(ist_img+1))
      end do
!
      end subroutine output_quilt_PVR_images
!
!  ---------------------------------------------------------------------
!
      end module select_LIC_rendering
