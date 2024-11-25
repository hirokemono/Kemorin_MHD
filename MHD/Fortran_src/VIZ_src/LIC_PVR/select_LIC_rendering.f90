!>@file   select_LIC_rendering.f90
!!@brief  module select_LIC_rendering
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      subroutine LIC_initialize_w_shared_mesh                         &
!!     &         (elps_PVR, elps_LIC, geofem, ele_comm, next_tbl,       &
!!     &          repart_p, rep_ref_m, repart_data, pvr, m_SR)
!!        type(elapsed_lables), intent(in) :: elps_PVR
!!        type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: ele_comm
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(lic_repart_reference), intent(in) :: rep_ref_m
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!        type(volume_rendering_module), intent(inout) :: pvr
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine LIC_visualize_w_shared_mesh                          &
!!     &         (istep_lic, time, elps_PVR, elps_LIC, geofem, nod_fld, &
!!     &          repart_p, repart_data, pvr, lic_param, m_SR)
!!        integer(kind = kint), intent(in) :: istep_lic
!!        real(kind = kreal), intent(in) :: time
!!        type(elapsed_lables), intent(in) :: elps_PVR
!!        type(elapsed_lables), intent(in) :: elps_LIC
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(lic_repartioned_mesh), intent(inout) :: repart_data
!!        type(volume_rendering_module), intent(inout) :: pvr
!!        type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine LIC_visualize_w_each_repart                          &
!!     &         (istep_lic, time, elps_PVR, elps_LIC,                  &
!!     &          geofem, ele_comm, next_tbl, nod_fld,                  &
!!     &          repart_p, rep_ref_m, repart_data, pvr,                &
!!     &          lic_param, rep_ref, m_SR)
!!        integer(kind = kint), intent(in) :: istep_lic
!!        real(kind = kreal), intent(in) :: time
!!        type(elapsed_lables), intent(in) :: elps_PVR
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
      subroutine LIC_initialize_w_shared_mesh                           &
     &         (elps_PVR, elps_LIC, geofem, ele_comm, next_tbl,         &
     &          repart_p, rep_ref_m, repart_data, pvr, m_SR)
!
      use each_LIC_rendering
      use each_volume_rendering
      use each_anaglyph_PVR
      use multi_volume_renderings
      use anaglyph_volume_renderings
!
      type(elapsed_lables), intent(in) :: elps_PVR, elps_LIC
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
!
!
      call LIC_init_shared_mesh(elps_LIC, geofem, ele_comm, next_tbl,   &
     &                          repart_p, rep_ref_m, repart_data, m_SR)
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
      call set_PVR_view_and_images(pvr%num_pvr, pvr%num_pvr_images,     &
     &    elps_PVR, repart_data%viz_fem%mesh, pvr%PVR_sort,             &
     &    pvr%pvr_rgb, pvr%pvr_param, pvr%pvr_bound, pvr%pvr_proj,      &
     &    m_SR)
      call PVR_anaglyph_view_and_images                                 &
     &   (pvr%num_pvr, pvr%num_pvr_images, elps_PVR,                    &
     &    repart_data%viz_fem%mesh, pvr%PVR_sort, pvr%pvr_rgb,          &
     &    pvr%pvr_param, pvr%pvr_bound, pvr%pvr_proj, m_SR)
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
!
      end subroutine LIC_initialize_w_shared_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine LIC_visualize_w_shared_mesh                            &
     &         (istep_lic, time, elps_PVR, elps_LIC, geofem, nod_fld,   &
     &          repart_p, repart_data, pvr, lic_param, m_SR)
!
      use m_work_time
      use LIC_anaglyph_w_shared_mesh
      use LIC_visualize_shared_mesh
      use multi_volume_renderings
      use write_multi_PVR_image
!
      integer(kind = kint), intent(in) :: istep_lic
      real(kind = kreal), intent(in) :: time
!
      type(elapsed_lables), intent(in) :: elps_PVR, elps_LIC
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
      integer(kind = kint) :: ist_lic, ied_lic
!
!
      if(elps_LIC%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_LIC%ist_elapsed+1)
      call alloc_lic_repart_ref(repart_data%viz_fem%mesh%node,          &
     &                          rep_ref_viz)
!
      call LIC_fixview_viz_shared_mesh                                  &
     &   (istep_lic, time, elps_LIC, geofem, nod_fld, repart_p,         &
     &    repart_data, pvr, lic_param, rep_ref_viz, m_SR)
      if(elps_LIC%flag_elapsed)                                         &
     &          call end_elapsed_time(elps_LIC%ist_elapsed+1)
!
      if(elps_LIC%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_LIC%ist_elapsed+2)
      ist_lic = pvr%PVR_sort%istack_PVR_modes(0) + 1
      ied_lic = pvr%PVR_sort%istack_PVR_modes(1)
      call output_PVR_images                                            &
     &   (istep_lic, pvr%num_pvr, ist_lic, ied_lic, pvr%num_pvr_images, &
     &    pvr%PVR_sort%istack_pvr_images, pvr%pvr_rgb)
!
      ist_lic = pvr%PVR_sort%istack_PVR_modes(1) + 1
      ied_lic = pvr%PVR_sort%istack_PVR_modes(2)
      call output_quilt_PVR_images                                      &
     &   (istep_lic, pvr%num_pvr, ist_lic, ied_lic,                     &
     &    pvr%num_pvr_images, pvr%PVR_sort%istack_pvr_images,           &
     &    pvr%pvr_param, pvr%pvr_rgb)
      if(elps_LIC%flag_elapsed)                                         &
     &          call end_elapsed_time(elps_LIC%ist_elapsed+2)
!
!
      if(elps_LIC%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_LIC%ist_elapsed+1)
      call LIC_movie_visualize_shared_mesh                              &
     &   (istep_lic, time, elps_PVR, elps_LIC, geofem, nod_fld,         &
     &    repart_p, repart_data, pvr, lic_param, rep_ref_viz, m_SR)
      call LIC_movie_quilt_shared_mesh                                  &
     &   (istep_lic, time, elps_PVR, elps_LIC, geofem, nod_fld,         &
     &    repart_p, repart_data, pvr, lic_param, rep_ref_viz, m_SR)
      if(elps_LIC%flag_elapsed)                                         &
     &          call end_elapsed_time(elps_LIC%ist_elapsed+1)
!
      call s_LIC_anaglyph_w_shared_mesh                                 &
     &   (istep_lic, time, elps_LIC, geofem, nod_fld, repart_p,         &
     &    repart_data, pvr, lic_param, rep_ref_viz, m_SR)
!
      if(elps_LIC%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_LIC%ist_elapsed+1)
      call LIC_movie_anaglyph_shared_mesh                               &
     &   (istep_lic, time, elps_PVR, elps_LIC, geofem, nod_fld,         &
     &    repart_p, repart_data, pvr, lic_param, rep_ref_viz, m_SR)
      call dealloc_lic_repart_ref(rep_ref_viz)
      if(elps_LIC%flag_elapsed)                                         &
     &          call end_elapsed_time(elps_LIC%ist_elapsed+1)
!
      end subroutine LIC_visualize_w_shared_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine LIC_visualize_w_each_repart                            &
     &         (istep_lic, time, elps_PVR, elps_LIC,                    &
     &          geofem, ele_comm, next_tbl, nod_fld,                    &
     &          repart_p, rep_ref_m, repart_data, pvr,                  &
     &          lic_param, rep_ref, m_SR)
!
      use m_work_time
      use LIC_visualize_each_repart
      use LIC_movie_w_each_repart
      use LIC_anaglyph_w_each_repart
      use multi_volume_renderings
      use write_multi_PVR_image
!
      integer(kind = kint), intent(in) :: istep_lic
      real(kind = kreal), intent(in) :: time
!
      type(elapsed_lables), intent(in) :: elps_PVR, elps_LIC
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
      integer(kind = kint) :: ist_lic, ied_lic
!
!
      if(elps_LIC%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_LIC%ist_elapsed+1)
      call LIC_fixview_render_each_repart                               &
     &   (istep_lic, time, elps_PVR, elps_LIC, geofem, ele_comm,        &
     &    next_tbl, nod_fld, repart_p, rep_ref_m, repart_data,          &
     &    pvr, lic_param, rep_ref, m_SR)
      call LIC_quilt_render_each_repart                                 &
     &   (istep_lic, time, elps_PVR, elps_LIC, geofem, ele_comm,        &
     &    next_tbl, nod_fld,  repart_p, rep_ref_m, repart_data,         &
     &    pvr, lic_param, rep_ref, m_SR)
      if(elps_LIC%flag_elapsed)                                         &
     &          call end_elapsed_time(elps_LIC%ist_elapsed+1)
!
      if(elps_LIC%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_LIC%ist_elapsed+2)
      ist_lic = pvr%PVR_sort%istack_PVR_modes(0) + 1
      ied_lic = pvr%PVR_sort%istack_PVR_modes(1)
      call output_PVR_images(istep_lic, pvr%num_pvr, ist_lic, ied_lic,  &
     &    pvr%num_pvr_images, pvr%PVR_sort%istack_pvr_images,           &
     &    pvr%pvr_rgb)
!
      ist_lic = pvr%PVR_sort%istack_PVR_modes(1) + 1
      ied_lic = pvr%PVR_sort%istack_PVR_modes(2)
      call output_quilt_PVR_images                                      &
     &   (istep_lic, pvr%num_pvr, ist_lic, ied_lic,                     &
     &    pvr%num_pvr_images, pvr%PVR_sort%istack_pvr_images,           &
     &    pvr%pvr_param, pvr%pvr_rgb)
      if(elps_LIC%flag_elapsed)                                         &
     &          call end_elapsed_time(elps_LIC%ist_elapsed+2)
!
!
      if(elps_LIC%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_LIC%ist_elapsed+1)
      call LIC_movie_visualize_each_repart                              &
     &   (istep_lic, time, elps_PVR, elps_LIC, geofem, ele_comm,        &
     &    next_tbl, nod_fld, repart_p, rep_ref_m, repart_data,          &
     &    pvr, lic_param, rep_ref, m_SR)
      call LIC_movie_quilt_each_repart                                  &
     &   (istep_lic, time, elps_PVR, elps_LIC, geofem, ele_comm,        &
     &    next_tbl, nod_fld, repart_p, rep_ref_m, repart_data,          &
     &    pvr, lic_param, rep_ref, m_SR)
      if(elps_LIC%flag_elapsed)                                         &
     &          call end_elapsed_time(elps_LIC%ist_elapsed+1)
!
      call s_LIC_anaglyph_w_each_repart                                 &
     &   (istep_lic, time, elps_PVR, elps_LIC, geofem, ele_comm,        &
     &    next_tbl, nod_fld, repart_p, rep_ref_m, repart_data,          &
     &    pvr, lic_param, rep_ref, m_SR)
!
      if(elps_LIC%flag_elapsed)                                         &
     &          call start_elapsed_time(elps_LIC%ist_elapsed+1)
      call LIC_movie_anaglyph_each_repart(istep_lic, time,              &
     &    elps_PVR, elps_LIC, geofem, ele_comm, next_tbl, nod_fld,      &
     &    repart_p, rep_ref_m, repart_data, pvr, lic_param,             &
     &    rep_ref, m_SR)
      if(elps_LIC%flag_elapsed)                                         &
     &          call end_elapsed_time(elps_LIC%ist_elapsed+1)
!
      end subroutine LIC_visualize_w_each_repart
!
!  ---------------------------------------------------------------------
!
      end module select_LIC_rendering
