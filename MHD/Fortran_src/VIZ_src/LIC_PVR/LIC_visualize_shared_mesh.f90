!>@file   LIC_visualize_shared_mesh.f90
!!@brief  module LIC_visualize_shared_mesh
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
!!        type(lic_repart_reference), intent(inout) :: rep_ref_m
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
!!@endverbatim
!
      module LIC_visualize_shared_mesh
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
      private :: LIC_fixview_init_shared_mesh
      private :: LIC_fixview_viz_shared_mesh
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
      use LIC_movie_w_shared_mesh
      use LIC_anaglyph_w_shared_mesh
!
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: ele_comm
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(volume_partioning_param), intent(in) :: repart_p
!
      type(lic_repart_reference), intent(inout) :: rep_ref_m
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(volume_rendering_module), intent(inout) :: pvr
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(repart_p%iflag_repart_ref .eq. i_INT_COUNT_BASED) then
        call init_lic_repart_ref(geofem%mesh, pvr%pvr_rgb(1),           &
     &                           repart_p, rep_ref_m)
      end if
!
      call LIC_init_shared_mesh(geofem, ele_comm, next_tbl,             &
     &    repart_p, rep_ref_m, repart_data, m_SR)
      call init_sf_grp_list_each_surf                                   &
     &   (repart_data%viz_fem%mesh%surf,                                &
     &    repart_data%viz_fem%group%surf_grp, pvr%sf_grp_4_sf)
!
      call LIC_fixview_init_shared_mesh(repart_data, pvr, m_SR)
      call LIC_movie_init_shared_mesh(repart_data, pvr)
      call LIC_anaglyph_init_shared_mesh(repart_data, pvr, m_SR)
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
      use LIC_movie_w_shared_mesh
      use LIC_anaglyph_w_shared_mesh
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
!
      call LIC_fixview_viz_shared_mesh(istep_lic, time,                 &
     &    geofem, nod_fld, repart_p, repart_data, pvr, lic_param, m_SR)
      call LIC_movie_visualize_shared_mesh(istep_lic, time,             &
     &    geofem, nod_fld, repart_p, repart_data, pvr, lic_param, m_SR)
      call LIC_movie_quilt_shared_mesh(istep_lic, time,                 &
     &    geofem, nod_fld, repart_p, repart_data, pvr, lic_param, m_SR)
!
      call s_LIC_anaglyph_w_shared_mesh(istep_lic, time,                &
     &    geofem, nod_fld, repart_p, repart_data, pvr, lic_param, m_SR)
      call LIC_movie_anaglyph_shared_mesh(istep_lic, time,              &
     &    geofem, nod_fld, repart_p, repart_data, pvr, lic_param, m_SR)
!
      end subroutine LIC_visualize_w_shared_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine LIC_fixview_init_shared_mesh(repart_data, pvr, m_SR)
!
      use each_LIC_rendering
      use each_volume_rendering
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(volume_rendering_module), intent(inout) :: pvr
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_lic, ist_lic, ied_lic, ist_img, num_img
!
!
      ist_lic = pvr%PVR_sort%istack_PVR_modes(0) + 1
      ied_lic = pvr%PVR_sort%istack_PVR_modes(2)
      do i_lic = ist_lic, ied_lic
        ist_img = pvr%istack_pvr_images(i_lic-1)
        num_img = pvr%istack_pvr_images(i_lic) - ist_img
        call each_PVR_initialize(num_img,                               &
     &      repart_data%viz_fem%mesh, repart_data%viz_fem%group,        &
     &      pvr%pvr_param(i_lic), pvr%pvr_bound(i_lic))
        call init_fixed_view_and_images                                 &
     &     (num_img, repart_data%viz_fem%mesh, pvr%pvr_rgb(ist_img+1),  &
     &      pvr%pvr_param(i_lic), pvr%pvr_bound(i_lic),                 &
     &      pvr%pvr_proj(ist_img+1), m_SR)
      end do
!
      end subroutine LIC_fixview_init_shared_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_fixview_viz_shared_mesh(istep_lic, time, geofem,   &
     &          nod_fld, repart_p, repart_data, pvr, lic_param, m_SR)
!
      use m_elapsed_labels_4_VIZ
      use cal_pvr_modelview_mat
      use each_LIC_rendering
      use write_PVR_image
      use each_volume_rendering
      use LIC_anaglyph_w_shared_mesh
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
      ist_lic = pvr%PVR_sort%istack_PVR_modes(0) + 1
      ied_lic = pvr%PVR_sort%istack_PVR_modes(2)
      do i_lic = ist_lic, ied_lic
        if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
        call cal_field_4_each_lic(geofem%mesh%node, nod_fld,            &
     &      lic_param(i_lic), repart_data%nod_fld_lic)
        if(iflag_debug .gt. 0) write(*,*) 'set_LIC_each_field'
        call set_LIC_each_field(geofem, repart_p, lic_param(i_lic),     &
     &                          repart_data, m_SR)
!
        ist_img = pvr%istack_pvr_images(i_lic-1)
        num_img = pvr%istack_pvr_images(i_lic  ) - ist_img
        if(my_rank .eq. 0) write(*,*) 's_each_LIC_rendering', i_lic
        call reset_lic_count_line_int(rep_ref_viz)
        call s_each_LIC_rendering                                       &
     &     (istep_lic, time, num_img, repart_data%viz_fem,              &
     &      repart_data%field_lic, pvr%sf_grp_4_sf, lic_param(i_lic),   &
     &      pvr%pvr_param(i_lic), pvr%pvr_proj(ist_img+1),              &
     &      pvr%pvr_rgb(ist_img+1), rep_ref_viz, m_SR)
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+2)
      ist_lic = pvr%PVR_sort%istack_PVR_modes(0) + 1
      ied_lic = pvr%PVR_sort%istack_PVR_modes(1)
      do i_lic = ist_lic, ied_lic
        ist_img = pvr%istack_pvr_images(i_lic-1) + 1
        ied_img = pvr%istack_pvr_images(i_lic  )
        do i_img = ist_img, ied_img
          call sel_write_pvr_image_file(istep_lic, -1,                  &
     &                                  pvr%pvr_rgb(i_img))
        end do
      end do
!
      ist_lic = pvr%PVR_sort%istack_PVR_modes(1) + 1
      ied_lic = pvr%PVR_sort%istack_PVR_modes(2)
      do i_lic = ist_lic, ied_lic
        ist_img = pvr%istack_pvr_images(i_lic-1)
        num_img = pvr%istack_pvr_images(i_lic  ) - ist_img
        call set_output_rot_sequence_image(istep_lic,                   &
     &      pvr%pvr_rgb(ist_img+1)%id_pvr_file_type,                    &
     &      pvr%pvr_rgb(ist_img+1)%pvr_prefix, num_img,                 &
     &      pvr%pvr_param(i_lic)%stereo_def%n_column_row_view,          &
     &      pvr%pvr_rgb(ist_img+1))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
!
      end subroutine LIC_fixview_viz_shared_mesh
!
!  ---------------------------------------------------------------------
!
      end module LIC_visualize_shared_mesh
