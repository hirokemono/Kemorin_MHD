!>@file   LIC_visualize_shared_mesh.f90
!!@brief  module LIC_visualize_shared_mesh
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      subroutine LIC_fixview_viz_shared_mesh                          &
!!     &         (istep_lic, time, elps_LIC, geofem, nod_fld, repart_p, &
!!     &          repart_data, pvr, lic_param, rep_ref_viz, m_SR)
!!      subroutine LIC_movie_visualize_shared_mesh(istep_lic, time,     &
!!     &          elps_PVR, elps_LIC, geofem, nod_fld, repart_p,        &
!!     &          repart_data, pvr, lic_param, rep_ref_viz, m_SR)
!!      subroutine LIC_movie_quilt_shared_mesh(istep_lic, time,         &
!!     &          elps_PVR, elps_LIC, geofem, nod_fld, repart_p,        &
!!     &          repart_data, pvr, lic_param, rep_ref_viz, m_SR)
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
!!        type(lic_repart_reference), intent(inout) :: rep_ref_viz
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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_fixview_viz_shared_mesh                            &
     &         (istep_lic, time, elps_LIC, geofem, nod_fld, repart_p,   &
     &          repart_data, pvr, lic_param, rep_ref_viz, m_SR)
!
      use cal_pvr_modelview_mat
      use each_LIC_rendering
      use each_volume_rendering
      use LIC_anaglyph_w_shared_mesh
!
      integer(kind = kint), intent(in) :: istep_lic
      real(kind = kreal), intent(in) :: time
!
      type(elapsed_lables), intent(in) :: elps_LIC
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(volume_partioning_param), intent(in) :: repart_p
!
      type(lic_repartioned_mesh), intent(inout) :: repart_data
      type(volume_rendering_module), intent(inout) :: pvr
      type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
      type(lic_repart_reference), intent(inout) :: rep_ref_viz
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_lic, ist_lic, ied_lic
      integer(kind = kint) :: ist_img, num_img
!
!
      ist_lic = pvr%PVR_sort%istack_PVR_modes(0) + 1
      ied_lic = pvr%PVR_sort%istack_PVR_modes(2)
      do i_lic = ist_lic, ied_lic
        if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
        call cal_field_4_each_lic(geofem%mesh%node, nod_fld,            &
     &      lic_param(i_lic), repart_data%nod_fld_lic)
        if(iflag_debug .gt. 0) write(*,*) 'set_LIC_each_field'
        call set_LIC_each_field(elps_LIC, geofem, repart_p,             &
     &                          lic_param(i_lic), repart_data, m_SR)
!
        if(pvr%pvr_param(i_lic)%movie_def%iflag_movie_mode              &
     &                                  .ne. IFLAG_NO_MOVIE) cycle
        ist_img = pvr%PVR_sort%istack_pvr_images(i_lic-1)
        num_img = pvr%PVR_sort%istack_pvr_images(i_lic  ) - ist_img
        if(my_rank .eq. 0) write(*,*) 's_each_LIC_rendering', i_lic
        call reset_lic_count_line_int(rep_ref_viz)
        call s_each_LIC_rendering                                       &
     &     (istep_lic, time, num_img, elps_LIC, repart_data%viz_fem,    &
     &      repart_data%field_lic, pvr%sf_grp_4_sf, lic_param(i_lic),   &
     &      pvr%pvr_param(i_lic), pvr%pvr_proj(ist_img+1),              &
     &      pvr%pvr_rgb(ist_img+1), rep_ref_viz, m_SR)
      end do
!
      end subroutine LIC_fixview_viz_shared_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_movie_visualize_shared_mesh(istep_lic, time,       &
     &          elps_PVR, elps_LIC, geofem, nod_fld, repart_p,          &
     &          repart_data, pvr, lic_param, rep_ref_viz, m_SR)
!
      use cal_pvr_modelview_mat
      use each_LIC_rendering
      use write_PVR_image
      use each_volume_rendering
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
      type(lic_repart_reference), intent(inout) :: rep_ref_viz
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_lic, ist_lic, ied_lic
      integer(kind = kint) :: ist_img, num_img
!
!
      ist_lic = pvr%PVR_sort%istack_PVR_modes(2) + 1
      ied_lic = pvr%PVR_sort%istack_PVR_modes(3)
      do i_lic = ist_lic, ied_lic
        ist_img = pvr%PVR_sort%istack_pvr_images(i_lic-1)
        num_img = pvr%PVR_sort%istack_pvr_images(i_lic) - ist_img
!
        call cal_field_4_each_lic(geofem%mesh%node, nod_fld,            &
     &      lic_param(i_lic), repart_data%nod_fld_lic)
        call set_LIC_each_field(elps_LIC, geofem, repart_p,             &
     &                          lic_param(i_lic), repart_data, m_SR)
        call reset_lic_count_line_int(rep_ref_viz)
!
        call s_each_LIC_rendering_w_rot                                 &
     &     (istep_lic, time, elps_PVR, elps_LIC, repart_data%viz_fem,   &
     &      repart_data%field_lic, pvr%sf_grp_4_sf, lic_param(i_lic),   &
     &      pvr%pvr_param(i_lic), pvr%pvr_bound(i_lic),                 &
     &      pvr%pvr_proj(ist_img+1), pvr%pvr_rgb(ist_img+1),            &
     &      rep_ref_viz, m_SR)
      end do
!
      end subroutine LIC_movie_visualize_shared_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_movie_quilt_shared_mesh(istep_lic, time,           &
     &          elps_PVR, elps_LIC, geofem, nod_fld, repart_p,          &
     &          repart_data, pvr, lic_param, rep_ref_viz, m_SR)
!
      use cal_pvr_modelview_mat
      use each_LIC_rendering
      use write_PVR_image
      use each_volume_rendering
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
      type(lic_repart_reference), intent(inout) :: rep_ref_viz
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_lic, ist_lic, ied_lic
      integer(kind = kint) :: ist_img, num_img
!
!
      ist_lic = pvr%PVR_sort%istack_PVR_modes(3) + 1
      ied_lic = pvr%PVR_sort%istack_PVR_modes(4)
      do i_lic = ist_lic, ied_lic
        ist_img = pvr%PVR_sort%istack_pvr_images(i_lic-1)
        num_img = pvr%PVR_sort%istack_pvr_images(i_lic) - ist_img
        call cal_field_4_each_lic(geofem%mesh%node, nod_fld,            &
     &      lic_param(i_lic), repart_data%nod_fld_lic)
        call set_LIC_each_field(elps_LIC, geofem, repart_p,             &
     &                          lic_param(i_lic), repart_data, m_SR)
        call reset_lic_count_line_int(rep_ref_viz)
!
        call each_LIC_quilt_rendering_w_rot                             &
     &     (istep_lic, time, num_img, elps_PVR, elps_LIC,               &
     &      repart_data%viz_fem, repart_data%field_lic,                 &
     &      pvr%sf_grp_4_sf, lic_param(i_lic), pvr%pvr_param(i_lic),    &
     &      pvr%pvr_bound(i_lic), pvr%pvr_proj(ist_img+1),              &
     &      pvr%pvr_rgb(ist_img+1), rep_ref_viz, m_SR)
      end do
!
      end subroutine LIC_movie_quilt_shared_mesh
!
!  ---------------------------------------------------------------------
!
      end module LIC_visualize_shared_mesh
