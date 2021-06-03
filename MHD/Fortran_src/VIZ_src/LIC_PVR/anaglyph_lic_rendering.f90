!>@file   anaglyph_lic_rendering.f90
!!@brief  module anaglyph_lic_rendering
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for anaglyph LIC rendering
!!
!!@verbatim
!!      subroutine anaglyph_LIC_initialize(increment_lic,               &
!!     &          geofem, next_tbl, nod_fld, repart_ctl, lic_ctls, lic)
!!      subroutine anaglyph_LIC_visualize(istep_lic, time,              &
!!     &          geofem, next_tbl, nod_fld, lic, v_sol)
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(viz_repartition_ctl), intent(in) :: repart_ctl
!!        type(lic_rendering_controls), intent(inout) :: lic_ctls
!!        type(lic_volume_rendering_module), intent(inout) :: lic
!!@endverbatim
!
      module anaglyph_lic_rendering
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
      subroutine anaglyph_LIC_initialize(increment_lic,                 &
     &          geofem, next_tbl, nod_fld, repart_ctl, lic_ctls, lic)
!
      use t_control_data_pvr_sections
      use t_surf_grp_list_each_surf
      use set_pvr_control
      use rendering_and_image_nums
      use set_lic_controls
      use select_anaglyph_LIC_by_mesh
!
      integer(kind = kint), intent(in) :: increment_lic
      type(mesh_data), intent(in), target :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(viz_repartition_ctl), intent(in) :: repart_ctl
      type(lic_rendering_controls), intent(inout) :: lic_ctls
      type(lic_volume_rendering_module), intent(inout) :: lic
!
      integer(kind = kint) :: i_lic
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
!
      call alloc_pvr_data(lic%pvr)
!
      do i_lic = 1, lic%pvr%num_pvr
        call alloc_iflag_pvr_boundaries(geofem%group%surf_grp,          &
     &      lic%pvr%pvr_param(i_lic)%draw_param)
      end do
!
      allocate(lic%lic_param(lic%pvr%num_pvr))
      call s_set_lic_controls(geofem%group, nod_fld, lic%pvr%num_pvr,   &
     &    lic_ctls%pvr_ctl_type, lic_ctls%lic_ctl_type,                 &
     &    lic%lic_param, lic%pvr%pvr_param, lic%flag_each_repart)
!
      call count_num_anaglyph_and_images(lic%pvr%num_pvr,               &
     &    lic%pvr%num_pvr_rendering, lic%pvr%num_pvr_images)
      call alloc_pvr_images(lic%pvr)
!
      call set_anaglyph_rendering_pes(nprocs, lic%pvr%num_pvr,          &
     &    lic_ctls%pvr_ctl_type, lic%pvr%pvr_rgb)
!
      do i_lic = 1, lic%pvr%num_pvr
        if(lic_ctls%fname_lic_ctl(i_lic) .ne. 'NO_FILE'                 &
     &      .or. my_rank .ne. 0) then
          call dealloc_lic_count_data(lic_ctls%pvr_ctl_type(i_lic),     &
     &        lic_ctls%lic_ctl_type(i_lic))
        end if
      end do
!
      do i_lic = 1, lic%pvr%num_pvr
        call init_each_PVR_image(ione, lic%pvr%pvr_param(i_lic),        &
     &                           lic%pvr%pvr_rgb(i_lic))
      end do
!
      call LIC_init_nodal_field(geofem, lic%pvr%num_pvr, lic%lic_param, &
     &                          lic%repart_data)
!
      if(lic%flag_each_repart) return
      call init_sf_grp_list_each_surf                                   &
     &   (geofem%mesh%surf, geofem%group%surf_grp, lic%pvr%sf_grp_4_sf)
!
      call LIC_anaglyph_init_shared_mesh(geofem, next_tbl,              &
     &    lic%repart_p, lic%repart_data, lic%pvr)
!
      end subroutine anaglyph_LIC_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_LIC_visualize(istep_lic, time,                &
     &          geofem, next_tbl, nod_fld, lic, v_sol)
!
      use m_elapsed_labels_4_VIZ
      use select_anaglyph_LIC_by_mesh
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
!
      if(lic%pvr%num_pvr .le. 0 .or. istep_lic.lt.0) return
!
      if(lic%flag_each_repart) then
        call LIC_anaglyph_w_each_repart                                 &
     &     (istep_lic, time, geofem, next_tbl, nod_fld, lic%repart_p,   &
     &      lic%repart_data, lic%pvr, lic%lic_param, v_sol)
      else
        call LIC_anaglyph_w_shared_mesh                                 &
     &     (istep_lic, time, geofem, nod_fld, lic%repart_p,             &
     &      lic%repart_data, lic%pvr, lic%lic_param, v_sol)
      end if
!
      end subroutine anaglyph_LIC_visualize
!
!  ---------------------------------------------------------------------
!
      end module anaglyph_lic_rendering
