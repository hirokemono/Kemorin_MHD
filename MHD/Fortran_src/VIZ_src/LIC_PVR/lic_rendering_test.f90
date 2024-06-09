!>@file   lic_rendering_test.f90
!!@brief  module lic_rendering_test
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      subroutine LIC_initialize_test(increment_lic, geofem, ele_comm, &
!!     &                               next_tbl, nod_fld, tracer, fline,&
!!     &                               lic_ctls, repart_ctl, lic, m_SR)
!!      subroutine LIC_visualize_test(istep_lic, time, geofem, ele_comm,&
!!     &                              next_tb, lnod_fld, lic, m_SR)
!!        type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: ele_comm
!!        type(phys_data), intent(in) :: nod_fld
!!        type(tracer_module), intent(in) :: tracer
!!        type(fieldline_module), intent(in) :: fline
!!        type(lic_rendering_controls), intent(inout) :: lic_ctls
!!        type(viz_repartition_ctl), intent(inout) :: repart_ctl
!!        type(lic_volume_rendering_module), intent(inout) :: lic
!!        type(mesh_SR), intent(inout) :: m_SR
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
      use t_comm_table
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
      use t_LIC_re_partition
      use t_control_param_LIC
      use t_mesh_SR
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
      subroutine LIC_initialize_test(increment_lic, geofem, ele_comm,   &
     &                               next_tbl, nod_fld, tracer, fline,  &
     &                               lic_ctls, repart_ctl, lic, m_SR)
!
      use t_control_data_pvr_sections
      use t_particle_trace
      use t_fieldline
      use set_pvr_control
      use rendering_and_image_nums
      use set_lic_controls
      use ctl_data_lic_pvr_IO
      use select_LIC_rendering
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: increment_lic
      type(mesh_data), intent(in), target :: geofem
      type(communication_table), intent(in) :: ele_comm
      type(phys_data), intent(in) :: nod_fld
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
      type(next_nod_ele_table), intent(in) :: next_tbl
!
      type(lic_rendering_controls), intent(inout) :: lic_ctls
      type(viz_repartition_ctl), intent(inout) :: repart_ctl
      type(lic_volume_rendering_module), intent(inout) :: lic
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_lic, ist_img, num_img
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
      call set_ctl_param_vol_repart(repart_ctl, lic%repart_p)
      call set_lic_repart_reference_param                               &
     &   (repart_ctl%new_part_ctl, lic%repart_p, lic%rep_ref_m)
      call dealloc_control_vol_repart(repart_ctl)
!
      call bcast_lic_controls(lic%pvr%num_pvr,                          &
     &    lic_ctls%pvr_ctl_type, lic_ctls%lic_ctl_type,                 &
     &    lic%pvr%cflag_update)
!
      call alloc_pvr_data(lic%pvr)
!
      allocate(lic%lic_param(lic%pvr%num_pvr))
      allocate(lic%rep_ref(lic%pvr%num_pvr))
      call s_set_lic_controls                                           &
     &   (geofem%group, nod_fld, tracer, fline, lic_ctls%fname_lic_ctl, &
     &    lic_ctls%pvr_ctl_type, lic_ctls%lic_ctl_type, lic%lic_param,  &
     &    lic%rep_ref, lic%pvr, lic%flag_each_repart)
!
      call count_num_rendering_and_images                               &
     &   (lic%pvr%num_pvr, lic%pvr%pvr_param,                           &
     &    lic%pvr%num_pvr_images, lic%pvr%PVR_sort%istack_pvr_images)
      call alloc_pvr_images(lic%pvr)
!
      call set_rendering_and_image_pes(nprocs,                          &
     &    lic%pvr%num_pvr, lic_ctls%pvr_ctl_type, lic%pvr%PVR_sort,     &
     &    lic%pvr%num_pvr_images, lic%pvr%pvr_rgb)
!
      call LIC_init_nodal_field(geofem, lic%pvr%num_pvr, lic%lic_param, &
     &                          lic%repart_data)
!
      do i_lic = 1, lic%pvr%num_pvr
        if(lic%lic_param(i_lic)%each_part_p%iflag_repart_ref            &
     &                                   .eq. i_INT_COUNT_BASED) then
          call init_lic_repart_ref(geofem%mesh, lic%pvr%pvr_rgb(i_lic), &
     &        lic%lic_param(i_lic)%each_part_p, lic%rep_ref(i_lic))
        end if
      end do
!
      do i_lic = 1, lic%pvr%num_pvr
        if((no_file_flag(lic_ctls%fname_lic_ctl(i_lic)) .eqv. .FALSE.)  &
     &      .or. my_rank .ne. 0) then
          call dealloc_lic_count_data(lic_ctls%pvr_ctl_type(i_lic),     &
     &        lic_ctls%lic_ctl_type(i_lic))
        end if
      end do
      call dealloc_sort_PVRs_list(lic%pvr%PVR_sort)
!
      do i_lic = 1, lic%pvr%num_pvr
        ist_img = lic%pvr%PVR_sort%istack_pvr_images(i_lic-1)
        num_img = lic%pvr%PVR_sort%istack_pvr_images(i_lic  ) - ist_img
        call init_each_PVR_image(num_img, lic%pvr%pvr_param(i_lic),     &
     &                           lic%pvr%pvr_rgb(ist_img+1))
      end do
!
      if(lic%flag_each_repart) return
      if(lic%repart_p%iflag_repart_ref .eq. i_INT_COUNT_BASED) then
        call init_lic_repart_ref(geofem%mesh, lic%pvr%pvr_rgb(1),       &
     &                           lic%repart_p, lic%rep_ref_m)
      end if
!
      call LIC_initialize_w_shared_mesh(geofem, ele_comm,  next_tbl,    &
     &    lic%repart_p, lic%rep_ref_m, lic%repart_data, lic%pvr, m_SR)
!
      end subroutine LIC_initialize_test
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_visualize_test(istep_lic, time, geofem, ele_comm,  &
     &                              next_tbl, nod_fld, lic, m_SR)
!
      use m_elapsed_labels_4_VIZ
      use select_LIC_rendering
!
      integer(kind = kint), intent(in) :: istep_lic
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: geofem
      type(communication_table), intent(in) :: ele_comm
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(phys_data), intent(in) :: nod_fld
!
      type(lic_volume_rendering_module), intent(inout) :: lic
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(lic%pvr%num_pvr.le.0 .or. istep_lic.le.0) return
!
      if(lic%flag_each_repart) then
        call LIC_visualize_w_each_repart(istep_lic, time,               &
     &      geofem, ele_comm, next_tbl, nod_fld, lic%repart_p,          &
     &      lic%rep_ref_m, lic%repart_data, lic%pvr, lic%lic_param,     &
     &      lic%rep_ref, m_SR)
      else
        call LIC_visualize_w_shared_mesh                                &
     &     (istep_lic, time, geofem, nod_fld, lic%repart_p,             &
     &      lic%repart_data, lic%pvr, lic%lic_param, m_SR)
      end if
!
      end subroutine LIC_visualize_test
!
!  ---------------------------------------------------------------------
!
      end module lic_rendering_test
