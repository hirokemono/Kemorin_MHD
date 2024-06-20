!>@file   t_LIC_visualizer.f90
!!@brief  module t_LIC_visualizer
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module to access LIC visualization programs
!!
!!@verbatim
!!      subroutine init_LIC_visualize(elps_VIZ, viz_step,               &
!!     &          geofem, nod_fld, VIZ_DAT, viz_ctls, lic_v, m_SR)
!!      subroutine visualize_LIC(elps_VIZ, viz_step, time_d,            &
!!     &                         geofem, nod_fld, VIZ_DAT, lic_v, m_SR)
!!        type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!        type(lic_visualize_modules), intent(inout) :: lic_v
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module t_LIC_visualizer
!
      use m_precision
!
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_elapsed_labels_4_VIZ
      use t_VIZ_step_parameter
      use t_VIZ_mesh_field
      use t_time_data
      use t_mesh_data
      use t_comm_table
      use t_phys_data
      use t_next_node_ele_4_node
      use t_jacobians
!
      use t_control_data_vizs
      use t_lic_rendering
      use t_mesh_SR
!
      implicit  none
!
      type lic_visualize_modules
        type(lic_volume_rendering_module) :: lic
      end type lic_visualize_modules
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_LIC_visualize(elps_VIZ, viz_step,                 &
     &          geofem, nod_fld, VIZ_DAT, viz_ctls, lic_v, m_SR)
!
      use t_fem_gauss_int_coefs
      use t_shape_functions
      use t_jacobians
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!
      type(visualization_controls), intent(inout) :: viz_ctls
      type(lic_visualize_modules), intent(inout) :: lic_v
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+9)
      call LIC_initialize(viz_step%LIC_t%increment,                     &
     &    elps_VIZ%elps_PVR, elps_VIZ%elps_LIC,                         &
     &    geofem, VIZ_DAT%ele_comm, VIZ_DAT%next_tbl, nod_fld,          &
     &    viz_ctls%repart_ctl, viz_ctls%lic_ctls, lic_v%lic, m_SR)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+9)
!
      call calypso_mpi_barrier
      call dealloc_viz_controls(viz_ctls)
!
      end subroutine init_LIC_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_LIC(elps_VIZ, viz_step, time_d,              &
     &                         geofem, nod_fld, VIZ_DAT, lic_v, m_SR)
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(time_data), intent(in) :: time_d
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: geofem
!!        type(communication_table), intent(in) :: ele_comm
      type(phys_data), intent(in) :: nod_fld
      type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!
      type(lic_visualize_modules), intent(inout) :: lic_v
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+10)
      call LIC_visualize(viz_step%istep_lic, time_d%time,               &
     &    elps_VIZ%elps_PVR, elps_VIZ%elps_LIC,                         &
     &    geofem, VIZ_DAT%ele_comm, VIZ_DAT%next_tbl, nod_fld,          &
     &    lic_v%lic, m_SR)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+10)
!
      call calypso_mpi_barrier
!
      end subroutine visualize_LIC
!
!  ---------------------------------------------------------------------
!
      end module t_LIC_visualizer
