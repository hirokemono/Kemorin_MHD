!>@file   t_LIC_visualizer.f90
!!@brief  module t_LIC_visualizer
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module to access LIC visualization programs
!!
!!@verbatim
!!      subroutine init_LIC_visualize(viz_step, geofem, nod_fld,        &
!!     &          VIZ_DAT, viz_ctls, lic_v, m_SR)
!!      subroutine visualize_LIC(viz_step, time_d, geofem, nod_fld,     &
!!     &          VIZ_DAT, lic_v, m_SR)
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
      use m_elapsed_labels_4_VIZ
      use calypso_mpi
!
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
      subroutine init_LIC_visualize(viz_step, geofem, nod_fld,          &
     &                              VIZ_DAT, viz_ctls, lic_v, m_SR)
!
      use t_fem_gauss_int_coefs
      use t_shape_functions
      use t_jacobians
!
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
      call LIC_initialize                                               &
     &   (viz_step%LIC_t%increment, elps_PVR1, elps_LIC1,               &
     &    geofem, VIZ_DAT%ele_comm, VIZ_DAT%next_tbl, nod_fld,          &
     &    viz_ctls%repart_ctl, viz_ctls%lic_ctls, lic_v%lic, m_SR)
!
      call calypso_mpi_barrier
      call dealloc_viz_controls(viz_ctls)
!
      end subroutine init_LIC_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_LIC(viz_step, time_d, geofem, nod_fld,       &
     &                         VIZ_DAT, lic_v, m_SR)
!
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
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+10)
      call LIC_visualize                                                &
     &   (viz_step%istep_lic, time_d%time, elps_PVR1, elps_LIC1,        &
     &    geofem, VIZ_DAT%ele_comm, VIZ_DAT%next_tbl, nod_fld,          &
     &    lic_v%lic, m_SR)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+10)
!
      call calypso_mpi_barrier
!
      end subroutine visualize_LIC
!
!  ---------------------------------------------------------------------
!
      end module t_LIC_visualizer
