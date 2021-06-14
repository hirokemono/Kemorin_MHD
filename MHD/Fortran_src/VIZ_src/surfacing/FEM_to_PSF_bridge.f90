!>@file   FEM_to_PSF_bridge.f90
!!@brief  module FEM_to_PSF_bridge
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Data structuresa for sectioning module
!!
!!@verbatim
!!      subroutine init_FEM_to_PSF_bridge(viz_step, geofem, edge_comm,  &
!!     &                                  SR_sig, SR_r, SR_i, SR_il)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(mesh_data), intent(inout) :: geofem
!!        type(communication_table), intent(inout) :: edge_comm
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!@endverbatim
!
      module FEM_to_PSF_bridge
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_phys_data
      use t_comm_table
      use t_VIZ_step_parameter
      use t_solver_SR
      use t_solver_SR_int
      use t_solver_SR_int8
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_FEM_to_PSF_bridge(viz_step, geofem, edge_comm,    &
     &                                  SR_sig, SR_r, SR_i, SR_il)
!
      use parallel_FEM_mesh_init
      use const_element_comm_tables
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(inout) :: geofem
      type(communication_table), intent(inout) :: edge_comm
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      integer(kind = kint) :: iflag
!
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(geofem%mesh, geofem%group,           &
     &                             SR_sig, SR_i)
!
!     --------------------- init for sectioning
!
      iflag = viz_step%PSF_t%increment + viz_step%ISO_t%increment
      if(iflag .gt. 0) then
        if(iflag_debug .gt. 0) write(*,*) 'const_edge_comm_table'
        call const_edge_comm_table                                      &
     &     (geofem%mesh%node, geofem%mesh%nod_comm,                     &
     &      edge_comm, geofem%mesh%edge, SR_sig, SR_r, SR_i, SR_il)
      end if
!
      end subroutine init_FEM_to_PSF_bridge
!
! ----------------------------------------------------------------------
!
      end module FEM_to_PSF_bridge
