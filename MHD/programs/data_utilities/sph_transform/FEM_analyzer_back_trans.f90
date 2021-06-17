!FEM_analyzer_back_trans.f90
!
!      module FEM_analyzer_back_trans
!
!      Written by H. Matsui
!
!!      subroutine FEM_initialize_back_trans(ucd_step, FEM_STR,         &
!!     &          SR_sig, SR_r, SR_i, SR_il)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!      subroutine FEM_analyze_back_trans                               &
!!     &         (i_step, ucd_step, visval, FEM_STR, SR_sig, SR_r)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module FEM_analyzer_back_trans
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_FEM_data_4_SPH_trans
      use t_VIZ_step_parameter
      use t_file_IO_parameter
      use t_shape_functions
      use t_solver_SR
      use t_solver_SR_int
      use t_solver_SR_int8
!
      implicit none
!
      type(shape_finctions_at_points), save, private :: spfs_TRNS
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_back_trans(ucd_step, FEM_STR,           &
     &          SR_sig, SR_r, SR_i, SR_il)
!
      use t_ucd_data
      use t_next_node_ele_4_node
      use t_ctl_params_sph_trans
      use t_jacobians
!
      use nod_phys_send_recv
      use int_volume_of_domain
      use set_normal_vectors
      use output_parallel_ucd_file
      use const_mesh_information
      use const_element_comm_tables
!
      type(IO_step_param), intent(in) :: ucd_step
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
!
!  -----    construct geometry informations
!
      if (iflag_debug.gt.0) write(*,*) 'FEM_comm_initialization'
      call FEM_comm_initialization(FEM_STR%geofem%mesh, FEM_STR%v_sol,  &
     &                             SR_sig, SR_r, SR_i, SR_il)
!
      if (iflag_debug.gt.0) write(*,*) 'alloc_phys_data'
      call alloc_phys_data(FEM_STR%geofem%mesh%node%numnod,             &
     &                     FEM_STR%field)
!
!  connect grid data to volume output
!
      if(ucd_step%increment .eq. 0) return
      call link_output_grd_file                                         &
     &   (FEM_STR%geofem%mesh%node, FEM_STR%geofem%mesh%ele,            &
     &    FEM_STR%geofem%mesh%nod_comm, FEM_STR%field,                  &
     &    FEM_STR%ucd_file_IO, FEM_STR%ucd, SR_sig, SR_i)
!
      end subroutine FEM_initialize_back_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_back_trans                                 &
     &         (i_step, ucd_step, visval, FEM_STR, SR_sig, SR_r)
!
      use t_ctl_params_sph_trans
      use t_time_data
      use t_ucd_data
      use t_IO_step_parameter
      use field_IO_select
      use parallel_ucd_IO_select
      use nod_phys_send_recv
!
      logical, intent(in) :: visval
      integer(kind = kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: ucd_step
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!*  ----------   Count steps for visualization
!*
      if(visval) then
        call nod_fields_send_recv(FEM_STR%geofem%mesh, FEM_STR%field,   &
     &                            FEM_STR%v_sol, SR_sig, SR_r)
!
!*  -----------  Output volume data --------------
!*
        if(output_IO_flag(i_step,ucd_step)) then
          call sel_write_parallel_ucd_file(i_step, FEM_STR%ucd_file_IO, &
     &        FEM_STR%time_IO, FEM_STR%ucd)
        end if
      end if
!
      end subroutine FEM_analyze_back_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!      subroutine SPH_to_FEM_bridge_back_trans(visval)
!
!
!      end subroutine SPH_to_FEM_bridge_back_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!      subroutine FEM_to_SPH_bridge
!
!
!      end subroutine FEM_to_SPH_bridge
!
!-----------------------------------------------------------------------
!
!      subroutine FEM_finalize_back_trans
!
!
!      end subroutine FEM_finalize_back_trans
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_back_trans
