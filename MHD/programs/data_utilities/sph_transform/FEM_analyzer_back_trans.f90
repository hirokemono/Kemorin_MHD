!FEM_analyzer_back_trans.f90
!
!      module FEM_analyzer_back_trans
!
!      Written by H. Matsui
!
!!      subroutine FEM_initialize_back_trans(ucd_step, FEM_STR)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
!!      subroutine FEM_analyze_back_trans                               &
!!     &         (i_step, ucd_step, visval, FEM_STR)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
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
      subroutine FEM_initialize_back_trans(ucd_step, FEM_STR)
!
      use t_ucd_data
      use t_next_node_ele_4_node
      use t_ctl_params_sph_trans
      use t_jacobians
!
      use nod_phys_send_recv
      use int_volume_of_domain
      use set_normal_vectors
      use set_surf_grp_vectors
      use output_parallel_ucd_file
      use const_mesh_information
      use const_element_comm_tables
!
      type(IO_step_param), intent(in) :: ucd_step
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
!
!
!  -----    construct geometry informations
!
      if (iflag_debug.gt.0) write(*,*) 'FEM_comm_initialization'
      call FEM_comm_initialization                                      &
     &   (FEM_STR%geofem%mesh, FEM_STR%v_sol)
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
     &    FEM_STR%ucd_file_IO, FEM_STR%ucd)
!
      end subroutine FEM_initialize_back_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_back_trans                                 &
     &         (i_step, ucd_step, visval, FEM_STR)
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
!
!*  ----------   Count steps for visualization
!*
      if(visval) then
        call nod_fields_send_recv(FEM_STR%geofem%mesh,                  &
     &                            FEM_STR%field, FEM_STR%v_sol)
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
