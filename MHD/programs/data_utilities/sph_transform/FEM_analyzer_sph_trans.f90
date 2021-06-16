!FEM_analyzer_sph_trans.f90
!
!!      subroutine FEM_initialize_sph_trans(init_d, ucd_step, FEM_STR)
!!      subroutine FEM_analyze_sph_trans(i_step, ucd_step, FEM_STR)
!!      subroutine SPH_to_FEM_bridge_sph_trans                          &
!!     &         (udt_file_param, rj_fld, fld_IO)
!!      subroutine FEM_finalize_sph_trans(ucd_step, FEM_STR)
!!        type(time_data), intent(in) :: init_d
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(field_IO), intent(inout) :: fld_IO
!!        type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
!
      module FEM_analyzer_sph_trans
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_FEM_data_4_SPH_trans
      use t_ucd_data
      use t_file_IO_parameter
      use t_time_data
      use m_solver_SR
!
      implicit none
!
!>        Instance for FEM field data IO
      type(ucd_data), save :: input_ucd
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_sph_trans(init_d, ucd_step, FEM_STR)
!
      use nod_phys_send_recv
      use int_volume_of_domain
      use set_normal_vectors
      use sum_normal_4_surf_group
      use output_parallel_ucd_file
      use parallel_ucd_IO_select
      use const_mesh_information
      use const_element_comm_tables
!
      type(time_data), intent(in) :: init_d
      type(IO_step_param), intent(in) :: ucd_step
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
!
      integer(kind = kint) :: istep_ucd
!
!  -----    construct geometry informations
!
      call mesh_setup_4_SPH_TRANS                                       &
     &   (FEM_STR, SR_sig1, SR_r1, SR_i1, SR_il1)
!
!  -------------------------------
!
      input_ucd%nnod = ione
      istep_ucd = IO_step_exc_zero_inc(init_d%i_time_step, ucd_step)
      call sel_read_parallel_udt_param                                  &
     &   (istep_ucd, FEM_STR%ucd_file_IO, FEM_STR%time_IO, input_ucd)
!
      end subroutine FEM_initialize_sph_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_sph_trans(i_step, ucd_step, FEM_STR)
!
      use t_ctl_params_sph_trans
      use output_parallel_ucd_file
      use nod_phys_send_recv
!
      integer(kind =kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: ucd_step
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
!
      integer(kind = kint) :: visval
      integer(kind = kint) :: istep_ucd
!
!*  ----------   Count steps for visualization
      visval = mod(i_step,ucd_step%increment)
      if(visval .ne. 0) return 
!
!*  -----------  Output volume data --------------
      istep_ucd = IO_step_exc_zero_inc(i_step, ucd_step)
      call set_data_by_read_ucd(i_step, FEM_STR%org_ucd_file_IO,        &
     &    FEM_STR%time_IO, input_ucd, FEM_STR%field)
      call nod_fields_send_recv(FEM_STR%geofem%mesh, FEM_STR%field,     &
     &                          FEM_STR%v_sol, SR_sig1, SR_r1)
!
      end subroutine FEM_analyze_sph_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_sph_trans(rj_fld, fld_IO)
!
      use t_field_data_IO
      use t_phys_data
      use copy_rj_phys_data_4_IO
!
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
!
!
      if (iflag_debug.gt.0) write(*,*) 'copy_rj_all_phys_name_to_IO'
      call copy_rj_phys_name_to_IO(rj_fld%num_phys, rj_fld, fld_IO)
      call alloc_phys_data_IO(fld_IO)
      call alloc_merged_field_stack(nprocs, fld_IO)
!
      end subroutine SPH_to_FEM_bridge_sph_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_finalize_sph_trans(ucd_step, FEM_STR)
!
      use output_parallel_ucd_file
!
      type(IO_step_param), intent(in) :: ucd_step
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
!
!
      if(ucd_step%increment .gt. 0) then
        call finalize_ucd_file_output                                   &
     &     (FEM_STR%org_ucd_file_IO, FEM_STR%ucd)
      end if
!
      end subroutine FEM_finalize_sph_trans
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_trans
