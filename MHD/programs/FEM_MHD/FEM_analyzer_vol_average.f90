!FEM_analyzer_vol_average.f90
!      module FEM_analyzer_vol_average
!
!      modified by H. Matsui on June, 2005 
!
!      subroutine FEM_initialize_snapshot
!      subroutine FEM_analyze_vol_average(i_step)
!
      module FEM_analyzer_vol_average
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use m_t_step_parameter
      use m_t_int_parameter
!
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine FEM_initialize_vol_average
!
      use m_control_parameter
!
      use load_mesh_data
      use input_control
      use initialize_4_snapshot
!
      use node_monitor_IO
      use open_sgs_model_coefs
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!  --  load FEM mesh data
!
      if (iflag_debug.eq.1) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
      if (iflag_debug.eq.1) write(*,*) 'input_meshes_4_MHD'
      call input_meshes_4_MHD
!
!     --------------------- 
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'init_analyzer_snap'
      call init_analyzer_snap
!
      end subroutine FEM_initialize_vol_average
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_vol_average(i_step)
!
      use m_control_parameter
!
      use read_udt_4_snapshot
!
      use nod_phys_send_recv
      use lead_physical_values
      use convert_temperatures
!
      use time_step_data_IO_control
      use output_parallel_ucd_file
!
      integer(kind=kint ), intent(in) :: i_step
!
!     ---- Load field data --- 
!
      if (my_rank.eq.0) write(*,*) 'step: ', i_step
!
      if (iflag_debug.eq.1)  write(*,*) 'read_udt_4_snap'
      call read_udt_4_snap(i_step)
      time = time_init + dt*dble(i_step)
!
!     ---- magnetic field update
!
      if (iflag_4_ref_temp .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1)  write(*,*) 'set_2_perturbation_temp'
        call set_2_perturbation_temp
      end if
!
!     ---------------------
!
      if (iflag_debug.eq.1)  write(*,*) 'phys_send_recv_all'
      call phys_send_recv_all
!
!     -----Output monitor date
!
      if (iflag_debug.eq.1) write(*,*) 'output_time_step_control'
      call output_time_step_control
!
      end subroutine FEM_analyze_vol_average
!
! ----------------------------------------------------------------------
!
!      subroutine FEM_finalize_vol_average
!
!      end subroutine FEM_finalize_vol_average
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_vol_average
