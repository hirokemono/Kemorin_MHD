!>@file   analyzer_sph_modify_restart.f90
!!@brief  module analyzer_sph_modify_restart
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to modifity spectr data for new simulation
!!
!!@verbatim
!!      subroutine evolution_sph_mod_restart
!!@endverbatim
!
      module analyzer_sph_modify_restart
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_MHD_step_parameter
      use t_MHD_step_parameter
      use t_step_parameter
      use t_MHD_file_parameter
!
      implicit none
!
      private :: SPH_analyze_mod_restart
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mod_restart
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_snap
!
      integer(kind = kint) :: iflag
!
!*  -----------  set initial step data --------------
!*
      call start_eleps_time(3)
      call s_initialize_time_step(MHD_step1%init_d, MHD_step1%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(MHD_step1%time_d)
!
        iflag = output_IO_flag(MHD_step1%time_d%i_time_step,            &
     &                         MHD_step1%rst_step)
        if(iflag .ne. 0) cycle
!
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_mod_restart'
        call SPH_analyze_mod_restart(MHD_step1%time_d%i_time_step,      &
     &      MHD_files1%fst_file_IO, MHD_files1, MHD_step1)
!*
!*  -----------  output field data --------------
!*
        if(MHD_step1%time_d%i_time_step                                 &
     &        .ge. MHD_step1%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      call end_eleps_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_files1%ucd_file_IO, MHD_step1)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
!      call SPH_finalize_snap
!
      call copy_COMM_TIME_to_elaps(num_elapsed)
      call end_eleps_time(1)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_mod_restart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_mod_restart                                &
     &         (i_step, fst_file_IO, MHD_files, MHD_step)
!
      use m_work_time
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_boundary_data_sph_MHD
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use set_sph_restart_IO
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
      use input_control_sph_MHD
!
      integer(kind = kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: fst_file_IO
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(MHD_step_param), intent(inout) :: MHD_step
!
      integer(kind = kint) :: iflag
!
!
      MHD_files%org_rst_file_IO%iflag_format                            &
     &    = fst_file_IO%iflag_format
      call read_alloc_sph_rst_4_snap(i_step,                            &
     &    MHD_files%org_rj_file_IO, MHD_files%org_rst_file_IO,          &
     &    sph1%sph_rj, ipol, rj_fld1, MHD_step%rst_step,                &
     &    MHD_step%init_d)
!
!*  ----------------Modify spectr data ... ----------
!*
      call set_modify_rj_fields
!
      if(iflag_debug.gt.0) write(*,*) 'output_sph_restart_control'
      call copy_time_step_data(MHD_step%init_d, MHD_step%time_d)
      call init_output_sph_restart_file(rj_fld1)
!
      iflag = set_IO_step_flag(MHD_step%time_d%i_time_step,             &
     &                         MHD_step%rst_step)
      if(iflag .eq. 0) then
        call output_sph_restart_control                                 &
     &     (fst_file_IO, MHD_step%time_d, rj_fld1, MHD_step%rst_step)
      end if
!*
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(11)
      iflag = output_IO_flag(MHD_step%time_d%i_time_step,               &
     &                       MHD_step%rms_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
        call output_rms_sph_mhd_control                                 &
     &     (MHD_step%time_d, sph1%sph_params, sph1%sph_rj,              &
     &      sph_MHD_bc1%sph_bc_U, trans_p1%leg, ipol, rj_fld1,          &
     &      pwr1, WK_pwr)
      end if
      call end_eleps_time(11)
      call end_eleps_time(4)
!
      end subroutine SPH_analyze_mod_restart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_modify_rj_fields
!
      use m_spheric_parameter
      use m_sph_spectr_data
!
      use cal_zonal_mean_sph_spectr
!
      integer (kind =kint), allocatable :: ipick_degree(:)
      integer(kind = kint) :: ltr_half
!
!
      ltr_half = 1
      allocate(ipick_degree(ltr_half))
      ipick_degree(1) = 0
!
      if (my_rank.eq.0) write(*,*) 'Take zonam mean of light element'
      call take_zonal_mean_rj_field                                     &
     &   (ione, ipol%i_light, sph1%sph_rj, rj_fld1)
!      if (my_rank.eq.0) write(*,*) 'Take sphere average of light element'
!      call pick_degree_sph_spectr(ltr_half, ipick_degree,               &
!     &    ione, ipol%i_light, sph1%sph_rj, rj_fld1)
!
      end subroutine set_modify_rj_fields
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_modify_restart
