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
      use m_t_step_parameter
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_snap
!
      integer(kind = kint) :: iflag
!
!*  -----------  set initial step data --------------
!*
      call start_eleps_time(3)
      call s_initialize_time_step(MHD_step1%init_d, time_d1)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call increment_step(time_d1)
!
        iflag = output_IO_flag(time_d1%i_time_step,MHD_step1%rst_step)
        if(iflag .ne. 0) cycle
!
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_mod_restart'
        call SPH_analyze_mod_restart(time_d1%i_time_step, MHD_step1)
!*
!*  -----------  output field data --------------
!*
        if(time_d1%i_time_step .ge. MHD_step1%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      call end_eleps_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_step1)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
!      call SPH_finalize_snap
!
      call copy_COMM_TIME_to_eleps(num_elapsed)
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
      subroutine SPH_analyze_mod_restart(i_step, MHD_step)
!
      use m_work_time
      use m_t_step_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
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
      type(MHD_IO_step_param), intent(inout) :: MHD_step
!
      integer(kind = kint) :: iflag
!
!
      call read_alloc_sph_rst_2_modify(i_step,                          &
     &    MHD1_org_files%rj_file_param, MHD1_org_files%rst_file_param,  &
     &    sph1%sph_rj, ipol, rj_fld1, MHD_step%rst_step,                &
     &    MHD_step%init_d)
!
!*  ----------------Modify spectr data ... ----------
!*
      call set_modify_rj_fields
!
      if(iflag_debug.gt.0) write(*,*) 'output_sph_restart_control'
      call copy_time_step_data(MHD_step1%init_d, time_d1)
      call init_output_sph_restart_file(rj_fld1)
      call output_sph_restart_control                                   &
     &   (time_d1, rj_fld1, MHD_step%rst_step)
!*
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(11)
      iflag = output_IO_flag(time_d1%i_time_step, MHD_step%rms_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
        call output_rms_sph_mhd_control                                 &
     &     (time_d1, sph1%sph_params, sph1%sph_rj, trans_p1%leg,        &
     &      ipol, rj_fld1, pwr1, WK_pwr)
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
