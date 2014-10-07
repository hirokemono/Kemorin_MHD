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
      use m_control_parameter
      use m_t_int_parameter
      use m_t_step_parameter
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_snap
!
!     ---------------------
!
      call start_eleps_time(3)
!
!*  -----------  set initial step data --------------
!*
      i_step_MHD = i_step_init - 1
!*
!*  -------  time evelution loop start -----------
!*
      do
        i_step_MHD = i_step_MHD + 1
        istep_max_dt = i_step_MHD
!
        if( mod(i_step_MHD,i_step_output_rst) .ne. 0) cycle
!
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_mod_restart'
        call SPH_analyze_mod_restart(i_step_MHD)
!*
!*  -----------  output field data --------------
!*
        if(i_step_MHD .ge. i_step_number) exit
      end do
!
!  time evolution end
!
      call end_eleps_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize
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
      subroutine SPH_analyze_mod_restart(i_step)
!
      use m_work_time
      use m_t_step_parameter
      use m_node_id_spherical_IO
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
      use m_field_data_IO
      use m_control_params_2nd_files
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use set_sph_restart_IO
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
!
      integer(kind = kint), intent(in) :: i_step
      character(len=kchara) :: restart_tmp_prefix
!
!
      restart_tmp_prefix = phys_file_head
      phys_file_head =     org_rst_header
      call read_alloc_sph_rst_4_snap(i_step)
      phys_file_head =     restart_tmp_prefix
!
!*  ----------------Modify spectr data ... ----------
!*
      call set_modify_rj_fields
!
      if(iflag_debug.gt.0) write(*,*) 'output_sph_restart_control'
      call set_sph_restart_num_to_IO
      call output_sph_restart_control
      call deallocate_phys_data_IO
      call deallocate_phys_data_name_IO
!*
      if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
      call output_rms_sph_mhd_control
!
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(11)
      if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
      call output_rms_sph_mhd_control
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
      use m_sph_phys_address
      use m_spheric_parameter
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
      call take_zonal_mean_rj_field(ione, ipol%i_light)
!      if (my_rank.eq.0) write(*,*) 'Take sphere average of light element'
!      call pick_degree_sph_spectr(ltr_half, ipick_degree,               &
!     &    ione, ipol%i_light)
!
      end subroutine set_modify_rj_fields
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_modify_restart
