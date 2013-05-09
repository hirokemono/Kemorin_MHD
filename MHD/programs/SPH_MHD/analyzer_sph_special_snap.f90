!>@file   analyzer_sph_special_snap.f90
!!@brief  module analyzer_sph_special_snap
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!        including some special treatment
!!
!!@verbatim
!!      subroutine evolution_sph_special_snap
!!@endverbatim
!
      module analyzer_sph_special_snap
!
      use m_precision
!
      use m_machine_parameter
      use m_parallel_var_dof
      use m_work_time
!
      implicit none
!
      private :: SPH_analyze_special_snap, lead_special_fields_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_special_snap
!
      use m_control_parameter
      use m_control_params_sph_MHD
      use m_t_int_parameter
      use m_t_step_parameter
!
      use const_coriolis_sph
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_snap
      use sections_for_1st
!
      integer(kind = kint) :: visval
      integer(kind = kint) :: istep_psf, istep_iso
      integer(kind = kint) :: istep_pvr, istep_fline
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
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_special_snap'
        call SPH_analyze_special_snap(i_step_MHD)
!*
!*  -----------  output field data --------------
!*
        call start_eleps_time(1)
        call start_eleps_time(4)
!
        if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge'
        call SPH_to_FEM_bridge
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze'
        call FEM_analyze(i_step_MHD, istep_psf, istep_iso,              &
     &      istep_pvr, istep_fline, visval)
!
        call end_eleps_time(4)
!
!*  ----------- Visualization --------------
!*
        if(visval .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_surface'
          call start_eleps_time(11)
          call visualize_surface(istep_psf, istep_iso, ierr)
          call end_eleps_time(11)
        end if
        call end_eleps_time(1)
!
!*  -----------  exit loop --------------
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
      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
      call SPH_finalize_snap
!
      call copy_COMM_TIME_to_eleps(num_elapsed)
      call end_eleps_time(1)
!
      call output_elapsed_times
!
      call time_prog_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_special_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_special_snap(i_step)
!
      use m_work_time
      use m_t_step_parameter
      use m_node_id_spherical_IO
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use set_reference_sph_mhd
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
!
      integer(kind = kint), intent(in) :: i_step
!
!
      call read_alloc_sph_rst_4_snap(i_step)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start
!
!*  ----------------Modify spectr data ... ----------
!*
!      call set_special_rj_fields
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_eleps_time(12)
      call nonlinear
      call end_eleps_time(12)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(4)
      call start_eleps_time(7)
!
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph
!*
!*  ---------------- Modify field data ... ----------
!*
      if(iflag_debug.gt.0) write(*,*) 'lead_special_fields_4_sph_mhd'
      call lead_special_fields_4_sph_mhd
      call end_eleps_time(7)
!
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(10)
      if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
      call output_rms_sph_mhd_control
      call end_eleps_time(10)
      call end_eleps_time(4)
!
!*  -----------  Output spectr data --------------
!*
      call output_spectr_4_snap(i_step)
!
      end subroutine SPH_analyze_special_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_special_rj_fields
!
      use m_sph_phys_address
!
      use cal_zonal_mean_sph_spectr
!
!
      call delete_rj_phys_data(itwo, ipol%i_velo)
      call take_zonal_mean_rj_field(ione, itor%i_velo)
!
      end subroutine set_special_rj_fields
!
! ----------------------------------------------------------------------
!
      subroutine lead_special_fields_4_sph_mhd
!
      use t_phys_address
      use m_sph_phys_address
      use output_viz_file_control
      use lead_fields_4_sph_mhd
!
      use cal_zonal_mean_sph_spectr
      use sph_rtp_zonal_rms_data
      use sph_transforms_4_MHD
      use products_sph_fields_smp
!
      integer (kind =kint) :: iflag
!
!
      call set_lead_physical_values_flag(iflag)
!
      if ( (iflag*mod(istep_max_dt,i_step_output_rst)) .eq.0 ) then
         call pressure_4_sph_mhd
      end if
!
      if(iflag .eq. 0) then
        call enegy_fluxes_4_sph_mhd
      end if
!
!
      call delete_rj_phys_data(ione, itor%i_magne)
      call take_zonal_mean_rj_field(itwo, ipol%i_magne)
      call sph_back_trans_4_MHD
!
!$omp parallel
      if((irtp%i_induction*irtp%i_me_gen) .gt. 0) then
       call cal_rtp_dot_product(irtp%i_induction, irtp%i_magne,        &
     &      irtp%i_me_gen)
      end if
!$omp end parallel
      call sph_forward_trans_snapshot_MHD
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_mean_all_sph_spectr'
      call zonal_mean_all_sph_spectr
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_mean_all_rtp_field'
      call zonal_mean_all_rtp_field
!
      end subroutine lead_special_fields_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_special_snap
