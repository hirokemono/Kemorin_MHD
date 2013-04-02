!analyzer_zm_sph_field.f90
!      module analyzer_zm_sph_field
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine initialize
!      subroutine analyze
!
      module analyzer_zm_sph_field
!
      use m_precision
      use m_parallel_var_dof
!
      use SPH_analyzer_sph_trans
      use SPH_analyzer_back_trans
      use FEM_analyzer_sph_trans
      use FEM_analyzer_back_trans
      use visualizer_all
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize
!
      use m_ctl_data_4_sph_trans
      use m_ctl_params_sph_trans
!
!
!     ---------------------
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans
      call set_ctl_data_4_pick_zm
!
      call time_prog_barrier
!
!    Initialize FEM grid
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans
!
!    Initialization for spherical tranform
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_sph_trans'
      call SPH_initialize_sph_trans
!
!    Set field IOP array by spectr fields
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_sph_trans'
      call SPH_to_FEM_bridge_sph_trans
!
!  -------------------------------
!
      call init_visualize(ierr)
!
      end subroutine initialize
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_ucd_data
      use m_t_step_parameter
      use m_ctl_params_sph_trans
      use sph_rtp_zonal_rms_data
!
      integer(kind=kint ) :: visval, i_step
      integer(kind=kint ) :: istep_psf, istep_iso
      integer(kind=kint ) :: istep_pvr, istep_fline
!
!
      do i_step = i_step_init, i_step_number
!
!   Input field data
        ucd_header_name = org_ucd_header
        call FEM_analyze_sph_trans(i_step, visval)
!
!   Transfer coordinate
        call FEM_to_SPH_bridge_sph_trans(visval)
!
!   Take zonal RMS
        if (iflag_debug.gt.0) write(*,*) 'cal_sph_zonal_ave_data'
        call cal_sph_zonal_ave_data
!
        ucd_header_name = zonal_udt_head
        call FEM_analyze_back_trans(i_step, istep_psf, istep_iso,       &
     &          istep_pvr, istep_fline, visval)
!
        if(visval .eq. 0) then
          call visualize_all(istep_psf, istep_iso,                      &
     &        istep_pvr, istep_fline, ierr)
        end if
      end do
!
      call FEM_finalize_sph_trans
!
        end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_zm_sph_field
