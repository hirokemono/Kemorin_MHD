!analyzer_volume_ave.f90
!      module analyzer_volume_ave
!
!..................................................
!
!      Written by H. Matsui on Dec., 2007
!
      module analyzer_volume_ave
!
      use m_precision
      use m_machine_parameter
      use FEM_analyzer_vol_average
      use t_FEM_SGS_MHD
      use t_control_data_tracers
      use t_control_data_vizs
!
      implicit none
!
      type(FEM_MHD), save, private :: FMHDs
      type(FEM_SGS_MHD), save, private ::  FSGSs
      type(visualization_controls), save, private :: vizs_ctl_F
      type(tracers_control), save, private :: tracer_ctls_F
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use input_control
!
!
        write(*,*) 'Simulation start: PE. ', my_rank
!
!     --------------------- 
!
      call input_control_4_FEM_snap                                     &
     &   (FMHDs%MHD_files, FMHDs%FEM_model%FEM_prm,                     &
     &    FSGSs%FEM_SGS%SGS_par, FMHDs%MHD_step,                        &
     &    FMHDs%FEM_model%MHD_prop, FMHDs%FEM_model%MHD_BC,             &
     &    FMHDs%FEM_MHD%geofem, FMHDs%FEM_MHD%field,                    &
     &    FSGSs%SGS_MHD_wk%ele_fld, FMHDs%FEM_MHD%nod_mntr,             &
     &    FMHDs%FEM_model%bc_FEM_IO, FSGSs%FEM_SGS%FEM_filters,         &
     &    FSGSs%SGS_MHD_wk%FEM_SGS_wk, FMHDs%MHD_CG,                    &
     &    tracer_ctls_F, vizs_ctl_F)
      call copy_delta_t(FMHDs%MHD_step%init_d, FMHDs%MHD_step%time_d)
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'FEM_initialize_vol_average'
      call FEM_initialize_vol_average                                   &
     &   (FMHDs%MHD_files, FMHDs%MHD_step, FMHDs%FEM_model,             &
     &    FMHDs%MHD_CG%ak_MHD, FMHDs%FEM_MHD,                           &
     &    FSGSs%FEM_SGS, FSGSs%SGS_MHD_wk, FMHDs%MHD_IO,                &
     &    FMHDs%fem_sq, FMHDs%m_SR)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
!
      integer(kind=kint ) :: i_step
!
      do i_step = FMHDs%MHD_step%init_d%i_time_step,                    &
     &           FMHDs%MHD_step%finish_d%i_end_step
        if (iflag_debug.eq.1)  write(*,*) 'FEM_analyze_vol_average'
        call FEM_analyze_vol_average                                    &
     &     (i_step, FMHDs%MHD_files, FSGSs%FEM_SGS%iphys_LES,           &
     &      FMHDs%FEM_model, FMHDs%MHD_step, FSGSs%SGS_MHD_wk,          &
     &      FMHDs%FEM_MHD, FMHDs%fem_sq, FMHDs%m_SR)
      end do
!
!      call FEM_finalize_vol_average
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_volume_ave
