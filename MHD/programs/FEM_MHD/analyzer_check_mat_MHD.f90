!analyzer_check_mat_MHD.f90
!      module analyzer_check_mat_MHD
!..................................................
!
!      Written by H. Matsui and H. Okuda
!      modified by H. Matsui on June, 2005 
!
      module analyzer_check_mat_MHD
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_FEM_SGS_MHD
      use t_control_data_tracers
      use FEM_check_MHD_matrices
      use t_control_data_tracers
      use t_control_data_vizs
!
      implicit none
!
!
      type(FEM_MHD), save, private :: FMHDs
      type(FEM_SGS_MHD), save, private ::  FSGSs
      type(tracers_control), save, private :: tracer_ctls_F
      type(visualization_controls), save, private :: vizs_ctl_F
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
      call input_control_4_FEM_MHD                                      &
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
      call FEM_check_MHD_mat                                            &
     &   (FMHDs%MHD_files, FMHDs%flex_MHD, FMHDs%MHD_step,              &
     &    FMHDs%FEM_model, FMHDs%MHD_CG, FMHDs%FEM_MHD,                 &
     &    FSGSs%FEM_SGS, FSGSs%SGS_MHD_wk, FMHDs%MHD_IO,                &
     &    FMHDs%fem_sq, FMHDs%m_SR)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      end module analyzer_check_mat_MHD
