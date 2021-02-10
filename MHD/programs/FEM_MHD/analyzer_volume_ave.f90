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
      use m_MHD_step_parameter
      use m_FEM_MHD_model_data
      use FEM_analyzer_vol_average
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use m_MHD_step_parameter
      use input_control
!
!
        write(*,*) 'Simulation start: PE. ', my_rank
!
!     --------------------- 
!
      call alloc_FEM_mesh_field_items(FEM_MHD1)
      call input_control_4_FEM_snap(MHD_files1, FEM_model1%FEM_prm,     &
     &    FEM_SGS1%SGS_par, MHD_step1, FEM_model1%MHD_prop,             &
     &    FEM_model1%MHD_BC, FEM_MHD1%geofem,                           &
     &    FEM_MHD1%field, SGS_MHD_wk1%ele_fld, FEM_model1%bc_FEM_IO,    &
     &    FEM_SGS1%FEM_filters, SGS_MHD_wk1%FEM_SGS_wk, MHD_CG1,        &
     &    vizs_rprt_c_F%vizs_ctl, vizs_rprt_c_F%repart_ctl)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'FEM_initialize_vol_average'
      call FEM_initialize_vol_average                                   &
     &   (MHD_files1, MHD_step1, FEM_model1, MHD_CG1%ak_MHD,            &
     &    FEM_MHD1, FEM_SGS1, SGS_MHD_wk1, MHD_IO1, fem_sq1)
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
      do i_step = MHD_step1%init_d%i_time_step,                         &
     &           MHD_step1%finish_d%i_end_step
        if (iflag_debug.eq.1)  write(*,*) 'FEM_analyze_vol_average'
        call FEM_analyze_vol_average                                    &
     &     (i_step, MHD_files1, FEM_SGS1%iphys_LES, FEM_model1,         &
     &      MHD_step1, SGS_MHD_wk1, FEM_MHD1, fem_sq1)
      end do
!
!      call FEM_finalize_vol_average
      call dealloc_FEM_mesh_field_items(FEM_MHD1)
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_volume_ave
