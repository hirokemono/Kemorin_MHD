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
      use m_FEM_MHD_model_data
      use calypso_mpi
!
      use FEM_check_MHD_matrices
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
      call alloc_FEM_mesh_field_items(FEM_MHD1)
      call input_control_4_FEM_MHD                                      &
     &   (MHD_files1, FEM_model1%FEM_prm, FEM_SGS1%SGS_par, MHD_step1,  &
     &    FEM_model1%MHD_prop, FEM_model1%MHD_BC, FEM_MHD1%geofem,      &
     &    FEM_MHD1%field, SGS_MHD_wk1%ele_fld, FEM_model1%bc_FEM_IO,    &
     &    FEM_SGS1%FEM_filters, SGS_MHD_wk1%FEM_SGS_wk, MHD_CG1,        &
     &    vizs_rprt_c_F%vizs_ctl, vizs_rprt_c_F%repart_ctl)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
!
      call FEM_check_MHD_mat                                            &
     &   (MHD_files1, flex_MHD1, MHD_step1, FEM_model1, MHD_CG1,        &
     &    FEM_MHD1, FEM_SGS1, SGS_MHD_wk1, MHD_IO1, fem_sq1)
      call dealloc_FEM_mesh_field_items(FEM_MHD1)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      end module analyzer_check_mat_MHD
