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
      use m_physical_property
      use m_3d_filter_coef_MHD
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
      use m_control_parameter
      use m_bc_data_list
      use m_MHD_step_parameter
      use m_mesh_data
      use m_node_phys_data
      use m_boundary_field_IO
      use m_solver_djds_MHD
      use m_flexible_time_step
      use m_mean_square_values
      use input_control
!
!
        write(*,*) 'Simulation start: PE. ', my_rank
!
      call input_control_4_FEM_MHD                                      &
     &   (MHD_files1, FEM_prm1, FEM_SGS1%SGS_par, flex_p1, MHD_step1,   &
     &    MHD_prop1, MHD_BC1, femmesh1, ele_mesh1, nod_fld1,            &
     &    SGS_MHD_wk1%ele_fld, bc_FEM_IO1, FEM_SGS1%FEM_filters,        &
     &    SGS_MHD_wk1%FEM_SGS_wk, MHD_CG1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
!
      call FEM_check_MHD_mat                                            &
     &   (MHD_files1, bc_FEM_IO1, flex_p1, flex_data1, MHD_step1,       &
     &    femmesh1, ele_mesh1, iphys_nod1, nod_fld1,                    &
     &    MHD_CG1, FEM_SGS1, SGS_MHD_wk1, fem_sq1, label_sim)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      end module analyzer_check_mat_MHD
