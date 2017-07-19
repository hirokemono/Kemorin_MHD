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
      use m_SGS_control_parameter
      use m_MHD_step_parameter
      use m_mesh_data
      use m_node_phys_data
      use m_3d_filter_coef_MHD
      use m_boundary_field_IO
      use m_solver_djds_MHD
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
      use input_control
!
!
        write(*,*) 'Simulation start: PE. ', my_rank
!
      call input_control_4_MHD                                          &
     &   (MHD_files1, FEM_prm1, SGS_par1, MHD_step1,                    &
     &    MHD_prop1, MHD_BC1, mesh1, group1, ele_mesh1, nod_fld1,       &
     &    IO_bc1, filtering1, wide_filtering, wk_filter1,               &
     &    MHD1_matrices, MGCG_WK1, MGCG_FEM1, MGCG_MHD_FEM1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
!
      call FEM_check_MHD_mat(MHD_files1)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      end module analyzer_check_mat_MHD
