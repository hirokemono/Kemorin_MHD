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
      use m_physical_property
      use m_mesh_data
      use m_mean_square_values
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
      use m_control_parameter
      use m_bc_data_list
      use m_SGS_control_parameter
      use m_node_phys_data
      use m_3d_filter_coef_MHD
      use m_boundary_field_IO
      use m_MHD_step_parameter
      use m_solver_djds_MHD
      use m_flexible_time_step
      use input_control
!
!
        write(*,*) 'Simulation start: PE. ', my_rank
!
!     --------------------- 
!
      call input_control_4_FEM_snap                                     &
     &   (MHD_files1, FEM_prm1, SGS_par1, flex_p1, MHD_step1,           &
     &    MHD_prop1, MHD_BC1, femmesh1, ele_mesh1, nod_fld1,            &
     &    bc_FEM_IO1, filtering1, wide_filtering, wk_filter1,           &
     &    MHD_CG1%MGCG_WK, MHD_CG1%MGCG_FEM, MHD_CG1%MGCG_MHD_FEM)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'FEM_initialize_vol_average'
      call FEM_initialize_vol_average                                   &
     &   (MHD_files1, bc_FEM_IO1, MHD_step1,                            &
     &    femmesh1, ele_mesh1, fem_sq1)
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
     &     (i_step, MHD_files1, femmesh1, MHD_step1, fem_sq1)
      end do
!
!      call FEM_finalize_vol_average
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_volume_ave
