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
      use m_SGS_control_parameter
      use m_mesh_data
      use m_node_phys_data
      use m_3d_filter_coef_MHD
      use m_boundary_field_IO
      use m_MHD_step_parameter
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
      use input_control
!
!
        write(*,*) 'Simulation start: PE. ', my_rank
!
!     --------------------- 
!
      call input_control_4_snapshot(FEM_prm1, SGS_par1, MHD_step1,      &
     &    iflag_scheme, MHD_prop1%fl_prop, MHD_prop1%cd_prop, MHD_prop1%ht_prop, MHD_prop1%cp_prop,         &
     &    MHD_prop1%ref_param_T, MHD_prop1%ref_param_C, MHD_prop1%takepito_T, MHD_prop1%takepito_C,         &
     &    mesh1, group1, ele_mesh1, nod_fld1, IO_bc1,                   &
     &    filtering1, wide_filtering, wk_filter1,                       &
     &    MGCG_WK1, MGCG_FEM1, MGCG_MHD_FEM1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'FEM_initialize_vol_average'
      call FEM_initialize_vol_average(MHD_step1)
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
      do i_step = MHD_step1%init_d%i_time_step, MHD_step1%finish_d%i_end_step
        if (iflag_debug.eq.1)  write(*,*) 'FEM_analyze_vol_average'
        call FEM_analyze_vol_average(i_step, MHD_step1)
      end do
!
!      call FEM_finalize_vol_average
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_volume_ave
