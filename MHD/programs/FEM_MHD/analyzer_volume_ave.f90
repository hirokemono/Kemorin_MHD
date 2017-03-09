!analyzer_volume_ave.f90
!      module analyzer_volume_ave
!
!..................................................
!
      module analyzer_volume_ave
!
!      Written by H. Matsui on Dec., 2007
!
      use m_precision
      use m_machine_parameter
      use m_MHD_step_parameter
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
      use input_control
!
!
        write(*,*) 'Simulation start: PE. ', my_rank
!
!     --------------------- 
!
      call input_control_4_snapshot                                     &
     &   (FEM_prm1, SGS_par1, mesh1, group1, ele_mesh1, nod_fld1,       &
     &    IO_bc1, filtering1, wide_filtering, wk_filter1)
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
      use m_t_step_parameter
!
!
      integer(kind=kint ) :: i_step
!
      do i_step = i_step_init, i_step_number
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
