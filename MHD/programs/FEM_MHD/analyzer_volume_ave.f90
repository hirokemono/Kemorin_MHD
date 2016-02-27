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
      use m_mesh_data
      use input_control
!
!
        write(*,*) 'Simulation start: PE. ', my_rank
!
!     --------------------- 
!
      call input_control_4_snapshot(mesh1, group1)
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'FEM_initialize_vol_average'
      call FEM_initialize_vol_average
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
        call FEM_analyze_vol_average(i_step)
      end do
!
!      call FEM_finalize_vol_average
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_volume_ave
