!main_zm_streamfunc.f90
!     program  zonal_mean_streamfunc
!
      program zonal_mean_streamfunc
!
      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_zm_streamfunc
!
      implicit none
!
      call parallel_cal_init
!
      call init_zm_streamfunc
      call analyze_zm_streamfunc

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program zonal_mean_streamfunc
