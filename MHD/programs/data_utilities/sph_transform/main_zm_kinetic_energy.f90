!main_zm_kinetic_energy.f90
!     program  zonal_mean_kinetic_energy
!
      program zonal_mean_kinetic_energy
!
      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_zm_kinetic_energy
!
      implicit none
!
      call parallel_cal_init
!
      call init_zm_kinetic_energy
      call analyze_zm_kinetic_energy

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program zonal_mean_kinetic_energy
