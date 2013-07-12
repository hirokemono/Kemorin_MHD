!main_zm_kinetic_energy.f90
!     program  zonal_mean_kinetic_energy
!
      program zonal_mean_kinetic_energy
!
      use m_precision
!
      use m_parallel_var_dof
      use analyzer_zm_kinetic_energy
!
      implicit none
!
      call parallel_cal_init
!
      call init_zm_kinetic_energy
      call analyze_zm_kinetic_energy

      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program zonal_mean_kinetic_energy
