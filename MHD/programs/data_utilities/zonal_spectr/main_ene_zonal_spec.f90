!main_ene_zonal_spec.f90
!     program  ene_equator_zonal_spectr

!-----------------------------------------------------------------------
      program ene_equator_zonal_spectr
!
!      Main routine for equatorial spectrum
!      Programmed by H. Matsui on 2009
!

      use m_precision
!
      use m_parallel_var_dof
      use analyzer_ene_zonal_spec

      implicit none
!
!
      call parallel_cal_init
!
      call init_analyzer

      call analyze

      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program ene_equator_zonal_spectr
