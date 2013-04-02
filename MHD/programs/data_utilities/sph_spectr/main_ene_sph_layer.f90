!main_ene_sph_layer.f90
!
!     program  ene_sph_layer
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program ene_sph_layer
!
      use m_precision
      use m_parallel_var_dof
!
      use analyzer_ene_sph_layer
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialize_ene_sph_layer
      call analyze_ene_sph_layer

      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program ene_sph_layer
