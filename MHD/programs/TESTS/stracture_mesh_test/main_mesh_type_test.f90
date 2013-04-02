!main_mesh_type_test.f90
!     program  stracture_mesh_test
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program stracture_mesh_test
!
      use m_precision
!
      use m_parallel_var_dof
      use analyzer_mesh_type_test

      implicit none
!

      call parallel_cal_init
!
      call  init_analyzer

      call    analyze

      call  parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program stracture_mesh_test
