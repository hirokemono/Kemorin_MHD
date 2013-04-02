!main_mesh_test.f90
!     program  mesh_test
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program mesh_test
!
      use m_precision
!
      use m_parallel_var_dof
      use analyzer_sph_bc_temp

      implicit none
!
!
      call parallel_cal_init
!
      call initilize_bc_temp

      call analyze_bc_temp

      call parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program mesh_test
