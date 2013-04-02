!main_MHD_matrices.f90
!     program  kemorin_FEM_MHD_matrices
!
      program kemorin_FEM_MHD_matrices
!
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      use m_precision
!
      use analyzer_check_mat_MHD
      use m_parallel_var_dof
!
      implicit none
!
!
!
      call parallel_cal_init
!
      call  init_analyzer
!
      call  parallel_cal_fin
!
      write(*,*) '***** program finished *****'
      stop
!
      end program kemorin_FEM_MHD_matrices
