!main_node_clustering_test.f90
!     program  node_clustering_test
!
!-----------------------------------------------------------------------
!    main routine for GeoFEM/Tiger version       on mar. 2000 (ver 1.0)
!
      program node_clustering_test
!
      use m_precision
!
      use calypso_mpi
      use analyzer_node_cluster_test

      implicit none
!
!
      call calypso_MPI_init
!
      call initialize_node_cluster_test

      call analyze_node_cluster_test

      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program node_clustering_test
