!
      program add_elegrp_sph
!
      use m_precision
!
      use m_parallel_var_dof
      use calypso_mpi
      use analyzer_add_ele_group
!
      implicit none
!
!
      call parallel_cal_init
!
      call initialize_add_egrp
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program add_elegrp_sph
