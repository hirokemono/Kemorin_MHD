!
!      module solve_by_crs_solver
!
!     Written by H. Matsui
!     Modified by H. Matsui on Apr., 2008
!
!      subroutine solve_by_crs_solver11
!      subroutine solve_by_crs_solver33
!      subroutine solve_by_crs_solverNN
!
      module solve_by_crs_solver
!
      use m_precision
!
      use calypso_mpi
      use m_parallel_var_dof
      use m_nod_comm_table
      use m_geometry_parameter
      use m_crs_connect
      use m_crs_matrix

      implicit none
!
      real(kind = kreal) :: rtime, starttime, endtime
!
!     solve using CRS matrix
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine solve_by_crs_solver11
!
      use solver
!
      call MPI_BARRIER  (CALYPSO_COMM,ierr_MPI)
      STARTTIME= MPI_WTIME()
 
      PRESET_crs= 2

      call  solve                                                       &
     &                  (internal_node, numnod, ntot_crs_l, ntot_crs_u, &
     &                   D_crs, AL_crs, istack_crs_l, item_crs_l,       &
     &                   AU_crs, istack_crs_u, item_crs_u, B_crs,       &
     &                   X_crs, PRESET_crs, num_neib,                   &
     &                   id_neib, istack_import, item_import,           &
     &                   istack_export, item_export,                    &
     &                   my_rank, CALYPSO_COMM, ITERactual, errno,      &
     &                   METHOD_crs, PRECOND_crs,                       &
     &                   INTARRAY_crs, REALARRAY_crs         )
!
      end subroutine solve_by_crs_solver11
!
!  ---------------------------------------------------------------------
!
      subroutine solve_by_crs_solver33
!
      use solver33
!
      call MPI_BARRIER  (CALYPSO_COMM,ierr_MPI)
      STARTTIME= MPI_WTIME()
 
      PRESET_crs= 2

      call  solve33                                                     &
     &                  (internal_node, numnod, ntot_crs_l, ntot_crs_u, &
     &                   D_crs, AL_crs, istack_crs_l, item_crs_l,       &
     &                   AU_crs, istack_crs_u, item_crs_u, B_crs,       &
     &                   X_crs, PRESET_crs, num_neib,                   &
     &                   id_neib, istack_import, item_import,           &
     &                   istack_export, item_export,                    &
     &                   my_rank, CALYPSO_COMM, ITERactual, errno,      &
     &                   METHOD_crs, PRECOND_crs,                       &
     &                   INTARRAY_crs, REALARRAY_crs         )
!
      end subroutine solve_by_crs_solver33
!
!  ---------------------------------------------------------------------
!
      subroutine solve_by_crs_solverNN
!
      use solverNN
!
!
      call MPI_BARRIER  (CALYPSO_COMM,ierr_MPI)
      STARTTIME= MPI_WTIME()
 
      PRESET_crs= 2

      call  solveNN                                                     &
     &                  (internal_node, numnod, NB_crs,                 &
     &                   ntot_crs_l, ntot_crs_u, D_crs, AL_crs,         &
     &                   istack_crs_l, item_crs_l, AU_crs,              &
     &                   istack_crs_u, item_crs_u,                      &
     &                   B_crs, X_crs,  PRESET_crs,                     &
     &                   num_neib, id_neib,                             &
     &                   istack_import, item_import,                    &
     &                   istack_export, item_export,                    &
     &                   my_rank, CALYPSO_COMM, ITERactual, ierr,       &
     &                   METHOD_crs, PRECOND_crs,                       &
     &                   INTARRAY_crs, REALARRAY_crs         )
!
      end  subroutine solve_by_crs_solverNN
!
!  ---------------------------------------------------------------------
!
      end module solve_by_crs_solver
