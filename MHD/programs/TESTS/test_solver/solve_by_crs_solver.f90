!
!      module solve_by_crs_solver
!
!     Written by H. Matsui
!     Modified by H. Matsui on Apr., 2008
!
!     solve using CRS matrix
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
      use m_nod_comm_table
      use m_geometry_data
      use m_crs_connect
      use m_crs_matrix

      implicit none
!
      real(kind = kreal) :: rtime, starttime, endtime
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
      integer(kind = kint) :: ierr
!
      call MPI_BARRIER  (CALYPSO_COMM,ierr_MPI)
      STARTTIME= MPI_WTIME()
 
      PRESET_crs= 2

      call  solve                                                       &
     &                  (node1%internal_node, node1%numnod,             &
     &                   ntot_crs_l, ntot_crs_u, D_crs, AL_crs,         &
     &                   istack_crs_l, item_crs_l, AU_crs,              &
     &                   istack_crs_u, item_crs_u,                      &
     &                   B_crs, X_crs, PRESET_crs,                      &
     &                   nod_comm%num_neib, nod_comm%id_neib,           &
     &                   nod_comm%istack_import, nod_comm%item_import,  &
     &                   nod_comm%istack_export, nod_comm%item_export,  &
     &                   ITERactual, ierr, METHOD_crs, PRECOND_crs,     &
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
      integer(kind = kint) :: ierr
!
      call MPI_BARRIER  (CALYPSO_COMM,ierr_MPI)
      STARTTIME= MPI_WTIME()
 
      PRESET_crs= 2

      call  solve33                                                     &
     &                  (node1%internal_node, node1%numnod,             &
     &                   ntot_crs_l, ntot_crs_u, D_crs, AL_crs,         &
     &                   istack_crs_l, item_crs_l, AU_crs,              &
     &                   istack_crs_u, item_crs_u,                      &
     &                   B_crs, X_crs, PRESET_crs,                      &
     &                   nod_comm%num_neib, nod_comm%id_neib,           &
     &                   nod_comm%istack_import, nod_comm%item_import,  &
     &                   nod_comm%istack_export, nod_comm%item_export,  &
     &                   ITERactual, ierr, METHOD_crs, PRECOND_crs,     &
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
      integer(kind = kint) :: ierr
!
      call MPI_BARRIER  (CALYPSO_COMM,ierr_MPI)
      STARTTIME= MPI_WTIME()
 
      PRESET_crs= 2

      call  solveNN                                                     &
     &                  (node1%internal_node, node1%numnod, NB_crs,     &
     &                   ntot_crs_l, ntot_crs_u, D_crs, AL_crs,         &
     &                   istack_crs_l, item_crs_l, AU_crs,              &
     &                   istack_crs_u, item_crs_u,                      &
     &                   B_crs, X_crs,  PRESET_crs,                     &
     &                   nod_comm%num_neib, nod_comm%id_neib,           &
     &                   nod_comm%istack_import, nod_comm%item_import,  &
     &                   nod_comm%istack_export, nod_comm%item_export,  &
     &                   ITERactual, ierr, METHOD_crs, PRECOND_crs,     &
     &                   INTARRAY_crs, REALARRAY_crs         )
!
      end  subroutine solve_by_crs_solverNN
!
!  ---------------------------------------------------------------------
!
      end module solve_by_crs_solver
