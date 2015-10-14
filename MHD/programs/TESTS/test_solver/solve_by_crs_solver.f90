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
 
      mat1_crs%PRESET_crs= 2

      call  solve                                                       &
     &            (node1%internal_node, node1%numnod,                   &
     &             tbl1_crs%ntot_l, tbl1_crs%ntot_u, mat1_crs%D_crs,    &
     &             mat1_crs%AL_crs, tbl1_crs%istack_l, tbl1_crs%item_l, &
     &             mat1_crs%AU_crs, tbl1_crs%istack_u, tbl1_crs%item_u, &
     &             mat1_crs%B_crs, mat1_crs%X_crs, mat1_crs%PRESET_crs, &
     &             nod_comm%num_neib, nod_comm%id_neib,                 &
     &             nod_comm%istack_import, nod_comm%item_import,        &
     &             nod_comm%istack_export, nod_comm%item_export,        &
     &             mat1_crs%ITERactual, ierr,                           &
     &             mat1_crs%METHOD_crs, mat1_crs%PRECOND_crs,           &
     &             mat1_crs%INTARRAY_crs, mat1_crs%REALARRAY_crs)
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
 
      mat1_crs%PRESET_crs= 2

      call  solve33                                                     &
     &            (node1%internal_node, node1%numnod,                   &
     &             tbl1_crs%ntot_l, tbl1_crs%ntot_u, mat1_crs%D_crs,    &
     &             mat1_crs%AL_crs, tbl1_crs%istack_l, tbl1_crs%item_l, &
     &             mat1_crs%AU_crs, tbl1_crs%istack_u, tbl1_crs%item_u, &
     &             mat1_crs%B_crs, mat1_crs%X_crs, mat1_crs%PRESET_crs, &
     &             nod_comm%num_neib, nod_comm%id_neib,                 &
     &             nod_comm%istack_import, nod_comm%item_import,        &
     &             nod_comm%istack_export, nod_comm%item_export,        &
     &             mat1_crs%ITERactual, ierr,                           &
     &             mat1_crs%METHOD_crs, mat1_crs%PRECOND_crs,           &
     &             mat1_crs%INTARRAY_crs, mat1_crs%REALARRAY_crs)
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
 
      mat1_crs%PRESET_crs= 2

      call  solveNN                                                     &
     &            (node1%internal_node, node1%numnod, mat1_crs%NB_crs,  &
     &             tbl1_crs%ntot_l, tbl1_crs%ntot_u, mat1_crs%D_crs,    &
     &             mat1_crs%AL_crs, tbl1_crs%istack_l, tbl1_crs%item_l, &
     &             mat1_crs%AU_crs, tbl1_crs%istack_u, tbl1_crs%item_u, &
     &             mat1_crs%B_crs, mat1_crs%X_crs, mat1_crs%PRESET_crs, &
     &             nod_comm%num_neib, nod_comm%id_neib,                 &
     &             nod_comm%istack_import, nod_comm%item_import,        &
     &             nod_comm%istack_export, nod_comm%item_export,        &
     &             mat1_crs%ITERactual, ierr,                           &
     &             mat1_crs%METHOD_crs, mat1_crs%PRECOND_crs,           &
     &             mat1_crs%INTARRAY_crs, mat1_crs%REALARRAY_crs)
!
      end  subroutine solve_by_crs_solverNN
!
!  ---------------------------------------------------------------------
!
      end module solve_by_crs_solver
