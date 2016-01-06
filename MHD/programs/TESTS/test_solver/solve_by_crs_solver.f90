!
!      module solve_by_crs_solver
!
!     Written by H. Matsui
!     Modified by H. Matsui on Apr., 2008
!
!     solve using CRS matrix
!
!      subroutine solve_by_crs_solver11(nod_comm, node)
!      subroutine solve_by_crs_solver33(nod_comm, node)
!      subroutine solve_by_crs_solverNN(nod_comm, node)
!
      module solve_by_crs_solver
!
      use m_precision
!
      use calypso_mpi
!
      use t_comm_table
      use t_geometry_data
      use t_crs_matrix

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
      subroutine solve_by_crs_solver11(nod_comm, node, tbl_crs, mat_crs)
!
      use solver
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(inout) :: mat_crs
      integer(kind = kint) :: ierr
!
      call MPI_BARRIER  (CALYPSO_COMM,ierr_MPI)
      STARTTIME= MPI_WTIME()
 
      mat_crs%PRESET_crs= 2

      call  solve                                                       &
     &            (node%internal_node, node%numnod,                     &
     &             tbl_crs%ntot_l, tbl_crs%ntot_u, mat_crs%D_crs,       &
     &             mat_crs%AL_crs, tbl_crs%istack_l, tbl_crs%item_l,    &
     &             mat_crs%AU_crs, tbl_crs%istack_u, tbl_crs%item_u,    &
     &             mat_crs%B_crs, mat_crs%X_crs, mat_crs%PRESET_crs,    &
     &             nod_comm%num_neib, nod_comm%id_neib,                 &
     &             nod_comm%istack_import, nod_comm%item_import,        &
     &             nod_comm%istack_export, nod_comm%item_export,        &
     &             mat_crs%ITERactual, ierr,                            &
     &             mat_crs%METHOD_crs, mat_crs%PRECOND_crs,             &
     &             mat_crs%INTARRAY_crs, mat_crs%REALARRAY_crs)
!
      end subroutine solve_by_crs_solver11
!
!  ---------------------------------------------------------------------
!
      subroutine solve_by_crs_solver33(nod_comm, node, tbl_crs, mat_crs)
!
      use solver33
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(inout) :: mat_crs
      integer(kind = kint) :: ierr
!
      call MPI_BARRIER  (CALYPSO_COMM,ierr_MPI)
      STARTTIME= MPI_WTIME()
 
      mat_crs%PRESET_crs= 2

      call  solve33                                                     &
     &            (node%internal_node, node%numnod,                     &
     &             tbl_crs%ntot_l, tbl_crs%ntot_u, mat_crs%D_crs,      &
     &             mat_crs%AL_crs, tbl_crs%istack_l, tbl_crs%item_l,   &
     &             mat_crs%AU_crs, tbl_crs%istack_u, tbl_crs%item_u,   &
     &             mat_crs%B_crs, mat_crs%X_crs, mat_crs%PRESET_crs, &
     &             nod_comm%num_neib, nod_comm%id_neib,                 &
     &             nod_comm%istack_import, nod_comm%item_import,        &
     &             nod_comm%istack_export, nod_comm%item_export,        &
     &             mat_crs%ITERactual, ierr,                           &
     &             mat_crs%METHOD_crs, mat_crs%PRECOND_crs,           &
     &             mat_crs%INTARRAY_crs, mat_crs%REALARRAY_crs)
!
      end subroutine solve_by_crs_solver33
!
!  ---------------------------------------------------------------------
!
      subroutine solve_by_crs_solverNN(nod_comm, node, tbl_crs, mat_crs)
!
      use solverNN
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(inout) :: mat_crs
      integer(kind = kint) :: ierr
!
      call MPI_BARRIER  (CALYPSO_COMM,ierr_MPI)
      STARTTIME= MPI_WTIME()
 
      mat_crs%PRESET_crs= 2

      call  solveNN                                                     &
     &            (node%internal_node, node%numnod, mat_crs%NB_crs,     &
     &             tbl_crs%ntot_l, tbl_crs%ntot_u, mat_crs%D_crs,       &
     &             mat_crs%AL_crs, tbl_crs%istack_l, tbl_crs%item_l,    &
     &             mat_crs%AU_crs, tbl_crs%istack_u, tbl_crs%item_u,    &
     &             mat_crs%B_crs, mat_crs%X_crs, mat_crs%PRESET_crs,    &
     &             nod_comm%num_neib, nod_comm%id_neib,                 &
     &             nod_comm%istack_import, nod_comm%item_import,        &
     &             nod_comm%istack_export, nod_comm%item_export,        &
     &             mat_crs%ITERactual, ierr,                            &
     &             mat_crs%METHOD_crs, mat_crs%PRECOND_crs,             &
     &             mat_crs%INTARRAY_crs, mat_crs%REALARRAY_crs)
!
      end  subroutine solve_by_crs_solverNN
!
!  ---------------------------------------------------------------------
!
      end module solve_by_crs_solver
