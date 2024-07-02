!
!      module solve_by_crs_solver
!
!     Written by H. Matsui
!     Modified by H. Matsui on Apr., 2008
!
!     solve using CRS matrix
!
!!      subroutine solve_by_crs_solver11(nod_comm, node, tbl_crs,       &
!!     &                                 mat_crs, SR_sig, SR_r,         &
!!     &                                 PRECtime, COMPtime, COMMtime)
!!      subroutine solve_by_crs_solver33(nod_comm, node, tbl_crs,       &
!!     &                                 mat_crs, SR_sig, SR_r,         &
!!     &                                 PRECtime, COMPtime, COMMtime)
!!      subroutine solve_by_crs_solverNN(nod_comm, node, tbl_crs,       &
!!     &                                 mat_crs, SR_sig, SR_r,         &
!!     &                                 PRECtime, COMPtime, COMMtime)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(CRS_matrix_connect), intent(in) :: tbl_crs
!!        type(CRS_matrix), intent(inout) :: mat_crs
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module solve_by_crs_solver
!
      use m_precision
!
      use calypso_mpi
!
      use t_solver_SR
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
      subroutine solve_by_crs_solver11(nod_comm, node, tbl_crs,         &
     &                                 mat_crs, SR_sig, SR_r,           &
     &                                 PRECtime, COMPtime, COMMtime)
!
      use solver
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(inout) :: mat_crs
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!>      Elapsed time for solver preconditioning
      real(kind = kreal), intent(inout) :: PRECtime
!>      Elapsed time for solver iteration
      real(kind = kreal), intent(inout) :: COMPtime
!>      Elapsed time for communication
      real(kind = kreal), intent(inout) :: COMMtime
!
      integer(kind = kint) :: ierr
!
      call MPI_BARRIER  (CALYPSO_COMM,ierr_MPI)
 
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
     &             mat_crs%INTARRAY_crs, mat_crs%REALARRAY_crs,         &
     &             SR_sig, SR_r, PRECtime, COMPtime, COMMtime)
!
      end subroutine solve_by_crs_solver11
!
!  ---------------------------------------------------------------------
!
      subroutine solve_by_crs_solver33(nod_comm, node, tbl_crs,         &
     &                                 mat_crs, SR_sig, SR_r,           &
     &                                 PRECtime, COMPtime, COMMtime)
!
      use solver33
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(inout) :: mat_crs
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!>      Elapsed time for solver preconditioning
      real(kind = kreal), intent(inout) :: PRECtime
!>      Elapsed time for solver iteration
      real(kind = kreal), intent(inout) :: COMPtime
!>      Elapsed time for communication
      real(kind = kreal), intent(inout) :: COMMtime
!
      integer(kind = kint) :: ierr
!
      call MPI_BARRIER  (CALYPSO_COMM,ierr_MPI)
 
      mat_crs%PRESET_crs= 2

      call  solve33                                                     &
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
     &             mat_crs%INTARRAY_crs, mat_crs%REALARRAY_crs,         &
     &             SR_sig, SR_r, PRECtime, COMPtime, COMMtime)
!
      end subroutine solve_by_crs_solver33
!
!  ---------------------------------------------------------------------
!
      subroutine solve_by_crs_solverNN(nod_comm, node, tbl_crs,         &
     &                                 mat_crs, SR_sig, SR_r,           &
     &                                 PRECtime, COMPtime, COMMtime)
!
      use solverNN
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(inout) :: mat_crs
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!>      Elapsed time for solver preconditioning
      real(kind = kreal), intent(inout) :: PRECtime
!>      Elapsed time for solver iteration
      real(kind = kreal), intent(inout) :: COMPtime
!>      Elapsed time for communication
      real(kind = kreal), intent(inout) :: COMMtime
!
      integer(kind = kint) :: ierr
!
      call MPI_BARRIER  (CALYPSO_COMM,ierr_MPI)

 
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
     &             mat_crs%INTARRAY_crs, mat_crs%REALARRAY_crs,         &
     &             SR_sig, SR_r, PRECtime, COMPtime, COMMtime)
!
      end  subroutine solve_by_crs_solverNN
!
!  ---------------------------------------------------------------------
!
      end module solve_by_crs_solver
