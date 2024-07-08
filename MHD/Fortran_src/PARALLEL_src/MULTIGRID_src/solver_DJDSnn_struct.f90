!>@file   solver_DJDSnn_struct.f90
!!@brief  module solver_DJDSnn_struct
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2005
!
!>@brief  Top routine for NxN block DJDS solver using structure
!!
!!@verbatim
!!      subroutine initNN_DJDS_struct(NP, NB, PEsmpTOT, METHOD, PRECOND,&
!!     &                              ierr, INITtime)
!!
!!      subroutine solveNN_DJDS_struct                                  &
!!     &         (NB, PEsmpTOT, comm_tbl, djds_tbl, matNN, NP, B, X,    &
!!     &          METHOD, PRECOND, SR_sig, SR_r,                        &
!!     &          ierr, eps, itr, itr_res, COMPtime, COMMtime)
!!      subroutine init_solveNN_DJDS_struct(NB, PEsmpTOT, comm_tbl,     &
!!     &          djds_tbl, matNN, NP, B, X, METHOD, PRECOND,           &
!!     &          SR_sig, SR_r, ierr, eps, itr,                         &
!!     &          itr_res, INITtime, COMPtime, COMMtime)
!!
!!      subroutine precond_DJDSnn_struct(NB, PEsmpTOT, djds_tbl, matNN, &
!!     &                                 PRECOND, sigma_diag, PRECtime)
!!@endverbatim
!
      module solver_DJDSnn_struct
!
      use m_precision
      use calypso_mpi
!
      use t_solver_djds
      use t_solver_SR
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine initNN_DJDS_struct(NP, NB, PEsmpTOT, METHOD, PRECOND,  &
     &                              ierr, INITtime)
!
      use solverNN_DJDS
!
      integer(kind = kint), intent(in)  :: NP, NB, PEsmpTOT
      character(len=kchara ), intent(in):: METHOD, PRECOND
      integer(kind = kint), intent(inout)  :: ierr
      real(kind = kreal), intent(inout) :: INITtime
!
!
      call init_solverNN_DJDS(NP, NB, PEsmpTOT, METHOD, PRECOND,        &
     &                        ierr, INITtime)
!
      end subroutine initNN_DJDS_struct
!
! ----------------------------------------------------------------------
!
      subroutine solveNN_DJDS_struct                                    &
     &         (NB, PEsmpTOT, comm_tbl, djds_tbl, matNN, NP, B, X,      &
     &          METHOD, PRECOND, SR_sig, SR_r,                          &
     &          ierr, eps, itr, itr_res, COMPtime, COMMtime)
!
      use t_comm_table
      use solverNN_DJDS
!
      integer(kind = kint), intent(in)  :: NB, PEsmpTOT
      integer(kind = kint), intent(in)  :: itr
      real(kind = kreal), intent(in) :: eps
      type(communication_table), intent(in) :: comm_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(DJDS_MATRIX), intent(in) :: matNN
!
      character(len=kchara ), intent(in):: METHOD, PRECOND
      integer(kind = kint), intent(in)  :: NP
      real(kind = kreal), intent(inout) :: B(NB*NP)
      real(kind = kreal), intent(inout) :: X(NB*NP)
      integer(kind = kint), intent(inout)  :: ierr, itr_res
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      real(kind = kreal), intent(inout) :: COMPtime
      real(kind = kreal), intent(inout) :: COMMtime
!
!
      call solveNN_DJDS_kemo                                            &
     &    (matNN%internal_diag, matNN%num_diag, NB,                     &
     &     djds_tbl%NLmax, djds_tbl%NUmax,                              &
     &     djds_tbl%itotal_l, djds_tbl%itotal_u, djds_tbl%NHYP,         &
     &     PEsmpTOT, djds_tbl%STACKmcG, djds_tbl%STACKmc,               &
     &     djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP, djds_tbl%IVECT,        &
     &     djds_tbl%NEWtoOLD, djds_tbl%OLDtoNEW_DJDS_L,                 &
     &     djds_tbl%OLDtoNEW_DJDS_U, djds_tbl%NEWtoOLD_DJDS_U,          &
     &     djds_tbl%LtoU, matNN%aiccg(1), B(1), X(1),                   &
     &     djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                  &
     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                    &
     &     matNN%aiccg(matNN%istart_l), matNN%aiccg(matNN%istart_u),    &
     &     matNN%ALUG_L, matNN%ALUG_U, eps, itr, ierr,                  &
     &     comm_tbl%num_neib, comm_tbl%id_neib,                         &
     &     comm_tbl%istack_import, comm_tbl%item_import,                &
     &     comm_tbl%istack_export, djds_tbl%NOD_EXPORT_NEW,             &
     &     METHOD, PRECOND, itr_res, SR_sig, SR_r, COMPtime, COMMtime)
!
      end subroutine solveNN_DJDS_struct
!
! ----------------------------------------------------------------------
!
      subroutine init_solveNN_DJDS_struct(NB, PEsmpTOT, comm_tbl,       &
     &          djds_tbl, matNN, NP, B, X, METHOD, PRECOND,             &
     &          SR_sig, SR_r, ierr, eps, itr,                           &
     &          itr_res, INITtime, COMPtime, COMMtime)
!
      use t_comm_table
      use solverNN_DJDS
!
      integer(kind = kint), intent(in)  :: NB, PEsmpTOT
      integer(kind = kint), intent(in)  :: itr
      real(kind = kreal), intent(in) :: eps
      type(communication_table), intent(in) :: comm_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(DJDS_MATRIX), intent(in) :: matNN
!
      character(len=kchara ), intent(in):: METHOD, PRECOND
      integer(kind = kint), intent(in)  :: NP
      real(kind = kreal), intent(inout) :: B(NB*NP)
      real(kind = kreal), intent(inout) :: X(NB*NP)
      integer(kind = kint), intent(inout)  :: ierr, itr_res
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      real(kind = kreal), intent(inout) :: INITtime
      real(kind = kreal), intent(inout) :: COMPtime
      real(kind = kreal), intent(inout) :: COMMtime
!
!
      call init_solveNN_DJDS_kemo                                       &
     &    (matNN%internal_diag, matNN%num_diag, NB,                     &
     &     djds_tbl%NLmax, djds_tbl%NUmax,                              &
     &     djds_tbl%itotal_l, djds_tbl%itotal_u, djds_tbl%NHYP,         &
     &     PEsmpTOT, djds_tbl%STACKmcG, djds_tbl%STACKmc,               &
     &     djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP, djds_tbl%IVECT,        &
     &     djds_tbl%NEWtoOLD, djds_tbl%OLDtoNEW_DJDS_L,                 &
     &     djds_tbl%OLDtoNEW_DJDS_U, djds_tbl%NEWtoOLD_DJDS_U,          &
     &     djds_tbl%LtoU, matNN%aiccg(1), B(1), X(1),                   &
     &     djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                  &
     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                    &
     &     matNN%aiccg(matNN%istart_l), matNN%aiccg(matNN%istart_u),    &
     &     matNN%ALUG_L, matNN%ALUG_U, eps, itr, ierr,                  &
     &     comm_tbl%num_neib, comm_tbl%id_neib,                         &
     &     comm_tbl%istack_import, comm_tbl%item_import,                &
     &     comm_tbl%istack_export, djds_tbl%NOD_EXPORT_NEW,             &
     &     METHOD, PRECOND, itr_res, SR_sig, SR_r,                      &
     &     INITtime, COMPtime, COMMtime)

      end subroutine init_solveNN_DJDS_struct
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine precond_DJDSnn_struct(NB, PEsmpTOT, djds_tbl, matNN,   &
     &                                 PRECOND, sigma_diag, PRECtime)
!
      use preconditioning_DJDSNN
!
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      integer(kind = kint), intent(in)  :: NB, PEsmpTOT
      real(kind = kreal), intent(in) :: sigma_diag
      character(len=kchara ), intent(in):: PRECOND
      type(DJDS_MATRIX), intent(inout) :: matNN
      real(kind = kreal), intent(inout) :: PRECtime
!
      real(kind = kreal) :: START_TIME
!
!C
!C== PRECONDITIONING
!
      START_TIME = MPI_WTIME()
      call precond_DJDSNN                                               &
     &    (matNN%internal_diag, matNN%num_diag, NB, PEsmpTOT,           &
     &     djds_tbl%STACKmcG,                                           &
     &     djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U,          &
     &     matNN%aiccg(1), matNN%ALUG_L, matNN%ALUG_U,                  &
     &     PRECOND, sigma_diag)
      PRECtime = PRECtime + (MPI_WTIME() - START_TIME)
!
      end subroutine precond_DJDSnn_struct
!
! ----------------------------------------------------------------------
!
      end module solver_DJDSnn_struct
