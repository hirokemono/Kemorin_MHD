!>@file   solver_DJDS11_struct.f90
!!@brief  module solver_DJDS11_struct
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2005
!
!>@brief  Top routine for DJDS solver using structure
!!
!!@verbatim
!!      subroutine init_DJDS11_struct(NP, PEsmpTOT, METHOD, PRECOND,    &
!!     &                              ierr, INITtime)
!!
!!      subroutine solve_DJDS11_struct(PEsmpTOT, comm_tbl, djds_tbl,    &
!!     &           mat11, NP, B, X, METHOD, PRECOND, SR_sig, SR_r,      &
!!     &           ierr, eps, itr, itr_res, COMPtime, COMMtime)
!!      subroutine init_solve_DJDS11_struct                             &
!!     &         (PEsmpTOT, comm_tbl, djds_tbl, mat11, NP, B, X,        &
!!     &          METHOD, PRECOND, SR_sig, SR_r, ierr, eps, itr,        &
!!     &          itr_res, INITtime, COMPtime, COMMtime)
!!
!!      subroutine precond_DJDS11_struct(PEsmpTOT, djds_tbl, mat11,     &
!!     &                                 PRECOND, sigma_diag, PRECtime)
!!@endverbatim
!
      module solver_DJDS11_struct
!
      use m_precision
      use calypso_mpi
!
      use t_solver_djds
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine init_DJDS11_struct(NP, PEsmpTOT, METHOD, PRECOND,      &
     &                              ierr, INITtime)
!
      use solver_DJDS
!
      integer(kind = kint), intent(in)  :: NP, PEsmpTOT
      character(len=kchara ), intent(in):: METHOD, PRECOND
      integer(kind = kint), intent(inout)  :: ierr
      real(kind = kreal), intent(inout) :: INITtime
!
!
      call init_solver_DJDS(NP, PEsmpTOT, METHOD, PRECOND,              &
     &                      ierr, INITtime)
!
      end subroutine init_DJDS11_struct
!
! ----------------------------------------------------------------------
!
      subroutine solve_DJDS11_struct(PEsmpTOT, comm_tbl, djds_tbl,      &
     &           mat11, NP, B, X, METHOD, PRECOND, SR_sig, SR_r,        &
     &           ierr, eps, itr, itr_res, COMPtime, COMMtime)
!
      use t_solver_SR
      use t_comm_table
      use solver_DJDS
!      use check_DJDS_ordering
!
      integer(kind = kint), intent(in)  :: PEsmpTOT
      integer(kind = kint), intent(in)  :: itr
      real(kind = kreal), intent(in) :: eps
      type(communication_table), intent(in) :: comm_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(DJDS_MATRIX), intent(in) :: mat11
!
      character(len=kchara ), intent(in):: METHOD, PRECOND
      integer(kind = kint), intent(in)  :: NP
      real(kind = kreal), intent(inout) :: B(NP)
      real(kind = kreal), intent(inout) :: X(NP)
      integer(kind = kint), intent(inout)  :: ierr, itr_res
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      real(kind = kreal), intent(inout) :: COMPtime
      real(kind = kreal), intent(inout) :: COMMtime
!
!
!       call s_check_DJDS_ordering                                      &
!     &    (mat11%internal_diag, mat11%num_diag,                        &
!     &     djds_tbl%NLmax, djds_tbl%NUmax,                             &
!     &     djds_tbl%itotal_l, djds_tbl%itotal_u,                       &
!     &     (djds_tbl%NLmax*PEsmpTOT), (djds_tbl%NUmax*PEsmpTOT),       &
!     &     djds_tbl%NHYP, PEsmpTOT, djds_tbl%STACKmcG,djds_tbl%STACKmc,&
!     &     djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP,                       &
!     &     djds_tbl%NEWtoOLD, djds_tbl%OLDtoNEW_DJDS_L,                &
!     &     djds_tbl%OLDtoNEW_DJDS_U, djds_tbl%NEWtoOLD_DJDS_U,         &
!     &     djds_tbl%LtoU,                                              &
!     &     djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                 &
!     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                   &
!     &     my_rank)
!
!       call reverse_DJDS_matrix                                        &
!     &    (mat11%internal_diag, mat11%num_diag,                        &
!     &     djds_tbl%NLmax, djds_tbl%NUmax,                             &
!     &     djds_tbl%itotal_l, djds_tbl%itotal_u,                       &
!     &     (djds_tbl%NLmax*PEsmpTOT), (djds_tbl%NUmax*PEsmpTOT),       &
!     &     djds_tbl%NHYP, PEsmpTOT, djds_tbl%STACKmcG,djds_tbl%STACKmc,&
!     &     djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP,                       &
!     &     djds_tbl%NEWtoOLD, djds_tbl%OLDtoNEW_DJDS_L,                &
!     &     djds_tbl%OLDtoNEW_DJDS_U, djds_tbl%NEWtoOLD_DJDS_U,         &
!     &     djds_tbl%LtoU,                                              &
!     &     djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                 &
!     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U, mat11%aiccg(1),   &
!     &     mat11%aiccg(mat11%istart_l), mat11%aiccg(mat11%istart_u),   &
!     &     my_rank)
!
!       call deallocate_check_djds_array
!
      call solve_DJDS_kemo                                              &
     &    (mat11%internal_diag, mat11%num_diag,                         &
     &     djds_tbl%NLmax, djds_tbl%NUmax,                              &
     &     djds_tbl%itotal_l, djds_tbl%itotal_u, djds_tbl%NHYP,         &
     &     PEsmpTOT, djds_tbl%STACKmcG, djds_tbl%STACKmc,               &
     &     djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP, djds_tbl%IVECT,        &
     &     djds_tbl%NEWtoOLD, djds_tbl%OLDtoNEW_DJDS_L,                 &
     &     djds_tbl%OLDtoNEW_DJDS_U, djds_tbl%NEWtoOLD_DJDS_U,          &
     &     djds_tbl%LtoU, mat11%aiccg(1), B(1), X(1),                   &
     &     djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                  &
     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                    &
     &     mat11%aiccg(mat11%istart_l), mat11%aiccg(mat11%istart_u),    &
     &     mat11%ALUG_L, mat11%ALUG_U, eps, itr, ierr,                  &
     &     comm_tbl%num_neib, comm_tbl%id_neib,                         &
     &     comm_tbl%istack_import, comm_tbl%item_import,                &
     &     comm_tbl%istack_export, djds_tbl%NOD_EXPORT_NEW,             &
     &     METHOD, PRECOND, itr_res, SR_sig, SR_r, COMPtime, COMMtime)
!
      end subroutine solve_DJDS11_struct
!
! ----------------------------------------------------------------------
!
      subroutine init_solve_DJDS11_struct                               &
     &         (PEsmpTOT, comm_tbl, djds_tbl, mat11, NP, B, X,          &
     &          METHOD, PRECOND, SR_sig, SR_r, ierr, eps, itr,          &
     &          itr_res, INITtime, COMPtime, COMMtime)
!
      use t_comm_table
      use solver_DJDS
!
      integer(kind = kint), intent(in)  :: PEsmpTOT
      integer(kind = kint), intent(in)  :: itr
      real(kind = kreal), intent(in) :: eps
      type(communication_table), intent(in) :: comm_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(DJDS_MATRIX), intent(in) :: mat11
!
      character(len=kchara ), intent(in):: METHOD, PRECOND
      integer(kind = kint), intent(in)  :: NP
      real(kind = kreal), intent(inout) :: B(NP)
      real(kind = kreal), intent(inout) :: X(NP)
      integer(kind = kint), intent(inout)  :: ierr, itr_res
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      real(kind = kreal), intent(inout) :: INITtime
      real(kind = kreal), intent(inout) :: COMPtime
      real(kind = kreal), intent(inout) :: COMMtime
!
!
      call init_solve_DJDS_kemo                                         &
     &    (mat11%internal_diag, mat11%num_diag,                         &
     &     djds_tbl%NLmax, djds_tbl%NUmax,                              &
     &     djds_tbl%itotal_l, djds_tbl%itotal_u, djds_tbl%NHYP,         &
     &     PEsmpTOT, djds_tbl%STACKmcG, djds_tbl%STACKmc,               &
     &     djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP, djds_tbl%IVECT,        &
     &     djds_tbl%NEWtoOLD, djds_tbl%OLDtoNEW_DJDS_L,                 &
     &     djds_tbl%OLDtoNEW_DJDS_U, djds_tbl%NEWtoOLD_DJDS_U,          &
     &     djds_tbl%LtoU, mat11%aiccg(1), B(1), X(1),                   &
     &     djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                  &
     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                    &
     &     mat11%aiccg(mat11%istart_l), mat11%aiccg(mat11%istart_u),    &
     &     mat11%ALUG_L, mat11%ALUG_U, eps, itr, ierr,                  &
     &     comm_tbl%num_neib, comm_tbl%id_neib,                         &
     &     comm_tbl%istack_import, comm_tbl%item_import,                &
     &     comm_tbl%istack_export, djds_tbl%NOD_EXPORT_NEW,             &
     &     METHOD, PRECOND, itr_res, SR_sig, SR_r,                      &
     &     INITtime, COMPtime, COMMtime)
!
      end subroutine init_solve_DJDS11_struct
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine precond_DJDS11_struct(PEsmpTOT, djds_tbl, mat11,       &
     &                                 PRECOND, sigma_diag, PRECtime)
!
      use preconditioning_DJDS11
!
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      integer(kind = kint), intent(in)  :: PEsmpTOT
      real(kind = kreal), intent(in) :: sigma_diag
      character(len=kchara ), intent(in):: PRECOND
      type(DJDS_MATRIX), intent(inout) :: mat11
      real(kind = kreal), intent(inout) :: PRECtime
!
      real(kind = kreal) :: START_TIME
!
!C
!C== PRECONDITIONING
!
      START_TIME = MPI_WTIME()
      call precond_DJDS11                                               &
     &    (mat11%internal_diag, mat11%num_diag, djds_tbl%NLmax,         &
     &     djds_tbl%itotal_l, djds_tbl%NHYP, PEsmpTOT,                  &
     &     djds_tbl%STACKmcG, djds_tbl%STACKmc,                         &
     &     djds_tbl%NLmaxHYP,  djds_tbl%IVECT,                          &
     &     djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U,          &
     &     djds_tbl%LtoU, mat11%aiccg(1),                               &
     &     djds_tbl%indexDJDS_l, djds_tbl%itemDJDS_L,                   &
     &     mat11%aiccg(mat11%istart_l), mat11%ALUG_L, mat11%ALUG_U,     &
     &     PRECOND, sigma_diag)
      PRECtime = PRECtime + (MPI_WTIME() - START_TIME)
!
      end subroutine precond_DJDS11_struct
!
! ----------------------------------------------------------------------
!
      end module solver_DJDS11_struct
