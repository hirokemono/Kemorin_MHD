!
!      module solver_DJDSnn_struct
!
!     Written by H. Matsui on Jan., 2005
!
!      subroutine initNN_DJDS_struct(NP, NB, PEsmpTOT,                  &
!     &          METHOD, PRECOND, ierr)
!
!      subroutine solveNN_DJDS_struct(NB, PEsmpTOT, comm_tbl, djds_tbl, &
!     &           matNN, NP, B, X, METHOD, PRECOND,ierr, eps, itr,      &
!     &           itr_res)
!      subroutine init_solveNN_DJDS_struct(NB, PEsmpTOT, comm_tbl,      &
!     &          djds_tbl, matNN, NP, B, X, METHOD, PRECOND, ierr,      &
!     &          eps, itr, itr_res)
!
!      subroutine precond_DJDSnn_struct(NB, PEsmpTOT, djds_tbl, matNN,  &
!     &          PRECOND, sigma_diag)
!
      module solver_DJDSnn_struct
!
      use m_precision
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
      subroutine initNN_DJDS_struct(NP, NB, PEsmpTOT,                   &
     &          METHOD, PRECOND, ierr)
!
      use solverNN_DJDS
!
      integer(kind = kint), intent(in)  :: NP, NB, PEsmpTOT
      character(len=kchara ), intent(in):: METHOD, PRECOND
      integer(kind = kint), intent(inout)  :: ierr
!
!
      call init_solverNN_DJDS(NP, NB, PEsmpTOT, METHOD, PRECOND,        &
     &    ierr)
!
      end subroutine initNN_DJDS_struct
!
! ----------------------------------------------------------------------
!
      subroutine solveNN_DJDS_struct(NB, PEsmpTOT, comm_tbl, djds_tbl,  &
     &           matNN, NP, B, X, METHOD, PRECOND,ierr, eps, itr,       &
     &           itr_res)
!
      use t_comm_table
      use solverNN_DJDS
!
      integer(kind = kint), intent(in)  :: NB, PEsmpTOT
      integer(kind = kint), intent(in)  :: itr
      real(kind = kreal), intent(in) :: eps
      type(communication_table), intent(in) :: comm_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      character(len=kchara ), intent(in):: METHOD, PRECOND
      integer(kind = kint), intent(in)  :: NP
      real(kind = kreal), intent(inout) :: B(NB*NP)
      real(kind = kreal), intent(inout) :: X(NB*NP)
      integer(kind = kint), intent(inout)  :: ierr, itr_res
      type(DJDS_MATRIX), intent(in) :: matNN
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
     &     djds_tbl%LtoU, matNN%D, B(1), X(1),                          &
     &     djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                  &
     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                    &
     &     matNN%AL, matNN%AU,  matNN%ALUG_L, matNN%ALUG_U,             &
     &     eps, itr, ierr, comm_tbl%num_neib, comm_tbl%id_neib,         &
     &     comm_tbl%istack_import, comm_tbl%item_import,                &
     &     comm_tbl%istack_export, djds_tbl%NOD_EXPORT_NEW,             &
     &     METHOD, PRECOND, itr_res)
!
      end subroutine solveNN_DJDS_struct
!
! ----------------------------------------------------------------------
!
      subroutine init_solveNN_DJDS_struct(NB, PEsmpTOT, comm_tbl,       &
     &          djds_tbl, matNN, NP, B, X, METHOD, PRECOND, ierr,       &
     &          eps, itr, itr_res)
!
      use t_comm_table
      use solverNN_DJDS
!
      integer(kind = kint), intent(in)  :: NB, PEsmpTOT
      integer(kind = kint), intent(in)  :: itr
      real(kind = kreal), intent(in) :: eps
      type(communication_table), intent(in) :: comm_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      character(len=kchara ), intent(in):: METHOD, PRECOND
      integer(kind = kint), intent(in)  :: NP
      real(kind = kreal), intent(inout) :: B(NB*NP)
      real(kind = kreal), intent(inout) :: X(NB*NP)
      integer(kind = kint), intent(inout)  :: ierr, itr_res
      type(DJDS_MATRIX), intent(in) :: matNN
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
     &     djds_tbl%LtoU, matNN%D, B(1), X(1),                          &
     &     djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                  &
     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                    &
     &     matNN%AL, matNN%AU,  matNN%ALUG_L, matNN%ALUG_U,             &
     &     eps, itr, ierr, comm_tbl%num_neib, comm_tbl%id_neib,         &
     &     comm_tbl%istack_import, comm_tbl%item_import,                &
     &     comm_tbl%istack_export, djds_tbl%NOD_EXPORT_NEW,             &
     &     METHOD, PRECOND, itr_res)
!
      end subroutine init_solveNN_DJDS_struct
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine precond_DJDSnn_struct(NB, PEsmpTOT, djds_tbl, matNN,   &
     &          PRECOND, sigma_diag)
!
      use preconditioning_DJDSNN
!
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      integer(kind = kint), intent(in)  :: NB, PEsmpTOT
      real(kind = kreal), intent(in) :: sigma_diag
      character(len=kchara ), intent(in):: PRECOND
      type(DJDS_MATRIX), intent(inout) :: matNN
!
!C
!C== PRECONDITIONING
!
      call precond_DJDSNN                                               &
     &    (matNN%internal_diag, matNN%num_diag, NB, PEsmpTOT,           &
     &     djds_tbl%STACKmcG,                                           &
     &     djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U,          &
     &     matNN%D, matNN%ALUG_L, matNN%ALUG_U, PRECOND, sigma_diag)
!
      end subroutine precond_DJDSnn_struct
!
! ----------------------------------------------------------------------
!
      end module solver_DJDSnn_struct
