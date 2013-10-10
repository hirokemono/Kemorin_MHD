!
!      module solver_DJDS33_struct
!
!     Written by H. Matsui on Jan., 2005
!
!      subroutine init33_DJDS_struct(NP, PEsmpTOT,                      &
!     &          METHOD, PRECOND, ierr)
!
!      subroutine solve33_DJDS_struct(PEsmpTOT, comm_tbl, djds_tbl,     &
!     &           mat33, NP, B, X, METHOD, PRECOND,ierr, eps, itr,      &
!     &           itr_res)
!      subroutine init_solve33_DJDS_struct(PEsmpTOT, comm_tbl, djds_tbl,&
!     &           mat33, NP, B, X, METHOD, PRECOND,ierr, eps, itr,      &
!     &           itr_res)
!
!      subroutine precond_DJDS33_struct(PEsmpTOT, djds_tbl, mat33,      &
!     &          PRECOND, sigma_diag)
!
      module solver_DJDS33_struct
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
      subroutine init33_DJDS_struct(NP, PEsmpTOT,                       &
     &          METHOD, PRECOND, ierr)
!
      use solver33_DJDS
!
      integer(kind = kint), intent(in)  :: NP, PEsmpTOT
      character(len=kchara ), intent(in):: METHOD, PRECOND
      integer(kind = kint), intent(inout)  :: ierr
!
!
      call init_solver33_DJDS(NP, PEsmpTOT, METHOD, PRECOND, ierr)
!
      end subroutine init33_DJDS_struct
!
! ----------------------------------------------------------------------
!
      subroutine solve33_DJDS_struct(PEsmpTOT, comm_tbl, djds_tbl,      &
     &           mat33, NP, B, X, METHOD, PRECOND,ierr, eps, itr,       &
     &           itr_res)
!
      use t_comm_table
      use solver33_DJDS
!
      integer(kind = kint), intent(in)  :: PEsmpTOT
      integer(kind = kint), intent(in)  :: itr
      real(kind = kreal), intent(in) :: eps
      type(communication_table), intent(in) :: comm_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      character(len=kchara ), intent(in):: METHOD, PRECOND
      integer(kind = kint), intent(in)  :: NP
      real(kind = kreal), intent(inout) :: B(3*NP)
      real(kind = kreal), intent(inout) :: X(3*NP)
      integer(kind = kint), intent(inout)  :: ierr, itr_res
      type(DJDS_MATRIX), intent(in) :: mat33
!
!
      call solve33_DJDS_kemo                                            &
     &    (mat33%internal_diag, mat33%num_diag,                         &
     &     djds_tbl%NLmax, djds_tbl%NUmax,                              &
     &     djds_tbl%itotal_l, djds_tbl%itotal_u, djds_tbl%NHYP,         &
     &     PEsmpTOT, djds_tbl%STACKmcG, djds_tbl%STACKmc,               &
     &     djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP, djds_tbl%IVECT,        &
     &     djds_tbl%NEWtoOLD, djds_tbl%OLDtoNEW_DJDS_L,                 &
     &     djds_tbl%OLDtoNEW_DJDS_U, djds_tbl%NEWtoOLD_DJDS_U,          &
     &     djds_tbl%LtoU, mat33%D, B(1), X(1),                          &
     &     djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                  &
     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                    &
     &     mat33%AL, mat33%AU,  mat33%ALUG_L, mat33%ALUG_U,             &
     &     eps, itr, ierr, comm_tbl%num_neib, comm_tbl%id_neib,         &
     &     comm_tbl%istack_import, comm_tbl%item_import,                &
     &     comm_tbl%istack_export, djds_tbl%NOD_EXPORT_NEW,             &
     &     METHOD, PRECOND, itr_res)
!
      end subroutine solve33_DJDS_struct
!
! ----------------------------------------------------------------------
!
      subroutine init_solve33_DJDS_struct(PEsmpTOT, comm_tbl, djds_tbl, &
     &           mat33, NP, B, X, METHOD, PRECOND,ierr, eps, itr,       &
     &           itr_res)
!
      use t_comm_table
      use solver33_DJDS
!
      integer(kind = kint), intent(in)  :: PEsmpTOT
      integer(kind = kint), intent(in)  :: itr
      real(kind = kreal), intent(in) :: eps
      type(communication_table), intent(in) :: comm_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      character(len=kchara ), intent(in):: METHOD, PRECOND
      integer(kind = kint), intent(in)  :: NP
      real(kind = kreal), intent(inout) :: B(3*NP)
      real(kind = kreal), intent(inout) :: X(3*NP)
      integer(kind = kint), intent(inout)  :: ierr, itr_res
      type(DJDS_MATRIX), intent(in) :: mat33
!
!
      call init_solve33_DJDS_kemo                                       &
     &    (mat33%internal_diag, mat33%num_diag,                         &
     &     djds_tbl%NLmax, djds_tbl%NUmax,                              &
     &     djds_tbl%itotal_l, djds_tbl%itotal_u, djds_tbl%NHYP,         &
     &     PEsmpTOT, djds_tbl%STACKmcG, djds_tbl%STACKmc,               &
     &     djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP, djds_tbl%IVECT,        &
     &     djds_tbl%NEWtoOLD, djds_tbl%OLDtoNEW_DJDS_L,                 &
     &     djds_tbl%OLDtoNEW_DJDS_U, djds_tbl%NEWtoOLD_DJDS_U,          &
     &     djds_tbl%LtoU, mat33%D, B(1), X(1),                          &
     &     djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                  &
     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                    &
     &     mat33%AL, mat33%AU,  mat33%ALUG_L, mat33%ALUG_U,             &
     &     eps, itr, ierr, comm_tbl%num_neib, comm_tbl%id_neib,         &
     &     comm_tbl%istack_import, comm_tbl%item_import,                &
     &     comm_tbl%istack_export, djds_tbl%NOD_EXPORT_NEW,             &
     &     METHOD, PRECOND, itr_res)
!
      end subroutine init_solve33_DJDS_struct
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine precond_DJDS33_struct(PEsmpTOT, djds_tbl, mat33,       &
     &          PRECOND, sigma_diag)
!
      use preconditioning_DJDS33
!
       type(DJDS_ordering_table), intent(in) :: djds_tbl
      integer(kind = kint), intent(in)  :: PEsmpTOT
      real(kind = kreal), intent(in) :: sigma_diag
      character(len=kchara ), intent(in):: PRECOND
       type(DJDS_MATRIX), intent(inout) :: mat33
!
!C
!C== PRECONDITIONING
!
      call precond_DJDS33                                               &
     &    (mat33%internal_diag, mat33%num_diag, PEsmpTOT,               &
     &     djds_tbl%STACKmcG,                                           &
     &     djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U,          &
     &     mat33%D, mat33%ALUG_L, mat33%ALUG_U, PRECOND, sigma_diag)
!
      end subroutine precond_DJDS33_struct
!
! ----------------------------------------------------------------------
!
      end module solver_DJDS33_struct
