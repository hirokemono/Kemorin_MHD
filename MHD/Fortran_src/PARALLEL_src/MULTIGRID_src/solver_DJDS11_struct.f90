!
!      module solver_DJDS11_struct
!
!     Written by H. Matsui on Jan., 2005
!
!      subroutine init_DJDS11_struct(NP, PEsmpTOT, METHOD, PRECOND,     &
!     &          ierr)
!
!      subroutine solve_DJDS11_struct(PEsmpTOT, comm_tbl, djds_tbl,     &
!     &           mat11, NP, B, X, METHOD, PRECOND,ierr, eps, itr,      &
!     &           itr_res)
!      subroutine init_solve_DJDS11_struct(PEsmpTOT, comm_tbl, djds_tbl,&
!     &           mat11, NP, B, X, METHOD, PRECOND,ierr, eps, itr,      &
!     &           itr_res)
!
!      subroutine precond_DJDS11_struct(PEsmpTOT, djds_tbl, mat11,      &
!     &          PRECOND, sigma_diag)
!
      module solver_DJDS11_struct
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
      subroutine init_DJDS11_struct(NP, PEsmpTOT, METHOD, PRECOND,      &
     &          ierr)
!
      use solver_DJDS
!
      integer(kind = kint), intent(in)  :: NP, PEsmpTOT
      character(len=kchara ), intent(in):: METHOD, PRECOND
      integer(kind = kint), intent(inout)  :: ierr
!
!
      call init_solver_DJDS(NP, PEsmpTOT, METHOD, PRECOND, ierr)
!
      end subroutine init_DJDS11_struct
!
! ----------------------------------------------------------------------
!
      subroutine solve_DJDS11_struct(PEsmpTOT, comm_tbl, djds_tbl,      &
     &           mat11, NP, B, X, METHOD, PRECOND, ierr, eps, itr,      &
     &           itr_res)
!
      use t_comm_table
      use solver_DJDS
!      use check_DJDS_ordering
!
      integer(kind = kint), intent(in)  :: PEsmpTOT
      integer(kind = kint), intent(in)  :: itr
      real(kind = kreal), intent(in) :: eps
      type(communication_table), intent(in) :: comm_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      character(len=kchara ), intent(in):: METHOD, PRECOND
      integer(kind = kint), intent(in)  :: NP
      real(kind = kreal), intent(inout) :: B(NP)
      real(kind = kreal), intent(inout) :: X(NP)
      integer(kind = kint), intent(inout)  :: ierr, itr_res
      type(DJDS_MATRIX), intent(in) :: mat11
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
!     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                   &
!     &     mat11%D, mat11%AL, mat11%AU, my_rank)
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
     &     djds_tbl%LtoU, mat11%D, B(1), X(1),                          &
     &     djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                  &
     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                    &
     &     mat11%AL, mat11%AU,  mat11%ALUG_L, mat11%ALUG_U,             &
     &     eps, itr, ierr, comm_tbl%num_neib, comm_tbl%id_neib,         &
     &     comm_tbl%istack_import, comm_tbl%item_import,                &
     &     comm_tbl%istack_export, djds_tbl%NOD_EXPORT_NEW,             &
     &     METHOD, PRECOND, itr_res)
!
      end subroutine solve_DJDS11_struct
!
! ----------------------------------------------------------------------
!
      subroutine init_solve_DJDS11_struct(PEsmpTOT, comm_tbl, djds_tbl, &
     &           mat11, NP, B, X, METHOD, PRECOND,ierr, eps, itr,       &
     &           itr_res)
!
      use t_comm_table
      use solver_DJDS
!
      integer(kind = kint), intent(in)  :: PEsmpTOT
      integer(kind = kint), intent(in)  :: itr
      real(kind = kreal), intent(in) :: eps
      type(communication_table), intent(in) :: comm_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      character(len=kchara ), intent(in):: METHOD, PRECOND
      integer(kind = kint), intent(in)  :: NP
      real(kind = kreal), intent(inout) :: B(NP)
      real(kind = kreal), intent(inout) :: X(NP)
      integer(kind = kint), intent(inout)  :: ierr, itr_res
      type(DJDS_MATRIX), intent(in) :: mat11
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
     &     djds_tbl%LtoU, mat11%D, B(1), X(1),                          &
     &     djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                  &
     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                    &
     &     mat11%AL, mat11%AU,  mat11%ALUG_L, mat11%ALUG_U,             &
     &     eps, itr, ierr, comm_tbl%num_neib, comm_tbl%id_neib,         &
     &     comm_tbl%istack_import, comm_tbl%item_import,                &
     &     comm_tbl%istack_export, djds_tbl%NOD_EXPORT_NEW,             &
     &     METHOD, PRECOND, itr_res)
!
      end subroutine init_solve_DJDS11_struct
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine precond_DJDS11_struct(PEsmpTOT, djds_tbl, mat11,       &
     &          PRECOND, sigma_diag)
!
      use preconditioning_DJDS11
!
       type(DJDS_ordering_table), intent(in) :: djds_tbl
      integer(kind = kint), intent(in)  :: PEsmpTOT
      real(kind = kreal), intent(in) :: sigma_diag
      character(len=kchara ), intent(in):: PRECOND
       type(DJDS_MATRIX), intent(inout) :: mat11
!
!C
!C== PRECONDITIONING
!
      call precond_DJDS11                                               &
     &    (mat11%internal_diag, mat11%num_diag, djds_tbl%NLmax,         &
     &     djds_tbl%itotal_l, djds_tbl%NHYP, PEsmpTOT,                  &
     &     djds_tbl%STACKmcG, djds_tbl%STACKmc,                         &
     &     djds_tbl%NLmaxHYP,  djds_tbl%IVECT,                          &
     &     djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U,          &
     &     djds_tbl%LtoU, mat11%D,                                      &
     &     djds_tbl%indexDJDS_l, djds_tbl%itemDJDS_L,                   &
     &     mat11%AL, mat11%ALUG_L, mat11%ALUG_U, PRECOND, sigma_diag)
!
      end subroutine precond_DJDS11_struct
!
! ----------------------------------------------------------------------
!
      end module solver_DJDS11_struct
