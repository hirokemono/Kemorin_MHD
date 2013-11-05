!link_solver_2_djds_struct.f90
!      module link_solver_2_djds_struct
!
!      Written by Hiroaki Matsui on Dec., 2008
!
!      subroutine link_djds_table_2_struct                              &
!     &         ( N, NP, NLmax, NUmax, itotal_l, itotal_u, NHYP, np_smp,&
!     &           STACKmcG, STACKmc, NLmaxHYP, NUmaxHYP, IVECT,         &
!     &           NEWtoOLD, OLDtoNEW, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, &
!     &           NEWtoOLD_DJDS_U, LtoU,                                &
!     &           indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,     &
!     &           NTOT_EXPORT, NOD_EXPORT_NEW, djds_tbl)
!
!         type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!      integer(kind=kint), intent(in) :: N, NP
!      integer(kind=kint), intent(in) :: NLmax, NUmax
!      integer(kind=kint), intent(in) :: itotal_l, itotal_u
!      integer(kind=kint), intent(in) :: np_smp, NHYP
!      integer(kind=kint), target, intent(in) :: NEWtoOLD(NP)
!      integer(kind=kint), target, intent(in) :: OLDtoNEW(NP)
!      integer(kind=kint), target, intent(in) :: NEWtoOLD_DJDS_U(NP)
!      integer(kind=kint), target, intent(in) :: LtoU(NP)
!      integer(kind=kint), target, intent(in) :: OLDtoNEW_DJDS_L(NP)
!      integer(kind=kint), target, intent(in) :: OLDtoNEW_DJDS_U(NP)
!      integer(kind=kint), target, intent(in) :: IVECT(0:NHYP)
!      integer(kind=kint), target, intent(in) :: NLmaxHYP(NHYP)
!      integer(kind=kint), target, intent(in) ::  NUmaxHYP(NHYP)
!
!      integer(kind=kint), target, intent(in)                           &
!     &            :: indexDJDS_L(0:NLmax*NHYP*np_smp)
!      integer(kind=kint), target, intent(in)                           &
!     &            :: indexDJDS_U(0:NUmax*NHYP*np_smp)
!      integer(kind=kint), target, intent(in) :: itemDJDS_L(itotal_l)
!      integer(kind=kint), target, intent(in) :: itemDJDS_U(itotal_u)
!
!      integer(kind=kint), target, intent(in) :: STACKmc(0:np_smp*NHYP)
!      integer(kind=kint), target, intent(in) :: STACKmcG(0:np_smp)
!
!      integer(kind=kint ), target, intent(in) :: NTOT_EXPORT
!      integer(kind=kint ), target, intent(in)                          &
!     &            :: NOD_EXPORT_NEW(NTOT_EXPORT)
!
!      subroutine link_djds_mat11_2_struct(NP, N, num_non0, istart_diag,&
!     &          istart_l, istart_u, aiccg, ALUG_L, ALUG_U, mat11)
!
!        integer(kind=kint), intent(in) :: N, NP, num_non0
!        integer(kind=kint), intent(in) :: istart_diag, istart_l, istart_u
!      real(kind = kreal), target, intent(in) :: aiccg(-8:9*num_non0)
!      real(kind = kreal), target, intent(in) :: ALUG_L(9*N)
!      real(kind = kreal), target, intent(in) :: ALUG_U(9*N)
!
!        type(DJDS_MATRIX), intent(inout) :: mat11
!
!
!      subroutine link_djds_mat33_2_struct(N, NP, num_non0, istart_diag,&
!     &          istart_l, istart_u, aiccg, ALUG_L, ALUG_U, mat33)
!
!        integer(kind=kint), intent(in) :: N, NP, num_non0
!        integer(kind=kint), intent(in) :: istart_diag, istart_l, istart_u
!      real(kind = kreal), target, intent(in) :: aiccg(-8:9*num_non0)
!      real(kind = kreal), target, intent(in) :: ALUG_L(9*N)
!      real(kind = kreal), target, intent(in) :: ALUG_U(9*N)
!
!        type(DJDS_MATRIX), intent(inout) :: mat33
!
!
!      subroutine link_djds_matNN_2_struct(N, NP, NB, num_non0,         &
!     &          istart_diag, istart_l, istart_u, aiccg, ALUG_L, ALUG_U,&
!     &          matNN)
!
!        integer(kind=kint), intent(in) :: NP, N, NB, num_non0
!        integer(kind=kint), intent(in) :: istart_diag, istart_l, istart_u
!      real(kind = kreal), target, intent(in)                           &
!     &          :: aiccg(-NB*NB+1:NB*NB*num_non0)
!      real(kind = kreal), target, intent(in) :: ALUG_L(NB*NB**N)
!      real(kind = kreal), target, intent(in) :: ALUG_U(NB*NB**N)
!
!          type(DJDS_MATRIX), intent(inout) :: matNN
!
      module link_solver_2_djds_struct
!
      use m_precision
!
      use t_solver_djds
!
      implicit none
!
! ------------------------------------------
!
      contains
!
! ------------------------------------------
!
      subroutine link_djds_table_2_struct                               &
     &         ( N, NP, NLmax, NUmax, itotal_l, itotal_u, NHYP, np_smp, &
     &           STACKmcG, STACKmc, NLmaxHYP, NUmaxHYP, IVECT,          &
     &           NEWtoOLD, OLDtoNEW, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,  &
     &           NEWtoOLD_DJDS_U, LtoU,                                 &
     &           indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,      &
     &           NTOT_EXPORT, NOD_EXPORT_NEW, djds_tbl)
!
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
      integer(kind=kint), intent(in) :: N, NP
      integer(kind=kint), intent(in) :: NLmax, NUmax
      integer(kind=kint), intent(in) :: itotal_l, itotal_u
      integer(kind=kint), intent(in) :: np_smp, NHYP

      integer(kind=kint), target, intent(in) :: NEWtoOLD(NP)
      integer(kind=kint), target, intent(in) :: OLDtoNEW(NP)
      integer(kind=kint), target, intent(in) :: NEWtoOLD_DJDS_U(NP)
      integer(kind=kint), target, intent(in) :: LtoU(NP)
      integer(kind=kint), target, intent(in) :: OLDtoNEW_DJDS_L(NP)
      integer(kind=kint), target, intent(in) :: OLDtoNEW_DJDS_U(NP)
      integer(kind=kint), target, intent(in) :: IVECT(0:NHYP)
      integer(kind=kint), target, intent(in) :: NLmaxHYP(NHYP)
      integer(kind=kint), target, intent(in) :: NUmaxHYP(NHYP)

      integer(kind=kint), target, intent(in)                            &
     &            :: indexDJDS_L(0:NLmax*NHYP*np_smp)
      integer(kind=kint), target, intent(in)                            &
     &            :: indexDJDS_U(0:NUmax*NHYP*np_smp)
      integer(kind=kint), target, intent(in) :: itemDJDS_L(itotal_l)
      integer(kind=kint), target, intent(in) :: itemDJDS_U(itotal_u)

      integer(kind=kint), target, intent(in) :: STACKmc(0:np_smp*NHYP)
      integer(kind=kint), target, intent(in) :: STACKmcG(0:np_smp)

      integer(kind=kint ), target, intent(in) :: NTOT_EXPORT
      integer(kind=kint ), target, intent(in)                           &
     &            :: NOD_EXPORT_NEW(NTOT_EXPORT)
!
!
      djds_tbl%itotal_l = itotal_l
      djds_tbl%itotal_u = itotal_u
      djds_tbl%NHYP =     NHYP
      djds_tbl%NLmax =    NLmax
      djds_tbl%NUmax =    NUmax
      djds_tbl%npLX1 =    NLmax * np_smp
      djds_tbl%npUX1 =    NUmax * np_smp
!
      djds_tbl%IVECT =>           IVECT
      djds_tbl%NEWtoOLD =>        NEWtoOLD
      djds_tbl%OLDtoNEW =>        OLDtoNEW
      djds_tbl%NEWtoOLD_DJDS_U => NEWtoOLD_DJDS_U
      djds_tbl%OLDtoNEW_DJDS_L => OLDtoNEW_DJDS_L
      djds_tbl%OLDtoNEW_DJDS_U => OLDtoNEW_DJDS_U
      djds_tbl%LtoU =>            LtoU
      djds_tbl%indexDJDS_L =>     indexDJDS_L
      djds_tbl%indexDJDS_U =>     indexDJDS_U
      djds_tbl%itemDJDS_L =>      itemDJDS_L
      djds_tbl%itemDJDS_U =>      itemDJDS_U
      djds_tbl%NLmaxHYP =>        NLmaxHYP
      djds_tbl%NUmaxHYP =>        NUmaxHYP
      djds_tbl%STACKmcG =>        STACKmcG
      djds_tbl%STACKmc =>         STACKmc
      djds_tbl%NOD_EXPORT_NEW =>  NOD_EXPORT_NEW
!
      end subroutine link_djds_table_2_struct
!
! ------------------------------------------
!
      subroutine link_djds_mat11_2_struct(N, NP, num_non0, istart_diag, &
     &          istart_l, istart_u, aiccg, ALUG_L, ALUG_U, mat11)
!
      integer(kind=kint), intent(in) :: N, NP, num_non0
      integer(kind=kint), intent(in) :: istart_diag, istart_l, istart_u
      real(kind = kreal), target, intent(in) :: aiccg(0:num_non0)
      real(kind = kreal), target, intent(in) :: ALUG_L(N)
      real(kind = kreal), target, intent(in) :: ALUG_U(N)
!
      type(DJDS_MATRIX), intent(inout) :: mat11
!
!
      mat11%num_diag =      NP
      mat11%internal_diag = N
      mat11%istart_diag =   istart_diag
      mat11%istart_l =      istart_l
      mat11%istart_u =      istart_u
      mat11%num_non0 =      num_non0
!
      mat11%aiccg =>  aiccg
      mat11%ALUG_L => ALUG_L
      mat11%ALUG_U => ALUG_U
!
      end subroutine link_djds_mat11_2_struct
!
! ------------------------------------------
!
      subroutine link_djds_mat33_2_struct(N, NP, num_non0, istart_diag, &
     &          istart_l, istart_u, aiccg, ALUG_L, ALUG_U, mat33)
!
      integer(kind=kint), intent(in) :: N, NP, num_non0
      integer(kind=kint), intent(in) :: istart_diag, istart_l, istart_u
      real(kind = kreal), target, intent(in) :: aiccg(-8:9*num_non0)
      real(kind = kreal), target, intent(in) :: ALUG_L(9*N)
      real(kind = kreal), target, intent(in) :: ALUG_U(9*N)
!
      type(DJDS_MATRIX), intent(inout) :: mat33
!
       mat33%num_diag =      NP
       mat33%internal_diag = N
       mat33%istart_diag =   istart_diag
       mat33%istart_u =      istart_u
       mat33%istart_l =      istart_l
       mat33%num_non0 =      num_non0
!
       mat33%aiccg =>  aiccg
       mat33%ALUG_U => ALUG_U
       mat33%ALUG_L => ALUG_L
!
      end subroutine link_djds_mat33_2_struct
!
! ------------------------------------------
!
      subroutine link_djds_matNN_2_struct(N, NP, NB, num_non0,          &
     &          istart_diag, istart_l, istart_u, aiccg, ALUG_L, ALUG_U, &
     &          matNN)
!
      integer(kind=kint), intent(in) :: N, NP, NB, num_non0
      integer(kind=kint), intent(in) :: istart_diag, istart_l, istart_u
      real(kind = kreal), target, intent(in)                            &
     &          :: aiccg(-NB*NB+1:NB*NB*num_non0)
      real(kind = kreal), target, intent(in) :: ALUG_L(NB*NB**N)
      real(kind = kreal), target, intent(in) :: ALUG_U(NB*NB**N)
!
      type(DJDS_MATRIX), intent(inout) :: matNN
!
       matNN%num_diag =      NP
       matNN%internal_diag = N
       matNN%istart_diag =   istart_diag
       matNN%istart_u =      istart_u
       matNN%istart_l =      istart_l
       matNN%num_non0 =      num_non0
!
       matNN%aiccg =>  aiccg
       matNN%ALUG_U => ALUG_U
       matNN%ALUG_L => ALUG_L
!
      end subroutine link_djds_matNN_2_struct
!
! ------------------------------------------
!
      end module link_solver_2_djds_struct
