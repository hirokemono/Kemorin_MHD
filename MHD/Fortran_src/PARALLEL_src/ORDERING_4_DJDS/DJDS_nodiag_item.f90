!
!     module DJDS_nodiag_item
!
!      Written by K. Nakajima in 2001
!        modified by H. Matsui on May. 2002
!        modified by H. Matsui on June. 2006
!        modified by H. Matsui on Jan. 2009
!
!!      subroutine set_item_djds_type(np_smp, NP, N, djds_tbl)
!!      subroutine set_item_djds(np_smp, NP, N, NHYP, IVECT, COLORon,   &
!!     &      STACKmc, NLmax, NUmax, npLX1, npUX1, NLmaxHYP, NUmaxHYP,  &
!!     &      itotal_l, itotal_u, indexDJDS_L, indexDJDS_U,             &
!!     &      itemDJDS_L, itemDJDS_U, OLDtoNEW, NEWtoOLD,               &
!!     &      NEWtoOLD_DJDS_L, NEWtoOLD_DJDS_U,                         &
!!     &      OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, LtoU)
!
      module DJDS_nodiag_item
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_item_djds_type(np_smp, NP, N, djds_tbl)
!
      use t_solver_djds
!
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: np_smp
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      call set_item_djds(np_smp, NP, N,                                 &
     &      djds_tbl%NHYP, djds_tbl%IVECT, djds_tbl%COLORon,            &
     &      djds_tbl%STACKmc, djds_tbl%NLmax, djds_tbl%NUmax,           &
     &      djds_tbl%npLX1, djds_tbl%npUX1,                             &
     &      djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP,                       &
     &      djds_tbl%itotal_l, djds_tbl%itotal_u,                       &
     &      djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                 &
     &      djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                   &
     &      djds_tbl%OLDtoNEW, djds_tbl%NEWtoOLD,                       &
     &      djds_tbl%NEWtoOLD_DJDS_L, djds_tbl%NEWtoOLD_DJDS_U,         &
     &      djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U,         &
     &      djds_tbl%LtoU)
!
      end subroutine set_item_djds_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_item_djds(np_smp, NP, N, NHYP, IVECT, COLORon,     &
     &      STACKmc, NLmax, NUmax, npLX1, npUX1, NLmaxHYP, NUmaxHYP,    &
     &      itotal_l, itotal_u, indexDJDS_L, indexDJDS_U,               &
     &      itemDJDS_L, itemDJDS_U, OLDtoNEW, NEWtoOLD,                 &
     &      NEWtoOLD_DJDS_L, NEWtoOLD_DJDS_U,                           &
     &      OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, LtoU)
!
      use m_matrix_work
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP, N
!
      integer (kind = kint), intent(in) :: NHYP
      integer (kind = kint), intent(in) :: NLmax, NUmax
      integer (kind = kint), intent(in) :: npLX1, npUX1
      integer (kind = kint), intent(in) :: itotal_l, itotal_u
      integer (kind = kint), intent(in) :: IVECT(0:NHYP)
      integer (kind = kint), intent(in) :: STACKmc(0:np_smp*NHYP)
      integer (kind = kint), intent(in) :: NLmaxHYP(NHYP)
      integer (kind = kint), intent(in) :: NUmaxHYP(NHYP)
      integer (kind = kint), intent(in)                                 &
     &       :: indexDJDS_L(0:np_smp*NLmax*NHYP)
      integer (kind = kint), intent(in)                                 &
     &       :: indexDJDS_U(0:np_smp*NUmax*NHYP)
      integer (kind = kint), intent(in) :: NEWtoOLD_DJDS_L(NP)
      integer (kind = kint), intent(in) :: NEWtoOLD_DJDS_U(NP)
      integer (kind = kint), intent(in) :: OLDtoNEW_DJDS_L(NP)
      integer (kind = kint), intent(in) :: OLDtoNEW_DJDS_U(NP)
!
      integer (kind = kint), intent(inout) :: COLORon(NP)
      integer (kind = kint), intent(inout) :: itemDJDS_L(itotal_l)
      integer (kind = kint), intent(inout) :: itemDJDS_U(itotal_u)
      integer (kind = kint), intent(inout) :: OLDtoNEW(NP)
      integer (kind = kint), intent(inout) :: NEWtoOLD(NP)
      integer (kind = kint), intent(inout) :: LtoU(NP)
!
      integer (kind = kint) :: ip, iv, in, id, j, kk, iStart, iSp
      integer (kind = kint) :: i, i0, i1, i2
!
!
      do iv= 1, NHYP
       do ip= 1, np_smp
        do j= 1, NLmaxHYP(iv)
          iStart = indexDJDS_L(npLX1*(iv-1)+NLmax*(ip-1)+j-1)
          iSp    =  inumDJDS_L(npLX1*(iv-1)+NLmax*(ip-1)+j  )
!voption novec
!cdir novector
          do kk= 1, iSp
            in= NEWtoOLD_DJDS_L(kk+STACKmc((iv-1)*np_smp+ip-1))
            id= iStart + kk
            itemDJDS_L(id)= OLDtoNEW_DJDS_L(IALmc(in,j))
          enddo
         enddo
        enddo
!
        do ip= 1, np_smp
          do j= 1, NUmaxHYP(iv)
            iStart = indexDJDS_U(npUX1*(iv-1)+NUmax*(ip-1)+j-1)
            iSp    =  inumDJDS_U(npUX1*(iv-1)+NUmax*(ip-1)+j  )
!voption novec
!cdir novector
            do kk= 1, iSp
              in= NEWtoOLD_DJDS_U(kk+STACKmc((iv-1)*np_smp+ip-1))
              id= iStart + kk
              itemDJDS_U(id)= OLDtoNEW_DJDS_U(IAUmc(in,j))
            enddo
          enddo
        enddo
!
      end do

!C
!C== HYPER plane ID
  
      do i = 1, N
        COLORon(i)= 0
      enddo

      do iv= 1, NHYP
        do j= IVECT(iv-1)+1, IVECT(iv)
          COLORon(j)= iv
        enddo
      enddo

!C
!C== LtoU

!CDIR NOVECTOR
      do i0= 1, N
        i1= OLDtoNEW_DJDS_L(i0)
        i2= OLDtoNEW_DJDS_U(i0)
        LtoU(i1)= i2
      enddo

      do i= N+1, NP
        LtoU(i)= i
      enddo

!C
!C== OLDtoNEW/NEWtoOLD

!CDIR NOVECTOR
      do i0= 1, N
        i1= OLDtoNEW(i0)
        i2= OLDtoNEWmc(i1)
        NEWtoOLD(i2)= i0
      enddo
      do i0= 1, N
                 i1 = NEWtoOLD(i0)
        OLDtoNEW(i1)= i0
      enddo
!
!
      end subroutine set_item_djds
!
! ----------------------------------------------------------------------
!
      end module DJDS_nodiag_item
