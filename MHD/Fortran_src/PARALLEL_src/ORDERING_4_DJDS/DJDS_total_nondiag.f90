!> @file  DJDS_total_nondiag.f90
!!      module DJDS_total_nondiag
!!
!!@author  H. Matsui
!!@date      Written by K. Nakajima in 2001
!!@n        modified by H. Matsui in May, 2002
!!@n        modified by H. Matsui in June, 2006
!!@n        modified by H. Matsui in Jan., 2009
!
!> @brief Work area for multicoloring
!!
!!@verbatim
!!      subroutine set_itotal_djds(np_smp, NP, N, ntot_mc_l, ntot_mc_u, &
!!     &          istack_mc_l, istack_mc_u, item_mc_l, item_mc_u,       &
!!     &          NHYP, npLX1, npUX1, NLmax, NUmax, NLmaxHYP, NUmaxHYP, &
!!     &          itotal_l, itotal_u, indexDJDS_L, indexDJDS_U,         &
!!     &          NEWtoOLDmc, IALmc, IAUmc)
!!@endverbatim
!
      module DJDS_total_nondiag
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
      subroutine set_itotal_djds(np_smp, NP, N, ntot_mc_l, ntot_mc_u,   &
     &          istack_mc_l, istack_mc_u, item_mc_l, item_mc_u,         &
     &          NHYP, npLX1, npUX1, NLmax, NUmax, NLmaxHYP, NUmaxHYP,   &
     &          itotal_l, itotal_u, indexDJDS_L, indexDJDS_U,           &
     &          NEWtoOLDmc, IALmc, IAUmc)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP, N
!
      integer(kind = kint), intent(in) :: ntot_mc_l, ntot_mc_u
      integer(kind = kint), intent(in) :: istack_mc_l(0:NP)
      integer(kind = kint), intent(in) :: istack_mc_u(0:NP)
      integer(kind = kint), intent(in) :: item_mc_l(ntot_mc_l)
      integer(kind = kint), intent(in) :: item_mc_u(ntot_mc_u)
      integer(kind = kint), intent(in) :: NEWtoOLDmc(NP)
!
      integer (kind = kint), intent(in) :: NHYP
      integer (kind = kint), intent(in) :: NLmax, NUmax
      integer (kind = kint), intent(in) :: npLX1, npUX1
      integer (kind = kint), intent(in) :: NLmaxHYP(NHYP)
      integer (kind = kint), intent(in) :: NUmaxHYP(NHYP)
      integer (kind = kint), intent(in)                                 &
     &       :: indexDJDS_L(0:np_smp*NLmax*NHYP)
      integer (kind = kint), intent(in)                                 &
     &       :: indexDJDS_U(0:np_smp*NUmax*NHYP)
!
      integer (kind = kint), intent(inout) :: itotal_l, itotal_u
      integer(kind=kint), intent(inout) :: IALmc(N,NLmax), IAUmc(N,NUmax)
!
!
      integer (kind = kint) :: icouL, icouU
      integer (kind = kint) :: itotal_lnew, itotal_unew
      integer (kind = kint) :: i, ii, ip, iv, j, k, iStart, iSp
      integer (kind = kint) :: kst, ked
!
!
!poption noparallel
!cdir noconcur
      do i= 1, N
        icouL= 0
        icouU= 0
        ii= NEWtoOLDmc(i)
!
        kst = istack_mc_l(ii-1) + 1
        ked = istack_mc_l(ii)
        do k= kst, ked
          j= item_mc_l(k)
          if (j.lt.i) then
            icouL= icouL + 1
            IALmc(i,icouL)= j
           else
            icouU= icouU + 1
            IAUmc(i,icouU)= j
          endif
        enddo
!
        kst = istack_mc_u(ii-1) + 1
        ked = istack_mc_u(ii)
        do k= kst, ked
          j= item_mc_u(k)
          if (j.lt.i) then
            icouL= icouL + 1
            IALmc(i,icouL)= j
           else
            icouU= icouU + 1
            IAUmc(i,icouU)= j
          endif
        enddo
      enddo
!C
!C    REcount number of off-diagonal component 
!C
      itotal_lnew = 0
      itotal_unew = 0
      do iv = 1, NHYP
       do ip = 1, np_smp
!C
!C    lower part
!C
        do j=1, NLmaxHYP(iv)
         iStart = indexDJDS_L(npLX1*(iv-1)+NLmax*(ip-1)+j-1)
         iSp= indexDJDS_L(npLX1*(iv-1)+NLmax*(ip-1)+j)
         itotal_lnew = itotal_lnew + (iSp-iStart)
        end do
!C
!C    upper  part
!C
        do j=1, NUmaxHYP(iv)
         iStart = indexDJDS_U(npUX1*(iv-1)+NUmax*(ip-1)+j-1)
         iSp= indexDJDS_U(npUX1*(iv-1)+NUmax*(ip-1)+j)
         itotal_unew = itotal_unew + (iSp-iStart)
        end do
       end do
      end do
      itotal_l = itotal_lnew
      itotal_u = itotal_unew
!
!
      end subroutine set_itotal_djds
!
! ----------------------------------------------------------------------
!
      end module DJDS_total_nondiag
