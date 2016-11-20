!> @file  DJDS_hyperplane.f90
!!      module DJDS_hyperplane
!!
!! @author  H. Matsui
!>@brief    Written by K. Nakajima in 2001
!!@n        modified by H. Matsui in May. 2002
!!@n        modified by H. Matsui in June. 2006
!!@n        modified by H. Matsui in Jan., 2009
!
!> @brief Work area for multicoloring
!!
!!@verbatim
!!      subroutine count_hyperplane(np_smp, NP, N,                      &
!!     &          ntot_mc_l, ntot_mc_u, num_mc_l, num_mc_u,             &
!!     &          istack_mc_l, istack_mc_u, item_mc_l, item_mc_u,       &
!!     &          NHYP, IVECT, npLX1, npUX1, NLmax, NUmax,              &
!!     &          NLmaxHYP, NUmaxHYP, itotal_l, itotal_u)
!!@endverbatim
!
      module DJDS_hyperplane
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
      subroutine count_hyperplane(np_smp, NP, N,                        &
     &          ntot_mc_l, ntot_mc_u, num_mc_l, num_mc_u,               &
     &          istack_mc_l, istack_mc_u, item_mc_l, item_mc_u,         &
     &          NHYP, IVECT, npLX1, npUX1, NLmax, NUmax,                &
     &          NLmaxHYP, NUmaxHYP, itotal_l, itotal_u)
!
      use m_matrix_work
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP, N
!
      integer (kind = kint), intent(in) :: NHYP
!
      integer(kind = kint), intent(in) :: ntot_mc_l, ntot_mc_u
      integer(kind = kint), intent(in) :: num_mc_l(NP)
      integer(kind = kint), intent(in) :: num_mc_u(NP)
      integer(kind = kint), intent(in) :: istack_mc_l(0:NP)
      integer(kind = kint), intent(in) :: istack_mc_u(0:NP)
      integer(kind = kint), intent(inout) :: item_mc_l(ntot_mc_l)
      integer(kind = kint), intent(inout) :: item_mc_u(ntot_mc_u)
!
      integer (kind = kint), intent(inout) :: itotal_l, itotal_u
      integer (kind = kint), intent(inout) :: NLmax, NUmax
      integer (kind = kint), intent(inout) :: npLX1, npUX1
      integer (kind = kint), intent(inout) :: IVECT(0:NHYP)
      integer (kind = kint), intent(inout) :: NLmaxHYP(NHYP)
      integer (kind = kint), intent(inout) :: NUmaxHYP(NHYP)
!
      integer (kind = kint) :: icouL, icouU
      integer (kind = kint) :: i, j, k, ic, in
      integer (kind = kint) :: iStart, iEnd, kst, ked
      integer (kind = kint) :: icouLmax, icouUmax
!
!
!
      NLmax = -NP
      NUmax = -NP
      itotal_l= 0
      itotal_u= 0
      do ic= 1, NHYP
        iStart= IVECmc(ic-1) + 1
        iEnd= IVECmc(ic  )
        icouLmax= -N
        icouUmax= -N
        do i= iStart, iEnd
          icouL= 0
          icouU= 0
          in= NEWtoOLDmc(i)
!
          kst = istack_mc_l(in-1) + 1
          ked = istack_mc_l(in)
          do k= kst, ked
            j= OLDtoNEWmc( item_mc_l(k) )
            item_mc_l(k)= j
            if (j.lt.i) icouL= icouL + 1
            if (j.gt.i) icouU= icouU + 1
          enddo
!
          kst = istack_mc_u(in-1) + 1
          ked = istack_mc_u(in)
          do k= kst, ked
            j= OLDtoNEWmc( item_mc_u(k) )
            item_mc_u(k)= j
            if (j.lt.i) icouL= icouL + 1
            if (j.gt.i) icouU= icouU + 1
          enddo
!
          INLmc(i)= icouL
          INUmc(i)= icouU
          itotal_l= itotal_l + icouL
          itotal_u= itotal_u + icouU
          icouLmax= max (icouLmax, icouL)
          icouUmax= max (icouUmax, icouU)
        enddo
        NLmaxHYP(ic)= icouLmax
        NUmaxHYP(ic)= icouUmax
        NLmax       = max (icouLmax, NLmax)
        NUmax       = max (icouUmax, NUmax)
      enddo
!
!CDIR NODEP
      do i= N+1, NP
        INLmc(i)= num_mc_l(i)
        INUmc(i)= num_mc_u(i)
      enddo
!
!C
!C-- TRANSFER

      npLX1= np_smp*NLmax
      npUX1= np_smp*NUmax
!
!
      do i= 0, NHYP
        IVECT(i)= IVECmc(i)
      enddo
!
!
      end subroutine count_hyperplane
!
! ----------------------------------------------------------------------
!
      end module DJDS_hyperplane
