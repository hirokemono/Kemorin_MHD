!
!     module DJDS_hyperplane
!
!      Written by K. Nakajima in 2001
!        modified by H. Matsui on May. 2002
!        modified by H. Matsui on June. 2006
!        modified by H. Matsui on Jan., 2009
!
!      subroutine count_hyperplane_type(np_smp, N, NP, djds_tbl)
!      subroutine count_hyperplane(np_smp, NP, N, NHYP, IVECT,          &
!     &          npLX1, npUX1, NLmax, NUmax, NLmaxHYP, NUmaxHYP,        &
!     &          itotal_l, itotal_u)
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
      subroutine count_hyperplane_type(np_smp, NP, N, djds_tbl)
!
      use t_solver_djds
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: NP, N
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      call count_hyperplane(np_smp, NP, N,                              &
     &                  djds_tbl%NHYP, djds_tbl%IVECT,                  &
     &                  djds_tbl%npLX1, djds_tbl%npUX1,                 &
     &                  djds_tbl%NLmax, djds_tbl%NUmax,                 &
     &                  djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP,           &
     &                  djds_tbl%itotal_l, djds_tbl%itotal_u)
!
      end subroutine count_hyperplane_type
!
! ----------------------------------------------------------------------
!
      subroutine count_hyperplane(np_smp, NP, N, NHYP, IVECT,           &
     &          npLX1, npUX1, NLmax, NUmax, NLmaxHYP, NUmaxHYP,         &
     &          itotal_l, itotal_u)
!
      use m_matrix_work
      use m_colored_connect
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP, N
!
      integer (kind = kint), intent(in) :: NHYP
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
