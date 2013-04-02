!
!      module calcs_4_CG11
!
!     Written by Kemorin
!
!      subroutine djds_z_plus_beta_p_11(NP, PEsmpTOT, STACKmcG,         &
!     &          WP, WZ, RHO, RHO1)
!<C +-----------------------------+
!<C | {p} = {z} + BETA*{p}        |
!<C +-----------------------------+
!       subroutine djds_x_and_residual_CG_11(NP, PEsmpTOT,              &
!     &           STACKmcG, DNRM, X, R, WP, WQ, ALPHA, DNRM_smp)
!<C +----------------------+
!<C | {x}= {x} + ALPHA*{p} |
!<C | {r}= {r} - ALPHA*{q} |
!<C | norm= {r}^2          |
!<C +----------------------+
!
      module calcs_4_CG11
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine djds_z_plus_beta_p_11(NP, PEsmpTOT, STACKmcG,          &
     &          WP, WZ, RHO, RHO1)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: RHO, RHO1
       real(kind = kreal), intent(in) :: WZ(NP)
       real(kind = kreal), intent(inout) :: WP(NP)
!
       integer (kind = kint) :: ip, iS, iE, i
       real(kind = kreal) :: BETA
!
!
        BETA = RHO / RHO1
!
!$omp parallel do private(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!voption indep (WP,WZ)
!OCL VECTOR, NOVREC
!cdir nodep
          do i= iS, iE
            WP(i)= WZ(i) + BETA * WP(i)
          enddo
        enddo
!$omp end parallel do
!
      end subroutine djds_z_plus_beta_p_11
!
!  ---------------------------------------------------------------------
!
       subroutine djds_x_and_residual_CG_11(NP, PEsmpTOT,               &
     &           STACKmcG, DNRM, X, R, WP, WQ, ALPHA, DNRM_smp)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA
       real(kind = kreal), intent(in) :: WP(NP), WQ(NP)
       real(kind = kreal), intent(inout) :: X(NP), R(NP)
       real(kind = kreal), intent(inout) :: DNRM
       real(kind = kreal), intent(inout) :: DNRM_smp(PEsmpTOT)
       integer (kind = kint) :: ip, iS, iE, i
!
!
       DNRM    = 0.0d0
        do ip= 1, PEsmpTOT
          DNRM_smp(ip)= 0.0d0
        enddo
!
!$omp parallel do private(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!voption indep (X,R,WP,WQ)
!OCL VECTOR, NOVREC
!cdir nodep
          do i= iS, iE
            X(i) = X(i) + ALPHA * WP(i)
            R(i) = R(i) - ALPHA * WQ(i)
            DNRM_smp(ip) = DNRM_smp(ip) + R(i)*R(i)
          enddo
        enddo
!$omp end parallel do
!
        do ip= 1, PEsmpTOT
          DNRM= DNRM + DNRM_smp(ip)
        enddo
!
       end subroutine djds_x_and_residual_CG_11
!
!  ---------------------------------------------------------------------
!
      end module calcs_4_CG11
