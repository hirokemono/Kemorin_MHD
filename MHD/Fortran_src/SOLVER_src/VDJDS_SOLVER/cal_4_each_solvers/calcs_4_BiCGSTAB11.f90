!
!      module calcs_4_BiCGSTAB11
!
!     Written by Kemorin
!
!      subroutine r_plus_beta_p_sub_omega_v_11(NP, PEsmpTOT, STACKmcG,  &
!     &          WP, R, V, OMEGA, ALPHA, RHO, RHO1)
!      subroutine r_sub_alpha_v_11(NP, PEsmpTOT, STACKmcG,              &
!     &          S, R, V, ALPHA)
!      subroutine cal_x_and_residual_BiCGSTAB_11(NP, PEsmpTOT,          &
!     &          STACKmcG, DNRM, X, R, WPT, ST, S, WT,                  &
!     &          ALPHA, OMEGA, DNRM_smp)
!
      module calcs_4_BiCGSTAB11
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
      subroutine r_plus_beta_p_sub_omega_v_11(NP, PEsmpTOT, STACKmcG,   &
     &          WP, R, V, OMEGA, ALPHA, RHO, RHO1)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: RHO, RHO1, ALPHA, OMEGA
       real(kind = kreal), intent(in) :: R(NP), V(NP)
       real(kind = kreal), intent(inout) :: WP(NP)
!
       integer (kind = kint) :: ip, iS, iE, i
       real(kind = kreal) :: BETA
!
!
        BETA = (RHO/RHO1) * (ALPHA/OMEGA)
!
!$omp parallel do private(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WP,R,V)
          do i= iS, iE
            WP(i) = R(i) + BETA * ( WP(i) - OMEGA*V(i) )
          enddo
        enddo
!$omp end parallel do
!
      end subroutine r_plus_beta_p_sub_omega_v_11
!
!  ---------------------------------------------------------------------
!
      subroutine r_sub_alpha_v_11(NP, PEsmpTOT, STACKmcG,               &
     &          S, R, V, ALPHA)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA
       real(kind = kreal), intent(in) :: R(NP), V(NP)
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S,R,V)
          do i= iS, iE
            S(i)= R(i) - ALPHA * V(i)
          enddo
        enddo
!$omp end parallel do
!
      end subroutine r_sub_alpha_v_11
!
!  ---------------------------------------------------------------------
!
      subroutine cal_x_and_residual_BiCGSTAB_11(NP, PEsmpTOT,           &
     &          STACKmcG, DNRM, X, R, WPT, ST, S, WT,                   &
     &          ALPHA, OMEGA, DNRM_smp)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA, OMEGA
       real(kind = kreal), intent(in) :: WPT(NP), ST(NP)
       real(kind = kreal), intent(in) :: S(NP),  WT(NP)
       real(kind = kreal), intent(inout) :: X(NP), R(NP)
       real(kind = kreal), intent(inout) :: DNRM
       real(kind = kreal), intent(inout) :: DNRM_smp(PEsmpTOT)
!
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
!voption indep (X,R,S,WP,ST,WPT)
!OCL VECTOR, NOVREC
!cdir nodep
          do i= iS, iE
            X(i) = X(i)  + ALPHA * WPT(i) + OMEGA * ST(i)
            R(i) = S(i) - OMEGA * WT(i)
            DNRM_smp(ip) = DNRM_smp(ip) + R(i)*R(i)
          enddo
        enddo
!$omp end parallel do
!
        do ip= 1, PEsmpTOT
          DNRM= DNRM + DNRM_smp(ip)
        enddo
!
      end subroutine cal_x_and_residual_BiCGSTAB_11
!
!  ---------------------------------------------------------------------
!
      end module calcs_4_BiCGSTAB11
