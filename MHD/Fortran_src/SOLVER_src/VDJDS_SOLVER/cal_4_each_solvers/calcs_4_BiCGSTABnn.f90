!
!      module calcs_4_BiCGSTABnn
!
!     Written by Kemorin
!
!      subroutine r_plus_beta_p_sub_omega_v_nn(NP, NB, PEsmpTOT,        &
!     &          STACKmcG, WP, R, V, BETA, OMEGA, ALPHA, RHO, RHO1)
!
!C +----------------------------------------+
!C | BETA= (RHO/RHO1) * (ALPHA/OMEGA)       |
!C | {p} = {r} + BETA * ( {p} - OMEGA*{v} ) |
!C +----------------------------------------+
!
!      subroutine r_sub_alpha_v_nn(NP, NB, PEsmpTOT, STACKmcG,          &
!     &          S, R, V, ALPHA)
!C
!C +----------------------+
!C | {s}= {r} - ALPHA*{v} |
!C +----------------------+
!
!      subroutine cal_x_and_residual_BiCGSTAB_nn(NP, NB, PEsmpTOT,      &
!     &          STACKmcG, DNRM, X, R, WPT, ST, S, WT,                  &
!     &          ALPHA, OMEGA, DNRM_smp)
!
!C +----------------+
!C | update {x},{r} |
!C +----------------+
!
      module calcs_4_BiCGSTABnn
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
      subroutine r_plus_beta_p_sub_omega_v_nn(NP, NB, PEsmpTOT,         &
     &          STACKmcG, WP, R, V, BETA, OMEGA, ALPHA, RHO, RHO1)
!
       integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: RHO, RHO1, ALPHA, OMEGA
       real(kind = kreal), intent(in) :: R(NB*NP), V(NB*NP)
       real(kind = kreal), intent(inout) :: WP(NB*NP)
       real(kind = kreal), intent(inout) :: BETA
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
        BETA= (RHO/RHO1) * (ALPHA/OMEGA)
!
!$omp parallel do private(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= NB*STACKmcG(ip-1) + 1
          iE= NB*STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WP,R,V)
            do i= iS, iE
              WP(i) = R(i) + BETA * ( WP(i) - OMEGA*V(i) )
            end do
        end do
!$omp end parallel do
!
      end subroutine r_plus_beta_p_sub_omega_v_nn
!
!  ---------------------------------------------------------------------
!
      subroutine r_sub_alpha_v_nn(NP, NB, PEsmpTOT, STACKmcG,           &
     &          S, R, V, ALPHA)
!
       integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA
       real(kind = kreal), intent(in) :: R(NB*NP), V(NB*NP)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= NB*STACKmcG(ip-1) + 1
          iE= NB*STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S,R,V)
            do i= iS, iE
              S(i)= R(i) - ALPHA * V(i)
            enddo
        enddo
!$omp end parallel do
!
      end subroutine r_sub_alpha_v_nn
!
!  ---------------------------------------------------------------------
!
      subroutine cal_x_and_residual_BiCGSTAB_nn(NP, NB, PEsmpTOT,       &
     &          STACKmcG, DNRM, X, R, WPT, ST, S, WT,                   &
     &          ALPHA, OMEGA, DNRM_smp)
!
       integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA, OMEGA
       real(kind = kreal), intent(in) :: WPT(NB*NP), ST(NB*NP)
       real(kind = kreal), intent(in) :: S(NB*NP),  WT(NB*NP)
       real(kind = kreal), intent(inout) :: X(NB*NP), R(NB*NP)
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
          iS= NB*STACKmcG(ip-1) + 1
          iE= NB*STACKmcG(ip  )
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
      end subroutine cal_x_and_residual_BiCGSTAB_nn
!
!  ---------------------------------------------------------------------
!
      end module calcs_4_BiCGSTABnn
