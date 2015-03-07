!
!      module calcs_4_BiCGSTAB
!
!     Written by Kemorin
!
!
!!C +----------------------------------------+
!!C | BETA= (RHO/RHO1) * (ALPHA/OMEGA)       |
!!C | {p} = {r} + BETA * ( {p} - OMEGA*{v} ) |
!!C +----------------------------------------+
!!      subroutine r_plus_beta_p_sub_omega_v_11(NP, PEsmpTOT, STACKmcG, &
!!     &          WP, R, V, OMEGA, ALPHA, RHO, RHO1)
!!      subroutine r_plus_beta_p_sub_omega_v_33(NP, PEsmpTOT, STACKmcG, &
!!     &          WP, R, V, OMEGA, ALPHA, RHO, RHO1)
!!      subroutine r_plus_beta_p_sub_omega_v_nn(NP, NB, PEsmpTOT,       &
!!     &          STACKmcG, WP, R, V, OMEGA, ALPHA, RHO, RHO1)
!!
!!C
!!C +----------------------+
!!C | {s}= {r} - ALPHA*{v} |
!!C +----------------------+
!!      subroutine r_sub_alpha_v_11(NP, PEsmpTOT, STACKmcG,             &
!!     &          S, R, V, ALPHA)
!!      subroutine r_sub_alpha_v_33(NP, PEsmpTOT, STACKmcG,             &
!!     &          S, R, V, ALPHA)
!!      subroutine r_sub_alpha_v_nn(NP, NB, PEsmpTOT, STACKmcG,         &
!!     &          S, R, V, ALPHA)
!!
!!C +----------------+
!!C | update {x},{r} |
!!C +----------------+
!!      subroutine cal_x_and_residual_BiCGSTAB_11(NP, PEsmpTOT,         &
!!     &          STACKmcG, DNRM, X, R, WPT, ST, S, WT, ALPHA, OMEGA)
!!      subroutine cal_x_and_residual_BiCGSTAB_33(NP, PEsmpTOT,         &
!!     &          STACKmcG, DNRM, X, R, WPT, ST, S, WT, ALPHA, OMEGA)
!!      subroutine cal_x_and_residual_BiCGSTAB_nn(NP, NB, PEsmpTOT,     &
!!     &          STACKmcG, DNRM, X, R, WPT, ST, S, WT, ALPHA, OMEGA)
!
      module calcs_4_BiCGSTAB
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
      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
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
      subroutine r_plus_beta_p_sub_omega_v_33(NP, PEsmpTOT, STACKmcG,   &
     &          WP, R, V, OMEGA, ALPHA, RHO, RHO1)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: RHO, RHO1, ALPHA, OMEGA
       real(kind = kreal), intent(in) :: R(3*NP), V(3*NP)
       real(kind = kreal), intent(inout) :: WP(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, i
       real(kind = kreal) :: BETA
!
!
        BETA= (RHO/RHO1) * (ALPHA/OMEGA)
!
      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!cdir nodep
!voption indep (WP,R,V)
          do i= iS, iE
            WP(3*i-2)= R(3*i-2) + BETA * ( WP(3*i-2)-OMEGA*V(3*i-2) )
            WP(3*i-1)= R(3*i-1) + BETA * ( WP(3*i-1)-OMEGA*V(3*i-1) )
            WP(3*i  )= R(3*i  ) + BETA * ( WP(3*i  )-OMEGA*V(3*i  ) )
          enddo
        enddo
!$omp end parallel do
!
      end subroutine r_plus_beta_p_sub_omega_v_33
!
!  ---------------------------------------------------------------------
!
      subroutine r_plus_beta_p_sub_omega_v_nn(NP, NB, PEsmpTOT,         &
     &          STACKmcG, WP, R, V, OMEGA, ALPHA, RHO, RHO1)
!
       integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: RHO, RHO1, ALPHA, OMEGA
       real(kind = kreal), intent(in) :: R(NB*NP), V(NB*NP)
       real(kind = kreal), intent(inout) :: WP(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, i
       real(kind = kreal) :: BETA
!
!
        BETA= (RHO/RHO1) * (ALPHA/OMEGA)
!
      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= NB*STACKmcG(ip-1) + 1
          iE= NB*STACKmcG(ip  )
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
      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
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
      subroutine r_sub_alpha_v_33(NP, PEsmpTOT, STACKmcG,               &
     &          S, R, V, ALPHA)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA
       real(kind = kreal), intent(in) :: R(3*NP), V(3*NP)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!cdir nodep
!voption indep (S,R,V)
          do i= iS, iE
            S(3*i-2)= R(3*i-2) - ALPHA * V(3*i-2)
            S(3*i-1)= R(3*i-1) - ALPHA * V(3*i-1)
            S(3*i  )= R(3*i  ) - ALPHA * V(3*i  )
          enddo
        enddo
!$omp end parallel do
!
      end subroutine r_sub_alpha_v_33
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
      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= NB*STACKmcG(ip-1) + 1
          iE= NB*STACKmcG(ip  )
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
!  ---------------------------------------------------------------------
!
      subroutine cal_x_and_residual_BiCGSTAB_11(NP, PEsmpTOT,           &
     &          STACKmcG, DNRM, X, R, WPT, ST, S, WT, ALPHA, OMEGA)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA, OMEGA
       real(kind = kreal), intent(in) :: WPT(NP), ST(NP)
       real(kind = kreal), intent(in) :: S(NP),  WT(NP)
       real(kind = kreal), intent(inout) :: X(NP), R(NP)
       real(kind = kreal), intent(inout) :: DNRM
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
       DNRM    = 0.0d0
!
      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i) reduction(+:DNRM)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!voption indep (X,R,S,WP,ST,WPT)
!cdir nodep
          do i= iS, iE
            X(i) = X(i) + ALPHA * WPT(i) + OMEGA * ST(i)
            R(i) = S(i) - OMEGA * WT(i)
            DNRM = DNRM + R(i)*R(i)
          enddo
        enddo
!$omp end parallel do
!
      end subroutine cal_x_and_residual_BiCGSTAB_11
!
!  ---------------------------------------------------------------------
!
      subroutine cal_x_and_residual_BiCGSTAB_33(NP, PEsmpTOT,           &
     &          STACKmcG, DNRM, X, R, WPT, ST, S, WT, ALPHA, OMEGA)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA, OMEGA
       real(kind = kreal), intent(in) :: WPT(3*NP), ST(3*NP)
       real(kind = kreal), intent(in) :: S(3*NP),  WT(3*NP)
       real(kind = kreal), intent(inout) :: X(3*NP), R(3*NP)
       real(kind = kreal), intent(inout) :: DNRM
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
       DNRM    = 0.0d0
!
      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i) reduction(+:DNRM)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!voption indep (X,R,S,WP,ST,WPT)
!cdir nodep
          do i= iS, iE
            X(3*i-2) = X(3*i-2) + ALPHA*WPT(3*i-2) + OMEGA*ST(3*i-2)
            X(3*i-1) = X(3*i-1) + ALPHA*WPT(3*i-1) + OMEGA*ST(3*i-1)
            X(3*i  ) = X(3*i  ) + ALPHA*WPT(3*i  ) + OMEGA*ST(3*i  )
            R(3*i-2) = S(3*i-2) - OMEGA * WT(3*i-2)
            R(3*i-1) = S(3*i-1) - OMEGA * WT(3*i-1)
            R(3*i  ) = S(3*i  ) - OMEGA * WT(3*i  )
            DNRM = DNRM + R(3*i-2)*R(3*i-2)                             &
     &                  + R(3*i-1)*R(3*i-1)                             &
     &                  + R(3*i  )*R(3*i  )
          enddo
        enddo
!$omp end parallel do
!
      end subroutine cal_x_and_residual_BiCGSTAB_33
!
!  ---------------------------------------------------------------------
!
      subroutine cal_x_and_residual_BiCGSTAB_nn(NP, NB, PEsmpTOT,       &
     &          STACKmcG, DNRM, X, R, WPT, ST, S, WT, ALPHA, OMEGA)
!
       integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA, OMEGA
       real(kind = kreal), intent(in) :: WPT(NB*NP), ST(NB*NP)
       real(kind = kreal), intent(in) :: S(NB*NP),  WT(NB*NP)
       real(kind = kreal), intent(inout) :: X(NB*NP), R(NB*NP)
       real(kind = kreal), intent(inout) :: DNRM
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
       DNRM    = 0.0d0
!
      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i) reduction(+:DNRM)
        do ip= 1, PEsmpTOT
          iS= NB*STACKmcG(ip-1) + 1
          iE= NB*STACKmcG(ip  )
!voption indep (X,R,S,WP,ST,WPT)
!cdir nodep
            do i= iS, iE
              X(i) = X(i)  + ALPHA * WPT(i) + OMEGA * ST(i)
              R(i) = S(i) - OMEGA * WT(i)
              DNRM = DNRM + R(i)*R(i)
            enddo
        enddo
!$omp end parallel do
!
      end subroutine cal_x_and_residual_BiCGSTAB_nn
!
!  ---------------------------------------------------------------------
!
      end module calcs_4_BiCGSTAB
