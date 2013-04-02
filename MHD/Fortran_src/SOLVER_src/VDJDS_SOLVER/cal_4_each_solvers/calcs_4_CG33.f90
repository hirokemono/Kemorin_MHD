!
!      module calcs_4_CG33
!
!     Written by Kemorin
!
!
!      subroutine djds_z_plus_beta_p_33(NP, PEsmpTOT, STACKmcG,         &
!     &          WP, WZ, RHO, RHO1)
!C +-----------------------------+
!C | BETA= RHO / RHO1            |
!C | {p} = {z} + BETA*{p}        |
!C +-----------------------------+
!
!       subroutine djds_x_and_residual_CG_33(NP, PEsmpTOT,              &
!     &           STACKmcG, DNRM, X, R, WP, WQ, ALPHA, DNRM_smp)
!C +----------------------+
!C | {x}= {x} + ALPHA*{p} |
!C | {r}= {r} - ALPHA*{q} |
!C | norm= {r}^2          |
!C +----------------------+
!
      module calcs_4_CG33
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
      subroutine djds_z_plus_beta_p_33(NP, PEsmpTOT, STACKmcG,          &
     &          WP, WZ, RHO, RHO1)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: RHO, RHO1
       real(kind = kreal), intent(in) :: WZ(3*NP)
       real(kind = kreal), intent(inout) :: WP(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, i
       real(kind = kreal) :: BETA
!
!
        BETA= RHO / RHO1
!
!$omp parallel do private(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!voption indep (WP,WZ)
!OCL VECTOR, NOVREC
!cdir nodep
          do i= iS, iE
            WP(3*i-2)= WZ(3*i-2) + BETA*WP(3*i-2)
            WP(3*i-1)= WZ(3*i-1) + BETA*WP(3*i-1)
            WP(3*i  )= WZ(3*i  ) + BETA*WP(3*i  )
          enddo
        enddo
!$omp end parallel do
!
      end subroutine djds_z_plus_beta_p_33
!
!  ---------------------------------------------------------------------
!
       subroutine djds_x_and_residual_CG_33(NP, PEsmpTOT,               &
     &           STACKmcG, DNRM, X, R, WP, WQ, ALPHA, DNRM_smp)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA
       real(kind = kreal), intent(in) :: WP(3*NP), WQ(3*NP)
       real(kind = kreal), intent(inout) :: X(3*NP), R(3*NP)
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
!voption indep (X,R,WP,WQ)
!OCL VECTOR, NOVREC
!cdir nodep
          do i= iS, iE
            X(3*i-2) = X(3*i-2) + ALPHA * WP(3*i-2)
            X(3*i-1) = X(3*i-1) + ALPHA * WP(3*i-1)
            X(3*i  ) = X(3*i  ) + ALPHA * WP(3*i  )
            R(3*i-2) = R(3*i-2) - ALPHA * WQ(3*i-2)
            R(3*i-1) = R(3*i-1) - ALPHA * WQ(3*i-1)
            R(3*i  ) = R(3*i  ) - ALPHA * WQ(3*i  )
            DNRM_smp(ip) = DNRM_smp(ip) + R(3*i-2)*R(3*i-2)             &
     &                                  + R(3*i-1)*R(3*i-1)             &
     &                                  + R(3*i  )*R(3*i  )
          enddo
        enddo
!$omp end parallel do
!
        do ip= 1, PEsmpTOT
          DNRM= DNRM + DNRM_smp(ip)
        enddo
!
       end subroutine djds_x_and_residual_CG_33
!
!  ---------------------------------------------------------------------
!
      end module calcs_4_CG33
