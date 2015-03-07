!
!      module calcs_4_GPBiCG33
!
!     Written by Kemorin on Dec., 2005
!
!      subroutine r_plus_beta_p_sub_u_33(NP, PEsmpTOT, STACKmcG,        &
!     &          WP, R, U, BETA)
!C
!C +----------------------------------+
!C | {p} = {r} + BETA * ( {p} - {u} ) |
!C +----------------------------------+
!
!      subroutine t_sub_ro_sub_alpha_w_ptld_33(NP, PEsmpTOT, STACKmcG,  &
!     &          WY, WT, RO, WW, PT, ALPHA)
!C
!C +------------------------------------------+
!C | {y}= {t} - {r} - ALPHA{w} + ALPHA{p_tld} |
!C | {t}= {r}                  - ALPHA{p_tld} |
!C +------------------------------------------+
!
!      subroutine cal_u_and_z_33(NP, PEsmpTOT, STACKmcG,                &
!     &          WU, WZ, W2, WT0, WR, WU, QSI, ETA, ALPHA, BETA)
!C
!C +----------------------------------------------------------+
!C | {u} = QSI [Minv]{pt} + ETA([Minv]{t0}-[Minv]{r}+BETA*{u} |
!C | {z} = QSI [Minv]{r}  + ETA*{z} - ALPHA*{u}               |
!C +----------------------------------------------------------+
!C===
!
!      subroutine cal_x_and_residual_GPBiCG_33(NP, PEsmpTOT,            &
!     &          STACKmcG, DNRM, COEF, X, WR, WT0, WP, WZ, WT, WY, WTT, &
!     &          WRT, ALPHA, ETA, QSI)
!
!C
!C +--------------------+
!C | update {x},{r},{w} |
!C +--------------------+
!C===
!
!      subroutine tt_add_beta_pt_33(NP, PEsmpTOT, STACKmcG,             &
!     &          W1, WTT, WPT, BETA, RHO, ALPHA, COEF, QSI)
!
!C +---------------------------------+
!C | BETA = ALPHA*COEF / (QSI*RHO)   |
!C | RHO = COEF                      |
!C | {w} = {tt} + BETA*{pt}          |
!C +---------------------------------+
!C===
!
      module calcs_4_GPBiCG33
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
      subroutine r_plus_beta_p_sub_u_33(NP, PEsmpTOT, STACKmcG,         &
     &          WP, R, U, BETA)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: BETA
       real(kind = kreal), intent(in) :: R(3*NP), U(3*NP)
       real(kind = kreal), intent(inout) :: WP(3*NP)
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
!voption indep (WP,R,U)
          do i= iS, iE
            WP(3*i-2)= R(3*i-2) + BETA * ( WP(3*i-2)-U(3*i-2) )
            WP(3*i-1)= R(3*i-1) + BETA * ( WP(3*i-1)-U(3*i-1) )
            WP(3*i  )= R(3*i  ) + BETA * ( WP(3*i  )-U(3*i  ) )
          enddo
        enddo
!$omp end parallel do
!
      end subroutine r_plus_beta_p_sub_u_33
!
!  ---------------------------------------------------------------------
!
      subroutine t_sub_ro_sub_alpha_w_ptld_33(NP, PEsmpTOT, STACKmcG,   &
     &          WY, WT, RO, WW, PT, ALPHA)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA
       real(kind = kreal), intent(in) :: RO(3*NP), WW(3*NP), PT(3*NP)
       real(kind = kreal), intent(inout) :: WY(3*NP), WT(3*NP)
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
!voption indep (WY,WT,RO,WW,PT)
          do i= iS, iE
            WY(3*i-2)= WT(3*i-2)                                        &
     &                - RO(3*i-2) + ALPHA * (-WW(3*i-2)+PT(3*i-2) )
            WY(3*i-1)= WT(3*i-1)                                        &
     &                - RO(3*i-1) + ALPHA * (-WW(3*i-1)+PT(3*i-1) )
            WY(3*i  )= WT(3*i  )                                        &
     &                - RO(3*i  ) + ALPHA * (-WW(3*i  )+PT(3*i  ) )
!
            WT(3*i-2)= RO(3*i-2) - ALPHA * PT(3*i-2)
            WT(3*i-1)= RO(3*i-1) - ALPHA * PT(3*i-1)
            WT(3*i  )= RO(3*i  ) - ALPHA * PT(3*i  )
          enddo
        enddo
!$omp end parallel do
!
      end subroutine t_sub_ro_sub_alpha_w_ptld_33
!
!  ---------------------------------------------------------------------
!
      subroutine cal_u_and_z_33(NP, PEsmpTOT, STACKmcG,                 &
     &          WU, WZ, W2, WT0, WR, QSI, ETA, ALPHA, BETA)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: QSI, ETA, ALPHA, BETA
       real(kind = kreal), intent(in) :: W2(3*NP), WT0(3*NP), WR(3*NP)
       real(kind = kreal), intent(inout) :: WU(3*NP), WZ(3*NP)
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
!voption indep (WU,WZ,W2,WT0,WR,WU)
          do i= iS, iE
            WU(3*i-2)= QSI* W2(3*i-2)                                   &
     &                + ETA*( WT0(3*i-2) - WR(3*i-2) + BETA*WU(3*i-2) )
            WU(3*i-1)= QSI* W2(3*i-1)                                   &
     &                + ETA*( WT0(3*i-1) - WR(3*i-1) + BETA*WU(3*i-1) )
            WU(3*i  )= QSI* W2(3*i  )                                   &
     &                + ETA*( WT0(3*i  ) - WR(3*i  ) + BETA*WU(3*i  ) )
!
            WZ(3*i-2)= QSI* WR(3*i-2)                                   &
     &                + ETA* WZ(3*i-2) - ALPHA*WU(3*i-2)
            WZ(3*i-1)= QSI* WR(3*i-1)                                   &
     &                + ETA* WZ(3*i-1) - ALPHA*WU(3*i-1)
            WZ(3*i  )= QSI* WR(3*i  )                                   &
     &                + ETA* WZ(3*i  ) - ALPHA*WU(3*i  )
          enddo
        enddo
!$omp end parallel do
!
      end subroutine cal_u_and_z_33
!
!  ---------------------------------------------------------------------
!
      subroutine cal_x_and_residual_GPBiCG_33(NP, PEsmpTOT,             &
     &          STACKmcG, DNRM, COEF, X, WR, WT0, WP, WZ, WT, WY, WTT,  &
     &          WRT, ALPHA, ETA, QSI)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA, ETA, QSI
       real(kind = kreal), intent(in) :: WP(3*NP), WZ(3*NP), WY(3*NP)
       real(kind = kreal), intent(in) :: WT(3*NP), WTT(3*NP), WRT(3*NP)
       real(kind = kreal), intent(inout) :: X(3*NP), WR(3*NP)
       real(kind = kreal), intent(inout) :: WT0(3*NP)
       real(kind = kreal), intent(inout) :: DNRM, COEF
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!
       DNRM    = 0.0d0
       COEF    = 0.0d0
!
      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i) reduction(+:DNRM,COEF)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!voption indep (X,WR,WT0,WP,WZ,WT,WY,WTT,WRT)
!cdir nodep
          do i= iS, iE
!
            X(3*i-2) =   X(3*i-2) + ALPHA*WP(3*i-2) + WZ(3*i-2)
            X(3*i-1) =   X(3*i-1) + ALPHA*WP(3*i-1) + WZ(3*i-1)
            X(3*i  ) =   X(3*i  ) + ALPHA*WP(3*i  ) + WZ(3*i  )
!
            WR(3*i-2) =  WT(3*i-2) - ETA*WY(3*i-2) - QSI*WTT(3*i-2)
            WR(3*i-1) =  WT(3*i-1) - ETA*WY(3*i-1) - QSI*WTT(3*i-1)
            WR(3*i  ) =  WT(3*i  ) - ETA*WY(3*i  ) - QSI*WTT(3*i  )
!
            WT0(3*i-2) = WT(3*i-2)
            WT0(3*i-1) = WT(3*i-1)
            WT0(3*i  ) = WT(3*i  )
!
            DNRM = DNRM + WR(3*i-2)*WR(3*i-2)                           &
     &                  + WR(3*i-1)*WR(3*i-1)                           &
     &                  + WR(3*i  )*WR(3*i  )
            COEF = COEF + WR(3*i-2)*WRT(3*i-2)                          &
     &                  + WR(3*i-1)*WRT(3*i-1)                          &
     &                  + WR(3*i  )*WRT(3*i  )
!
          end do
        end do
!$omp end parallel do
!
      end subroutine cal_x_and_residual_GPBiCG_33
!
!  ---------------------------------------------------------------------
!
      subroutine tt_add_beta_pt_33(NP, PEsmpTOT, STACKmcG,              &
     &          W1, WTT, WPT, BETA, RHO, ALPHA, COEF, QSI)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA, COEF, QSI
       real(kind = kreal), intent(inout) :: BETA, RHO
       real(kind = kreal), intent(in) :: WPT(3*NP), WTT(3*NP)
       real(kind = kreal), intent(inout) :: W1(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
       BETA = ALPHA*COEF / (QSI*RHO)
       RHO  = COEF
!
      if(NP .le. 0) return
!
!$omp parallel do private(iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!voption indep (W1,WTT,WPT)
!cdir nodep
          do i= iS, iE
            W1(3*i-2) =  WTT(3*i-2) + BETA*WPT(3*i-2)
            W1(3*i-1) =  WTT(3*i-1) + BETA*WPT(3*i-1)
            W1(3*i  ) =  WTT(3*i  ) + BETA*WPT(3*i  )
          enddo
        enddo
!$omp end parallel do
!
      end subroutine tt_add_beta_pt_33
!
!  ---------------------------------------------------------------------
!
      end module calcs_4_GPBiCG33
