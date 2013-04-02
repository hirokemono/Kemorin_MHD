!
!      module calcs_4_GPBiCG11
!
!     Written by Kemorin on Dec., 2005
!
!      subroutine r_plus_beta_p_sub_u_11(NP, PEsmpTOT, STACKmcG,        &
!     &          WP, R, U, BETA)
!C
!C +----------------------------------+
!C | {p} = {r} + BETA * ( {p} - {u} ) |
!C +----------------------------------+
!
!      subroutine t_sub_ro_sub_alpha_w_ptld_11(NP, PEsmpTOT,            &
!     &          STACKmcG, WY, WT, RO, WW, PT, ALPHA)
!C
!C +------------------------------------------+
!C | {y}= {t} - {r} - ALPHA{w} + ALPHA{p_tld} |
!C | {t}= {r}                  - ALPHA{p_tld} |
!C +------------------------------------------+
!
!      subroutine cal_u_and_z_11(NP, PEsmpTOT, STACKmcG,                &
!     &          WU, WZ, W2, WT0, WR, WU, QSI, ETA, ALPHA, BETA)
!C
!C +----------------------------------------------------------+
!C | {u} = QSI [Minv]{pt} + ETA([Minv]{t0}-[Minv]{r}+BETA*{u} |
!C | {z} = QSI [Minv]{r}  + ETA*{z} - ALPHA*{u}               |
!C +----------------------------------------------------------+
!C===
!
!      subroutine cal_x_and_residual_GPBiCG_11(NP, PEsmpTOT,            &
!     &          STACKmcG, DNRM, COEF, X, WR, WT0, WP, WZ, WT, WY, WTT, &
!     &          WRT, ALPHA, ETA, QSI, DNRM_smp, COEF_smp)
!
!C
!C +--------------------+
!C | update {x},{r},{w} |
!C +--------------------+
!C===
!
!      subroutine tt_add_beta_pt_11(NP, PEsmpTOT, STACKmcG,             &
!     &          W1, WTT, WPT, BETA, RHO, ALPHA, COEF, QSI)
!
!C +---------------------------------+
!C | BETA = ALPHA*COEF / (QSI*RHO)   |
!C | RHO = COEF                      |
!C | {w} = {tt} + BETA*{pt}          |
!C +---------------------------------+
!C===
!
      module calcs_4_GPBiCG11
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
      subroutine r_plus_beta_p_sub_u_11(NP, PEsmpTOT, STACKmcG,         &
     &          WP, R, U, BETA)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: BETA
       real(kind = kreal), intent(in) :: R(NP), U(NP)
       real(kind = kreal), intent(inout) :: WP(NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!cdir parallel do private(iS,iE,i)
!$omp parallel do private(iS,iE,i)
!poption indep (WP,R,U,STACKmcG) tlocal (iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WP,R,U)
          do i= iS, iE
            WP(i)= R(i) + BETA * (WP(i) - U(i))
          enddo
        enddo
!$omp end parallel do
!
      end subroutine r_plus_beta_p_sub_u_11
!
!  ---------------------------------------------------------------------
!
      subroutine t_sub_ro_sub_alpha_w_ptld_11(NP, PEsmpTOT,             &
     &          STACKmcG, WY, WT, RO, WW, PT, ALPHA)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA
       real(kind = kreal), intent(in) :: RO(NP), WW(NP), PT(NP)
       real(kind = kreal), intent(inout) :: WY(NP), WT(NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!cdir parallel do private(iS,iE,i)
!$omp parallel do private(iS,iE,i)
!poption indep (WY,WT,RO,WW,PT,STACKmcG) tlocal (iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WY,WT,RO,WW,PT)
          do i= iS, iE
            WY(i)= WT(i) - RO(i) + ALPHA * (-WW(i)+PT(i) )
            WT(i)= RO(i)         - ALPHA * PT(i)
          enddo
        enddo
!$omp end parallel do
!
      end subroutine t_sub_ro_sub_alpha_w_ptld_11
!
!  ---------------------------------------------------------------------
!
      subroutine cal_u_and_z_11(NP, PEsmpTOT, STACKmcG,                 &
     &          WU, WZ, W2, WT0, WR, QSI, ETA, ALPHA, BETA)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: QSI, ETA, ALPHA, BETA
       real(kind = kreal), intent(in) :: W2(NP), WT0(NP), WR(NP)
       real(kind = kreal), intent(inout) :: WU(NP), WZ(NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!cdir parallel do private(iS,iE,i)
!$omp parallel do private(iS,iE,i)
!poption indep (WU,WZ,W2,WT0,WR,WU,STACKmcG) tlocal (iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (WU,WZ,W2,WT0,WR,WU)
          do i= iS, iE
            WU(i) = QSI * W2(i) + ETA*(WT0(i) - WR(i) + BETA*WU(i) )
            WZ(i) = QSI * WR(i) + ETA* WZ(i) - ALPHA*WU(i)
          enddo
        enddo
!$omp end parallel do
!
      end subroutine cal_u_and_z_11
!
!  ---------------------------------------------------------------------
!
      subroutine cal_x_and_residual_GPBiCG_11(NP, PEsmpTOT,             &
     &          STACKmcG, DNRM, COEF, X, WR, WT0, WP, WZ, WT, WY, WTT,  &
     &          WRT, ALPHA, ETA, QSI, DNRM_smp, COEF_smp)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA, ETA, QSI
       real(kind = kreal), intent(in) :: WP(NP), WZ(NP), WY(NP)
       real(kind = kreal), intent(in) :: WT(NP), WTT(NP), WRT(NP)
       real(kind = kreal), intent(inout) :: X(NP), WR(NP)
       real(kind = kreal), intent(inout) :: WT0(NP)
       real(kind = kreal), intent(inout) :: DNRM, COEF
       real(kind = kreal), intent(inout) :: DNRM_smp(PEsmpTOT)
       real(kind = kreal), intent(inout) :: COEF_smp(PEsmpTOT)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!
       DNRM    = 0.0d0
       COEF    = 0.0d0
       do ip= 1, PEsmpTOT
         DNRM_smp(ip)= 0.0d0
         COEF_smp(ip)= 0.0d0
       enddo
!
!cdir parallel do private(iS,iE,i)
!$omp parallel do private(iS,iE,i)
!poption indep (X,WR,WT0,WP,WZ,WT,WY,WTT,WRT,STACKmcG) tlocal (iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!voption indep (X,WR,WT0,WP,WZ,WT,WY,WTT,WRT)
!OCL VECTOR, NOVREC
!cdir nodep
          do i= iS, iE
!
            X(i) =   X(i) + ALPHA*WP(i) + WZ(i)
            WR(i) =  WT(i) - ETA*WY(i) - QSI*WTT(i)
            WT0(i) = WT(i)
!
            DNRM_smp(ip) = DNRM_smp(ip) + WR(i)*WR(i)
            COEF_smp(ip) = COEF_smp(ip) + WR(i)*WRT(i)
!
          enddo
        enddo
!$omp end parallel do
!
        do ip= 1, PEsmpTOT
          DNRM = DNRM + DNRM_smp(ip)
          COEF = COEF + COEF_smp(ip)
        end do
!
      end subroutine cal_x_and_residual_GPBiCG_11
!
!  ---------------------------------------------------------------------
!
      subroutine tt_add_beta_pt_11(NP, PEsmpTOT, STACKmcG,             &
     &          W1, WTT, WPT, BETA, RHO, ALPHA, COEF, QSI)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       real(kind = kreal), intent(in) :: ALPHA, COEF, QSI
       real(kind = kreal), intent(inout) :: BETA, RHO
       real(kind = kreal), intent(in) :: WPT(NP), WTT(NP)
       real(kind = kreal), intent(inout) :: W1(NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
       BETA = ALPHA*COEF / (QSI*RHO)
       RHO  = COEF
!
!cdir parallel do private(iS,iE,i)
!$omp parallel do private(iS,iE,i)
!poption indep (W1,WTT,WPT,STACKmcG) tlocal (iS,iE,i)
        do ip= 1, PEsmpTOT
          iS= STACKmcG(ip-1) + 1
          iE= STACKmcG(ip  )
!voption indep (W1,WTT,WPT)
!OCL VECTOR, NOVREC
!cdir nodep
          do i= iS, iE
            W1(i) =  WTT(i) + BETA*WPT(i)
          enddo
        enddo
!$omp end parallel do
!
      end subroutine tt_add_beta_pt_11
!
!  ---------------------------------------------------------------------
!
      end module calcs_4_GPBiCG11
