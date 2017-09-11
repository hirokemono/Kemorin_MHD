!
!     module fem_skv_induction
!
!     numerical integration for finite elememt equations of induction
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!!      subroutine cal_mag_induct_vec_nod                               &
!!     &         (numnod, np_smp, inod_smp_stack, ex_magne, coef_uxb,   &
!!     &          ml_o_cd, velo, magne, ff)
!!
!!      subroutine fem_skv_induction_pg                                 &
!!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,&
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, k2, xjac, an1, dnx2,              &
!!     &          velo_1, magne_1, vxe, bxe_ex, coef_uxb, sk_v)
!!      subroutine fem_skv_induction_upm                                &
!!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,&
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, k2, dt, xjac, an1, dnx1, dnx2,    &
!!     &          velo_1, magne_1, vxe, bxe_ex, bxe_up, coef_uxb, sk_v)
!
      module fem_skv_induction
!
      use m_precision
      use m_constants
      use m_phys_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_mag_induct_vec_nod                                 &
     &         (numnod, np_smp, inod_smp_stack, ex_magne, coef_uxb,     &
     &          ml_o_cd, velo, magne, ff)
!
      integer (kind=kint), intent(in) :: numnod, np_smp
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef_uxb
      real (kind=kreal), intent(in) :: velo(numnod,3)
      real (kind=kreal), intent(in) :: magne(numnod,3)
      real (kind=kreal), intent(in) :: ex_magne(3)
      real (kind=kreal), intent(in) :: ml_o_cd(numnod)
!
      real (kind=kreal), intent(inout) :: ff(numnod,3)
!
      integer (kind=kint) :: iproc, inod
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!cdir nodep
!voption, indep, vec
        do inod = ist, ied
          ff(inod,1) = ff(inod,1)                                       &
     &      + ( (ex_magne(2)+magne(inod,2)) * velo(inod,1)              &
     &        + (ex_magne(3)+magne(inod,3)) * velo(inod,1)              &
     &        - velo(inod,2) * (ex_magne(1)+magne(inod,1))              &
     &        - velo(inod,3) * (ex_magne(1)+magne(inod,1)) )            &
     &       * coef_uxb * ml_o_cd(inod)
          ff(inod,2) = ff(inod,2)                                       &
     &      + ( (ex_magne(1)+magne(inod,1)) * velo(inod,2)              &
     &        + (ex_magne(3)+magne(inod,3)) * velo(inod,2)              &
     &        - velo(inod,1) * (ex_magne(2)+magne(inod,2))              &
     &        - velo(inod,3) * (ex_magne(2)+magne(inod,2)) )            &
     &       * coef_uxb * ml_o_cd(inod)
          ff(inod,3) = ff(inod,3)                                       &
     &      + ( (ex_magne(1)+magne(inod,1)) * velo(inod,3)              &
     &        + (ex_magne(2)+magne(inod,2)) * velo(inod,3)              &
     &        - velo(inod,1) * (ex_magne(3)+magne(inod,3))              &
     &        - velo(inod,2) * (ex_magne(3)+magne(inod,3)) )            &
     &       * coef_uxb * ml_o_cd(inod)

        end do
      end do
!$omp end parallel do
!
      end subroutine cal_mag_induct_vec_nod
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_induction_pg                                   &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,  &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, k2, xjac, an1, dnx2,                &
     &          velo_1, magne_1, vxe, bxe_ex, coef_uxb, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: np_smp, n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer(kind=kint), intent(in) :: ntot_int_3d
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal),   intent(in) :: an1(nnod_4_e1, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      real (kind=kreal), intent(in) :: velo_1(numele,3)
      real (kind=kreal), intent(in) :: magne_1(numele,3)
      real (kind=kreal), intent(in) :: vxe(numele,3)
      real (kind=kreal), intent(in) :: bxe_ex(numele,3)
      real (kind=kreal), intent(in) :: coef_uxb
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
!
      real (kind=kreal) :: coef1, coef2
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied,coef1,coef2)
      do iproc = 1, np_smp
       ist = iele_fsmp_stack(iproc-1)+1
       ied = iele_fsmp_stack(iproc)
!
       do ii= 1, n_int * n_int * n_int 
        ix = int_start3(n_int) + ii
        do k1 = 1, nnod_4_e1
!
!cdir nodep
         do iele = ist, ied
!  ------  set inertia term
!
              coef1 = an1(k1,ix) * (vxe(iele,1)*dnx2(iele,k2,ix,1)      &
     &                            + vxe(iele,2)*dnx2(iele,k2,ix,2)      &
     &                            + vxe(iele,3)*dnx2(iele,k2,ix,3) )
!
!  ------  set magntic tension
!
              coef2 = an1(k1,ix) * (bxe_ex(iele,1)*dnx2(iele,k2,ix,1)   &
     &                            + bxe_ex(iele,2)*dnx2(iele,k2,ix,2)   &
     &                            + bxe_ex(iele,3)*dnx2(iele,k2,ix,3))
!
! -------  caliculate 
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                         + (-coef1 * magne_1(iele,1)              &
     &                           + coef2 * velo_1(iele,1) )             &
     &                         * coef_uxb * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,2,k1) = sk_v(iele,2,k1)                         &
     &                         + (-coef1 * magne_1(iele,2)              &
     &                           + coef2 * velo_1(iele,2) )             &
     &                         * coef_uxb * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,3,k1) = sk_v(iele,3,k1)                         &
     &                         + (-coef1 * magne_1(iele,3)              &
     &                           + coef2 * velo_1(iele,3) )             &
     &                         * coef_uxb * xjac(iele,ix) * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_induction_pg
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_induction_upm                                  &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,  &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, k2, dt, xjac, an1, dnx1, dnx2,      &
     &          velo_1, magne_1, vxe, bxe_ex, bxe_up, coef_uxb, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: np_smp, n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer(kind=kint), intent(in) :: ntot_int_3d
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal),   intent(in) :: an1(nnod_4_e1, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      real (kind=kreal), intent(in) :: velo_1(numele,3)
      real (kind=kreal), intent(in) :: magne_1(numele,3)
      real (kind=kreal), intent(in) :: vxe(numele,3)
      real (kind=kreal), intent(in) :: bxe_ex(numele,3)
      real (kind=kreal), intent(in) :: bxe_up(numele,3)
      real (kind=kreal), intent(in) :: dt, coef_uxb
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
!
      real (kind=kreal) :: coef1, coef2, tau
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied,coef1,coef2,tau)
      do iproc = 1, np_smp
        ist = iele_fsmp_stack(iproc-1)+1
        ied = iele_fsmp_stack(iproc)
!
        do k1 = 1, nnod_4_e1
          do ii= 1, n_int * n_int * n_int 
            ix = int_start3(n_int) + ii
!
!cdir nodep
            do iele = ist, ied
!
!  -----  set weighting function
!
              tau = an1(k1,ix) + half * dt                              &
     &                  * ( bxe_up(iele,1)*dnx1(iele,k1,ix,1)           &
     &                    + bxe_up(iele,2)*dnx1(iele,k1,ix,2)           &
     &                    + bxe_up(iele,3)*dnx1(iele,k1,ix,3) )
!
!  ------  set inertia term
!
              coef1 = tau * (vxe(iele,1)*dnx2(iele,k2,ix,1)             &
     &                     + vxe(iele,2)*dnx2(iele,k2,ix,2)             &
     &                     + vxe(iele,3)*dnx2(iele,k2,ix,3) )
!
!  ------  set magntic tension
!
              coef2 = tau * (bxe_ex(iele,1)*dnx2(iele,k2,ix,1)          &
     &                    +  bxe_ex(iele,2)*dnx2(iele,k2,ix,2)          &
     &                    +  bxe_ex(iele,3)*dnx2(iele,k2,ix,3) )
!
! -------  caliculate 
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                         + (-coef1 * magne_1(iele,1)              &
     &                           + coef2 * velo_1(iele,1) )             &
     &                         * coef_uxb * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,2,k1) = sk_v(iele,2,k1)                         &
     &                         + (-coef1 * magne_1(iele,2)              &
     &                           + coef2 * velo_1(iele,2) )             &
     &                         * coef_uxb * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,3,k1) = sk_v(iele,3,k1)                         &
     &                         + (-coef1 * magne_1(iele,3)              &
     &                           + coef2 * velo_1(iele,3) )             &
     &                         * coef_uxb * xjac(iele,ix) * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_induction_upm
!
!-----------------------------------------------------------------------
!
      end module fem_skv_induction
