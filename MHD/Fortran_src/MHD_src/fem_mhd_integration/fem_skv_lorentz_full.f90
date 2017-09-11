!
!      module fem_skv_lorentz_full
!
!     programmed by H.Matsui on July 2002
!     modified by H. Matsui on Aug., 2005
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine fem_skv_lorentz_rot                                  &
!!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,&
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, k2, xjac, dnx1, dnx2,             &
!!     &          vect_1, bxe_ex, sk_v)
!!
!!      subroutine fem_skv_lorentz_full_pg                              &
!!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,&
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, k2, xjac, an1, dnx2,              &
!!     &          coef_lor, magne_1, bxe, ex_magne, sk_v)
!!      subroutine fem_skv_lorentz_full_upw                             &
!!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,&
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          ntot_int_3d, n_int, k2, dt, xjac, an1, dnx1, dnx2,    &
!!     &          coef_lor, magne_1, vxe, bxe, ex_magne, sk_v)
!
      module fem_skv_lorentz_full
!
      use m_precision
      use m_constants
      use m_phys_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_lorentz_rot                                    &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,  &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, k2, xjac, dnx1, dnx2,               &
     &          vect_1, bxe_ex, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: np_smp, ntot_int_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: n_int, k2
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in)                                     &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
      real (kind=kreal), intent(in)                                     &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      real (kind=kreal), intent(in) :: vect_1(numele,3)
      real (kind=kreal), intent(in) :: bxe_ex(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal) :: lor_1, lor_2, lor_3
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied,lor_1,lor_2,lor_3)
       do iproc = 1, np_smp
         ist = iele_fsmp_stack(iproc-1)+1
         ied = iele_fsmp_stack(iproc)
!
         do ii= 1, n_int * n_int * n_int 
           ix = int_start3(n_int) + ii
           do k1 = 1, nnod_4_e1
!
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
            do iele = ist, ied
!
!  ------  set inertia term
!
              lor_1 = (dnx1(iele,k1,ix,1)*dnx2(iele,k2,ix,1)            &
     &               + dnx1(iele,k1,ix,2)*dnx2(iele,k2,ix,2)            &
     &               + dnx1(iele,k1,ix,3)*dnx2(iele,k2,ix,3))           &
     &                * (vect_1(iele,3)*bxe_ex(iele,2)                  &
     &                 - vect_1(iele,2)*bxe_ex(iele,3))
              lor_2 = (dnx1(iele,k1,ix,1)*dnx2(iele,k2,ix,1)            &
     &               + dnx1(iele,k1,ix,2)*dnx2(iele,k2,ix,2)            &
     &               + dnx1(iele,k1,ix,3)*dnx2(iele,k2,ix,3))           &
     &                * (vect_1(iele,1)*bxe_ex(iele,3)                  &
     &                 - vect_1(iele,3)*bxe_ex(iele,1))
              lor_3 = (dnx1(iele,k1,ix,1)*dnx2(iele,k2,ix,1)            &
     &               + dnx1(iele,k1,ix,2)*dnx2(iele,k2,ix,2)            &
     &               + dnx1(iele,k1,ix,3)*dnx2(iele,k2,ix,3))           &
     &                * (vect_1(iele,2)*bxe_ex(iele,1)                  &
     &                 - vect_1(iele,1)*bxe_ex(iele,2))
!
! -------  caliculate 
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                         + lor_1 * xjac(iele,ix)  * owe3d(ix)
              sk_v(iele,2,k1) = sk_v(iele,2,k1)                         &
     &                         + lor_2 * xjac(iele,ix)  * owe3d(ix)
              sk_v(iele,3,k1) = sk_v(iele,3,k1)                         &
     &                         + lor_3 * xjac(iele,ix)  * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_lorentz_rot
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine fem_skv_lorentz_full_pg                                &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,  &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, k2, xjac, an1, dnx2,                &
     &          coef_lor, magne_1, bxe, ex_magne, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: np_smp, ntot_int_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: n_int, k2
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in) :: an1(nnod_4_e1,ntot_int_3d)
      real (kind=kreal), intent(in)                                     &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      real (kind=kreal), intent(in) :: coef_lor
      real (kind=kreal), intent(in) :: magne_1(numele,3)
      real (kind=kreal), intent(in) :: bxe(numele,3)
      real (kind=kreal), intent(in) :: ex_magne(3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal) :: m_tension, m_press
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied,m_tension,m_press)
      do iproc = 1, np_smp
        ist = iele_fsmp_stack(iproc-1)+1
        ied   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
            do iele = ist, ied
!
!  ------  set magntic tension
!
              m_tension = an1(k1,ix)                                    &
     &               * ( (bxe(iele,1)+ex_magne(1))*dnx2(iele,k2,ix,1)   &
     &                 + (bxe(iele,2)+ex_magne(2))*dnx2(iele,k2,ix,2)   &
     &                 + (bxe(iele,3)+ex_magne(3))*dnx2(iele,k2,ix,3))
!
!  ------  set magntic pressure
!
              m_press = -an1(k1,ix)                                     &
     &                    * ( half * (magne_1(iele,1)+ex_magne(1))**2   &
     &                             + (magne_1(iele,2)+ex_magne(2))**2   &
     &                             + (magne_1(iele,3)+ex_magne(3))**2)
!
! -------  caliculate
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                     + coef_lor * (m_tension * magne_1(iele,1)    &
     &                                  + m_press * dnx2(iele,k2,ix,1)) &
     &                      * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                     + coef_lor * (m_tension * magne_1(iele,2)    &
     &                                  + m_press * dnx2(iele,k2,ix,2)) &
     &                      * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                     + coef_lor * (m_tension * magne_1(iele,3)    &
     &                                  + m_press * dnx2(iele,k2,ix,3)) &
     &                      * xjac(iele,ix) * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_lorentz_full_pg
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_lorentz_full_upw                               &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,  &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          ntot_int_3d, n_int, k2, dt, xjac, an1, dnx1, dnx2,      &
     &          coef_lor, magne_1, vxe, bxe, ex_magne, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: np_smp, ntot_int_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real (kind=kreal), intent(in) :: an1(nnod_4_e1,ntot_int_3d)
      real (kind=kreal), intent(in)                                     &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
      real (kind=kreal), intent(in)                                     &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      real (kind=kreal), intent(in) :: coef_lor, dt
      real (kind=kreal), intent(in) :: magne_1(numele,3)
      real (kind=kreal), intent(in) :: bxe(numele,3)
      real (kind=kreal), intent(in) :: vxe(numele,3)
      real (kind=kreal), intent(in) :: ex_magne(3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal) :: tau, m_tension, m_press
!
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied,m_press,m_tension,tau)
      do iproc = 1, np_smp
        ist = iele_fsmp_stack(iproc-1)+1
        ied = iele_fsmp_stack(iproc)
!
        do k1 = 1, nnod_4_e1
          do ii= 1, n_int * n_int * n_int 
            ix = int_start3(n_int) + ii
!
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
            do iele = ist, ied
!
!  -----  set weighting function
!
               tau = an1(k1,ix) + half * dt                             &
     &                  * ( vxe(iele,1)*dnx1(iele,k1,ix,1)              &
     &                    + vxe(iele,2)*dnx1(iele,k1,ix,2)              &
     &                    + vxe(iele,3)*dnx1(iele,k1,ix,3) )
!
!  ------  set magntic tension
!
              m_tension = tau                                           &
     &               * ( (bxe(iele,1)+ex_magne(1))*dnx2(iele,k2,ix,1)   &
     &                 + (bxe(iele,2)+ex_magne(2))*dnx2(iele,k2,ix,2)   &
     &                 + (bxe(iele,3)+ex_magne(3))*dnx2(iele,k2,ix,3))
!
!  ------  set magntic pressure
!
              m_press =-tau * (half * (magne_1(iele,1)+ex_magne(1))**2  &
     &                              + (magne_1(iele,2)+ex_magne(2))**2  &
     &                              + (magne_1(iele,3)+ex_magne(3))**2)
!
! -------  caliculate
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                     + coef_lor * (m_tension * magne_1(iele,1)    &
     &                                  + m_press * dnx2(iele,k2,ix,1)) &
     &                      * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                     + coef_lor * (m_tension * magne_1(iele,2)    &
     &                                  + m_press * dnx2(iele,k2,ix,2)) &
     &                      * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                     + coef_lor * (m_tension * magne_1(iele,3)    &
     &                                  + m_press * dnx2(iele,k2,ix,3)) &
     &                      * xjac(iele,ix) * owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_lorentz_full_upw
!
! ----------------------------------------------------------------------
!
      end module fem_skv_lorentz_full
