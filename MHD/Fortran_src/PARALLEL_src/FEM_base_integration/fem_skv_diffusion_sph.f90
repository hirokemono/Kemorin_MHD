!fem_skv_diffusion_sph.f90
!      module fem_skv_diffusion_sph
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine fem_skv_scalar_diffuse(numele, nedge_4_e1, nedge_4_e2,&
!     &          np_smp, iele_fsmp_stack, n_int, k2, ntot_int_3d,       &
!     &          xjac, an1, an2, dnx1, dnx2, ar1, ar2, ltr, j, g3,      &
!     &          ak_d, scalar_e, sk_v)
!      subroutine fem_skv_vector_diffuse(numele, nedge_4_e1, nedge_4_e2,&
!     &          np_smp, iele_fsmp_stack, n_int, k2, ntot_int_3d,       &
!     &          xjac, an1, an2, dnx1, dnx2, ar2, ltr, j, g3, ak_d,     &
!     &          vector_e, sk_v)
!
      module fem_skv_diffusion_sph
!
      use m_precision
      use m_constants
!
      use m_phys_constants
      use m_fem_gauss_int_coefs
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_diffuse(numele, nedge_4_e1, nedge_4_e2, &
     &          np_smp, iele_fsmp_stack, n_int, k2, ntot_int_3d,        &
     &          xjac, an1, an2, dnx1, dnx2, ar1, ar2, ltr, j, g3,       &
     &          ak_d, scalar_e, sk_v)
!
      integer(kind=kint), intent(in) :: numele, nedge_4_e1, nedge_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal),   intent(in) :: xjac(numele, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: an1(nedge_4_e1,ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: an2(nedge_4_e2,ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx1(numele,nedge_4_e1,ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx2(numele,nedge_4_e2,ntot_int_3d)
!
      real (kind=kreal), intent(in) :: ak_d(numele)
      real (kind=kreal), intent(in) :: scalar_e(numele)
      real (kind=kreal), intent(in) :: ar1(nedge_4_e1)
      real (kind=kreal), intent(in) :: ar2(nedge_4_e1)
      integer(kind=kint), intent(in) :: ltr, j
      real (kind=kreal), intent(in) :: g3(ltr)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nedge_4_e1)
!
      real (kind=kreal) :: diffuse2,diffuse0,diffuse1
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist,ied
!
!
!$omp parallel do                                                       &
!$omp&   private(k1,ii,ix,iele,ist,ied,diffuse2,diffuse0,diffuse1)
      do iproc = 1, np_smp
        ist = iele_fsmp_stack(iproc-1)+1
        ied   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int
          ix = int_start1(n_int) + ii
          do k1 = 1, nedge_4_e1
!
!cdir nodep
!voption, indep, vec
            do iele = ist, ied
!
!  ------  set diffusion term
!
              diffuse2 = -dnx1(iele,k1,ix)*dnx2(iele,k2,ix)
              diffuse1 =  two * ar1(k2) * an1(k1,ix) * dnx2(iele,k2,ix)
              diffuse0 = -g3(j) * ar2(k2) * an1(k1,ix) * an2(k2,ix)
!
! -------  caliculate 
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                         + ak_d(iele) * scalar_e(iele)            &
     &                          * (diffuse2 + diffuse0 + diffuse1)      &
     &                         * xjac(iele,ix) * owe(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_scalar_diffuse
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_vector_diffuse(numele, nedge_4_e1, nedge_4_e2, &
     &          np_smp, iele_fsmp_stack, n_int, k2, ntot_int_3d,        &
     &          xjac, an1, an2, dnx1, dnx2, ar2, ltr, j, g3, ak_d,      &
     &          vector_e, sk_v)
!
      integer(kind=kint), intent(in) :: numele, nedge_4_e1, nedge_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal),   intent(in) :: xjac(numele, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: an1(nedge_4_e1,ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: an2(nedge_4_e2,ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx1(numele,nedge_4_e1,ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx2(numele,nedge_4_e2,ntot_int_3d)
!
      real (kind=kreal), intent(in) :: ak_d(numele)
      real (kind=kreal), intent(in) :: vector_e(numele,3)
      real (kind=kreal), intent(in) :: ar2(nedge_4_e1)
      integer(kind=kint), intent(in) :: ltr, j
      real (kind=kreal), intent(in) :: g3(ltr)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nedge_4_e1)
!
      real (kind=kreal) :: diffuse2, diffuse0
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist,ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied,diffuse2,diffuse0)
      do iproc = 1, np_smp
        ist = iele_fsmp_stack(iproc-1)+1
        ied   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int
          ix = int_start1(n_int) + ii
          do k1 = 1, nedge_4_e1
!
!cdir nodep
!voption, indep, vec
            do iele = ist, ied
!
!  ------  set diffusion term
!
              diffuse2 = -dnx1(iele,k1,ix)*dnx2(iele,k2,ix)
              diffuse0 = -g3(j) * ar2(k2) * an1(k1,ix) * an2(k2,ix)
!
! -------  caliculate 
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                         + ak_d(iele) * vector_e(iele,1)          &
     &                          * (diffuse2 + diffuse0)                 &
     &                         * xjac(iele,ix) * owe(ix)
              sk_v(iele,2,k1) = sk_v(iele,2,k1)                         &
     &                         + ak_d(iele) * vector_e(iele,2)          &
     &                          * (diffuse2 + diffuse0)                 &
     &                         * xjac(iele,ix) * owe(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_vector_diffuse
!
! ----------------------------------------------------------------------
!
      end module fem_skv_diffusion_sph
