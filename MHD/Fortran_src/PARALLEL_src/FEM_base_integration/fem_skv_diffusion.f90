!
!      module fem_skv_diffusion
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine fem_skv_scalar_diffuse(numele, nnod_4_e1, nnod_4_e2,  &
!     &          np_smp, iele_fsmp_stack, n_int, k2, ntot_int_3d,       &
!     &          xjac, dnx1, dnx2, ak_d, scalar_e, sk_v)
!      subroutine fem_skv_vector_diffuse(numele, nnod_4_e1, nnod_4_e2,  &
!     &          np_smp, iele_fsmp_stack, n_int, k2, ntot_int_3d,       &
!     &          xjac, dnx1, dnx2, ak_d, vector_e, sk_v)
!
!      subroutine fem_skv_poisson(numele, nnod_4_e1, nnod_4_e2,         &
!     &          np_smp, iele_smp_stack,  num_int, k2, ntot_int_3d,     &
!     &          xjac, dnx1, dnx2, sk_v)
!
      module fem_skv_diffusion
!
      use m_precision
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
      subroutine fem_skv_scalar_diffuse(numele, nnod_4_e1, nnod_4_e2,   &
     &          np_smp, iele_fsmp_stack, n_int, k2, ntot_int_3d,        &
     &          xjac, dnx1, dnx2, ak_d, scalar_e, sk_v)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_3d
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int, k2
!
      real(kind=kreal),   intent(in) :: xjac(numele, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      real (kind=kreal), intent(in) :: ak_d(numele)
      real (kind=kreal), intent(in) :: scalar_e(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal) :: diffuse
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist,ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied,diffuse)
      do iproc = 1, np_smp
        ist = iele_fsmp_stack(iproc-1)+1
        ied   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!voption, indep, vec
            do iele = ist, ied
!
!  ------  set diffusion term
!
              diffuse = - ak_d(iele)                                    &
     &          * ( dnx1(iele,k1,ix,1)*dnx2(iele,k2,ix,1)               &
     &            + dnx1(iele,k1,ix,2)*dnx2(iele,k2,ix,2)               &
     &            + dnx1(iele,k1,ix,3)*dnx2(iele,k2,ix,3) )
!
! -------  caliculate 
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                    + diffuse * scalar_e(iele)                    &
     &                     * xjac(iele,ix) * owe3d(ix)
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
      subroutine fem_skv_vector_diffuse(numele, nnod_4_e1, nnod_4_e2,   &
     &          np_smp, iele_fsmp_stack, n_int, k2, ntot_int_3d,        &
     &          xjac, dnx1, dnx2, ak_d, vector_e, sk_v)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_3d
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int, k2
!
      real(kind=kreal),   intent(in) :: xjac(numele, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      real (kind=kreal), intent(in) :: ak_d(numele)
      real (kind=kreal), intent(in) :: vector_e(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal) :: diffuse
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist,ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied,diffuse)
      do iproc = 1, np_smp
        ist = iele_fsmp_stack(iproc-1)+1
        ied = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!voption, indep, vec
            do iele = ist, ied
!
!  ------  set diffusion term
!
              diffuse = - ak_d(iele)                                    &
     &          * ( dnx1(iele,k1,ix,1)*dnx2(iele,k2,ix,1)               &
     &            + dnx1(iele,k1,ix,2)*dnx2(iele,k2,ix,2)               &
     &            + dnx1(iele,k1,ix,3)*dnx2(iele,k2,ix,3) )
!
! -------  caliculate 
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                    + diffuse * vector_e(iele,1)                  &
     &                     * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,2,k1) = sk_v(iele,2,k1)                         &
     &                    + diffuse * vector_e(iele,2)                  &
     &                     * xjac(iele,ix) * owe3d(ix)
              sk_v(iele,3,k1) = sk_v(iele,3,k1)                         &
     &                    + diffuse * vector_e(iele,3)                  &
     &                     * xjac(iele,ix) * owe3d(ix)
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
! ----------------------------------------------------------------------
!
      subroutine fem_skv_poisson(numele, nnod_4_e1, nnod_4_e2,          &
     &          np_smp, iele_smp_stack, num_int, k2, ntot_int_3d,       &
     &          xjac, dnx1, dnx2, sk_v)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: np_smp
      integer (kind=kint), intent(in) :: iele_smp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: ntot_int_3d, num_int, k2
      real (kind=kreal), intent(in)                                     &
     &                   :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
      real (kind=kreal), intent(in)                                     &
     &                   :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
      real (kind=kreal), intent(in)  :: xjac(numele,ntot_int_3d)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal) :: poisson
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(k1,ii,ix,iele,ist,ied,poisson) 
       do iproc = 1, np_smp
         ist = iele_smp_stack(iproc-1)+1
         ied = iele_smp_stack(iproc)
!
         do ii = 1, num_int * num_int * num_int
           ix = int_start3(num_int) + ii
           do  k1 = 1, nnod_4_e1
!
!cdir nodep
             do iele = ist, ied
!
               poisson = dnx1(iele,k1,ix,1)*dnx2(iele,k2,ix,1)          &
     &                 + dnx1(iele,k1,ix,2)*dnx2(iele,k2,ix,2)          &
     &                 + dnx1(iele,k1,ix,3)*dnx2(iele,k2,ix,3)
!
               sk_v(iele,1,k1) = sk_v(iele,1,k1)                        &
     &              + poisson * xjac(iele,ix) * owe3d(ix)
!
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_poisson
!
! ----------------------------------------------------------------------
!
      end module fem_skv_diffusion
