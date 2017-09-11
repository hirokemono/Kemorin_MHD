!fem_skv_inertia_upw.f90
!     module fem_skv_inertia_upw
!
!     programmed by H.Matsui and H.Okuda
!                                    on July 2002
!     modified by H. Matsui on Aug., 2005
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine fem_skv_scalar_inertia_upw                           &
!!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,&
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          n_int, k2, dt, ntot_int_3d, xjac, an1, dnx1, dnx2,    &
!!     &          scalar_e, vxe, vxe_up, sk_v)
!!      subroutine fem_skv_vector_inertia_upw                           &
!!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,&
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          n_int, k2, dt, ntot_int_3d, xjac, an1, dnx1, dnx2,    &
!!     &          vector_e, vxe, vxe_up, sk_v)
!!      subroutine fem_skv_inertia_rot_upw                              &
!!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,&
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          n_int, k2, dt, ntot_int_3d, xjac, an1, dnx1, an2,     &
!!     &          vector_e, wxe, vxe_up, sk_v)
!!
!!      subroutine fem_skv_coriolis_upw                                 &
!!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,&
!!     &          max_int_point, maxtot_int_3d, int_start3, owe3d,      &
!!     &          n_int, k2, dt, ntot_int_3d, xjac, an1, dnx1, an2,     &
!!     &          vector_e, angular, vxe_up, sk_v)
!
!
      module fem_skv_inertia_upw
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
      subroutine fem_skv_scalar_inertia_upw                             &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,  &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          n_int, k2, dt, ntot_int_3d, xjac, an1, dnx1, dnx2,      &
     &          scalar_e, vxe, vxe_up, sk_v)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_3d
      integer(kind=kint), intent(in) :: n_int, k2
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal),   intent(in) :: an1(nnod_4_e1, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      real (kind=kreal), intent(in) :: dt
      real (kind=kreal), intent(in) :: scalar_e(numele)
      real (kind=kreal), intent(in) :: vxe(numele,3)
      real (kind=kreal), intent(in) :: vxe_up(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal) :: inertia, tau
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend,tau,inertia)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
            do iele = istart, iend
!
!  ------  set inertia term
!
              tau = an1(k1,ix) + half * dt                              &
     &                  * ( vxe_up(iele,1)*dnx1(iele,k1,ix,1)           &
     &                    + vxe_up(iele,2)*dnx1(iele,k1,ix,2)           &
     &                    + vxe_up(iele,3)*dnx1(iele,k1,ix,3) )
!
              inertia  =  tau * ( vxe(iele,1)*dnx2(iele,k2,ix,1)        &
     &                          + vxe(iele,2)*dnx2(iele,k2,ix,2)        &
     &                          + vxe(iele,3)*dnx2(iele,k2,ix,3) )
!
! -------  caliculate 
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                         + inertia * scalar_e(iele)               &
     &                          * xjac(iele,ix)*owe3d(ix)
!
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_scalar_inertia_upw
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_inertia_upw                             &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,  &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          n_int, k2, dt, ntot_int_3d, xjac, an1, dnx1, dnx2,      &
     &          vector_e, vxe, vxe_up, sk_v)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal),   intent(in) :: an1(nnod_4_e1, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx2(numele,nnod_4_e2,ntot_int_3d,3)
!
      real (kind=kreal), intent(in) :: dt
      real (kind=kreal), intent(in) :: vector_e(numele,3)
      real (kind=kreal), intent(in) :: vxe(numele,3)
      real (kind=kreal), intent(in) :: vxe_up(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal) :: inertia, tau
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend,tau,inertia)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
            do iele = istart, iend
!
!  -----  set weighting function
!
              tau = an1(k1,ix) + half * dt                              &
     &                  * ( vxe_up(iele,1)*dnx1(iele,k1,ix,1)           &
     &                    + vxe_up(iele,2)*dnx1(iele,k1,ix,2)           &
     &                    + vxe_up(iele,3)*dnx1(iele,k1,ix,3) )
!
              inertia  =  tau * ( vxe(iele,1)*dnx2(iele,k2,ix,1)        &
     &                          + vxe(iele,2)*dnx2(iele,k2,ix,2)        &
     &                          + vxe(iele,3)*dnx2(iele,k2,ix,3) )
!
! -------  caliculate 
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                         + inertia * vector_e(iele,1)             &
     &                          * xjac(iele,ix)*owe3d(ix)
              sk_v(iele,2,k1) = sk_v(iele,2,k1)                         &
     &                         + inertia * vector_e(iele,2)             &
     &                          * xjac(iele,ix)*owe3d(ix)
              sk_v(iele,3,k1) = sk_v(iele,3,k1)                         &
     &                         + inertia * vector_e(iele,3)             &
     &                          * xjac(iele,ix)*owe3d(ix)
!
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_vector_inertia_upw
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_inertia_rot_upw                                &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,  &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          n_int, k2, dt, ntot_int_3d, xjac, an1, dnx1, an2,       &
     &          vector_e, wxe, vxe_up, sk_v)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal),   intent(in) :: an1(nnod_4_e1, ntot_int_3d)
      real(kind=kreal),   intent(in) :: an2(nnod_4_e2, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
!
      real (kind=kreal), intent(in) :: dt
      real (kind=kreal), intent(in) :: vector_e(numele,3)
      real (kind=kreal), intent(in) :: wxe(numele,3)
      real (kind=kreal), intent(in) :: vxe_up(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal) :: inertia1, inertia2, inertia3, tau
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend,                    &
!$omp&                    inertia1,inertia2,inertia3,tau)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
            do iele = istart, iend
!
!  ------  set inertia term
!
              tau = an1(k1,ix) + half * dt                              &
     &                  * ( vxe_up(iele,1)*dnx1(iele,k1,ix,1)           &
     &                    + vxe_up(iele,2)*dnx1(iele,k1,ix,2)           &
     &                    + vxe_up(iele,3)*dnx1(iele,k1,ix,3) )
!
              inertia1  =   tau * an2(k2,ix)                            &
     &                           * ( wxe(iele,2)*vector_e(iele,3)       &
     &                             - wxe(iele,3)*vector_e(iele,2) )
              inertia2  =   tau * an2(k2,ix)                            &
     &                           * ( wxe(iele,3)*vector_e(iele,1)       &
     &                             - wxe(iele,1)*vector_e(iele,3) )
              inertia3  =   tau * an2(k2,ix)                            &
     &                           * ( wxe(iele,1)*vector_e(iele,2)       &
     &                             - wxe(iele,2)*vector_e(iele,1) )
!
! -------  caliculate 
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                          + inertia1 * xjac(iele,ix)*owe3d(ix)
              sk_v(iele,2,k1) = sk_v(iele,2,k1)                         &
     &                          + inertia2 * xjac(iele,ix)*owe3d(ix)
              sk_v(iele,3,k1) = sk_v(iele,3,k1)                         &
     &                          + inertia3 * xjac(iele,ix)*owe3d(ix)
!
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_inertia_rot_upw
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_coriolis_upw                                   &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,  &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          n_int, k2, dt, ntot_int_3d, xjac, an1, dnx1, an2,       &
     &          vector_e, angular, vxe_up, sk_v)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal),   intent(in) :: an1(nnod_4_e1, ntot_int_3d)
      real(kind=kreal),   intent(in) :: an2(nnod_4_e2, ntot_int_3d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx1(numele,nnod_4_e1,ntot_int_3d,3)
!
      real (kind=kreal), intent(in) :: dt
      real (kind=kreal), intent(in) :: vector_e(numele,3)
      real (kind=kreal), intent(in) :: angular(3)
      real (kind=kreal), intent(in) :: vxe_up(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_e1)
!
      real (kind=kreal) :: inertia1, inertia2, inertia3, tau
      integer (kind=kint) :: k1
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
!
!$omp parallel do private(k1,ii,ix,iele,istart,iend,                    &
!$omp&                    inertia1,inertia2,inertia3,tau)
      do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
          do k1 = 1, nnod_4_e1
!
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
            do iele = istart, iend
!
!  ------  set inertia term
!
              tau = an1(k1,ix) + half * dt                              &
     &                  * ( vxe_up(iele,1)*dnx1(iele,k1,ix,1)           &
     &                    + vxe_up(iele,2)*dnx1(iele,k1,ix,2)           &
     &                    + vxe_up(iele,3)*dnx1(iele,k1,ix,3) )
!
              inertia1  =   tau * an2(k2,ix)                            &
     &                           * ( angular(2)*vector_e(iele,3)        &
     &                             - angular(3)*vector_e(iele,2) )
              inertia2  =   tau * an2(k2,ix)                            &
     &                           * ( angular(3)*vector_e(iele,1)        &
     &                             - angular(1)*vector_e(iele,3) )
              inertia3  =   tau * an2(k2,ix)                            &
     &                           * ( angular(1)*vector_e(iele,2)        &
     &                             - angular(2)*vector_e(iele,1) )
!
! -------  caliculate 
!
              sk_v(iele,1,k1) = sk_v(iele,1,k1)                         &
     &                          - inertia1 * xjac(iele,ix)*owe3d(ix)
              sk_v(iele,2,k1) = sk_v(iele,2,k1)                         &
     &                          - inertia2 * xjac(iele,ix)*owe3d(ix)
              sk_v(iele,3,k1) = sk_v(iele,3,k1)                         &
     &                          - inertia3 * xjac(iele,ix)*owe3d(ix)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_skv_coriolis_upw
!
!-----------------------------------------------------------------------
!
      end module fem_skv_inertia_upw
