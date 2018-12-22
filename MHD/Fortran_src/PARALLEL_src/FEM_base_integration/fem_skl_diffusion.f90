!>@file   fem_skl_diffusion.f90
!!@brief  module fem_skl_diffusion
!!
!!@author H. Matsui and H.Okuda
!!@date Programmed in July 2000 (ver 1.1)
!!      Modified in Oct., 2006
!!      Modified in Oct., 2011
!!      Modified in Dec., 2018
!!
!> @brief FEM integration for 1-D  diffusion and Poisson equation
!!
!!@verbatim
!!      subroutine fem_skl_scalar_diffuse                               &
!!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,&
!!     &          max_int_point, maxtot_int_3d, int_start1, owe1d,      &
!!     &          n_int, k2, ntot_int_1d, xjac_1d, dnx1_1d, dnx2_1d,    &
!!     &          ak_d, scalar_e, sk)
!!
!!      subroutine fem_skl_poisson                                      &
!!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_smp_stack, &
!!     &          max_int_point, maxtot_int_3d, int_start1, owe1d,      &
!!     &          num_int, k2, ntot_int_1d, xjac_1d, dnx1_1d, dnx2_1d, 7&
!!     &          sk)
!!@endverbatim
!
      module fem_skl_diffusion
!
      use m_precision
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
      subroutine fem_skl_scalar_diffuse                                 &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,  &
     &          max_int_point, maxtot_int_3d, int_start1, owe1d,        &
     &          n_int, k2, ntot_int_1d, xjac_1d, dnx1_1d, dnx2_1d,      &
     &          ak_d, scalar_e, sk)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_1d
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int, k2
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start1(max_int_point)
      real(kind = kreal),   intent(in) :: owe1d(maxtot_int_3d)
!
      real(kind=kreal),   intent(in) :: xjac_1d(numele, ntot_int_1d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx1_1d(numele,nnod_4_e1,ntot_int_1d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx2_1d(numele,nnod_4_e2,ntot_int_1d)
!
      real (kind=kreal), intent(in) :: ak_d(numele)
      real (kind=kreal), intent(in) :: scalar_e(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk(numele,nnod_4_e1)
!
      integer (kind=kint) :: k1, iele, ii, ix
!
!
      do ii= 1, n_int
        ix = int_start1(n_int) + ii
        do k1 = 1, nnod_4_e1
!
!cdir nodep
!voption, indep, vec
          do iele = iele_fsmp_stack(0)+1, iele_fsmp_stack(np_smp)
!
! -------  caliculate 
            sk(iele,k1) = sk(iele,k1)                                   &
     &                   - ak_d(iele)  * scalar_e(iele)                 &
     &                   * dnx1_1d(iele,k1,ix)*dnx2_1d(iele,k2,ix)      &
     &                   * xjac_1d(iele,ix) * owe1d(ix)
          end do
!
        end do
      end do
!
      end subroutine fem_skl_scalar_diffuse
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine fem_skl_poisson                                        &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_smp_stack,   &
     &          max_int_point, maxtot_int_3d, int_start1, owe1d,        &
     &          num_int, k2, ntot_int_1d, xjac_1d, dnx1_1d, dnx2_1d,    &
     &          sk)
!
      integer (kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer (kind=kint), intent(in) :: np_smp
      integer (kind=kint), intent(in) :: iele_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start1(max_int_point)
      real(kind = kreal),   intent(in) :: owe1d(maxtot_int_3d)
!
      integer (kind=kint), intent(in) :: ntot_int_1d, num_int, k2
      real (kind=kreal), intent(in)                                     &
     &                   :: dnx1_1d(numele,nnod_4_e1,ntot_int_1d)
      real (kind=kreal), intent(in)                                     &
     &                   :: dnx2_1d(numele,nnod_4_e2,ntot_int_1d)
      real (kind=kreal), intent(in)  :: xjac_1d(numele,ntot_int_1d)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk(numele,nnod_4_e1)
!
      integer (kind=kint) :: k1, iele, ii, ix
!
!
       do ii = 1, num_int
         ix = int_start1(num_int) + ii
         do  k1 = 1, nnod_4_e1
!
!cdir nodep
           do iele = iele_smp_stack(0)+1, iele_smp_stack(np_smp)
             sk(iele,k1) = sk(iele,k1)                                  &
     &                    + dnx1_1d(iele,k1,ix) * dnx2_1d(iele,k2,ix)   &
     &                     * xjac_1d(iele,ix) * owe1d(ix)
          end do
        end do
      end do
!
      end subroutine fem_skl_poisson
!
! ----------------------------------------------------------------------
!
      end module fem_skl_diffusion
