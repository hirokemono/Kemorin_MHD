!>@file   cal_3d_noise.f90
!!@brief  module cal_3d_noise
!!
!!@author H. Matsui
!!@date Programmed in Apr. 2018
!
!> @brief Construct 3D noise data for LIC
!!
!!@verbatim
!!      subroutine init_kemo_mt19937
!!      subroutine finish_kemo_mt19937
!!      subroutine const_3d_noise                                       &
!!     &         (i_stepsize, nidx, nnod_gl, rnoise_grad)
!!        integer(kind = kint), intent(in) :: i_stepsize
!!        integer(kind = kint), intent(in) :: nidx(3)
!!        integer(kind = kint_gl), intent(in) :: nnod_gl
!!        real(kind = kreal), intent(inout) :: rnoise_grad(0:3,nnod_gl)
!!      subroutine noise_normalization(nnod_gl, nidx, rnoise_grad)
!!        integer(kind = kint_gl), intent(in) :: nnod_gl
!!        integer(kind = kint), intent(in) :: nidx(3)
!!        real(kind = kreal), intent(inout) :: rnoise_grad(0:3,nnod_gl)
!!      subroutine grad_3d_noise                                        &
!!     &         (nnod_gl, nidx, asize_cube, rnoise_grad)
!!        integer(kind = kint_gl), intent(in) :: nnod_gl
!!        integer(kind = kint), intent(in) :: nidx(3)
!!        real(kind = kreal), intent(in) :: asize_cube(3)
!!        real(kind = kreal), intent(inout) :: rnoise_grad(0:3,nnod_gl)
!!
!!      subroutine cvt_rnoise_to_chara(nnod_gl, rnoise_grad, cnoise)
!!        integer(kind = kint_gl), intent(in) :: nnod_gl
!!        real(kind = kreal), intent(in) :: rnoise_grad(0:3,nnod_gl)
!!        character(len = 1), intent(inout) :: cnoise(nnod_gl)
!!      subroutine cvt_cnoise_to_real(nnod_gl, cnoise, rnoise_grad)
!!        integer(kind = kint_gl), intent(in) :: nnod_gl
!!        character(len = 1), intent(in) :: cnoise(nnod_gl)
!!        real(kind = kreal), intent(inout) :: rnoise_grad(0:3,nnod_gl)
!!@endverbatim
!
      module cal_3d_noise
!
      use m_precision
      use m_constants
!
      use mt_stream
!
      implicit none
!
      logical, save :: flag_mts = .FALSE.
      type(mt_state), save, private :: mts(1)
      integer(kind = kint), parameter :: ifix_seed =  1337
!
      private :: whitenoise3D, halton_sequence
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_kemo_mt19937
!
      integer :: iseeda(4) = (/ 123, 234, 345, 456 /)
!
      if(flag_mts) return
!
      call set_mt19937
      call new(mts(1))
!      call init(mts,iseed)  ! init by scalar
      call init(mts(1),iseeda)  ! init by array
      flag_mts = .TRUE.
!
      end subroutine init_kemo_mt19937
!
!  ---------------------------------------------------------------------
!
      subroutine finish_kemo_mt19937
!
!
      if(flag_mts) call delete(mts(1))
!
      end subroutine finish_kemo_mt19937
!
!  ---------------------------------------------------------------------
!
      subroutine const_3d_noise                                         &
     &         (i_stepsize, nidx, nnod_gl, rnoise_grad)
!
      integer(kind = kint), intent(in) :: i_stepsize
      integer(kind = kint), intent(in) :: nidx(3)
      integer(kind = kint_gl), intent(in) :: nnod_gl
      real(kind = kreal), intent(inout) :: rnoise_grad(0:3,nnod_gl)
!
      integer(kind = kint_gl), parameter :: ibase = 17
      integer(kind = kint_gl), parameter :: jbase = 31
      integer(kind = kint_gl), parameter :: kbase = 57
      integer(kind = kint_gl) :: index = 23
!
      integer(kind = kint) :: i, j, k
      integer(kind = kint_gl) :: inod_gl, itmp_gl, icou_gl
      real(kind = kreal) :: frand
!      real(kind = kreal) :: x, y, z, length
!
!
      icou_gl = 0
      do
        i = int(dble(nidx(1)) * halton_sequence(index, ibase)) + 1
        j = int(dble(nidx(2)) * halton_sequence(index, jbase)) + 1
        k = int(dble(nidx(3)) * halton_sequence(index, kbase)) + 1
!
        frand = genrand_double1(mts(1))
        index = index + mod(int(frand*32768.0), i_stepsize) + 1
        if(index .gt. nnod_gl) exit
!
!        x = dble(i) - 0.5d0 * dble(nidx(1))
!        y = dble(j) - 0.5d0 * dble(nidx(2))
!        z = dble(k) - 0.5d0 * dble(nidx(3))
!        length = sqrt(x*x + y*y + z*z)
!
        icou_gl = icou_gl + 1
        itmp_gl = int(((k-1) * nidx(2) + j),KIND(itmp_gl))
        inod_gl = int(i,KIND(inod_gl))                                  &
     &           + (itmp_gl-1) * int(nidx(1),KIND(inod_gl))
!
        rnoise_grad(0,inod_gl) = genrand_double1(mts(1))
      end do
!
      end subroutine const_3d_noise
!
!  ---------------------------------------------------------------------
!
      subroutine noise_normalization(nnod_gl, nidx, rnoise_grad)
!
      integer(kind = kint_gl), intent(in) :: nnod_gl
      integer(kind = kint), intent(in) :: nidx(3)
      real(kind = kreal), intent(inout) :: rnoise_grad(0:3,nnod_gl)
!
      integer(kind = kint_gl) :: i0
      real(kind = kreal) :: anoise
!
!
      anoise = 0.0d0
!$omp parallel do reduction(+:anoise)
      do i0 = 1, nnod_gl
        anoise = anoise + rnoise_grad(0,i0)
      end do
!$omp end parallel do
      anoise = dble(nidx(1)) * dble(nidx(2)) * dble(nidx(3)) / anoise
      write(*,*) 'anoise', anoise
!
!$omp parallel do
      do i0 = 1, nnod_gl
        rnoise_grad(0,i0) = rnoise_grad(0,i0) * anoise
      end do
!$omp end parallel do
!
      end subroutine noise_normalization
!
!  ---------------------------------------------------------------------
!
      subroutine grad_3d_noise                                          &
     &         (nnod_gl, nidx, asize_cube, rnoise_grad)
!
      integer(kind = kint_gl), intent(in) :: nnod_gl
      integer(kind = kint), intent(in) :: nidx(3)
      real(kind = kreal), intent(in) :: asize_cube(3)
!
      real(kind = kreal), intent(inout) :: rnoise_grad(0:3,nnod_gl)
!
      integer(kind = kint_gl) :: nidx64(3)
      integer(kind = kint_gl) :: i0, j0, k0, inod
      integer(kind = kint_gl) :: in, jn, kn, ip, jp, kp
      integer(kind = kint_gl) :: km_zn, km_gl, km_zp
      integer(kind = kint_gl) :: jm_gl, jm_yn, jm_yp, jm_zn, jm_zp
!
!     inod = i + n1 * (j-1) + n1*n2 * (k-1)
!          = i + n1 * (j-1 + n2*(k-1))
!
      nidx64(1) = int(nidx(1),KIND(inod))
      nidx64(2) = int(nidx(2),KIND(inod))
      nidx64(3) = int(nidx(3),KIND(inod))
!$omp parallel do private(i0,j0,k0,in,jn,kn,ip,jp,kp,km_zn,km_gl,km_zp, &
!$omp&                    jm_gl,jm_yn,jm_yp,jm_zn,jm_zp,inod)
      do kn = 1, nidx64(3)
        k0 = mod(kn,  nidx64(3)) + 1
        kp = mod(kn+1,nidx64(3)) + 1
        km_zn = nidx64(2) * (kn-1)
        km_gl = nidx64(2) * (k0-1)
        km_zp = nidx64(2) * (kp-1)
        do jn = 1, nidx64(2)
          j0 = mod(jn,  nidx64(2)) + 1
          jp = mod(jn+1,nidx64(2)) + 1
          jm_gl = nidx64(1) * (j0-1 + km_gl)
          jm_yn = nidx64(1) * (jn-1 + km_gl)
          jm_yp = nidx64(1) * (jp-1 + km_gl)
          jm_zn = nidx64(1) * (j0-1 + km_zn)
          jm_zp = nidx64(1) * (j0-1 + km_zp)
          do in = 1, nidx64(1)
            i0 = mod(in,  nidx64(1)) + 1
            ip = mod(in+1,nidx64(1)) + 1
            inod = i0 + jm_gl
!
            rnoise_grad(1,inod)                                         &
     &           = rnoise_grad(0,in+jm_gl) - rnoise_grad(0,ip+jm_gl)
            rnoise_grad(2,inod)                                         &
     &           = rnoise_grad(0,i0+jm_yp) - rnoise_grad(0,i0+jm_yn)
            rnoise_grad(3,inod)                                         &
     &           = rnoise_grad(0,i0+jm_zp) - rnoise_grad(0,i0+jm_zn)
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel workshare
      rnoise_grad(1,1:nnod_gl) = half * dble(nidx(1)) * asize_cube(1)   &
     &                          * rnoise_grad(1,1:nnod_gl)
      rnoise_grad(2,1:nnod_gl) = half * dble(nidx(2)) * asize_cube(2)   &
     &                          * rnoise_grad(2,1:nnod_gl)
      rnoise_grad(3,1:nnod_gl) = half * dble(nidx(3)) * asize_cube(3)   &
     &                          * rnoise_grad(3,1:nnod_gl)
!$omp end parallel workshare
!
      end subroutine grad_3d_noise
!
!  ---------------------------------------------------------------------
!
      real(kind = kreal) function whitenoise3D(m_seed, ix, iy, iz)
!
      integer(kind = kint), intent(in) :: m_seed
      integer(kind = kint), intent(in) :: ix, iy, iz
!
      integer(kind = kint), parameter :: iX_PRIME =  1619
      integer(kind = kint), parameter :: iY_PRIME = 31337
      integer(kind = kint), parameter :: iZ_PRIME =  6971
      integer(kind = kint_gl), parameter :: iprm_60493 = 60493
      real(kind = kreal), parameter :: rhuge =   2147483648.0
!
      integer(kind = kint) :: n
      integer(kind = kint_gl) :: n64
!
      n = m_seed
      n = xor(n, iX_PRIME * ix)
      n = xor(n, iY_PRIME * iy)
      n = xor(n, iZ_PRIME * iz)
!
      n64 = int(n,KIND(n64))
      whitenoise3D = dble(n64*n64*n64 * iprm_60493) / rhuge
!
      end function whitenoise3D
!
!  ---------------------------------------------------------------------
!
      real(kind = kreal) function halton_sequence(index, ib)
!
      integer(kind = kint_gl), intent(in) :: index, ib
!
      integer(kind = kint_gl) :: i
      real(kind = kreal) :: f, res
!
      i = index
      f =    1.0d0
      res  = 0.0d0
      do
        f = f / dble(ib)
        res = res + f * mod(i,ib)
        i = i / ib
        if(i .le. 0) exit
      end do
      halton_sequence = res
!
      end function halton_sequence
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cvt_rnoise_to_chara(nnod_gl, rnoise_grad, cnoise)
!
      integer(kind = kint_gl), intent(in) :: nnod_gl
      real(kind = kreal), intent(in) :: rnoise_grad(0:3,nnod_gl)
      character(len = 1), intent(inout) :: cnoise(nnod_gl)
!
      integer(kind = kint_gl) :: inod_gl, inoise
!
!$omp parallel do private(inod_gl, inoise)
      do inod_gl = 1, nnod_gl
        inoise = int((rnoise_grad(0,inod_gl) * 256),KIND(inoise))
        cnoise(inod_gl) = char(inoise)
      end do
!$omp end parallel do
!
      end subroutine cvt_rnoise_to_chara
!
!  ---------------------------------------------------------------------
!
      subroutine cvt_cnoise_to_real(nnod_gl, cnoise, rnoise_grad)
!
      integer(kind = kint_gl), intent(in) :: nnod_gl
      character(len = 1), intent(in) :: cnoise(nnod_gl)
      real(kind = kreal), intent(inout) :: rnoise_grad(0:3,nnod_gl)
!
      real(kind = kreal) :: pol
      integer(kind = kint_gl) :: inod_gl
!
      pol = 1.0d0 / 256.0
!$omp parallel do private(inod_gl)
      do inod_gl = 1, nnod_gl
        rnoise_grad(0,inod_gl) = dble( ichar(cnoise(inod_gl)) ) * pol
      end do
!$omp end parallel do
!
      end subroutine cvt_cnoise_to_real
!
!  ---------------------------------------------------------------------
!
      end module cal_3d_noise
