!>@file   ispack_FFT_wrapper.f90
!!@brief  module ispack_FFT_wrapper
!!
!!@author H. Matsui
!!@date Programmed in 2008
!
!
!>@brief  Fourier transform using ISPACK
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_4_ispack(Nsmp, Nstacksmp, Nfft)
!!      subroutine verify_work_4_ispack(Nsmp, Nstacksmp, Nfft)
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine FTTRUF_kemo(Nsmp, Nstacksmp, M, Nfft, X)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for FFT in ISPACK
!!
!! a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\grac{2\pijk}{Nfft})
!! b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\grac{2\pijk}{Nfft})
!!
!! a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!! K = Nfft/2....
!! a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\grac{2\pijk}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine FTTRUB_kemo(Nsmp, Nstacksmp, M, Nfft, X)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for inverse FFT for ISPACK
!!
!! x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!! (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!!
!! ------------------------------------------------------------------
!!
!! i = 1:     a_{0}
!! i = 2:     a_{Nfft/2}
!! i = 3:     a_{1}
!! i = 4:     b_{1}
!! ...
!! i = 2*k+1: a_{k}
!! i = 2*k+2: b_{k}
!! ...
!! i = Nfft-1:   a_{Nfft/2-1}
!! i = Nfft:     b_{Nfft/2-1}
!!
!! ------------------------------------------------------------------
!!@endverbatim
!
      module ispack_FFT_wrapper
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Data for multiple Fourier transform
      real(kind = 8), allocatable :: X_ispack(:,:)
!>      Work area for ISPACK
      integer(kind = 4) :: IT_ispack(5)
!>      Work constants for ISPACK
      real(kind = 8), allocatable :: T_ispack(:)
!>      Work area for ISPACK
      real(kind = 8), allocatable :: WORK_ispack(:,:)
!>      flag for length of Fourier transform
      integer(kind = kint) :: iflag_fft_len = -1
!>      flag for number of components for Fourier transform
      integer(kind = kint) :: iflag_fft_comp = -1
!
      private :: IT_ispack, T_ispack, WORK_ispack
      private :: iflag_fft_len, iflag_fft_comp
!
      private :: FTTRUI_kemo
      private :: allocate_work_4_ispack, allocate_const_4_ispack
      private :: deallocate_work_4_ispack, deallocate_const_4_ispack
!
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_4_ispack(Nsmp, Nstacksmp, Nfft)
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      integer(kind = kint) :: M, ip
!
!
      M = Nstacksmp(1)
      do ip = 1, Nsmp
        M = max(M, (Nstacksmp(ip) - Nstacksmp(ip-1)) )
      end do
!
      call allocate_const_4_ispack(Nfft)
      call FTTRUI_kemo(Nfft)
!
      call allocate_work_4_ispack(Nsmp, M, Nfft)
!
      end subroutine init_4_ispack
!
! ------------------------------------------------------------------
!
      subroutine verify_work_4_ispack(Nsmp, Nstacksmp, Nfft)
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      integer(kind = kint) :: M, ip
!
!
      M = Nstacksmp(1)
      do ip = 1, Nsmp
        M = max(M, (Nstacksmp(ip) - Nstacksmp(ip-1)) )
      end do
!
      if( iflag_fft_len .ne. Nfft) then
!
        if( iflag_fft_len .lt. 0) then
          call allocate_const_4_ispack(Nfft)
        else if( Nfft .gt. iflag_fft_comp ) then
          call deallocate_const_4_ispack
          call allocate_const_4_ispack(Nfft)
        end if
!
        call FTTRUI_kemo(Nfft)
!
      end if
!
      if( iflag_fft_comp .lt. 0) then
        call allocate_work_4_ispack(Nsmp, M, Nfft)
      else if( (M*Nfft) .gt. iflag_fft_comp ) then
        call deallocate_work_4_ispack
        call allocate_work_4_ispack(Nsmp, M, Nfft)
      end if
!
      end subroutine verify_work_4_ispack
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine FTTRUI_kemo(Nfft)
!
      integer(kind = kint), intent(in) :: Nfft
!
      call FTTRUI( Nfft, IT_ispack, T_ispack(1) )
!
      end subroutine FTTRUI_kemo
!
! ------------------------------------------------------------------
!
      subroutine FTTRUF_kemo(Nsmp, Nstacksmp, M, Nfft, X)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
!
      integer(kind = kint) :: i, j, ismp, ist, num
      integer(kind = kint) :: inum, inod_s, inod_c
!
!
! normalization
!
!$omp parallel do private(i,j,ist,num,inum,inod_s,inod_c)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num = Nstacksmp(ismp) - Nstacksmp(ismp-1)
!
        do i = 1, Nfft/2
          do inum = 1, num
            j = ist + inum
            inod_c = inum + (2*i-2) * num
            inod_s = inum + (2*i-1) * num
            X_ispack(inod_c,ismp) = X(j,2*i-1)
            X_ispack(inod_s,ismp) = X(j,2*i  )
          end do
        end do
!
        call FTTRUF(num, Nfft, X_ispack(1,ismp),                    &
     & WORK_ispack(1,ismp), IT_ispack(1), T_ispack(1))
!
        do inum = 1, num
          j = ist + inum
          inod_c = inum
          inod_s = inum + num
          X(j,1) = X_ispack(inod_c,ismp)
          X(j,2) = X_ispack(inod_s,ismp)
        end do
        do i = 2, Nfft/2
          do inum = 1, num
            j = ist + inum
            inod_c = inum + (2*i-2) * num
            inod_s = inum + (2*i-1) * num
            X(j,2*i-1) =   two * X_ispack(inod_c,ismp)
            X(j,2*i  ) = - two * X_ispack(inod_s,ismp)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine FTTRUF_kemo
!
! ------------------------------------------------------------------
!
      subroutine FTTRUB_kemo(Nsmp, Nstacksmp, M, Nfft, X)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
!
      integer(kind = kint) ::  i, j, ismp, ist, num
      integer(kind = kint) :: inum, inod_s, inod_c
!
!
! normalization
!
!$omp parallel do private(i,j,ist,num,inum,inod_s,inod_c)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num = Nstacksmp(ismp) - Nstacksmp(ismp-1)
!
        do inum = 1, num
          j = ist + inum
            inod_c = inum
            inod_s = inum + num
          X_ispack(inod_c,ismp) = X(j,1)
          X_ispack(inod_s,ismp) = X(j,2)
        end do
        do i = 2, Nfft/2
          do inum = 1, num
            j = ist + inum
            inod_c = inum + (2*i-2) * num
            inod_s = inum + (2*i-1) * num
            X_ispack(inod_c,ismp) =  half * X(j,2*i-1)
            X_ispack(inod_s,ismp) = -half * X(j,2*i  )
          end do
        end do
!
        call FTTRUB(num, Nfft, X_ispack(1,ismp),                    &
     & WORK_ispack(1,ismp), IT_ispack(1), T_ispack(1) )
!
        do i = 1, Nfft/2
          do inum = 1, num
            j = ist + inum
            inod_c = inum + (2*i-2) * num
            inod_s = inum + (2*i-1) * num
            X(j,2*i-1) = X_ispack(inod_c,ismp)
            X(j,2*i  ) = X_ispack(inod_s,ismp)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine FTTRUB_kemo
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine allocate_work_4_ispack(Nsmp, M, Nfft)
!
      integer(kind = kint), intent(in) :: Nsmp, M, Nfft
!
!
      iflag_fft_comp = M*Nfft
      allocate( X_ispack(iflag_fft_comp,Nsmp) )
      allocate( WORK_ispack(iflag_fft_comp,Nsmp) )
      WORK_ispack = 0.0d0
!
      end subroutine allocate_work_4_ispack
!
! ------------------------------------------------------------------
!
      subroutine allocate_const_4_ispack(nfft)
!
      integer(kind = kint), intent(in) :: nfft
!
      iflag_fft_len = nfft
      allocate( T_ispack(2*nfft) )
      T_ispack = 0.0d0
!
      end subroutine allocate_const_4_ispack
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine deallocate_work_4_ispack
!
      deallocate(X_ispack, WORK_ispack )
      iflag_fft_comp = 0
!
      end subroutine deallocate_work_4_ispack
!
! ------------------------------------------------------------------
!
      subroutine deallocate_const_4_ispack
!
      deallocate( T_ispack )
      iflag_fft_len = 0
!
      end subroutine deallocate_const_4_ispack
!
! ------------------------------------------------------------------
!
      end module ispack_FFT_wrapper
