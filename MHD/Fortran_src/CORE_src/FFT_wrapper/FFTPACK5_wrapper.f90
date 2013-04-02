!>@file   FFTPACK5_wrapper.f90
!!@brief  module FFTPACK5_wrapper
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTPACK5
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!      subroutine init_4_FFTPACK(Nsmp, Nstacksmp, Nfft)
!!      subroutine verify_work_4_FFTPACK(Nsmp, Nstacksmp, Nfft)
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT in ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine RFFTMF_norm(Nsmp, Nstacksmp, M, Nfft, X)
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for FFT in ISPACK
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          *  \cos (\grac{2\pijk}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          *  \cos (\grac{2\pijk}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          * \cos (\grac{2\pijk}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine RFFTMB_norm(Nsmp, Nstacksmp, M, Nfft, X)
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for inverse FFT in ISPACK
!!
!!   x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!!          (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!!
!! ------------------------------------------------------------------
!!
!!       i = 1:     a_{0}
!!       i = 2:     a_{Nfft/2}
!!       i = 3:     a_{1}
!!       i = 4:     b_{1}
!!       ...
!!       i = 2*k+1: a_{k}
!!       i = 2*k+2: b_{k}
!!       ...
!!       i = Nfft-1:   a_{Nfft/2-1}
!!       i = Nfft:     b_{Nfft/2-1}
!!
!! ------------------------------------------------------------------
!!@endverbatim
!
      module FFTPACK5_wrapper
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Data for multiple Fourier transform
      real(kind = 8), allocatable :: X_FFTPACK5(:,:)
!
!>      Work area for FFTPACK
      integer(kind = kint) :: lensav_FFTPACK
!>      Work constatnts for FFTPACK
      real(kind = 8), allocatable :: WSAVE_FFTPACK(:)
!>      Work area for FFTPACK
      real(kind = 8), allocatable :: WORK_FFTPACK(:,:)
!>      flag for length of Fourier transform
      integer(kind = kint) :: iflag_fft_len =  -1
!>      flag for number of components for Fourier transform
      integer(kind = kint) :: iflag_fft_comp = -1
!
      private :: lensav_FFTPACK, WSAVE_FFTPACK, WORK_FFTPACK
      private :: iflag_fft_len, iflag_fft_comp
!
      private :: RFFTMI_norm
      private :: allocate_work_4_FFTPACK, allocate_const_4_FFTPACK
      private :: deallocate_work_4_FFTPACK, deallocate_const_4_FFTPACK
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_4_FFTPACK(Nsmp, Nstacksmp, Nfft)
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
      call allocate_const_4_FFTPACK(Nfft)
      call RFFTMI_norm(Nfft)
!
      call allocate_work_4_FFTPACK(Nsmp, M, Nfft)
!
      end subroutine init_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine verify_work_4_FFTPACK(Nsmp, Nstacksmp, Nfft)
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
          call allocate_const_4_FFTPACK(Nfft)
        else if( Nfft .gt. iflag_fft_comp ) then
          call deallocate_const_4_FFTPACK
          call allocate_const_4_FFTPACK(Nfft)
        end if
!
        call RFFTMI_norm(Nfft)
      end if
!
      if( iflag_fft_comp .lt. 0) then
        call allocate_work_4_FFTPACK(Nsmp, M, Nfft)
      else if( (M*Nfft) .gt. iflag_fft_comp ) then
        call deallocate_work_4_FFTPACK
        call allocate_work_4_FFTPACK(Nsmp, M, Nfft)
      end if
!
      end subroutine verify_work_4_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine RFFTMI_norm(Nfft)
!
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = kint) :: ierr
!
!
      call RFFTMI (Nfft, WSAVE_FFTPACK, lensav_FFTPACK, ierr)
!
      end subroutine RFFTMI_norm
!
! ------------------------------------------------------------------
!
      subroutine RFFTMF_norm(Nsmp, Nstacksmp, M, Nfft, X)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
!
      integer(kind = kint) ::  i, j, ismp, ist, num, inum, nsize
      integer(kind = kint) :: inod_s, inod_c, ierr
!
!
!   normalization
!
!$omp parallel do private(i,j,ist,num,inum,nsize,inod_s,inod_c)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        nsize = num*Nfft
!
        do i = 1, Nfft
          do inum = 1, num
            j = ist + inum
            inod_c = inum + (i-1) * num
            X_FFTPACK5(inod_c,ismp) = X(j,i)
          end do
        end do
!
        call RFFTMF(num, ione, Nfft, num, X_FFTPACK5(1,ismp), nsize,    &
     &      WSAVE_FFTPACK, lensav_FFTPACK, WORK_FFTPACK(1,ismp),        &
     &      nsize, ierr)
!
        do inum = 1, num
          j = ist + inum
          inod_s = inum + (Nfft-1) * num
          X(j,1) = X_FFTPACK5(inum,ismp)
          X(j,2) = X_FFTPACK5(inod_s,ismp)
        end do
        do i = 1, (Nfft+1)/2 - 1
          do inum = 1, num
            j = ist + inum
            inod_c = inum + (2*i-1) * num
            inod_s = inum + (2*i  ) * num
            X(j,2*i+1) = X_FFTPACK5(inod_c,ismp)
            X(j,2*i+2) = X_FFTPACK5(inod_s,ismp)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine RFFTMF_norm
!
! ------------------------------------------------------------------
!
      subroutine RFFTMB_norm(Nsmp, Nstacksmp, M, Nfft, X)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
!
      integer(kind = kint) ::  i, j, ismp, ist, inum, num, nsize
      integer(kind = kint) :: inod_s, inod_c, ierr
!
!
!   normalization
!
!$omp parallel do private(i,j,ist,num,inum,nsize,inod_s,inod_c)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        nsize = num*Nfft
!
        do inum = 1, num
          j = ist + inum
          inod_s = inum + (Nfft-1) * num
          X_FFTPACK5(inum,ismp) =   X(j,1)
          X_FFTPACK5(inod_s,ismp) = X(j,2)
        end do
        do i = 1, (Nfft+1)/2 - 1
          do inum = 1, num
            j = ist + inum
            inod_c = inum + (2*i-1) * num
            inod_s = inum + (2*i  ) * num
            X_FFTPACK5(inod_c,ismp) = X(j,2*i+1)
            X_FFTPACK5(inod_s,ismp) = X(j,2*i+2)
          end do
        end do
        if(mod(Nfft,2) .eq. 0) then
          do inum = 1, num
            j = ist + inum
          end do
        end if
!
        call RFFTMB (num, ione, Nfft, num, X_FFTPACK5(1,ismp), nsize,   &
     &      WSAVE_FFTPACK, lensav_FFTPACK, WORK_FFTPACK(1,ismp),        &
     &      nsize, ierr)
!
        do i = 1, Nfft
          do inum = 1, num
            j = ist + inum
            inod_c = inum + (i-1) * num
            X(j,i) = X_FFTPACK5(inod_c,ismp)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine RFFTMB_norm
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine allocate_work_4_FFTPACK(Nsmp, M, Nfft)
!
      integer(kind = kint), intent(in) :: Nsmp, M, Nfft
!
!
      iflag_fft_comp = M*Nfft
      allocate( X_FFTPACK5(iflag_fft_comp,Nsmp) )
      allocate( WORK_FFTPACK(iflag_fft_comp,Nsmp) )
      WORK_FFTPACK = 0.0d0
!
      end subroutine allocate_work_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine allocate_const_4_FFTPACK(nfft)
!
      integer(kind = kint), intent(in) :: nfft
!
      iflag_fft_len = nfft
      lensav_FFTPACK = Nfft                                             &
     &                + int ( log ( real(Nfft) ) / log(two) ) + ifour
      allocate(WSAVE_FFTPACK(lensav_FFTPACK) )
      WSAVE_FFTPACK = 0.0d0
!
      end subroutine allocate_const_4_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine deallocate_work_4_FFTPACK
!
      deallocate(X_FFTPACK5, WORK_FFTPACK)
      iflag_fft_comp = 0
!
      end subroutine deallocate_work_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine deallocate_const_4_FFTPACK
!
      deallocate( WSAVE_FFTPACK )
      iflag_fft_len = 0
!
      end subroutine deallocate_const_4_FFTPACK
!
! ------------------------------------------------------------------
!
      end module FFTPACK5_wrapper
