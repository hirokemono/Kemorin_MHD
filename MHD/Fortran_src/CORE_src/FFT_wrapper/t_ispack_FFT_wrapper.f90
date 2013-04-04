!>@file   t_ispack_FFT_wrapper.f90
!!@brief  module t_ispack_FFT_wrapper
!!
!!@author H. Matsui
!!@date Programmed on Apr., 2013
!
!
!>@brief  Fourier transform with work structures for ISPACK
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_wk_ispack_t(Nsmp, Nstacksmp, Nfft, WK)
!!      subroutine verify_wk_ispack_t(Nsmp, Nstacksmp, Nfft, WK)
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine FTTRUF_kemo_t(Nsmp, Nstacksmp, M, Nfft, X, WK)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by ISPACK
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
!!      subroutine FTTRUB_kemo_t(Nsmp, Nstacksmp, M, Nfft, X, WK)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by ISPACK
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
!!
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!!@n @param WK          Work structure for ISPACK
!
      module t_ispack_FFT_wrapper
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      structure for working data for ISPACK
      type working_ISPACK
!>        Data for multiple Fourier transform
        real(kind = 8), pointer :: X_ispack(:,:)
!>        Work area for ISPACK
        integer(kind = 4) :: IT_ispack(5)
!>        Work constants for ISPACK
        real(kind = 8), pointer :: T_ispack(:)
!>        Work area for ISPACK
        real(kind = 8), pointer :: WORK_ispack(:,:)
!>        flag for length of Fourier transform
        integer(kind = kint) :: iflag_fft_len = -1
!>        flag for number of components for Fourier transform
        integer(kind = kint) :: iflag_fft_comp = -1
      end type working_ISPACK
!
!
      private :: FTTRUI_kemo_type
      private :: alloc_work_ispack_t, alloc_const_ispack_t
      private :: dealloc_work_ispack_t, dealloc_const_ispack_t
!
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_wk_ispack_t(Nsmp, Nstacksmp, Nfft, WK)
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_ISPACK), intent(inout) :: WK
!
      integer(kind = kint) :: M, ip
!
!
      M = Nstacksmp(1)
      do ip = 1, Nsmp
        M = max(M, (Nstacksmp(ip) - Nstacksmp(ip-1)) )
      end do
!
      call alloc_const_ispack_t(Nfft, WK)
      call FTTRUI_kemo_type(Nfft, WK)
!
      call alloc_work_ispack_t(Nsmp, M, Nfft, WK)
!
      end subroutine init_wk_ispack_t
!
! ------------------------------------------------------------------
!
      subroutine verify_wk_ispack_t(Nsmp, Nstacksmp, Nfft, WK)
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_ISPACK), intent(inout) :: WK
!
      integer(kind = kint) :: M, ip
!
!
      M = Nstacksmp(1)
      do ip = 1, Nsmp
        M = max(M, (Nstacksmp(ip) - Nstacksmp(ip-1)) )
      end do
!
      if( WK%iflag_fft_len .ne. Nfft) then
!
        if( WK%iflag_fft_len .lt. 0) then
          call alloc_const_ispack_t(Nfft, WK)
        else if( Nfft .gt. WK%iflag_fft_comp ) then
          call dealloc_const_ispack_t(WK)
          call alloc_const_ispack_t(Nfft, WK)
        end if
!
        call FTTRUI_kemo_type(Nfft, WK)
!
      end if
!
      if( WK%iflag_fft_comp .lt. 0) then
        call alloc_work_ispack_t(Nsmp, M, Nfft, WK)
      else if( (M*Nfft) .gt. WK%iflag_fft_comp ) then
        call dealloc_work_ispack_t(WK)
        call alloc_work_ispack_t(Nsmp, M, Nfft, WK)
      end if
!
      end subroutine verify_wk_ispack_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine FTTRUI_kemo_type(Nfft, WK)
!
      integer(kind = kint), intent(in) :: Nfft
      type(working_ISPACK), intent(inout) :: WK
!
!
      call FTTRUI( Nfft, WK%IT_ispack, WK%T_ispack )
!
      end subroutine FTTRUI_kemo_type
!
! ------------------------------------------------------------------
!
      subroutine FTTRUF_kemo_t(Nsmp, Nstacksmp, M, Nfft, X, WK)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
      type(working_ISPACK), intent(inout) :: WK
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
            WK%X_ispack(inod_c,ismp) = X(j,2*i-1)
            WK%X_ispack(inod_s,ismp) = X(j,2*i  )
          end do
        end do
!
        call FTTRUF(num, Nfft, WK%X_ispack(1:M*Nfft,ismp),              &
     &      WK%WORK_ispack(1:M*Nfft,ismp), WK%IT_ispack,                &
     &      WK%T_ispack(1:2*nfft) )
!
        do inum = 1, num
          j = ist + inum
          inod_c = inum
          inod_s = inum + num
          X(j,1) = WK%X_ispack(inod_c,ismp)
          X(j,2) = WK%X_ispack(inod_s,ismp)
        end do
        do i = 2, Nfft/2
          do inum = 1, num
            j = ist + inum
            inod_c = inum + (2*i-2) * num
            inod_s = inum + (2*i-1) * num
            X(j,2*i-1) =   two * WK%X_ispack(inod_c,ismp)
            X(j,2*i  ) = - two * WK%X_ispack(inod_s,ismp)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine FTTRUF_kemo_t
!
! ------------------------------------------------------------------
!
      subroutine FTTRUB_kemo_t(Nsmp, Nstacksmp, M, Nfft, X, WK)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
      type(working_ISPACK), intent(inout) :: WK
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
          WK%X_ispack(inod_c,ismp) = X(j,1)
          WK%X_ispack(inod_s,ismp) = X(j,2)
        end do
        do i = 2, Nfft/2
          do inum = 1, num
            j = ist + inum
            inod_c = inum + (2*i-2) * num
            inod_s = inum + (2*i-1) * num
            WK%X_ispack(inod_c,ismp) =  half * X(j,2*i-1)
            WK%X_ispack(inod_s,ismp) = -half * X(j,2*i  )
          end do
        end do
!
        call FTTRUB(num, Nfft, WK%X_ispack(1:M*Nfft,ismp),              &
     &      WK%WORK_ispack(1:M*Nfft,ismp), WK%IT_ispack,                &
     &      WK%T_ispack(1:2*nfft) )
!
        do i = 1, Nfft/2
          do inum = 1, num
            j = ist + inum
            inod_c = inum + (2*i-2) * num
            inod_s = inum + (2*i-1) * num
            X(j,2*i-1) = WK%X_ispack(inod_c,ismp)
            X(j,2*i  ) = WK%X_ispack(inod_s,ismp)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine FTTRUB_kemo_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_ispack_t(Nsmp, M, Nfft, WK)
!
      integer(kind = kint), intent(in) :: Nsmp, M, Nfft
      type(working_ISPACK), intent(inout) :: WK
!
!
      WK%iflag_fft_comp = M*Nfft
      allocate( WK%X_ispack(WK%iflag_fft_comp,Nsmp) )
      allocate( WK%WORK_ispack(WK%iflag_fft_comp,Nsmp) )
      WK%WORK_ispack = 0.0d0
!
      end subroutine alloc_work_ispack_t
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_ispack_t(nfft, WK)
!
      integer(kind = kint), intent(in) :: nfft
      type(working_ISPACK), intent(inout) :: WK
!
      WK%iflag_fft_len = nfft
      allocate( WK%T_ispack(2*nfft) )
      WK%T_ispack = 0.0d0
!
      end subroutine alloc_const_ispack_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_ispack_t(WK)
!
      type(working_ISPACK), intent(inout) :: WK
!
!
      deallocate(WK%X_ispack, WK%WORK_ispack )
      WK%iflag_fft_comp = 0
!
      end subroutine dealloc_work_ispack_t
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_ispack_t(WK)
!
      type(working_ISPACK), intent(inout) :: WK
!
!
      deallocate( WK%T_ispack )
      WK%iflag_fft_len = 0
!
      end subroutine dealloc_const_ispack_t
!
! ------------------------------------------------------------------
!
      end module t_ispack_FFT_wrapper
