!>@file   select_fourier_transform.F90
!!@brief  module select_fourier_transform
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief Select Fourier transform routine by elapsed time
!!
!!@verbatim
!!      subroutine s_select_fourier_transform(ncomp, Nstacksmp)
!!
!!       Current problem
!!      FFTW crashes when both single and multi transforms are 
!!      comparaed
!!@endverbatim
!
      module select_fourier_transform
!
      use m_precision
!
      use calypso_mpi
      use m_work_time
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_work_4_sph_trans
      use FFT_selector
!
      implicit none
!
      private :: test_fourier_trans_vector
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_select_fourier_transform(ncomp, Nstacksmp)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: Nstacksmp(0:np_smp)
!
      integer(kind = kint) :: iflag_chosen
      real(kind = kreal) :: etime_shortest
      real(kind = kreal) :: etime_fft(1:4)
!
!
      iflag_FFT = iflag_FFTPACK
      call test_fourier_trans_vector(ncomp, Nstacksmp,                  &
     &    etime_fft(iflag_FFT))
!
      iflag_chosen = iflag_FFT
      etime_shortest = etime_fft(iflag_FFT)
!
!
#ifdef FFTW3
      iflag_FFT = iflag_FFTW
      call test_fourier_trans_vector(ncomp, Nstacksmp,                  &
     &    etime_fft(iflag_FFTW))
!
      if(etime_fft(iflag_FFTW) .lt. etime_shortest) then
        iflag_FFT = iflag_chosen
        call finalize_FFT_select(np_smp, Nstacksmp)
!
        iflag_chosen = iflag_FFTW
        etime_shortest = etime_fft(iflag_FFTW)
      else
        call finalize_FFT_select(np_smp, Nstacksmp)
      end if
!
!
!      iflag_FFT = iflag_FFTW_SINGLE
!      call test_fourier_trans_vector(ncomp, Nstacksmp,                  &
!     &    etime_fft(iflag_FFTW_SINGLE))
!
!      if(etime_fft(iflag_FFTW_SINGLE) .lt. etime_shortest) then
!        iflag_FFT = iflag_chosen
!        call finalize_FFT_select(np_smp, Nstacksmp)
!
!        iflag_chosen = iflag_FFTW_SINGLE
!        etime_shortest = etime_fft(iflag_FFTW_SINGLE)
!      else
!        call finalize_FFT_select(np_smp, Nstacksmp)
!      end if
#endif
!
      iflag_FFT = iflag_ISPACK
      call test_fourier_trans_vector(ncomp, Nstacksmp,                  &
     &    etime_fft(iflag_ISPACK))
!
      if(etime_fft(iflag_ISPACK) .lt. etime_shortest) then
        iflag_FFT = iflag_chosen
        call finalize_FFT_select(np_smp, Nstacksmp)
!
        iflag_chosen = iflag_ISPACK
        etime_shortest = etime_fft(iflag_ISPACK)
      else
        call finalize_FFT_select(np_smp, Nstacksmp)
      end if
!
      iflag_FFT = iflag_chosen
!
      if(my_rank .gt. 0) return
        write(*,'(a,i4)', advance='no') 'Selected Fourier transform: ', &
     &                          iflag_FFT
!
        if     (iflag_FFT .eq. iflag_FFTPACK) then
          write(*,'(a,a)') ' (FFTPACK) '
        else if(iflag_FFT .eq. iflag_FFTW) then
          write(*,'(a,a)') ' (FFTW) '
        else if(iflag_FFT .eq. iflag_ISPACK) then
          write(*,'(a,a)') ' (ISPACK) '
        else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
          write(*,'(a,a)') ' (FFTW_SINGLE) '
        end if
!
        write(*,*)   '1: elapsed by FFTPACK: ',                         &
     &            etime_fft(iflag_FFTPACK)
        if(etime_fft(iflag_FFTW) .gt. zero) then
          write(*,*) '2: elapsed by FFTW3:   ', etime_fft(iflag_FFTW)
        end if
!        if(etime_fft(iflag_FFTW_SINGLE) .gt. zero) then
!          write(*,*) '3: elapsed by single FFTW3:   ',                 &
!     &            etime_fft(iflag_FFTW_SINGLE)
!        end if
        if(etime_fft(iflag_ISPACK) .gt. zero) then
          write(*,*) '4: elapsed by ISPACK:         ',                  &
     &            etime_fft(iflag_ISPACK)
        end if
!
      end subroutine s_select_fourier_transform
!
! -----------------------------------------------------------------------
!
      subroutine test_fourier_trans_vector(ncomp, Nstacksmp, etime_fft)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: Nstacksmp(0:np_smp)
      real(kind = kreal), intent(inout) :: etime_fft
!
      real(kind = kreal) :: stime, etime
!
!
      call initialize_FFT_select(my_rank, np_smp, Nstacksmp,            &
     &    nidx_rtp(3))
!
      stime = MPI_WTIME()
      call backward_FFT_select(np_smp, Nstacksmp, ncomp, nidx_rtp(3),   &
     &    vr_rtp)
      call forward_FFT_select(np_smp, Nstacksmp, ncomp, nidx_rtp(3),    &
     &    vr_rtp)
      etime = MPI_WTIME() - stime
!
      call MPI_allREDUCE (etime, etime_fft, ione,                       &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      etime_fft = etime_fft / dble(nprocs)
!
      end subroutine test_fourier_trans_vector
!
! -----------------------------------------------------------------------
!
      end module select_fourier_transform
