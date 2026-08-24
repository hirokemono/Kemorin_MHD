! ------------------------------------------------------------------
!
      subroutine sel_multi_pout_bwd_rocFFT(Ncomp, bwd, WK_fft, X,       &
     &                                     elapsed_fft, elapsed_cpy)
!
      use copy_field_for_FFT
      use normalize_for_rocFFT
      use calypso_multi_rocFFT
!
      integer(kind = kint), intent(in) :: iflag_fft
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), intent(in), target :: bwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_fft
      real(kind = kreal), intent(inout) :: X(Ncomp,bwd%Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: i, ist
!
!
      start = OMP_GET_WTIME()
      call sel_norm_rtp_to_bwd_rocFFT(iflag_fft, ione, Ncomp, X(1,1),   &
     &                                bwd_rocFFT, WK_rocFFT)
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_pout_backward_rocFFT(iflag_fft, bwd_rocFFT, WK_rocFFT)
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_copy_pout_field_from_rocFFT                              &
     &   (iflag_fft, Ncomp, bwd_rocFFT, WK_rocFFT, X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine sel_multi_pout_bwd_rocFFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sel_norm_rtp_to_bwd_rocFFT(iflag_fft, Ncomp, X,        &
     &                                       bwd_rocFFT, WK_rocFFT)
!
      integer(kind = kint), intent(in) :: iflag_fft
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
      real(kind = kreal), intent(in) :: X(Ncomp,bwd_rocFFT%Nfft)
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!
!
      if((iflag_fft/10) .eq. (iflag_rocFFT/10)) then
        call norm_rtp_to_bwd_OMP_FFTW                                   &
     &     (ione, Ncomp, int(bwd_rocFFT%Nfft), X(1,1),                  &
     &      int(bwd_rocFFT%Ncomp), int(WK_rocFFT%Nfft_c),               &
     &      WK_rocFFT%C_rocFFT(1))
!      else if((iflag_fft/10) .eq. (iflag_real_rocFFT/10)) then
!      else if((iflag_fft/10) .eq. (iflag_OMP_rocFFT/10)) then
      else
        call norm_rtp_to_bwd_rocFFT                                     &
     &     (ione, Ncomp, int(bwd_rocFFT%Nfft), X(1,1),                  &
     &      int(bwd_rocFFT%Ncomp), int(WK_rocFFT%Nfft_r),               &
     &      WK_rocFFT%X_rocFFT(1))
      end if
!
      end subroutine sel_norm_rtp_to_bwd_rocFFT
!
! ------------------------------------------------------------------
!
      subroutine sel_pout_backward_rocFFT(iflag_fft, bwd_rocFFT,        &
     &                                    WK_rocFFT)
!
      integer(kind = kint), intent(in) :: iflag_fft
      type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!
!
      if((iflag_fft/10) .eq. (iflag_rocFFT/10)) then
        call calypso_backward_rocFFT_c2r(bwd_rocFFT%rocFFT_plan,        &
     &      bwd_rocFFT%rocFFT_wk_info, bwd_rocFFT%Ncomp,                &
     &      WK_rocFFT%Nfft_c, WK_rocFFT%C_rocFFT(1),                    &
     &      WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1),                    &
     &      bwd_rocFFT%Nbytes, WK_rocFFT%data_ptr)
      else if((iflag_fft/10) .eq. (iflag_real_rocFFT/10)) then
        call calypso_backward_rocFFT_r2r                                &
     &     (bwd_rocFFT%rocFFT_plan, bwd_rocFFT%rocFFT_wk_info,          &
     &      bwd_rocFFT%Ncomp, WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1),  &
     &      bwd_rocFFT%Nbytes, WK_rocFFT%data_ptr)
!      else if((iflag_fft/10) .eq. (iflag_OMP_rocFFT/10)) then
      else
        call calypso_bwd_OpenMP_rocFFT                                  &
           (bwd_rocFFT%rocFFT_plan, bwd_rocFFT%rocFFT_wk_info,          &
     &      bwd_rocFFT%Ncomp, WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1))
      end if
!
      end subroutine sel_pout_backward_rocFFT
!
! ------------------------------------------------------------------
!
      subroutine sel_copy_pout_field_from_rocFFT                        &
     &         (iflag_fft, Ncomp, bwd_rocFFT, WK_rocFFT, X)
!
      integer(kind = kint), intent(in) :: iflag_fft
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
      type(calypso_rocFFT_work), intent(in) :: WK_rocFFT
!
      real(kind = kreal), intent(inout) :: X(Ncomp,bwd_rocFFT%Nfft)
!
!      if((iflag_fft/10) .eq. (iflag_rocFFT/10)) then
!      else if((iflag_fft/10) .eq. (iflag_real_rocFFT/10)) then
!      else if((iflag_fft/10) .eq. (iflag_OMP_rocFFT/10)) then
!      else
        call copy_pout_field_from_FFT(int(bwd_rocFFT%Ncomp),            &
     &      int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1),               &
     &      Ncomp, int(bwd_rocFFT%Nfft), ione, X(1,1))
!      end if
!
      end subroutine sel_copy_pout_field_from_rocFFT
!
! ------------------------------------------------------------------
