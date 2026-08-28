!>@file   sel_copy_field_pout_FFT.F90
!!@brief  module sel_copy_field_pout_FFT
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2026
!>@brief  Selector for FFT
!!
!!@verbatim
!!      subroutine sel_norm_pout_field_to_FFT(iflag_FFT, Ncomp_CPU,     &
!!     &          Ncomp, Nfft, ist_comp, X, WKS)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: Ncomp_CPU, ist_comp
!!        integer(kind = kint), intent(in) :: Nfft, Ncomp
!!        real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!!        type(working_FFTs), intent(inout) :: WKS
!!      subroutine sel_norm_pout_spectr_from_FFT(iflag_FFT, Ncomp_CPU,  &
!!     &          WKS, Ncomp, Nfft, ist_comp, X)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: Ncomp_CPU, ist_comp
!!        integer(kind = kint), intent(in) :: Nfft, Ncomp
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!        type(working_FFTs), intent(inout) :: WKS
!!
!!      subroutine sel_copy_pout_spectr_to_FFT(iflag_FFT, Ncomp_CPU,    &
!!     &          Ncomp, Nfft, ist_comp, X, WKS)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: Ncomp_CPU, ist_comp
!!        integer(kind = kint), intent(in) :: Nfft, Ncomp
!!        real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!!        type(working_FFTs), intent(inout) :: WKS
!!      subroutine sel_copy_pout_field_from_FFT(iflag_FFT, Ncomp_CPU,   &
!!     &          WKS, Ncomp, Nfft, ist_comp, X)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        type(working_FFTs), intent(in) :: WKS
!!        integer(kind = kint), intent(in) :: Ncomp_CPU, ist_comp
!!        integer(kind = kint), intent(in) :: Nfft, Ncomp
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!@endverbatim
!
      module sel_copy_field_pout_FFT
!
      use omp_lib
!
      use m_precision
      use m_machine_parameter
      use m_FFT_selector
!
      use t_FFT_selector
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine sel_norm_pout_field_to_FFT(iflag_FFT, Ncomp_CPU,       &
     &          Ncomp, Nfft, ist_comp, X, WKS)
!
      use transfer_to_long_integers
      use copy_field_for_FFT
      use normalize_for_FFTPACK
      use normalize_for_ISPACK
      use swap_rtp_data_for_FFTW
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: Ncomp_CPU, ist_comp
      integer(kind = kint), intent(in) :: Nfft, Ncomp
      real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!
      type(working_FFTs), intent(inout) :: WKS
!
!
      if     ((iflag_FFT/10) .eq. (iflag_ISPACK0/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'copy_rtp_fld_to_FXRTFA'
        call copy_rtp_fld_to_FXRTFA(WKS%WK_ISPACK1%Nplan_ISPACK,        &
     &      WKS%WK_ISPACK1%istack_ISPACK, WKS%WK_ISPACK1%Mmax_smp,      &
     &      cast_long(Nfft), cast_long(Ncomp), X(1,1),                  &
     &      WKS%WK_ISPACK1%X_ispack(1,1))
      else if((iflag_FFT/10) .eq. (iflag_ISPACK3/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'copy_rtp_fld_to_FXRTFA'
        call copy_rtp_fld_to_FXRTFA(WKS%WK_ISPACK3%Nplan_ISPACK3,       &
     &      WKS%WK_ISPACK3%istack_ISPACK3, WKS%WK_ISPACK3%Mmax_smp,     &
     &      cast_long(Nfft), cast_long(Ncomp), X(1,1),                  &
     &      WKS%WK_ISPACK3%X_ispack(1,1))
#ifdef FFTW3
      else if((iflag_FFT/10) .eq. (iflag_FFTW/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_to_rtp_fwd_FFTW'
        call swap_to_rtp_fwd_FFTW(ist_comp, Ncomp, Nfft, X(1,1),        &
     &      Ncomp_CPU, Nfft, WKS%WK_MUL_FFTW%X_FFTW_mul(1,1))
#endif
#ifdef OMP_FFTW3
      else if((iflag_FFT/10) .eq. (iflag_OMP_FFTW/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'copy_pout_field_to_FFT'
        call copy_pout_field_to_FFT(ist_comp, Ncomp, Nfft, X(1,1),      &
     &      Ncomp_CPU, Nfft, WKS%WK_MUL_FFTW%X_FFTW_mul(1,1))
#endif
      else if((iflag_FFT/10) .eq. (iflag_FFTPACK/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'copy_rtp_fld_to_RFFTMF'
        call copy_rtp_fld_to_RFFTMF(WKS%WK_FFTPACK%Nplan_FFTPACK,       &
     &      WKS%WK_FFTPACK%istack_FFTPACK, WKS%WK_FFTPACK%Mmax_smp,     &
     &      Nfft, Ncomp, X(1,1), WKS%WK_FFTPACK%X_FFTPACK5)
      end if
!
      end subroutine sel_norm_pout_field_to_FFT
!
! ------------------------------------------------------------------
!
      subroutine sel_norm_pout_spectr_from_FFT(iflag_FFT, Ncomp_CPU,    &
     &          WKS, Ncomp, Nfft, ist_comp, X)
!
      use transfer_to_long_integers
      use copy_field_for_FFT
      use swap_rtp_data_for_FFTW
      use normalize_for_FFTW
      use normalize_for_OMP_FFTW
      use normalize_for_FFTPACK
      use normalize_for_ISPACK
!
      integer(kind = kint), intent(in) :: iflag_FFT
!
      integer(kind = kint), intent(in) :: Ncomp_CPU, ist_comp
      integer(kind = kint), intent(in) :: Nfft, Ncomp
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
      type(working_FFTs), intent(inout) :: WKS
!
!
      if     ((iflag_FFT/10) .eq. (iflag_ISPACK0/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'norm_rtp_spectr_from_FXRTFA'
        call norm_rtp_spectr_from_FXRTFA(WKS%WK_ISPACK1%Nplan_ISPACK,   &
     &     WKS%WK_ISPACK1%istack_ISPACK, WKS%WK_ISPACK1%Mmax_smp,       &
     &     cast_long(Nfft), WKS%WK_ISPACK1%X_ispack(1,1),               &
     &     cast_long(Ncomp), X(1,1))
      else if((iflag_FFT/10) .eq. (iflag_ISPACK3/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'norm_rtp_spectr_from_FXRTFA'
        call norm_rtp_spectr_from_FXRTFA(WKS%WK_ISPACK3%Nplan_ISPACK3,  &
     &     WKS%WK_ISPACK3%istack_ISPACK3,  WKS%WK_ISPACK3%Mmax_smp,     &
     &     cast_long(Nfft), WKS%WK_ISPACK3%X_ispack(1,1),               &
     &     cast_long(Ncomp), X(1,1))
#ifdef FFTW3
      else if((iflag_FFT/10) .eq. (iflag_FFTW/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_from_rtp_fwd_OMP_FFTW'
        call normalize_fwd_OMP_FFTW(WKS%WK_MUL_FFTW%aNfft, Ncomp_CPU,   &
     &      WKS%WK_MUL_FFTW%Nfft_c, WKS%WK_MUL_FFTW%C_FFTW_mul(1,1))
        call swap_from_rtp_fwd_OMP_FFTW(Ncomp_CPU,                      &
     &      WKS%WK_MUL_FFTW%Nfft_c, WKS%WK_MUL_FFTW%C_FFTW_mul(1,1),    &
     &      Ncomp, Nfft, ist_comp, X(1,1))
#endif
#ifdef OMP_FFTW3
      else if((iflag_FFT/10) .eq. (iflag_OMP_FFTW/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'norm_rtp_from_fwd_OMP_FFTW'
        call normalize_fwd_OMP_FFTW(WKS%WK_MUL_FFTW%aNfft, Ncomp_CPU,   &
     &      WKS%WK_MUL_FFTW%Nfft_c, WKS%WK_MUL_FFTW%C_FFTW_mul(1,1))
        call norm_rtp_from_fwd_OMP_FFTW(Ncomp_CPU,                      &
     &      WKS%WK_MUL_FFTW%Nfft_c, WKS%WK_MUL_FFTW%C_FFTW_mul(1,1),    &
     &      ist_comp, Ncomp, Nfft, X(1,1))
#endif
      else if((iflag_FFT/10) .eq. (iflag_FFTPACK/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'copy_rtp_spectr_from_RFFTMF'
        call copy_rtp_spectr_from_RFFTMF(WKS%WK_FFTPACK%Nplan_FFTPACK,  &
     &      WKS%WK_FFTPACK%istack_FFTPACK, WKS%WK_FFTPACK%Mmax_smp,     &
     &      Nfft, WKS%WK_FFTPACK%X_FFTPACK5, Ncomp, X(1,1))
      end if
!
      end subroutine sel_norm_pout_spectr_from_FFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sel_copy_pout_spectr_to_FFT(iflag_FFT, Ncomp_CPU,      &
     &          Ncomp, Nfft, ist_comp, X, WKS)
!
      use transfer_to_long_integers
      use normalize_for_FFTPACK
      use normalize_for_ISPACK
      use normalize_for_OMP_FFTW
      use swap_rtp_data_for_FFTW
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: Ncomp_CPU, ist_comp
      integer(kind = kint), intent(in) :: Nfft, Ncomp
      real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!
      type(working_FFTs), intent(inout) :: WKS
!
!
      if     ((iflag_FFT/10) .eq. (iflag_ISPACK0/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'norm_rtp_spectr_to_FXRTBA'
        call norm_rtp_spectr_to_FXRTBA(WKS%WK_ISPACK1%Nplan_ISPACK,     &
     &      WKS%WK_ISPACK1%istack_ISPACK, WKS%WK_ISPACK1%Mmax_smp,      &
     &      cast_long(Nfft), cast_long(Ncomp), X(1,1),                  &
     &      WKS%WK_ISPACK1%X_ispack(1,1))
      else if((iflag_FFT/10) .eq. (iflag_ISPACK3/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'norm_rtp_spectr_to_FXRTBA'
        call norm_rtp_spectr_to_FXRTBA(WKS%WK_ISPACK3%Nplan_ISPACK3,    &
     &      WKS%WK_ISPACK3%istack_ISPACK3, WKS%WK_ISPACK3%Mmax_smp,     &
     &      cast_long(Nfft), cast_long(Ncomp), X(1,1),                  &
     &      WKS%WK_ISPACK3%X_ispack(1,1))
#ifdef FFTW3
      else if((iflag_FFT/10) .eq. (iflag_FFTW/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_to_rtp_bwd_OMP_FFTW'
        call swap_to_rtp_bwd_OMP_FFTW                                   &
     &     (Ncomp, Nfft, ist_comp, X(1,1), Ncomp_CPU,                   &
     &      WKS%WK_MUL_FFTW%Nfft_c, WKS%WK_MUL_FFTW%C_FFTW_mul(1,1))
#endif
#ifdef OMP_FFTW3
      else if((iflag_FFT/10) .eq. (iflag_OMP_FFTW/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'norm_rtp_to_bwd_OMP_FFTW'
        call norm_rtp_to_bwd_OMP_FFTW(ist_comp, Ncomp, Nfft, X(1,1),    &
     &                               Ncomp_CPU, WKS%WK_MUL_FFTW%NFFT_c, &
     &                               WKS%WK_MUL_FFTW%C_FFTW_mul(1,1))
#endif
      else if((iflag_FFT/10) .eq. (iflag_FFTPACK/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'copy_rtp_spectr_to_RFFTMB'
        call copy_rtp_spectr_to_RFFTMB(WKS%WK_FFTPACK%Nplan_FFTPACK,    &
     &      WKS%WK_FFTPACK%istack_FFTPACK, WKS%WK_FFTPACK%Mmax_smp,     &
     &      Nfft, Ncomp, X(1,1), WKS%WK_FFTPACK%X_FFTPACK5(1,1))
      end if
!
      end subroutine sel_copy_pout_spectr_to_FFT
!
! ------------------------------------------------------------------
!
      subroutine sel_copy_pout_field_from_FFT(iflag_FFT, Ncomp_CPU,     &
     &          WKS, Ncomp, Nfft, ist_comp, X)
!
      use transfer_to_long_integers
      use copy_field_for_FFT
      use normalize_for_FFTPACK
      use normalize_for_ISPACK
      use swap_rtp_data_for_FFTW
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(working_FFTs), intent(in) :: WKS
!
      integer(kind = kint), intent(in) :: Ncomp_CPU, ist_comp
      integer(kind = kint), intent(in) :: Nfft, Ncomp
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!
!
      if     ((iflag_FFT/10) .eq. (iflag_ISPACK0/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'copy_rtp_fld_from_FXRTBA'
        call copy_rtp_fld_from_FXRTBA(WKS%WK_ISPACK1%Nplan_ISPACK,      &
     &      WKS%WK_ISPACK1%istack_ISPACK, WKS%WK_ISPACK1%Mmax_smp,      &
     &      cast_long(Nfft), WKS%WK_ISPACK1%X_ispack(1,1),              &
     &      cast_long(Ncomp), X(1,1))
      else if((iflag_FFT/10) .eq. (iflag_ISPACK3/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'copy_rtp_fld_from_FXRTBA'
        call copy_rtp_fld_from_FXRTBA(WKS%WK_ISPACK3%Nplan_ISPACK3,     &
     &      WKS%WK_ISPACK3%istack_ISPACK3, WKS%WK_ISPACK3%Mmax_smp,     &
     &      cast_long(Nfft), WKS%WK_ISPACK3%X_ispack(1,1),              &
     &      cast_long(Ncomp), X(1,1))
#ifdef FFTW3
      else if((iflag_FFT/10) .eq. (iflag_FFTW/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_from_rtp_bwd_FFTW'
        call swap_from_rtp_bwd_FFTW                                     &
     &     (Ncomp_CPU, Nfft, WKS%WK_MUL_FFTW%X_FFTW_mul(1,1),           &
     &      ist_comp, Ncomp, Nfft, X(1,1))
#endif
#ifdef OMP_FFTW3
      else if((iflag_FFT/10) .eq. (iflag_OMP_FFTW/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'copy_pout_field_from_FFT'
        call copy_pout_field_from_FFT                                   &
     &     (Ncomp_CPU, Nfft, WKS%WK_MUL_FFTW%X_FFTW_mul(1,1),           &
     &      Ncomp, Nfft, ist_comp, X(1,1))
#endif
      else if((iflag_FFT/10) .eq. (iflag_FFTPACK/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'copy_rtp_fld_from_RFFTMB'
        call copy_rtp_fld_from_RFFTMB(WKS%WK_FFTPACK%Nplan_FFTPACK,     &
     &      WKS%WK_FFTPACK%istack_FFTPACK, WKS%WK_FFTPACK%Mmax_smp,     &
     &      Nfft, WKS%WK_FFTPACK%X_FFTPACK5(1,1), Ncomp, X(1,1))
      end if
!
      end subroutine sel_copy_pout_field_from_FFT
!
! ------------------------------------------------------------------
!
      end module sel_copy_field_pout_FFT
