!>@file   sel_swap_field_pin_FFT.F90
!!@brief  module sel_swap_field_pin_FFT
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2026
!>@brief  Selector for FFT
!!
!!@verbatim
!!      subroutine sel_swap_pin_field_to_FFT(iflag_FFT,                 &
!!     &                                     Ncomp, Nfft, X, WKS)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: Nfft, Ncomp
!!        real(kind = kreal), intent(in) :: X(Nfft,Ncomp)
!!        type(working_FFTs), intent(inout) :: WKS
!!      subroutine sel_swap_prt_spectr_from_FFT(iflag_FFT, WKS,         &
!!     &                                        Ncomp, Nfft, X)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        type(working_FFTs), intent(in) :: WKS
!!        integer(kind = kint), intent(in) :: Nfft, Ncomp
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!
!!      subroutine sel_swap_pin_spectr_to_FFT(iflag_FFT, Ncomp, Nfft,   &
!!     &                                      X, WKS)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: Nfft, Ncomp
!!        real(kind = kreal), intent(in) :: X(Nfft,Ncomp)
!!        type(working_FFTs), intent(inout) :: WKS
!!      subroutine sel_swap_pin_field_from_FFT(iflag_FFT, WKS,          &
!!     &                                       Ncomp, Nfft, X)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        type(working_FFTs), intent(in) :: WKS
!!        integer(kind = kint), intent(in) :: Nfft, Ncomp
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!@endverbatim
!
      module sel_swap_field_pin_FFT
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
      subroutine sel_swap_pin_field_to_FFT(iflag_FFT,                   &
     &                                     Ncomp, Nfft, X, WKS)
!
      use transfer_to_long_integers
      use swap_prt_data_for_ISPACK
      use swap_prt_data_for_FFTPACK
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: Nfft, Ncomp
      real(kind = kreal), intent(in) :: X(Nfft,Ncomp)
!
      type(working_FFTs), intent(inout) :: WKS
!
!
      if     ((iflag_FFT/10) .eq. (iflag_ISPACK0/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_prt_fld_to_FXRTFA'
        call swap_prt_fld_to_FXRTFA(WKS%WK_ISPACK1%Nplan_ISPACK,        &
     &      WKS%WK_ISPACK1%istack_ISPACK, WKS%WK_ISPACK1%Mmax_smp,      &
     &      cast_long(Nfft), cast_long(Ncomp), X(1,1),                  &
     &      WKS%WK_ISPACK1%X_ispack(1,1))
      else if((iflag_FFT/10) .eq. (iflag_ISPACK3/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_prt_fld_to_FXRTFA'
        call swap_prt_fld_to_FXRTFA(WKS%WK_ISPACK3%Nplan_ISPACK3,       &
     &      WKS%WK_ISPACK3%istack_ISPACK3, WKS%WK_ISPACK3%Mmax_smp,     &
     &      cast_long(Nfft), cast_long(Ncomp), X(1,1),                  &
     &      WKS%WK_ISPACK3%X_ispack(1,1))
!      else if((iflag_FFT/10) .eq. (iflag_FFTW/10)) then
!      else if((iflag_FFT/10) .eq. (iflag_OMP_FFTW/10)) then
      else if((iflag_FFT/10) .eq. (iflag_FFTPACK/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_prt_fld_to_RFFTMF'
        call swap_prt_fld_to_RFFTMF(WKS%WK_FFTPACK%Nplan_FFTPACK,       &
     &      WKS%WK_FFTPACK%istack_FFTPACK, WKS%WK_FFTPACK%Mmax_smp,     &
     &      Nfft, Ncomp, X(1,1), WKS%WK_FFTPACK%X_FFTPACK5(1,1))
      end if
!
      end subroutine sel_swap_pin_field_to_FFT
!
! ------------------------------------------------------------------
!
      subroutine sel_swap_prt_spectr_from_FFT(iflag_FFT, WKS,           &
     &                                        Ncomp, Nfft, X)
!
      use transfer_to_long_integers
      use normalize_for_FFTW
      use swap_prt_data_for_ISPACK
      use swap_prt_data_for_FFTPACK
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: Nfft, Ncomp
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
      type(working_FFTs), intent(inout) :: WKS
!
!
      if     ((iflag_FFT/10) .eq. (iflag_ISPACK0/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_prt_spectr_from_FXRTFA'
        call swap_prt_spectr_from_FXRTFA(WKS%WK_ISPACK1%Nplan_ISPACK,   &
     &      WKS%WK_ISPACK1%istack_ISPACK, WKS%WK_ISPACK1%Mmax_smp,      &
     &      cast_long(Nfft), WKS%WK_ISPACK1%X_ispack(1,1),              &
     &      cast_long(Ncomp), X(1,1))
      else if((iflag_FFT/10) .eq. (iflag_ISPACK3/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_prt_spectr_from_FXRTFA'
        call swap_prt_spectr_from_FXRTFA(WKS%WK_ISPACK3%Nplan_ISPACK3,  &
     &      WKS%WK_ISPACK3%istack_ISPACK3, WKS%WK_ISPACK3%Mmax_smp,     &
     &      cast_long(Nfft), WKS%WK_ISPACK3%X_ispack(1,1),              &
     &      cast_long(Ncomp), X(1,1))
#ifdef FFTW3
      else if((iflag_FFT/10) .eq. (iflag_FFTW/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'copy_from_prt_fwd_OMP_FFTW'
        call normalize_fwd_OMP_FFTW(WKS%WK_MUL_FFTW%aNfft, Ncomp,       &
     &      WKS%WK_MUL_FFTW%Nfft_c, WKS%WK_MUL_FFTW%C_FFTW_mul(1,1))
        call copy_from_prt_fwd_OMP_FFTW(Ncomp, WKS%WK_MUL_FFTW%Nfft_c,  &
     &      WKS%WK_MUL_FFTW%C_FFTW_mul(1,1), Ncomp, Nfft, X(1,1))
#endif
#ifdef OMP_FFTW3
      else if((iflag_FFT/10) .eq. (iflag_OMP_FFTW/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'normalize_fwd_OMP_FFTW'
        call normalize_fwd_OMP_FFTW(WKS%WK_MUL_FFTW%aNfft, Ncomp,       &
     &      WKS%WK_MUL_FFTW%Nfft_c, WKS%WK_MUL_FFTW%C_FFTW_mul(1,1))
        call copy_from_prt_fwd_OMP_FFTW(Ncomp, WKS%WK_MUL_FFTW%Nfft_c,  &
     &      WKS%WK_MUL_FFTW%C_FFTW_mul(1,1), Ncomp, Nfft, X(1,1))
#endif
      else if((iflag_FFT/10) .eq. (iflag_FFTPACK/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_prt_spectr_from_RFFTMF'
        call swap_prt_spectr_from_RFFTMF(WKS%WK_FFTPACK%Nplan_FFTPACK,  &
     &      WKS%WK_FFTPACK%istack_FFTPACK, WKS%WK_FFTPACK%Mmax_smp,     &
     &      Nfft, WKS%WK_FFTPACK%X_FFTPACK5(1,1), Ncomp, X(1,1))
      end if
!
      end subroutine sel_swap_prt_spectr_from_FFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sel_swap_pin_spectr_to_FFT(iflag_FFT, Ncomp, Nfft,     &
     &                                      X, WKS)
!
      use transfer_to_long_integers
      use swap_prt_data_for_ISPACK
      use swap_prt_data_for_FFTPACK
      use normalize_for_FFTW
!
      integer(kind = kint), intent(in) :: iflag_FFT
!
      integer(kind = kint), intent(in) :: Nfft, Ncomp
      real(kind = kreal), intent(in) :: X(Nfft,Ncomp)
!
      type(working_FFTs), intent(inout) :: WKS
!
!
      if     ((iflag_FFT/10) .eq. (iflag_ISPACK0/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_prt_spectr_to_FXRTBA'
        call swap_prt_spectr_to_FXRTBA(WKS%WK_ISPACK1%Nplan_ISPACK,     &
     &      WKS%WK_ISPACK1%istack_ISPACK, WKS%WK_ISPACK1%Mmax_smp,      &
     &      cast_long(Nfft), cast_long(Ncomp), X(1,1),                  &
     &      WKS%WK_ISPACK1%X_ispack(1,1))
      else if((iflag_FFT/10) .eq. (iflag_ISPACK3/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_prt_spectr_to_FXRTBA'
        call swap_prt_spectr_to_FXRTBA(WKS%WK_ISPACK3%Nplan_ISPACK3,    &
     &      WKS%WK_ISPACK3%istack_ISPACK3, WKS%WK_ISPACK3%Mmax_smp,     &
     &      cast_long(Nfft), cast_long(Ncomp), X(1,1),                  &
     &      WKS%WK_ISPACK3%X_ispack(1,1))
#ifdef FFTW3
      else if((iflag_FFT/10) .eq. (iflag_FFTW/10)) then
        if(iflag_debug .gt. 0)                                          &
     &      write(*,*) 'norm_copy_to_prt_bwd_OMP_FFTW'
        call norm_copy_to_prt_bwd_OMP_FFTW                              &
     &     (Ncomp, Nfft, X(1,1), Ncomp,                                 &
     &      WKS%WK_MUL_FFTW%Nfft_c, WKS%WK_MUL_FFTW%C_FFTW_mul(1,1))
#endif
#ifdef OMP_FFTW3
      else if((iflag_FFT/10) .eq. (iflag_OMP_FFTW/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'norm_copy_to_prt_bwd_OMP_FFTW'
        call norm_copy_to_prt_bwd_OMP_FFTW                              &
     &     (Ncomp, Nfft, X(1,1), Ncomp,                                 &
     &      WKS%WK_MUL_FFTW%Nfft_c, WKS%WK_MUL_FFTW%C_FFTW_mul(1,1))
#endif
      else if((iflag_FFT/10) .eq. (iflag_FFTPACK/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_prt_spectr_to_RFFTMB'
        call swap_prt_spectr_to_RFFTMB(WKS%WK_FFTPACK%Nplan_FFTPACK,    &
     &      WKS%WK_FFTPACK%istack_FFTPACK, WKS%WK_FFTPACK%Mmax_smp,     &
     &      Nfft, Ncomp, X(1,1), WKS%WK_FFTPACK%X_FFTPACK5(1,1))
      end if
!
      end subroutine sel_swap_pin_spectr_to_FFT
!
! ------------------------------------------------------------------
!
      subroutine sel_swap_pin_field_from_FFT(iflag_FFT, WKS,            &
     &                                       Ncomp, Nfft, X)
!
      use transfer_to_long_integers
      use swap_prt_data_for_ISPACK
      use swap_prt_data_for_FFTPACK
      use normalize_for_FFTW
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(working_FFTs), intent(in) :: WKS
!
      integer(kind = kint), intent(in) :: Nfft, Ncomp
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!
!
      if     ((iflag_FFT/10) .eq. (iflag_ISPACK0/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_prt_fld_from_FXRTBA'
        call swap_prt_fld_from_FXRTBA(WKS%WK_ISPACK1%Nplan_ISPACK,      &
     &      WKS%WK_ISPACK1%istack_ISPACK, WKS%WK_ISPACK1%Mmax_smp,      &
     &      cast_long(Nfft), WKS%WK_ISPACK1%X_ispack(1,1),              &
     &      cast_long(Ncomp), X(1,1))
      else if((iflag_FFT/10) .eq. (iflag_ISPACK3/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_prt_fld_from_FXRTBA'
        call swap_prt_fld_from_FXRTBA                                   &
     &    (WKS%WK_ISPACK3%Nplan_ISPACK3, WKS%WK_ISPACK3%istack_ISPACK3, &
     &     WKS%WK_ISPACK3%Mmax_smp, cast_long(Nfft),                    &
     &     WKS%WK_ISPACK3%X_ispack(1,1), cast_long(Ncomp), X(1,1))
      else if((iflag_FFT/10) .eq. (iflag_FFTPACK/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'swap_prt_fld_from_RFFTMB'
        call swap_prt_fld_from_RFFTMB(WKS%WK_FFTPACK%Nplan_FFTPACK,     &
     &      WKS%WK_FFTPACK%istack_FFTPACK, WKS%WK_FFTPACK%Mmax_smp,     &
     &      Nfft, WKS%WK_FFTPACK%X_FFTPACK5(1,1), Ncomp, X(1,1))
      end if
!
      end subroutine sel_swap_pin_field_from_FFT
!
! ------------------------------------------------------------------
!
      end module sel_swap_field_pin_FFT
