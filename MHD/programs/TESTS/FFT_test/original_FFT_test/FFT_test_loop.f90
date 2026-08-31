!!@brief  module FFT_test_loop
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!!
!!
!>@brief Main loop for FFT test
!!
!!@verbatim
!!      subroutine FFT_test_with_phi_out_data(iflag_FFT, ft, WK_FFT)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        type(fft_test_data), intent(inout) :: ft
!!        type(working_FFTs), intent(inout) :: WK_FFT
!!      subroutine FFT_test_with_phi_in_data(iflag_FFT, n_loop,         &
!!     &                                     ft, WK_FFT)
!!        integer(kind = kint), intent(in) :: iflag_FFT, n_loop
!!        type(fft_test_data), intent(inout) :: ft
!!        type(working_FFTs), intent(inout) :: WK_FFT
!!@endverbatim
      module FFT_test_loop
!
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_FFT_selector
      use t_fft_test_data
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine FFT_test_with_phi_out_data(iflag_FFT, n_loop,          &
     &                                      ft, WK_FFT)
!
      integer(kind = kint), intent(in) :: iflag_FFT, n_loop
      type(fft_test_data), intent(inout) :: ft
      type(working_FFTs), intent(inout) :: WK_FFT
!
      integer(kind = kint) :: iloop = 0
!
!
      call initialize_FFT_select(0, iflag_FFT, np_smp, ft%nstack,       &
     &                           ft%ngrd, WK_FFT, ft%elapsed(1))
!
      do iloop = 1, n_loop
        if(mod(iloop, 20) .eq. 0) write(*,*) 'loop count: ', iloop
!
        ft%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft%s_k(1:ft%nfld,1:ft%ngrd) = ft%org(1:ft%nfld,1:ft%ngrd)
!$omp end parallel workshare
        ft%elapsed(3) = ft%elapsed(3) + OMP_GET_WTIME() - ft%start
!
        call forward_FFT_select(iflag_FFT, ft%nfld, ft%ngrd, ft%s_k,    &
     &                          WK_FFT, ft%elapsed(2), ft%elapsed(3))
!
        ft%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft%f_x(1:ft%nfld,1:ft%ngrd) = ft%s_k(1:ft%nfld,1:ft%ngrd)
!$omp end parallel workshare
        ft%elapsed(3) = ft%elapsed(3) + OMP_GET_WTIME() - ft%start
!
        call backward_FFT_select(iflag_FFT, ft%nfld, ft%ngrd, ft%f_x,   &
     &                           WK_FFT, ft%elapsed(2), ft%elapsed(3))
      end do
!
      end subroutine FFT_test_with_phi_out_data
!
! ------------------------------------------------------------------
!
      subroutine FFT_test_with_phi_in_data(iflag_FFT, n_loop,           &
     &                                     ft, WK_FFT)
!
      use phi_inside_FFT_selector
!
      integer(kind = kint), intent(in) :: iflag_FFT, n_loop
      type(fft_test_data), intent(inout) :: ft
      type(working_FFTs), intent(inout) :: WK_FFT
!
      integer(kind = kint) :: iloop = 0
!
!
      write(*,*) 'swap_fft_test_input_to_pin'
      call swap_fft_test_input_to_pin(ft)
      call init_pin_FFT_select(0, iflag_FFT, np_smp, ft%nstack,         &
     &                         ft%ngrd, WK_FFT, ft%elapsed(1))
!
      do iloop = 1, n_loop
        if(mod(iloop, 20) .eq. 0) write(*,*) 'loop count: ', iloop
!
        ft%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft%s_k(1:ft%ngrd,1:ft%nfld) = ft%org(1:ft%ngrd,1:ft%nfld)
!$omp end parallel workshare
        ft%elapsed(3) = ft%elapsed(3) + OMP_GET_WTIME() - ft%start
!
        call fwd_pin_FFT_select(iflag_FFT, ft%nfld, ft%ngrd, ft%s_k,    &
     &                          WK_FFT, ft%elapsed(2), ft%elapsed(3))
!
        ft%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft%f_x(1:ft%ngrd,1:ft%nfld) = ft%s_k(1:ft%ngrd,1:ft%nfld)
!$omp end parallel workshare
        ft%elapsed(3) = ft%elapsed(3) + OMP_GET_WTIME() - ft%start
!
        call back_pin_FFT_select(iflag_FFT, ft%nfld, ft%ngrd, ft%f_x,   &
     &                           WK_FFT, ft%elapsed(2), ft%elapsed(3))
      end do
!
      call swap_fft_test_data_to_pout(ft)
!
      end subroutine FFT_test_with_phi_in_data
!
! ------------------------------------------------------------------
!
      end module FFT_test_loop
