!
      program test_ROCmfft_rtp
!
      use iso_c_binding
      use omp_lib
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use m_FFT_size
      use t_fft_test_data
      use t_ROCmFFT_wrapper
      use normalize_for_OMP_FFTW
!
      implicit none
!
      character(len=kchara) :: file_name = 'rtp_ROCmfft_test.dat'
      real(kind = kreal) :: start, finish, elapsed(3)
!
      type(fft_test_data) :: ft1
!
      type(calypso_ROCmfft_params), target :: fwd
      type(calypso_ROCmfft_params), target :: bwd
      type(calypso_ROCmfft_work), target :: WK_fwd
      type(calypso_ROCmfft_work), target :: WK_bwd
!
      integer(c_size_t), parameter :: ione_c =  ione
!
      integer(kind = kint) :: i, nd
      integer(kind = kint) :: icou
!
!
      np_smp = omp_get_max_threads()
      write(*,*) 'Number of threads:  ', np_smp
      call init_fft_test_data(n_field, ngrid, ft1)
!
      call calypso_ROCmFFT_set_size(n_field, ngrid, fwd, WK_fwd)
      call calypso_ROCmFFT_set_size(n_field, ngrid, bwd, WK_bwd)
!
      allocate(WK_fwd%X_ROCmFFT(n_field,WK_fwd%Nfft_r))
      allocate(WK_bwd%X_ROCmFFT(n_field,WK_bwd%Nfft_r))
!$omp parallel workshare
      WK_fwd%X_ROCmFFT(1:n_field,1:WK_fwd%Nfft_r) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
      WK_bwd%X_ROCmFFT(1:n_field,1:WK_bwd%Nfft_r) = 0.0d0
!$omp end parallel workshare
!
      allocate(WK_fwd%C_ROCmFFT(n_field,WK_fwd%Nfft_c))
      allocate(WK_bwd%C_ROCmFFT(n_field,WK_bwd%Nfft_c))
!$omp parallel workshare
      WK_fwd%C_ROCmFFT(1:n_field,1:WK_fwd%Nfft_c) = 0.0d0
      WK_bwd%C_ROCmFFT(1:n_field,1:WK_bwd%Nfft_c) = 0.0d0
!$omp end parallel workshare
!
!
!   Initialize Forward transform
      start = OMP_GET_WTIME()
!
      fwd%in_strides_size =   1
      fwd%in_strides(1) =     n_field
      fwd%in_distance =       1
      fwd%out_strides_size =  1
      fwd%out_strides(1) = n_field
      fwd%out_distance = 1
!
      call hipCheck(hipMalloc(WK_fwd%data_ptr,WK_fwd%Nbytes))
!
      call rocfftCheck                                                  &
     &   (rocfft_plan_description_create(fwd%ROCfft_description))
      call rocfftCheck(rocfft_plan_description_set_data_layout          &
     &                                      (fwd%ROCfft_description,    &
     &                                       rocfft_array_type_real,    &
     &                                      rocfft_array_type_unset,    &
     &                                               fwd%in_offsets,    &
     &                                              fwd%out_offsets,    &
     &                                          fwd%in_strides_size,    &
     &                                     c_loc(fwd%in_strides(1)),    &
     &                                              fwd%in_distance,    &
     &                                         fwd%out_strides_size,    &
     &                                    c_loc(fwd%out_strides(1)),    &
     &                                            fwd%out_distance))
      call rocfftCheck(rocfft_plan_create(fwd%ROCfft_plan,              &
     &                                    rocfft_placement_inplace,     &
     &                          rocfft_transform_type_real_forward,     &
     &                                     rocfft_precision_double,     &
     &                                     ione_c, c_loc(fwd%Nfft),     &
     &                           fwd%Ncomp, fwd%ROCfft_description))
!
      call rocfftCheck                                                  &
     &   (rocfft_plan_get_work_buffer_size(fwd%ROCfft_plan,             &
     &                                     fwd%ROCfft_wk_buf_size))
      write(*,*) 'fwd%ROCfft_wk_buf_size', fwd%ROCfft_wk_buf_size
      call rocfftCheck                                                  &
     &   (rocfft_execution_info_create(fwd%ROCfft_wk_info))
      if(fwd%ROCfft_wk_buf_size > 0) then
        call hipCheck(hipMalloc(fwd%ROCfft_wk_buffer,                   &
     &                          fwd%ROCfft_wk_buf_size))
        call rocfftCheck(rocfft_execution_info_set_work_buffer          &
     &                                          (fwd%ROCfft_wk_info,    &
     &                                         fwd%ROCfft_wk_buffer,    &
     &                                       fwd%ROCfft_wk_buf_size))
      end if
!
!   Initialize Backword transform
      bwd%in_strides_size =  1
      bwd%in_strides(1) =    n_field
      bwd%in_distance =      1
      bwd%out_strides_size = 1
      bwd%out_strides(1) =   n_field
      bwd%out_distance =     1
!
      call hipCheck(hipMalloc(WK_bwd%data_ptr,WK_fwd%Nbytes))
!
      call rocfftCheck                                                  &
     &   (rocfft_plan_description_create(bwd%ROCfft_description))
      call rocfftCheck(rocfft_plan_description_set_data_layout          &
     &                                      (bwd%ROCfft_description,    &
     &                                      rocfft_array_type_unset,    &
     &                                       rocfft_array_type_real,    &
     &                                               bwd%in_offsets,    &
     &                                              bwd%out_offsets,    &
     &                                          bwd%in_strides_size,    &
     &                                     c_loc(bwd%in_strides(1)),    &
     &                                              bwd%in_distance,    &
     &                                         bwd%out_strides_size,    &
     &                                    c_loc(bwd%out_strides(1)),    &
     &                                            bwd%out_distance))
!
      call rocfftCheck(rocfft_plan_create(bwd%ROCfft_plan,              &
     &                                      rocfft_placement_inplace,   &
     &                            rocfft_transform_type_real_inverse,   &
     &                                       rocfft_precision_double,   &
     &                                       ione_c, c_loc(fwd%Nfft),   &
     &                            bwd%Ncomp, bwd%ROCfft_description))
!
      call rocfftCheck                                                  &
     &   (rocfft_plan_get_work_buffer_size(bwd%ROCfft_plan,             &
     &                                     bwd%ROCfft_wk_buf_size))
      write(*,*) 'bwd%ROCfft_wk_buf_size', bwd%ROCfft_wk_buf_size
      call rocfftCheck                                                  &
     &   (rocfft_execution_info_create(bwd%ROCfft_wk_info))
      if(bwd%ROCfft_wk_buf_size > 0) then
        call hipCheck(hipMalloc(bwd%ROCfft_wk_buffer,                   &
     &                          bwd%ROCfft_wk_buf_size))
        call rocfftCheck(rocfft_execution_info_set_work_buffer          &
     &                                        (bwd%ROCfft_wk_info,      &
     &                                         bwd%ROCfft_wk_buffer,    &
     &                                         bwd%ROCfft_wk_buf_size))
      end if
      elapsed(3) = OMP_GET_WTIME() - start
!
      elapsed(1:2) = zero
      do icou = 1, n_loop
        if(mod(icou, 20) .eq. 0) write(*,*) 'loop count: ', icou
!
        start = OMP_GET_WTIME()
!$omp target teams distribute parallel do collapse(2)
        do i = 1, ft1%ngrd
          do nd = 1, ft1%nfld
            ft1%s_k(nd,i) = ft1%org(nd,i)
          end do
        end do
!$omp end target teams distribute parallel do
!
!   Forward transform
!$omp target teams distribute parallel do collapse(2)
        do i = 1, ft1%ngrd
          do nd = 1, ft1%nfld
            WK_fwd%X_ROCmFFT(nd,i) = ft1%s_k(nd,i)
          end do
        end do
!$omp end target teams distribute parallel do
        if(ft1%ngrd .lt. WK_fwd%Nfft_r) then
!$omp target teams distribute parallel do collapse(2)
          do i = ft1%ngrd+1, WK_fwd%Nfft_r
            do nd = 1, ft1%nfld
              WK_fwd%X_ROCmFFT(nd,i) = 0.0d0
            end do
          end do
!$omp end target teams distribute parallel do
        end if
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_forward_ROCmFFT(fwd,                               &
     &                            WK_fwd%Nfft_r, WK_fwd%X_ROCmFFT(1,1), &
     &                            WK_fwd%Nfft_c, WK_fwd%C_ROCmFFT(1,1), &
     &                            WK_fwd%Nbytes, WK_fwd%data_ptr)
        elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call norm_rtp_from_fwd_OMP_FFTW(ft1%nfld, WK_fwd%aNfft,         &
     &      WK_fwd%NFFT_c, WK_fwd%C_ROCmFFT(1,1), ft1%ngrd, ft1%s_k(1,1))
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
!$omp target teams distribute parallel do collapse(2)
        do i = 1, ft1%ngrd
          do nd = 1, ft1%nfld
            ft1%f_x(nd,i) = ft1%s_k(nd,i)
          end do
        end do
!$omp end target teams distribute parallel do
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!   Backword transform
!
        start = OMP_GET_WTIME()
        call norm_rtp_to_bwd_OMP_FFTW(ft1%nfld, ft1%ngrd, ft1%f_x(1,1), &
     &      WK_bwd%NFFT_c, WK_bwd%C_ROCmFFT(1,1))
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_backward_ROCmFFT(bwd,                              &
     &                            WK_bwd%Nfft_c, WK_bwd%C_ROCmFFT(1,1), &
     &                            WK_bwd%Nfft_r, WK_bwd%X_ROCmFFT(1,1), &
     &                            WK_bwd%Nbytes, WK_bwd%data_ptr)
        elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
!$omp target teams distribute parallel do collapse(2)
        do i = 1, ft1%ngrd
          do nd = 1, ft1%nfld
            ft1%f_x(nd,i) = WK_bwd%X_ROCmFFT(nd,i)
          end do
        end do
!$omp end target teams distribute parallel do
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
      end do
!
!   Finalize
      start = OMP_GET_WTIME()
      call calypso_pin_ROCmFFT_fin(fwd, WK_fwd, bwd, WK_bwd)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
  10  continue
      if(n_loop .eq. 1) call write_fft_test_data(file_name, ft1)
      call dealloc_fft_test_data(ft1)
!
      write(*, '("Time for ROCmfft:    ",1pE16.6e3)') elapsed(1)
      write(*, '("Time for Initialize: ",1pE16.6e3)') elapsed(3)
      write(*, '("Time for Data copy:  ",1pE16.6e3)') elapsed(2)
!
      stop 'finish'
      end program test_ROCmfft_rtp
