!>@file   fftw_access.f03
!!        module fftw_access
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Wrapper multi one dimentional FFT using FFTW
!!
!!@verbatim
!!      subroutine kemo_fftw_plan_dft_r2c_1d_f                          &
!!     &           (plan, nfft, X_FFTW, C_FFTW, iflag)
!!      subroutine kemo_fftw_plan_dft_c2r_1d_f                          &
!!     &         (plan, nfft, C_FFTW, X_FFTW, iflag)
!!        integer(kind = fftw_plan), target, intent(in) :: plan
!!        real(kind = kreal), target, intent(inout) :: X_FFTW(nfft)
!!        complex(kind = fftw_complex), target, intent(inout)           &
!!     &              :: C_FFTW(nfft/2+1)
!!      subroutine kemo_fftw_plan_many_dft_r2c_f                        &
!!     &           (plan, irank, nfft, ncomp,                           &
!!     &            X_FFTW,  inembed, istride, idist,                   &
!!     &            C_FFTW, onembed, ostride, odist, iflag)
!!      subroutine kemo_fftw_plan_many_dft_c2r_f                        &
!!     &         (plan, irank, nfft, ncomp,                             &
!!     &          C_FFTW, inembed, istride, idist,                      &
!!     &          X_FFTW, onembed, ostride, odist, iflag)
!!        integer(kind = fftw_plan), target, intent(in) :: plan
!!        real(kind = kreal), target, intent(inout) :: X_FFTW(nfft,ncomp)
!!        complex(kind = fftw_complex), target, intent(inout)           &
!!       &              :: C_FFTW(nfft/2+1,ncomp)
!!
!!      subroutine kemo_fftw_destroy_plan_f(plan)
!!      subroutine kemo_fftw_cleanup_f()
!!
!!      subroutine kemo_fftw_execute_f(plan)
!!      subroutine kemo_fftw_execute_dft_r2c_f                          &
!!     &         (plan, nfft, ncomp, X_FFTW, C_FFTW)
!!      subroutine kemo_fftw_execute_dft_c2r_f                          &
!!     &         (plan, nfft, ncomp, C_FFTW, X_FFTW)
!!        integer(kind = fftw_plan), target, intent(in) :: plan
!!        real(kind = kreal), target, intent(inout) :: X_FFTW(nfft,ncomp)
!!        complex(kind = fftw_complex), target, intent(inout)           &
!!       &              :: C_FFTW(nfft/2+1,ncomp)
!!@endverbatim
!
      module fftw_access
!
      use ISO_C_BINDING
      use m_precision
      use m_fftw_parameters
!
      implicit none
!
!  -----------------
!
      interface
!
!  -----------------
        subroutine kemo_fftw_plan_dft_r2c_1d                            &
     &           (plan, n_size, dble_in, cplx_out, iflag)               &
     &           BIND(C, name = 'kemo_fftw_plan_dft_r2c_1d')
          use ISO_C_BINDING
!
          type(C_ptr), value, intent(in) :: plan
          integer(C_int), intent(in) :: n_size
          type(C_ptr), value, intent(in) :: dble_in
          type(C_ptr), value, intent(in) :: cplx_out
          integer(C_int), intent(in) :: iflag
        end subroutine kemo_fftw_plan_dft_r2c_1d
!  -----------------
        subroutine kemo_fftw_plan_dft_c2r_1d                            &
     &           (plan, n_size, cplx_in, dble_out, iflag)               &
     &           BIND(C, name = 'kemo_fftw_plan_dft_c2r_1d')
          use ISO_C_BINDING
!
          type(C_ptr), value, intent(in) :: plan
          integer(C_int), intent(in) :: n_size
          type(C_ptr), value, intent(in) :: cplx_in
          type(C_ptr), value, intent(in) :: dble_out
          integer(C_int), intent(in) :: iflag
        end subroutine kemo_fftw_plan_dft_c2r_1d
!  -----------------
        subroutine kemo_fftw_plan_many_dft_r2c                          &
     &           (plan, irank, n_size, howmany,                         &
     &            dble_in,  inembed, istride, idist,                    &
     &            cplx_out, onembed, ostride, odist, iflag)             &
     &           BIND(C, name = 'kemo_fftw_plan_many_dft_r2c')
          use ISO_C_BINDING
!
          type(C_ptr), value, intent(in) :: plan
          integer(C_int), intent(in) :: irank, n_size, howmany
          type(C_ptr), value, intent(in) :: dble_in
          integer(C_int), intent(in) :: inembed, istride, idist
          type(C_ptr), value, intent(in) :: cplx_out
          integer(C_int), intent(in) :: onembed, ostride, odist
          integer(C_int), intent(in) :: iflag
        end subroutine kemo_fftw_plan_many_dft_r2c
!  -----------------
        subroutine kemo_fftw_plan_many_dft_c2r                          &
     &           (plan, irank, n_size, howmany,                         &
     &            cplx_in,  inembed, istride, idist,                    &
     &            dble_out, onembed, ostride, odist, iflag)             &
     &           BIND(C, name = 'kemo_fftw_plan_many_dft_c2r')
          use ISO_C_BINDING
!
          type(C_ptr), value, intent(in) :: plan
          integer(C_int), intent(in) :: irank, n_size, howmany
          type(C_ptr), value, intent(in) :: cplx_in
          integer(C_int), intent(in) :: inembed, istride, idist
          type(C_ptr), value, intent(in) :: dble_out
          integer(C_int), intent(in) :: onembed, ostride, odist
          integer(C_int), intent(in) :: iflag
        end subroutine kemo_fftw_plan_many_dft_c2r
!  -----------------
!
        subroutine kemo_fftw_destroy_plan(plan)                         &
     &           BIND(C, name = 'kemo_fftw_destroy_plan')
          use ISO_C_BINDING
!
          type(C_ptr), value, intent(in) :: plan
        end subroutine kemo_fftw_destroy_plan
!  -----------------
        subroutine kemo_fftw_cleanup()                                  &
     &           BIND(C, name = 'kemo_fftw_cleanup')
          use ISO_C_BINDING
        end subroutine kemo_fftw_cleanup
!
!  -----------------
!
        subroutine kemo_fftw_execute(plan)                              &
     &           BIND(C, name = 'kemo_fftw_execute')
          use ISO_C_BINDING
!
          type(C_ptr), value, intent(in) :: plan
        end subroutine kemo_fftw_execute
!  -----------------
        subroutine kemo_fftw_execute_dft_r2c(plan, dble_in, cplx_out)   &
     &           BIND(C, name = 'kemo_fftw_execute_dft_r2c')
          use ISO_C_BINDING
!
          type(C_ptr), value, intent(in) :: plan
          type(C_ptr), value, intent(in) :: dble_in
          type(C_ptr), value, intent(in) :: cplx_out
        end subroutine kemo_fftw_execute_dft_r2c
!  -----------------
        subroutine kemo_fftw_execute_dft_c2r(plan, cplx_in, dble_out)   &
     &           BIND(C, name = 'kemo_fftw_execute_dft_c2r')
          use ISO_C_BINDING
!
          type(C_ptr), value, intent(in) :: plan
          type(C_ptr), value, intent(in) :: cplx_in
          type(C_ptr), value, intent(in) :: dble_out
        end subroutine kemo_fftw_execute_dft_c2r
!  -----------------
      end interface
!
!  ---------------------------------------------------------------------
!
      contains
!
!
!  ---------------------------------------------------------------------
!
      subroutine kemo_fftw_plan_dft_r2c_1d_f                            &
     &           (plan, nfft, X_FFTW, C_FFTW, iflag)
!
      integer(kind = fftw_plan), target, intent(in) :: plan
      integer(kind = kint), intent(in) :: nfft
      real(kind = kreal), target, intent(inout) :: X_FFTW(nfft)
      complex(kind = fftw_complex), target, intent(inout)               &
     &              :: C_FFTW(nfft/2+1)
!
      integer, intent(in) :: iflag
!
      integer(C_int) :: iflag_p, n_size_p
      integer(kind = fftw_plan), pointer :: plan_p
      real(kind = kreal), pointer :: dble_in(:)
      complex(kind = fftw_complex), pointer :: cplx_out(:)
!
      n_size_p =  int(nfft, KIND(n_size_p))
      iflag_p =   int(iflag,  KIND(iflag_p))
      plan_p =>    plan
      dble_in =>  X_FFTW
      cplx_out => C_FFTW
      call kemo_fftw_plan_dft_r2c_1d(C_LOC(plan_p), n_size_p,           &
     &    C_LOC(dble_in), C_LOC(cplx_out), iflag_p)
      nullify(plan_p, dble_in, cplx_out)
!
      end subroutine kemo_fftw_plan_dft_r2c_1d_f
!
!  ---------------------------------------------------------------------
!
      subroutine kemo_fftw_plan_dft_c2r_1d_f                            &
     &         (plan, nfft, C_FFTW, X_FFTW, iflag)
!
      integer(kind = fftw_plan), target, intent(in) :: plan
      integer(kind = kint), intent(in) :: nfft
      real(kind = kreal), target, intent(inout) :: X_FFTW(nfft)
      complex(kind = fftw_complex), target, intent(inout)               &
     &              :: C_FFTW(nfft/2+1)
!
      integer, intent(in) :: iflag
!
      integer(C_int) :: iflag_p, n_size_p
      integer(kind = fftw_plan), pointer :: plan_p
      complex(kind = fftw_complex), pointer :: cplx_in(:)
      real(kind = kreal), pointer :: dble_out(:)
!
      n_size_p =  int(nfft, KIND(n_size_p))
      iflag_p =   int(iflag,  KIND(iflag_p))
      plan_p =>   plan
      dble_out => X_FFTW
      cplx_in =>  C_FFTW
      call kemo_fftw_plan_dft_c2r_1d(C_LOC(plan_p), n_size_p,           &
     &    C_LOC(cplx_in), C_LOC(dble_out), iflag_p)
      nullify(plan_p, cplx_in, dble_out)
!
      end subroutine kemo_fftw_plan_dft_c2r_1d_f
!
!  ---------------------------------------------------------------------
!
      subroutine kemo_fftw_plan_many_dft_r2c_f                          &
     &           (plan, irank, nfft, ncomp,                             &
     &            X_FFTW,  inembed, istride, idist,                     &
     &            C_FFTW, onembed, ostride, odist, iflag)
!
      integer(kind = fftw_plan), target, intent(in) :: plan
      integer(kind = kint), intent(in) :: nfft, ncomp
      real(kind = kreal), target, intent(inout) :: X_FFTW(nfft,ncomp)
      complex(kind = fftw_complex), target, intent(inout)               &
     &              :: C_FFTW(nfft/2+1,ncomp)
!
      integer, intent(in) :: irank
      integer, intent(in) :: inembed, istride, idist
      integer, intent(in) :: onembed, ostride, odist
      integer, intent(in) :: iflag
!
      integer(C_int) :: irank_p, n_size_p, howmany_p
      integer(C_int) :: inembed_p, istride_p, idist_p
      integer(C_int) :: onembed_p, ostride_p, odist_p
      integer(C_int) :: iflag_p
!
      integer(kind = fftw_plan), pointer :: plan_p
      real(kind = kreal), pointer :: dble2_in(:,:)
      complex(kind = fftw_complex), pointer :: cplx2_out(:,:)
!
      irank_p =   int(irank,  KIND(irank_p))
      n_size_p =  int(nfft, KIND(n_size_p))
      howmany_p = int(ncomp,KIND(howmany_p))
      inembed_p = int(inembed,KIND(inembed_p))
      istride_p = int(istride,KIND(istride_p))
      idist_p =   int(idist,  KIND(idist_p))
      onembed_p = int(onembed,KIND(onembed_p))
      ostride_p = int(ostride,KIND(ostride_p))
      odist_p =   int(odist,  KIND(odist_p))
      iflag_p =   int(iflag,  KIND(iflag_p))
!
      plan_p =>    plan
      dble2_in =>  X_FFTW
      cplx2_out => C_FFTW
!
      call kemo_fftw_plan_many_dft_r2c                                  &
     &   (C_LOC(plan_p), irank_p, n_size_p, howmany_p,                  &
     &    C_LOC(dble2_in),  inembed_p, istride_p, idist_p,              &
     &    C_LOC(cplx2_out), onembed_p, ostride_p, odist_p, iflag_p)
      nullify(plan_p, dble2_in, cplx2_out)
!
        end subroutine kemo_fftw_plan_many_dft_r2c_f
!
!  ---------------------------------------------------------------------
!
      subroutine kemo_fftw_plan_many_dft_c2r_f                          &
     &         (plan, irank, nfft, ncomp,                               &
     &          C_FFTW, inembed, istride, idist,                        &
     &          X_FFTW, onembed, ostride, odist, iflag)
          use ISO_C_BINDING
!
      integer(kind = fftw_plan), target, intent(in) :: plan
      integer(kind = kint), intent(in) :: nfft, ncomp
      complex(kind = fftw_complex), target, intent(inout)               &
     &              :: C_FFTW(nfft/2+1,ncomp)
      real(kind = kreal), target, intent(inout) :: X_FFTW(nfft,ncomp)
!
      integer, intent(in) :: irank
      integer, intent(in) :: inembed, istride, idist
      integer, intent(in) :: onembed, ostride, odist
      integer, intent(in) :: iflag
!
      integer(C_int) :: irank_p, n_size_p, howmany_p
      integer(C_int) :: inembed_p, istride_p, idist_p
      integer(C_int) :: onembed_p, ostride_p, odist_p
      integer(C_int) :: iflag_p
!
      integer(kind = fftw_plan), pointer :: plan_p
      complex(kind = fftw_complex), pointer :: cplx2_in(:,:)
      real(kind = kreal), pointer :: dble2_out(:,:)
!
      irank_p =   int(irank,  KIND(irank_p))
      n_size_p =  int(nfft, KIND(n_size_p))
      howmany_p = int(ncomp,KIND(howmany_p))
      inembed_p = int(inembed,KIND(inembed_p))
      istride_p = int(istride,KIND(istride_p))
      idist_p =   int(idist,  KIND(idist_p))
      onembed_p = int(onembed,KIND(onembed_p))
      ostride_p = int(ostride,KIND(ostride_p))
      odist_p =   int(odist,  KIND(odist_p))
      iflag_p =   int(iflag,  KIND(iflag_p))
!
      plan_p =>    plan
      dble2_out => X_FFTW
      cplx2_in =>  C_FFTW
!
      call kemo_fftw_plan_many_dft_c2r                                  &
     &   (C_LOC(plan_p), irank_p, n_size_p, howmany_p,                  &
     &    C_LOC(cplx2_in),  inembed_p, istride_p, idist_p,              &
     &    C_LOC(dble2_out), onembed_p, ostride_p, odist_p, iflag_p)
      nullify(plan_p, cplx2_in, dble2_out)
!
        end subroutine kemo_fftw_plan_many_dft_c2r_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine kemo_fftw_destroy_plan_f(plan)
!
      integer(kind = fftw_plan), target, intent(inout) :: plan
      integer(kind = fftw_plan), pointer :: plan_p
!
      plan_p =>    plan
      call kemo_fftw_destroy_plan(C_LOC(plan_p))
      nullify(plan_p)
!
      end subroutine kemo_fftw_destroy_plan_f
!
!  ---------------------------------------------------------------------
!
      subroutine kemo_fftw_cleanup_f()
!
      call kemo_fftw_cleanup()
!
      end subroutine kemo_fftw_cleanup_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine kemo_fftw_execute_f(plan)
!
      integer(kind = fftw_plan), target, intent(inout) :: plan
      integer(kind = fftw_plan), pointer :: plan_p
!
      plan_p =>    plan
      call kemo_fftw_execute(C_LOC(plan_p))
      nullify(plan_p)
!
      end subroutine kemo_fftw_execute_f
!
!  ---------------------------------------------------------------------
!
      subroutine kemo_fftw_execute_dft_r2c_f                            &
     &         (plan, nfft, ncomp, X_FFTW, C_FFTW)
!
      integer(kind = fftw_plan), target, intent(in) :: plan
      integer(kind = kint), intent(in) :: nfft, ncomp
      real(kind = kreal), target, intent(inout) :: X_FFTW(nfft,ncomp)
      complex(kind = fftw_complex), target, intent(inout)               &
     &              :: C_FFTW(nfft/2+1,ncomp)
!
      integer(kind = fftw_plan), pointer :: plan_p
      real(kind = kreal), pointer :: dble2_in(:,:)
      complex(kind = fftw_complex), pointer :: cplx2_out(:,:)
!
      plan_p =>    plan
      dble2_in =>  X_FFTW
      cplx2_out => C_FFTW
      call kemo_fftw_execute_dft_r2c                                    &
     &   (C_LOC(plan_p), C_LOC(dble2_in), C_LOC(cplx2_out))
      nullify(plan_p, dble2_in, cplx2_out)
!
      end subroutine kemo_fftw_execute_dft_r2c_f
!
!  ---------------------------------------------------------------------
!
      subroutine kemo_fftw_execute_dft_c2r_f                            &
     &         (plan, nfft, ncomp, C_FFTW, X_FFTW)
!
      integer(kind = fftw_plan), target, intent(in) :: plan
      integer(kind = kint), intent(in) :: nfft, ncomp
      real(kind = kreal), target, intent(inout) :: X_FFTW(nfft,ncomp)
      complex(kind = fftw_complex), target, intent(inout)               &
     &              :: C_FFTW(nfft/2+1,ncomp)
!
      integer(kind = fftw_plan), pointer :: plan_p
      complex(kind = fftw_complex), pointer :: cplx2_in(:,:)
      real(kind = kreal), pointer :: dble2_out(:,:)
!
      plan_p =>    plan
      dble2_out => X_FFTW
      cplx2_in =>  C_FFTW
      call kemo_fftw_execute_dft_c2r                                    &
     &   (C_LOC(plan_p), C_LOC(cplx2_in), C_LOC(dble2_out))
      nullify(plan_p, cplx2_in, dble2_out)
!
      end subroutine kemo_fftw_execute_dft_c2r_f
!
!  ---------------------------------------------------------------------
!
      end module fftw_access
