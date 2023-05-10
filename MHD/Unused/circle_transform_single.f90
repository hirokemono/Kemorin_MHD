!>@file   circle_transform_single.f90
!!@brief  module circle_transform_single
!!
!!@author H. Matsui
!!@date Programmed on June., 2013
!
!>@brief  spherical transform at a specific circle at @f$(r, theta)@f$
!!
!!@verbatim
!!      subroutine sph_transfer_on_circle                               &
!!     &         (iflag_FFT, sph_rj, rj_fld, cdat)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(circle_fld_maker), intent(inout) :: cdat
!!      subroutine circle_transfer_vector                               &
!!     &         (iflag_FFT, ifld, circle, circ_spec, WK_circle_fft)
!!      subroutine circle_transfer_scalar                               &
!!     &         (iflag_FFT, ifld, circle, circ_spec, WK_circle_fft)
!!      subroutine circle_transfer_sym_tensor                           &
!!     &         (iflag_FFT, ifld, circle, circ_spec, WK_circle_fft)
!!        type(fields_on_circle), intent(inout) :: circle
!!        type(circle_transform_spetr), intent(inout) :: circ_spec
!!        type(working_FFTs), intent(inout) :: WK_circle_fft
!!      subroutine circle_lag_transfer_scalar(ltr_circle, jmax_circle,  &
!!     &          P_circle, jmax, d_rj_circle, vcirc_rtm)
!!      subroutine circle_lag_transfer_vector(ltr_circle, jmax_circle,  &
!!     &          P_circle, dPdt_circle, ar_circle, ar2_circle,         &
!!     &          jmax, d_rj_circle, vcirc_rtm)
!!@endverbatim
!!
!
      module circle_transform_single
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_FFT_selector
      use t_schmidt_polynomial
      use t_fields_on_circle
      use t_circle_transform
!
      implicit none
!
!
      type(legendre_polynomials), save :: leg_c
!
      private :: leg_c
      private :: alloc_circle_transform
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sph_transfer_on_circle                                 &
     &         (iflag_FFT, sph_rj, rj_fld, cdat)
!
      use calypso_mpi
      use m_phys_constants
!
      use t_spheric_rj_data
      use t_phys_data
!
      use circle_transform_single
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(circle_fld_maker), intent(inout) :: cdat
!
      integer(kind = kint) :: ifld, icomp, m, nd
!
!
      call collect_spectr_for_circle(sph_rj%nidx_rj(2),                 &
     &    sph_rj%nidx_global_rj, sph_rj%idx_gl_1d_rj_j,                 &
     &    rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,            &
     &    rj_fld%istack_component, rj_fld%phys_name, rj_fld%d_fld,      &
     &    cdat%d_circle, cdat%circle)
!
!    spherical transfer
!
      if(my_rank .gt. 0) return
!
      do ifld = 1, cdat%d_circle%num_phys_viz
        icomp =  cdat%d_circle%istack_component(ifld-1) + 1
        if(cdat%d_circle%num_component(ifld) .eq. n_sym_tensor) then
          call circle_transfer_sym_tensor(iflag_FFT, icomp,             &
     &        cdat%circle, cdat%circ_spec, cdat%WK_circle_fft)
        else if(cdat%d_circle%num_component(ifld) .eq. n_vector) then
          call circle_transfer_vector(iflag_FFT, icomp,                 &
     &        cdat%circle, cdat%circ_spec, cdat%WK_circle_fft)
        else
          call circle_transfer_scalar(iflag_FFT, icomp,                 &
     &        cdat%circle, cdat%circ_spec, cdat%WK_circle_fft)
        end if
!
        do nd = 1, cdat%d_circle%num_component(ifld)
          do m = 1, cdat%circle%mphi_circle
            cdat%d_circle%d_fld(m,icomp+nd-1)                           &
     &         = cdat%circle%v_rtp_circle(m,nd)
          end do
        end do
      end do
!
      end subroutine sph_transfer_on_circle
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine circle_transfer_vector                                 &
     &         (iflag_FFT, ifld, circle, circ_spec, WK_circle_fft)
!
      use m_geometry_constants
      use cal_circle_transform
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: ifld
      type(fields_on_circle), intent(inout) :: circle
      type(circle_transform_spetr), intent(inout) :: circ_spec
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
      integer ::  j
!
      call circle_lag_transfer_vector                                   &
     &   (circ_spec%ltr_circle, circ_spec%jmax_circle,                  &
     &    circ_spec%P_circle, circ_spec%dPdt_circle,                    &
     &    circ_spec%ar_circle, circ_spec%ar2_circle,                    &
     &    circ_spec%jmax_circle, circle%d_rj_circle(0,ifld),            &
     &    circ_spec%vcirc_rtm)
!
!      write(61,*) 'j, circ_spec%vcirc_rtm(j,1:3)', ifld
!      do j = -circ_spec%ltr_circle, circ_spec%ltr_circle
!        write(61,*) j, circ_spec%vcirc_rtm(j,1:3)
!      end do
!
      if(circle%iflag_circle_coord .eq. iflag_circle_cyl) then
        call overwrt_circle_sph_vect_2_cyl(circ_spec%theta_circle,      &
     &      circ_spec%ltr_circle, circ_spec%vcirc_rtm)
      end if
!
      call cal_circle_spectrum_vector                                   &
     &   (ithree, circ_spec%ltr_circle, circ_spec%vcirc_rtm,            &
     &    circle%mphi_circle, circle%vrtm_mag(0,ifld),                  &
     &    circle%vrtm_phase(0,ifld))
      call copy_circle_spectrum_4_fft                                   &
     &   (ithree, circ_spec%ltr_circle, circ_spec%vcirc_rtm,            &
     &    circle%mphi_circle, circle%v_rtp_circle(1,1))
!
      call backward_FFT_select                                          &
     &   (iflag_FFT, np_smp, circ_spec%istack_circfft_smp, ione,        &
     &    circle%mphi_circle, circle%v_rtp_circle(1,1),                 &
     &    WK_circle_fft)
      call backward_FFT_select                                          &
     &   (iflag_FFT, np_smp, circ_spec%istack_circfft_smp, ione,        &
     &    circle%mphi_circle, circle%v_rtp_circle(1,2),                 &
     &    WK_circle_fft)
      call backward_FFT_select                                          &
     &   (iflag_FFT, np_smp, circ_spec%istack_circfft_smp, ione,        &
     &    circle%mphi_circle, circle%v_rtp_circle(1,3),                 &
     &    WK_circle_fft)
!
      end subroutine circle_transfer_vector
!
! ----------------------------------------------------------------------
!
      subroutine circle_transfer_scalar                                 &
     &         (iflag_FFT, ifld, circle, circ_spec, WK_circle_fft)
!
      use m_FFT_selector
      use cal_circle_transform
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: ifld
      type(fields_on_circle), intent(inout) :: circle
      type(circle_transform_spetr), intent(inout) :: circ_spec
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
      integer :: j
!
      call circle_lag_transfer_scalar                                   &
     &   (circ_spec%ltr_circle, circ_spec%jmax_circle,                  &
     &    circ_spec%P_circle, circ_spec%jmax_circle,                    &
     &    circle%d_rj_circle(0,ifld), circ_spec%vcirc_rtm)
!
!      write(61,*) 'j, circ_spec%vcirc_rtm(j,1:3)', ifld
!      do j = -circ_spec%ltr_circle, circ_spec%ltr_circle
!        write(61,*) j, circ_spec%vcirc_rtm(j,1)
!      end do
!
      call cal_circle_spectrum_vector                                   &
     &   (ione, circ_spec%ltr_circle, circ_spec%vcirc_rtm,              &
     &    circle%mphi_circle, circle%vrtm_mag(0,ifld),                  &
     &    circle%vrtm_phase(0,ifld))
      call copy_circle_spectrum_4_fft                                   &
     &   (ione, circ_spec%ltr_circle, circ_spec%vcirc_rtm,              &
     &    circle%mphi_circle, circle%v_rtp_circle(1,1))
!
      call backward_FFT_select                                          &
     &   (iflag_FFT, np_smp, circ_spec%istack_circfft_smp, ione,        &
     &    circle%mphi_circle, circle%v_rtp_circle(1,1),                 &
     &    WK_circle_fft)
!
      end subroutine circle_transfer_scalar
!
! ----------------------------------------------------------------------
!
      subroutine circle_transfer_sym_tensor                             &
     &         (iflag_FFT, ifld, circle, circ_spec, WK_circle_fft)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: ifld
      type(fields_on_circle), intent(inout) :: circle
      type(circle_transform_spetr), intent(inout) :: circ_spec
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
      integer(kind = kint) :: nd
!
!
      do nd = 0, n_sym_tensor-1
        call circle_transfer_scalar                                     &
     &     (iflag_FFT, ifld+nd, circle, circ_spec, WK_circle_fft)
      end do
!
      end subroutine circle_transfer_sym_tensor
!
! ----------------------------------------------------------------------
!
      subroutine circle_lag_transfer_scalar(ltr_circle, jmax_circle,    &
     &          P_circle, jmax, d_rj_circle, vcirc_rtm)
!
      integer(kind = kint), intent(in) :: ltr_circle
      integer(kind = kint), intent(in) :: jmax_circle
      real(kind = kreal), intent(in) :: P_circle(0:jmax_circle)
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: d_rj_circle(0:jmax)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: vcirc_rtm(-ltr_circle:ltr_circle,3)
!
      integer(kind = kint) :: l, m, j
!
!
      vcirc_rtm = 0.0d0
      vcirc_rtm(0,1) = d_rj_circle(0)
      do l = 1, ltr_circle
        do m = -l, l
          j = l*(l+1) + m
          vcirc_rtm(m,1) = vcirc_rtm(m,1) + d_rj_circle(j)              &
     &                    * P_circle(j)
        end do
      end do
!
      end subroutine circle_lag_transfer_scalar
!
! ----------------------------------------------------------------------
!
      subroutine circle_lag_transfer_vector(ltr_circle, jmax_circle,    &
     &          P_circle, dPdt_circle, ar_circle, ar2_circle,           &
     &          jmax, d_rj_circle, vcirc_rtm)
!
      integer(kind = kint), intent(in) :: ltr_circle
      integer(kind = kint), intent(in) :: jmax_circle
      real(kind = kreal), intent(in) :: ar_circle, ar2_circle
      real(kind = kreal), intent(in) :: P_circle(0:jmax_circle)
      real(kind = kreal), intent(in) :: dPdt_circle(0:jmax_circle)
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: d_rj_circle(0:jmax,3)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: vcirc_rtm(-ltr_circle:ltr_circle,3)
!
      integer(kind = kint) :: l, m, j
!
!
      vcirc_rtm = 0.0d0
      do l = 1, ltr_circle
        do m = -l, l
          j = l*(l+1) + m
          vcirc_rtm(m,1) = vcirc_rtm(m,1) + d_rj_circle(j,1)            &
     &                  * P_circle(j) * dble(l)*dble(l+1)
          vcirc_rtm(m,2) = vcirc_rtm(m,2) + d_rj_circle(j,2)            &
     &                  * dPdt_circle(j)
          vcirc_rtm(m,3) = vcirc_rtm(m,3) - d_rj_circle(j,3)            &
     &                  * dPdt_circle(j)
        end do
!
        do m = -l, l
          j = l*(l+1) + m
          vcirc_rtm(-m,2) = vcirc_rtm(-m,2) + d_rj_circle(j,3)          &
     &                   * P_circle(j) * dble(-m)
!
          vcirc_rtm(-m,3) = vcirc_rtm(-m,3) + d_rj_circle(j,2)          &
     &                   * P_circle(j) * dble(-m)
        end do
      end do
!
      do m = -ltr_circle, ltr_circle
        vcirc_rtm(m,1) = vcirc_rtm(m,1) * ar2_circle
        vcirc_rtm(m,2) = vcirc_rtm(m,2) * ar_circle
        vcirc_rtm(m,3) = vcirc_rtm(m,3) * ar_circle
      end do
!
      end subroutine circle_lag_transfer_vector
!
! ----------------------------------------------------------------------
!
     end module circle_transform_single
