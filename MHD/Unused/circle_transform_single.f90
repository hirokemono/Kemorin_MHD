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
!!     &         (iflag_FFT, ifld, circle, leg_circ, WK_circle_fft)
!!      subroutine circle_transfer_scalar                               &
!!     &         (iflag_FFT, ifld, circle, leg_circ, WK_circle_fft)
!!      subroutine circle_transfer_sym_tensor                           &
!!     &         (iflag_FFT, ifld, circle, leg_circ, WK_circle_fft)
!!        type(fields_on_circle), intent(inout) :: circle
!!        type(circle_transform_spectr), intent(inout) :: leg_circ
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
     &        cdat%circle, cdat%leg_circ, cdat%WK_circle_fft)
        else if(cdat%d_circle%num_component(ifld) .eq. n_vector) then
          call circle_transfer_vector(iflag_FFT, icomp,                 &
     &        cdat%circle, cdat%leg_circ, cdat%WK_circle_fft)
        else
          call circle_transfer_scalar(iflag_FFT, icomp,                 &
     &        cdat%circle, cdat%leg_circ, cdat%WK_circle_fft)
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
     &         (iflag_FFT, ifld, circle, leg_circ, WK_circle_fft)
!
      use m_geometry_constants
      use cal_circle_transform
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: ifld
      type(fields_on_circle), intent(inout) :: circle
      type(circle_transform_spectr), intent(inout) :: leg_circ
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
      integer ::  j
!
      call circle_lag_transfer_vector                                   &
     &   (leg_circ%ltr_circle, leg_circ%jmax_circle,                    &
     &    leg_circ%P_circle, leg_circ%dPdt_circle,                      &
     &    leg_circ%ar_circle, leg_circ%ar2_circle,                      &
     &    leg_circ%jmax_circle, circle%d_rj_circle(0,ifld),             &
     &    leg_circ%vcirc_rtm)
!
!      write(61,*) 'j, leg_circ%vcirc_rtm(j,1:3)', ifld
!      do j = -leg_circ%ltr_circle, leg_circ%ltr_circle
!        write(61,*) j, leg_circ%vcirc_rtm(j,1:3)
!      end do
!
      if(circle%iflag_circle_coord .eq. iflag_circle_cyl) then
        call overwrt_circle_sph_vect_2_cyl(leg_circ%theta_circle,       &
     &      leg_circ%ltr_circle, leg_circ%vcirc_rtm)
      end if
!
      call cal_circle_spectrum_vector                                   &
     &   (ithree, leg_circ%ltr_circle, leg_circ%vcirc_rtm,              &
     &    circle%mphi_circle, circle%vrtm_mag(0,ifld),                  &
     &    circle%vrtm_phase(0,ifld))
      call copy_circle_spectrum_4_fft                                   &
     &   (ithree, leg_circ%ltr_circle, leg_circ%vcirc_rtm,              &
     &    circle%mphi_circle, circle%v_rtp_circle(1,1))
!
      call backward_FFT_select                                          &
     &   (iflag_FFT, np_smp, leg_circ%istack_circfft_smp, ione,         &
     &    circle%mphi_circle, circle%v_rtp_circle(1,1),                 &
     &    WK_circle_fft)
      call backward_FFT_select                                          &
     &   (iflag_FFT, np_smp, leg_circ%istack_circfft_smp, ione,         &
     &    circle%mphi_circle, circle%v_rtp_circle(1,2),                 &
     &    WK_circle_fft)
      call backward_FFT_select                                          &
     &   (iflag_FFT, np_smp, leg_circ%istack_circfft_smp, ione,         &
     &    circle%mphi_circle, circle%v_rtp_circle(1,3),                 &
     &    WK_circle_fft)
!
      end subroutine circle_transfer_vector
!
! ----------------------------------------------------------------------
!
      subroutine circle_transfer_scalar                                 &
     &         (iflag_FFT, ifld, circle, leg_circ, WK_circle_fft)
!
      use m_FFT_selector
      use cal_circle_transform
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: ifld
      type(fields_on_circle), intent(inout) :: circle
      type(circle_transform_spectr), intent(inout) :: leg_circ
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
      integer :: j
!
      call circle_lag_transfer_scalar                                   &
     &   (leg_circ%ltr_circle, leg_circ%jmax_circle,                    &
     &    leg_circ%P_circle, leg_circ%jmax_circle,                      &
     &    circle%d_rj_circle(0,ifld), leg_circ%vcirc_rtm)
!
!      write(61,*) 'j, leg_circ%vcirc_rtm(j,1:3)', ifld
!      do j = -leg_circ%ltr_circle, leg_circ%ltr_circle
!        write(61,*) j, leg_circ%vcirc_rtm(j,1)
!      end do
!
      call cal_circle_spectrum_vector                                   &
     &   (ione, leg_circ%ltr_circle, leg_circ%vcirc_rtm,                &
     &    circle%mphi_circle, circle%vrtm_mag(0,ifld),                  &
     &    circle%vrtm_phase(0,ifld))
      call copy_circle_spectrum_4_fft                                   &
     &   (ione, leg_circ%ltr_circle, leg_circ%vcirc_rtm,                &
     &    circle%mphi_circle, circle%v_rtp_circle(1,1))
!
      call backward_FFT_select                                          &
     &   (iflag_FFT, np_smp, leg_circ%istack_circfft_smp, ione,         &
     &    circle%mphi_circle, circle%v_rtp_circle(1,1),                 &
     &    WK_circle_fft)
!
      end subroutine circle_transfer_scalar
!
! ----------------------------------------------------------------------
!
      subroutine circle_transfer_sym_tensor                             &
     &         (iflag_FFT, ifld, circle, leg_circ, WK_circle_fft)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: ifld
      type(fields_on_circle), intent(inout) :: circle
      type(circle_transform_spectr), intent(inout) :: leg_circ
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
      integer(kind = kint) :: nd
!
!
      do nd = 0, n_sym_tensor-1
        call circle_transfer_scalar                                     &
     &     (iflag_FFT, ifld+nd, circle, leg_circ, WK_circle_fft)
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
      subroutine collect_spectr_for_circle                              &
     &         (jmax, nidx_global_rj, idx_gl_1d_rj_j, nnod_rj,          &
     &          num_phys_rj, ntot_phys_rj, istack_phys_comp_rj,         &
     &          phys_name_rj, d_rj, d_circle, circle)
!
      use calypso_mpi
      use calypso_mpi_real
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      integer(kind = kint), intent(in) :: nidx_global_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(jmax,3)
      integer(kind = kint), intent(in) :: num_phys_rj, ntot_phys_rj
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_phys_comp_rj(0:num_phys_rj)
      character (len=kchara), intent(in) :: phys_name_rj(num_phys_rj)
      real (kind=kreal), intent(in) :: d_rj(nnod_rj,ntot_phys_rj)
      type(phys_data), intent(in) :: d_circle
!
      type(fields_on_circle), intent(inout) :: circle
!
      integer(kind = kint) :: j, j_gl, i_in, i_ot, ncomp
      integer(kind = kint) :: ist_comp, jst_comp, nd, ifld, jfld
      integer(kind = kint_gl) :: num64
!
!
!    pickup spectrum for circle point
!
      do ifld = 1, d_circle%num_phys_viz
        ist_comp = d_circle%istack_component(ifld-1)
        do jfld = 1, num_phys_rj
          if(d_circle%phys_name(ifld) .eq. phys_name_rj(jfld)) then
            jst_comp = istack_phys_comp_rj(jfld-1)
            ncomp = istack_phys_comp_rj(jfld)                           &
     &             - istack_phys_comp_rj(jfld-1)
            if(iflag_debug .gt. 0) write(*,*)                           &
     &              trim(d_circle%phys_name(ifld)), ifld, jfld, ncomp
            do nd = 1, ncomp
              do j = 1, jmax
                j_gl = idx_gl_1d_rj_j(j,1)
                i_in = j + (circle%kr_gl_rcirc_in-1) *  jmax
                i_ot = j + (circle%kr_gl_rcirc_out-1) * jmax
!
                circle%d_rj_circ_lc(j_gl,ist_comp+nd)                   &
     &            = circle%coef_gl_rcirc_in * d_rj(i_in,jst_comp+nd)    &
     &             + circle%coef_gl_rcirc_out * d_rj(i_ot,jst_comp+nd)
              end do
            end do
            exit
          end if
        end do
      end do
!
!    collect data to rank 0
!
      num64 = d_circle%ntot_phys * (nidx_global_rj(2) + 1)
      if(my_rank .eq. 0) circle%d_rj_circle =   zero
      call calypso_mpi_reduce_real                                      &
     &   (circle%d_rj_circ_lc(0,1), circle%d_rj_circle(0,1), num64,     &
     &    MPI_SUM, 0)
!
      end subroutine collect_spectr_for_circle
!
! ----------------------------------------------------------------------
!
     end module circle_transform_single
