!>@file   circle_bwd_transfer_rj.f90
!!@brief  module circle_bwd_transfer_rj
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform considering symmetry
!!
!!@verbatim
!!      subroutine s_circle_bwd_transfer_rj                             &
!!     &         (iflag_FFT, colat, sph_rj, ntot_comp, d_rj, circle,    &
!!     &          circ_spec, P_circ, dPdt_circ, d_circle, istack_rj_cmp,&
!!     &          WK_circle_fft)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        real(kind = kreal), intent(in) :: colat
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj,ntot_comp)
!!        real(kind = kreal), intent(in) :: P_circ(sph_rj%nidx_rj(2))
!!        real(kind = kreal), intent(in) :: dPdt_circ(sph_rj%nidx_rj(2))
!!        type(fields_on_circle), intent(inout) :: circle
!!        type(phys_data), intent(inout) :: d_circle
!!        integer(kind = kint), intent(in)                                  &
!!       &                     :: istack_rj_cmp(0:d_circle%num_phys)
!!        type(circle_transform_spetr), intent(inout) :: circ_spec
!!        type(working_FFTs), intent(inout) :: WK_circle_fft
!!@endverbatim
      module circle_bwd_transfer_rj
!
      use m_precision
      use m_machine_parameter
      use m_constants
!
      use calypso_mpi
!
      use t_spheric_rj_data
      use t_phys_data
      use t_fields_on_circle
      use t_FFT_selector
      use t_field_on_circle
!
      implicit none
!
      private :: circle_bwd_leg_trans_sym_tensor
      private :: circle_bwd_leg_trans_vector
      private :: circle_bwd_leg_trans_scalar
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_circle_bwd_transfer_rj                               &
     &         (iflag_FFT, colat, sph_rj, ntot_comp, rj_fld, circle,      &
     &          circ_spec, P_circ, dPdt_circ, d_circle, istack_rj_cmp,  &
     &          WK_circle_fft)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: iflag_FFT
      real(kind = kreal), intent(in) :: colat
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: ntot_comp
      type(phys_data), intent(in) :: rj_fld
      real(kind = kreal), intent(in) :: P_circ(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(in) :: dPdt_circ(sph_rj%nidx_rj(2))
      type(fields_on_circle), intent(inout) :: circle
      type(phys_data), intent(inout) :: d_circle
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_rj_cmp(0:d_circle%num_phys)
      type(circle_transform_spetr), intent(inout) :: circ_spec
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
      integer(kind = kint_gl) :: num64
      integer(kind = kint) :: ifld, icomp_c, icomp_rj, nd
!
      do ifld = 1, d_circle%num_phys
        icomp_rj = 1 + istack_rj_cmp(ifld-1)
        icomp_c =  1 + d_circle%istack_component(ifld-1)
        if(d_circle%num_component(ifld) .eq. n_sym_tensor) then
!          call circle_bwd_leg_trans_sym_tensor(colat, sph_rj,          &
!     &       circle, circ_spec, P_circ, dPdt_circ, rj_fld%d_fld(1,icomp_rj),   &
!     &        circ_spec%vcirc_lc(-circ_spec%ltr_circle,icomp_c))
          do nd = 0, 5
            call circle_bwd_leg_trans_scalar                            &
     &         (sph_rj, circle, circ_spec, P_circ, rj_fld%d_fld(1,icomp_rj),    &
     &          circ_spec%vcirc_lc(-circ_spec%ltr_circle,icomp_c+nd))
          end do
        else if(d_circle%num_component(ifld) .eq. n_vector) then
          call circle_bwd_leg_trans_vector(colat, sph_rj,               &
     &        circle, circ_spec, P_circ, dPdt_circ, rj_fld%d_fld(1,icomp_rj),   &
     &        circ_spec%vcirc_lc(-circ_spec%ltr_circle,icomp_c))
        else
          call circle_bwd_leg_trans_scalar                              &
     &       (sph_rj, circle, circ_spec, P_circ, rj_fld%d_fld(1,icomp_rj),      &
     &        circ_spec%vcirc_lc(-circ_spec%ltr_circle,icomp_c))
        end if
      end do
!
      num64 = cast_long(ntot_comp * (2*circ_spec%ltr_circle+1))
      call calypso_mpi_reduce_real                                      &
     &   (circ_spec%vcirc_lc(-circ_spec%ltr_circle,1),                  &
     &    circ_spec%vcirc_rtm(-circ_spec%ltr_circle,1),                 &
     &    num64, MPI_SUM, 0)
!
      call cal_circle_spectrum_vector                                   &
     &   (d_circle%ntot_phys, circ_spec%ltr_circle, d_circle,           &
     &    circle%mphi_circle, circle%vrtm_mag, circle%vrtm_phase)
      call copy_circle_spectrum_4_fft                                   &
     &  (d_circle%ntot_phys, circ_spec%ltr_circle, circ_spec%vcirc_rtm, &
     &   circle%mphi_circle, circle%v_rtp_circle(1,1))
!
      call backward_FFT_select(iflag_FFT, np_smp,                       &
     &    circ_spec%istack_circfft_smp, d_circle%ntot_phys,             &
     &    circle%mphi_circle, circle%v_rtp_circle(1,1),                 &
     &    WK_circle_fft)
!
      end subroutine s_circle_bwd_transfer_rj
!
! ----------------------------------------------------------------------
!
      subroutine circle_bwd_leg_trans_scalar                            &
     &         (sph_rj, circle, circ_spec, P_circ, d_rj, scl_circ)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fields_on_circle), intent(in) :: circle
      type(circle_transform_spetr), intent(in) :: circ_spec
      real(kind = kreal), intent(in) :: P_circ(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &        :: scl_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle)
!
      real(kind = kreal) :: c_in, c_out
      real(kind = kreal) :: d_mid
      integer(kind = kint) :: i_in, i_out, j, m
!
      c_in =  circle%coef_gl_rcirc_in
      c_out = circle%coef_gl_rcirc_out
      do j = 1, sph_rj%nidx_rj(2)
        i_in =  1 + (circle%kr_gl_rcirc_in-1 ) * sph_rj%istep_rj(1)     &
     &            + (j-1) * sph_rj%istep_rj(2)
        i_out = 1 + (circle%kr_gl_rcirc_out-1) * sph_rj%istep_rj(1)     &
     &            + (j-1) * sph_rj%istep_rj(2)
        m = sph_rj%idx_gl_1d_rj_j(j,3)
        d_mid = (c_in*d_rj(i_in) + c_out*d_rj(i_out)) 
        scl_circ(m) = scl_circ(m) + P_circ(j) * d_mid
      end do
!
      end subroutine circle_bwd_leg_trans_scalar
!
! ----------------------------------------------------------------------
!
      subroutine circle_bwd_leg_trans_vector(colat, sph_rj,             &
     &          circle, circ_spec, P_circ, dPdt_circ, d_rj, vec_circ)
!
      real(kind = kreal), intent(in) :: colat
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fields_on_circle), intent(in) :: circle
      type(circle_transform_spetr), intent(in) :: circ_spec
      real(kind = kreal), intent(in) :: P_circ(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(in) :: dPdt_circ(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj,3)
!
      real(kind = kreal), intent(inout)                                 &
     &        :: vec_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,3)
!
      real(kind = kreal) :: ar1, ar2, c_in, c_out
      real(kind = kreal) :: g3, asin_t, dydp_circ
      real(kind = kreal) :: d_mid(3)
      integer(kind = kint) :: i_in, i_out, j, l, m
!
      ar1 = circ_spec%ar_circle
      ar2 = circ_spec%ar2_circle
      c_in =  circle%coef_gl_rcirc_in
      c_out = circle%coef_gl_rcirc_out
      asin_t = one / sin(colat)
      do j = 1, sph_rj%nidx_rj(2)
        i_in =  1 + (circle%kr_gl_rcirc_in-1 ) * sph_rj%istep_rj(1)     &
     &            + (j-1) * sph_rj%istep_rj(2)
        i_out = 1 + (circle%kr_gl_rcirc_out-1) * sph_rj%istep_rj(1)     &
     &            + (j-1) * sph_rj%istep_rj(2)
        l = sph_rj%idx_gl_1d_rj_j(j,2)
        m = sph_rj%idx_gl_1d_rj_j(j,3)
        g3 = dble(l * (l+1))
        dydp_circ = - dble(m) * asin_t * P_circ(j)
        d_mid(1) = (c_in*d_rj(i_in,1) + c_out*d_rj(i_out,1))
        d_mid(2) = (c_in*d_rj(i_in,2) + c_out*d_rj(i_out,2))
        d_mid(3) = (c_in*d_rj(i_in,3) + c_out*d_rj(i_out,3))
!
        vec_circ( m,1) = vec_circ( m,1) + g3 * P_circ(j) * d_mid(1)
!
        vec_circ( m,2) = vec_circ( m,2) + dPdt_circ(j) * d_mid(2)
        vec_circ(-m,3) = vec_circ(-m,3) + dydp_circ *    d_mid(3)
!
        vec_circ(-m,2) = vec_circ(-m,2) + dydp_circ *    d_mid(2)
        vec_circ( m,3) = vec_circ( m,3) - dPdt_circ(j) * d_mid(3)
      end do
!
!$mop parallel workshare
      vec_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,1)            &
     &  = vec_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,1)        &
     &   * circ_spec%ar2_circle
      vec_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,2)            &
     &  = vec_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,1)        &
     &   * circ_spec%ar_circle
      vec_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,3)            &
     &  = vec_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,1)        &
     &   * circ_spec%ar_circle
!$mop end parallel workshare
!
      end subroutine circle_bwd_leg_trans_vector
!
! ----------------------------------------------------------------------
!
      subroutine circle_bwd_leg_trans_sym_tensor(colat, sph_rj,         &
     &          circle, circ_spec, P_circ, dPdt_circ, d_rj, tsr_circ)
!
      real(kind = kreal), intent(in) :: colat
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fields_on_circle), intent(in) :: circle
      type(circle_transform_spetr), intent(in) :: circ_spec
      real(kind = kreal), intent(in) :: P_circ(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(in) :: dPdt_circ(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj,6)
!
      real(kind = kreal), intent(inout)                                 &
     &        :: tsr_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,6)
!
      real(kind = kreal) :: ar1, ar2, c_in, c_out
      real(kind = kreal) :: g3, asin_t, cos_t, dydp_circ
      real(kind = kreal) :: atdydt_circ, d2ydp2_circ, d2ydtdp_circ
      real(kind = kreal) :: d_mid(6)
      integer(kind = kint) :: i_in, i_out, j, l, m
!
      ar1 = circ_spec%ar_circle
      ar2 = circ_spec%ar2_circle
      c_in =  circle%coef_gl_rcirc_in
      c_out = circle%coef_gl_rcirc_out
      asin_t = one / sin(colat)
      cos_t =  cos(colat)
      do j = 1, sph_rj%nidx_rj(2)
        i_in =  1 + (circle%kr_gl_rcirc_in-1 ) * sph_rj%istep_rj(1)     &
     &            + (j-1) * sph_rj%istep_rj(2)
        i_out = 1 + (circle%kr_gl_rcirc_out-1) * sph_rj%istep_rj(1)     &
     &            + (j-1) * sph_rj%istep_rj(2)
        l = sph_rj%idx_gl_1d_rj_j(j,2)
        m = sph_rj%idx_gl_1d_rj_j(j,3)
        g3 = dble(l * (l+1))
        dydp_circ =   - dble(m) * asin_t * P_circ(j)
        atdydt_circ =   two * cos_t*asin_t * dPdt_circ(j)
        d2ydp2_circ = - two * (dble(m)*asin_t)**2 * P_circ(j)
        d2ydtdp_circ = - two * dble(m) * asin_t                         &
     &                  * (dPdt_circ(j) - cos_t*asin_t * P_circ(j))
!
        d_mid(1) = (c_in*d_rj(i_in,1) + c_out*d_rj(i_out,1))
        d_mid(2) = (c_in*d_rj(i_in,2) + c_out*d_rj(i_out,2))
        d_mid(3) = (c_in*d_rj(i_in,3) + c_out*d_rj(i_out,3))
        d_mid(4) = (c_in*d_rj(i_in,3) + c_out*d_rj(i_out,4))
        d_mid(5) = (c_in*d_rj(i_in,3) + c_out*d_rj(i_out,5))
        d_mid(6) = (c_in*d_rj(i_in,3) + c_out*d_rj(i_out,6))
!
        tsr_circ( m,1) = tsr_circ( m,1) + g3 * P_circ(j) * d_mid(1)
!
        tsr_circ( m,2) = tsr_circ( m,2) + dPdt_circ(j) * d_mid(2)
        tsr_circ(-m,3) = tsr_circ(-m,3) + dydp_circ *    d_mid(3)
!
        tsr_circ(-m,2) = tsr_circ(-m,2) + dydp_circ *    d_mid(2)
        tsr_circ( m,3) = tsr_circ( m,3) - dPdt_circ(j) * d_mid(3)
!
        tsr_circ( m,4) = tsr_circ( m,4) + g3 * P_circ(j) * d_mid(4)
!
        tsr_circ( m,5) = tsr_circ( m,5)                                 &
     &                  - (g3*P_circ(j) + atdydt_circ + d2ydp2_circ)    &
     &                   * d_mid(5)
        tsr_circ(-m,6) = tsr_circ(-m,6) + d2ydtdp_circ * d_mid(5)
!
        tsr_circ(-m,5) = tsr_circ(-m,5) - d2ydtdp_circ * d_mid(6)
        tsr_circ( m,6) = tsr_circ( m,6)                                 &
     &                  - (g3*P_circ(j) + atdydt_circ + d2ydp2_circ)    &
     &                   * d_mid(5)
      end do
!
!$mop parallel workshare
      tsr_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,1)            &
     &  = tsr_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,1)        &
     &   * circ_spec%ar2_circle
      tsr_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,2)            &
     &  = tsr_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,1)        &
     &   * circ_spec%ar_circle
      tsr_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,3)            &
     &  = tsr_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,1)        &
     &   * circ_spec%ar_circle
      tsr_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,4)            &
     &  = tsr_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,4)        &
     &   * circ_spec%ar2_circle
      tsr_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,5)            &
     &  = tsr_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,5)        &
     &   * circ_spec%ar2_circle
      tsr_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,6)            &
     &  = tsr_circ(-circ_spec%ltr_circle:circ_spec%ltr_circle,6)        &
     &   * circ_spec%ar2_circle
!$mop end parallel workshare
!
      end subroutine circle_bwd_leg_trans_sym_tensor
!
! ----------------------------------------------------------------------

      end module circle_bwd_transfer_rj
