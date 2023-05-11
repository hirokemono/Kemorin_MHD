!>@file   circle_bwd_transfer_rj.f90
!!@brief  module circle_bwd_transfer_rj
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform considering symmetry
!!
!!@verbatim
!!      subroutine dbench_leg_bwd_trans_rj                              &
!!     &         (iflag_FFT, sph_rj, rj_fld, ipol, iphys_dbench,        &
!!     &          circle, circ_spec, d_circle, WK_circle_fft)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(phys_address), intent(in) :: ipol
!!        type(base_field_address), intent(in) :: iphys_dbench
!!        type(fields_on_circle), intent(in) :: circle
!!        type(circle_transform_spetr), intent(inout) :: circ_spec
!!        type(phys_data), intent(inout) :: d_circle
!!        type(working_FFTs), intent(inout) :: WK_circle_fft
!!
!!      subroutine circle_leg_bwd_trans_rj                              &
!!     &         (iflag_FFT, sph_rj, rj_fld, nod_fld, ipol_circle_trns, &
!!     &          circle, circ_spec, d_circle, WK_circle_fft)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(phys_data), intent(in) :: nod_fld
!!        integer(kind = kint), intent(in)                              &
!!       &                      :: ipol_circle_trns(nod_fld%num_phys_viz)
!!        type(fields_on_circle), intent(in) :: circle
!!        type(circle_transform_spetr), intent(inout) :: circ_spec
!!        type(phys_data), intent(inout) :: d_circle
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
      subroutine dbench_leg_bwd_trans_rj                                &
     &         (iflag_FFT, sph_rj, rj_fld, ipol, iphys_dbench,          &
     &          circle, circ_spec, d_circle, WK_circle_fft)
!
      use t_base_field_labels
      use t_FFT_selector
      use calypso_mpi_real
      use transfer_to_long_integers
      use cal_circle_transform
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(phys_address), intent(in) :: ipol
      type(base_field_address), intent(in) :: iphys_dbench
      type(fields_on_circle), intent(in) :: circle
!
      type(phys_data), intent(inout) :: d_circle
      type(circle_transform_spetr), intent(inout) :: circ_spec
!
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
      integer(kind = kint_gl) :: num64
      integer(kind = kint) :: i_comp
!
!
      call each_circle_leg_bwd_trans_rj                                 &
     &   (n_vector, ipol%base%i_velo, iphys_dbench%i_velo,              &
     &    sph_rj, rj_fld, circle, circ_spec%ltr_circle,                 &
     &    circ_spec%P_circ, circ_spec%dPdt_circ,                        &
     &    circ_spec%ar_circle, circ_spec%ar2_circle,                    &
     &    d_circle%ntot_phys, circ_spec%d_circ_lc)
      call each_circle_leg_bwd_trans_rj                                 &
     &   (n_vector, ipol%base%i_magne, iphys_dbench%i_magne,            &
     &    sph_rj, rj_fld, circle, circ_spec%ltr_circle,                 &
     &    circ_spec%P_circ, circ_spec%dPdt_circ,                        &
     &    circ_spec%ar_circle, circ_spec%ar2_circle,                    &
     &    d_circle%ntot_phys, circ_spec%d_circ_lc)
      call each_circle_leg_bwd_trans_rj                                 &
     &   (n_scalar, ipol%base%i_temp, iphys_dbench%i_temp,              &
     &    sph_rj, rj_fld, circle, circ_spec%ltr_circle,                 &
     &    circ_spec%P_circ, circ_spec%dPdt_circ,                        &
     &    circ_spec%ar_circle, circ_spec%ar2_circle,                    &
     &    d_circle%ntot_phys, circ_spec%d_circ_lc)
      call each_circle_leg_bwd_trans_rj                                 &
     &   (n_scalar, ipol%base%i_light, iphys_dbench%i_light,            &
     &    sph_rj, rj_fld, circle, circ_spec%ltr_circle,                 &
     &    circ_spec%P_circ, circ_spec%dPdt_circ,                        &
     &    circ_spec%ar_circle, circ_spec%ar2_circle,                    &
     &    d_circle%ntot_phys, circ_spec%d_circ_lc)
!
      num64                                                             &
     &  = cast_long(d_circle%ntot_phys * (2*circ_spec%ltr_circle+1))
      call calypso_mpi_reduce_real                                      &
     &   (circ_spec%d_circ_lc(-circ_spec%ltr_circle,1),                 &
     &    circ_spec%d_circ_gl(-circ_spec%ltr_circle,1),                 &
     &    num64, MPI_SUM, 0)
!
      if(my_rank .ne. 0) return
!
      if(circle%iflag_circle_coord .eq. iflag_circle_cyl) then
        if(iphys_dbench%i_velo .gt. 0) then
          i_comp = iphys_dbench%i_velo
          call overwrt_circle_sph_vect_2_cyl                            &
     &      (circ_spec%theta_circle, circ_spec%ltr_circle,              &
     &       circ_spec%d_circ_gl(-circ_spec%ltr_circle,i_comp))
        end if
        if(iphys_dbench%i_magne .gt. 0) then
          i_comp = iphys_dbench%i_magne
          call overwrt_circle_sph_vect_2_cyl                            &
     &      (circ_spec%theta_circle, circ_spec%ltr_circle,              &
     &       circ_spec%d_circ_gl(-circ_spec%ltr_circle,i_comp))
        end if
      end if
!
      call cal_circle_spectrum_vector                                   &
     &   (d_circle%ntot_phys, circ_spec%ltr_circle, circ_spec%d_circ_gl, &
     &    circle%mphi_circle, circ_spec%vrtm_mag, circ_spec%vrtm_phase)
      call copy_circle_spectrum_4_fft                                   &
     &   (d_circle%ntot_phys, circ_spec%ltr_circle,                     &
     &    circ_spec%d_circ_gl, circle%mphi_circle, d_circle%d_fld(1,1))
!
      do i_comp = 1, d_circle%ntot_phys
        call backward_FFT_select                                        &
     &     (iflag_FFT, np_smp, circ_spec%istack_circfft_smp, ione,      &
     &      circle%mphi_circle, d_circle%d_fld(1,i_comp),               &
     &      WK_circle_fft)
      end do
!
      end subroutine dbench_leg_bwd_trans_rj
!
! ----------------------------------------------------------------------
!
      subroutine circle_leg_bwd_trans_rj                                &
     &         (iflag_FFT, sph_rj, rj_fld, nod_fld, ipol_circle_trns,   &
     &          circle, circ_spec, d_circle, WK_circle_fft)
!
      use t_FFT_selector
      use calypso_mpi_real
      use transfer_to_long_integers
      use cal_circle_transform
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in)                                  &
     &                      :: ipol_circle_trns(nod_fld%num_phys_viz)
      type(fields_on_circle), intent(in) :: circle
!
      type(circle_transform_spetr), intent(inout) :: circ_spec
      type(phys_data), intent(inout) :: d_circle
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
      integer(kind = kint) :: i_fld, num_comp, i_trns
      integer(kind = kint_gl) :: num64
!
!
      do i_fld = 1, nod_fld%num_phys_viz
        num_comp = nod_fld%istack_component(i_fld)                      &
     &            - nod_fld%istack_component(i_fld-1)
        i_trns = nod_fld%istack_component(i_fld-1) + 1
        call each_circle_leg_bwd_trans_rj                               &
     &     (num_comp, ipol_circle_trns(i_fld), i_trns,                  &
     &      sph_rj, rj_fld, circle, circ_spec%ltr_circle,               &
     &      circ_spec%P_circ, circ_spec%dPdt_circ,                      &
     &      circ_spec%ar_circle, circ_spec%ar2_circle,                  &
     &      d_circle%ntot_phys, circ_spec%d_circ_lc)
      end do
!
      num64                                                             &
     &  = cast_long(d_circle%ntot_phys * (2*circ_spec%ltr_circle+1))
      call calypso_mpi_reduce_real                                      &
     &   (circ_spec%d_circ_lc(-circ_spec%ltr_circle,1),                 &
     &    circ_spec%d_circ_gl(-circ_spec%ltr_circle,1),                 &
     &    num64, MPI_SUM, 0)
!
!
      if(my_rank .ne. 0) return
!
      if(circle%iflag_circle_coord .eq. iflag_circle_cyl) then
        do i_fld = 1, nod_fld%num_phys_viz
          num_comp = nod_fld%istack_component(i_fld)                    &
     &            - nod_fld%istack_component(i_fld-1)
          i_trns = nod_fld%istack_component(i_fld-1) + 1
          if(num_comp .eq. n_vector) then
            call overwrt_circle_sph_vect_2_cyl                          &
     &         (circ_spec%theta_circle, circ_spec%ltr_circle,           &
     &          circ_spec%d_circ_gl(-circ_spec%ltr_circle,i_trns))
!          else if(num_comp .eq. n_sym_tensor) then
!            call overwrt_circle_sph_vect_2_cyl                         &
!     &         (circ_spec%theta_circle, circ_spec%ltr_circle,          &
!     &          circ_spec%d_circ_gl(-circ_spec%ltr_circle,i_trns))
          end if
        end do
      end if
!
      call cal_circle_spectrum_vector                                   &
     &   (d_circle%ntot_phys, circ_spec%ltr_circle, circ_spec%d_circ_gl,          &
     &    circle%mphi_circle, circ_spec%vrtm_mag, circ_spec%vrtm_phase)
      call copy_circle_spectrum_4_fft                                   &
     &   (d_circle%ntot_phys, circ_spec%ltr_circle,                     &
     &    circ_spec%d_circ_gl, circle%mphi_circle, d_circle%d_fld(1,1))
!
      do i_fld = 1, d_circle%ntot_phys
        call backward_FFT_select                                        &
     &    (iflag_FFT, np_smp, circ_spec%istack_circfft_smp, ione,       &
     &     circle%mphi_circle, d_circle%d_fld(1,i_fld), WK_circle_fft)
      end do
!
      end subroutine circle_leg_bwd_trans_rj
!
! ----------------------------------------------------------------------
!
      subroutine each_circle_leg_bwd_trans_rj                           &
     &         (num_comp, ipol_rj, i_trns, sph_rj, rj_fld, circle,      &
     &          ltr_circ, P_circ, dPdt_circ, ar_circle, ar2_circle,     &
     &          ntot_comp, d_circ_lc)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: num_comp
      integer(kind = kint), intent(in) :: ipol_rj, i_trns
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: ltr_circ
      real(kind = kreal), intent(in) :: P_circ(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(in) :: dPdt_circ(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(in) :: ar_circle, ar2_circle
      type(fields_on_circle), intent(in) :: circle
      integer(kind = kint), intent(in) :: ntot_comp
!
      real(kind = kreal), intent(inout)                                 &
     &                     :: d_circ_lc(-ltr_circ:ltr_circ, ntot_comp)
!
      integer(kind = kint) :: i_fld, nd
!
!
      if((ipol_rj*i_trns) .le. 0) return
      if(num_comp .eq. n_sym_tensor) then
!          call circle_bwd_leg_trans_sym_tensor(sph_rj, circle,         &
!     &        ltr_circ, ar_circle, ar2_circle, P_circ, dPdt_circ,      &
!     &        rj_fld%d_fld(1,ipol_rj), d_circ_lc(-ltr_circ,i_trns))
        do nd = 0, 5
          call circle_bwd_leg_trans_scalar(sph_rj, circle, ltr_circ,    &
     &        P_circ, rj_fld%d_fld(1,ipol_rj),                          &
     &        d_circ_lc(-ltr_circ,i_trns+nd))
        end do
      else if(num_comp .eq. n_vector) then
        call circle_bwd_leg_trans_vector(sph_rj, circle, ltr_circ,      &
     &      ar_circle, ar2_circle, P_circ, dPdt_circ,                   &
     &      rj_fld%d_fld(1,ipol_rj), d_circ_lc(-ltr_circ,i_trns))
      else
        call circle_bwd_leg_trans_scalar(sph_rj, circle, ltr_circ,      &
     &      P_circ, rj_fld%d_fld(1,ipol_rj),                            &
     &      d_circ_lc(-ltr_circ,i_trns))
      end if
!
      end subroutine each_circle_leg_bwd_trans_rj
!
! ----------------------------------------------------------------------
!
      subroutine circle_bwd_leg_trans_scalar                            &
     &         (sph_rj, circle, ltr, P_circ, d_rj, scl_circ)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fields_on_circle), intent(in) :: circle
      integer(kind = kint), intent(in) :: ltr
      real(kind = kreal), intent(in) :: P_circ(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout) :: scl_circ(-ltr:ltr)
!
      real(kind = kreal) :: c_in, c_out
      real(kind = kreal) :: d_mid
      integer(kind = kint) :: i_in, i_out, j, m
!
!$omp parallel workshare
      scl_circ(-ltr:ltr) = 0.0d0
!$omp end parallel workshare
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
      subroutine circle_bwd_leg_trans_vector(sph_rj, circle,            &
     &          ltr, ar_circle, ar2_circle, P_circ, dPdt_circ,          &
     &          d_rj, vec_circ)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fields_on_circle), intent(in) :: circle
      integer(kind = kint), intent(in) :: ltr
      real(kind = kreal), intent(in) :: ar_circle, ar2_circle
      real(kind = kreal), intent(in) :: P_circ(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(in) :: dPdt_circ(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj,3)
!
      real(kind = kreal), intent(inout) :: vec_circ(-ltr:ltr,3)
!
      real(kind = kreal) :: c_in, c_out
      real(kind = kreal) :: g3, asin_t, dydp_circ
      real(kind = kreal) :: d_mid(3)
      integer(kind = kint) :: i_in, i_out, j, l, m
!
!$omp parallel workshare
      vec_circ(-ltr:ltr,1:3) = 0.0d0
!$omp end parallel workshare
!
      c_in =  circle%coef_gl_rcirc_in
      c_out = circle%coef_gl_rcirc_out
      asin_t = one / sin(circle%colat_circle)
      do j = 1, sph_rj%nidx_rj(2)
        i_in =  1 + (circle%kr_gl_rcirc_in-1 ) * sph_rj%istep_rj(1)     &
     &            + (j-1) * sph_rj%istep_rj(2)
        i_out = 1 + (circle%kr_gl_rcirc_out-1) * sph_rj%istep_rj(1)     &
     &            + (j-1) * sph_rj%istep_rj(2)
        l = sph_rj%idx_gl_1d_rj_j(j,2)
        m = sph_rj%idx_gl_1d_rj_j(j,3)
        g3 = dble(l * (l+1))
        d_mid(1) = (c_in*d_rj(i_in,1) + c_out*d_rj(i_out,1))
        d_mid(2) = (c_in*d_rj(i_in,2) + c_out*d_rj(i_out,2))
        d_mid(3) = (c_in*d_rj(i_in,3) + c_out*d_rj(i_out,3))
!
        vec_circ( m,1) = vec_circ( m,1) + g3 * P_circ(j) * d_mid(1)
        vec_circ( m,2) = vec_circ( m,2) + dPdt_circ(j) *   d_mid(2)
        vec_circ( m,3) = vec_circ( m,3) - dPdt_circ(j) *   d_mid(3)
      end do
!
      do j = 1, sph_rj%nidx_rj(2)
        i_in =  1 + (circle%kr_gl_rcirc_in-1 ) * sph_rj%istep_rj(1)     &
     &            + (j-1) * sph_rj%istep_rj(2)
        i_out = 1 + (circle%kr_gl_rcirc_out-1) * sph_rj%istep_rj(1)     &
     &            + (j-1) * sph_rj%istep_rj(2)
        l = sph_rj%idx_gl_1d_rj_j(j,2)
        m = sph_rj%idx_gl_1d_rj_j(j,3)
        dydp_circ = - dble(m) * asin_t * P_circ(j)
        d_mid(2) = (c_in*d_rj(i_in,2) + c_out*d_rj(i_out,2))
        d_mid(3) = (c_in*d_rj(i_in,3) + c_out*d_rj(i_out,3))
!
        vec_circ(-m,2) = vec_circ(-m,2) + dydp_circ *    d_mid(3)
        vec_circ(-m,3) = vec_circ(-m,3) + dydp_circ *    d_mid(2)
      end do
!
!$mop parallel workshare
      vec_circ(-ltr:ltr,1) = vec_circ(-ltr:ltr,1) * ar2_circle
      vec_circ(-ltr:ltr,2) = vec_circ(-ltr:ltr,2) * ar_circle
      vec_circ(-ltr:ltr,3) = vec_circ(-ltr:ltr,3) * ar_circle
!$mop end parallel workshare
!
      end subroutine circle_bwd_leg_trans_vector
!
! ----------------------------------------------------------------------
!
      subroutine circle_bwd_leg_trans_sym_tensor(sph_rj, circle,        &
     &          ltr, ar_circle, ar2_circle,                             &
     &          P_circ, dPdt_circ, d_rj, tsr_circ)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fields_on_circle), intent(in) :: circle
      integer(kind = kint), intent(in) :: ltr
      real(kind = kreal), intent(in) :: ar_circle, ar2_circle
      real(kind = kreal), intent(in) :: P_circ(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(in) :: dPdt_circ(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj,6)
!
      real(kind = kreal), intent(inout) :: tsr_circ(-ltr:ltr,6)
!
      real(kind = kreal) :: c_in, c_out
      real(kind = kreal) :: g3, asin_t, cos_t, dydp_circ
      real(kind = kreal) :: atdydt_circ, d2ydp2_circ, d2ydtdp_circ
      real(kind = kreal) :: d_mid(6)
      integer(kind = kint) :: i_in, i_out, j, l, m
!
!$omp parallel workshare
      tsr_circ(-ltr:ltr,1:6) = 0.0d0
!$omp end parallel workshare
!
      c_in =  circle%coef_gl_rcirc_in
      c_out = circle%coef_gl_rcirc_out
      asin_t = one / sin(circle%colat_circle)
      cos_t =  cos(circle%colat_circle)
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
        tsr_circ( m,3) = tsr_circ( m,3) - dPdt_circ(j) * d_mid(3)
!
        tsr_circ( m,4) = tsr_circ( m,4) + g3 * P_circ(j) * d_mid(4)
!
        tsr_circ( m,5) = tsr_circ( m,5)                                 &
     &                  - (g3*P_circ(j) + atdydt_circ + d2ydp2_circ)    &
     &                   * d_mid(5)
        tsr_circ( m,6) = tsr_circ( m,6)                                 &
     &                  - (g3*P_circ(j) + atdydt_circ + d2ydp2_circ)    &
     &                   * d_mid(6)
      end do
!
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
        d_mid(2) = (c_in*d_rj(i_in,2) + c_out*d_rj(i_out,2))
        d_mid(3) = (c_in*d_rj(i_in,3) + c_out*d_rj(i_out,3))
        d_mid(5) = (c_in*d_rj(i_in,3) + c_out*d_rj(i_out,5))
        d_mid(6) = (c_in*d_rj(i_in,3) + c_out*d_rj(i_out,6))
!
        tsr_circ(-m,2) = tsr_circ(-m,2) + dydp_circ *    d_mid(3)
        tsr_circ(-m,3) = tsr_circ(-m,3) + dydp_circ *    d_mid(2)
!
        tsr_circ(-m,5) = tsr_circ(-m,5) - d2ydtdp_circ * d_mid(6)
        tsr_circ(-m,6) = tsr_circ(-m,6) + d2ydtdp_circ * d_mid(5)
      end do
!
!$mop parallel workshare
      tsr_circ(-ltr:ltr,1) = tsr_circ(-ltr:ltr,1) * ar2_circle
      tsr_circ(-ltr:ltr,2) = tsr_circ(-ltr:ltr,2) * ar_circle
      tsr_circ(-ltr:ltr,3) = tsr_circ(-ltr:ltr,3) * ar_circle
      tsr_circ(-ltr:ltr,4) = tsr_circ(-ltr:ltr,4) * ar2_circle
      tsr_circ(-ltr:ltr,5) = tsr_circ(-ltr:ltr,5) * ar2_circle
      tsr_circ(-ltr:ltr,6) = tsr_circ(-ltr:ltr,6) * ar2_circle
!$mop end parallel workshare
!
      end subroutine circle_bwd_leg_trans_sym_tensor
!
! ----------------------------------------------------------------------

      end module circle_bwd_transfer_rj
