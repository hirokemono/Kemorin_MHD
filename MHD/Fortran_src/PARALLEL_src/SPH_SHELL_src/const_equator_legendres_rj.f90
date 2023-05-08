!>@file   const_equator_legendres_rj.f90
!!@brief  module const_equator_legendres_rj
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform considering symmetry
!!
!!@verbatim
!!      subroutine s_const_equator_legendres_rj                         &
!!     &         (sph_params, sph_rj, sph_rlm, sph_rtm, comms_sph,      &
!!     &          trans_p, Pvec_rj, SR_sig, SR_r)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        real(kind = kreal), intent(inout) :: Pvec_rj(sph_rj%nnod_rj,4)
!!         type(send_recv_status), intent(inout) :: SR_sig
!!         type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module const_equator_legendres_rj
!
      use m_precision
      use m_machine_parameter
      use m_constants
!
      use t_legendre_work_sym_matmul
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_phys_data
      use t_work_4_sph_trans
      use t_solver_SR
!
      implicit none
!
      private :: set_equator_lagende, set_equator_legendre_lj
      private :: eq_leg_fwd_trans_vector_sym
      private :: eq_leg_fwd_trans_scalar_sym
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_const_equator_legendres_rj                           &
     &         (sph_params, sph_rj, sph_rlm, sph_rtm, comms_sph,        &
     &          trans_p, Pvec_rj, SR_sig, SR_r)
!
      use calypso_mpi
      use set_legendre_matrices
      use spherical_SRs_N
      use copy_spectr_4_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(phys_data), intent(inout) :: Pvec_rj
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      real(kind = kreal), allocatable :: P_eq(:)
      real(kind = kreal), allocatable :: dPdt_eq(:)
!
      real(kind = kreal), allocatable :: Ps_eq(:)
      real(kind = kreal), allocatable :: dPsdt_eq(:)
!
      allocate( P_eq(sph_rlm%nidx_rlm(2)) )
      allocate( dPdt_eq(sph_rlm%nidx_rlm(2)) )
!
      allocate(Ps_eq(sph_rlm%nidx_rlm(2)))
      allocate(dPsdt_eq(sph_rlm%nidx_rlm(2)))
!
!$omp parallel workshare
      P_eq(1:sph_rlm%nidx_rlm(2)) = 0.0d0
      dPdt_eq(1:sph_rlm%nidx_rlm(2)) = 0.0d0
!
      Ps_eq(1:sph_rlm%nidx_rlm(2)) =    0.0d0
      dPsdt_eq(1:sph_rlm%nidx_rlm(2)) = 0.0d0
!$omp end parallel workshare
!
      call set_equator_lagende(sph_params%l_truncation,                 &
     &    sph_rtm, sph_rlm, trans_p%idx_trns, P_eq, dPdt_eq)
!
      call set_equator_legendre_lj                                      &
     &  (sph_rtm%nidx_rtm(3), sph_rlm%nidx_rlm(2),                      &
     &   trans_p%idx_trns%lstack_rlm, trans_p%idx_trns%lstack_even_rlm, &
     &   P_eq, dPdt_eq, Ps_eq, dPsdt_eq)
      deallocate(P_eq, dPdt_eq)
!
!$omp parallel workshare
      SR_r%WS(1:SR_r%n_WS) = 0.0d0
!$omp end parallel workshare
      call eq_leg_fwd_trans_vector_sym(sph_rtm, sph_rlm,                &
     &    comms_sph%comm_rlm, trans_p%idx_trns, trans_p%leg%g_sph_rlm,  &
     &    Ps_eq, dPsdt_eq, SR_r%n_WS, SR_r%WS(1))
      call eq_leg_fwd_trans_scalar_sym(sph_rtm, sph_rlm,                &
     &    comms_sph%comm_rlm, trans_p%idx_trns,                         &
     &    Ps_eq, SR_r%n_WS, SR_r%WS(1))
      deallocate(Ps_eq, dPsdt_eq)
!
!
      call calypso_sph_comm_N(ifour,                                    &
     &    comms_sph%comm_rlm, comms_sph%comm_rj, SR_sig, SR_r)
      call finish_send_recv_sph(comms_sph%comm_rlm, SR_sig)
!
      call sel_sph_rj_vector_from_recv                                  &
     &   (trans_p%iflag_SPH_recv, ifour, ione,   ione,                  &
     &    comms_sph%comm_rj, SR_r%n_WR, SR_r%WR(1), Pvec_rj)
      call sel_sph_rj_scalar_from_recv                                  &
     &   (trans_p%iflag_SPH_recv, ifour, ifour, ifour,                  &
     &    comms_sph%comm_rj, SR_r%n_WR, SR_r%WR(1), Pvec_rj)
!
      end subroutine s_const_equator_legendres_rj
!
! -----------------------------------------------------------------------
!
      subroutine eq_leg_fwd_trans_vector_sym                            &
     &         (sph_rtm, sph_rlm, comm_rlm, idx_trns, g_sph_rlm,        &
     &          Ps_eq, dPsdt_eq, n_WS, WS)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind = kreal), intent(in) :: Ps_eq(sph_rlm%nidx_rlm(2))
      real(kind = kreal), intent(in)                                    &
     &           :: g_sph_rlm(sph_rlm%nidx_rlm(2),17)
      real(kind = kreal), intent(in) :: dPsdt_eq(sph_rlm%nidx_rlm(2))
      integer(kind = kint), intent(in) :: n_WS
!
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, ie_rlm, io_rlm
      integer(kind = kint) :: mp_rlm, ie_send, io_send
      integer(kind = kint) :: jst, nj_rlm, jj, je_rlm, jo_rlm
      integer(kind = kint) :: nle_rtm, nlo_rtm, n_jk_e, n_jk_o
      real(kind = kreal) :: gme, gmo, g3e, g3o
!
!
!$omp parallel workshare
      WS(1:4*comm_rlm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      nle_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
      nlo_rtm = sph_rtm%nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,kst,ked,jj,k_rlm,je_rlm,jo_rlm,            &
!$omp&                    mp_rlm,jst,nj_rlm,n_jk_e,n_jk_o,              &
!$omp&                    ie_rlm,io_rlm,ie_send,io_send,gme,gmo,g3e,g3o)
      do ip = 1, np_smp
        kst = sph_rlm%istack_rlm_kr_smp(ip-1) + 1
        ked = sph_rlm%istack_rlm_kr_smp(ip  )
        do k_rlm = kst, ked
!
          do mp_rlm = 1, sph_rtm%nidx_rtm(3)
            jst = idx_trns%lstack_rlm(mp_rlm-1)
            nj_rlm = idx_trns%lstack_rlm(mp_rlm)                        &
     &              - idx_trns%lstack_rlm(mp_rlm-1)
            n_jk_e = (nj_rlm+1) / 2
            n_jk_o =  nj_rlm - n_jk_e
!    even l-m
!    odd  l-m
            do jj = 1, nj_rlm/2
              je_rlm = 2*jj + jst - 1
              jo_rlm = 2*jj + jst
              ie_rlm = 1 + (je_rlm-1) * sph_rlm%istep_rlm(2)            &
     &                   + (k_rlm-1) *  sph_rlm%istep_rlm(1)
              io_rlm = 1 + (jo_rlm-1) * sph_rlm%istep_rlm(2)            &
     &                   + (k_rlm-1)  * sph_rlm%istep_rlm(1)
!
              g3e = g_sph_rlm(je_rlm,3)
              g3o = g_sph_rlm(jo_rlm,3)
              gme = dble(sph_rlm%idx_gl_1d_rlm_j(je_rlm,3))
              gmo = dble(sph_rlm%idx_gl_1d_rlm_j(jo_rlm,3))
!
              ie_send = (comm_rlm%irev_sr(ie_rlm) - 1) * 4
              io_send = (comm_rlm%irev_sr(io_rlm) - 1) * 4
!
              WS(ie_send+1) =   Ps_eq(jj+jst)*g3e
              WS(ie_send+2) = - Ps_eq(jj+jst)*gme
              WS(ie_send+3) =   dPsdt_eq(jj+jst)
!
              WS(io_send+1) =   Ps_eq(jj+jst+n_jk_e)*g3o
              WS(io_send+2) = - Ps_eq(jj+jst+n_jk_e)*gmo
              WS(io_send+3) =   dPsdt_eq(jj+jst+n_jk_e)
            end do
!
!   the last even l-m
            do jj = nj_rlm/2+1, (nj_rlm+1)/2
              je_rlm = 2*jj + jst - 1
              ie_rlm = 1 + (je_rlm-1) * sph_rlm%istep_rlm(2)            &
     &                   + (k_rlm-1) *  sph_rlm%istep_rlm(1)
              g3e = g_sph_rlm(je_rlm,3)
              gme = dble(sph_rlm%idx_gl_1d_rlm_j(je_rlm,3))
!
              ie_send = (comm_rlm%irev_sr(ie_rlm) - 1) * 4
!
              WS(ie_send+1) =   Ps_eq(jj+jst)*g3e
              WS(ie_send+2) = - Ps_eq(jj+jst)*gme
              WS(ie_send+3) =   dPsdt_eq(jj+jst)
            end do
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine eq_leg_fwd_trans_vector_sym
!
! -----------------------------------------------------------------------
!
      subroutine eq_leg_fwd_trans_scalar_sym(sph_rtm, sph_rlm,          &
     &          comm_rlm, idx_trns, Ps_eq, n_WS, WS)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind = kreal), intent(in) :: Ps_eq(sph_rlm%nidx_rlm(2))
      integer(kind = kint), intent(in) :: n_WS
!
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      integer(kind = kint) :: ip, kst, ked, k_rlm
      integer(kind = kint) :: nle_rtm, nlo_rtm, je_rlm, jo_rlm
      integer(kind = kint) :: ie_rlm, io_rlm, ie_send, io_send
      integer(kind = kint) :: mp_rlm, jst, nj_rlm, jj, n_jk_e, n_jk_o
!
!
      nle_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
      nlo_rtm = sph_rtm%nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,jj,k_rlm,mp_rlm,n_jk_e,n_jk_o,     &
!$omp&                    jst,nj_rlm,ie_rlm,io_rlm,je_rlm,jo_rlm,       &
!$omp&                    ie_send,io_send)
      do ip = 1, np_smp
        kst = sph_rlm%istack_rlm_kr_smp(ip-1) + 1
        ked = sph_rlm%istack_rlm_kr_smp(ip  )
        do k_rlm = kst, ked
          do mp_rlm = 1, sph_rtm%nidx_rtm(3)
            jst = idx_trns%lstack_rlm(mp_rlm-1)
            nj_rlm = idx_trns%lstack_rlm(mp_rlm)                        &
     &              - idx_trns%lstack_rlm(mp_rlm-1)
            n_jk_e = (nj_rlm+1) / 2
            n_jk_o =  nj_rlm - n_jk_e
!    even l-m
!    odd  l-m
            do jj = 1, nj_rlm/2
              je_rlm = 2*jj + jst - 1
              jo_rlm = 2*jj + jst
!
              ie_rlm = 1 + (je_rlm-1) * sph_rlm%istep_rlm(2)            &
     &                   + (k_rlm-1) *  sph_rlm%istep_rlm(1)
              io_rlm = 1 + (jo_rlm-1) * sph_rlm%istep_rlm(2)            &
     &                   + (k_rlm-1) *  sph_rlm%istep_rlm(1)
              ie_send = 4 + (comm_rlm%irev_sr(ie_rlm) - 1) * 4
              io_send = 4 + (comm_rlm%irev_sr(io_rlm) - 1) * 4
!
              WS(ie_send) = Ps_eq(jj+jst)
              WS(io_send) = Ps_eq(jj+jst+n_jk_e)
            end do
!
!   the last even l-m
            do jj = nj_rlm/2+1, (nj_rlm+1)/2
              je_rlm = 2*jj + jst - 1
              ie_rlm = 1 + (je_rlm-1) * sph_rlm%istep_rlm(2)            &
     &                   + (k_rlm-1) *  sph_rlm%istep_rlm(1)
              ie_send = 4 + (comm_rlm%irev_sr(ie_rlm) - 1) * 4
              WS(ie_send) = Ps_eq(jj+jst)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine eq_leg_fwd_trans_scalar_sym
!
! -----------------------------------------------------------------------
!
      subroutine set_equator_legendre_lj                                &
     &         (mphi_rtm, jmax_rlm, lstack_rlm, lstack_even_rlm,        &
     &          P_eq, dPdt_eq, Ps_eq, dPsdt_eq)
!
      integer(kind = kint), intent(in) :: mphi_rtm, jmax_rlm
      integer(kind = kint), intent(in) :: lstack_rlm(0:mphi_rtm)
      integer(kind = kint), intent(in) :: lstack_even_rlm(0:mphi_rtm)
!
      real(kind= kreal), intent(in) :: P_eq(jmax_rlm)
      real(kind= kreal), intent(in) :: dPdt_eq(jmax_rlm)
!
      real(kind= kreal), intent(inout) :: Ps_eq(jmax_rlm)
      real(kind= kreal), intent(inout) :: dPsdt_eq(jmax_rlm)
!
      integer(kind = kint) :: j_rlm
      integer(kind = kint) :: mp_rlm, jst, n_jk_e, n_jk_o, jj
!
!
!$omp parallel do private(jst,j_rlm,jj,n_jk_e,n_jk_o)
      do mp_rlm = 1, mphi_rtm
        jst = lstack_rlm(mp_rlm-1)
        n_jk_e = lstack_even_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
        n_jk_o = lstack_rlm(mp_rlm) - lstack_even_rlm(mp_rlm)
!
        do jj = 1, n_jk_e
          j_rlm = 2*jj + jst - 1
          Ps_eq(jj+jst) =     P_eq(j_rlm)
          dPsdt_eq(jj+jst) =  dPdt_eq(j_rlm)
        end do
!
        do jj = 1, n_jk_o
          j_rlm = 2*jj + jst
          Ps_eq(jj+jst+n_jk_e) =     P_eq(j_rlm)
          dPsdt_eq(jj+jst+n_jk_e) =  dPdt_eq(j_rlm)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_equator_legendre_lj
!
! -----------------------------------------------------------------------
!
      subroutine set_equator_lagende(l_truncation, sph_rtm, sph_rlm,    &
     &                               idx_trns, P_eq, dPdt_eq)
!
      use schmidt_fix_m
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      integer(kind = kint), intent(in) :: l_truncation
!
      real(kind= kreal), intent(inout) :: P_eq(sph_rlm%nidx_rlm(2))
      real(kind= kreal), intent(inout) :: dPdt_eq(sph_rlm%nidx_rlm(2))
!
      integer(kind = kint) :: j, l, m, mm, jj
      integer(kind = kint) :: jst, jed
      real(kind = kreal) :: p_m(0:l_truncation), dp_m(0:l_truncation)
      real(kind = kreal) :: pmp1(0:l_truncation), pmn1(0:l_truncation)
      real(kind = kreal) :: df_m(0:l_truncation+2)
      real(kind = kreal) :: colat_eq
!
      colat_eq = two * atan(one)
!
!$omp parallel do private(j,l,m,mm,jj,jst,jed,p_m,dp_m,pmn1,pmp1,df_m)
      do m = 1, sph_rtm%nidx_rtm(3)
        mm = abs(sph_rtm%idx_gl_1d_rtm_m(m,2))
        jst = idx_trns%lstack_rlm(m-1) + 1
        jed = idx_trns%lstack_rlm(m)
        call schmidt_legendres_m(l_truncation, mm, colat_eq,            &
     &                           p_m, dp_m, pmn1, pmp1, df_m)
!
        do j = jst, jed
          jj = sph_rlm%idx_gl_1d_rlm_j(j,1)
          l =  sph_rlm%idx_gl_1d_rlm_j(j,2)
          P_eq(j) =    p_m(l)
          dPdt_eq(j) = dp_m(l)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_equator_lagende
!
! -----------------------------------------------------------------------
!
      end module const_equator_legendres_rj
