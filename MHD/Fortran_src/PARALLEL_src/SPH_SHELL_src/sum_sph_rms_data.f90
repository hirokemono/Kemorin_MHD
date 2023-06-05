!>@file   sum_sph_rms_data.f90
!!@brief      module sum_sph_rms_data
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!> @brief  Evaluate mean square by spherical hermonics coefficients
!!
!!@verbatim
!!      subroutine set_sum_table_4_sph_spectr                           &
!!     &      (l_truncation, nidx_rj, idx_gl_1d_rj_j,                   &
!!     &       num_mode_sum_l, num_mode_sum_m, num_mode_sum_lm,         &
!!     &       istack_mode_sum_l, istack_mode_sum_m, istack_mode_sum_lm,&
!!     &       item_mode_sum_l, item_mode_sum_m, item_mode_sum_lm)
!!      subroutine sum_sph_layerd_pwr(l_truncation, sph_rj, ipol,       &
!!     &                              rj_fld, g_sph_rj, pwr, WK_pwr)
!!      subroutine sum_sph_layerd_correlate(l_truncation, sph_rj,       &
!!     &          rj_fld1, rj_fld2, g_sph_rj, pwr, WK_pwr)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
!!@endverbatim
!
      module sum_sph_rms_data
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sum_table_4_sph_spectr                             &
     &      (l_truncation, nidx_rj, idx_gl_1d_rj_j,                     &
     &       num_mode_sum_l, num_mode_sum_m, num_mode_sum_lm,           &
     &       istack_mode_sum_l, istack_mode_sum_m, istack_mode_sum_lm,  &
     &       item_mode_sum_l, item_mode_sum_m, item_mode_sum_lm)
!
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
!
      integer(kind = kint), intent(inout)                               &
     &           :: num_mode_sum_l(0:l_truncation)
      integer(kind = kint), intent(inout)                               &
     &           :: num_mode_sum_m(0:l_truncation)
      integer(kind = kint), intent(inout)                               &
     &           :: num_mode_sum_lm(0:l_truncation)
!
      integer(kind = kint), intent(inout)                               &
     &           :: istack_mode_sum_l(-1:l_truncation)
      integer(kind = kint), intent(inout)                               &
     &           :: istack_mode_sum_m(-1:l_truncation)
      integer(kind = kint), intent(inout)                               &
     &           :: istack_mode_sum_lm(-1:l_truncation)
!
      integer(kind = kint), intent(inout)                               &
     &           :: item_mode_sum_l(nidx_rj(2))
      integer(kind = kint), intent(inout)                               &
     &           :: item_mode_sum_m(nidx_rj(2))
      integer(kind = kint), intent(inout)                               &
     &           :: item_mode_sum_lm(nidx_rj(2))
!
      integer(kind = kint) :: j, lg, mg, lm
      integer(kind = kint) :: icou, lcou, mcou
!
!
      num_mode_sum_l(0:l_truncation) =  0
      num_mode_sum_m(0:l_truncation) =  0
      num_mode_sum_lm(0:l_truncation) = 0
      do j = 1, nidx_rj(2)
        lg = idx_gl_1d_rj_j(j,2)
        mg = idx_gl_1d_rj_j(j,3)
        mg = abs(mg)
        lm = lg - mg
        num_mode_sum_l(lg) =  num_mode_sum_l(lg) +  1
        num_mode_sum_m(mg) =  num_mode_sum_m(mg) +  1
        num_mode_sum_lm(lm) = num_mode_sum_lm(lm) + 1
      end do
!
      istack_mode_sum_l(-1) =  0
      istack_mode_sum_m(-1) =  0
      istack_mode_sum_lm(-1) = 0
      do lm = 0, l_truncation
        istack_mode_sum_l(lm) = istack_mode_sum_l(lm-1)                 &
     &                         + num_mode_sum_l(lm)
        istack_mode_sum_m(lm) = istack_mode_sum_m(lm-1)                 &
     &                         + num_mode_sum_m(lm)
        istack_mode_sum_lm(lm) = istack_mode_sum_lm(lm-1)               &
     &                         + num_mode_sum_lm(lm)
      end do
!
      do lm = 0, l_truncation
        lcou = istack_mode_sum_l(lm-1)
        mcou = istack_mode_sum_m(lm-1)
        icou = istack_mode_sum_lm(lm-1)
        do j = 1, nidx_rj(2)
          lg = idx_gl_1d_rj_j(j,2)
          mg = idx_gl_1d_rj_j(j,3)
          mg = abs(mg)
          if (lg .eq. lm) then
            lcou = lcou + 1
            item_mode_sum_l(lcou) = j
          end if
          if (mg .eq. lm) then
            mcou = mcou + 1
            item_mode_sum_m(mcou) = j
          end if
          if ((lg-mg) .eq. lm) then
            icou = icou + 1
            item_mode_sum_lm(icou) = j
          end if
        end do
      end do
!
      end subroutine set_sum_table_4_sph_spectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sum_sph_layerd_pwr(l_truncation, sph_rj, ipol,         &
     &                              rj_fld, g_sph_rj, pwr, WK_pwr)
!
      use t_spheric_rj_data
      use t_phys_data
      use t_sph_volume_mean_square
      use t_sum_sph_rms_data
      use t_rms_4_sph_spectr
!
      use cal_rms_by_sph_spectr
      use cal_ave_4_rms_vector_sph
      use radial_int_for_sph_spec
      use sum_sph_rms_by_degree
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: l_truncation
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      type(sph_mean_squares), intent(in) :: pwr
!
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
      integer(kind = kint) :: j_fld, i_fld
      integer(kind = kint) :: icomp_rj, jcomp_st, ncomp_rj
      integer(kind = kint) :: num, i
!
!
!$omp parallel workshare
      WK_pwr%shl_l_local =  zero
      WK_pwr%shl_m_local =  zero
      WK_pwr%shl_lm_local = zero
      WK_pwr%vol_l_local =  zero
      WK_pwr%vol_m_local =  zero
      WK_pwr%vol_lm_local = zero
!$omp end parallel workshare
!
      do j_fld = 1, pwr%num_fld_sq
        i_fld = pwr%id_field(j_fld)
        icomp_rj = rj_fld%istack_component(i_fld-1) + 1
        jcomp_st = pwr%istack_comp_sq(j_fld-1) + 1
        ncomp_rj = pwr%istack_comp_sq(j_fld)                            &
     &            - pwr%istack_comp_sq(j_fld-1)
        num = sph_rj%nidx_rj(2) * ncomp_rj
        call cal_rms_sph_spec_one_field                                 &
     &     (sph_rj, ipol, ncomp_rj, g_sph_rj, icomp_rj,                 &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld,             &
     &      WK_pwr%shl_rj(0,1,1))
!
        do i = 1, pwr%num_vol_spectr
          call radial_integration                                       &
     &       (pwr%v_spectr(i)%kr_inside, pwr%v_spectr(i)%kr_outside,    &
     &        sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, num,            &
     &        WK_pwr%shl_rj(0,1,1), WK_pwr%volume_j(1,1))
!
          call sum_sph_v_rms_by_degree(l_truncation, sph_rj%nidx_rj(2), &
     &        WK_pwr%istack_mode_sum_l,  WK_pwr%item_mode_sum_l,        &
     &        ncomp_rj, WK_pwr%volume_j(1,1),                           &
     &        WK_pwr%vol_l_local(0,jcomp_st,i))
          call sum_sph_v_rms_by_degree(l_truncation, sph_rj%nidx_rj(2), &
     &        WK_pwr%istack_mode_sum_m,  WK_pwr%item_mode_sum_m,        &
     &        ncomp_rj, WK_pwr%volume_j(1,1),                           &
     &        WK_pwr%vol_m_local(0,jcomp_st,i))
          call sum_sph_v_rms_by_degree(l_truncation, sph_rj%nidx_rj(2), &
     &        WK_pwr%istack_mode_sum_lm, WK_pwr%item_mode_sum_lm,       &
     &        ncomp_rj, WK_pwr%volume_j(1,1),                           &
     &        WK_pwr%vol_lm_local(0,jcomp_st,i))
        end do
!
        if(pwr%nri_rms .le. 0) cycle
        call sum_sph_l_rms_by_degree(pwr, l_truncation, sph_rj%nidx_rj, &
     &      WK_pwr%istack_mode_sum_l,  WK_pwr%item_mode_sum_l,          &
     &      ncomp_rj, WK_pwr%shl_rj, WK_pwr%shl_l_local(1,0,jcomp_st))
        call sum_sph_l_rms_by_degree(pwr, l_truncation, sph_rj%nidx_rj, &
     &      WK_pwr%istack_mode_sum_m,  WK_pwr%item_mode_sum_m,          &
     &      ncomp_rj, WK_pwr%shl_rj, WK_pwr%shl_m_local(1,0,jcomp_st))
        call sum_sph_l_rms_by_degree(pwr, l_truncation, sph_rj%nidx_rj, &
     &      WK_pwr%istack_mode_sum_lm, WK_pwr%item_mode_sum_lm,         &
     &      ncomp_rj, WK_pwr%shl_rj, WK_pwr%shl_lm_local(1,0,jcomp_st))
      end do
!
      end subroutine sum_sph_layerd_pwr
!
! -----------------------------------------------------------------------
!
      subroutine sum_sph_layerd_correlate(l_truncation, sph_rj,         &
     &          rj_fld1, rj_fld2, g_sph_rj, pwr, WK_pwr)
!
      use t_spheric_rj_data
      use t_phys_data
      use t_sph_volume_mean_square
      use t_sum_sph_rms_data
      use t_rms_4_sph_spectr
!
      use correlation_by_sph_spectr
      use cal_ave_4_rms_vector_sph
      use radial_int_for_sph_spec
      use sum_sph_rms_by_degree
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld1, rj_fld2
      type(sph_mean_squares), intent(in) :: pwr
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
      integer(kind = kint) :: j_fld, i_fld
      integer(kind = kint) :: icmp_rj, jcomp_st, ncomp_rj
      integer(kind = kint) :: num, i
!
!
!$omp parallel workshare
      WK_pwr%shl_l_local =  zero
      WK_pwr%shl_m_local =  zero
      WK_pwr%shl_lm_local = zero
      WK_pwr%vol_l_local =  zero
      WK_pwr%vol_m_local =  zero
      WK_pwr%vol_lm_local = zero
!$omp end parallel workshare
!
      do j_fld = 1, pwr%num_fld_sq
        i_fld = pwr%id_field(j_fld)
        icmp_rj = rj_fld1%istack_component(i_fld-1) + 1
        jcomp_st = pwr%istack_comp_sq(j_fld-1) + 1
        ncomp_rj = pwr%istack_comp_sq(j_fld)                            &
     &            - pwr%istack_comp_sq(j_fld-1)
        num = sph_rj%nidx_rj(2) * ncomp_rj
        call correlate_sph_spec_one_field(sph_rj, ncomp_rj, g_sph_rj,   &
     &      icmp_rj, rj_fld1%n_point, rj_fld1%ntot_phys, rj_fld1%d_fld, &
     &      icmp_rj, rj_fld2%n_point, rj_fld2%ntot_phys, rj_fld2%d_fld, &
     &      WK_pwr%shl_rj(0,1,1))
!
        do i = 1, pwr%num_vol_spectr
          call radial_integration                                       &
     &       (pwr%v_spectr(i)%kr_inside, pwr%v_spectr(i)%kr_outside,    &
     &        sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, num,            &
     &        WK_pwr%shl_rj(0,1,1), WK_pwr%volume_j(1,1))
!
          call sum_sph_v_rms_by_degree(l_truncation, sph_rj%nidx_rj(2), &
     &        WK_pwr%istack_mode_sum_l,  WK_pwr%item_mode_sum_l,        &
     &        ncomp_rj, WK_pwr%volume_j(1,1),                           &
     &        WK_pwr%vol_l_local(0,jcomp_st,i))
        end do
!
        if(pwr%nri_rms .le. 0) cycle
        call sum_sph_l_rms_by_degree(pwr, l_truncation, sph_rj%nidx_rj, &
     &      WK_pwr%istack_mode_sum_l,  WK_pwr%item_mode_sum_l,          &
     &      ncomp_rj, WK_pwr%shl_rj, WK_pwr%shl_l_local(1,0,jcomp_st))
      end do
!
      end subroutine sum_sph_layerd_correlate
!
! -----------------------------------------------------------------------
!
      end module sum_sph_rms_data
