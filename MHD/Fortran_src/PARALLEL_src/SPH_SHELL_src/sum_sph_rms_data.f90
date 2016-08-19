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
!!      subroutine sum_sph_layerd_rms(kg_st, kg_ed, l_truncation,       &
!!     &       sph_rj, ipol, g_sph_rj, rj_fld, nri_rms, num_rms_rj,     &
!!     &       ntot_rms_rj, istack_rms_comp_rj, ifield_rms_rj,          &
!!     &       istack_mode_sum_l, istack_mode_sum_m, istack_mode_sum_lm,&
!!     &       item_mode_sum_l, item_mode_sum_m, item_mode_sum_lm,      &
!!     &       kr_for_rms, rms_sph_rj, rms_sph_vol_j,                   &
!!     &       rms_sph_l_local, rms_sph_m_local, rms_sph_lm_local,      &
!!     &       rms_sph_vl_local, rms_sph_vm_local, rms_sph_vlm_local)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!@endverbatim
!
      module sum_sph_rms_data
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: sum_sph_v_rms_by_degree, sum_sph_rms_by_degree
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
      subroutine sum_sph_layerd_rms(kg_st, kg_ed, l_truncation,         &
     &       sph_rj, ipol, g_sph_rj, rj_fld, nri_rms, num_rms_rj,       &
     &       ntot_rms_rj, istack_rms_comp_rj, ifield_rms_rj,            &
     &       istack_mode_sum_l, istack_mode_sum_m, istack_mode_sum_lm,  &
     &       item_mode_sum_l, item_mode_sum_m, item_mode_sum_lm,        &
     &       kr_for_rms, rms_sph_rj, rms_sph_vol_j,                     &
     &       rms_sph_l_local, rms_sph_m_local, rms_sph_lm_local,        &
     &       rms_sph_vl_local, rms_sph_vm_local, rms_sph_vlm_local)
!
      use t_spheric_rj_data
      use t_phys_data
!
      use cal_rms_by_sph_spectr
      use cal_ave_4_rms_vector_sph
      use radial_int_for_sph_spec
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: kg_st, kg_ed
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nri_rms
      integer(kind = kint), intent(in) :: num_rms_rj, ntot_rms_rj
      integer(kind = kint), intent(in)                                  &
     &            :: istack_rms_comp_rj(0:num_rms_rj)
      integer(kind = kint), intent(in) :: ifield_rms_rj(num_rms_rj)
      integer(kind = kint), intent(in) :: kr_for_rms(nri_rms)
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      integer(kind = kint), intent(in)                                  &
     &           :: istack_mode_sum_l(-1:l_truncation)
      integer(kind = kint), intent(in)                                  &
     &           :: istack_mode_sum_m(-1:l_truncation)
      integer(kind = kint), intent(in)                                  &
     &           :: istack_mode_sum_lm(-1:l_truncation)
!
      integer(kind = kint), intent(in)                                  &
     &           :: item_mode_sum_l(sph_rj%nidx_rj(2))
      integer(kind = kint), intent(in)                                  &
     &           :: item_mode_sum_m(sph_rj%nidx_rj(2))
      integer(kind = kint), intent(in)                                  &
     &           :: item_mode_sum_lm(sph_rj%nidx_rj(2))
!
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_sph_rj(0:sph_rj%nidx_rj(1),sph_rj%nidx_rj(2),3)
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_sph_vol_j(sph_rj%nidx_rj(2),3)
!
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_sph_l_local(nri_rms,0:l_truncation,ntot_rms_rj)
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_sph_m_local(nri_rms,0:l_truncation,ntot_rms_rj)
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_sph_lm_local(nri_rms,0:l_truncation,ntot_rms_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_sph_vl_local(0:l_truncation,ntot_rms_rj)
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_sph_vm_local(0:l_truncation,ntot_rms_rj)
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_sph_vlm_local(0:l_truncation,ntot_rms_rj)
!
      integer(kind = kint) :: j_fld, i_fld
      integer(kind = kint) :: icomp_rj, jcomp_st, ncomp_rj
      integer(kind = kint) :: num
!
!
!$omp parallel workshare
      rms_sph_l_local =   zero
      rms_sph_m_local =   zero
      rms_sph_lm_local =  zero
      rms_sph_vl_local =  zero
      rms_sph_vm_local =  zero
      rms_sph_vlm_local = zero
!$omp end parallel workshare
!
      do j_fld = 1, num_rms_rj
        i_fld = ifield_rms_rj(j_fld)
        icomp_rj = rj_fld%istack_component(i_fld-1) + 1
        jcomp_st = istack_rms_comp_rj(j_fld-1) + 1
        ncomp_rj = istack_rms_comp_rj(j_fld)                            &
     &            - istack_rms_comp_rj(j_fld-1)
        num = sph_rj%nidx_rj(2) * ncomp_rj
        call cal_rms_sph_spec_one_field(sph_rj, ipol,                   &
     &      ncomp_rj, icomp_rj, g_sph_rj, rj_fld%n_point,               &
     &      rj_fld%ntot_phys, rj_fld%d_fld, rms_sph_rj(0,1,1))
        call radial_integration(kg_st, kg_ed, sph_rj%nidx_rj(1),        &
     &      sph_rj%radius_1d_rj_r, num,                                 &
     &      rms_sph_rj(0,1,1), rms_sph_vol_j(1,1))
!
        call sum_sph_v_rms_by_degree(l_truncation, sph_rj%nidx_rj(2),   &
     &      istack_mode_sum_l,  item_mode_sum_l,  ncomp_rj,             &
     &      rms_sph_vol_j, rms_sph_vl_local(0,jcomp_st))
        call sum_sph_v_rms_by_degree(l_truncation, sph_rj%nidx_rj(2),   &
     &      istack_mode_sum_m,  item_mode_sum_m,  ncomp_rj,             &
     &      rms_sph_vol_j, rms_sph_vm_local(0,jcomp_st))
        call sum_sph_v_rms_by_degree(l_truncation, sph_rj%nidx_rj(2),   &
     &      istack_mode_sum_lm, item_mode_sum_lm, ncomp_rj,             &
     &      rms_sph_vol_j, rms_sph_vlm_local(0,jcomp_st))
!
        if(nri_rms .le. 0) cycle
        call sum_sph_rms_by_degree(l_truncation, sph_rj%nidx_rj,        &
     &      nri_rms, kr_for_rms, istack_mode_sum_l,  item_mode_sum_l,   &
     &      ncomp_rj, rms_sph_rj, rms_sph_l_local(1,0,jcomp_st))
        call sum_sph_rms_by_degree(l_truncation, sph_rj%nidx_rj,        &
     &      nri_rms, kr_for_rms, istack_mode_sum_m,  item_mode_sum_m,   &
     &      ncomp_rj, rms_sph_rj, rms_sph_m_local(1,0,jcomp_st))
        call sum_sph_rms_by_degree(l_truncation, sph_rj%nidx_rj,        &
     &      nri_rms, kr_for_rms, istack_mode_sum_lm, item_mode_sum_lm,  &
     &      ncomp_rj, rms_sph_rj, rms_sph_lm_local(1,0,jcomp_st))
      end do
!
      end subroutine sum_sph_layerd_rms
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sum_sph_v_rms_by_degree(ltr, nidx_j, istack_sum,       &
     &          item_mode_4_sum, ncomp, rms_sph_vol_j, rms_sph_vlc)
!
      integer(kind = kint), intent(in) :: ltr, nidx_j
      integer(kind = kint), intent(in) :: ncomp
!
      integer(kind = kint), intent(in) :: istack_sum(-1:ltr)
      integer(kind = kint), intent(in) :: item_mode_4_sum(nidx_j)
      real(kind = kreal), intent(in) :: rms_sph_vol_j(nidx_j,3)
!
      real(kind = kreal), intent(inout) :: rms_sph_vlc(0:ltr,ncomp)
!
      integer(kind = kint) :: lm, j, l0, icomp
      integer(kind = kint) :: lst, led
!
!
!$omp parallel private(icomp)
      do icomp = 1, ncomp
!$omp do private(lm,lst,led,l0,j)
        do lm = 0, ltr
          lst = istack_sum(lm-1) + 1
          led = istack_sum(lm)
          do l0 = lst, led
            j = item_mode_4_sum(l0)
            rms_sph_vlc(lm,icomp) = rms_sph_vlc(lm,icomp)               &
     &                                + rms_sph_vol_j(j,icomp)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine sum_sph_v_rms_by_degree
!
! -----------------------------------------------------------------------
!
      subroutine sum_sph_rms_by_degree(ltr, nidx_rj, nri_rms,           &
     &          kr_for_rms, istack_sum, item_mode_4_sum, ncomp,         &
     &          rms_sph_rj, rms_sph_lc)
!
      integer(kind = kint), intent(in) :: ltr, nri_rms
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: kr_for_rms(nri_rms)
      integer(kind = kint), intent(in) :: ncomp
!
      integer(kind = kint), intent(in) :: istack_sum(-1:ltr)
      integer(kind = kint), intent(in) :: item_mode_4_sum(nidx_rj(2))
      real(kind = kreal), intent(in)                                    &
     &                   :: rms_sph_rj(0:nidx_rj(1),nidx_rj(2),3)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: rms_sph_lc(nri_rms,0:ltr,ncomp)
!
      integer(kind = kint) :: lm, k, kg, j, l0, icomp
      integer(kind = kint) :: lst, led
!
!
!$omp parallel private(icomp)
      do icomp = 1, ncomp
!$omp do private(k,kg,lm,lst,led,l0,j)
        do k = 1, nri_rms
          kg = kr_for_rms(k)
          do lm = 0, ltr
            lst = istack_sum(lm-1) + 1
            led = istack_sum(lm)
            do l0 = lst, led
              j = item_mode_4_sum(l0)
              rms_sph_lc(k,lm,icomp) = rms_sph_lc(k,lm,icomp)           &
     &                                + rms_sph_rj(kg,j,icomp)
            end do
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine sum_sph_rms_by_degree
!
! -----------------------------------------------------------------------
!
      end module sum_sph_rms_data
