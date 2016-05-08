!set_item_4_sph_groups.f90
!      module set_item_4_sph_groups
!
!     Written by H. Matsui on July, 2007
!
!      subroutine set_item_rtp_radial_grp
!      subroutine set_item_rj_radial_grp
!      subroutine set_item_rj_spectr_grp
!
      module set_item_4_sph_groups
!
      use m_precision
      use m_constants
!
      use t_spheric_rtp_data
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_spheric_rj_data
      use t_group_data
!
      implicit none
!
      private :: set_item_sph_grp_by_list, set_item_sph_grp_by_rng
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_item_rtp_radial_grp(sph_rtp, radial_rtp_grp)
!
      use m_spheric_parameter
      use m_sph_1d_global_index
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(group_data), intent(inout) :: radial_rtp_grp
!
      integer(kind = kint) :: icou, nlayer_ed, inum
!
      icou = 0
      call set_item_sph_grp_by_list(icou, nlayer_ICB, nlayer_ICB,       &
     &    sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                 &
     &    radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
!
      call set_item_sph_grp_by_list(icou, nlayer_CMB, nlayer_CMB,       &
     &    sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                 &
     &    radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
!
      call set_item_sph_grp_by_list(icou,                               &
     &    nlayer_2_center, nlayer_2_center,                             &
     &    sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                 &
     &    radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
!
      if (sph_rtp%nidx_global_rtp(1) .gt. nlayer_CMB) then
        call set_item_sph_grp_by_list(icou,                             &
     &      sph_rtp%nidx_global_rtp(1), sph_rtp%nidx_global_rtp(1),     &
     &      sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,               &
     &      radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
      end if
!
      if (nlayer_mid_OC .gt. 0) then
        call set_item_sph_grp_by_list(icou,                             &
     &      nlayer_mid_OC, nlayer_mid_OC,                               &
     &      sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,               &
     &      radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
      end if
!
      nlayer_ed = nlayer_ICB-1
      call set_item_sph_grp_by_list(icou, nlayer_2_center, nlayer_ed,   &
     &    sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                 &
     &    radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
!
      call set_item_sph_grp_by_list(icou, nlayer_ICB, nlayer_CMB,       &
     &    sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                 &
     &    radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
!
!
      do inum = 1, numlayer_sph_bc
        call set_item_sph_grp_by_list(icou,                             &
     &      kr_sph_boundary(inum), kr_sph_boundary(inum),               &
     &      sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,               &
     &      radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
      end do
!
      end subroutine set_item_rtp_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_item_rj_radial_grp(sph_rj, radial_rj_grp)
!
      use m_spheric_parameter
      use m_sph_1d_global_index
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(inout) :: radial_rj_grp
!
      integer(kind = kint) :: icou, nlayer_ed, inum
!
!
      icou = 0
      call set_item_sph_grp_by_rng(icou, nlayer_ICB, nlayer_ICB,        &
     &    sph_rj%ist_rj(1), sph_rj%ied_rj(1),                           &
     &    radial_rj_grp%num_item, radial_rj_grp%item_grp)
!
      call set_item_sph_grp_by_rng(icou, nlayer_CMB, nlayer_CMB,        &
     &    sph_rj%ist_rj(1), sph_rj%ied_rj(1),                           &
     &    radial_rj_grp%num_item, radial_rj_grp%item_grp)
!
      call set_item_sph_grp_by_rng(icou,                                &
     &    nlayer_2_center, nlayer_2_center,                             &
     &    sph_rj%ist_rj(1), sph_rj%ied_rj(1),                           &
     &    radial_rj_grp%num_item, radial_rj_grp%item_grp)
!
      if (sph_rtp1%nidx_global_rtp(1) .gt. nlayer_CMB) then
        call set_item_sph_grp_by_rng(icou, sph_rtp1%nidx_global_rtp(1), &
     &      sph_rtp1%nidx_global_rtp(1), sph_rj%ist_rj(1),              &
     &      sph_rj%ied_rj(1), radial_rj_grp%num_item,                   &
     &      radial_rj_grp%item_grp)
      end if
!
      if (nlayer_mid_OC .gt. 0) then
        call set_item_sph_grp_by_rng(icou,                              &
     &      nlayer_mid_OC, nlayer_mid_OC,                               &
     &      sph_rj%ist_rj(1), sph_rj%ied_rj(1),                         &
     &      radial_rj_grp%num_item, radial_rj_grp%item_grp)
      end if
!
      nlayer_ed = nlayer_ICB-1
      call set_item_sph_grp_by_rng(icou, nlayer_2_center, nlayer_ed,    &
     &    sph_rj%ist_rj(1), sph_rj%ied_rj(1),                           &
     &    radial_rj_grp%num_item, radial_rj_grp%item_grp)
!
      call set_item_sph_grp_by_rng(icou, nlayer_ICB, nlayer_CMB,        &
     &    sph_rj%ist_rj(1), sph_rj%ied_rj(1),                           &
     &    radial_rj_grp%num_item, radial_rj_grp%item_grp)
!
      do inum = 1, numlayer_sph_bc
        call set_item_sph_grp_by_rng(icou,                              &
     &      kr_sph_boundary(inum), kr_sph_boundary(inum),               &
     &      sph_rj%ist_rj(1), sph_rj%ied_rj(1),                         &
     &      radial_rj_grp%num_item, radial_rj_grp%item_grp)
      end do
!
      end subroutine set_item_rj_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_item_rj_spectr_grp(sph_rj, sphere_rj_grp)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(inout) :: sphere_rj_grp
!
      integer(kind = kint) :: icou
!
      icou = 0
      call set_item_sph_grp_by_rng(icou, izero, izero,                  &
     &    sph_rj%ist_rj(2), sph_rj%ied_rj(2),                           &
     &    sphere_rj_grp%num_item, sphere_rj_grp%item_grp)
!
      call set_item_sph_grp_by_rng(icou, ione, ione,                    &
     &    sph_rj%ist_rj(2), sph_rj%ied_rj(2),                           &
     &    sphere_rj_grp%num_item, sphere_rj_grp%item_grp)
!
      call set_item_sph_grp_by_rng(icou, itwo, itwo,                    &
     &    sph_rj%ist_rj(2), sph_rj%ied_rj(2),                           &
     &    sphere_rj_grp%num_item, sphere_rj_grp%item_grp)
!
      call set_item_sph_grp_by_rng(icou, ithree, ithree,                &
     &    sph_rj%ist_rj(2), sph_rj%ied_rj(2),                           &
     &    sphere_rj_grp%num_item, sphere_rj_grp%item_grp)
!
      end subroutine set_item_rj_spectr_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_item_sph_grp_by_list(icou, nlayer_st, nlayer_ed,   &
     &          nidx, idx_gl_1d, ntot_grp, item_grp)
!
      integer(kind = kint), intent(in) :: nlayer_st, nlayer_ed
      integer(kind = kint), intent(in) :: nidx, idx_gl_1d(nidx)
      integer(kind = kint), intent(in) :: ntot_grp
!
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: item_grp(ntot_grp)
!
      integer(kind = kint) :: j
!
!
      if (nlayer_st .ge. 0) then
        do j = 1, nidx
          if (idx_gl_1d(j) .ge. nlayer_st                               &
     &      .and. idx_gl_1d(j) .le. nlayer_ed ) then
            icou = icou + 1
            item_grp(icou) = j
          end if
        end do
      end if
!
      end subroutine set_item_sph_grp_by_list
!
! ----------------------------------------------------------------------
!
      subroutine set_item_sph_grp_by_rng(icou, nlayer_st, nlayer_ed,    &
     &          ist_domain, ied_domain, ntot_grp, item_grp)
!
      integer(kind = kint), intent(in) :: nlayer_st, nlayer_ed
      integer(kind = kint), intent(in) :: ist_domain, ied_domain
      integer(kind = kint), intent(in) :: ntot_grp
!
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: item_grp(ntot_grp)
!
      integer(kind = kint) :: j
!
!
      if (nlayer_st .ge. 0) then
        do j = nlayer_st, nlayer_ed
          if (j.ge.ist_domain .and. j.le.ied_domain ) then
            icou = icou + 1
            item_grp(icou) = j - ist_domain + 1
          end if
        end do
      end if
!
      end subroutine set_item_sph_grp_by_rng
!
! ----------------------------------------------------------------------
!
      end module set_item_4_sph_groups
