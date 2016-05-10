!>@file   set_stack_4_sph_groups.f90
!!@brief  module set_stack_4_sph_groups
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Make group informations for spherical shell model
!!
!!
!!@verbatim
!!      subroutine set_stack_rtp_radial_grp
!!      subroutine set_stack_rj_radial_grp
!!      subroutine set_stack_rj_spectr_grp
!!@endverbatim
!
      module set_stack_4_sph_groups
!
      use m_precision
      use m_constants
!
      implicit none
!
      character(len = kchara), parameter :: name_mid =     'Mid_OC'
      character(len = kchara), parameter :: name_ED = 'Outmost_of_Shell'
!
      character(len = kchara), parameter :: name_y00 =  'Y_0_0'
      character(len = kchara), parameter :: name_y10 =  'Y_1_0'
      character(len = kchara), parameter :: name_y11s = 'Y_1_1s'
      character(len = kchara), parameter :: name_y11c = 'Y_1_1c'
!
      private :: name_mid
      private :: name_y00, name_y10, name_y11s, name_y11c
      private :: set_stack_sph_grp_by_list, set_stack_sph_grp_by_rng
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_stack_rtp_radial_grp
!
      use m_sph_1d_global_index
      use m_spheric_parameter
      use m_group_data_sph_specr
!
      integer(kind = kint) :: icou, nlayer_ed, inum
!
!
      icou = 0
      call set_stack_sph_grp_by_list(icou, nlayer_ICB, nlayer_ICB,      &
     &    sph_rtp1%nidx_rtp(1), sph_rtp1%idx_gl_1d_rtp_r,               &
     &    ICB_nod_grp_name, radial_rtp_grp1%num_grp,                    &
     &    radial_rtp_grp1%istack_grp, radial_rtp_grp1%grp_name)
!
      call set_stack_sph_grp_by_list(icou, nlayer_CMB, nlayer_CMB,      &
     &    sph_rtp1%nidx_rtp(1), sph_rtp1%idx_gl_1d_rtp_r,               &
     &    CMB_nod_grp_name, radial_rtp_grp1%num_grp,                    &
     &    radial_rtp_grp1%istack_grp, radial_rtp_grp1%grp_name)
!
      call set_stack_sph_grp_by_list(icou,                              &
     &    sph_param1%nlayer_2_center, sph_param1%nlayer_2_center,       &
     &    sph_rtp1%nidx_rtp(1), sph_rtp1%idx_gl_1d_rtp_r,               &
     &    CTR_nod_grp_name, radial_rtp_grp1%num_grp,                    &
     &    radial_rtp_grp1%istack_grp, radial_rtp_grp1%grp_name)
!
      if (sph_rtp1%nidx_global_rtp(1) .gt. nlayer_CMB) then
        call set_stack_sph_grp_by_list(icou,                            &
     &      sph_rtp1%nidx_global_rtp(1), sph_rtp1%nidx_global_rtp(1),   &
     &      sph_rtp1%nidx_rtp(1), sph_rtp1%idx_gl_1d_rtp_r, name_ED,    &
     &      radial_rtp_grp1%num_grp, radial_rtp_grp1%istack_grp,        &
     &      radial_rtp_grp1%grp_name)
      end if
!
      if (sph_param1%nlayer_mid_OC .gt. 0) then
        call set_stack_sph_grp_by_list(icou,                            &
     &     sph_param1%nlayer_mid_OC, sph_param1%nlayer_mid_OC,          &
     &     sph_rtp1%nidx_rtp(1), sph_rtp1%idx_gl_1d_rtp_r,              &
     &     name_mid, radial_rtp_grp1%num_grp,                           &
     &     radial_rtp_grp1%istack_grp, radial_rtp_grp1%grp_name)
      end if
!
      nlayer_ed = nlayer_ICB-1
      call set_stack_sph_grp_by_list                                    &
     &   (icou, sph_param1%nlayer_2_center, nlayer_ed,                  &
     &    sph_rtp1%nidx_rtp(1), sph_rtp1%idx_gl_1d_rtp_r,               &
     &    IC_ele_grp_name, radial_rtp_grp1%num_grp,                     &
     &    radial_rtp_grp1%istack_grp, radial_rtp_grp1%grp_name)
!
      call set_stack_sph_grp_by_list(icou, nlayer_ICB, nlayer_CMB,      &
     &    sph_rtp1%nidx_rtp(1), sph_rtp1%idx_gl_1d_rtp_r,               &
     &    OC_ele_grp_name, radial_rtp_grp1%num_grp,                     &
     &    radial_rtp_grp1%istack_grp, radial_rtp_grp1%grp_name)
!
!
      do inum = 1, numlayer_sph_bc
        call set_stack_sph_grp_by_list(icou,                            &
     &      kr_sph_boundary(inum), kr_sph_boundary(inum),               &
     &      sph_rtp1%nidx_rtp(1), sph_rtp1%idx_gl_1d_rtp_r,             &
     &      sph_bondary_name(inum), radial_rtp_grp1%num_grp,            &
     &      radial_rtp_grp1%istack_grp, radial_rtp_grp1%grp_name)
      end do
!
      radial_rtp_grp1%num_item                                          &
     &      = radial_rtp_grp1%istack_grp(radial_rtp_grp1%num_grp)
!
      end subroutine set_stack_rtp_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_stack_rj_radial_grp
!
      use m_sph_1d_global_index
      use m_spheric_parameter
      use m_group_data_sph_specr
!
      integer(kind = kint) :: icou, nlayer_ed, inum
!
!
      icou = 0
      call set_stack_sph_grp_by_rng(icou, nlayer_ICB, nlayer_ICB,       &
     &    sph_rj1%ist_rj(1), sph_rj1%ied_rj(1), ICB_nod_grp_name,       &
     &    radial_rj_grp1%num_grp, radial_rj_grp1%istack_grp,            &
     &    radial_rj_grp1%grp_name)
!
      call set_stack_sph_grp_by_rng(icou, nlayer_CMB, nlayer_CMB,       &
     &    sph_rj1%ist_rj(1), sph_rj1%ied_rj(1), CMB_nod_grp_name,       &
     &    radial_rj_grp1%num_grp, radial_rj_grp1%istack_grp,            &
     &    radial_rj_grp1%grp_name)
!
      call set_stack_sph_grp_by_rng                                     &
     &   (icou, sph_param1%nlayer_2_center, sph_param1%nlayer_2_center, &
     &    sph_rj1%ist_rj(1), sph_rj1%ied_rj(1), CTR_nod_grp_name,       &
     &    radial_rj_grp1%num_grp, radial_rj_grp1%istack_grp,            &
     &    radial_rj_grp1%grp_name)
!
      if (sph_rtp1%nidx_global_rtp(1) .gt. nlayer_CMB) then
        call set_stack_sph_grp_by_rng                                   &
     &     (icou, sph_rtp1%nidx_global_rtp(1),                          &
     &      sph_rtp1%nidx_global_rtp(1), sph_rj1%ist_rj(1),             &
     &      sph_rj1%ied_rj(1), name_ED, radial_rj_grp1%num_grp,         &
     &     radial_rj_grp1%istack_grp, radial_rj_grp1%grp_name)
      end if
!
      if (sph_param1%nlayer_mid_OC .gt. 0) then
        call set_stack_sph_grp_by_rng                                   &
     &    (icou, sph_param1%nlayer_mid_OC, sph_param1%nlayer_mid_OC,    &
     &     sph_rj1%ist_rj(1), sph_rj1%ied_rj(1), name_mid,              &
     &     radial_rj_grp1%num_grp, radial_rj_grp1%istack_grp,           &
     &     radial_rj_grp1%grp_name)
      end if
!
      nlayer_ed = nlayer_ICB-1
      call set_stack_sph_grp_by_rng                                     &
     &   (icou, sph_param1%nlayer_2_center, nlayer_ed,                  &
     &    sph_rj1%ist_rj(1), sph_rj1%ied_rj(1), IC_ele_grp_name,        &
     &    radial_rj_grp1%num_grp, radial_rj_grp1%istack_grp,            &
     &    radial_rj_grp1%grp_name)
!
      call set_stack_sph_grp_by_rng(icou, nlayer_ICB, nlayer_CMB,       &
     &    sph_rj1%ist_rj(1), sph_rj1%ied_rj(1), OC_ele_grp_name,        &
     &    radial_rj_grp1%num_grp, radial_rj_grp1%istack_grp,            &
     &    radial_rj_grp1%grp_name)
!
      do inum = 1, numlayer_sph_bc
        call set_stack_sph_grp_by_rng                                   &
     &     (icou, kr_sph_boundary(inum), kr_sph_boundary(inum),         &
     &      sph_rj1%ist_rj(1), sph_rj1%ied_rj(1),                       &
     &      sph_bondary_name(inum), radial_rj_grp1%num_grp,             &
     &      radial_rj_grp1%istack_grp, radial_rj_grp1%grp_name)
      end do
!
      radial_rj_grp1%num_item                                           &
     &      = radial_rj_grp1%istack_grp(radial_rj_grp1%num_grp)
!
      end subroutine set_stack_rj_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_stack_rj_spectr_grp
!
      use m_spheric_parameter
      use m_group_data_sph_specr
!
      integer(kind = kint) :: icou
!
      icou = 0
      call set_stack_sph_grp_by_rng(icou, izero, izero,                 &
     &    sph_rj1%ist_rj(2), sph_rj1%ied_rj(2),                         &
     &    name_y00, sphere_rj_grp1%num_grp,                             &
     &    sphere_rj_grp1%istack_grp, sphere_rj_grp1%grp_name)
!
      call set_stack_sph_grp_by_rng(icou, ione, ione,                   &
     &    sph_rj1%ist_rj(2), sph_rj1%ied_rj(2),                         &
     &    name_y11s, sphere_rj_grp1%num_grp,                            &
     &    sphere_rj_grp1%istack_grp, sphere_rj_grp1%grp_name)
!
      call set_stack_sph_grp_by_rng(icou, itwo, itwo,                   &
     &    sph_rj1%ist_rj(2), sph_rj1%ied_rj(2),                         &
     &    name_y10, sphere_rj_grp1%num_grp,                             &
     &    sphere_rj_grp1%istack_grp, sphere_rj_grp1%grp_name)
!
      call set_stack_sph_grp_by_rng(icou, ithree, ithree,               &
     &    sph_rj1%ist_rj(2), sph_rj1%ied_rj(2),                         &
     &     name_y11c, sphere_rj_grp1%num_grp,                           &
     &    sphere_rj_grp1%istack_grp, sphere_rj_grp1%grp_name)
!
      sphere_rj_grp1%num_item                                           &
     &     = sphere_rj_grp1%istack_grp(sphere_rj_grp1%num_grp)
!
      end subroutine set_stack_rj_spectr_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_stack_sph_grp_by_list(icou, nlayer_st, nlayer_ed,  &
     &          nidx, idx_gl_1d, grp_name, num_grp, istack_grp,         &
     &          name_grp)
!
      integer(kind = kint), intent(in) :: nlayer_st, nlayer_ed
      integer(kind = kint), intent(in) :: nidx, idx_gl_1d(nidx)
      integer(kind = kint), intent(in) :: num_grp
      character(len = kchara), intent(in) :: grp_name
!
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: istack_grp(0:num_grp)
      character(len = kchara), intent(inout) :: name_grp(num_grp)
!
      integer(kind = kint) :: j
!
!
      if (nlayer_st .ge. 0) then
        icou = icou + 1
        name_grp(icou) =   grp_name
        istack_grp(icou) = istack_grp(icou-1)
        do j = 1, nidx
          if (idx_gl_1d(j) .ge. nlayer_st                               &
     &      .and. idx_gl_1d(j) .le. nlayer_ed ) then
            istack_grp(icou) = istack_grp(icou) + 1
          end if
        end do
      end if
!
      end subroutine set_stack_sph_grp_by_list
!
! -----------------------------------------------------------------------
!
      subroutine set_stack_sph_grp_by_rng(icou, nlayer_st, nlayer_ed,   &
     &          ist_domain, ied_domain, grp_name, num_grp, istack_grp,  &
     &          name_grp)
!
      integer(kind = kint), intent(in) :: nlayer_st, nlayer_ed
      integer(kind = kint), intent(in) :: ist_domain, ied_domain
      integer(kind = kint), intent(in) :: num_grp
      character(len = kchara), intent(in) :: grp_name
!
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: istack_grp(0:num_grp)
      character(len = kchara), intent(inout) :: name_grp(num_grp)
!
      integer(kind = kint) :: j
!
!
      if (nlayer_st .ge. 0) then
        icou = icou + 1
        name_grp(icou) =   grp_name
        istack_grp(icou) = istack_grp(icou-1)
        do j = nlayer_st, nlayer_ed
          if (j.ge.ist_domain .and. j.le.ied_domain ) then
            istack_grp(icou) = istack_grp(icou) + 1
          end if
        end do
      end if
!
      end subroutine set_stack_sph_grp_by_rng
!
! -----------------------------------------------------------------------
!
      end module set_stack_4_sph_groups
