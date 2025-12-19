!> @file  select_SGS_diff_adv_source.f90
!!      module select_SGS_diff_adv_source
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate time evolution explicitly
!!
!!@verbatim
!!      subroutine sel_scl_diff_adv_SGS_src_adams                       &
!!     &         (iflag_SGS, kr_st, kr_ed, ipol_diffuse, ipol_advect,   &
!!     &          ipol_SGS_advect, ipol_source, ipol_scalar, ipol_pre,  &
!!     &          dt, coef_exp, coef_src, rj_fld)
!!      subroutine sel_scl_diff_adv_SGS_src_elr                         &
!!     &        (iflag_SGS, ist, ied, inod_center, ipol_diffuse,        &
!!     &         ipol_advect, ipol_SGS_advect, ipol_source, ipol_scalar,&
!!     &         dt, coef_exp, coef_adv, coef_src, rj_fld)
!!      subroutine sel_ini_adams_scl_w_src_SGS                          &
!!     &         (iflag_SGS, ist, ied, inod_center,                     &
!!     &          ipol_advect, ipol_SGS_advect, ipol_source, ipol_pre,  &
!!     &          coef_src, rj_fld)
!!        integer(kind = kint), intent(in) :: iflag_SGS
!!        integer(kind = kint), intent(in) :: ist, ied, inod_center
!!        integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
!!        integer(kind = kint), intent(in) :: ipol_SGS_advect
!!        integer(kind = kint), intent(in) :: ipol_source
!!        integer(kind = kint), intent(in) :: ipol_scalar, ipol_pre
!!        real(kind = kreal), intent(in) :: coef_exp, coef_src
!!        real(kind = kreal), intent(in) :: dt
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param kr_st         Radial address for inner boundary
!!@param kr_ed         Radial address for outer boundary
!!@param ipol_diffuse  address for diffusion term
!!@param ipol_advect   address for advection term
!!@param ipol_source   address for source term
!!@param ipol_scalar   address for scalar field to update
!!@param ipol_pre      address for storeing previous evolution
!!@param coef_exp      coeefient for expilict evolution for diffusion
!!@param coef_src      coefficient for source term
!
      module select_SGS_diff_adv_source
!
      use m_precision
      use m_constants
!
      use t_SGS_control_parameter
      use t_phys_data
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sel_scl_diff_adv_SGS_src_adams                         &
     &         (iflag_SGS, ist, ied, inod_center,                       &
     &          ipol_diffuse, ipol_advect, ipol_SGS_advect,             &
     &          ipol_source, ipol_scalar, ipol_pre,                     &
     &          dt, coef_exp, coef_src, rj_fld)
!
      use SGS_diff_adv_src_explicit
      use select_diff_adv_source
!
      integer(kind = kint), intent(in) :: iflag_SGS
      integer(kind = kint), intent(in) :: ist, ied, inod_center
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_SGS_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar, ipol_pre
      real(kind = kreal), intent(in) :: coef_exp, coef_src
      real(kind = kreal), intent(in) :: dt
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(iflag_SGS .gt. id_SGS_none) then
        if(ipol_source .eq. izero) then
          call SGS_scalar_diff_advect_adams                             &
     &       (ist, ied, ipol_diffuse, ipol_advect, ipol_SGS_advect,     &
     &        ipol_scalar, ipol_pre, dt, coef_exp,                      &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        else
          call SGS_scalar_diff_adv_src_adams                            &
     &       (ist, ied, inod_center, ipol_diffuse,                      &
     &        ipol_advect, ipol_SGS_advect, ipol_source,                &
     &        ipol_scalar, ipol_pre, dt, coef_exp, coef_src,            &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
      else
        call sel_scalar_diff_adv_src_adams(ist, ied, inod_center,       &
     &      ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,        &
     &      ipol_pre, dt, coef_exp, coef_src, rj_fld)
      end if
!
      end subroutine sel_scl_diff_adv_SGS_src_adams
!
! ----------------------------------------------------------------------
!
      subroutine sel_scl_diff_adv_SGS_src_elr                           &
     &        (iflag_SGS, ist, ied, inod_center, ipol_diffuse,          &
     &         ipol_advect, ipol_SGS_advect, ipol_source, ipol_scalar,  &
     &         dt, coef_exp, coef_adv, coef_src, rj_fld)
!
      use SGS_diff_adv_src_explicit
      use select_diff_adv_source
!
      integer(kind = kint), intent(in) :: iflag_SGS
      integer(kind = kint), intent(in) :: ist, ied, inod_center
      integer(kind = kint), intent(in) :: ipol_diffuse, ipol_advect
      integer(kind = kint), intent(in) :: ipol_SGS_advect
      integer(kind = kint), intent(in) :: ipol_source
      integer(kind = kint), intent(in) :: ipol_scalar
      real(kind = kreal), intent(in) :: coef_exp, coef_adv, coef_src
      real(kind = kreal), intent(in) :: dt
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(coef_adv .eq. zero) then
        call sel_exp_static_src_euler(ist, ied, inod_center,           &
     &      ipol_source, ipol_scalar, coef_src, rj_fld)
      else if(iflag_SGS .gt. id_SGS_none) then
        if(ipol_source .eq. izero) then
          call SGS_scalar_diff_advect_euler(ist, ied, ipol_diffuse,     &
     &        ipol_advect, ipol_SGS_advect, ipol_scalar, dt, coef_exp,  &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        else
          call SGS_scalar_diff_adv_src_euler(ist, ied, inod_center,     &
     &        ipol_diffuse, ipol_advect, ipol_SGS_advect, ipol_source,  &
     &        ipol_scalar, dt, coef_exp, coef_src,                      &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
      else
        call sel_exp_scl_diff_adv_src_euler(ist, ied, inod_center,      &
     &      ipol_diffuse, ipol_advect, ipol_source, ipol_scalar,        &
     &      dt, coef_exp, coef_src, rj_fld)
      end if
!
      end subroutine sel_scl_diff_adv_SGS_src_elr
!
! ----------------------------------------------------------------------
!
      subroutine sel_ini_adams_scl_w_src_SGS                            &
     &         (iflag_SGS, ist, ied, inod_center,                       &
     &          ipol_advect, ipol_SGS_advect, ipol_source, ipol_pre,    &
     &          coef_src, rj_fld)
!
      use SGS_diff_adv_src_explicit
      use select_diff_adv_source
!
      integer(kind = kint), intent(in) :: iflag_SGS
      integer(kind = kint), intent(in) :: ist, ied, inod_center
      integer(kind = kint), intent(in) :: ipol_advect, ipol_source
      integer(kind = kint), intent(in) :: ipol_SGS_advect
      integer(kind = kint), intent(in) :: ipol_pre
      real(kind = kreal), intent(in) :: coef_src
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(iflag_SGS .gt. id_SGS_none) then
        if(ipol_source .eq. izero) then
          call SGS_ini_adams_scalar                                     &
     &       (ist, ied, ipol_advect, ipol_SGS_advect, ipol_pre,         &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        else
          call SGS_ini_adams_scalar_w_src                               &
     &       (ist, ied, inod_center, ipol_advect,                       &
     &        ipol_SGS_advect, ipol_source, ipol_pre, coef_src,         &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
      else
        call sel_ini_adams_scalar_w_src(ist, ied, inod_center,          &
     &      ipol_advect, ipol_source, ipol_pre, coef_src, rj_fld)
      end if
!
      end subroutine sel_ini_adams_scl_w_src_SGS
!
! ----------------------------------------------------------------------
!
      end module select_SGS_diff_adv_source
