!>@file   cal_rot_r_const_buo_sph.f90
!!@brief  module cal_rot_r_const_buo_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate rotation of buoyancy under constant radial gravity
!!
!!@verbatim
!!      subroutine cal_rot_radial_const_gravity(sph_bc_U, rj_fld)
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic velocity
!!                 boundary condition parameters
!
      module cal_rot_r_const_buo_sph
!
      use m_precision
!
      use m_constants
      use m_control_parameter
      use m_physical_property
      use m_sph_phys_address
!
      implicit  none
!
      private :: cal_rot_double_cst_buo_sph
      private :: cal_rot_cst_buo_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_rot_radial_const_gravity(sph_bc_U, rj_fld)
!
      use m_machine_parameter
      use m_spheric_parameter
!
      use t_phys_data
      use t_boundary_params_sph_MHD
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      if ((iflag_4_gravity*iflag_4_composit_buo) .gt. id_turn_OFF) then
!
        if (iflag_debug.eq.1)                                           &
     &      write(*,*)'cal_rot_double_cst_buo_sph', ipol%i_temp
          call cal_rot_double_cst_buo_sph                               &
     &       (sph_bc_U%kr_in, sph_bc_U%kr_out, coef_buo, ipol%i_temp,   &
     &        coef_comp_buo, ipol%i_light, itor%i_rot_buoyancy,         &
     &        nidx_rj, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if ( iflag_4_gravity .gt. id_turn_OFF) then
!
        if (iflag_debug.eq.1) write(*,*) 'cal_rot_cst_buo_sph'
        call cal_rot_cst_buo_sph(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &      coef_buo, ipol%i_temp, itor%i_rot_buoyancy,                 &
     &      nidx_rj, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if ( iflag_4_composit_buo .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_rot_cst_buo_sph'
        call cal_rot_cst_buo_sph(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &      coef_comp_buo, ipol%i_light, itor%i_rot_comp_buo,           &
     &      nidx_rj, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if (iflag_4_filter_gravity .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_rot_cst_buo_sph'
        call cal_rot_cst_buo_sph(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &      coef_buo, ipol%i_filter_temp, itor%i_rot_filter_buo,        &
     &      nidx_rj, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine cal_rot_radial_const_gravity
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_rot_double_cst_buo_sph(kr_in, kr_out,              &
     &          coef_t_buo, is_t, coef_c_buo, is_c, it_res,             &
     &          nidx_rj, nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_t, is_c
      integer(kind= kint), intent(in) :: it_res
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: coef_t_buo, coef_c_buo
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d_rj(inod,it_res) =  (coef_t_buo * d_rj(inod,is_t)              &
     &                      + coef_c_buo * d_rj(inod,is_c))
      end do
!$omp end parallel do
!
      end subroutine cal_rot_double_cst_buo_sph
!
!-----------------------------------------------------------------------
!
      subroutine cal_rot_cst_buo_sph(kr_in, kr_out, coef,               &
     &          is_fld, it_res, nidx_rj, nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_fld, it_res
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
        d_rj(inod,it_res) =  coef * d_rj(inod,is_fld)
      end do
!$omp end parallel do
!
      end subroutine cal_rot_cst_buo_sph
!
!-----------------------------------------------------------------------
!
      end module cal_rot_r_const_buo_sph
