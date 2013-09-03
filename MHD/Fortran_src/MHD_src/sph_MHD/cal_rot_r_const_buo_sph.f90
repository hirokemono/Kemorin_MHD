!>@file   cal_rot_r_const_buo_sph.f90
!!@brief  module cal_rot_r_const_buo_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate rotation of buoyancy under constant radial gravity
!!
!!@verbatim
!!      subroutine cal_rot_radial_const_gravity
!!@endverbatim
!
      module cal_rot_r_const_buo_sph
!
      use m_precision
!
      use m_constants
      use m_control_parameter
      use m_spheric_parameter
      use m_physical_property
      use m_sph_spectr_data
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
      subroutine cal_rot_radial_const_gravity
!
      use m_machine_parameter
!
!
      if ((iflag_4_gravity*iflag_4_composit_buo) .gt. id_turn_OFF) then
!
        if (iflag_debug.eq.1)                                           &
     &      write(*,*)'cal_rot_double_cst_buo_sph', ipol%i_temp
          call cal_rot_double_cst_buo_sph(ipol%i_temp)
!
      else if ( iflag_4_gravity .gt. id_turn_OFF) then
!
        if (iflag_debug.eq.1) write(*,*) 'cal_rot_cst_buo_sph'
        call cal_rot_cst_buo_sph(coef_buo,                              &
     &      ipol%i_temp, itor%i_rot_buoyancy)
!
      else if ( iflag_4_composit_buo .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_rot_cst_buo_sph'
        call cal_rot_cst_buo_sph(coef_comp_buo, ipol%i_light,           &
     &      itor%i_rot_comp_buo)
!
      else if (iflag_4_filter_gravity .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_rot_cst_buo_sph'
        call cal_rot_cst_buo_sph(coef_buo, ipol%i_filter_temp,          &
     &      itor%i_rot_filter_buo)
      end if
!
      end subroutine cal_rot_radial_const_gravity
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_rot_double_cst_buo_sph(is_t)
!
      integer(kind= kint), intent(in) :: is_t
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (nlayer_ICB-1)*nidx_rj(2) + 1
      ied = nlayer_CMB * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d_rj(inod,itor%i_rot_buoyancy)                                  &
     &          =  ( coef_buo * d_rj(inod,is_t)                         &
     &             + coef_comp_buo * d_rj(inod,ipol%i_light)  )
      end do
!$omp end parallel do
!
      end subroutine cal_rot_double_cst_buo_sph
!
!-----------------------------------------------------------------------
!
      subroutine cal_rot_cst_buo_sph(coef, is_fld, it_res)
!
      integer(kind= kint), intent(in) :: is_fld, it_res
      real(kind = kreal), intent(in) :: coef
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (nlayer_ICB-1)*nidx_rj(2) + 1
      ied = nlayer_CMB * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
        d_rj(inod,it_res)                                               &
     &          =  coef * d_rj(inod,is_fld)
      end do
!$omp end parallel do
!
      end subroutine cal_rot_cst_buo_sph
!
!-----------------------------------------------------------------------
!
      end module cal_rot_r_const_buo_sph
