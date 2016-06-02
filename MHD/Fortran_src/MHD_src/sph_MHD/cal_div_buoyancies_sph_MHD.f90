!>@file   cal_div_buoyancies_sph_MHD.f90
!!@brief  module cal_div_buoyancies_sph_MHD
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate divergence of buoyancy for pressure evaluation
!!
!!@verbatim
!!      subroutine sel_div_buoyancies_sph_MHD(sph_rj, sph_bc_U, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic velocity
!!                 boundary condition parameters
!
      module cal_div_buoyancies_sph_MHD
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
      private :: cal_div_double_buoyancy_sph_MHD
      private :: cal_div_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_div_buoyancies_sph_MHD(sph_rj, sph_bc_U, rj_fld)
!
      use m_machine_parameter
      use t_spheric_rj_data
      use t_phys_data
      use t_boundary_params_sph_MHD
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      if ((iflag_4_gravity*iflag_4_composit_buo) .gt. id_turn_OFF) then
        if(iflag_4_ref_temp .ne. id_sphere_ref_temp) then
          if (iflag_debug.ge.1) write(*,*)                              &
     &        'cal_div_double_buoyancy_sph_MHD by temp', ipol%i_temp
          call cal_div_double_buoyancy_sph_MHD                          &
     &       (sph_bc_U%kr_in, sph_bc_U%kr_out,                          &
     &        coef_buo, ipol%i_temp, ipol%i_grad_t, coef_comp_buo,      &
     &        ipol%i_light, ipol%i_grad_composit, ipol%i_div_buoyancy,  &
     &        sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                    &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        else
          if (iflag_debug.ge.1) write(*,*)                              &
     &      'cal_div_double_buoyancy_sph_MHD by part.temp',             &
     &       ipol%i_par_temp
          call cal_div_double_buoyancy_sph_MHD                          &
     &       (sph_bc_U%kr_in, sph_bc_U%kr_out, coef_buo,                &
     &        ipol%i_par_temp, ipol%i_grad_part_t, coef_comp_buo,       &
     &        ipol%i_light, ipol%i_grad_composit, ipol%i_div_buoyancy,  &
     &        sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                    &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
      else if (iflag_4_gravity .gt. id_turn_OFF) then
        if(iflag_4_ref_temp .ne. id_sphere_ref_temp) then
          if (iflag_debug.ge.1)  write(*,*)                             &
     &      'cal_div_buoyancy_sph_MHD by temperature'
          call cal_div_buoyancy_sph_MHD                                 &
     &       (sph_bc_U%kr_in, sph_bc_U%kr_out, coef_buo,                &
     &        ipol%i_temp, ipol%i_grad_t, ipol%i_div_buoyancy,          &
     &        sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                    &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        else
          if (iflag_debug.ge.1)  write(*,*)                             &
     &      'cal_div_buoyancy_sph_MHD by pert. temperature'
          call cal_div_buoyancy_sph_MHD                                 &
     &       (sph_bc_U%kr_in, sph_bc_U%kr_out, coef_buo,                &
     &        ipol%i_par_temp, ipol%i_grad_part_t, ipol%i_div_buoyancy, &
     &        sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                    &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
      else if (iflag_4_composit_buo .gt. id_turn_OFF) then
          if (iflag_debug.ge.1)  write(*,*)                             &
     &      'cal_div_buoyancy_sph_MHD by composition'
        call cal_div_buoyancy_sph_MHD                                   &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, coef_comp_buo,             &
     &      ipol%i_light, ipol%i_grad_composit, ipol%i_div_comp_buo,    &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if(iflag_4_filter_gravity .gt. id_turn_OFF) then
          if (iflag_debug.ge.1)  write(*,*)                             &
     &      'cal_div_buoyancy_sph_MHD by filtrered temperature'
        call cal_div_buoyancy_sph_MHD                                   &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, coef_buo,                  &
     &      ipol%i_filter_temp, ipol%i_grad_filter_temp,                &
     &      ipol%i_div_filter_buo,                                      &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_div_buoyancies_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_double_buoyancy_sph_MHD(kr_in, kr_out,         &
     &          coef_t_buo, is_t, ids_t,  coef_c_buo, is_c, ids_c,      &
     &          is_div, nidx_rj, radius_1d_rj_r,                        &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_t, ids_t
      integer(kind= kint), intent(in) :: is_c, ids_c
      integer(kind= kint), intent(in) :: is_div
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef_t_buo, coef_c_buo
!
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
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
          d_rj(inod,is_div)                                             &
     &          = three * (coef_buo * d_rj(inod,is_t)                   &
     &                   + coef_t_buo * d_rj(inod,is_c))                &
     &                   +  ( coef_buo * d_rj(inod,ids_t)               &
     &                   + coef_c_buo * d_rj(inod,ids_c) )              &
     &                 * radius_1d_rj_r(k)
        end do
!$omp end parallel do
!
      end subroutine cal_div_double_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_buoyancy_sph_MHD(kr_in, kr_out, coef,          &
     &          is_fld, ids_fld, is_div, nidx_rj, radius_1d_rj_r,       &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_fld, ids_fld, is_div
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
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
        d_rj(inod,is_div) = coef * ( three * d_rj(inod,is_fld)          &
     &                       + d_rj(inod,ids_fld) * radius_1d_rj_r(k))
      end do
!$omp end parallel do
!
      end subroutine cal_div_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      end module cal_div_buoyancies_sph_MHD
