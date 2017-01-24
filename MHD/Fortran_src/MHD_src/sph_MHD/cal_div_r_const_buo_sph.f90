!>@file   cal_div_r_const_buo_sph.f90
!!@brief  module cal_div_r_const_buo_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate divergence of buoyancy under constant radial gravity
!!       for pressure evaluation
!!
!!@verbatim
!!      subroutine cal_div_radial_const_gravity                         &
!!     &         (sph_rj, ipol, sph_bc_U, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic velocity
!!                 boundary condition parameters
!
      module cal_div_r_const_buo_sph
!
      use m_precision
      use m_machine_parameter
!
      use m_constants
      use m_control_parameter
      use m_physical_property
!
      implicit  none
!
      private :: cal_div_double_cst_buo_sph
      private :: cal_div_cst_buo_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_radial_const_gravity                           &
     &         (sph_rj, ipol, sph_bc_U, rj_fld)
!
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_boundary_params_sph_MHD
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      if ((iflag_4_gravity*iflag_4_composit_buo) .gt. id_turn_OFF) then
!
        if(ref_param_T1%iflag_reference .ne. id_sphere_ref_temp) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*)'cal_div_double_cst_buo_sph', ipol%i_temp
          call cal_div_double_cst_buo_sph                               &
     &       (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop1%coef_buo,       &
     &        ipol%i_temp, ipol%i_grad_t, fl_prop1%coef_comp_buo,       &
     &        ipol%i_light, ipol%i_grad_composit, ipol%i_div_buoyancy,  &
     &        sph_rj%nidx_rj, sph_rj%a_r_1d_rj_r,                       &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        else
          if (iflag_debug.eq.1)                                         &
     &      write(*,*)'cal_div_double_cst_buo_sph', ipol%i_par_temp
          call cal_div_double_cst_buo_sph                               &
     &       (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop1%coef_buo,       &
     &        ipol%i_par_temp, ipol%i_grad_part_t,                      &
     &        fl_prop1%coef_comp_buo, ipol%i_light,                     &
     &        ipol%i_grad_composit, ipol%i_div_buoyancy,                &
     &        sph_rj%nidx_rj, sph_rj%a_r_1d_rj_r,                       &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
      else if (iflag_4_gravity .gt. id_turn_OFF) then
!
        if(ref_param_T1%iflag_reference .ne. id_sphere_ref_temp) then
          if (iflag_debug.eq.1) write(*,*) 'cal_div_cst_buo_sph'
          call cal_div_cst_buo_sph                                      &
     &       (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop1%coef_buo,       &
     &        ipol%i_temp, ipol%i_grad_t, ipol%i_div_buoyancy,          &
     &        sph_rj%nidx_rj, sph_rj%a_r_1d_rj_r,                       &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        else
          if (iflag_debug.eq.1) write(*,*) 'cal_div_cst_buo_sph'
          call cal_div_cst_buo_sph(sph_bc_U%kr_in, sph_bc_U%kr_out,     &
     &       fl_prop1%coef_buo, ipol%i_par_temp, ipol%i_grad_part_t,    &
     &       ipol%i_div_buoyancy, sph_rj%nidx_rj, sph_rj%a_r_1d_rj_r,   &
     &       rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
      else if (iflag_4_composit_buo .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_div_cst_buo_sph'
        call cal_div_cst_buo_sph(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &      fl_prop1%coef_comp_buo, ipol%i_light, ipol%i_grad_composit, &
     &      ipol%i_div_comp_buo, sph_rj%nidx_rj, sph_rj%a_r_1d_rj_r,    &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if(iflag_4_filter_gravity .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_div_cst_buo_sph'
        call cal_div_cst_buo_sph                                        &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop1%coef_buo,         &
     &      ipol%i_filter_temp, ipol%i_grad_filter_temp,                &
     &      ipol%i_div_filter_buo, sph_rj%nidx_rj, sph_rj%a_r_1d_rj_r,  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine cal_div_radial_const_gravity
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_double_cst_buo_sph(kr_in, kr_out,              &
     &          coef_t_buo, is_t, ids_t, coef_c_buo,                    &
     &          is_c, ids_c, is_div, nidx_rj, a_r_1d_rj_r,              &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_t, ids_t
      integer(kind= kint), intent(in) :: is_c, ids_c
      integer(kind= kint), intent(in) :: is_div
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
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
          d_rj(inod,is_div) = two * a_r_1d_rj_r(k)                      &
     &                       * (coef_t_buo * d_rj(inod,is_t)            &
     &                        + coef_c_buo * d_rj(inod,is_c))           &
     &                     +  ( coef_t_buo * d_rj(inod,ids_t)           &
     &                        + coef_c_buo * d_rj(inod,ids_c))
        end do
!$omp end parallel do
!
      end subroutine cal_div_double_cst_buo_sph
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_cst_buo_sph(kr_in, kr_out, coef,               &
     &          is_fld, ids_fld, is_div, nidx_rj, a_r_1d_rj_r,          &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_fld, ids_fld, is_div
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef
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
          d_rj(inod,is_div) = coef * ( two * d_rj(inod,is_fld)        &
     &                       * a_r_1d_rj_r(k) + d_rj(inod,ids_fld))
        end do
!$omp end parallel do
!
      end subroutine cal_div_cst_buo_sph
!
!-----------------------------------------------------------------------
!
      end module cal_div_r_const_buo_sph
