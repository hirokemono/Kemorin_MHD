!>@file   rot_self_buoyancies_sph.f90
!!@brief  module rot_self_buoyancies_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate rotation of buoyancy
!!
!!@verbatim
!!      subroutine sel_rot_self_buoyancy_sph(sph_rj,                    &
!!     &          ipol_base, ipol_rot_frc, fl_prop, sph_bc_U, rj_fld)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(base_force_address), intent(in) :: ipol_rot_frc
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine cal_rot_radial_self_gravity                          &
!!     &         (sph_rj, ipol, fl_prop, sph_bc_U, rj_fld)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine cal_rot_buoyancy_sph_MHD(kr_in, kr_out, coef,        &
!!     &          is_fld, it_res, nidx_rj, radius_1d_rj_r,              &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine cal_boussinesq_density_sph                           &
!!     &         (ipol, kr_in, kr_out, coef_buo, coef_comp_buo,         &
!!     &          nidx_rj, nnod_rj, ntot_phys_rj, d_rj)
!!        type(phys_address), intent(in) :: ipol
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic velocity
!!                 boundary condition parameters
!!@param kr_in     Radial ID for inner boundary
!!@param kr_out    Radial ID for outer boundary
!
      module rot_self_buoyancies_sph
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
!
      implicit  none
!
      private :: cal_rot_double_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_rot_self_buoyancy_sph(sph_rj,                      &
     &          ipol_base, ipol_rot_frc, fl_prop, sph_bc_U, rj_fld)
!
      use t_physical_property
      use t_spheric_rj_data
      use t_base_field_labels
      use t_base_force_labels
      use t_phys_data
      use t_boundary_params_sph_MHD
!
      type(fluid_property), intent(in) :: fl_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_rot_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: it_rot_buo
!
!
      if ((fl_prop%iflag_4_gravity * fl_prop%iflag_4_composit_buo)      &
     &     .gt. id_turn_OFF) then
!
        if (iflag_debug.eq.1)                                           &
     &    write(*,*)'cal_rot_double_buoyancy_sph_MHD', ipol_base%i_temp
          it_rot_buo = ipol_rot_frc%i_buoyancy + 2
          call cal_rot_double_buoyancy_sph_MHD                          &
     &      (sph_bc_U%kr_in, sph_bc_U%kr_out,                           &
     &       fl_prop%coef_buo, ipol_base%i_temp, fl_prop%coef_comp_buo, &
     &       ipol_base%i_light, it_rot_buo,                             &
     &       sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                     &
     &       rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if (fl_prop%iflag_4_gravity .gt. id_turn_OFF) then
!
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'cal_rot_buoyancy_sph_MHD', ipol_base%i_temp
        it_rot_buo = ipol_rot_frc%i_buoyancy + 2
        call cal_rot_buoyancy_sph_MHD                                   &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_buo,          &
     &      ipol_base%i_temp, it_rot_buo,                               &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if (fl_prop%iflag_4_composit_buo .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'cal_rot_buoyancy_sph_MHD', ipol_base%i_light
        it_rot_buo = ipol_rot_frc%i_comp_buo + 2
        call cal_rot_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out,  &
     &      fl_prop%coef_comp_buo, ipol_base%i_light, it_rot_buo,       &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_rot_self_buoyancy_sph
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_rot_double_buoyancy_sph_MHD(kr_in, kr_out,         &
     &          coef_t_buo, is_t, coef_c_buo, is_c, it_res,             &
     &          nidx_rj, radius_1d_rj_r, nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_t, is_c
      integer(kind= kint), intent(in) :: it_res
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef_t_buo, coef_c_buo
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
        d_rj(inod,it_res) =  ( coef_t_buo * d_rj(inod,is_t)             &
     &                       + coef_c_buo * d_rj(inod,is_c)  )          &
     &                      * radius_1d_rj_r(k)
      end do
!$omp end parallel do
!
      end subroutine cal_rot_double_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine cal_rot_buoyancy_sph_MHD(kr_in, kr_out, coef,          &
     &          is_fld, it_res, nidx_rj, radius_1d_rj_r,                &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_fld, it_res
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
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
        d_rj(inod,it_res)                                               &
     &          =  coef * d_rj(inod,is_fld) * radius_1d_rj_r(k)
      end do
!$omp end parallel do
!
      end subroutine cal_rot_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_boussinesq_density_sph                             &
     &         (ipol, kr_in, kr_out, coef_buo, coef_comp_buo,           &
     &          nidx_rj, nnod_rj, ntot_phys_rj, d_rj)
!
      use t_phys_address
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal) , intent(in) :: coef_buo, coef_comp_buo
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
        d_rj(inod,ipol%base%i_density)                                  &
     &        = -(d_rj(inod,ipol%base%i_temp)                           &
     &         + coef_comp_buo/coef_buo * d_rj(inod,ipol%base%i_light))
      end do
!$omp end parallel do
!
      end subroutine cal_boussinesq_density_sph
!
!-----------------------------------------------------------------------
!
      end module rot_self_buoyancies_sph
