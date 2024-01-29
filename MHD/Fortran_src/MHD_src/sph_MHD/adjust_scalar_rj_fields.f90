!>@file   adjust_scalar_rj_fields.f90
!!@brief      module adjust_scalar_rj_fields
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2024
!
!> @brief  Evaluate shifted scalar by CMB value
!!
!!@verbatim
!!      subroutine s_adjust_scalar_rj_fields(sph, ipol_base, ipol_cmp,  &
!!     &                                     rj_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(field_component_address), intent(in) :: ipol_cmp
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module adjust_scalar_rj_fields
!
      use t_spheric_parameter
!
      implicit none
!
      private :: shift_by_CMB_average, remove_sphere_average
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_adjust_scalar_rj_fields(sph, ipol_base, ipol_cmp,    &
     &                                     rj_fld)
!
      use t_base_field_labels
      use t_field_component_labels
      use t_phys_data
!
      type(sph_grids), intent(in) :: sph
      type(base_field_address), intent(in) :: ipol_base
      type(field_component_address), intent(in) :: ipol_cmp
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol_cmp%i_temp_from_CMB .gt. 0) then
        call shift_by_CMB_average(sph%sph_params, sph%sph_rj,           &
     &      rj_fld%d_fld(1,ipol_base%i_temp),                           &
     &      rj_fld%d_fld(1,ipol_cmp%i_temp_from_CMB))
      end if
!
      if(ipol_cmp%i_light_from_CMB .gt. 0) then
        call shift_by_CMB_average(sph%sph_params, sph%sph_rj,           &
     &      rj_fld%d_fld(1,ipol_base%i_light),                          &
     &      rj_fld%d_fld(1,ipol_cmp%i_light_from_CMB))
      end if
!
      if(ipol_cmp%i_entropy_from_CMB .gt. 0) then
        call shift_by_CMB_average(sph%sph_params, sph%sph_rj,           &
     &      rj_fld%d_fld(1,ipol_base%i_entropy),                        &
     &      rj_fld%d_fld(1,ipol_cmp%i_entropy_from_CMB))
      end if
!
      if(ipol_cmp%i_density_from_CMB .gt. 0) then
        call shift_by_CMB_average(sph%sph_params, sph%sph_rj,           &
     &      rj_fld%d_fld(1,ipol_base%i_density),                        &
     &      rj_fld%d_fld(1,ipol_cmp%i_density_from_CMB))
      end if
!
!
      if(ipol_cmp%i_asph_pressure .gt. 0) then
        call remove_sphere_average(sph%sph_rj,                          &
     &      rj_fld%d_fld(1,ipol_base%i_press),                          &
     &      rj_fld%d_fld(1,ipol_cmp%i_asph_pressure))
      end if
!
      end subroutine s_adjust_scalar_rj_fields
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine shift_by_CMB_average(sph_params, sph_rj,               &
     &                                d_scalar, d_rj_part)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      real(kind = kreal), intent(in) :: d_scalar(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout) :: d_rj_part(sph_rj%nnod_rj)
!
      integer(kind = kint) :: kr, inod, iCMB
!
!
      if(sph_rj%idx_rj_degree_zero .eq. 0) return
!
      iCMB = 1 + (sph_params%nlayer_CMB-1) * sph_rj%istep_rj(1)         &
     &         + (sph_rj%idx_rj_degree_zero-1) * sph_rj%istep_rj(2)
!$omp parallel do private(kr,inod)
      do kr = 1, sph_rj%nidx_rj(1)
        inod = 1 + (kr-1) * sph_rj%istep_rj(1)                          &
     &           + (sph_rj%idx_rj_degree_zero-1) * sph_rj%istep_rj(2)
        d_rj_part(inod) = d_scalar(inod) - d_scalar(iCMB)
      end do
!$omp end parallel do
!
      end subroutine shift_by_CMB_average
!
!-----------------------------------------------------------------------
!
      subroutine remove_sphere_average(sph_rj, d_scalar, d_rj_part)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      real(kind = kreal), intent(in) :: d_scalar(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout) :: d_rj_part(sph_rj%nnod_rj)
!
      integer(kind = kint) :: kr, inod, iCMB
!
!
      if(sph_rj%idx_rj_degree_zero .eq. 0) return
!
!$omp parallel do private(kr,inod)
      do kr = 1, sph_rj%nidx_rj(1)
        inod = 1 + (kr-1) * sph_rj%istep_rj(1)                          &
     &           + (sph_rj%idx_rj_degree_zero-1) * sph_rj%istep_rj(2)
        d_rj_part(inod) = 0.0d0
      end do
!$omp end parallel do
!
      end subroutine remove_sphere_average
!
!-----------------------------------------------------------------------
!
      end module adjust_scalar_rj_fields
