!> @file  pole_energy_flux_sph.f90
!!      module pole_energy_flux_sph
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2012
!
!> @brief Evaluate nonlinear terms at poles
!!
!!@verbatim
!!      subroutine pole_energy_flux_rtp(sph_rtp, node,                  &
!!     &          fl_prop, cd_prop, ref_param_T, ref_param_C,           &
!!     &         iphys, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(node_data), intent(in) :: node
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module pole_energy_flux_sph
!
      use m_precision
      use m_constants
!
      use t_physical_property
      use t_spheric_rtp_data
      use t_geometry_data
      use t_phys_address
      use t_phys_data
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine pole_energy_flux_rtp(sph_rtp, node,                    &
     &          fl_prop, cd_prop, ref_param_T, ref_param_C,             &
     &         iphys, nod_fld)
!
      use m_machine_parameter
      use t_reference_scalar_param
!
      use products_at_poles
      use pole_poynting_flux_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(fluid_property), intent(in) :: fl_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
!$omp parallel
      if(iphys%prod_fld%i_electric .gt. 0) then
        call cal_pole_electric_field_smp                                &
     &     (node%numnod, node%internal_node, node%xx,                   &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1),                      &
     &      cd_prop%coef_diffuse, nod_fld%ntot_phys,                    &
     &      iphys%base%i_current, iphys%forces%i_vp_induct,             &
     &      iphys%prod_fld%i_electric, nod_fld%d_fld)
      end if
!
      if(iphys%prod_fld%i_poynting .gt. 0) then
        call cal_pole_poynting_flux_smp                                 &
     &     (node%numnod, node%internal_node, node%xx,                   &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1),                      &
     &      cd_prop%coef_diffuse, nod_fld%ntot_phys,                    &
     &      iphys%base%i_current, iphys%forces%i_vp_induct,             &
     &      iphys%base%i_magne, iphys%prod_fld%i_poynting,              &
     &      nod_fld%d_fld)
      end if
!$omp end parallel
!
      end subroutine pole_energy_flux_rtp
!
!-----------------------------------------------------------------------
!
      end module pole_energy_flux_sph
