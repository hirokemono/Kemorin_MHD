!> @file  cal_energy_flux_w_SGS_rtp.f90
!!      module cal_energy_flux_w_SGS_rtp
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2009
!! @n    Modified in Apr., 2013
!
!> @brief Evaluate energy fluxes for MHD dynamo in physical space
!!
!!@verbatim
!!      subroutine cal_filterd_buo_flux_rtp(sph_rtp, fl_prop,           &
!!     &          b_trns_base, b_trns_fil, f_trns_fefx,                 &
!!     &          trns_b_snap, trns_f_snap)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(base_field_address), intent(in) :: b_trns_base
!!        type(base_field_address), intent(in) :: b_trns_fil
!!        type(energy_flux_address), intent(in) :: f_trns_fefx
!!        type(address_each_sph_trans), intent(in) :: trns_b_snap
!!        type(address_each_sph_trans), intent(inout) :: trns_f_snap
!!      subroutine pole_filterd_buo_flux_rtp(sph_rtp, node,             &
!!     &          fl_prop, iphys_base, iphys_fil, iphys_fefx, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(node_data), intent(in) :: node
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_field_address), intent(in) :: iphys_fil
!!        type(energy_flux_address), intent(in) :: iphys_fefx
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module cal_energy_flux_w_SGS_rtp
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_phys_address
      use t_spheric_rtp_data
      use t_physical_property
      use t_reference_scalar_param
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
!
      implicit  none
!
      private :: pole_filterd_buo_flux_rtp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_filterd_buo_flux_rtp(sph_rtp, fl_prop,             &
     &          b_trns_base, b_trns_fil, f_trns_fefx,                   &
     &          trns_b_snap, trns_f_snap)
!
      use cal_energy_flux_rtp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(base_field_address), intent(in) :: b_trns_base
      type(base_field_address), intent(in) :: b_trns_fil
      type(energy_flux_address), intent(in) :: f_trns_fefx
      type(address_each_sph_trans), intent(in) :: trns_b_snap
!
      type(address_each_sph_trans), intent(inout) :: trns_f_snap
!
!
!$omp parallel
      if(f_trns_fefx%i_buo_gen .gt. 0) then
        call cal_buoyancy_flux_rtp_smp(np_smp, sph_rtp%nnod_rtp,        &
     &      sph_rtp%nidx_rtp(1),  sph_rtp%istack_inod_rtp_smp,          &
     &      sph_rtp%radius_1d_rtp_r, fl_prop%coef_buo,                  &
     &      trns_b_snap%fld_rtp(1,b_trns_fil%i_temp),                   &
     &      trns_b_snap%fld_rtp(1,b_trns_base%i_velo),                  &
     &      trns_f_snap%fld_rtp(1,f_trns_fefx%i_buo_gen))
      end if
!
      if(f_trns_fefx%i_c_buo_gen .gt. 0) then
        call cal_buoyancy_flux_rtp_smp(np_smp, sph_rtp%nnod_rtp,        &
     &      sph_rtp%nidx_rtp(1),  sph_rtp%istack_inod_rtp_smp,          &
     &      sph_rtp%radius_1d_rtp_r, fl_prop%coef_comp_buo,             &
     &      trns_b_snap%fld_rtp(1,b_trns_fil%i_light),                  &
     &      trns_b_snap%fld_rtp(1,b_trns_base%i_velo),                  &
     &      trns_f_snap%fld_rtp(1,f_trns_fefx%i_c_buo_gen))
      end if
!$omp end parallel
!
      end subroutine cal_filterd_buo_flux_rtp
!
!-----------------------------------------------------------------------
!
      subroutine pole_filterd_buo_flux_rtp(sph_rtp, node,               &
     &          fl_prop, iphys_base, iphys_fil, iphys_fefx, nod_fld)
!
      use pole_energy_flux_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(fluid_property), intent(in) :: fl_prop
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(energy_flux_address), intent(in) :: iphys_fefx
      type(phys_data), intent(inout) :: nod_fld
!
!
!$omp parallel
      if(iphys_fefx%i_buo_gen .gt. 0) then
        call pole_sph_buoyancy_flux                                     &
     &       (node%numnod, node%internal_node, node%xx,                 &
     &        sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), fl_prop%coef_buo,  &
     &        nod_fld%ntot_phys, iphys_fil%i_temp,                      &
     &        iphys_base%i_velo, iphys_fefx%i_buo_gen, nod_fld%d_fld)
      end if
!
      if(iphys_fefx%i_c_buo_gen .gt. 0) then
        call pole_sph_buoyancy_flux                                     &
     &       (node%numnod, node%internal_node, node%xx,                 &
     &        sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1),                    &
     &        fl_prop%coef_comp_buo, nod_fld%ntot_phys,                 &
     &        iphys_fil%i_light, iphys_base%i_velo,                     &
     &        iphys_fefx%i_c_buo_gen, nod_fld%d_fld)
      end if
!$omp end parallel
!
      end subroutine pole_filterd_buo_flux_rtp
!
!-----------------------------------------------------------------------
!
      end module cal_energy_flux_w_SGS_rtp
