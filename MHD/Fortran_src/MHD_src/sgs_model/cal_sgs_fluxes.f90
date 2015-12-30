!
!      module cal_sgs_fluxes
!
!      Written by H. Matsui
!
!      subroutine cal_sgs_heat_flux
!      subroutine cal_sgs_momentum_flux
!      subroutine cal_sgs_maxwell
!      subroutine cal_sgs_magne_induction
!      subroutine cal_sgs_uxb_2_evo
!
      module cal_sgs_fluxes
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use m_control_parameter
      use m_t_step_parameter
!
!
      use cal_sgs_fluxes_simi
      use cal_sgs_fluxes_diffuse
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_heat_flux
!
      use m_node_phys_data
      use m_SGS_address
      use cal_sgs_heat_fluxes_grad
!
!
      if (     iflag_SGS_heat .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'cal_sgs_h_flux_grad', ifilter_final
        call cal_sgs_h_flux_grad(ifilter_final)
!
      else if (iflag_SGS_heat .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1) write(*,*) 'cal_sgs_hf_simi'
        call cal_sgs_hf_simi(iphys%i_SGS_h_flux, iphys%i_sgs_temp,      &
     &      iphys%i_filter_temp, icomp_sgs_hf)
!
      else if (iflag_SGS_heat .eq. id_SGS_diffusion) then
        if (iflag_debug.eq.1) write(*,*) 'cal_sgs_h_flux_diffuse'
        call cal_sgs_h_flux_diffuse
      end if
!
      end subroutine cal_sgs_heat_flux
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_momentum_flux
!
      use m_geometry_data
      use m_node_phys_data
      use m_SGS_address
!
      use cal_sgs_mom_fluxes_grad
!
!
      if (  iflag_SGS_inertia .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_m_flux_grad', ifilter_final
        call cal_sgs_m_flux_grad(ifilter_final)
!
      else if (iflag_SGS_inertia .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_mf_simi', iphys%i_SGS_m_flux
        call cal_sgs_mf_simi(iphys%i_SGS_m_flux, iphys%i_velo,          &
     &      iphys%i_filter_velo, icomp_sgs_lor)
!
      else if (iflag_SGS_inertia .eq. id_SGS_diffusion) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_m_flux_diffuse', iphys%i_SGS_m_flux
        call cal_sgs_m_flux_diffuse(node1%numnod, nod_fld1%ntot_phys,   &
     &      iphys%i_velo, iphys%i_sgs_diffuse, iphys%i_SGS_m_flux,      &
     &      nod_fld1%d_fld)
      end if
!
      end subroutine cal_sgs_momentum_flux
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_maxwell
!
      use m_geometry_data
      use m_node_phys_data
      use m_SGS_address
      use cal_sgs_mom_fluxes_grad
!
!
      if (     iflag_SGS_lorentz .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_maxwell_grad', ifilter_final
        call cal_sgs_maxwell_grad(ifilter_final)
!
      else if (iflag_SGS_lorentz .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'cal_sgs_mf_simi',  iphys%i_SGS_maxwell
        call cal_sgs_mf_simi(iphys%i_SGS_maxwell, iphys%i_magne,        &
     &      iphys%i_filter_magne, icomp_sgs_lor)
!
      else if (iflag_SGS_lorentz .eq. id_SGS_diffusion) then
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'cal_sgs_m_flux_diffuse', iphys%i_SGS_maxwell
        call cal_sgs_m_flux_diffuse(node1%numnod, nod_fld1%ntot_phys,   &
     &      iphys%i_magne, iphys%i_sgs_diffuse, iphys%i_SGS_maxwell,    &
     &      nod_fld1%d_fld)
      end if
!
      end subroutine cal_sgs_maxwell
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_magne_induction
!
      use m_node_phys_data
      use m_SGS_address
      use cal_sgs_inductions_grad
!
!
      if     (iflag_SGS_induction .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &    write(*,*) 'cal_sgs_induct_t_grad', ifilter_final
        call cal_sgs_induct_t_grad(ifilter_final)
!
      else if(iflag_SGS_induction .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &      write(*,*) 'cal_sgs_induct_t_simi'
        call cal_sgs_induct_t_simi(iphys%i_SGS_induct_t, iphys%i_velo,  &
     &      iphys%i_magne, iphys%i_filter_velo, iphys%i_filter_magne,   &
     &      icomp_sgs_uxb)
      end if
!
      end subroutine cal_sgs_magne_induction
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_2_evo
!
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_finite_element_matrix
      use cal_rotation
      use cal_sgs_uxb_grad
      use cal_sgs_uxb_dynamic_simi
!
!
      if     (iflag_SGS_induction .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)                                           &
     &      write(*,*) 'cal_sgs_uxb_2_ff_grad', ifilter_final
        call cal_sgs_uxb_2_ff_grad(ifilter_final)
!
      else if(iflag_SGS_induction .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &      write(*,*) 'cal_sgs_uxb_2_ff_simi', ifilter_final
        call cal_sgs_uxb_2_ff_simi
!
      else if(iflag_SGS_induction .eq. id_SGS_diffusion) then
         if (iflag_debug.eq.1)                                          &
     &      write(*,*) 'choose_int_vol_rotations'
         call choose_int_vol_rotations(iflag_mag_supg,                  &
     &       fluid1%istack_ele_fld_smp, node1, ele1, nod_fld1,          &
     &       iphys%i_magne)
      end if
!
      end subroutine cal_sgs_uxb_2_evo
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_fluxes
