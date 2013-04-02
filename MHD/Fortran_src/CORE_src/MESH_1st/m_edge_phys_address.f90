!m_edge_phys_address.f90
!     module m_edge_phys_address
!
!> @brief Field data for FEM
!
!     Written by H. Matsui
!
      module m_edge_phys_address
!
      use m_precision
      use m_constants
!
      implicit  none
! 
      integer (kind=kint) :: ieg_velo  =           izero
      integer (kind=kint) :: ieg_press =           izero
      integer (kind=kint) :: ieg_vort  =           izero
!
      integer (kind=kint) :: ieg_temp  =           izero
      integer (kind=kint) :: ieg_dscalar =         izero
      integer (kind=kint) :: ieg_density =         izero
      integer (kind=kint) :: ieg_entropy =         izero
!
      integer (kind=kint) :: ieg_magne =           izero
      integer (kind=kint) :: ieg_vecp =            izero
      integer (kind=kint) :: ieg_current =         izero
      integer (kind=kint) :: ieg_electric =        izero
      integer (kind=kint) :: ieg_mag_p =           izero
      integer (kind=kint) :: ieg_scalar_p =        izero
!
      integer (kind=kint) :: ieg_par_temp =        izero
      integer (kind=kint) :: ieg_par_dscalar =     izero
      integer (kind=kint) :: ieg_par_density =     izero
      integer (kind=kint) :: ieg_par_entropy =     izero
!
      integer (kind=kint) :: ieg_p_phi =           izero
      integer (kind=kint) :: ieg_m_phi =           izero
      integer (kind=kint) :: ieg_ref_t =           izero
      integer (kind=kint) :: ieg_gref_t =          izero
      integer (kind=kint) :: ieg_ref_density =     izero
      integer (kind=kint) :: ieg_ref_entropy =     izero
!
      integer (kind=kint) :: ieg_filter_velo  =    izero
      integer (kind=kint) :: ieg_filter_temp  =    izero
      integer (kind=kint) :: ieg_filter_magne =    izero
      integer (kind=kint) :: ieg_filter_vecp =     izero
      integer (kind=kint) :: ieg_filter_par_t =    izero
      integer (kind=kint) :: ieg_filter_comp =     izero
!
      integer (kind=kint) :: ieg_k_heli =          izero
      integer (kind=kint) :: ieg_m_heli =          izero
      integer (kind=kint) :: ieg_c_heli =          izero
      integer (kind=kint) :: ieg_x_heli =          izero
!
      integer (kind=kint) :: ieg_me_gen =          izero
      integer (kind=kint) :: ieg_buo_gen =         izero
      integer (kind=kint) :: ieg_c_buo_gen =       izero
      integer (kind=kint) :: ieg_temp_gen =        izero
      integer (kind=kint) :: ieg_par_t_gen =       izero
      integer (kind=kint) :: ieg_ujb =             izero
      integer (kind=kint) :: ieg_nega_ujb =        izero
      integer (kind=kint) :: ieg_m_tension_wk  =   izero
      integer (kind=kint) :: ieg_vis_e_diffuse =   izero
      integer (kind=kint) :: ieg_mag_e_diffuse =   izero
      integer (kind=kint) :: ieg_poynting =        izero
!
      integer (kind=kint) :: ieg_t_diffuse =       izero
      integer (kind=kint) :: ieg_v_diffuse =       izero
      integer (kind=kint) :: ieg_w_diffuse =       izero
      integer (kind=kint) :: ieg_vp_diffuse =      izero
      integer (kind=kint) :: ieg_b_diffuse =       izero
      integer (kind=kint) :: ieg_dscalar_diffuse = izero
!
      integer (kind=kint) :: ieg_h_flux =          izero
      integer (kind=kint) :: ieg_ph_flux =         izero
      integer (kind=kint) :: ieg_c_flux =          izero
      integer (kind=kint) :: ieg_m_flux =          izero
      integer (kind=kint) :: ieg_maxwell =         izero
      integer (kind=kint) :: ieg_induct_t =        izero
!
      integer (kind=kint) :: ieg_h_advect =        izero
      integer (kind=kint) :: ieg_ph_advect =       izero
      integer (kind=kint) :: ieg_m_advect =        izero
      integer (kind=kint) :: ieg_h_flux_div =      izero
      integer (kind=kint) :: ieg_ph_flux_div =     izero
      integer (kind=kint) :: ieg_m_flux_div =      izero
      integer (kind=kint) :: ieg_maxwell_div =     izero
      integer (kind=kint) :: ieg_induct_div =      izero
      integer (kind=kint) :: ieg_m_tension =       izero
      integer (kind=kint) :: ieg_lorentz =         izero
      integer (kind=kint) :: ieg_coriolis =        izero
      integer (kind=kint) :: ieg_buoyancy =        izero
      integer (kind=kint) :: ieg_comp_buo =        izero
      integer (kind=kint) :: ieg_induction =       izero
      integer (kind=kint) :: ieg_vp_induct =       izero
      integer (kind=kint) :: ieg_dscalar_advect =  izero
!
      integer (kind=kint) :: ieg_SGS_h_flux =      izero
      integer (kind=kint) :: ieg_SGS_m_flux =      izero
      integer (kind=kint) :: ieg_SGS_maxwell =     izero
      integer (kind=kint) :: ieg_SGS_induct_t =    izero
!
      integer (kind=kint) :: ieg_SGS_div_h_flux=   izero
      integer (kind=kint) :: ieg_SGS_div_m_flux=   izero
      integer (kind=kint) :: ieg_SGS_Lorentz =     izero
      integer (kind=kint) :: ieg_SGS_induction =   izero
      integer (kind=kint) :: ieg_SGS_vp_induct =   izero
!
!
      integer (kind=kint) :: ieg_SGS_div_hf_true = izero
      integer (kind=kint) :: ieg_SGS_div_mf_true = izero
      integer (kind=kint) :: ieg_SGS_Lor_true =    izero
      integer (kind=kint) :: ieg_SGS_idct_true =   izero
!
      integer (kind=kint) :: ieg_SGS_t_gen_tr =    izero
      integer (kind=kint) :: ieg_SGS_me_gen_tr =   izero
      integer (kind=kint) :: ieg_SGS_Lor_wk_tr =   izero
      integer (kind=kint) :: ieg_reynolds_wk_tr =  izero
!
!
      integer (kind=kint) :: ieg_SGS_temp_gen =    izero
      integer (kind=kint) :: ieg_SGS_me_gen =      izero
      integer (kind=kint) :: ieg_SGS_Lor_wk =      izero
      integer (kind=kint) :: ieg_reynolds_wk =     izero
!
      integer (kind=kint) :: ieg_grad_vx = izero
      integer (kind=kint) :: ieg_grad_vy = izero
      integer (kind=kint) :: ieg_grad_vz = izero
      integer (kind=kint) :: ieg_grad_wx = izero
      integer (kind=kint) :: ieg_grad_wy = izero
      integer (kind=kint) :: ieg_grad_wz = izero
      integer (kind=kint) :: ieg_grad_ax = izero
      integer (kind=kint) :: ieg_grad_ay = izero
      integer (kind=kint) :: ieg_grad_az = izero
      integer (kind=kint) :: ieg_grad_bx = izero
      integer (kind=kint) :: ieg_grad_by = izero
      integer (kind=kint) :: ieg_grad_bz = izero
      integer (kind=kint) :: ieg_grad_jx = izero
      integer (kind=kint) :: ieg_grad_jy = izero
      integer (kind=kint) :: ieg_grad_jz = izero
!
      integer (kind=kint) :: ieg_grad_t =          izero
      integer (kind=kint) :: ieg_grad_part_t =     izero
      integer (kind=kint) :: ieg_grad_dscalar =    izero
!
      integer (kind=kint) :: ieg_sgs_simi =        izero
      integer (kind=kint) :: ieg_sgs_grad =        izero
      integer (kind=kint) :: ieg_sgs_grad_f =      izero
      integer (kind=kint) :: ieg_sgs_diffuse =     izero
!
      integer (kind=kint) :: ieg_sgs_temp =        izero
      integer (kind=kint) :: ieg_sgs_composit =    izero
!
      integer (kind=kint) :: ieg_wide_fil_velo  =  izero
      integer (kind=kint) :: ieg_wide_fil_temp  =  izero
      integer (kind=kint) :: ieg_wide_fil_magne =  izero
      integer (kind=kint) :: ieg_wide_fil_vecp =   izero
!
!  divergence of momentum equations
!
      integer (kind=kint) :: ieg_div_inertia =  izero
      integer (kind=kint) :: ieg_div_Lorentz =  izero
      integer (kind=kint) :: ieg_div_Coriolis = izero
      integer (kind=kint) :: ieg_div_buoyancy = izero
      integer (kind=kint) :: ieg_div_comp_buo = izero
      integer (kind=kint) :: ieg_div_viscous =  izero
!
!  rotation of momentum equations
!
      integer (kind=kint) :: ieg_rot_inertia =  izero
      integer (kind=kint) :: ieg_rot_Lorentz =  izero
      integer (kind=kint) :: ieg_rot_Coriolis = izero
      integer (kind=kint) :: ieg_rot_buoyancy = izero
      integer (kind=kint) :: ieg_rot_comp_buo = izero
!
!  arrays for current forces
!
      integer (kind=kint) :: ieg_forces =       izero
      integer (kind=kint) :: ieg_rot_forces =   izero
      integer (kind=kint) :: ieg_div_forces =   izero
!
!  arrays for previous evolution
!
      integer (kind=kint) :: ieg_pre_mom =     izero
      integer (kind=kint) :: ieg_pre_uxb =     izero
      integer (kind=kint) :: ieg_pre_heat =    izero
      integer (kind=kint) :: ieg_pre_dscalar = izero
      integer (kind=kint) :: ieg_pre_press =   izero
!
!  arrays for evolution check
!
      integer (kind=kint) :: ieg_chk_mom =       izero
      integer (kind=kint) :: ieg_chk_uxb =       izero
      integer (kind=kint) :: ieg_chk_heat =      izero
      integer (kind=kint) :: ieg_chk_dscalar =   izero
      integer (kind=kint) :: ieg_chk_press =     izero
      integer (kind=kint) :: ieg_chk_potential = izero
!
      integer (kind=kint) :: ieg_chk_mom_2 =       izero
      integer (kind=kint) :: ieg_chk_uxb_2 =       izero
      integer (kind=kint) :: ieg_chk_heat_2 =      izero
      integer (kind=kint) :: ieg_chk_dscalar_2 =   izero
      integer (kind=kint) :: ieg_chk_press_2 =     izero
      integer (kind=kint) :: ieg_chk_potential_2 = izero
!
! -------------------------------------------------------------------
!
!      contains
!
! -------------------------------------------------------------------
!
      end module m_edge_phys_address
