!m_volume_average_labels.f90
!      module m_volume_average_labels
!
!        programmed by H.Matsui on June, 2009
!
!      subroutine set_vector_vol_average_labels
!
      module m_volume_average_labels
!
      use m_precision
!
      use m_phys_labels
!
      implicit none
!
!
      character(len=kchara), parameter :: e_hd_volume = 'Volume'
!
!
      character(len=kchara), parameter :: e_hd_k_ene = 'K_ene'
      character(len=kchara), parameter :: e_hd_div_v = 'div_v'
!
      character(len=kchara), parameter :: e_hd_vvec(3)                  &
     &            = (/'vx', 'vy', 'vz'/)
      character(len=kchara), parameter :: e_hd_lvec(3)                  &
     &            = (/'L_x', 'L_y', 'L_z'/)
!
!
      character(len=kchara), parameter :: e_hd_m_ene =    'M_ene'
      character(len=kchara), parameter :: e_hd_m_ene_cd = 'M_ene_cd'
      character(len=kchara), parameter :: e_hd_div_b =    'div_B'
!
      character(len=kchara), parameter :: e_hd_bvec(3)                  &
     &            = (/'Bx', 'By', 'Bz'/)
!
      character(len=kchara), parameter :: e_hd_bvec_cd(3)               &
     &            = (/'Bx_cd', 'By_cd', 'Bz_cd'/)
!
      character(len=kchara), parameter :: e_hd_div_a =    'div_A'
!
!
      character(len=kchara), parameter :: e_hd_sq_w =  'sq_w'
      character(len=kchara), parameter :: e_hd_rms_w = 'RMS_omega'
      character(len=kchara), parameter :: e_hd_wvec(3)                  &
     &            = (/'wx', 'wy', 'wz'/)
!
      character(len=kchara), parameter :: e_hd_sq_j =     'sq_J'
      character(len=kchara), parameter :: e_hd_sq_j_cd =  'sq_J_cd'
      character(len=kchara), parameter :: e_hd_rms_j =    'RMS_J'
      character(len=kchara), parameter :: e_hd_rms_j_cd = 'RMS_J_cd'
!
      character(len=kchara), parameter :: e_hd_jvec(3)                  &
     &            = (/'Jx', 'Jy', 'Jz'/)
      character(len=kchara), parameter :: e_hd_jvec_cd(3)               &
     &            = (/'Jx_cd', 'Jy_cd', 'Jz_cd'/)
!
!
      character(len=kchara), parameter :: e_hd_rms_e = 'RMS_E'
      character(len=kchara), parameter :: e_hd_evec(3)                  &
     &            = (/'Ex', 'Ey', 'Ez'/)
!
!
      character(len=kchara), parameter                                  &
     &                      :: e_hd_rms_pflux = 'RMS_poynting_f'
      character(len=kchara), parameter :: e_hd_pflux_v(3)               &
     &            = (/'poynting_x', 'poynting_y', 'poynting_z'/)
!
!
      character(len=kchara), parameter :: e_hd_temp      = 'Temp'
      character(len=kchara), parameter :: e_hd_part_temp = 'per_temp'
      character(len=kchara), parameter :: e_hd_press = 'Press'
      character(len=kchara), parameter :: e_hd_mag_p = 'mag_potential'
!
!
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_k_ene = 'filter_K_ene'
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_div_v = 'div_filter_v'
!
      character(len=kchara), parameter :: e_hd_fil_vvec(3)              &
     &            = (/'filter_vx', 'filter_vy', 'filter_vz'/)
      character(len=kchara), parameter :: e_hd_fil_lvec(3)              &
     &            = (/'filter_L_x', 'filter_L_y', 'filter_L_z'/)
!
!
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_m_ene =    'filter_M_ene'
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_m_ene_cd = 'filter_M_ene_cd'
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_div_b =    'div_filter_B'
!
      character(len=kchara), parameter :: e_hd_fil_bvec(3)              &
     &            = (/'filter_Bx', 'filter_By', 'filter_Bz'/)
!
      character(len=kchara), parameter :: e_hd_fil_bvec_cd(3)           &
     &            = (/'filter_Bx_cd','filter_By_cd','filter_Bz_cd'/)
!
!
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_div_a =    'div_filter_A'
!
!
      character(len=kchara) :: e_hd_press_grad_v(3)
!
      character(len=kchara), parameter                                  &
     &                      :: e_hd_mag_tension = 'mag_tension'
      character(len=kchara) :: e_hd_mag_tension_v(3)
!
      character(len=kchara) :: e_hd_inertia_v(3)
      character(len=kchara) :: e_hd_div_m_flux_v(3)
!
      character(len=kchara), parameter                                  &
     &                      :: e_hd_div_maxwell = 'div_maxwell'
      character(len=kchara) :: e_hd_div_maxwell_v(3)
!
      character(len=kchara), parameter                                  &
     &                      :: e_hd_div_induct_t = fhd_div_induct_t
      character(len=kchara), parameter :: e_hd_div_induct_v(3)          &
     &            = (/'div_induct_x', 'div_induct_y', 'div_induct_z'/)
!
      character(len=kchara), parameter                                  &
     &                      :: e_hd_mag_induct =   'mag_induct'
      character(len=kchara) :: e_hd_mag_induct_v(3)
!
      character(len=kchara), parameter                                  &
     &                      :: e_hd_mag_diffuse =   'mag_diffuse'
      character(len=kchara) :: e_hd_mag_diffuse_v(3)
!
      character(len=kchara), parameter                                  &
     &                      :: e_hd_vis_diffuse =   'vis_diffuse'
      character(len=kchara) :: e_hd_vis_diffuse_v(3)
!
!
      character(len=kchara), parameter :: e_hd_Lorentz =   'Lorentz'
      character(len=kchara) :: e_hd_Lorentz_v(3)
!
      character(len=kchara), parameter :: e_hd_Coriolis =   'Coriolis'
      character(len=kchara) :: e_hd_Coriolis_v(3)
!
      character(len=kchara) :: e_hd_buoyancy_v(3)
      character(len=kchara) :: e_hd_comp_buo_v(3)
      character(len=kchara) :: e_hd_filter_buo_v(3)
!
!
      character(len=kchara) :: e_hd_ph_flux_v(3)
      character(len=kchara) :: e_hd_h_flux_v(3)
!
      character(len=kchara) :: e_hd_c_flux_v(3)
!
      character(len=kchara), parameter :: e_hd_m_flux_st(6)             &
     &            = (/'m_flux_xx', 'm_flux_xy', 'm_flux_xz',            &
     &                'm_flux_yy', 'm_flux_yz', 'm_flux_zz'/)
      character(len=kchara), parameter :: e_hd_maxwell_st(6)            &
     &            = (/'maxwell_xx', 'maxwell_xy', 'maxwell_xz',         &
     &                'maxwell_yy', 'maxwell_yz', 'maxwell_zz'/)
      character(len=kchara), parameter :: e_hd_induct_at(3)             &
     &            = (/'induct_t_xy', 'induct_t_xz', 'induct_t_yz'/)
!
!
      character(len=kchara) :: e_hd_SGS_hf_v(3)
      character(len=kchara) :: e_hd_SGS_cf_v(3)
!
      character(len=kchara), parameter :: e_hd_SGS_mf_st(6)             &
     &    = (/'SGS_mom_flux_xx', 'SGS_mom_flux_xy', 'SGS_mom_flux_xz',  &
     &        'SGS_mom_flux_yy', 'SGS_mom_flux_yz', 'SGS_mom_flux_zz'/)
      character(len=kchara), parameter :: e_hd_SGS_mxwl_st(6)           &
     &       = (/'SGS_maxwell_xx', 'SGS_maxwell_xy', 'SGS_maxwell_xz',  &
     &           'SGS_maxwell_yy', 'SGS_maxwell_yz', 'SGS_maxwell_zz'/)
      character(len=kchara), parameter :: e_hd_SGS_idct_at(3)           &
     &            = (/'SGS_induct_t_xy', 'SGS_induct_t_xz',             &
     &                'SGS_induct_t_yz'/)
!
!
      character(len=kchara), parameter :: e_hd_SGS_inertia              &
     &                                   = 'SGS_inertia'
      character(len=kchara) :: e_hd_SGS_inertia_v(3)
      character(len=kchara) :: e_hd_SGS_Lorentz_v(3)
!
      character(len=kchara), parameter :: e_hd_SGS_induct               &
     &                                   = 'SGS_induct'
      character(len=kchara) :: e_hd_SGS_induct_v(3)
!
      character(len=kchara), parameter :: e_hd_SGS_vp_induct            &
     &                                   = 'SGS_vp_induct'
      character(len=kchara) :: e_hd_SGS_vp_induct_v(3)
      character(len=kchara) :: e_hd_SGS_buoyancy_v(3)
      character(len=kchara) :: e_hd_SGS_comp_buo_v(3)
!
      character(len=kchara), parameter :: e_hd_ME_generate              &
     &                                   = 'ME_generate'
!
      character(len=kchara), parameter :: e_hd_temp_gen = 'temp_gen'
!
      character(len=kchara), parameter :: e_hd_SGS_m_ene_gen            &
     &                                   = 'SGS_ME_gen'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_vector_vol_average_labels
!
!
      call set_vector_label(e_hd_mag_tension, e_hd_press_grad_v)
      call set_vector_label(e_hd_mag_tension, e_hd_mag_tension_v)
      call set_vector_label(fhd_inertia,     e_hd_inertia_v)
      call set_vector_label(fhd_div_m_flux, e_hd_div_m_flux_v)
      call set_vector_label(e_hd_div_maxwell, e_hd_div_maxwell_v)
!
      call set_vector_label(e_hd_mag_induct, e_hd_mag_induct_v)
!
      call set_vector_label(e_hd_mag_diffuse, e_hd_mag_diffuse_v)
      call set_vector_label(e_hd_vis_diffuse, e_hd_vis_diffuse_v)
!
      call set_vector_label(e_hd_Lorentz,   e_hd_Lorentz_v)
      call set_vector_label(e_hd_Coriolis,  e_hd_Coriolis_v)
      call set_vector_label(fhd_buoyancy,   e_hd_buoyancy_v)
      call set_vector_label(fhd_comp_buo,   e_hd_comp_buo_v)
      call set_vector_label(fhd_filter_buo, e_hd_filter_buo_v)
!
      call set_vector_label(fhd_ph_flux, e_hd_ph_flux_v)
      call set_vector_label(fhd_h_flux,  e_hd_h_flux_v)
!
      call set_vector_label(fhd_c_flux,  e_hd_c_flux_v)
!
      call set_vector_label(fhd_SGS_h_flux,  e_hd_SGS_hf_v)
      call set_vector_label(fhd_SGS_c_flux,  e_hd_SGS_cf_v)
!
      call set_vector_label(e_hd_SGS_inertia,   e_hd_SGS_inertia_v)
      call set_vector_label(fhd_SGS_Lorentz,    e_hd_SGS_Lorentz_v)
      call set_vector_label(e_hd_SGS_induct,    e_hd_SGS_induct_v)
      call set_vector_label(e_hd_SGS_vp_induct, e_hd_SGS_vp_induct_v)
      call set_vector_label(fhd_SGS_buoyancy,   e_hd_SGS_buoyancy_v)
      call set_vector_label(fhd_SGS_comp_buo,   e_hd_SGS_comp_buo_v)
!
      end subroutine set_vector_vol_average_labels
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_vector_label(fid_label, label_v)
!
      use add_direction_labels
!
      character(len=kchara), intent(in) :: fid_label
      character(len=kchara), intent(inout) :: label_v(3)
!
      call add_vector_direction_label_xyz(fid_label,                    &
     &    label_v(1), label_v(2), label_v(3) )
!
      end subroutine set_vector_label
!
! ----------------------------------------------------------------------
!
      subroutine set_sym_tensor_label(fid_label, label_st)
!
      use add_direction_labels
!
      character(len=kchara), intent(in) :: fid_label
      character(len=kchara), intent(inout) :: label_st(6)
!
      call add_tensor_direction_label_xyz(fid_label,                    &
     &    label_st(1), label_st(2), label_st(3),                        &
     &    label_st(4), label_st(5), label_st(6)  )
!
      end subroutine set_sym_tensor_label
!
! ----------------------------------------------------------------------
!
      end module m_volume_average_labels
