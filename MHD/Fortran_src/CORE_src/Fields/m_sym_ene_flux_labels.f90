!>@file   m_sym_ene_flux_labels.f90
!!        module m_sym_ene_flux_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for energy fluxes by sym field
!!
!!@verbatim
!!      logical function check_filter_enegy_fluxes(field_name)
!!
!!      subroutine set_sym_ene_flux_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!!  List of energy flux !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Symmetric energy fluxes by rot(F_asym) X F_asym !!!!!!!!!!!!!!!
!!
!!   mns_us_d_ws_x_ua  [eflux_s_sxa%i_m_advect_work]
!!       : Work of inertia:              -u_s \cdot (\omega_s \times u_a)
!!   us_d_js_x_ba      [eflux_s_sxa%i_ujb]
!!       :  Work of Lorentz force:        u_s \cdot (J_s \times B_a)
!!
!!   us_d_ws_x_ua      [eflux_s_sxa%i_uwu]
!!       : Work against of inertia:       u_s \cdot (\omega_s \times u_a)
!!   mns_us_d_js_x_ba  [eflux_s_sxa%i_nega_ujb]
!!       :  Work against Lorentz force:  -u_s \cdot (J_s \times B_a)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Symmetric energy fluxes by rot(F_sym) X F_sym !!!!!!!!!!!!!!!
!!
!!   mns_us_d_wa_x_us  [eflux_s_axs%i_m_advect_work]
!!       : Work of inertia:              -u_s \cdot (\omega_a \times u_s)
!!   mns_us_d_z_x_us   [i_Coriolis_work]
!!       :  Work of Coriolis force:      -2u_s \cdot (\Omega \times u_s)
!!   us_d_ja_x_bs      [eflux_s_axs%i_ujb]
!!       :  Work of Lorentz force:          u_s \cdot (J_a \times B_s)
!!
!!   us_d_wa_x_us      [eflux_s_axs%i_uwu]
!!       : Work against of inertia:       u_s \cdot (\omega_a \times u_s)
!!   us_d_z_x_us       [eflux_s_axs%i_work_against_Coriolis]
!!       : Work against of Coriolis force: 2u_s \cdot (\Omega \times u_s)
!!   mns_us_d_ja_x_bs  [eflux_s_axs%i_nega_ujb]
!!       :  Work against Lorentz force:    -u_s \cdot (J_a \times B_s)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Antisymmetric energy fluxes by rot(F_asym) X F_sym !!!!!!!!!!!!!!!
!!
!!   mns_ua_d_ws_x_us  [eflux_a_sxs%i_m_advect_work]
!!       : Work of inertia:              -u_a \cdot (\omega_s \times u_s)
!!   ua_d_ws_x_us      [eflux_a_sxs%i_ujb]
!!       :  Work of Lorentz force:          u_a \cdot (J_s \times B_s)
!!
!!   ua_d_js_x_bs      [eflux_a_sxs%i_uwu]
!!       : Work against of inertia:       u_a \cdot (\omega_s \times u_s)
!!   mns_ua_d_js_x_bs  [eflux_a_sxs%i_nega_ujb]
!!       :  Work against Lorentz force:    -u_a \cdot (J_s \times B_s)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Antisymmetric energy fluxes by rot(F_sym) X F_asym !!!!!!!!!!!!!!!
!!
!!   mns_ua_d_wa_x_ua  [eflux_a_axa%i_m_advect_work]
!!       : Work of inertia:              -u_a \cdot (\omega_a \times u_a)
!!   mns_ua_d_z_x_ua   [i_Coriolis_work]
!!       :  Work of Coriolis force:       -2u_a \cdot (\Omega \times u_a)
!!   ua_d_ja_x_ba      [eflux_a_axa%i_ujb]
!!       :  Work of Lorentz force:          u_a \cdot (J_a \times B_a)
!!
!!   ua_d_wa_x_ua      [eflux_a_axa%i_uwu]
!!       : Work against of inertia:       u_a \cdot (\omega_a \times u_a)
!!   ua_d_z_x_ua       [eflux_a_axa%i_work_against_Coriolis]
!!       : Work against of Coriolis force: 2u_a \cdot (\Omega \times u_a)
!!   mns_ua_d_ja_x_ba  [eflux_a_axa%i_nega_ujb]
!!       :  Work against Lorentz force:    -u_a \cdot (J_a \times B_a)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!sym_buoyancy_flux
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
        module m_sym_ene_flux_labels
!
        use m_precision
        use m_phys_constants
        use t_field_labels
!
        implicit  none
!
!>        Field label of work of inertia
!!         @f$ -u_{Si} (u_{Sj} \partial_{j} u_{Ai}) @f$,
!!         @f$ -u_{Si} (e_{ijk} omega_{Sj} u_{Ak}) @f$
        type(field_def), parameter :: mns_us_d_ws_x_ua                  &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = '-us_d_ws_x_ua',                        &
        &                math = '$-u_{Si} (e_{ijk}'                     &
        &                     //  ' \omega_{Sj} u_{Ak})$')
!>        Field label of work of inertia
!!         @f$ -u_{Si} (u_{Aj} \partial_{j} u_{Si}) @f$,
!!         @f$ -u_{Si} (e_{ijk} omega_{Aj} u_{Sk}) @f$
        type(field_def), parameter :: mns_us_d_wa_x_us                  &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = '-us_d_wa_x_us',                        &
        &                math = '$-u_{iS} (e_{ijk}'                     &
        &                     //  ' \omega_{Aj} u_{Sk})$')
!>        Field label of work of inertia
!!         @f$ -u_{Ai} (u_{Sj} \partial_{Sj} u_{Si}) @f$,
!!         @f$ -u_{Ai} (e_{ijk} omega_{Sj} u_{Sk}) @f$
        type(field_def), parameter :: mns_ua_d_ws_x_us                  &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = '-ua_d_ws_x_us',                        &
        &                math = '$-u_{Ai} (e_{ijk}'                     &
        &                     //  ' \omega_{Sj} u_{Sk})$')
!>        Field label of work of inertia
!!         @f$ -u_{Ai} (u_{j} \partial_{Aj} u_{Ai}) @f$,
!!         @f$ -u_{Ai} (e_{ijk} omega_{Aj} u_{Ak}) @f$
        type(field_def), parameter :: mns_ua_d_wa_x_ua                  &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = '-ua_d_wa_x_ua',                        &
        &                math = '$-u_{Ai} (e_{ijk}'                     &
        &                     //  ' \omega_{Aj} u_{Ak})$')
!
!>        Field label of work against of inertia
!!         @f$ u_{Si} (u_{Sj} \partial_{j} u_{Ai}) @f$,
!!         @f$ u_{Si} (e_{ijk} omega_{Sj} u_{Ak}) @f$
        type(field_def), parameter :: us_d_ws_x_ua                      &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = 'us_d_ws_x_ua',                         &
        &                math = '$ u_{Si} (e_{ijk}'                     &
        &                     //  ' \omega_{Sj} u_{Ak})$')
!>        Field label of work against of inertia
!!         @f$ u_{Si} (u_{Aj} \partial_{j} u_{Si}) @f$,
!!         @f$ u_{Si} (e_{ijk} omega_{Aj} u_{Sk}) @f$
        type(field_def), parameter :: us_d_wa_x_us                      &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = 'us_d_wa_x_us',                         &
        &                math = '$ u_{Si} (e_{ijk}'                     &
        &                     //  ' \omega_{Aj} u_{Sk})$')
!>        Field label of work against of inertia
!!         @f$ u_{Ai} (u_{Sj} \partial_{Sj} u_{Si}) @f$,
!!         @f$ u_{Ai} (e_{ijk} omega_{Sj} u_{Sk}) @f$
        type(field_def), parameter :: ua_d_ws_x_us                      &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = 'ua_d_ws_x_us',                         &
        &                math = '$ u_{Ai} (e_{ijk}'                     &
        &                     //  ' \omega_{Sj} u_{Sk})$')
!>        Field label of work against of inertia
!!         @f$ -u_{Ai} (u_{j} \partial_{Aj} u_{Ai}) @f$,
!!         @f$ -u_{Ai} (e_{ijk} omega_{Aj} u_{Ak}) @f$
        type(field_def), parameter :: ua_d_wa_x_ua                      &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = 'ua_d_wa_x_ua',                         &
        &                math = '$ u_{Ai} (e_{ijk}'                     &
        &                     //  ' \omega_{Aj} u_{Ak})$')
!
!
!>        Field label of work of Coriolis force
!!         @f$ -2\Omega u_{Si} (e_{ijk} \hat{z}_{Sj} u_{Ak}) @f$
!        type(field_def), parameter :: mns_us_d_z_x_ua                  &
!        &    = field_def(n_comp = n_scalar,                            &
!        &                name = '-us_d_z_x_ua',                        &
!        &                math = '$-2\Omega u_{Si} (e_{ijk}'            &
!        &                     //  ' \hat{z}_{Sj} u_{Ak})$')
!>        Field label of work of Coriolis force
!!         @f$ -2\Omega u_{Si} (e_{ijk} \hat{z}_{Aj} u_{Sk}) @f$
        type(field_def), parameter :: mns_us_d_z_x_us                   &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = '-us_d_z_x_us',                         &
        &                math = '$-2\Omega  u_{iS} (e_{ijk}'            &
        &                     //  ' \hat{z}_{Aj} u_{Sk})$')
!>        Field label of work of Coriolis force
!!         @f$ -2\Omega u_{Ai} (e_{ijk} \hat{z}_{Sj} u_{Sk}) @f$
!        type(field_def), parameter :: mns_ua_d_z_x_us                  &
!        &    = field_def(n_comp = n_scalar,                            &
!        &                name = '-ua_d_z_x_us',                        &
!        &                math = '$-2\Omega u_{Ai} (e_{ijk}'            &
!        &                     //  ' \hat{z}_{Sj} u_{Sk})$')
!>        Field label of work of Coriolis force
!!         @f$ -2\Omega u_{Ai} (e_{ijk} \hat{z}_{Aj} u_{Ak}) @f$
        type(field_def), parameter :: mns_ua_d_z_x_ua                   &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = '-ua_d_z_x_ua',                         &
        &                math = '$-2\Omega u_{Ai} (e_{ijk}'             &
        &                     //  ' \hat{z}_{Aj} u_{Ak})$')
!
!>        Field label of work against of Coriolis force
!!         @f$ 2\Omega u_{Si} (e_{ijk} \hat{z}_{Sj} u_{Ak}) @f$
!        type(field_def), parameter :: us_d_z_x_ua                      &
!        &    = field_def(n_comp = n_scalar,                            &
!        &                name = ' us_d_z_x_ua',                        &
!        &                math = '$ 2\Omega u_{Si} (e_{ijk}'            &
!        &                     //  ' \hat{z}_{Sj} u_{Ak})$')
!>        Field label of work against of Coriolis force
!!         @f$ 2\Omega u_{Si} (e_{ijk} \hat{z}_{Aj} u_{Sk}) @f$
        type(field_def), parameter :: us_d_z_x_us                       &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = ' us_d_z_x_us',                         &
        &                math = '$ 2\Omega  u_{iS} (e_{ijk}'            &
        &                     //  ' \hat{z}_{Aj} u_{Sk})$')
!>        Field label of work against of Coriolis force
!!         @f$ 2\Omega u_{Ai} (e_{ijk} \hat{z}_{Sj} u_{Sk}) @f$
!        type(field_def), parameter :: mns_ua_d_z_x_us                  &
!        &    = field_def(n_comp = n_scalar,                            &
!        &                name = ' ua_d_z_x_us',                        &
!        &                math = '$ 2\Omega u_{Ai} (e_{ijk}'            &
!        &                     //  ' \hat{z}_{Sj} u_{Sk})$')
!>        Field label of work against of Coriolis force
!!         @f$ 2\Omega u_{Ai} (e_{ijk} \hat{z}_{Aj} u_{Ak}) @f$
        type(field_def), parameter :: ua_d_z_x_ua                       &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = ' ua_d_z_x_ua',                         &
        &                math = '$ 2\Omega u_{Ai} (e_{ijk}'             &
        &                     //  ' \hat{z}_{Aj} u_{Ak})$')
!
!
!>        Field label of work of Lorentz force
!!         @f$ u_{symi} (e_{ijk} J_{symj} B_{asymk}) @f$
        type(field_def), parameter :: us_d_js_x_ba                      &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = 'us_d_js_x_ba',                         &
        &         math = '$ u_{symi} (e_{ijk} J_{symj} B_{asymk})$')
!>        Field label of work of Lorentz force
!!         @f$ u_{symi} (e_{ijk} J_{asymj} B_{symk}) @f$
        type(field_def), parameter :: us_d_ja_x_bs                      &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = 'us_d_ja_x_bs',                         &
        &         math = '$ u_{symi} (e_{ijk} J_{asymj} B_{symk})$')
!>        Field label of work of Lorentz force
!!         @f$ u_{asymi} (e_{ijk} J_{symj} B_{symk}) @f$
        type(field_def), parameter :: ua_d_js_x_bs                      &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = 'ua_d_js_x_bs',                         &
        &         math = '$ u_{asymi} (e_{ijk} J_{symj} B_{symk})$')
!>        Field label of work of Lorentz force
!!         @f$ u_{asymi} (e_{ijk} J_{asymj} B_{asymk}) @f$
        type(field_def), parameter :: ua_d_ja_x_ba                      &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = 'ua_d_ja_x_ba',                         &
        &         math = '$ u_{asymi} (e_{ijk} J_{asymj} B_{asymk})$')
!
!>        Field label of work against of Lorentz force
!!         @f$ -u_{symi} (e_{ijk} J_{symj} B_{asymk}) @f$
        type(field_def), parameter :: mns_us_d_js_x_ba                  &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = '-us_d_js_x_ba',                        &
        &         math = '$-u_{symi} (e_{ijk} J_{symj} B_{asymk})$')
!>        Field label of work against of Lorentz force
!!         @f$ -u_{symi} (e_{ijk} J_{asymj} B_{symk}) @f$
        type(field_def), parameter :: mns_us_d_ja_x_bs                  &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = '-us_d_ja_x_bs',                        &
        &         math = '$-u_{symi} (e_{ijk} J_{asymj} B_{symk})$')
!>        Field label of work against of Lorentz force
!!         @f$ -u_{asymi} (e_{ijk} J_{symj} B_{symk}) @f$
        type(field_def), parameter :: mns_ua_d_js_x_bs                  &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = '-ua_d_js_x_bs',                        &
        &         math = '$-u_{asymi} (e_{ijk} J_{symj} B_{symk})$')
!>        Field label of work against of Lorentz force
!!         @f$ -u_{asymi} (e_{ijk} J_{asymj} B_{asymk}) @f$
        type(field_def), parameter :: mns_ua_d_ja_x_ba                  &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = '-ua_d_ja_x_ba',                        &
        &         math = '$-u_{asymi} (e_{ijk} J_{asymj} B_{asymk})$')
!
! 
!>        Field label for symmetric buoyancy flux
!!         @f$ -u_{symi} \alpha_{T} g_{i} T_{sym} @f$
        type(field_def), parameter :: sym_buoyancy_flux                 &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = 'sym_buoyancy_flux',                    &
        &            math = '$ -u_{symi} \alpha_{T} g_{i} T_{sym} $')
!>        Field label for anti-symmetric buoyancy flux
!!         @f$ -u_{asymi} \alpha_{T} g_{i} T_{asym} @f$
        type(field_def), parameter :: asym_buoyancy_flux                &
        &    = field_def(n_comp = n_scalar,                             &
        &                name = 'asym_buoyancy_flux',                   &
        &            math = '$ -u_{asymi} \alpha_{T} g_{i} T_{asym} $')
!
!>        Field label of buoyancy flux
!!         @f$ -u \cdot (\alpha_{T} g_{i} T_{sym}) @f$
      type(field_def), parameter :: sym_termal_buo_flux                 &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'sym_termal_buo_flux',                         &
     &            math = '$ -u \cdot (\alpha_{T} g_{i} T_{sym}) $')
!>        Field label of buoyancy flux
!!         @f$ -u \cdot (\alpha_{T} g_{i} T_{asym}) @f$
      type(field_def), parameter :: asym_termal_buo_flux                &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'asym_termal_buo_flux',                        &
     &            math = '$ -u \cdot (\alpha_{T} g_{i} T_{asym}) $')
!
!>        Field label of compositional buoyancy flux
!!         @f$ -u \cdot (\alpha_{C} g_{i} C_{sym}) @f$
      type(field_def), parameter :: sym_composite_buo_flux              &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'sym_composite_buo_flux',                      &
     &            math = '$ -u \cdot (\alpha_{C} g_{i} C_{sym}) $')
!>        Field label of compositional buoyancy flux
!!         @f$ -u \cdot (\alpha_{C} g_{i} C_{asym}) @f$
      type(field_def), parameter :: asym_composite_buo_flux             &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'asym_composite_buo_flux',                     &
     &            math = '$ -u \cdot (\alpha_{C} g_{i} C_{asym}) $')
!
! ----------------------------------------------------------------------
!
        contains
!
! ----------------------------------------------------------------------
!
        logical function check_enegy_fluxes_w_sym(field_name)
!
        character(len = kchara), intent(in) :: field_name
!
!
        check_enegy_fluxes_w_sym                                        &
        &   =    (field_name .eq. mns_us_d_ws_x_ua%name)                &
        &   .or. (field_name .eq. mns_us_d_wa_x_us%name)                &
        &   .or. (field_name .eq. mns_ua_d_ws_x_us%name)                &
        &   .or. (field_name .eq. mns_ua_d_wa_x_ua%name)                &
!
        &   .or. (field_name .eq. us_d_js_x_ba%name)                    &
        &   .or. (field_name .eq. us_d_ja_x_bs%name)                    &
        &   .or. (field_name .eq. ua_d_js_x_bs%name)                    &
        &   .or. (field_name .eq. ua_d_ja_x_ba%name)                    &
!
        &   .or. (field_name .eq. mns_us_d_z_x_us%name)                 &
        &   .or. (field_name .eq. mns_ua_d_z_x_ua%name)                 &
!
        &   .or. (field_name .eq. us_d_ws_x_ua%name)                    &
        &   .or. (field_name .eq. us_d_wa_x_us%name)                    &
        &   .or. (field_name .eq. ua_d_ws_x_us%name)                    &
        &   .or. (field_name .eq. ua_d_wa_x_ua%name)                    &
!
        &   .or. (field_name .eq. mns_us_d_js_x_ba%name)                &
        &   .or. (field_name .eq. mns_us_d_ja_x_bs%name)                &
        &   .or. (field_name .eq. mns_ua_d_js_x_bs%name)                &
        &   .or. (field_name .eq. mns_ua_d_ja_x_ba%name)                &
!
        &   .or. (field_name .eq. us_d_z_x_us%name)                     &
        &   .or. (field_name .eq. ua_d_z_x_ua%name)                     &
!
        &   .or. (field_name .eq. sym_buoyancy_flux%name)               &
        &   .or. (field_name .eq. asym_buoyancy_flux%name)
!
        end function check_enegy_fluxes_w_sym
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_sym_ene_flux_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(sym_buoyancy_flux,       array_c2i)
      call set_field_label_to_ctl(asym_buoyancy_flux,      array_c2i)
      call set_field_label_to_ctl(sym_termal_buo_flux,     array_c2i)
      call set_field_label_to_ctl(asym_termal_buo_flux,    array_c2i)
      call set_field_label_to_ctl(sym_composite_buo_flux,  array_c2i)
      call set_field_label_to_ctl(asym_composite_buo_flux, array_c2i)
!
      call set_field_label_to_ctl(us_d_js_x_ba,       array_c2i)
      call set_field_label_to_ctl(us_d_ja_x_bs,       array_c2i)
      call set_field_label_to_ctl(ua_d_js_x_bs,       array_c2i)
      call set_field_label_to_ctl(ua_d_ja_x_ba,       array_c2i)
!
      call set_field_label_to_ctl(mns_us_d_ws_x_ua,   array_c2i)
      call set_field_label_to_ctl(mns_us_d_wa_x_us,   array_c2i)
      call set_field_label_to_ctl(mns_ua_d_ws_x_us,   array_c2i)
      call set_field_label_to_ctl(mns_ua_d_wa_x_ua,   array_c2i)
!
      call set_field_label_to_ctl(mns_us_d_z_x_us,    array_c2i)
      call set_field_label_to_ctl(mns_ua_d_z_x_ua,    array_c2i)
!
      call set_field_label_to_ctl(mns_us_d_js_x_ba,   array_c2i)
      call set_field_label_to_ctl(mns_us_d_ja_x_bs,   array_c2i)
      call set_field_label_to_ctl(mns_ua_d_js_x_bs,   array_c2i)
      call set_field_label_to_ctl(mns_ua_d_ja_x_ba,   array_c2i)
!
      call set_field_label_to_ctl(us_d_ws_x_ua,       array_c2i)
      call set_field_label_to_ctl(us_d_wa_x_us,       array_c2i)
      call set_field_label_to_ctl(ua_d_ws_x_us,       array_c2i)
      call set_field_label_to_ctl(ua_d_wa_x_ua,       array_c2i)
!
      call set_field_label_to_ctl(us_d_z_x_us,        array_c2i)
      call set_field_label_to_ctl(ua_d_z_x_ua,        array_c2i)
!
      end subroutine set_sym_ene_flux_names
!
! ----------------------------------------------------------------------
!
        end module m_sym_ene_flux_labels
