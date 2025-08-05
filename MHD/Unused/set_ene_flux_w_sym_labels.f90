!>@file   set_ene_flux_w_sym_labels.f90
!!        module set_ene_flux_w_sym_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!!      subroutine set_ene_flux_w_sym_addresses(i_phys, field_name,     &
!!     &          eflux_sym1_sym2, eflux_asym1_asym2,                   &
!!     &          eflux_sym1_asym2, eflux_asym1_sym2, flag)
!!        type(energy_flux_address), intent(inout) :: eflux_sym1_sym2
!!        type(energy_flux_address), intent(inout) :: eflux_asym1_asym2
!!        type(energy_flux_address), intent(inout) :: eflux_sym1_asym2
!!        type(energy_flux_address), intent(inout) :: eflux_asym1_sym2
!!
!! !!!!!  energy flux names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   u_dot_wsym_x_usym, u_dot_wasym_x_uasym,
!!   u_dot_wsym_x_uasym, u_dot_wasym_x_usym:
!!          Work of Reynolds stress   u \cdot (\omega \times u)
!!   rev_u_dot_Jsym_x_Bsym, rev_u_dot_Jasym_x_Basym,
!!   rev_u_dot_Jsym_x_Basym, rev_u_dot_Jasym_x_Bsym:
!!          Work against Lorentz force       -u \cdot (J \times B)
!!   u_dot_Jsym_x_Bsym, u_dot_Jasym_x_Basym,
!!   u_dot_Jsym_x_Basym, u_dot_Jasym_x_Bsym:
!!          Work of Lorentz force             u \cdot (J \times B)
!!   u_dot_Bsym_nabla_Bsym, u_dot_Basym_nabla_Basym,
!!   u_dot_Bsym_nabla_Basym, u_dot_Basym_nabla_Bsym:
!!          Work of magnetic tension          u \cdot( (B \nabla) B)
!!
!!   sym_termal_buo_flux, asym_termal_buo_flux:
!!          Thermal buoyancy flux            -u \cdot (\alpha_{T} g T)
!!   sym_composite_buo_flux, asym_composite_buo_flux:
!!          Compositional buoyancy flux      -u \cdot (\alpha_{C} g C)
!!
!!   B_rot_Bsym_x_usym, B_rot_Basym_x_uasym,
!!   B_rot_Bsym_x_uasym, B_rot_Basym_x_usym:
!!         Energy flux by magneitic induction
!!                              B \cdot (\nabla \times (u \times B))
!!   B_dot_Bsym_nabla_usym, B_dot_Basym_nabla_uasym,
!!   B_dot_Bsym_nabla_uasym, B_dot_Basym_nabla_usym:
!!        Energy flux by magneitic streatch    B \cdot ((B \nabla) u)
!!
!!   T_usym_nabla_Tsym, T_uasym_nabla_Tasym,
!!   T_usym_nabla_Tasym, T_uasym_nabla_Tsym:
!!                       Heat advection flux   T (u \cdot \nabla) T
!!   pT_usym_nabla_pTsym, pT_uasym_nabla_pTasym,
!!   pT_usym_nabla_pTasym, pT_uasym_nabla_pTsym:
!!       Perturbation of heat advection flux
!!                                     \Theta (u \cdot \nabla) \Theta
!!   C_usym_nabla_Csym, C_uasym_nabla_Casym,
!!   C_usym_nabla_Casym, C_uasym_nabla_Csym:
!!       Composition advection flux            C (u \cdot \nabla) C
!!   pC_usym_nabla_pCsym, pC_uasym_nabla_pCasym,
!!   pC_usym_nabla_pCasym, pC_uasym_nabla_pCsym:
!!   pert_comp_advect:      perturbation of composition advection flux
!!                                   (C-C_0) (u \cdot \nabla) (C-C_0)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module set_ene_flux_w_sym_labels
!
      use m_precision
      use m_constants
      use t_energy_flux_labels
!
      implicit  none
! 
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{symj} \partial_{j} u_{symi}) @f$
!!         or @f$ u \cdot (\omega_{sym} \times u_{sym}) @f$
      type(field_def), parameter :: u_dot_wsym_x_usym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'u_dot_wsym_x_usym',                              &
     &         math = '$ u \cdot (u_{symj} \partial_{j} u_{symi})$, '   &
     &             //' $ u \cdot (\omega_{sym} \times u_{sym})$')
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{asymj} \partial_{j} u_{asymi}) @f$
!!         or @f$ u \cdot (\omega_{asym} \times u_{asym}) @f$
      type(field_def), parameter :: u_dot_wasym_x_uasym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'u_dot_wasym_x_uasym',                            &
     &         math = '$ u \cdot (u_{asymj} \partial_{j} u_{asymi})$, ' &
     &             //' $ u \cdot (\omega_{asym} \times u_{asym})$')
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{symj} \partial_{j} u_{asymi}) @f$
!!         or @f$ u \cdot (\omega_{sym} \times u_{asym}) @f$
      type(field_def), parameter :: u_dot_wsym_x_uasym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'u_dot_wsym_x_uasym',                             &
     &         math = '$ u \cdot (u_{symj} \partial_{j} u_{asymi})$, '  &
     &             //' $ u \cdot (\omega_{sym} \times u_{asym})$')
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{asymj} \partial_{j} u_{symi}) @f$
!!         or @f$ u \cdot (\omega_{asym} \times u_{sym}) @f$
      type(field_def), parameter :: u_dot_wasym_x_usym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'u_dot_wasym_x_usym',                             &
     &         math = '$ u \cdot (u_{asymj} \partial_{j} u_{symi})$, '  &
     &             //' $ u \cdot (\omega_{asym} \times u_{sym})$')
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{sym} \times B_{sym}) @f$
      type(field_def), parameter :: rev_u_dot_Jsym_x_Bsym               &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'rev_u_dot_Jsym_x_Bsym',                       &
     &            math = '$ -u \cdot (J_{sym} \times B_{sym}) $')
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{asym} \times B_{asym}) @f$
      type(field_def), parameter :: rev_u_dot_Jasym_x_Basym             &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'rev_u_dot_Jasym_x_Basym',                     &
     &            math = '$ -u \cdot (J_{asym} \times B_{asym}) $')
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{sym} \times B_{asym}) @f$
      type(field_def), parameter :: rev_u_dot_Jsym_x_Basym              &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'rev_u_dot_Jsym_x_Basym',                      &
     &            math = '$ -u \cdot (J_{sym} \times B_{asym}) $')
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{asym} \times B_{sym}) @f$
      type(field_def), parameter :: rev_u_dot_Jasym_x_Bsym              &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'rev_u_dot_Jasym_x_Bsym',                      &
     &            math = '$ -u \cdot (J_{asym} \times B_{sym}) $')
!
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{sym} \times B_{sym}) @f$
      type(field_def), parameter :: u_dot_Jsym_x_Bsym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Jsym_x_Bsym',                           &
     &            math = '$ u \cdot (J_{sym} \times B_{sym}) $')
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{asym} \times B_{asym}) @f$
      type(field_def), parameter :: u_dot_Jasym_x_Basym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Jasym_x_Basym',                         &
     &            math = '$ u \cdot (J_{asym} \times B_{asym}) $')
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{sym} \times B_{asym}) @f$
      type(field_def), parameter :: u_dot_Jsym_x_Basym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Jsym_x_Basym',                          &
     &            math = '$ u \cdot (J_{sym} \times B_{asym}) $')
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{asym} \times B_{sym}) @f$
      type(field_def), parameter :: u_dot_Jasym_x_Bsym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Jasym_x_Bsym',                          &
     &            math = '$ u \cdot (J_{asym} \times B_{sym}) $')
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_ene_flux_w_sym_addresses(i_phys, field_name,       &
     &          eflux_sym1_sym2, eflux_asym1_asym2,                     &
     &          eflux_sym1_asym2, eflux_asym1_sym2, flag)
!
      use m_energy_flux_w_sym_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(energy_flux_address), intent(inout) :: eflux_sym1_sym2
      type(energy_flux_address), intent(inout) :: eflux_asym1_asym2
      type(energy_flux_address), intent(inout) :: eflux_sym1_asym2
      type(energy_flux_address), intent(inout) :: eflux_asym1_sym2
      logical, intent(inout) :: flag
!
!
      flag = check_ene_fluxes_w_sym(field_name)
      if(flag) then
        if      (field_name .eq. u_dot_wsym_x_usym%name) then
          eflux_sym1_sym2%i_m_advect_work =   i_phys
        else if (field_name .eq. u_dot_wasym_x_uasym%name) then
          eflux_asym1_asym2%i_m_advect_work = i_phys
        else if (field_name .eq. u_dot_wsym_x_uasym%name) then
          eflux_sym1_asym2%i_m_advect_work =  i_phys
        else if (field_name .eq. u_dot_wasym_x_usym%name) then
          eflux_asym1_sym2%i_m_advect_work =  i_phys
!
        else if (field_name .eq. rev_u_dot_Jsym_x_Bsym%name) then
          eflux_sym1_sym2%i_nega_ujb =     i_phys
        else if (field_name .eq. rev_u_dot_Jasym_x_Basym%name) then
          eflux_asym1_asym2%i_nega_ujb =   i_phys
        else if (field_name .eq. rev_u_dot_Jsym_x_Basym%name) then
          eflux_sym1_asym2%i_nega_ujb =    i_phys
        else if (field_name .eq. rev_u_dot_Jasym_x_Bsym%name) then
          eflux_asym1_sym2%i_nega_ujb =    i_phys
!
        else if (field_name .eq. u_dot_Jsym_x_Bsym%name) then
          eflux_sym1_sym2%i_ujb =     i_phys
        else if (field_name .eq. u_dot_Jasym_x_Basym%name) then
          eflux_asym1_asym2%i_ujb =   i_phys
        else if (field_name .eq. u_dot_Jsym_x_Basym%name) then
          eflux_sym1_asym2%i_ujb =    i_phys
        else if (field_name .eq. u_dot_Jasym_x_Bsym%name) then
          eflux_asym1_sym2%i_ujb =    i_phys
!
        else if (field_name .eq. u_dot_Bsym_nabla_Bsym%name) then
          eflux_sym1_sym2%i_m_tension_wk =  i_phys
        else if (field_name .eq. u_dot_Basym_nabla_Basym%name) then
          eflux_asym1_asym2%i_m_tension_wk =  i_phys
        else if (field_name .eq. u_dot_Bsym_nabla_Basym%name) then
          eflux_sym1_asym2%i_m_tension_wk =  i_phys
        else if (field_name .eq. u_dot_Basym_nabla_Bsym%name) then
          eflux_asym1_sym2%i_m_tension_wk =  i_phys
!
        else if (field_name .eq. sym_termal_buo_flux%name) then
          eflux_sym1_sym2%i_buo_gen =   i_phys
        else if (field_name .eq. asym_termal_buo_flux%name) then
          eflux_asym1_asym2%i_buo_gen =   i_phys
!
        else if (field_name .eq. sym_composite_buo_flux%name) then
          eflux_sym1_sym2%i_c_buo_gen =   i_phys
        else if (field_name .eq. asym_composite_buo_flux%name) then
          eflux_asym1_asym2%i_c_buo_gen =   i_phys
!
        else if (field_name .eq. B_rot_Bsym_x_usym%name) then
          eflux_sym1_sym2%i_me_gen =   i_phys
        else if (field_name .eq. B_rot_Basym_x_uasym%name) then
          eflux_asym1_asym2%i_me_gen = i_phys
        else if (field_name .eq. B_rot_Bsym_x_uasym%name) then
          eflux_sym1_asym2%i_me_gen =  i_phys
        else if (field_name .eq. B_rot_Basym_x_usym%name) then
          eflux_asym1_sym2%i_me_gen =  i_phys
!
        else if (field_name .eq. B_dot_Bsym_nabla_usym%name) then
          eflux_sym1_sym2%i_mag_stretch_flux =   i_phys
        else if (field_name .eq. B_dot_Basym_nabla_uasym%name) then
          eflux_asym1_asym2%i_mag_stretch_flux = i_phys
        else if (field_name .eq. B_dot_Bsym_nabla_uasym%name) then
          eflux_sym1_asym2%i_mag_stretch_flux =  i_phys
        else if (field_name .eq. B_dot_Basym_nabla_usym%name) then
          eflux_asym1_sym2%i_mag_stretch_flux =  i_phys
!
        else if (field_name .eq. T_usym_nabla_Tsym%name) then
          eflux_sym1_sym2%i_temp_gen =  i_phys
        else if (field_name .eq. T_uasym_nabla_Tasym%name) then
          eflux_asym1_asym2%i_temp_gen =  i_phys
        else if (field_name .eq. T_usym_nabla_Tasym%name) then
          eflux_sym1_asym2%i_temp_gen =  i_phys
        else if (field_name .eq. T_uasym_nabla_Tsym%name) then
          eflux_asym1_sym2%i_temp_gen =  i_phys
!
        else if (field_name .eq. pT_usym_nabla_pTsym%name) then
          eflux_sym1_sym2%i_par_t_gen = i_phys
        else if (field_name .eq. pT_uasym_nabla_pTasym%name) then
          eflux_asym1_asym2%i_par_t_gen = i_phys
        else if (field_name .eq. pT_usym_nabla_pTasym%name) then
          eflux_sym1_asym2%i_par_t_gen = i_phys
        else if (field_name .eq. pT_uasym_nabla_pTsym%name) then
          eflux_asym1_sym2%i_par_t_gen = i_phys
!
        else if (field_name .eq. C_usym_nabla_Csym%name) then
          eflux_sym1_sym2%i_comp_gen =    i_phys
        else if (field_name .eq. C_uasym_nabla_Casym%name) then
          eflux_asym1_asym2%i_comp_gen =  i_phys
        else if (field_name .eq. C_usym_nabla_Casym%name) then
          eflux_sym1_asym2%i_comp_gen =   i_phys
        else if (field_name .eq. C_uasym_nabla_Csym%name) then
          eflux_asym1_sym2%i_comp_gen =   i_phys
!
        else if (field_name .eq. pC_usym_nabla_pCsym%name) then
          eflux_sym1_sym2%i_par_c_gen =   i_phys
        else if (field_name .eq. pC_uasym_nabla_pCasym%name) then
          eflux_asym1_asym2%i_par_c_gen = i_phys
        else if (field_name .eq. pC_usym_nabla_pCasym%name) then
          eflux_sym1_asym2%i_par_c_gen =  i_phys
        else if (field_name .eq. pC_uasym_nabla_pCsym%name) then
          eflux_asym1_sym2%i_par_c_gen =  i_phys
        end if
      end if
!
      end subroutine set_ene_flux_w_sym_addresses
!
! ----------------------------------------------------------------------
!
      logical function check_ene_fluxes_w_sym(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_ene_fluxes_w_sym = .FALSE.
      if (    (field_name .eq. u_dot_wsym_x_usym%name)                  &
     &   .or. (field_name .eq. u_dot_wasym_x_uasym%name)                &
     &   .or. (field_name .eq. u_dot_wsym_x_uasym%name)                 &
     &   .or. (field_name .eq. u_dot_wasym_x_usym%name)                 &
!
     &   .or. (field_name .eq. rev_u_dot_Jsym_x_Bsym%name)              &
     &   .or. (field_name .eq. rev_u_dot_Jasym_x_Basym%name)            &
     &   .or. (field_name .eq. rev_u_dot_Jsym_x_Basym%name)             &
     &   .or. (field_name .eq. rev_u_dot_Jasym_x_Bsym%name)             &
!
     &   .or. (field_name .eq. u_dot_Jsym_x_Bsym%name)                  &
     &   .or. (field_name .eq. u_dot_Jasym_x_Basym%name)                &
     &   .or. (field_name .eq. u_dot_Jsym_x_Basym%name)                 &
     &   .or. (field_name .eq. u_dot_Jasym_x_Bsym%name)                 &
!
     &   )   check_ene_fluxes_w_sym = .TRUE.
!
      end function check_ene_fluxes_w_sym
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ene_flux_w_symmetry_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(u_dot_wsym_x_usym,       array_c2i)
      call set_field_label_to_ctl(u_dot_wasym_x_uasym,     array_c2i)
      call set_field_label_to_ctl(u_dot_wsym_x_uasym,      array_c2i)
      call set_field_label_to_ctl(u_dot_wasym_x_usym,      array_c2i)
!
      call set_field_label_to_ctl(rev_u_dot_Jsym_x_Bsym,   array_c2i)
      call set_field_label_to_ctl(rev_u_dot_Jasym_x_Basym, array_c2i)
      call set_field_label_to_ctl(rev_u_dot_Jsym_x_Basym,  array_c2i)
      call set_field_label_to_ctl(rev_u_dot_Jasym_x_Bsym,  array_c2i)
!
      call set_field_label_to_ctl(u_dot_Jsym_x_Bsym,       array_c2i)
      call set_field_label_to_ctl(u_dot_Jasym_x_Basym,     array_c2i)
      call set_field_label_to_ctl(u_dot_Jsym_x_Basym,      array_c2i)
      call set_field_label_to_ctl(u_dot_Jasym_x_Bsym,      array_c2i)
!
      end subroutine set_ene_flux_w_symmetry_names
!
! ----------------------------------------------------------------------
!
      end module set_ene_flux_w_sym_labels
