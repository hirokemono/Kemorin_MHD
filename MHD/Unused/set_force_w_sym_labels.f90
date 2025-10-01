!>@file   set_force_w_sym_labels.f90
!!        module set_force_w_sym_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses of basic forces
!!
!!@verbatim
!!      subroutine set_force_w_sym_addresses(i_phys, field_name,        &
!!     &          force_sym1_sym2, force_asym1_asym2,                   &
!!     &          force_sym1_asym2, force_asym1_sym2, flag)
!!      subroutine set_flux_tensor_w_sym_addresses(i_phys, field_name,  &
!!     &          force_sym1_sym2, force_asym1_asym2, force_sym1_asym2, &
!!     &          flag)
!!        type(base_force_address), intent(inout) :: force_sym1_sym2
!!        type(base_force_address), intent(inout) :: force_asym1_asym2
!!        type(base_force_address), intent(inout) :: force_sym1_asym2
!!        type(base_force_address), intent(inout) :: force_asym1_sym2
!!
!! !!!!!  Base field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   inertia
!!                 :        inertia (\omega \times u)
!!   momentum_flux
!!                 :  momentum flux     u_{i} u_{j}
!!   Lorentz_force
!!                 :  Lorentz force     J \times B
!!   magnetic_tension
!!                 :  magnetic tension   (B \nabla) B
!!   maxwell_tensor_sym_sym, maxwell_tensor_asym_asym,
!!   maxwell_tensor_sym_asym
!!                 :  maxwell tensor       B_{i} B_{j}
!!
!!   sym_thermal_buoyancy, asym_thermal_buoyancy
!!                 :   Thermal buoyancy       - \alpha_{T} g T
!!   sym_composite_buoyancy, asym_composite_buoyancy
!!                 :   compositional buoyancy  - \alpha_{C} g C
!!
!!   usym_x_Bsym, uasym_x_Basym, usym_x_Basym, uasym_x_Bsym
!!                 :     induction                           u \times B
!!   Bsym_nabla_usym, Basym_nabla_uasym,
!!   Bsym_nabla_uasym, Basym_nabla_usym
!!                 :    magneitic streatch         (B \nabla) u
!!   usym_Bsym, uasym_Basym, usym_Basym
!!                 :    induction induction tensor
!!                                 u_{i} B_{j}  - B_{i} u_{J}
!!
!!   usym_nabla_Tsym, uasym_nabla_Tasym,
!!   usym_nabla_Tasym, uasym_nabla_Tsym
!!                 :    heat advection     (u \cdot \nabla) T
!!   usym_nabla_pTsym, uasym_nabla_pTasym,
!!   usym_nabla_pTasym, uasym_nabla_pTsym
!!                 :  perturbation of heat advection
!!                                      (u \cdot \nabla) \Theta
!!   heat_flux_sym_sym, heat_flux_asym_asym,
!!   heat_flux_sym_asym, heat_flux_asym_sym
!!                 :    heat flux                   uT
!!   pert_h_flux_sym_sym, pert_h_flux_asym_asym
!!   pert_h_flux_sym_asym, pert_h_flux_asym_sym
!!                 :  perturbation of heat flux   u\Theta
!!
!!   usym_nabla_Csym, uasym_nabla_Casym
!!   usym_nabla_Casym, uasym_nabla_Csym
!!                 :    composition advection     (u \cdot \nabla) C
!!   usym_nabla_pCsym, uasym_nabla_pCasym,
!!   usym_nabla_pCasym, uasym_nabla_pCsym
!!                 :  perturbation of composition advection
!!                                      (u \cdot \nabla) (C-C_0)
!!   composite_flux_sym_sym, composite_flux_asym_asym, 
!!   composite_flux_sym_asym, composite_flux_asym_sym
!!                 :    composition flux                   uC
!!   pert_c_flux_sym_sym, pert_c_flux_asym_asym,
!!   pert_c_flux_sym_asym, pert_c_flux_asym_sym
!!                 :  perturbation of composition flux   u(C-C_0)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module set_force_w_sym_labels
!
      use m_precision
      use m_constants
      use t_base_force_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_force_w_sym_addresses(i_phys, field_name,          &
     &          force_sym1_sym2, force_asym1_asym2,                     &
     &          force_sym1_asym2, force_asym1_sym2, flag)
!
      use m_force_w_sym_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: force_sym1_sym2
      type(base_force_address), intent(inout) :: force_asym1_asym2
      type(base_force_address), intent(inout) :: force_sym1_asym2
      type(base_force_address), intent(inout) :: force_asym1_sym2
      logical, intent(inout) :: flag
!
!
      flag = check_forces_w_sym(field_name)
      if(flag) then
        if      (field_name .eq. wsym_x_usym%name) then
          force_sym1_sym2%i_m_advect =   i_phys
        else if (field_name .eq. wasym_x_uasym%name) then
          force_asym1_asym2%i_m_advect =   i_phys
        else if (field_name .eq. wsym_x_uasym%name) then
          force_sym1_asym2%i_m_advect =   i_phys
        else if (field_name .eq. wasym_x_usym%name) then
          force_asym1_sym2%i_m_advect =   i_phys
!
        else if (field_name .eq. Jsym_x_Bsym%name) then
          force_sym1_sym2%i_lorentz =    i_phys
        else if (field_name .eq. Jasym_x_Basym%name) then
          force_asym1_asym2%i_lorentz =    i_phys
        else if (field_name .eq. Jsym_x_Basym%name) then
          force_sym1_asym2%i_lorentz =    i_phys
        else if (field_name .eq. Jasym_x_Bsym%name) then
          force_asym1_sym2%i_lorentz =    i_phys
!
        else if (field_name .eq. Bsym_nabla_Bsym%name) then
          force_sym1_sym2%i_m_tension =  i_phys
        else if (field_name .eq. Basym_nabla_Basym%name) then
          force_asym1_asym2%i_m_tension =  i_phys
        else if (field_name .eq. Bsym_nabla_Basym%name) then
          force_sym1_asym2%i_m_tension =  i_phys
        else if (field_name .eq. Basym_nabla_Bsym%name) then
          force_asym1_sym2%i_m_tension =  i_phys
!
        else if (field_name .eq. sym_thermal_buoyancy%name) then
          force_sym1_sym2%i_buoyancy =   i_phys
        else if (field_name .eq. asym_thermal_buoyancy%name) then
          force_asym1_asym2%i_buoyancy =   i_phys
!
        else if (field_name .eq. sym_composite_buoyancy%name) then
          force_sym1_sym2%i_comp_buo =   i_phys
        else if (field_name .eq. asym_composite_buoyancy%name) then
          force_asym1_asym2%i_comp_buo =   i_phys
!
        else if (field_name .eq. usym_x_Bsym%name) then
          force_sym1_sym2%i_vp_induct =    i_phys
        else if (field_name .eq. uasym_x_Basym%name) then
          force_asym1_asym2%i_vp_induct =  i_phys
        else if (field_name .eq. usym_x_Basym%name) then
          force_sym1_asym2%i_vp_induct =   i_phys
        else if (field_name .eq. uasym_x_Bsym%name) then
          force_asym1_sym2%i_vp_induct =   i_phys
!
        else if (field_name .eq. rot_usym_x_Bsym%name) then
          force_sym1_sym2%i_induction =    i_phys
        else if (field_name .eq. rot_uasym_x_Basym%name) then
          force_asym1_asym2%i_induction =  i_phys
        else if (field_name .eq. rot_usym_x_Basym%name) then
          force_sym1_asym2%i_induction =   i_phys
        else if (field_name .eq. rot_uasym_x_Bsym%name) then
          force_asym1_sym2%i_induction =   i_phys
!
        else if (field_name .eq. Bsym_nabla_usym%name) then
          force_sym1_sym2%i_mag_stretch =   i_phys
        else if (field_name .eq. Basym_nabla_uasym%name) then
          force_asym1_asym2%i_mag_stretch = i_phys
        else if (field_name .eq. Bsym_nabla_uasym%name) then
          force_sym1_asym2%i_mag_stretch =  i_phys
        else if (field_name .eq. Basym_nabla_usym%name) then
          force_asym1_sym2%i_mag_stretch =  i_phys
!
        else if (field_name .eq. usym_nabla_Tsym%name) then
          force_sym1_sym2%i_h_advect =  i_phys
        else if (field_name .eq. uasym_nabla_Tasym%name) then
          force_asym1_asym2%i_h_advect =  i_phys
        else if (field_name .eq. usym_nabla_Tasym%name) then
          force_sym1_asym2%i_h_advect =  i_phys
        else if (field_name .eq. uasym_nabla_Tsym%name) then
          force_asym1_sym2%i_h_advect =  i_phys
!
        else if (field_name .eq. usym_nabla_pTsym%name) then
          force_sym1_sym2%i_ph_advect = i_phys
        else if (field_name .eq. uasym_nabla_pTasym%name) then
          force_asym1_asym2%i_ph_advect = i_phys
        else if (field_name .eq. usym_nabla_pTasym%name) then
          force_sym1_asym2%i_ph_advect = i_phys
        else if (field_name .eq. uasym_nabla_pTsym%name) then
          force_asym1_sym2%i_ph_advect = i_phys
!
        else if (field_name .eq. heat_flux_sym_sym%name) then
          force_sym1_sym2%i_h_flux =    i_phys
        else if (field_name .eq. heat_flux_asym_asym%name) then
          force_asym1_asym2%i_h_flux =  i_phys
        else if (field_name .eq. heat_flux_sym_asym%name) then
          force_sym1_asym2%i_h_flux =   i_phys
        else if (field_name .eq. heat_flux_asym_sym%name) then
          force_asym1_sym2%i_h_flux =   i_phys
!
        else if (field_name .eq. pert_h_flux_sym_sym%name) then
          force_sym1_sym2%i_ph_flux =   i_phys
        else if (field_name .eq. pert_h_flux_asym_asym%name) then
          force_asym1_asym2%i_ph_flux =   i_phys
        else if (field_name .eq. pert_h_flux_sym_asym%name) then
          force_sym1_asym2%i_ph_flux =   i_phys
        else if (field_name .eq. pert_h_flux_asym_sym%name) then
          force_asym1_sym2%i_ph_flux =   i_phys
!
        else if (field_name .eq. usym_nabla_Csym%name) then
          force_sym1_sym2%i_c_advect =    i_phys
        else if (field_name .eq. uasym_nabla_Casym%name) then
          force_asym1_asym2%i_c_advect =  i_phys
        else if (field_name .eq. usym_nabla_Casym%name) then
          force_sym1_asym2%i_c_advect =   i_phys
        else if (field_name .eq. uasym_nabla_Csym%name) then
          force_asym1_sym2%i_c_advect =   i_phys
!
        else if (field_name .eq. usym_nabla_pCsym%name) then
          force_sym1_sym2%i_pc_advect =   i_phys
        else if (field_name .eq. uasym_nabla_pCasym%name) then
          force_asym1_asym2%i_pc_advect = i_phys
        else if (field_name .eq. usym_nabla_pCasym%name) then
          force_sym1_asym2%i_pc_advect =  i_phys
        else if (field_name .eq. uasym_nabla_pCsym%name) then
          force_asym1_sym2%i_pc_advect =  i_phys
!
        else if (field_name .eq. composite_flux_sym_sym%name) then
          force_sym1_sym2%i_c_flux =    i_phys
        else if (field_name .eq. composite_flux_asym_asym%name) then
          force_asym1_asym2%i_c_flux =  i_phys
        else if (field_name .eq. composite_flux_sym_asym%name) then
          force_sym1_asym2%i_c_flux =   i_phys
        else if (field_name .eq. composite_flux_asym_sym%name) then
          force_asym1_sym2%i_c_flux =   i_phys
!
        else if (field_name .eq. pert_c_flux_sym_sym%name) then
          force_sym1_sym2%i_pc_flux =   i_phys
        else if (field_name .eq. pert_c_flux_asym_asym%name) then
          force_asym1_asym2%i_pc_flux = i_phys
        else if (field_name .eq. pert_c_flux_sym_asym%name) then
          force_sym1_asym2%i_pc_flux =  i_phys
        else if (field_name .eq. pert_c_flux_asym_sym%name) then
          force_asym1_sym2%i_pc_flux =  i_phys
!
        else if (field_name .eq. usym_Bsym%name) then
          force_sym1_sym2%i_induct_t =     i_phys
        else if (field_name .eq. uasym_Basym%name) then
          force_asym1_asym2%i_induct_t =   i_phys
        else if (field_name .eq. usym_Basym%name) then
          force_sym1_asym2%i_induct_t =    i_phys
        else if (field_name .eq. uasym_Bsym%name) then
          force_asym1_sym2%i_induct_t =    i_phys
        end if
      end if
!
      end subroutine set_force_w_sym_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_flux_tensor_w_sym_addresses(i_phys, field_name,    &
     &          force_sym1_sym2, force_asym1_asym2, force_sym1_asym2,   &
     &          flag)
!
      use m_force_w_sym_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: force_sym1_sym2
      type(base_force_address), intent(inout) :: force_asym1_asym2
      type(base_force_address), intent(inout) :: force_sym1_asym2
      logical, intent(inout) :: flag
!
!
      flag = check_flux_tensors_w_sym(field_name)
      if(flag) then
        if      (field_name .eq. m_flux_sym_sym%name) then
          force_sym1_sym2%i_m_flux =     i_phys
        else if (field_name .eq. m_flux_asym_asym%name) then
          force_asym1_asym2%i_m_flux =     i_phys
        else if (field_name .eq. m_flux_sym_asym%name) then
          force_sym1_asym2%i_m_flux =     i_phys
!
        else if (field_name .eq. maxwell_tensor_sym_sym%name) then
          force_sym1_sym2%i_maxwell =    i_phys
        else if (field_name .eq. maxwell_tensor_asym_asym%name) then
          force_asym1_asym2%i_maxwell =    i_phys
        else if (field_name .eq. maxwell_tensor_sym_asym%name) then
          force_sym1_asym2%i_maxwell =    i_phys
        end if
      end if
!
      end subroutine set_flux_tensor_w_sym_addresses
!
! ----------------------------------------------------------------------
!
      end module set_force_w_sym_labels
