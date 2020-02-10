!>@file   t_ene_flux_w_sym_address.f90
!!        module t_ene_flux_w_sym_address
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Structure of energy fluxes addresses with symmetry
!!
!!@verbatim
!!        logical function check_energy_fluxes_w_symmetry(field_name)
!!      subroutine set_flux_w_symmetry_addresse                         &
!!     &         (i_phys, field_name, sym_flux, flag)
!!        type(ene_fluxes_w_sym_address), intent(inout) :: sym_flux
!!      subroutine mean_square_flux_address_w_sym                       &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_sym_flux, ave_sym_flux, flag)
!!        type(ene_fluxes_w_sym_address), intent(inout) :: rms_sym_flux
!!        type(ene_fluxes_w_sym_address), intent(inout) :: ave_sym_flux
!!@endverbatim
!!
      module t_ene_flux_w_sym_address
!
      use m_precision
      use m_constants
!
      implicit  none
! 
!>       Field label for work of Lorentz force by dipolar field
!!         @f$  u_{sim} \cdot \left(J_{sim} \times B_{asim} \right) @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_dipolar_Lorentz_work = 'dipolar_Lorentz_work'
!>       Field label for work of Lorentz force by quadrapolar field
!!         @f$  u_{sim} \cdot \left(J_{asim} \times B_{sim} \right) @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_quadra_Lorentz_work = 'quadra_Lorentz_work'
!>       Field label for work of Lorentz force by asymmetric J and B
!!         @f$  u_{asim} \cdot \left(J_{asim} \times B_{asim} \right) @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_asym_j_cross_B_work = 'asym_j_cross_B_work'
!>       Field label for work of Lorentz force by symmetric J and B
!!         @f$  u_{asim} \cdot \left(J_{sim} \times B_{sim} \right) @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_sym_j_cross_B_work = 'sym_j_cross_B_work'
!
!>       Field label for work against Lorentz force by dipolar field
!!         @f$ - u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_wk_agst_dipl_Lorentz = 'work_against_dipolar_Lorentz'
!>       Field label for work against Lorentz force by quadrapolar field
!!         @f$ - u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_wk_agst_quad_Lorentz = 'work_against_quadra_Lorentz'
!>       Field label for work against Lorentz force by asymmetric J and B
!!         @f$ - u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_wk_agst_asym_jxB = 'work_against_asym_jxB'
!>       Field label for work against Lorentz force by symmetric J and B
!!         @f$ - u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_wk_agst_sym_jxB = 'work_against_sym_jxB'
!
!>        Field label for buoyancy flux
!!         @f$ -u_{i} \alpha_{T} g_{i} T @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_buoyancy_flux = 'symmetic_thermal_buoyancy_flux'
!>        Field label for compositional buoyancy flux
!!         @f$ -u_{i} \alpha_{c} g_{i} C @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_comp_buo_flux = 'symmetic_composite_buoyancy_flux'
!
!>        Field label for symmetric buoyancy flux
!!         @f$ -u_{s} \cdot (\alpha_{T} g T_{s}) @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_sym_thrm_buo_flux = 'sym_thermal_buoyancy_flux'
!>        Field label for symmetric compositional buoyancy flux
!!         @f$ -u_{s} \cdot (\alpha_{c} g C_{s}) @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_sym_comp_buo_flux = 'sym_composite_buoyancy_flux'
!>        Field label for asymmetric buoyancy flux
!!         @f$ -u_{a} \cdot (\alpha_{T} g T_{s}) @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_asym_thrm_buo_flux = 'asym_thermal_buoyancy_flux'
!>        Field label for asymmetric compositional buoyancy flux
!!         @f$ -u_{a} \cdot (\alpha_{c} g C_{s}) @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_asym_comp_buo_flux = 'asym_composite_buoyancy_flux'
!
!
!>       Structure for start address for energy fluxes
      type ene_fluxes_w_sym_address
!>        start address for work of Lorentz force by dipolar field
!!         @f$  u_{sim} \cdot \left(J_{sim} \times B_{asim} \right) @f$
        integer (kind=kint) :: i_dipolar_Lorentz_work = izero
!>        start address for work of Lorentz force by quadrapolar field
!!         @f$  u_{sim} \cdot \left(J_{asim} \times B_{sim} \right) @f$
        integer (kind=kint) :: i_quadra_Lorentz_work =  izero
!>        start address for work of Lorentz force by asymmetric J and B
!!         @f$  u_{asim} \cdot \left(J_{asim} \times B_{asim} \right) @f$
        integer (kind=kint) :: i_asym_j_cross_B_work =  izero
!>        start address for work of Lorentz force by symmetric J and B
!!         @f$  u_{asim} \cdot \left(J_{sim} \times B_{sim} \right) @f$
        integer (kind=kint) :: i_sym_j_cross_B_work =   izero
!
!>        start address for work of Lorentz force by dipolar field
!!         @f$ -u_{sim} \cdot \left(J_{sim} \times B_{asim} \right) @f$
        integer (kind=kint) :: i_wk_agst_dipl_Lorentz = izero
!>        start address for work of Lorentz force by quadrapolar field
!!         @f$ -u_{sim} \cdot \left(J_{asim} \times B_{sim} \right) @f$
        integer (kind=kint) :: i_wk_agst_quad_Lorentz = izero
!>        start address for work of Lorentz force by asymmetric J and B
!!         @f$ -u_{asim} \cdot \left(J_{asim} \times B_{asim} \right) @f$
        integer (kind=kint) :: i_wk_agst_asym_jxB =     izero
!>        start address for work of Lorentz force by symmetric J and B
!!         @f$ -u_{asim} \cdot \left(J_{sim} \times B_{sim} \right) @f$
        integer (kind=kint) :: i_wk_agst_sym_jxB =      izero
!
!>        start address for symmetric buoyancy flux
!!         @f$ -u_{s} \cdot (\alpha_{T} g T_{s}) @f$
        integer (kind=kint) :: i_sym_thrm_buo_flux =    izero
!>        start address for symmetric compositional buoyancy flux
!!         @f$ -u_{s} \cdot (\alpha_{c} g C_{s}) @f$
        integer (kind=kint) :: i_sym_comp_buo_flux =    izero
!
!>        start address for asymmetric buoyancy flux
!!         @f$ -u_{a} \cdot (\alpha_{T} g T_{s}) @f$
        integer (kind=kint) :: i_asym_thrm_buo_flux =   izero
!>        start address for asymmetric compositional buoyancy flux
!!         @f$ -u_{a} \cdot (\alpha_{c} g C_{s}) @f$
        integer (kind=kint) :: i_asym_comp_buo_flux =   izero
      end type ene_fluxes_w_sym_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_energy_fluxes_w_symmetry(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_energy_fluxes_w_symmetry = .FALSE.
      if (    (field_name .eq. fhd_dipolar_Lorentz_work)                &
     &   .or. (field_name .eq. fhd_quadra_Lorentz_work )                &
     &   .or. (field_name .eq. fhd_asym_j_cross_B_work )                &
     &   .or. (field_name .eq. fhd_sym_j_cross_B_work  )                &
     &   .or. (field_name .eq. fhd_wk_agst_dipl_Lorentz)                &
     &   .or. (field_name .eq. fhd_wk_agst_quad_Lorentz)                &
     &   .or. (field_name .eq. fhd_wk_agst_asym_jxB    )                &
     &   .or. (field_name .eq. fhd_wk_agst_sym_jxB     )                &
     &   .or. (field_name .eq. fhd_sym_thrm_buo_flux   )                &
     &   .or. (field_name .eq. fhd_sym_comp_buo_flux   )                &
     &   .or. (field_name .eq. fhd_asym_thrm_buo_flux  )                &
     &   .or. (field_name .eq. fhd_asym_comp_buo_flux  )                &
     &      )   check_energy_fluxes_w_symmetry = .TRUE.
!
      end function check_energy_fluxes_w_symmetry
!
! ----------------------------------------------------------------------
!
      subroutine set_flux_w_symmetry_addresse                           &
     &         (i_phys, field_name, sym_flux, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(ene_fluxes_w_sym_address), intent(inout) :: sym_flux
      logical, intent(inout) :: flag
!
!
      flag = check_energy_fluxes_w_symmetry(field_name)
      if(flag .eqv. .FALSE.) return
!
      if (field_name .eq. fhd_dipolar_Lorentz_work) then
        sym_flux%i_dipolar_Lorentz_work =  i_phys
      else if (field_name .eq. fhd_quadra_Lorentz_work) then
        sym_flux%i_quadra_Lorentz_work =   i_phys
      else if (field_name .eq. fhd_asym_j_cross_B_work) then
        sym_flux%i_asym_j_cross_B_work =   i_phys
      else if (field_name .eq. fhd_sym_j_cross_B_work) then
        sym_flux%i_sym_j_cross_B_work =    i_phys
      else if (field_name .eq. fhd_wk_agst_dipl_Lorentz) then
        sym_flux%i_wk_agst_dipl_Lorentz =  i_phys
      else if (field_name .eq. fhd_wk_agst_quad_Lorentz) then
        sym_flux%i_wk_agst_quad_Lorentz =  i_phys
      else if (field_name .eq. fhd_wk_agst_asym_jxB) then
        sym_flux%i_wk_agst_asym_jxB =      i_phys
      else if (field_name .eq. fhd_wk_agst_sym_jxB) then
        sym_flux%i_wk_agst_sym_jxB =       i_phys
      else if (field_name .eq. fhd_sym_thrm_buo_flux) then
        sym_flux%i_sym_thrm_buo_flux =     i_phys
      else if (field_name .eq. fhd_sym_comp_buo_flux) then
        sym_flux%i_sym_comp_buo_flux =     i_phys
      else if (field_name .eq. fhd_asym_thrm_buo_flux) then
        sym_flux%i_asym_thrm_buo_flux =    i_phys
      else if (field_name .eq. fhd_asym_comp_buo_flux) then
        sym_flux%i_asym_comp_buo_flux =    i_phys
      end if
!
      end subroutine set_flux_w_symmetry_addresse
!
! ----------------------------------------------------------------------
!
      subroutine mean_square_flux_address_w_sym                         &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_sym_flux, ave_sym_flux, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(ene_fluxes_w_sym_address), intent(inout) :: rms_sym_flux
      type(ene_fluxes_w_sym_address), intent(inout) :: ave_sym_flux
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call set_flux_w_symmetry_addresse                                 &
     &   ((numrms+1), field_name, rms_sym_flux, flag_r)
      call set_flux_w_symmetry_addresse                                 &
     &   ((numave+1), field_name, ave_sym_flux, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine mean_square_flux_address_w_sym
!
! ----------------------------------------------------------------------
!
      end module t_ene_flux_w_sym_address
