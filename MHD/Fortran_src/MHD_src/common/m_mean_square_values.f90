!>@file   m_mean_square_values.f90
!!        module m_mean_square_values
!!
!! @author H. Matsui
!! @date   Programmed in 2002
!! @n      Modified  on Jan., 2013
!!
!
!> @brief addresses for volume integrated data
!!
!!@verbatim
!!      subroutine count_mean_square_values(nod_fld)
!!      subroutine set_mean_square_values(nod_fld)
!!@endverbatim
!
      module m_mean_square_values
!
      use m_precision
!
      use t_phys_address
      use t_phys_data
!
      implicit  none
!
!>      number of fields for volume average data
      integer (kind = kint) :: num_bulk
!>      number of fields for volume mean square data
      integer (kind = kint) :: num_rms
!
!>      volume average data for each subdomaine
      real(kind=kreal), dimension(:), allocatable :: bulk_local
!>      volume average data for entire domain
      real(kind=kreal), dimension(:), allocatable :: bulk_global
!
!>      volume mean square data for each subdomaine
      real(kind=kreal), dimension(:), allocatable :: rms_local
!>      volume mean square data for entire domain
      real(kind=kreal), dimension(:), allocatable :: rms_global
!
!>      Structure for addresses of volume average
      type(phys_address), save :: i_rms
!>      Structure for addresses of mean square
      type(phys_address), save :: j_ave
!
!
!>      Address for mean square of divergence of velocity
      integer(kind=kint) :: ir_divv = 0
!>      Address for average of divergence of velocity
      integer(kind=kint) :: ja_divv = 0
!
!>      Address for root mean square of vorticity
      integer(kind=kint) :: ir_rms_w = 0
!
!>      Address for average of angular momentum
      integer(kind=kint) :: ja_amom = 0
!
!>      Address for mean square of divergence of magnetic field
      integer(kind=kint) :: ir_divb = 0
!>      Address for average of divergence of magnetic field
      integer(kind=kint) :: ja_divb = 0
!
!>      Address for magnetic energy including inner core
      integer(kind=kint) :: ir_me_ic = 0
!>      Address for average magnetic field including inner core
      integer(kind=kint) :: ja_mag_ic = 0
!
!
!>      Address for mean square of divergence 
!!        of magnetic vector potential
      integer(kind=kint) :: ir_diva = 0
!>      Address for average of divergence of magnetic vector potential
      integer(kind=kint) :: ja_diva = 0
!
!>      Address for mean square of current density including inner core
      integer(kind=kint) :: ir_sqj_ic = 0
!>      Address for average of current density including inner core
      integer(kind=kint) :: ja_j_ic = 0
!
!>      Address for RMS of current density
      integer(kind=kint) :: ir_rms_j = 0
!>      Address for RMS of current density including inner core
      integer(kind=kint) :: ir_rms_j_ic = 0
!
!>      Address for mean square of divergence of filtered velocity
      integer(kind=kint) :: ir_divv_f = 0
!>      Address for average of divergence of filtered velocity
      integer(kind=kint) :: ja_divv_f = 0
!
!>      Address for average of filtered angular momentum
      integer(kind=kint) :: jr_amom_f = 0
!
!>      Address for filtered magnetic energy including inner core
      integer(kind=kint) :: ir_me_f_ic = 0
!>      Address for average filtererd magnetic field including inner core
      integer(kind=kint) :: ja_mag_f_ic = 0
!
!>      Address for mean square of divergence of filtered magnetic field
      integer(kind=kint) :: ir_divb_f = 0
!>      Address for average of divergence of filtered magnetic field
      integer(kind=kint) :: ja_divb_f = 0
!
!>      Address for mean square of divergence
!!      of filtered magnetic vector potential
      integer(kind=kint) :: ir_diva_f = 0
!>      Address for average of divergence
!!      of filtered magnetic vector potential
      integer(kind=kint) :: ja_diva_f = 0
!
!>      Address of volume of fluid area
      integer(kind=kint) :: ivol = 0
!
      real(kind=kreal) :: ave_mp_core
      real(kind=kreal) :: ave_mp_core_local
!
      real(kind=kreal) :: ave_flux_local
!
      private :: set_rms_address
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_mean_square_values(nod_fld)
!
      use m_phys_labels
      use m_phys_constants
!
      type(phys_data), intent(in) :: nod_fld
!
      integer (kind = kint) :: i, i0, j0
      character(len = kchara) :: field_name
!
      i0 = 0
      j0 = 0
      do i = 1, nod_fld%num_phys
        field_name = nod_fld%phys_name(i)
        if (nod_fld%iflag_monitor(i) .eq. 1) then
          if     (field_name .eq. fhd_velo                              &
     &       .or. field_name .eq. fhd_filter_v                          &
     &      ) then
         i0 = i0 + 2
         j0 = j0 + 7
        else if ( field_name .eq. fhd_magne                             &
     &       .or. field_name .eq. fhd_filter_b                          &
     &      ) then
         i0 = i0 + 3
         j0 = j0 + 7
        else if ( field_name .eq. fhd_current ) then
         i0 = i0 + 4
         j0 = j0 + 6
        else if ( field_name .eq. fhd_vort ) then
         i0 = i0 + 2
         j0 = j0 + 3
        else if ( field_name .eq. fhd_vecp                              &
     &       .or. field_name .eq. fhd_filter_a                          &
     &       .or. field_name .eq. fhd_temp                              &
     &       .or. field_name .eq. fhd_part_temp                         &
     &       .or. field_name .eq. fhd_filter_temp                       &
     &       .or. field_name .eq. fhd_filter_comp                       &
     &       .or. field_name .eq. fhd_filter_part_temp                  &
     &       .or. field_name .eq. fhd_press                             &
     &       .or. field_name .eq. fhd_mag_potential                     &
     &       .or. field_name .eq. fhd_light                             &
     &       .or. field_name .eq. fhd_part_light                        &
     &       .or. field_name .eq. fhd_entropy                           &
     &       .or. field_name .eq. fhd_per_entropy                       &
     &       .or. field_name .eq. fhd_heat_source                       &
     &       .or. field_name .eq. fhd_light_source                      &
     &       .or. field_name .eq. fhd_entropy_source                    &
     &       .or. field_name .eq. fhd_density                           &
     &       .or. field_name .eq. fhd_per_density                       &
     &       .or. field_name .eq. fhd_mag_ene_gen                       &
     &       .or. field_name .eq. fhd_work_agst_Lorentz                 &
     &       .or. field_name .eq. fhd_Lorentz_work                      &
     &       .or. field_name .eq. fhd_mag_tension_work                  &
     &       .or. field_name .eq. fhd_buoyancy_flux                     &
     &       .or. field_name .eq. fhd_buoyancy_work                     &
     &       .or. field_name .eq. fhd_comp_buo_flux                     &
     &       .or. field_name .eq. fhd_filter_buo_flux                   &
     &       .or. field_name .eq. fhd_heat_advect                       &
     &       .or. field_name .eq. fhd_part_h_advect                     &
     &       .or. field_name .eq. fhd_div_h_flux                        &
     &       .or. field_name .eq. fhd_div_ph_flux                       &
     &       .or. field_name .eq. fhd_temp_generation                   &
     &       .or. field_name .eq. fhd_part_temp_gen                     &
     &       .or. field_name .eq. fhd_div_SGS_h_flux                    &
     &       .or. field_name .eq. fhd_SGS_temp_gen                      &
     &       .or. field_name .eq. fhd_SGS_m_ene_gen                     &
     &       .or. field_name .eq. fhd_SGS_Lorentz_work                  &
     &       .or. field_name .eq. fhd_Reynolds_work                     &
     &       .or. field_name .eq. fhd_SGS_buo_flux                      &
     &       .or. field_name .eq. fhd_SGS_comp_buo_flux                 &
     &       .or. field_name .eq. fhd_c_diffuse                         &
     &       .or. field_name .eq. fhd_thermal_diffusion                 &
     &       .or. field_name .eq. fhd_vis_ene_diffuse                   &
     &       .or. field_name .eq. fhd_mag_ene_diffuse                   &
     &       .or. field_name .eq. fhd_SGS_div_h_flux_true               &
     &       .or. field_name .eq. fhd_SGS_Lorentz_wk_true               &
     &       .or. field_name .eq. fhd_Reynolds_work_true                &
     &       .or. field_name .eq. fhd_SGS_temp_gen_true                 &
     &       .or. field_name .eq. fhd_SGS_m_ene_gen_true                &
     &      ) then
         i0 = i0 + 1
         j0 = j0 + n_scalar
        else if ( field_name .eq. fhd_mag_tension                       &
     &       .or. field_name .eq. fhd_inertia                           &
     &       .or. field_name .eq. fhd_div_m_flux                        &
     &       .or. field_name .eq. fhd_div_maxwell_t                     &
     &       .or. field_name .eq. fhd_div_induct_t                      &
     &       .or. field_name .eq. fhd_mag_induct                        &
     &       .or. field_name .eq. fhd_vp_induct                         &
     &       .or. field_name .eq. fhd_press_grad                        &
     &       .or. field_name .eq. fhd_mag_stretch                       &
     &       .or. field_name .eq. fhd_Lorentz                           &
     &       .or. field_name .eq. fhd_Coriolis                          &
     &       .or. field_name .eq. fhd_buoyancy                          &
     &       .or. field_name .eq. fhd_comp_buo                          &
     &       .or. field_name .eq. fhd_filter_buo                        &
     &       .or. field_name .eq. fhd_h_flux                            &
     &       .or. field_name .eq. fhd_ph_flux                           &
     &       .or. field_name .eq. fhd_c_flux                            &
     &       .or. field_name .eq. fhd_e_field                           &
     &       .or. field_name .eq. fhd_poynting                          &
     &       .or. field_name .eq. fhd_grad_v_1                          &
     &       .or. field_name .eq. fhd_grad_v_2                          &
     &       .or. field_name .eq. fhd_grad_v_3                          &
     &      ) then
         i0 = i0 + 1
         j0 = j0 + n_vector
        else if(  field_name .eq. fhd_SGS_h_flux                        &
     &       .or. field_name .eq. fhd_SGS_c_flux                        &
     &       .or. field_name .eq. fhd_div_SGS_m_flux                    &
     &       .or. field_name .eq. fhd_SGS_Lorentz                       &
     &       .or. field_name .eq. fhd_SGS_induction                     &
     &       .or. field_name .eq. fhd_SGS_vp_induct                     &
     &       .or. field_name .eq. fhd_viscous                           &
     &       .or. field_name .eq. fhd_vecp_diffuse                      &
     &       .or. field_name .eq. fhd_mag_diffuse                       &
     &       .or. field_name .eq. fhd_SGS_div_m_flux_true               &
     &       .or. field_name .eq. fhd_SGS_Lorentz_true                  &
     &       .or. field_name .eq. fhd_SGS_mag_induct_true               &
     &       .or. field_name .eq. fhd_SGS_buoyancy                      &
     &       .or. field_name .eq. fhd_SGS_comp_buo                      &
     &      ) then
         i0 = i0 + 1
         j0 = j0 + n_vector
        else if ( field_name .eq. fhd_mom_flux                          &
     &       .or. field_name .eq. fhd_maxwell_t                         &
     &       .or. field_name .eq. fhd_induct_t                          &
     &       .or. field_name .eq. fhd_SGS_m_flux                        &
     &       .or. field_name .eq. fhd_SGS_maxwell_t                     &
     &       .or. field_name .eq. fhd_SGS_induct_t                      &
     &      ) then
         i0 = i0 + 1
         j0 = j0 + n_sym_tensor
!
        else if ( field_name .eq. fhd_velocity_scale                    &
     &       .or. field_name .eq. fhd_magnetic_scale                    &
     &       .or. field_name .eq. fhd_temp_scale                        &
     &       .or. field_name .eq. fhd_composition_scale                 &
     &      ) then
         i0 = i0 + 1
         j0 = j0 + n_scalar
        end if
!
        else
          if    ( field_name .eq. fhd_velo                              &
     &       .or. field_name .eq. fhd_filter_v                          &
     &       .or. field_name .eq. fhd_magne                             &
     &       .or. field_name .eq. fhd_filter_b                          &
     &       .or. field_name .eq. fhd_vecp                              &
     &       .or. field_name .eq. fhd_filter_a                          &
     &       .or. field_name .eq. fhd_mag_potential                     &
     &      ) then
            i0 = i0 + 1
            j0 = j0 + n_scalar
          end if
        end if
!
      end do
!
      num_rms =  i0 + 1
      num_bulk = j0
!
       return
       end subroutine count_mean_square_values
!
!-----------------------------------------------------------------------
!
      subroutine set_mean_square_values(nod_fld)
!
      use m_phys_labels
      use m_phys_constants
!
      type(phys_data), intent(in) :: nod_fld
!
      integer (kind = kint) :: i, i0, j0, num_comps
      character(len = kchara) :: field_name
!
!
      allocate (rms_local(num_rms))
      allocate (rms_global(num_rms))
      allocate (bulk_local(num_bulk))
      allocate (bulk_global(num_bulk))
      rms_local  = 0.0d0
      rms_global = 0.0d0
      bulk_local  = 0.0d0
      bulk_global = 0.0d0
!
      i0 = 0
      j0 = 0
      do i = 1, nod_fld%num_phys
        field_name = nod_fld%phys_name(i)
        num_comps =  nod_fld%num_component(i)
        if (nod_fld%iflag_monitor(i) .eq. 1) then
          if ( field_name .eq. fhd_velo) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_velo, j_ave%i_velo)
            call set_rms_address(n_scalar, i0, j0,                      &
     &          ir_divv, ja_divv)
!
            ja_amom = j0 + 1
            j0 = j0 + 3
          end if
!
          if ( field_name .eq. fhd_magne ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_magne, j_ave%i_magne)
            call set_rms_address(num_comps, i0, j0,                     &
     &          ir_me_ic, ja_mag_ic)
            call set_rms_address(n_scalar, i0, j0,                      &
     &          ir_divb, ja_divb)
          end if
!
          if ( field_name .eq. fhd_vecp ) then
            call set_rms_address(n_scalar, i0, j0,                      &
     &          ir_diva, ja_diva)
          end if

          if ( field_name .eq. fhd_vort ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_vort, j_ave%i_vort)
            ir_rms_w   = i0 + 1
            i0 = i0 + 1
          end if
!
          if ( field_name .eq. fhd_current ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_current, j_ave%i_current)
            call set_rms_address(num_comps, i0, j0,                     &
     &          ir_sqj_ic, ja_j_ic)
!
            ir_rms_j    = i0 + 1
            ir_rms_j_ic = i0 + 2
            i0 = i0 + 2
          end if
!
          if ( field_name .eq. fhd_e_field ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_electric, j_ave%i_electric)
          else if ( field_name .eq. fhd_poynting ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_poynting, j_ave%i_poynting)
          else if ( field_name .eq. fhd_temp ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_temp, j_ave%i_temp)
          else if ( field_name .eq. fhd_press ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_press, j_ave%i_press)
          else if ( field_name .eq. fhd_mag_potential ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_mag_p, j_ave%i_mag_p)
          end if
!
          if ( field_name .eq. fhd_part_temp ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_par_temp, j_ave%i_par_temp)
          else if ( field_name .eq. fhd_light ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_light, j_ave%i_light)
          else if ( field_name .eq. fhd_part_light ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_par_light, j_ave%i_par_light)
          else if ( field_name .eq. fhd_entropy ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_entropy, j_ave%i_entropy)
          else if ( field_name .eq. fhd_per_entropy ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_par_entropy, j_ave%i_par_entropy)
          else if ( field_name .eq. fhd_density ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_density, j_ave%i_density)
          else if ( field_name .eq. fhd_per_density ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_par_density, j_ave%i_par_density)
!
          else if ( field_name .eq. fhd_heat_source ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_heat_source, j_ave%i_heat_source)
          else if ( field_name .eq. fhd_light_source ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_light_source, j_ave%i_light_source)
          else if ( field_name .eq. fhd_entropy_source ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_entropy_source, j_ave%i_entropy_source)
          end if
!
          if ( field_name .eq. fhd_press_grad ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_press_grad, j_ave%i_press_grad)
          end if
!
          if ( field_name .eq. fhd_mag_tension ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_m_tension, j_ave%i_m_tension)
          end if
!
          if ( field_name .eq. fhd_filter_v ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_filter_velo, j_ave%i_filter_velo)
            call set_rms_address(n_scalar, i0, j0,                      &
     &          ir_divv_f, ja_divv_f)
            jr_amom_f = i0 + 1
            j0 = j0 + 3
          end if
!
          if ( field_name .eq. fhd_filter_b ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_filter_magne, j_ave%i_filter_magne)
            call set_rms_address(num_comps, i0, j0,                     &
     &          ir_me_f_ic, ja_mag_f_ic)
            call set_rms_address(n_scalar, i0, j0,                      &
     &          ir_divb_f, ja_divb_f)
          end if
!
          if ( field_name .eq. fhd_filter_a ) then
            call set_rms_address(n_scalar, i0, j0,                      &
     &          ir_diva_f, ja_diva_f)
          else if ( field_name .eq. fhd_filter_temp ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_filter_temp, j_ave%i_filter_temp)
          else if ( field_name .eq. fhd_filter_comp ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_filter_comp, j_ave%i_filter_comp)
          end if
!
          if ( field_name .eq. fhd_grad_v_1 ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_grad_vx, j_ave%i_grad_vx)
          else if ( field_name .eq. fhd_grad_v_2 ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_grad_vy, j_ave%i_grad_vy)
          else if ( field_name .eq. fhd_grad_v_3 ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_grad_vz, j_ave%i_grad_vz)
          end if
!
          if ( field_name .eq. fhd_mom_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_m_flux, j_ave%i_m_flux)
          else if ( field_name .eq. fhd_maxwell_t ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_maxwell, j_ave%i_maxwell)
          else if ( field_name .eq. fhd_induct_t ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_induct_t, j_ave%i_induct_t)
          else if ( field_name .eq. fhd_inertia ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_m_advect, j_ave%i_m_advect)
          else if ( field_name .eq. fhd_div_m_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_m_flux_div, j_ave%i_m_flux_div)
          else if ( field_name .eq. fhd_div_maxwell_t ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_maxwell_div, j_ave%i_maxwell_div)
          else if ( field_name .eq. fhd_div_induct_t ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_induct_div, j_ave%i_induct_div)
          else if ( field_name .eq. fhd_mag_induct ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_induction, j_ave%i_induction)
          else if ( field_name .eq. fhd_vp_induct ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_vp_induct, j_ave%i_vp_induct)
          else if ( field_name .eq. fhd_mag_stretch ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_mag_stretch, j_ave%i_mag_stretch)
          else if ( field_name .eq. fhd_Lorentz ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_lorentz, j_ave%i_lorentz)
          else if ( field_name .eq. fhd_Coriolis ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_coriolis, j_ave%i_coriolis)
          else if ( field_name .eq. fhd_buoyancy ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_buoyancy, j_ave%i_buoyancy)
          else if ( field_name .eq. fhd_comp_buo ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_comp_buo, j_ave%i_comp_buo)
          else if ( field_name .eq. fhd_filter_buo ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_filter_buo, j_ave%i_filter_buo)
          end if
!
          if ( field_name .eq. fhd_viscous ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_v_diffuse, j_ave%i_v_diffuse)
          else if ( field_name .eq. fhd_vecp_diffuse ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_vp_diffuse, j_ave%i_vp_diffuse)
          else if ( field_name .eq. fhd_mag_diffuse ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_b_diffuse, j_ave%i_b_diffuse)
          else if ( field_name .eq. fhd_thermal_diffusion ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_t_diffuse, j_ave%i_t_diffuse)
          else if ( field_name .eq. fhd_c_diffuse) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_c_diffuse, j_ave%i_c_diffuse)
          end if
!
          if ( field_name .eq. fhd_SGS_m_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_m_flux, j_ave%i_SGS_m_flux)
          else if ( field_name .eq. fhd_SGS_maxwell_t ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_maxwell, j_ave%i_SGS_maxwell)
          else if ( field_name .eq. fhd_SGS_induct_t ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_induct_t, j_ave%i_SGS_induct_t)
          else if ( field_name .eq. fhd_div_SGS_m_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_div_m_flux, j_ave%i_SGS_div_m_flux)
          else if ( field_name .eq. fhd_SGS_Lorentz ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_Lorentz, j_ave%i_SGS_Lorentz)
          else if ( field_name .eq. fhd_SGS_induction ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_induction, j_ave%i_SGS_induction)
          else if ( field_name .eq. fhd_SGS_vp_induct ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_vp_induct, j_ave%i_SGS_vp_induct)
          else if ( field_name .eq. fhd_SGS_buoyancy ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_buoyancy, j_ave%i_SGS_buoyancy)
          else if ( field_name .eq. fhd_SGS_comp_buo ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_comp_buo, j_ave%i_SGS_comp_buo)
          end if
!
          if ( field_name .eq. fhd_mag_ene_gen ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_me_gen, j_ave%i_me_gen)
          else if ( field_name .eq. fhd_Lorentz_work ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_ujb, j_ave%i_ujb)
          else if ( field_name .eq. fhd_work_agst_Lorentz ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_nega_ujb, j_ave%i_nega_ujb)
          else if ( field_name .eq. fhd_mag_tension_work ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_m_tension_wk, j_ave%i_m_tension_wk)
          else if ( field_name .eq. fhd_buoyancy_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_buo_gen, j_ave%i_buo_gen)
          else if ( field_name .eq. fhd_comp_buo_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_c_buo_gen, j_ave%i_c_buo_gen)
          else if ( field_name .eq. fhd_filter_buo_flux) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_f_buo_gen, j_ave%i_f_buo_gen)
          end if
!
          if ( field_name .eq. fhd_vis_ene_diffuse ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_vis_e_diffuse, j_ave%i_vis_e_diffuse)
          else if ( field_name .eq. fhd_mag_ene_diffuse ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_mag_e_diffuse, j_ave%i_mag_e_diffuse)
          else if ( field_name .eq. fhd_h_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_h_flux, j_ave%i_h_flux)
          else if ( field_name .eq. fhd_ph_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_ph_flux, j_ave%i_ph_flux)
          else if ( field_name .eq. fhd_c_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_c_flux, j_ave%i_c_flux)
          else if ( field_name .eq. fhd_heat_advect ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_h_advect, j_ave%i_h_advect)
          else if ( field_name .eq. fhd_part_h_advect ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_ph_advect, j_ave%i_ph_advect)
          else if ( field_name .eq. fhd_div_h_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_h_flux_div, j_ave%i_h_flux_div)
          else if ( field_name .eq. fhd_div_ph_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_ph_flux_div, j_ave%i_ph_flux_div)
          else if ( field_name .eq. fhd_temp_generation ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_temp_gen, j_ave%i_temp_gen)
          else if ( field_name .eq. fhd_part_temp_gen ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_par_t_gen, j_ave%i_par_t_gen)
          else if ( field_name .eq. fhd_SGS_h_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_h_flux, j_ave%i_SGS_h_flux)
          else if ( field_name .eq. fhd_div_SGS_h_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_div_h_flux, j_ave%i_SGS_div_h_flux)
          else if ( field_name .eq. fhd_SGS_c_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_c_flux, j_ave%i_SGS_c_flux)
          else if ( field_name .eq. fhd_SGS_temp_gen ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_temp_gen, j_ave%i_SGS_temp_gen)
          else if ( field_name .eq. fhd_SGS_m_ene_gen ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_me_gen, j_ave%i_SGS_me_gen)
          else if ( field_name .eq. fhd_SGS_Lorentz_work ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_Lor_wk, j_ave%i_SGS_Lor_wk)
          else if ( field_name .eq. fhd_Reynolds_work ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_reynolds_wk, j_ave%i_reynolds_wk)
          else if ( field_name .eq. fhd_SGS_buo_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_buo_wk, j_ave%i_SGS_buo_wk)
          else if ( field_name .eq. fhd_SGS_comp_buo_flux ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_comp_buo_wk, j_ave%i_SGS_comp_buo_wk)
          end if
!
          if (field_name .eq. fhd_SGS_div_h_flux_true) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_div_hf_true, j_ave%i_SGS_div_hf_true)
          else if (field_name .eq. fhd_SGS_div_m_flux_true) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_div_mf_true, j_ave%i_SGS_div_mf_true)
          else if ( field_name .eq. fhd_SGS_Lorentz_true ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_Lor_true, j_ave%i_SGS_Lor_true)
          else if ( field_name .eq. fhd_SGS_mag_induct_true ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_idct_true, j_ave%i_SGS_idct_true)
          end if
!
          if ( field_name .eq. fhd_SGS_Lorentz_wk_true ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_Lor_wk_tr, j_ave%i_SGS_Lor_wk_tr)
          else if ( field_name .eq. fhd_Reynolds_work_true ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_reynolds_wk_tr, j_ave%i_reynolds_wk_tr)
          else if ( field_name .eq. fhd_SGS_temp_gen_true ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_t_gen_tr, j_ave%i_SGS_t_gen_tr)
          else if ( field_name .eq. fhd_SGS_m_ene_gen_true ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_SGS_me_gen_tr, j_ave%i_SGS_me_gen_tr)
          end if
!
          if ( field_name .eq. fhd_velocity_scale ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_velo_scale, j_ave%i_velo_scale)
          else if ( field_name .eq. fhd_magnetic_scale ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_magne_scale, j_ave%i_magne_scale)
          else if ( field_name .eq. fhd_temp_scale ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_temp_scale, j_ave%i_temp_scale)
          else if ( field_name .eq. fhd_composition_scale ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_comp_scale, j_ave%i_comp_scale)
          end if
!
!   Old field label... Should be deleted later!!
          if ( field_name .eq. fhd_buoyancy_work ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_buo_gen, j_ave%i_buo_gen)
          end if
!
        else
          if ( field_name .eq. fhd_velo) then
            call set_rms_address(n_scalar, i0, j0,                      &
     &          ir_divv, ja_divv)
          else if ( field_name .eq. fhd_magne ) then
            call set_rms_address(n_scalar, i0, j0,                      &
     &          ir_divb, ja_divb)
          else if ( field_name .eq. fhd_vecp ) then
            call set_rms_address(n_scalar, i0, j0,                      &
     &          ir_diva, ja_diva)
          else if ( field_name .eq. fhd_filter_v ) then
            call set_rms_address(n_scalar, i0, j0,                      &
     &          ir_divv_f, ja_divv_f)
          else if ( field_name .eq. fhd_filter_b ) then
            call set_rms_address(n_scalar, i0, j0,                      &
     &          ir_divb_f, ja_divb_f)
          else if ( field_name .eq. fhd_filter_a ) then
            call set_rms_address(n_scalar, i0, j0,                      &
     &          ir_diva_f, ja_diva_f)
          else if ( field_name .eq. fhd_mag_potential ) then
            call set_rms_address(num_comps, i0, j0,                     &
     &          i_rms%i_mag_p, j_ave%i_mag_p)
          end if

        end if
      end do
!
      ivol = i0 + 1
      i0 = i0 + 1
!
      end subroutine set_mean_square_values
!
! ----------------------------------------------------------------------
!
      subroutine set_rms_address(numdir, i0, j0, ir_rms, ja_ave)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(inout) :: ir_rms, ja_ave, i0, j0
!
      ir_rms = i0 + 1
      ja_ave = j0 + 1
!
      i0 = i0 + 1
      j0 = j0 + numdir
!
      end subroutine set_rms_address
!
! ----------------------------------------------------------------------
!
      end module m_mean_square_values
