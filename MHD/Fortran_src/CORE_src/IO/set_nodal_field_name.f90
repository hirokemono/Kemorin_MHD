!>@file   set_nodal_field_name.f90
!!@brief  module set_nodal_field_name
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2008
!!@n        modified by H.Matsui on Oct.,  2009
!!@n        modified by H.Matsui on June., 2012
!
!>@brief  Set field names from control data
!!
!!@verbatim
!!      subroutine set_vector_field_name(phys_nod_name_ctl, icou,       &
!!     &          phys_nod_name, num_nod_component, iflag)
!!      subroutine set_scalar_field_name(phys_nod_name_ctl, icou,       &
!!     &          phys_nod_name, num_nod_component, iflag)
!!      subroutine set_tensor_field_name(phys_nod_name_ctl, icou,       &
!!     &          phys_nod_name, num_nod_component, iflag)
!!
!!      subroutine check_vis_control_flag(visualize_ctl, iflag_viz)
!!      subroutine check_monitor_control_flag(iflag, monitor_ctl,       &
!!     &          iflag_fld_monitor)
!!
!!      subroutine set_vis_control_flag(iflag_viz, visualize_ctl)
!!      subroutine set_monitor_control_flag                             &
!!     &         (iflag_fld_monitor, monitor_ctl)
!!@endverbatim
!
      module set_nodal_field_name
!
      use m_precision
      use m_phys_labels
!
      implicit  none
!
      character(len = kchara), parameter :: cflag_viz_on =  'Viz_On'
      character(len = kchara), parameter :: cflag_viz_off = 'Viz_Off'
!
      character(len = kchara), parameter                                &
     &                        :: cflag_monitor_on =  'Monitor_On'
      character(len = kchara), parameter                                &
     &                        :: cflag_monitor_off = 'Monitor_Off'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_vector_field_name(phys_nod_name_ctl, icou,         &
     &          phys_nod_name, num_nod_component, iflag)
!
      use t_diffusion_term_labels
      use t_SGS_term_labels
!
      use m_rot_force_labels
      use m_div_force_labels
      use m_true_SGS_term_labels
      use m_rot_filtered_force_labels
      use m_div_filtered_force_labels
      use m_grad_filter_field_labels
!
      character(len = kchara), intent(in) :: phys_nod_name_ctl
      integer (kind = kint), intent(inout) :: icou
      integer (kind = kint), intent(inout) :: num_nod_component
      character(len = kchara), intent(inout) :: phys_nod_name
      integer (kind = kint), intent(inout) :: iflag
!
!  set number of components ( vector and scalar )
!
      if (iflag .gt. 0) return
!
      if (   (phys_nod_name_ctl .eq. fhd_velo               )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_vort               )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_magne              )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_vecp               )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_current            )           &
     &      )   iflag = 1
!
      if (   (phys_nod_name_ctl .eq. filter_velocity%name        )           &
     &  .or. (phys_nod_name_ctl .eq. filter_vorticity%name        )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_filter_vecp        )           &
     &  .or. (phys_nod_name_ctl .eq. filter_magne%name       )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_filter_current     )           &
     &      )   iflag = 1
!
      if (   (phys_nod_name_ctl .eq. geostrophic_balance%name)          &
     &      )   iflag = 1
!
      if (   (phys_nod_name_ctl .eq. fhd_pre_mom            )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_pre_uxb            )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_chk_mom            )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_chk_uxb            )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_chk_mom_2          )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_chk_uxb_2          )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_forces             )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_rot_forces         )           &
     &      )   iflag = 1
!
      if(     check_force_vectors(phys_nod_name_ctl)                    &
     &   .or. check_rot_force(phys_nod_name_ctl)                        &
     &   .or. check_div_flux_tensor(phys_nod_name_ctl)                  &
     &   .or. check_gradient_field(phys_nod_name_ctl)                   &
     &   .or. check_vector_diffusion(phys_nod_name_ctl)                 &
     &   .or. check_field_product_vectors(phys_nod_name_ctl)            &
     &   .or. check_SGS_vector_terms(phys_nod_name_ctl)                 &
     &   .or. check_div_SGS_flux_tensor(phys_nod_name_ctl)              &
     &   .or. check_rot_SGS_terms(phys_nod_name_ctl)                    &
     &   .or. check_force_w_SGS(phys_nod_name_ctl)                      &
     &   .or. check_true_SGS_vector_terms(phys_nod_name_ctl)            &
     &   .or. check_true_div_SGS_flux_tensor(phys_nod_name_ctl)         &
     &   .or. check_filtered_force(phys_nod_name_ctl)                   &
     &   .or. check_rot_fil_force(phys_nod_name_ctl)                    &
     &   .or. check_wide_filter_vector(phys_nod_name_ctl)               &
     &   .or. check_wide_filter_grad(phys_nod_name_ctl)                 &
     &   .or. check_double_filter_grad(phys_nod_name_ctl)               &
     &   .or. check_double_filter_vector(phys_nod_name_ctl)             &
     &   .or. check_difference_vectors(phys_nod_name_ctl)               &
     &   .or. check_grad_filter_field(phys_nod_name_ctl)                &
     &   .or. check_diff_filter_vectors(phys_nod_name_ctl)              &
     &   .or. check_wide_SGS_vector_terms(phys_nod_name_ctl)            &
     &   .or. check_double_SGS_vector_terms(phys_nod_name_ctl)          &
     &        ) iflag = 1
!

      if (iflag .eq. 1) then
        icou = icou + 1
        phys_nod_name = phys_nod_name_ctl
        num_nod_component = 3
      end if
!
      end subroutine set_vector_field_name
!
! -----------------------------------------------------------------------
!
      subroutine set_scalar_field_name(phys_nod_name_ctl, icou,         &
     &          phys_nod_name, num_nod_component, iflag)
!
      use t_diffusion_term_labels
      use t_SGS_term_labels
      use t_SGS_model_coef_labels
!
      use m_div_force_labels
      use m_true_SGS_term_labels
      use m_div_filtered_force_labels
      use m_grad_filter_field_labels
!
      character(len = kchara), intent(in) :: phys_nod_name_ctl
      integer (kind = kint), intent(inout) :: icou
      integer (kind = kint), intent(inout) :: num_nod_component
      character(len = kchara), intent(inout) :: phys_nod_name
      integer (kind = kint), intent(inout) :: iflag
!
!  set number of components ( vector and scalar )
!
      if (iflag .gt. 0) return
!
      if (   (phys_nod_name_ctl .eq. fhd_press                )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_temp                 )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_light                )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_mag_potential        )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_scalar_potential     )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_entropy              )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_heat_source          )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_light_source         )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_entropy_source       )         &
     &      )   iflag = 1
!
      if (    (phys_nod_name_ctl .eq. fhd_ref_temp            )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_density             )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_ref_light           )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_part_light          )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_per_density         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_ref_density         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_per_entropy         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_ref_entropy         )         &
     &      )   iflag = 1
!
      if (    (phys_nod_name_ctl .eq. fhd_filter_temp         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_filter_pert_temp    )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_filter_comp         )         &
     &      )   iflag = 1
!
      if (    (phys_nod_name_ctl .eq. fhd_part_temp           )         &
     &      )   iflag = 1
!
      if (    (phys_nod_name_ctl .eq. fhd_pre_heat            )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_pre_composit        )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_pre_press           )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_heat            )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_composit        )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_press           )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_potential       )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_heat_2          )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_composit_2      )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_press_2         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_potential_2     )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_div_forces          )         &
     &      )   iflag = 1
!
      if(     check_enegy_fluxes(phys_nod_name_ctl)                     &
     &   .or. check_scalar_advection(phys_nod_name_ctl)                 &
     &   .or. check_div_force(phys_nod_name_ctl)                        &
     &   .or. check_div_scalar_flux(phys_nod_name_ctl)                  &
     &   .or. check_divergence_field(phys_nod_name_ctl)                 &
     &   .or. check_scalar_diffusion(phys_nod_name_ctl)                 &
     &   .or. check_field_product_scalars(phys_nod_name_ctl)            &
     &   .or. check_div_SGS_flux_vector(phys_nod_name_ctl)              &
     &   .or. check_SGS_ene_fluxes(phys_nod_name_ctl)                   &
     &   .or. check_SGS_moedel_coefs(phys_nod_name_ctl)                 &
     &   .or. check_true_div_SGS_flux_vector(phys_nod_name_ctl)         &
     &   .or. check_true_SGS_ene_fluxes(phys_nod_name_ctl)              &
     &   .or. check_filtered_scallar_flux(phys_nod_name_ctl)            &
     &   .or. check_div_fil_force(phys_nod_name_ctl)                    &
     &   .or. check_filter_enegy_fluxes(phys_nod_name_ctl)              &
     &   .or. check_wide_filter_scalar(phys_nod_name_ctl)               &
     &   .or. check_double_filter_scalar(phys_nod_name_ctl)             &
     &   .or. check_div_filter_field(phys_nod_name_ctl)                 &
     &   .or. check_work_4_poisson(phys_nod_name_ctl)                   &
     &   .or. check_commute_SGS_work(phys_nod_name_ctl)) iflag = 1
!
      if (iflag .eq. 1) then
        icou = icou + 1
        phys_nod_name = phys_nod_name_ctl
        num_nod_component = 1
      end if
!
!   Old field label... Should be deleted later!!
      if(phys_nod_name_ctl .eq. buoyancy_work%name) then
        iflag = 1
        icou = icou + 1
        phys_nod_name = buoyancy_flux%name
        num_nod_component = 1
      end if
!
      end subroutine set_scalar_field_name
!
! -----------------------------------------------------------------------
!
      subroutine set_tensor_field_name(phys_nod_name_ctl, icou,         &
     &          phys_nod_name, num_nod_component, iflag)
!
      use t_SGS_term_labels
      use t_SGS_model_coef_labels
!
      character(len = kchara), intent(in) :: phys_nod_name_ctl
      integer (kind = kint), intent(inout) :: icou
      integer (kind = kint), intent(inout) :: num_nod_component
      character(len = kchara), intent(inout) :: phys_nod_name
      integer (kind = kint), intent(inout) :: iflag
!
!
!  set number of components ( vector and scalar )
!
      if (iflag .gt. 0) return
!
      if(     check_SGS_tensor_terms(phys_nod_name_ctl)                 &
     &   .or. check_flux_tensor_w_SGS(phys_nod_name_ctl)                &
     &   .or. check_flux_tensors(phys_nod_name_ctl)                     &
     &   .or. check_filtered_flux_tensor(phys_nod_name_ctl)) then
        iflag = 1
        icou = icou + 1
        phys_nod_name = phys_nod_name_ctl
        num_nod_component = n_sym_tensor
      end if
!
      if(    check_asym_flux_tensors(phys_nod_name_ctl)                 &
     &  .or. check_SGS_induction_tensor(phys_nod_name_ctl)              &
     &  .or. check_induction_tensor_w_SGS(phys_nod_name_ctl)) then
        iflag = 1
        icou = icou + 1
        phys_nod_name = phys_nod_name_ctl
        num_nod_component = 3
      end if
!
      if(check_dynamic_SGS_work(phys_nod_name_ctl)) then
        iflag = 1
        icou = icou + 1
        phys_nod_name = phys_nod_name_ctl
        num_nod_component = 6
      end if
!
      end subroutine set_tensor_field_name
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_vis_control_flag(visualize_ctl, iflag_viz)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: visualize_ctl
      integer (kind = kint), intent(inout) :: iflag_viz
!
      if (cmp_no_case(visualize_ctl, cflag_viz_on)) then
        iflag_viz = 1
      else
        iflag_viz = 0
      end if
!
      end subroutine check_vis_control_flag
!
! -----------------------------------------------------------------------
!
      subroutine check_monitor_control_flag(iflag, monitor_ctl,         &
     &          iflag_fld_monitor)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: monitor_ctl
      integer (kind = kint), intent(in) :: iflag
      integer (kind = kint), intent(inout) :: iflag_fld_monitor
!
      if (iflag .eq. 0) return
      if(cmp_no_case(monitor_ctl, cflag_monitor_on)) then
        iflag_fld_monitor = 1
      else
        iflag_fld_monitor = 0
      end if
!
      end subroutine check_monitor_control_flag
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_vis_control_flag(iflag_viz, visualize_ctl)
!
      integer (kind = kint), intent(in) :: iflag_viz
      character(len = kchara), intent(inout) :: visualize_ctl
!
      if(iflag_viz .gt. 0) then
        visualize_ctl = cflag_viz_on
      else
        visualize_ctl = cflag_viz_off
      end if
!
      end subroutine set_vis_control_flag
!
! -----------------------------------------------------------------------
!
      subroutine set_monitor_control_flag                               &
     &         (iflag_fld_monitor, monitor_ctl)
!
      integer (kind = kint), intent(in) :: iflag_fld_monitor
      character(len = kchara), intent(inout) :: monitor_ctl
!
      if(iflag_fld_monitor .gt. 0) then
        monitor_ctl = cflag_monitor_on
      else
        monitor_ctl = cflag_monitor_off
      end if
!
      end subroutine set_monitor_control_flag
!
! -----------------------------------------------------------------------
!
      end module set_nodal_field_name
