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
!!      subroutine set_vector_field_name                                &
!!     &         (phys_name_ctl, phys_name, num_component, flag)
!!      subroutine set_scalar_field_name                                &
!!     &         (phys_name_ctl, phys_name, num_component, flag)
!!      subroutine set_tensor_field_name                                &
!!     &         (phys_name_ctl, phys_name, num_component, flag)
!!
!!      logical function check_vis_control_flag(visualize_ctl)
!!      integer(kind = kint) function check_monitor_control_flag        &
!!     &                            (monitor_ctl)
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
      subroutine set_vector_field_name                                  &
     &         (phys_name_ctl, phys_name, num_component, flag)
!
      use t_base_field_labels
      use t_diffusion_term_labels
!
      use t_SGS_term_labels
!
      use m_rot_force_labels
      use m_div_force_labels
!
      use m_force_w_SGS_labels
      use m_diff_SGS_term_labels
      use m_true_SGS_term_labels
      use m_filtered_field_labels
      use m_filtered_force_labels
      use m_diff_filter_vect_labels
      use m_rot_filtered_force_labels
      use m_div_filtered_force_labels
      use m_grad_filter_field_labels
      use m_dble_filter_field_labels
      use m_wide_filter_field_labels
      use m_wide_SGS_term_labels
!
      character(len = kchara), intent(in) :: phys_name_ctl
      integer (kind = kint), intent(inout) :: num_component
      character(len = kchara), intent(inout) :: phys_name
      logical, intent(inout) :: flag
!
!  set number of components ( vector and scalar )
!
      if(flag) return
!
      flag =  check_base_vector(phys_name_ctl)                          &
     &   .or. check_force_vectors(phys_name_ctl)                        &
     &   .or. check_rot_force(phys_name_ctl)                            &
     &   .or. check_div_flux_tensor(phys_name_ctl)                      &
     &   .or. check_gradient_field(phys_name_ctl)                       &
     &   .or. check_vector_diffusion(phys_name_ctl)                     &
     &   .or. check_field_product_vectors(phys_name_ctl)                &
     &   .or. check_vector_work_field(phys_name_ctl)                    &
     &   .or. check_vector_check_field(phys_name_ctl)                   &
     &   .or. check_SGS_vector_terms(phys_name_ctl)                     &
     &   .or. check_div_SGS_flux_tensor(phys_name_ctl)                  &
     &   .or. check_rot_SGS_terms(phys_name_ctl)                        &
     &   .or. check_force_w_SGS(phys_name_ctl)                          &
     &   .or. check_true_SGS_vector_terms(phys_name_ctl)                &
     &   .or. check_true_div_SGS_flux_tensor(phys_name_ctl)             &
     &   .or. check_filter_vector(phys_name_ctl)                        &
     &   .or. check_filtered_force(phys_name_ctl)                       &
     &   .or. check_rot_fil_force(phys_name_ctl)                        &
     &   .or. check_wide_filter_vector(phys_name_ctl)                   &
     &   .or. check_wide_filter_grad(phys_name_ctl)                     &
     &   .or. check_double_filter_grad(phys_name_ctl)                   &
     &   .or. check_double_filter_vector(phys_name_ctl)                 &
     &   .or. check_difference_vectors(phys_name_ctl)                   &
     &   .or. check_grad_filter_field(phys_name_ctl)                    &
     &   .or. check_diff_filter_vectors(phys_name_ctl)                  &
     &   .or. check_wide_SGS_vector_terms(phys_name_ctl)                &
     &   .or. check_double_SGS_vector_terms(phys_name_ctl)
      if(flag) then
        phys_name = phys_name_ctl
        num_component = n_vector
        return
      end if
!
      if(phys_name_ctl .eq. geostrophic_balance%name) then
        flag = .TRUE.
        phys_name = rest_of_geostrophic%name
        num_component = n_vector
        return
      end if
!
      end subroutine set_vector_field_name
!
! -----------------------------------------------------------------------
!
      subroutine set_scalar_field_name                                  &
     &         (phys_name_ctl, phys_name, num_component, flag)
!
      use t_base_field_labels
      use t_diffusion_term_labels
!
      use t_SGS_term_labels
      use t_SGS_model_coef_labels
!
      use m_div_force_labels
!
      use m_diff_SGS_term_labels
      use m_true_SGS_term_labels
      use m_filtered_field_labels
      use m_filtered_force_labels
      use m_filtered_ene_flux_labels
      use m_div_filtered_force_labels
      use m_grad_filter_field_labels
      use m_wide_filter_field_labels
      use m_dble_filter_field_labels
!
      character(len = kchara), intent(in) :: phys_name_ctl
      integer (kind = kint), intent(inout) :: num_component
      character(len = kchara), intent(inout) :: phys_name
      logical, intent(inout) :: flag
!
!  set number of components ( vector and scalar )
!
      if(flag) return
!
      flag =  check_base_scalar(phys_name_ctl)                          &
     &   .or. check_enegy_fluxes(phys_name_ctl)                         &
     &   .or. check_scalar_advection(phys_name_ctl)                     &
     &   .or. check_div_force(phys_name_ctl)                            &
     &   .or. check_div_scalar_flux(phys_name_ctl)                      &
     &   .or. check_divergence_field(phys_name_ctl)                     &
     &   .or. check_scalar_diffusion(phys_name_ctl)                     &
     &   .or. check_field_product_scalars(phys_name_ctl)                &
     &   .or. check_scalar_work_field(phys_name_ctl)                    &
     &   .or. check_scalar_check_field(phys_name_ctl)                   &
     &   .or. check_div_SGS_flux_vector(phys_name_ctl)                  &
     &   .or. check_SGS_ene_fluxes(phys_name_ctl)                       &
     &   .or. check_SGS_moedel_coefs(phys_name_ctl)                     &
     &   .or. check_true_div_SGS_flux_vector(phys_name_ctl)             &
     &   .or. check_true_SGS_ene_fluxes(phys_name_ctl)                  &
     &   .or. check_filter_scalar(phys_name_ctl)                        &
     &   .or. check_filtered_scalar_flux(phys_name_ctl)                 &
     &   .or. check_div_fil_force(phys_name_ctl)                        &
     &   .or. check_filter_enegy_fluxes(phys_name_ctl)                  &
     &   .or. check_wide_filter_scalar(phys_name_ctl)                   &
     &   .or. check_double_filter_scalar(phys_name_ctl)                 &
     &   .or. check_div_filter_field(phys_name_ctl)                     &
     &   .or. check_work_4_poisson(phys_name_ctl)                       &
     &   .or. check_commute_SGS_work(phys_name_ctl)
      if(flag) then
        phys_name = phys_name_ctl
        num_component = n_scalar
        return
      end if
!
!   Old field label... Should be deleted later!!
      if(phys_name_ctl .eq. buoyancy_work%name) then
        flag = .TRUE.
        phys_name = buoyancy_flux%name
        num_component = n_scalar
        return
      end if
!
      end subroutine set_scalar_field_name
!
! -----------------------------------------------------------------------
!
      subroutine set_tensor_field_name                                  &
     &         (phys_name_ctl, phys_name, num_component, flag)
!
      use t_SGS_term_labels
      use t_SGS_model_coef_labels
      use m_filtered_force_labels
      use m_force_w_SGS_labels
!
      character(len = kchara), intent(in) :: phys_name_ctl
      integer (kind = kint), intent(inout) :: num_component
      character(len = kchara), intent(inout) :: phys_name
      logical, intent(inout) :: flag
!
!
!  set number of components ( vector and scalar )
!
      if(flag) return
!
      flag =  check_SGS_tensor_terms(phys_name_ctl)                     &
     &   .or. check_flux_tensor_w_SGS(phys_name_ctl)                    &
     &   .or. check_flux_tensors(phys_name_ctl)                         &
     &   .or. check_filtered_flux_tensor(phys_name_ctl)
      if(flag) then
        phys_name = phys_name_ctl
        num_component = n_sym_tensor
        return
      end if
!
      flag =  check_asym_flux_tensors(phys_name_ctl)                    &
     &  .or. check_SGS_induction_tensor(phys_name_ctl)                  &
     &  .or. check_induction_tensor_w_SGS(phys_name_ctl)
      if(flag) then
        phys_name = phys_name_ctl
        num_component = 3
        return
      end if
!
      flag =  check_dynamic_SGS_work(phys_name_ctl)
      if(flag) then
        phys_name = phys_name_ctl
        num_component = 6
        return
      end if
!
      end subroutine set_tensor_field_name
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      logical function check_vis_control_flag(visualize_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: visualize_ctl
!
      check_vis_control_flag = cmp_no_case(visualize_ctl, cflag_viz_on)
!
      end function check_vis_control_flag
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function check_monitor_control_flag          &
     &                            (monitor_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: monitor_ctl
!
      if(cmp_no_case(monitor_ctl, cflag_monitor_on)) then
        check_monitor_control_flag = 1
      else
        check_monitor_control_flag = 0
      end if
!
      end function check_monitor_control_flag
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
