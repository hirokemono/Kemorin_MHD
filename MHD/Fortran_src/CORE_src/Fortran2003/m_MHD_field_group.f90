!>@file   m_MHD_field_group.f90
!!        module m_MHD_field_group
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      type(c_ptr) function c_link_MHD_field_groups_f(c_ctl)           &
!!     &          bind(C, name = 'c_link_MHD_field_groups_f')
!!      type(c_ptr) function c_link_MHD_sym_field_groups_f()            &
!!     &           bind(C, name = 'c_link_MHD_sym_field_groups_f')
!!      type(c_ptr) function c_link_SGS_MHD_field_groups_f()            &
!!     &           bind(C, name = 'c_link_SGS_MHD_field_groups_f')
!!@endverbatim
!!
      module m_MHD_field_group
!
      use ISO_C_BINDING
!
      use m_precision
      use t_control_array_character
!
      use t_base_field_labels
      use t_base_force_labels
      use t_diffusion_term_labels
      use t_energy_flux_labels
      use t_grad_field_labels
      use t_field_product_labels
      use t_explicit_term_labels
      use t_diff_vector_labels
!
      use m_rot_force_labels
      use m_div_force_labels
!
      use m_field_w_symmetry_labels
      use m_force_w_sym_labels
      use m_energy_flux_w_sym_labels
!
      use m_field_w_symmetry_labels
      use m_force_w_sym_labels
      use m_energy_flux_w_sym_labels
!
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use t_SGS_model_coef_labels
      use t_diff_vector_labels
!
      use m_diff_SGS_term_labels
      use m_filtered_field_labels
      use m_filtered_force_labels
      use m_filtered_ene_flux_labels
      use m_diff_filter_vect_labels
      use m_div_filtered_force_labels
      use m_rot_filtered_force_labels
      use m_grad_filter_field_labels
      use m_wide_SGS_term_labels
      use m_wide_filter_field_labels
      use m_dble_filter_field_labels
      use m_force_w_SGS_labels
      use m_true_SGS_term_labels

      implicit  none
! 
      integer(kind = kint), parameter :: ngrp_MHD_fields = 14
      character(len = kchara), parameter                                &
     &              :: MHD_field_group(ngrp_MHD_fields)                 &
     &                  = (/"Base_fields                 ",             &
     &                      "Forces                      ",             &
     &                      "Enegy_fluxes                ",             &
     &                      "Diffusion_terms             ",             &
     &                      "Rotation_of_forces          ",             &
     &                      "Divergence_of_forces        ",             &
     &                      "Products_of_fields          ",             &
     &                      "Gradient_of_scalar          ",             &
     &                      "Divergence_of_fields        ",             &
     &                      "Diffusivities               ",             &
     &                      "Work_area_for_explicit_terms",             &
     &                      "Field_Checks                ",             &
     &                      "components_of_U_and_B       ",             &
     &                      "Diffreenciation_of_vector   "/)
!
      integer(kind = kint), parameter :: ngrp_MHD_sym_fields = 3
      character(len = kchara), parameter                                &
     &              :: MHD_sym_field_group(ngrp_MHD_sym_fields)         &
     &                  = (/"Fields_w_symmtery       ",                 &
     &                      "Forces_w_symmtery       ",                 &
     &                      "Energy_fluxes_w_symmtery"/)
!
      integer(kind = kint), parameter :: ngrp_SGS_MHD_fields = 18
      character(len = kchara), parameter                                &
     &              :: SGS_MHD_field_group(ngrp_SGS_MHD_fields)         &
     &                  = (/"SGS_terms                         ",       &
     &                      "Energy_fluxes_by_SGS_term         ",       &
     &                      "Differential_of_SGS_terms         ",       &
     &                      "Model_coefficients_of_SGS_term    ",       &
     &                      "Filtered_fields                   ",       &
     &                      "Filtered_force                    ",       &
     &                      "Energy_fluxes_by_filtered_force   ",       &
     &                      "Rotation_of_Filtered_force        ",       &
     &                      "Divergence_of_Filtered_force      ",       &
     &                      "Gradient_of_filtered_fields       ",       &
     &                      "Divergence_of_filtered_fields     ",       &
     &                      "Wide_filtered_fields              ",       &
     &                      "Double_filtered_fields            ",       &
     &                      "Diffreenciation_of_filtered_vector",       &
     &                      "SGS_terms_by_wider_filtering      ",       &
     &                      "Forces_including_SGS_term         ",       &
     &                      "True_SGS_terms                    ",       &
     &                      "Work_area_for_SGS_model           "/)
!
      type(ctl_array_chara), save, private, target :: MHD_fld_grp_list
      type(ctl_array_chara), save, private, target :: sym_fld_grp_list
      type(ctl_array_chara), save, private, target :: SGS_fld_grp_list
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      type(c_ptr) function c_link_MHD_field_groups_f(c_ctl)             &
     &          bind(C, name = 'c_link_MHD_field_groups_f')
!
      type(c_ptr), value, intent(in) :: c_ctl
      integer(kind = kint) :: i
!
      if(.not. allocated(MHD_fld_grp_list%c_tbl)) then
        MHD_fld_grp_list%array_name = '  '
        MHD_fld_grp_list%num =         0
        call alloc_control_array_chara(MHD_fld_grp_list)
!
        do i = 1, ngrp_MHD_fields
          call append_c_to_ctl_array(MHD_field_group(i),                &
     &                               MHD_fld_grp_list)
        end do
      end if
!
      c_link_MHD_field_groups_f = C_loc(MHD_fld_grp_list)
!
      end function c_link_MHD_field_groups_f
!
! ----------------------------------------------------------------------
!
      type(c_ptr) function c_link_MHD_sym_field_groups_f()              &
     &           bind(C, name = 'c_link_MHD_sym_field_groups_f')
!
      integer(kind = kint) :: i
!
      if(.not. allocated(sym_fld_grp_list%c_tbl)) then
        sym_fld_grp_list%array_name = '  '
        sym_fld_grp_list%num =         0
        call alloc_control_array_chara(sym_fld_grp_list)
!
        do i = 1, ngrp_MHD_sym_fields
          call append_c_to_ctl_array(MHD_sym_field_group(i),            &
     &                               sym_fld_grp_list)
        end do
      end if
!
      c_link_MHD_sym_field_groups_f = C_loc(sym_fld_grp_list)
!
      end function c_link_MHD_sym_field_groups_f
!
! ----------------------------------------------------------------------
!
      type(c_ptr) function c_link_SGS_MHD_field_groups_f()              &
     &           bind(C, name = 'c_link_SGS_MHD_field_groups_f')
!
      integer(kind = kint) :: i
!
      if(.not. allocated(SGS_fld_grp_list%c_tbl)) then
        SGS_fld_grp_list%array_name = '  '
        SGS_fld_grp_list%num =         0
        call alloc_control_array_chara(SGS_fld_grp_list)
!
        do i = 1, ngrp_SGS_MHD_fields
          call append_c_to_ctl_array(SGS_MHD_field_group(i),            &
     &                               SGS_fld_grp_list)
        end do
      end if
!
      c_link_SGS_MHD_field_groups_f = C_loc(SGS_fld_grp_list)
!
      end function c_link_SGS_MHD_field_groups_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_filxed_lengh_chara(num, field_in, field_out)
!
      integer(kind = kint), intent(in) :: num
      character(len = kchara), intent(in) :: field_in(num)
      character(len = kchara), intent(inout) :: field_out(num)
!
      integer(kind = kint) :: i
!
      do i = 1, num
        write(field_out(i), '(a,a1)') trim(field_in(i)) // char(0)
      end do
!
      end subroutine copy_filxed_lengh_chara
!
! ----------------------------------------------------------------------
!
      end module m_MHD_field_group
