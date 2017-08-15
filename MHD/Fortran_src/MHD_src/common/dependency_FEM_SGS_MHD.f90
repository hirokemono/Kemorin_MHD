!>@file   dependency_FEM_SGS_MHD.f90
!!@brief  module dependency_FEM_SGS_MHD
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief  Check dependecy of field list fro MHD dynamo
!!
!!@verbatim
!!      subroutine set_FEM_SGS_MHD_field_data                           &
!!     &         (SGS_param, cmt_param, node, ele, MHD_prop,            &
!!     &          iphys, nod_fld, iphys_ele, ele_fld)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(phys_address), intent(inout) :: iphys, iphys_ele
!!        type(phys_data), intent(inout) :: nod_fld, ele_fld
!!@endverbatim
!
      module dependency_FEM_SGS_MHD
!
      use m_precision
      use m_error_IDs
      use m_machine_parameter
!
      use calypso_mpi
      use m_phys_labels
!
      use t_control_parameter
      use t_SGS_control_parameter
      use t_phys_data
      use t_phys_address
      use t_physical_property
!
      use check_dependency_for_MHD
!
      implicit none
!
      private :: check_dependence_4_FEM_SGS
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_FEM_SGS_MHD_field_data                            &
     &         (SGS_param, cmt_param, node, ele, MHD_prop,             &
     &          iphys, nod_fld, iphys_ele, ele_fld)
!
      use t_geometry_data
      use set_field_address
      use check_MHD_dependency_by_id
      use initialize_element_field
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(phys_address), intent(inout) :: iphys, iphys_ele
      type(phys_data), intent(inout) :: nod_fld, ele_fld
!
!
      call set_FEM_MHD_field_data                                       &
     &   (node, MHD_prop, iphys, nod_fld)
!
      call check_dependence_4_FEM_SGS (SGS_param, cmt_param,            &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, iphys, nod_fld)
!
      call alloc_phys_data_type(ele%numele, ele_fld)
      call set_element_field_address(ele_fld, iphys_ele)
!
      end subroutine set_FEM_SGS_MHD_field_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_dependence_4_FEM_SGS(SGS_param, cmt_param,       &
     &          fl_prop, cd_prop, ht_prop, cp_prop, iphys, fld)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: fld
!
!
      character(len=kchara) :: msg
!
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        if ( SGS_param%iflag_SGS_m_flux .ne. id_SGS_none) then
          msg = 'solving SGS momentum flux needs'
          call check_missing_field_w_msg(fld, msg, iphys%i_SGS_m_flux)
        end if
!
        if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
          msg = 'solving SGS lorentz term needs'
          call check_missing_field_w_msg(fld, msg, iphys%i_SGS_maxwell)
        end if
      end if
!
!
      if ( ht_prop%iflag_scheme .gt. id_no_evolution) then
        if ( SGS_param%iflag_SGS_h_flux .ne. id_SGS_none) then
          msg = 'solving SGS heat flux needs'
          call check_missing_field_w_msg(fld, msg, iphys%i_SGS_h_flux)
        end if
      end if
!
!
      if ( cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if ( SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
          msg = 'solving SGS magnetic induction needs'
          call check_missing_field_w_msg                                &
     &       (fld, msg, iphys%i_SGS_induct_t)
          call check_missing_field_w_msg                                &
     &       (fld, msg, iphys%i_SGS_vp_induct)
        end if
      end if
!
!
      if ( cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if ( SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
          msg = 'solving SGS induction needs'
          call check_missing_field_w_msg                                &
     &       (fld, msg, iphys%i_SGS_vp_induct)
        end if
      end if
!
!
      if ( cp_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_c_flux .ne. id_SGS_none) then
          msg = 'solving SGS compsition flux needs'
          call check_missing_field_w_msg(fld, msg, iphys%i_SGS_c_flux)
        end if
      end if
!
      if ( fl_prop%iflag_scheme .gt. id_no_evolution) then
        if ( SGS_param%iflag_SGS_m_flux .eq. id_SGS_similarity          &
     &     .and. SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
          msg = 'SGS momentum flux needs'
          call check_missing_field_w_msg(fld, msg, iphys%i_filter_velo)
        end if
!
        if (     SGS_param%iflag_SGS_lorentz .eq. id_SGS_similarity     &
     &     .and. SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
          msg = 'SGS Lorentz term needs'
          call check_missing_field_w_msg                                &
     &       (fld, msg, iphys%i_filter_magne)
        end if
      end if
!
      if(SGS_param%iflag_SGS_gravity .gt. id_SGS_none) then
        if(fl_prop%iflag_4_gravity .eq. id_turn_OFF                     &
     &     .and. fl_prop%iflag_4_composit_buo .eq. id_turn_OFF) then
          call calypso_MPI_abort(ierr_fld,                              &
     &       'set one of buoyancy sources')
        end if
        if(fl_prop%iflag_4_gravity .gt. id_turn_OFF) then
          if(SGS_param%iflag_SGS_m_flux.eq.id_SGS_none                  &
     &       .or. SGS_param%iflag_SGS_h_flux.eq.id_SGS_none) then
            call calypso_MPI_abort(ierr_fld,                            &
     &          'Turn on SGS momentum flux and heat flux')
          end if
        end if
        if(fl_prop%iflag_4_composit_buo .gt. id_turn_OFF) then
          if(SGS_param%iflag_SGS_m_flux .eq. id_SGS_none                &
     &       .or. SGS_param%iflag_SGS_c_flux .eq. id_SGS_none) then
              call calypso_MPI_abort(ierr_fld,                          &
     &          'Turn on SGS momentum flux and composition flux')
          end if
        end if
      end if
!
      if ( ht_prop%iflag_scheme .gt. id_no_evolution) then
        if    (SGS_param%iflag_SGS_h_flux .eq. id_SGS_similarity        &
     &   .and. SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
          msg = 'SGS heat flux needs'
          call check_missing_field_w_msg(fld, msg, iphys%i_filter_temp)
        end if
      end if
!
!
      if (    cd_prop%iflag_Bevo_scheme .gt. id_no_evolution            &
     &   .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if    (SGS_param%iflag_SGS_uxb .eq. id_SGS_similarity           &
     &   .and. SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
          msg = 'SGS induction needs'
          call check_missing_field_w_msg(fld, msg, iphys%i_filter_velo)
          call check_missing_field_w_msg                                &
     &       (fld, msg, iphys%i_filter_magne)
        end if
      end if
!
!
      if ( cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if (   cmt_param%iflag_commute .gt. id_SGS_commute_OFF          &
     &   .and. SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
          msg = 'filterd A is required for dynamic model'
          call check_missing_field_w_msg(fld, msg, iphys%i_filter_vecp)
        end if
      end if
!
!
      end subroutine check_dependence_4_FEM_SGS
!
! -----------------------------------------------------------------------
!
      end module dependency_FEM_SGS_MHD
