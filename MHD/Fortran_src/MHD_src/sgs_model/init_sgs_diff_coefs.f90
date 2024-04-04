!>@file   init_sgs_diff_coefs.f90
!!        module init_sgs_diff_coefs
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief initialize model coefficients for commutation
!!
!!@verbatim
!!      subroutine define_sgs_diff_coefs(numele, SGS_param, cmt_param,  &
!!     &          layer_tbl, MHD_prop, wk_diff, Csims_FEM_MHD)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!!@end verbatim
!
      module init_sgs_diff_coefs
!
      use m_precision
      use m_machine_parameter
      use t_SGS_control_parameter
      use t_control_parameter
      use t_physical_property
      use t_base_field_labels
      use t_SGS_term_labels
      use t_FEM_SGS_model_coefs
!
      implicit none
!
      private :: count_sgs_diff_coefs, set_sgs_diff_addresses
      private :: check_sgs_diff_addresses
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine def_sgs_commute_component(SGS_par, mesh, layer_tbl,    &
     &          MHD_prop, Csims_FEM_MHD, FEM_SGS_wk)
!
      use t_mesh_data
      use t_layering_ele_list
      use t_work_FEM_dynamic_SGS
      use count_sgs_components
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(layering_tbl), intent(in) :: layer_tbl
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!
!
      call define_sgs_components(mesh%node%numnod, mesh%ele%numele,     &
     &    SGS_par%model_p, layer_tbl, MHD_prop, FEM_SGS_wk%wk_sgs,      &
     &    Csims_FEM_MHD)
      call define_sgs_diff_coefs(mesh%ele%numele,                       &
     &    SGS_par%model_p, SGS_par%commute_p, layer_tbl, MHD_prop,      &
     &    FEM_SGS_wk%wk_diff, Csims_FEM_MHD)
!
      end subroutine def_sgs_commute_component
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine define_sgs_diff_coefs(numele, SGS_param, cmt_param,    &
     &          layer_tbl, MHD_prop, wk_diff, Csims_FEM_MHD)
!
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_FEM_SGS_model_coefs
!
      integer(kind = kint), intent(in) :: numele
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(layering_tbl), intent(in) :: layer_tbl
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!
      integer(kind = kint) :: num_diff_field, ntot_diff_comp
!
!
      call count_sgs_diff_coefs(SGS_param, cmt_param,                   &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    num_diff_field, ntot_diff_comp)
      call alloc_sgs_coefs_layer(layer_tbl%e_grp%num_grp,               &
     &    num_diff_field, ntot_diff_comp, wk_diff)
!
      call set_sgs_diff_addresses(SGS_param, cmt_param,                 &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    wk_diff, Csims_FEM_MHD%diff_coefs)
!
      if(Csims_FEM_MHD%diff_coefs%Cdiff_velo%iak_Csim .gt. 0) then
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_velo)
      else
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_velo)
      end if
!
      if(Csims_FEM_MHD%diff_coefs%Cdiff_magne%iak_Csim .gt. 0) then
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_magne)
      else
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_magne)
      end if
!
      if(Csims_FEM_MHD%diff_coefs%Cdiff_temp%iak_Csim .gt. 0) then
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_temp)
      else
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_temp)
      end if
!
      if(Csims_FEM_MHD%diff_coefs%Cdiff_light%iak_Csim .gt. 0) then
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_light)
      else
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_light)
      end if
!
!
      if(Csims_FEM_MHD%diff_coefs%Cdiff_SGS_uxb%iak_Csim .gt. 0) then
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_SGS_uxb)
      else
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_SGS_uxb)
      end if
!
      if(Csims_FEM_MHD%diff_coefs%Cdiff_SGS_lor%iak_Csim .gt. 0) then
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_SGS_lor)
      else
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_SGS_lor)
      end if
!
      if(Csims_FEM_MHD%diff_coefs%Cdiff_SGS_mf%iak_Csim .gt. 0) then
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_SGS_mf)
      else
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_SGS_mf)
      end if
!
      if(Csims_FEM_MHD%diff_coefs%Cdiff_SGS_hf%iak_Csim .gt. 0) then
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_SGS_hf)
      else
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_SGS_hf)
      end if
!
      if(Csims_FEM_MHD%diff_coefs%Cdiff_SGS_cf%iak_Csim .gt. 0) then
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_SGS_cf)
      else
         call alloc_SGS_model_coefficient(numele, ione,                 &
     &       Csims_FEM_MHD%diff_coefs%Cdiff_SGS_cf)
      end if
!
!
      if(iflag_debug .gt. 0) then
        call check_sgs_diff_addresses(wk_diff,                          &
     &                                Csims_FEM_MHD%diff_coefs)
      end if
!
      end subroutine define_sgs_diff_coefs
!
!  ------------------------------------------------------------------
!
      subroutine count_sgs_diff_coefs(SGS_param, cmt_param,             &
     &          fl_prop, cd_prop, ht_prop, cp_prop,                     &
     &          num_diff_field, ntot_diff_comp)
!
      use calypso_mpi
!
      use t_base_field_labels
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
!
      integer(kind = kint), intent(inout) :: num_diff_field
      integer(kind = kint), intent(inout) :: ntot_diff_comp
!
!    count coefficients for SGS terms
!
      num_diff_field = 0
      ntot_diff_comp = 0
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none) then
          if (SGS_param%SGS_heat%iflag_commute_flux                     &
     &       .eq. id_SGS_commute_ON) then
            num_diff_field = num_diff_field + 1
            ntot_diff_comp = ntot_diff_comp + 3
          end if
        end if
      end if
!
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%SGS_momentum%iflag_SGS_flux                        &
     &        .ne. id_SGS_none) then
          if(SGS_param%SGS_momentum%iflag_commute_flux                  &
     &      .eq. id_SGS_commute_ON) then
            num_diff_field = num_diff_field + 1
            ntot_diff_comp = ntot_diff_comp + 9
          end if
        end if
      end if
!
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
          if (cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON) then
            num_diff_field = num_diff_field + 1
            ntot_diff_comp = ntot_diff_comp + 9
          end if
        end if
      end if
!
      if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
          if(cmt_param%iflag_c_uxb .eq. id_SGS_commute_ON) then
            num_diff_field = num_diff_field + 1
            ntot_diff_comp = ntot_diff_comp + 9
          end if
        end if
      end if
!
      if (cp_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none) then
          if (SGS_param%SGS_light%iflag_commute_flux                    &
     &      .eq. id_SGS_commute_ON) then
            num_diff_field = num_diff_field + 1
            ntot_diff_comp = ntot_diff_comp + 3
          end if
        end if
      end if
!
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                         &
     &      .and. SGS_param%SGS_heat%iflag_commute_field                &
     &           .eq. id_SGS_commute_ON) then
          num_diff_field = num_diff_field + 1
          ntot_diff_comp = ntot_diff_comp + 3
        end if
      end if
!
      if (cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                         &
     &      .and. SGS_param%SGS_light%iflag_commute_field               &
     &           .eq. id_SGS_commute_ON) then
          num_diff_field = num_diff_field + 1
          ntot_diff_comp = ntot_diff_comp + 3
        end if
      end if
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                         &
     &       .and. SGS_param%SGS_momentum%iflag_commute_field           &
     &            .eq. id_SGS_commute_ON) then
          num_diff_field = num_diff_field + 1
          ntot_diff_comp = ntot_diff_comp + 9
        end if
      end if
!
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                         &
     &      .and. cmt_param%iflag_c_magne .eq. id_SGS_commute_ON) then
          num_diff_field = num_diff_field + 1
          ntot_diff_comp = ntot_diff_comp + 9
        end if
      end if
!
      if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .gt. id_SGS_none                         &
     &      .and. cmt_param%iflag_c_magne .eq. id_SGS_commute_ON) then
          num_diff_field = num_diff_field + 1
          ntot_diff_comp = ntot_diff_comp + 9
        end if
      end if
!
      end subroutine count_sgs_diff_coefs
!
!  ------------------------------------------------------------------
!
      subroutine set_sgs_diff_addresses(SGS_param, cmt_param,           &
     &          fl_prop, cd_prop, ht_prop, cp_prop,                     &
     &          wk_diff, diff_coefs)
!
      use calypso_mpi
      use t_base_field_labels
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_SGS_term_labels
!
      use m_base_field_labels
      use m_SGS_term_labels
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
!
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_commutation_coefs), intent(inout) :: diff_coefs
!
      integer(kind = kint) :: id, jd, num_comp
!
!
      id = 1
      jd = 1
      num_comp = 0
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none) then
          if (SGS_param%SGS_heat%iflag_commute_flux                     &
     &      .eq. id_SGS_commute_ON) then
            wk_diff%name(jd) = SGS_heat_flux%name
            diff_coefs%Cdiff_SGS_hf%icomp_Csim = id
            diff_coefs%Cdiff_SGS_hf%iak_Csim =   jd
            num_comp = n_vector
            id = id + num_comp
            jd = jd + 1
          end if
        end if
      end if
!
      num_comp = 0
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%SGS_momentum%iflag_SGS_flux                        &
     &     .ne. id_SGS_none) then
          if(SGS_param%SGS_momentum%iflag_commute_flux                  &
     &     .eq. id_SGS_commute_ON) then
            wk_diff%name(jd) = SGS_momentum_flux%name
            diff_coefs%Cdiff_SGS_mf%icomp_Csim = id
            diff_coefs%Cdiff_SGS_mf%iak_Csim =   jd
            num_comp = n_sym_tensor
            id = id + num_comp
            jd = jd + 1
           end if
        end if
      end if
!
      num_comp = 0
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
          if (cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON) then
            wk_diff%name(jd) = SGS_Lorentz%name
            diff_coefs%Cdiff_SGS_lor%icomp_Csim = id
            diff_coefs%Cdiff_SGS_lor%iak_Csim =   jd
            num_comp = n_sym_tensor
            id = id + num_comp
            jd = jd + 1
          end if
        end if
      end if
!
      num_comp = 0
       if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
         if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
           if (cmt_param%iflag_c_uxb .eq. id_SGS_commute_ON) then
             wk_diff%name(jd) = SGS_induction%name
             diff_coefs%Cdiff_SGS_uxb%icomp_Csim = id
             diff_coefs%Cdiff_SGS_uxb%iak_Csim =   jd
             num_comp = n_vector
             id = id + num_comp
             jd = jd + 1
           end if
         end if
       end if
!
      num_comp = 0
       if (cp_prop%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none) then
           if(SGS_param%SGS_light%iflag_commute_flux                    &
     &        .eq. id_SGS_commute_ON) then
             wk_diff%name(jd) = SGS_composit_flux%name
             diff_coefs%Cdiff_SGS_cf%icomp_Csim = id
             diff_coefs%Cdiff_SGS_cf%iak_Csim =   jd
             num_comp = n_vector
             id = id + num_comp
             jd = jd + 1
           end if
         end if
       end if
!
!
      num_comp = 0
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                        &
     &      .and. SGS_param%SGS_heat%iflag_commute_field               &
     &           .eq. id_SGS_commute_ON) then
            wk_diff%name(jd) = temperature%name
            diff_coefs%Cdiff_temp%icomp_Csim = id
            diff_coefs%Cdiff_temp%iak_Csim =   jd
            num_comp = n_scalar
            id = id + num_comp
            jd = jd + 1
        end if
      end if
!
      num_comp = 0
      if (cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                        &
     &      .and. SGS_param%SGS_light%iflag_commute_field              &
     &           .eq. id_SGS_commute_ON) then
            wk_diff%name(jd) = composition%name
            diff_coefs%Cdiff_light%icomp_Csim = id
            diff_coefs%Cdiff_light%iak_Csim =   jd
            num_comp = n_scalar
            id = id + num_comp
            jd = jd + 1
        end if
      end if
!
      num_comp = 0
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                        &
     &      .and. SGS_param%SGS_momentum%iflag_commute_field           &
     &           .eq. id_SGS_commute_ON) then
            wk_diff%name(jd) = velocity%name
            diff_coefs%Cdiff_velo%icomp_Csim = id
            diff_coefs%Cdiff_velo%iak_Csim =   jd
            num_comp = n_vector
            id = id + num_comp
            jd = jd + 1
        end if
      end if
!
      num_comp = 0
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                        &
     &      .and. cmt_param%iflag_c_magne .eq. id_SGS_commute_ON) then
            wk_diff%name(jd) = magnetic_field%name
            diff_coefs%Cdiff_magne%icomp_Csim = id
            diff_coefs%Cdiff_magne%iak_Csim =   jd
            num_comp = n_vector
            id = id + num_comp
            jd = jd + 1
        end if
      else if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS .ne. id_SGS_none                        &
     &      .and. cmt_param%iflag_c_magne .eq. id_SGS_commute_ON) then
            diff_coefs%Cdiff_magne%icomp_Csim = id
            diff_coefs%Cdiff_magne%iak_Csim =   jd
            wk_diff%name(jd) = magnetic_field%name
            num_comp = n_vector
            id = id + n_vector
            jd = jd + 1
        end if
      end if
!
      end subroutine set_sgs_diff_addresses
!
!  ------------------------------------------------------------------
!
      subroutine check_sgs_diff_addresses(wk_diff, diff_coefs)
!
      use calypso_mpi
!
      use t_base_field_labels
      use t_SGS_term_labels
      use t_ele_info_4_dynamic
!
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_commutation_coefs), intent(inout) :: diff_coefs
!
!
      write(*,*) 'diff_coefs%num_field', wk_diff%num_kinds
      write(*,*) 'wk_diff%ntot_comp', wk_diff%ntot_comp
!
        if(diff_coefs%Cdiff_SGS_hf%iak_Csim .gt. 0) then
          write(*,*) 'iak_diff_hf', diff_coefs%Cdiff_SGS_hf%iak_Csim,   &
     &        diff_coefs%Cdiff_SGS_hf%icomp_Csim,                       &
     &        diff_coefs%Cdiff_SGS_hf%num_comp,                         &
     &        trim(wk_diff%name(diff_coefs%Cdiff_SGS_hf%iak_Csim))
        end if
!
        if(diff_coefs%Cdiff_SGS_cf%iak_Csim .gt. 0) then
          write(*,*) 'iak_diff_hf', diff_coefs%Cdiff_SGS_cf%iak_Csim,   &
     &        diff_coefs%Cdiff_SGS_cf%icomp_Csim,                       &
     &        diff_coefs%Cdiff_SGS_cf%num_comp,                         &
     &        trim(wk_diff%name(diff_coefs%Cdiff_SGS_cf%iak_Csim))
        end if
!
        if(diff_coefs%Cdiff_SGS_mf%iak_Csim .gt. 0) then
          write(*,*) 'iak_diff_mf',                                     &
     &        diff_coefs%Cdiff_SGS_mf%iak_Csim,                         &
     &        diff_coefs%Cdiff_SGS_mf%icomp_Csim,                       &
     &        diff_coefs%Cdiff_SGS_mf%num_comp,                         &
     &        trim(wk_diff%name(diff_coefs%Cdiff_SGS_mf%iak_Csim))
        end if
        if(diff_coefs%Cdiff_SGS_lor%iak_Csim .gt. 0) then
          write(*,*) 'iak_diff_lor',                                    &
     &        diff_coefs%Cdiff_SGS_lor%iak_Csim,                        &
     &        diff_coefs%Cdiff_SGS_lor%icomp_Csim,                      &
     &        diff_coefs%Cdiff_SGS_lor%num_comp,                        &
     &        trim(wk_diff%name(diff_coefs%Cdiff_SGS_lor%iak_Csim))
        end if
        if(diff_coefs%Cdiff_SGS_uxb%iak_Csim .gt. 0) then
          write(*,*) 'iak_diff_uxb',                                    &
     &        diff_coefs%Cdiff_SGS_uxb%iak_Csim,                        &
     &        diff_coefs%Cdiff_SGS_uxb%icomp_Csim,                      &
     &        diff_coefs%Cdiff_SGS_uxb%num_comp,                        &
     &        trim(wk_diff%name(diff_coefs%Cdiff_SGS_uxb%iak_Csim))
        end if
!
        if(diff_coefs%Cdiff_temp%iak_Csim .gt. 0) then
          write(*,*) 'iak_diff_t',                                      &
     &        diff_coefs%Cdiff_temp%iak_Csim,                           &
     &        diff_coefs%Cdiff_temp%icomp_Csim,                         &
     &        diff_coefs%Cdiff_temp%num_comp,                           &
     &        trim(wk_diff%name(diff_coefs%Cdiff_temp%iak_Csim))
        end if
        if(diff_coefs%Cdiff_velo%iak_Csim .gt. 0) then
          write(*,*) 'iak_diff_v',                                      &
     &        diff_coefs%Cdiff_velo%iak_Csim,                           &
     &        diff_coefs%Cdiff_velo%icomp_Csim,                         &
     &        diff_coefs%Cdiff_velo%num_comp,                           &
     &        trim(wk_diff%name(diff_coefs%Cdiff_velo%iak_Csim))
        end if
        if(diff_coefs%Cdiff_magne%iak_Csim .gt. 0) then
          write(*,*) 'iak_diff_b',                                      &
     &        diff_coefs%Cdiff_magne%iak_Csim,                          &
     &        diff_coefs%Cdiff_magne%icomp_Csim,                        &
     &        diff_coefs%Cdiff_magne%num_comp,                          &
     &        trim(wk_diff%name(diff_coefs%Cdiff_magne%iak_Csim))
        end if
        if(diff_coefs%Cdiff_light%iak_Csim .gt. 0) then
          write(*,*) 'iak_diff_c',                                      &
     &        diff_coefs%Cdiff_light%iak_Csim,                          &
     &        diff_coefs%Cdiff_light%icomp_Csim,                        &
     &        diff_coefs%Cdiff_light%num_comp,                          &
     &        trim(wk_diff%name(diff_coefs%Cdiff_light%iak_Csim))
        end if
!
      end subroutine check_sgs_diff_addresses
!
! -------------------------------------------------------------------
!
      end module init_sgs_diff_coefs
