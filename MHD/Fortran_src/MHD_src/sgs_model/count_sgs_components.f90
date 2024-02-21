!
!     module count_sgs_components
!
!      Written by H. Matsui on 2004
!      Modified by H. Matsui on July, 2007
!
!!      subroutine define_sgs_components                                &
!!     &         (numnod, numele, SGS_param, layer_tbl, MHD_prop,       &
!!     &          wk_sgs, Csims_FEM_MHD)
!!
!!      subroutine set_sgs_addresses                                    &
!!     &          (SGS_param, fl_prop, cd_prop, ht_prop, cp_prop,       &
!!     &           wk_sgs, sgs_coefs)
!!      subroutine s_count_sgs_components(SGS_param,                    &
!!     &          fl_prop, cd_prop, ht_prop, cp_prop, sgs_coefs)
!!      subroutine set_SGS_ele_fld_addresses(cd_prop, SGS_param,        &
!!     &                                     mhd_fem_wk)
!!      subroutine check_sgs_addresses(wk_sgs, sgs_coefs)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module count_sgs_components
!
      use m_precision
      use m_machine_parameter
      use t_FEM_SGS_model_coefs
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine define_sgs_components                                  &
     &         (numnod, numele, SGS_param, layer_tbl, MHD_prop,         &
     &          wk_sgs, Csims_FEM_MHD)
!
      use calypso_mpi
!
      use t_control_parameter
      use t_SGS_control_parameter
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_physical_property
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(layering_tbl), intent(in) :: layer_tbl
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!
      integer(kind = kint) :: num_SGS_terms, ntot_SGS_comps
!
!
      call s_count_sgs_components(SGS_param,                            &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    num_SGS_terms, ntot_SGS_comps)
!
!   set index for model coefficients
!
      call alloc_sgs_coefs_layer(layer_tbl%e_grp%num_grp,               &
     &    num_SGS_terms, ntot_SGS_comps, wk_sgs)
!
      call set_sgs_addresses(numnod, numele, SGS_param,                 &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    wk_sgs, Csims_FEM_MHD%sgs_coefs)
      call check_sgs_addresses(wk_sgs, Csims_FEM_MHD%sgs_coefs)
!
      end subroutine define_sgs_components
!
!  ------------------------------------------------------------------
!
      subroutine s_count_sgs_components(SGS_param,                      &
     &          fl_prop, cd_prop, ht_prop, cp_prop,                     &
     &          num_SGS_terms, ntot_SGS_comps)
!
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_physical_property
      use t_physical_property
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      integer(kind = kint), intent(inout) :: num_SGS_terms
      integer(kind = kint), intent(inout) :: ntot_SGS_comps
!
!    count coefficients for SGS terms
!
      num_SGS_terms =  0
      ntot_SGS_comps = 0
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none) then
          num_SGS_terms = num_SGS_terms +   1
          ntot_SGS_comps = ntot_SGS_comps + 3
        end if
      end if
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none) then
          num_SGS_terms = num_SGS_terms +   1
          ntot_SGS_comps = ntot_SGS_comps + 6
        end if
!
        if(SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
          num_SGS_terms = num_SGS_terms +   1
          ntot_SGS_comps = ntot_SGS_comps + 6
        end if
!
        if(SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
          if(fl_prop%iflag_4_gravity) then
            num_SGS_terms = num_SGS_terms +   1
            ntot_SGS_comps = ntot_SGS_comps + 6
          end if
          if(fl_prop%iflag_4_composit_buo) then
            num_SGS_terms = num_SGS_terms +   1
            ntot_SGS_comps = ntot_SGS_comps + 6
          end if
        end if
      end if
!
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
          num_SGS_terms = num_SGS_terms +  1
          ntot_SGS_comps = ntot_SGS_comps + 3
        end if
      end if
      if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
          num_SGS_terms = num_SGS_terms +   1
          ntot_SGS_comps = ntot_SGS_comps + 3
        end if
      end if
!
      if (cp_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none) then
          num_SGS_terms = num_SGS_terms +   1
          ntot_SGS_comps = ntot_SGS_comps + 3
        end if
      end if
!
      end subroutine s_count_sgs_components
!
!  ------------------------------------------------------------------
!
      subroutine set_sgs_addresses(numnod, numele, SGS_param,           &
     &          fl_prop, cd_prop, ht_prop, cp_prop, wk_sgs, sgs_coefs)
!
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_physical_property
      use t_SGS_term_labels
!
      use m_SGS_term_labels
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
      integer(kind = kint) :: i_cmp, i_fld, id, jd, num_comp
!
!
      i_cmp = 1
      i_fld = 1
      id = 1
      jd = 1
      num_comp = 0
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none) then
          num_comp = n_vector
          sgs_coefs%Csim_SGS_hf%num_comp = num_comp
          sgs_coefs%Csim_SGS_hf%iak_Csim =   i_fld
          sgs_coefs%Csim_SGS_hf%icomp_Csim = i_cmp
          wk_sgs%name(i_fld) = SGS_heat_flux%name
          i_cmp = i_cmp + num_comp
          i_fld = i_fld + 1
        end if
      end if
      call alloc_SGS_model_coefficient(numele, num_comp,                &
     &                                 sgs_coefs%Csim_SGS_hf)
!
      num_comp = 0
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%SGS_momentum%iflag_SGS_flux                       &
     &         .ne. id_SGS_none) then
          num_comp = n_sym_tensor
          sgs_coefs%Csim_SGS_mf%num_comp = num_comp
          sgs_coefs%Csim_SGS_mf%iak_Csim =   i_fld
          sgs_coefs%Csim_SGS_mf%icomp_Csim = i_cmp
          wk_sgs%name(i_fld) = SGS_momentum_flux%name
          i_cmp = i_cmp + num_comp
          i_fld = i_fld + 1
        end if
      end if
      call alloc_SGS_model_coefficient(numele, num_comp,               &
     &                                 sgs_coefs%Csim_SGS_mf)
!
      num_comp = 0
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
          num_comp = n_sym_tensor
          sgs_coefs%Csim_SGS_lor%num_comp = num_comp
          sgs_coefs%Csim_SGS_lor%iak_Csim =   i_fld
          sgs_coefs%Csim_SGS_lor%icomp_Csim = i_cmp
          wk_sgs%name(i_fld) = SGS_maxwell_tensor%name
          i_cmp = i_cmp + num_comp
          i_fld = i_fld + 1
        end if
      end if
      call alloc_SGS_model_coefficient(numele, num_comp,                &
     &                                  sgs_coefs%Csim_SGS_lor)
!
      num_comp = 0
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
          if(fl_prop%iflag_4_gravity) then
            num_comp = n_sym_tensor
            sgs_coefs%Csim_SGS_tbuo%num_comp = num_comp
            sgs_coefs%Csim_SGS_tbuo%iak_Csim =   i_fld
            sgs_coefs%Csim_SGS_tbuo%icomp_Csim = i_cmp
            wk_sgs%name(i_fld) = SGS_buoyancy%name
          i_cmp = i_cmp + num_comp
            i_fld = i_fld + 1
          end if
        end if
      end if
      call alloc_SGS_model_coefficient(numele, num_comp,                &
     &                                  sgs_coefs%Csim_SGS_lor)
!
      num_comp = 0
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
       num_comp = 0
        if (SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
          if(fl_prop%iflag_4_composit_buo) then
            num_comp = n_sym_tensor
            sgs_coefs%Csim_SGS_cbuo%num_comp = num_comp
            sgs_coefs%Csim_SGS_cbuo%iak_Csim =   i_fld
            sgs_coefs%Csim_SGS_cbuo%icomp_Csim = i_cmp
            wk_sgs%name(i_fld) = SGS_composit_buoyancy%name
          i_cmp = i_cmp + num_comp
            i_fld = i_fld + 1
          end if
        end if
      end if
!
      num_comp = 0
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
          num_comp = n_vector
          sgs_coefs%Csim_SGS_uxb%num_comp = num_comp
          sgs_coefs%Csim_SGS_uxb%iak_Csim =   i_fld
          sgs_coefs%Csim_SGS_uxb%icomp_Csim = i_cmp
          wk_sgs%name(i_fld) = SGS_induction%name
          i_cmp = i_cmp + num_comp
          i_fld = i_fld + 1
        end if
      else if(cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
          num_comp = n_vector
          sgs_coefs%Csim_SGS_uxb%num_comp = num_comp
          sgs_coefs%Csim_SGS_uxb%iak_Csim =   i_fld
          sgs_coefs%Csim_SGS_uxb%icomp_Csim = i_cmp
          wk_sgs%name(i_fld) = SGS_induction%name
          i_cmp = i_cmp + num_comp
          i_fld = i_fld + 1
        end if
      end if
      call alloc_SGS_model_coefficient(numele, num_comp,                &
     &                                 sgs_coefs%Csim_SGS_uxb)
!
      num_comp = 0
      if (cp_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none) then
          num_comp = n_vector
          sgs_coefs%Csim_SGS_cf%num_comp = num_comp
          sgs_coefs%Csim_SGS_cf%iak_Csim =   i_fld
          sgs_coefs%Csim_SGS_cf%icomp_Csim = i_cmp
          wk_sgs%name(i_fld) = SGS_composit_flux%name
          i_cmp = i_cmp + num_comp
          i_fld = i_fld + 1
        end if
      end if
      call alloc_SGS_model_coefficient(numele, num_comp,                &
     &                                 sgs_coefs%Csim_SGS_cf)
!
      if(     SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF           &
     &   .or. SGS_param%iflag_SGS.eq.id_SGS_similarity)  then
        call alloc_SGS_model_coef_on_nod(numnod, sgs_coefs%Csim_SGS_hf)
        call alloc_SGS_model_coef_on_nod(numnod, sgs_coefs%Csim_SGS_cf)
        call alloc_SGS_model_coef_on_nod(numnod, sgs_coefs%Csim_SGS_mf)
        call alloc_SGS_model_coef_on_nod(numnod,sgs_coefs%Csim_SGS_lor)
        call alloc_SGS_model_coef_on_nod(numnod,sgs_coefs%Csim_SGS_uxb)
      end if
!
      end subroutine set_sgs_addresses
!
!  ------------------------------------------------------------------
!
      subroutine set_SGS_ele_fld_addresses(cd_prop, SGS_param,          &
     &                                     mhd_fem_wk)
!
      use t_SGS_control_parameter
      use t_physical_property
      use t_MHD_finite_element_mat
!
      type(conductive_property), intent(in) :: cd_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind = kint) :: ifil_elediff_v,  ifil_elediff_b
      integer(kind = kint) :: iphys_elediff_v, iphys_elediff_b
      integer(kind = kint) :: i
!
      ifil_elediff_v = 0
      ifil_elediff_b = 0
      iphys_elediff_v = 0
      iphys_elediff_b = 0
!
      i = 1
      if(SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        if (  SGS_param%SGS_heat%iflag_SGS_flux .ne.   id_SGS_none      &
     &   .or. SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none    &
     &   .or. SGS_param%SGS_light%iflag_SGS_flux .ne.   id_SGS_none     &
     &   .or. SGS_param%iflag_SGS_uxb .ne. id_SGS_none ) then
         iphys_elediff_v = i
         ifil_elediff_v = i + 9
         i = i + 18
        end if
!
        if ( SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
         iphys_elediff_b = i
         ifil_elediff_b = i + 9
         i = i + 18
        else if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none               &
     &     .and. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
         iphys_elediff_b = i
         ifil_elediff_b = i + 9
         i = i + 18
        end if
!
      else if (SGS_param%iflag_SGS .ne. id_SGS_none                     &
     &   .and. SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) then
        if (  SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none        &
     &   .or. SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none    &
     &   .or. SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none       &
     &   .or. SGS_param%iflag_SGS_uxb .ne.    id_SGS_none) then
         iphys_elediff_v = i
         i = i + 9
        end if
!
        if ( SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
         iphys_elediff_b = i
         i = i + 9
        else if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none               &
     &     .and. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
         iphys_elediff_b = i
         i = i + 9
        end if
      end if
!
      mhd_fem_wk%ifil_elediff_v =  ifil_elediff_v
      mhd_fem_wk%ifil_elediff_b =  ifil_elediff_b
      mhd_fem_wk%iphys_elediff_v = iphys_elediff_v
      mhd_fem_wk%iphys_elediff_b = iphys_elediff_b
!
      end subroutine set_SGS_ele_fld_addresses
!
!  ------------------------------------------------------------------
!  ------------------------------------------------------------------
!
      subroutine check_sgs_addresses(wk_sgs, sgs_coefs)
!
      use calypso_mpi
!
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_SGS_term_labels
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'num_sgs_kinds', wk_sgs%num_kinds
        write(*,*) 'num_sgs_coefs', wk_sgs%ntot_comp
!
        if(sgs_coefs%Csim_SGS_hf%iak_Csim .gt. 0) then
          write(*,*) 'iak_sgs_hf', sgs_coefs%Csim_SGS_hf%iak_Csim,      &
     &                             sgs_coefs%Csim_SGS_hf%icomp_Csim,    &
     &                             sgs_coefs%Csim_SGS_hf%num_comp,      &
     &         trim(wk_sgs%name(sgs_coefs%Csim_SGS_hf%iak_Csim))
        end if
        if(sgs_coefs%Csim_SGS_cf%iak_Csim .gt. 0) then
          write(*,*) 'iak_sgs_cf', sgs_coefs%Csim_SGS_cf%iak_Csim,      &
     &                             sgs_coefs%Csim_SGS_cf%icomp_Csim,    &
     &                             sgs_coefs%Csim_SGS_cf%num_comp,      &
     &         trim(wk_sgs%name(sgs_coefs%Csim_SGS_cf%iak_Csim))
        end if
        if(sgs_coefs%Csim_SGS_mf%iak_Csim .gt. 0) then
          write(*,*) 'iak_sgs_mf', sgs_coefs%Csim_SGS_mf%iak_Csim,      &
     &                             sgs_coefs%Csim_SGS_mf%icomp_Csim,    &
     &                             sgs_coefs%Csim_SGS_mf%num_comp,      &
     &         trim(wk_sgs%name(sgs_coefs%Csim_SGS_mf%iak_Csim))
        end if
        if(sgs_coefs%Csim_SGS_lor%iak_Csim .gt. 0) then
          write(*,*) 'iak_sgs_lor', sgs_coefs%Csim_SGS_lor%iak_Csim,    &
     &                             sgs_coefs%Csim_SGS_lor%icomp_Csim,   &
     &                             sgs_coefs%Csim_SGS_lor%num_comp,     &
     &            trim(wk_sgs%name(sgs_coefs%Csim_SGS_lor%iak_Csim))
        end if
        if(sgs_coefs%Csim_SGS_tbuo%iak_Csim .gt. 0) then
          write(*,*) 'iak_sgs_tbuo', sgs_coefs%Csim_SGS_tbuo%iak_Csim,  &
     &                              sgs_coefs%Csim_SGS_tbuo%icomp_Csim, &
     &                              sgs_coefs%Csim_SGS_tbuo%num_comp,   &
     &             trim(wk_sgs%name(sgs_coefs%Csim_SGS_tbuo%iak_Csim))
        end if
        if(sgs_coefs%Csim_SGS_cbuo%iak_Csim .gt. 0) then
          write(*,*) 'iak_sgs_cbuo', sgs_coefs%Csim_SGS_cbuo%iak_Csim,  &
     &                              sgs_coefs%Csim_SGS_cbuo%icomp_Csim, &
     &                              sgs_coefs%Csim_SGS_cbuo%num_comp,   &
     &             trim(wk_sgs%name(sgs_coefs%Csim_SGS_cbuo%iak_Csim))
        end if
        if(sgs_coefs%Csim_SGS_uxb%iak_Csim .gt. 0) then
          write(*,*) 'iak_sgs_uxb', sgs_coefs%Csim_SGS_uxb%iak_Csim,    &
     &                              sgs_coefs%Csim_SGS_uxb%icomp_Csim,  &
     &                              sgs_coefs%Csim_SGS_uxb%num_comp,    &
     &             trim(wk_sgs%name(sgs_coefs%Csim_SGS_uxb%iak_Csim))
        end if
      end if
!
      end subroutine check_sgs_addresses
!
! -------------------------------------------------------------------
!
      end module count_sgs_components
      
