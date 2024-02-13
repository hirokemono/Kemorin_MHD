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
!!     &           iak_sgs_term, icomp_sgs_term, wk_sgs, sgs_coefs)
!!      subroutine s_count_sgs_components(SGS_param,                    &
!!     &          fl_prop, cd_prop, ht_prop, cp_prop, sgs_coefs)
!!      subroutine set_SGS_ele_fld_addresses(cd_prop, SGS_param,        &
!!     &          iphys_elediff_vec, iphys_elediff_fil)
!!      subroutine check_sgs_addresses                                  &
!!     &         (iak_sgs_term, icomp_sgs_term, wk_sgs, sgs_coefs)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(SGS_term_address), intent(inout) :: iak_sgs_term
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
!!        type(base_field_address), intent(inout) :: iphys_elediff_vec
!!        type(base_field_address), intent(inout) :: iphys_elediff_fil
!!        type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!
      module count_sgs_components
!
      use m_precision
      use m_machine_parameter
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
      use t_FEM_SGS_model_coefs
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(layering_tbl), intent(in) :: layer_tbl
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!
!
      call s_count_sgs_components(SGS_param,                            &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, Csims_FEM_MHD%sgs_coefs)
!
!   set index for model coefficients
!
      call alloc_sgs_coefs_layer(layer_tbl%e_grp%num_grp,               &
     &    Csims_FEM_MHD%sgs_coefs%num_field,                            &
     &    Csims_FEM_MHD%sgs_coefs%ntot_comp, wk_sgs)
!
      call alloc_SGS_num_coefs(Csims_FEM_MHD%sgs_coefs)
      call alloc_SGS_coefs(numele, Csims_FEM_MHD%sgs_coefs)
!
      call set_sgs_addresses(SGS_param,                                 &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    Csims_FEM_MHD%iak_sgs_term, Csims_FEM_MHD%icomp_sgs_term,     &
     &    wk_sgs, Csims_FEM_MHD%sgs_coefs)
      call check_sgs_addresses                                          &
     &   (Csims_FEM_MHD%iak_sgs_term, Csims_FEM_MHD%icomp_sgs_term,     &
     &    wk_sgs, Csims_FEM_MHD%sgs_coefs)
!
      if(     SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF           &
     &   .or. SGS_param%iflag_SGS.eq.id_SGS_similarity)  then
        call copy_SGS_num_coefs                                         &
     &     (Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod)
        call alloc_SGS_coefs(numnod, Csims_FEM_MHD%sgs_coefs_nod)
      end if
!
      end subroutine define_sgs_components
!
!  ------------------------------------------------------------------
!
      subroutine s_count_sgs_components(SGS_param,                      &
     &          fl_prop, cd_prop, ht_prop, cp_prop, sgs_coefs)
!
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_physical_property
      use t_physical_property
      use t_SGS_model_coefs
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
!    count coefficients for SGS terms
!
      sgs_coefs%num_field = 0
      sgs_coefs%ntot_comp = 0
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 3
        end if
      end if
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_m_flux .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 6
        end if
!
        if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 6
        end if
!
        if (SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
          if(fl_prop%iflag_4_gravity) then
            sgs_coefs%num_field = sgs_coefs%num_field + 1
            sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 6
          end if
          if(fl_prop%iflag_4_composit_buo) then
            sgs_coefs%num_field = sgs_coefs%num_field + 1
            sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 6
          end if
        end if
      end if
!
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 3
        end if
      end if
      if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 3
        end if
      end if
!
      if (cp_prop%iflag_scheme .gt. id_no_evolution) then
        if (SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none) then
          sgs_coefs%num_field = sgs_coefs%num_field + 1
          sgs_coefs%ntot_comp = sgs_coefs%ntot_comp + 3
        end if
      end if
!
      end subroutine s_count_sgs_components
!
!  ------------------------------------------------------------------
!
      subroutine set_sgs_addresses                                      &
     &          (SGS_param, fl_prop, cd_prop, ht_prop, cp_prop,         &
     &           iak_sgs_term, icomp_sgs_term, wk_sgs, sgs_coefs)
!
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_physical_property
      use t_SGS_model_coefs
      use t_SGS_term_labels
!
      use m_SGS_term_labels
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
!
      type(SGS_term_address), intent(inout) :: iak_sgs_term
      type(SGS_term_address), intent(inout) :: icomp_sgs_term
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
      integer(kind = kint) :: i_cmp, i_fld, id, jd
!
!
       i_cmp = 1
       i_fld = 1
       id = 1
       jd = 1
       if (ht_prop%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none) then
           icomp_sgs_term%i_SGS_h_flux = i_cmp
           iak_sgs_term%i_SGS_h_flux =  i_fld
           wk_sgs%name(i_fld) = SGS_heat_flux%name
           sgs_coefs%num_comps(i_fld) = 3
           i_cmp = i_cmp + sgs_coefs%num_comps(i_fld)
           i_fld = i_fld + 1
         end if
       end if
!
       if (fl_prop%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%iflag_SGS_m_flux .ne. id_SGS_none) then
           icomp_sgs_term%i_SGS_m_flux = i_cmp
           iak_sgs_term%i_SGS_m_flux =  i_fld
           wk_sgs%name(i_fld) = SGS_momentum_flux%name
           sgs_coefs%num_comps(i_fld) = 6
           i_cmp = i_cmp + sgs_coefs%num_comps(i_fld)
           i_fld = i_fld + 1
         end if
!
         if (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
           icomp_sgs_term%i_SGS_Lorentz = i_cmp
           iak_sgs_term%i_SGS_Lorentz =  i_fld
           wk_sgs%name(i_fld) = SGS_maxwell_tensor%name
           sgs_coefs%num_comps(i_fld) = 6
           i_cmp = i_cmp + sgs_coefs%num_comps(i_fld)
           i_fld = i_fld + 1
         end if
!
        if (SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
          if(fl_prop%iflag_4_gravity) then
            icomp_sgs_term%i_SGS_buoyancy = i_cmp
            iak_sgs_term%i_SGS_buoyancy =  i_fld
            wk_sgs%name(i_fld) = SGS_buoyancy%name
            sgs_coefs%num_comps(i_fld) = 6
            i_cmp = i_cmp + sgs_coefs%num_comps(i_fld)
            i_fld = i_fld + 1
          end if
          if(fl_prop%iflag_4_composit_buo) then
            icomp_sgs_term%i_SGS_comp_buo = i_cmp
            iak_sgs_term%i_SGS_comp_buo =  i_fld
            wk_sgs%name(i_fld) = SGS_composit_buoyancy%name
            sgs_coefs%num_comps(i_fld) = 6
            i_cmp = i_cmp + sgs_coefs%num_comps(i_fld)
            i_fld = i_fld + 1
          end if
        end if
       end if
!
       if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
         if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
           icomp_sgs_term%i_SGS_induction = i_cmp
           iak_sgs_term%i_SGS_induction =  i_fld
           wk_sgs%name(i_fld) = SGS_induction%name
           sgs_coefs%num_comps(i_fld) = 3
           i_cmp = i_cmp + sgs_coefs%num_comps(i_fld)
           i_fld = i_fld + 1
         end if
       end if
       if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
         if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
           icomp_sgs_term%i_SGS_induction = i_cmp
           iak_sgs_term%i_SGS_induction =  i_fld
           wk_sgs%name(i_fld) = SGS_induction%name
           sgs_coefs%num_comps(i_fld) = 3
           i_cmp = i_cmp + sgs_coefs%num_comps(i_fld)
           i_fld = i_fld + 1
         end if
       end if
!
       if (cp_prop%iflag_scheme .gt. id_no_evolution) then
         if (SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none) then
           icomp_sgs_term%i_SGS_c_flux = i_cmp
           iak_sgs_term%i_SGS_c_flux =  i_fld
           wk_sgs%name(i_fld) = SGS_composit_flux%name
           sgs_coefs%num_comps(i_fld) = 3
           i_cmp = i_cmp + sgs_coefs%num_comps(i_fld)
           i_fld = i_fld + 1
         end if
       end if
!
       sgs_coefs%istack_comps(0) = 0
       do i_cmp = 1, sgs_coefs%num_field
         sgs_coefs%istack_comps(i_cmp)                                  &
     &          = sgs_coefs%istack_comps(i_cmp-1)                       &
     &           + sgs_coefs%num_comps(i_cmp)
       end do
!
      end subroutine set_sgs_addresses
!
!  ------------------------------------------------------------------
!
      subroutine set_SGS_ele_fld_addresses(cd_prop, SGS_param,          &
     &          iphys_elediff_vec, iphys_elediff_fil)
!
      use t_SGS_control_parameter
      use t_physical_property
      use t_base_field_labels
!
      type(conductive_property), intent(in) :: cd_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
!
      type(base_field_address), intent(inout) :: iphys_elediff_vec
      type(base_field_address), intent(inout) :: iphys_elediff_fil
!
      integer(kind = kint) :: i
!
      i = 1
      if(SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        if (  SGS_param%SGS_heat%iflag_SGS_flux .ne.   id_SGS_none      &
     &   .or. SGS_param%iflag_SGS_m_flux .ne.   id_SGS_none             &
     &   .or. SGS_param%SGS_light%iflag_SGS_flux .ne.   id_SGS_none     &
     &   .or. SGS_param%iflag_SGS_uxb .ne. id_SGS_none ) then
         iphys_elediff_vec%i_velo = i
         iphys_elediff_fil%i_velo = i + 9
         i = i + 18
        end if
!
        if ( SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
         iphys_elediff_vec%i_magne = i
         iphys_elediff_fil%i_magne = i + 9
         i = i + 18
        else if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none               &
     &     .and. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
         iphys_elediff_vec%i_magne = i
         iphys_elediff_fil%i_magne = i + 9
         i = i + 18
        end if
!
      else if (SGS_param%iflag_SGS .ne. id_SGS_none                     &
     &   .and. SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) then
        if (  SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none        &
     &   .or. SGS_param%iflag_SGS_m_flux .ne. id_SGS_none               &
     &   .or. SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none       &
     &   .or. SGS_param%iflag_SGS_uxb .ne.    id_SGS_none) then
         iphys_elediff_vec%i_velo = i
         i = i + 9
        end if
!
        if ( SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
         iphys_elediff_vec%i_magne = i
         i = i + 9
        else if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none               &
     &     .and. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
         iphys_elediff_vec%i_magne = i
         i = i + 9
        end if
      end if
!
      end subroutine set_SGS_ele_fld_addresses
!
!  ------------------------------------------------------------------
!  ------------------------------------------------------------------
!
      subroutine check_sgs_addresses                                    &
     &         (iak_sgs_term, icomp_sgs_term, wk_sgs, sgs_coefs)
!
      use calypso_mpi
!
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_SGS_term_labels
      use t_SGS_model_coefs
!
!
      type(SGS_term_address), intent(in) :: iak_sgs_term
      type(SGS_term_address), intent(in) :: icomp_sgs_term
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'num_sgs_kinds', sgs_coefs%num_field
        write(*,*) 'num_sgs_coefs', sgs_coefs%ntot_comp
!
        if(iak_sgs_term%i_SGS_h_flux .gt. 0) then
          write(*,*) 'iak_sgs_hf',                                      &
     &       iak_sgs_term%i_SGS_h_flux, icomp_sgs_term%i_SGS_h_flux,    &
     &       sgs_coefs%num_comps(iak_sgs_term%i_SGS_h_flux),            &
     &       trim(wk_sgs%name(iak_sgs_term%i_SGS_h_flux))
        end if
        if(iak_sgs_term%i_SGS_m_flux .gt. 0) then
          write(*,*) 'iak_sgs_mf',                                      &
     &       iak_sgs_term%i_SGS_m_flux, icomp_sgs_term%i_SGS_m_flux,    &
     &       sgs_coefs%num_comps(iak_sgs_term%i_SGS_m_flux),            &
     &       trim(wk_sgs%name(iak_sgs_term%i_SGS_m_flux))
        end if
        if(iak_sgs_term%i_SGS_Lorentz .gt. 0) then
          write(*,*) 'iak_sgs_lor',                                     &
     &       iak_sgs_term%i_SGS_Lorentz, icomp_sgs_term%i_SGS_Lorentz,  &
     &       sgs_coefs%num_comps(iak_sgs_term%i_SGS_Lorentz),           &
     &       trim(wk_sgs%name(iak_sgs_term%i_SGS_Lorentz))
        end if
        if(iak_sgs_term%i_SGS_buoyancy .gt. 0) then
          write(*,*) 'iak_sgs_tbuo',                                    &
     &      iak_sgs_term%i_SGS_buoyancy, icomp_sgs_term%i_SGS_buoyancy, &
     &      sgs_coefs%num_comps(iak_sgs_term%i_SGS_buoyancy),           &
     &      trim(wk_sgs%name(iak_sgs_term%i_SGS_buoyancy))
        end if
        if(iak_sgs_term%i_SGS_comp_buo .gt. 0) then
          write(*,*) 'iak_sgs_cbuo',                                    &
     &      iak_sgs_term%i_SGS_comp_buo, icomp_sgs_term%i_SGS_comp_buo, &
     &      sgs_coefs%num_comps(iak_sgs_term%i_SGS_comp_buo),           &
     &      trim(wk_sgs%name(iak_sgs_term%i_SGS_comp_buo))
        end if
        if(iak_sgs_term%i_SGS_induction .gt. 0) then
          write(*,*) 'iak_sgs_uxb',                                     &
     &       iak_sgs_term%i_SGS_induction,                              &
     &       icomp_sgs_term%i_SGS_induction,                            &
     &       sgs_coefs%num_comps(iak_sgs_term%i_SGS_induction),         &
     &       trim(wk_sgs%name(iak_sgs_term%i_SGS_induction))
        end if
        if(iak_sgs_term%i_SGS_c_flux .gt. 0) then
          write(*,*) 'iak_sgs_cf',                                      &
     &       iak_sgs_term%i_SGS_c_flux, icomp_sgs_term%i_SGS_c_flux,    &
     &       sgs_coefs%num_comps(iak_sgs_term%i_SGS_c_flux),            &
     &       trim(wk_sgs%name(iak_sgs_term%i_SGS_c_flux))
        end if
      end if
!
      end subroutine check_sgs_addresses
!
! -------------------------------------------------------------------
!
      end module count_sgs_components
      
