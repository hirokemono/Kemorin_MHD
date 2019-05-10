!>@file   update_with_scalars.f90
!!        module update_with_scalars
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine update_with_temperature(iak_diff_t, icomp_diff_t,    &
!!     &        i_step, dt, FEM_prm, SGS_par, mesh, group, fluid,       &
!!     &        sf_bcs, iphys, iphys_ele, ele_fld, fem_int, FEM_filters,&
!!     &        mk_MHD, FEM_SGS_wk, rhs_mat, nod_fld, diff_coefs)
!!      subroutine update_with_dummy_scalar(iak_diff_c, icomp_diff_c,   &
!!     &        i_step, dt, FEM_prm, SGS_par, mesh, group, fluid,       &
!!     &        sf_bcs, iphys, iphys_ele, ele_fld, fem_int, FEM_filters,&
!!     &        mk_MHD, FEM_SGS_wk, rhs_mat, nod_fld, diff_coefs)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(scaler_surf_bc_type), intent(in) :: sf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!@endverbatim
!
      module update_with_scalars
!
      use m_precision
!
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
      use t_surface_data
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_table_FEM_const
      use t_MHD_mass_matrices
      use t_FEM_MHD_filter_data
      use t_material_property
      use t_ele_info_4_dynamic
      use t_surface_bc_data
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine update_with_temperature(iak_diff_t, icomp_diff_t,      &
     &        i_step, dt, FEM_prm, SGS_par, mesh, group, fluid,         &
     &        sf_bcs, iphys, iphys_ele, ele_fld, fem_int, FEM_filters,  &
     &        mk_MHD, FEM_SGS_wk, rhs_mat, nod_fld, diff_coefs)
!
      use average_on_elements
      use cal_filtering_scalars
      use cal_diff_vector_on_ele
      use cal_diff_coef_temp
      use cal_filtering_scalars
      use copy_nodal_fields
!
      integer(kind=kint), intent(in) :: i_step
      real(kind=kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: iak_diff_t, icomp_diff_t
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(field_geometry_data), intent(in) :: fluid
      type(scaler_surf_bc_type), intent(in) :: sf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      integer (kind = kint) :: iflag_dmc, iflag2
!
!
      iflag_dmc = dynamic_SGS_flag(i_step, SGS_par)
!
!
      if (iphys%i_sgs_temp .gt. 0) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &      write(*,*) 'iflag_SGS_parterbuation',                       &
     &                  SGS_par%model_p%iflag_parterbuation
        if(SGS_par%model_p%iflag_parterbuation .eq. id_SGS_REFERENCE)   &
     &   then
          call copy_scalar_component(nod_fld,                           &
     &        iphys%i_par_temp, iphys%i_sgs_temp)
        else
          call copy_scalar_component(nod_fld,                           &
     &        iphys%i_temp, iphys%i_sgs_temp)
        end if
      end if
!
      if(iflag_debug .ge. iflag_routine_msg) write(*,*)                 &
     &            'i_filter_temp', iphys%i_filter_temp
      if(iflag_debug .ge. iflag_routine_msg) write(*,*)                 &
                  'iflag_SGS_heat', SGS_par%model_p%iflag_SGS_h_flux
      if (iphys%i_filter_temp .gt. 0) then
        if(SGS_par%model_p%iflag_SGS_h_flux .ne. id_SGS_none) then
!
          if(SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF      &
     &         .and. iflag_dmc .eq. 0) then
            iflag2 = 1
          else if (SGS_par%model_p%iflag_SGS .eq. id_SGS_similarity)    &
     &     then
            iflag2 = 1
          else
            iflag2 = 0
          end if
          if (iflag_debug .gt. 0)   write(*,*) 'iflag2', iflag2
!
          if (iflag2 .eq. 1) then
            if (iflag_debug.gt.0) write(*,*) 'cal_filtered_temperature'
            call cal_filtered_scalar_whole(SGS_par%filter_p,            &
     &         mesh%nod_comm, mesh%node, FEM_filters%filtering,         &
     &          iphys%i_filter_temp, iphys%i_sgs_temp,                  &
     &          FEM_SGS_wk%wk_filter, nod_fld)
            nod_fld%iflag_update(iphys%i_filter_temp) = 1
          end if
!
          if (iphys%i_wide_fil_temp.ne.0 .and. iflag_dmc .eq. 0) then
            if (iflag_debug.gt.0)                                       &
     &        write(*,*) 'cal_w_filtered_scalar', iphys%i_wide_fil_temp
            call cal_filtered_scalar_whole(SGS_par%filter_p,            &
     &          mesh%nod_comm, mesh%node, FEM_filters%wide_filtering,   &
     &          iphys%i_wide_fil_temp, iphys%i_filter_temp,             &
     &          FEM_SGS_wk%wk_filter, nod_fld)
          end if
        end if
!
        if( (iphys%i_filter_buo+iphys%i_f_buo_gen) .gt. 0) then
          if (iflag_debug.gt.0) write(*,*) 'filter temp for buoyancy'
          call cal_filtered_scalar_whole(SGS_par%filter_p,              &
     &        mesh%nod_comm, mesh%node, FEM_filters%filtering,          &
     &        iphys%i_filter_temp, iphys%i_temp, FEM_SGS_wk%wk_filter,  &
     &        nod_fld)
          nod_fld%iflag_update(iphys%i_filter_temp) = 1
        end if
      end if
!
      if(SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF          &
     &     .and. iflag_dmc .eq. 0) then
         if(SGS_par%commute_p%iflag_c_temp .eq. id_SGS_commute_ON) then
           if ( diff_coefs%iflag_field(iak_diff_t) .eq. 0) then
!
             if(SGS_par%model_p%iflag_SGS_h_flux .eq. id_SGS_NL_grad)   &
     &        then
               if (iflag_debug.gt.0)                                    &
     &            write(*,*) 's_cal_diff_coef_scalar temp'
               call s_cal_diff_coef_scalar                              &
     &            (FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int,   &
     &             dt, iphys%i_sgs_temp, iphys%i_filter_temp,           &
     &             iak_diff_t, icomp_diff_t, SGS_par,                   &
     &             mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,       &
     &             group%surf_grp, sf_bcs, iphys, iphys_ele, ele_fld,   &
     &             fluid, FEM_filters%layer_tbl,                        &
     &             fem_int%jcs, fem_int%rhs_tbl,                        &
     &             FEM_filters%FEM_elens, FEM_filters%filtering,        &
     &             mk_MHD%mlump_fl, FEM_SGS_wk%wk_filter,               &
     &             FEM_SGS_wk%wk_cor, FEM_SGS_wk%wk_lsq,                &
     &             FEM_SGS_wk%wk_diff, rhs_mat%fem_wk, rhs_mat%surf_wk, &
     &             rhs_mat%f_l, rhs_mat%f_nl, nod_fld, diff_coefs)
             end if
           end if
!
         end if
       end if
!
       end subroutine update_with_temperature
!
!-----------------------------------------------------------------------
!
      subroutine update_with_dummy_scalar(iak_diff_c, icomp_diff_c,     &
     &        i_step, dt, FEM_prm, SGS_par, mesh, group, fluid,         &
     &        sf_bcs, iphys, iphys_ele, ele_fld, fem_int, FEM_filters,  &
     &        mk_MHD, FEM_SGS_wk, rhs_mat, nod_fld, diff_coefs)
!
      use average_on_elements
      use cal_filtering_scalars
      use cal_diff_vector_on_ele
      use cal_diff_coef_temp
      use cal_filtering_scalars
      use copy_nodal_fields
!
      integer(kind=kint), intent(in) :: i_step
      real(kind=kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: iak_diff_c, icomp_diff_c
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(scaler_surf_bc_type), intent(in) :: sf_bcs
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      integer (kind = kint) :: iflag_dmc, iflag2
!
!
      iflag_dmc = dynamic_SGS_flag(i_step, SGS_par)
!
      if (iphys%i_sgs_composit .ne. 0) then
        if(SGS_par%model_p%iflag_parterbuation .eq. id_SGS_REFERENCE)   &
     &   then
          call copy_scalar_component(nod_fld,                           &
     &        iphys%i_par_light, iphys%i_sgs_composit)
        else
          call copy_scalar_component(nod_fld,                           &
     &        iphys%i_light, iphys%i_sgs_composit)
        end if
      end if
!
      iflag2 = 0
!
      if(SGS_par%model_p%iflag_SGS_c_flux .ne. id_SGS_none              &
     &       .and. iphys%i_filter_comp .ne. 0) then
        if (iflag2.eq.1) then
          if (iflag_debug.gt.0)   write(*,*) 'cal_filtered_composition'
          call cal_filtered_scalar_whole(SGS_par%filter_p,              &
     &        mesh%nod_comm, mesh%node, FEM_filters%filtering,          &
     &        iphys%i_filter_comp, iphys%i_sgs_composit,                &
     &        FEM_SGS_wk%wk_filter, nod_fld)
          nod_fld%iflag_update(iphys%i_filter_comp) = 1
        end if
!
        if (iphys%i_wide_fil_temp.ne.0 .and. iflag_dmc .eq. 0) then
          if (iflag_debug.gt.0)                                         &
     &      write(*,*) 'cal_w_filtered_scalar', iphys%i_wide_fil_temp
          call cal_filtered_scalar_whole (SGS_par%filter_p,             &
     &        mesh%nod_comm, mesh%node, FEM_filters%wide_filtering,     &
     &        iphys%i_wide_fil_temp, iphys%i_filter_comp,               &
     &        FEM_SGS_wk%wk_filter, nod_fld)
        end if
      end if
!
!
       if(SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF         &
     &   .and. iflag_dmc .eq. 0) then
         if(SGS_par%commute_p%iflag_c_light .eq. id_SGS_commute_ON)     &
     &    then
           if ( diff_coefs%iflag_field(iak_diff_c) .eq. 0) then
!
             if(SGS_par%model_p%iflag_SGS_c_flux .eq. id_SGS_NL_grad)   &
     &        then
               if (iflag_debug.gt.0)  write(*,*)                        &
     &                        's_cal_diff_coef_scalar composition'
               call s_cal_diff_coef_scalar                              &
     &            (FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int,   &
     &             dt, iphys%i_sgs_composit, iphys%i_filter_comp,       &
     &             iak_diff_c, icomp_diff_c, SGS_par,                   &
     &             mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,       &
     &             group%surf_grp, sf_bcs, iphys, iphys_ele, ele_fld,   &
     &             fluid, FEM_filters%layer_tbl,                        &
     &             fem_int%jcs, fem_int%rhs_tbl,                        &
     &             FEM_filters%FEM_elens, FEM_filters%filtering,        &
     &             mk_MHD%mlump_fl, FEM_SGS_wk%wk_filter,               &
     &             FEM_SGS_wk%wk_cor, FEM_SGS_wk%wk_lsq,                &
     &             FEM_SGS_wk%wk_diff, rhs_mat%fem_wk, rhs_mat%surf_wk, &
     &             rhs_mat%f_l, rhs_mat%f_nl, nod_fld, diff_coefs)
             end if

           end if
         end if
       end if
!
       end subroutine update_with_dummy_scalar
!
!-----------------------------------------------------------------------
!
      end module update_with_scalars
