!>@file   update_with_scalars.f90
!!        module update_with_scalars
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine update_with_temperature(i_step, dt,                  &
!!     &          FEM_prm, SGS_par, mesh, group, fluid, sf_bcs,         &
!!     &          iphys_base, iphys_fil, iphys_wfl,                     &
!!     &          iphys_fil_frc, iphys_fefx, iphys_SGS_wk,              &
!!     &          iphys_ele_base, ele_fld, fem_int, FEM_filters,        &
!!     &          iak_diff_base, icomp_diff_base, mk_MHD, FEM_SGS_wk,   &
!!     &          rhs_mat, nod_fld, diff_coefs, v_sol, SR_sig, SR_r)
!!      subroutine update_with_dummy_scalar(i_step, dt,                 &
!!     &          FEM_prm, SGS_par, mesh, group, fluid, sf_bcs,         &
!!     &          iphys_base, iphys_fil, iphys_wfl, iphys_SGS_wk,       &
!!     &          iphys_ele_base, ele_fld, fem_int, FEM_filters,        &
!!     &          iak_diff_base, icomp_diff_base, mk_MHD, FEM_SGS_wk,   &
!!     &          rhs_mat, nod_fld, diff_coefs, v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(scaler_surf_bc_type), intent(in) :: sf_bcs
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_field_address), intent(in) :: iphys_fil
!!        type(base_field_address), intent(in) :: iphys_wfl
!!        type(base_force_address), intent(in) :: iphys_fil_frc
!!        type(energy_flux_address), intent(in) :: iphys_fefx
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(base_field_address), intent(in) :: iak_diff_base
!!        type(base_field_address), intent(in) :: icomp_diff_base
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
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
      use t_base_field_labels
      use t_base_force_labels
      use t_energy_flux_labels
      use t_SGS_model_coef_labels
      use t_jacobians
      use t_table_FEM_const
      use t_MHD_mass_matrices
      use t_FEM_MHD_filter_data
      use t_material_property
      use t_ele_info_4_dynamic
      use t_surface_bc_scalar
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
      use t_vector_for_solver
      use t_solver_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine update_with_temperature(i_step, dt,                    &
     &          FEM_prm, SGS_par, mesh, group, fluid, sf_bcs,           &
     &          iphys_base, iphys_fil, iphys_wfl,                       &
     &          iphys_fil_frc, iphys_fefx, iphys_SGS_wk,                &
     &          iphys_ele_base, ele_fld, fem_int, FEM_filters,          &
     &          iak_diff_base, icomp_diff_base, mk_MHD, FEM_SGS_wk,     &
     &          rhs_mat, nod_fld, diff_coefs, v_sol, SR_sig, SR_r)
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
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(field_geometry_data), intent(in) :: fluid
      type(scaler_surf_bc_type), intent(in) :: sf_bcs
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(base_field_address), intent(in) :: iphys_wfl
      type(base_force_address), intent(in) :: iphys_fil_frc
      type(energy_flux_address), intent(in) :: iphys_fefx
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(base_field_address), intent(in) :: iak_diff_base
      type(base_field_address), intent(in) :: icomp_diff_base
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer (kind = kint) :: iflag2
      logical :: iflag_dmc
!
!
      iflag_dmc = dynamic_SGS_flag(i_step, SGS_par)
      if (iphys_SGS_wk%i_sgs_temp .gt. 0) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &      write(*,*) 'iflag_SGS_parterbuation',                       &
     &                  SGS_par%model_p%iflag_parterbuation
        if(SGS_par%model_p%iflag_parterbuation .eq. id_SGS_REFERENCE)   &
     &   then
          call copy_scalar_component(nod_fld,                           &
     &        iphys_base%i_per_temp, iphys_SGS_wk%i_sgs_temp)
        else
          call copy_scalar_component(nod_fld,                           &
     &        iphys_base%i_temp, iphys_SGS_wk%i_sgs_temp)
        end if
      end if
!
      if(iflag_debug .ge. iflag_routine_msg) write(*,*)                 &
     &            'filter_fld%i_temp', iphys_fil%i_temp
      if(iflag_debug .ge. iflag_routine_msg) write(*,*)                 &
                  'iflag_SGS_heat', SGS_par%model_p%iflag_SGS_h_flux
      if (iphys_fil%i_temp .gt. 0) then
        if(SGS_par%model_p%iflag_SGS_h_flux .ne. id_SGS_none) then
!
          if(SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF      &
     &         .and. iflag_dmc) then
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
     &          iphys_fil%i_temp, iphys_SGS_wk%i_sgs_temp,              &
     &          FEM_SGS_wk%wk_filter, nod_fld, v_sol, SR_sig, SR_r)
            nod_fld%iflag_update(iphys_fil%i_temp) = 1
          end if
!
          if(iphys_wfl%i_temp.ne.0 .and. iflag_dmc) then
            if (iflag_debug.gt.0) write(*,*) 'cal_w_filtered_scalar',   &
     &                           iphys_wfl%i_temp
            call cal_filtered_scalar_whole(SGS_par%filter_p,            &
     &          mesh%nod_comm, mesh%node, FEM_filters%wide_filtering,   &
     &          iphys_wfl%i_temp, iphys_fil%i_temp,                     &
     &          FEM_SGS_wk%wk_filter, nod_fld, v_sol, SR_sig, SR_r)
          end if
        end if
!
        if(iphys_fefx%i_buo_gen .gt. 0) then
          if (iflag_debug.gt.0) write(*,*) 'filter temp for buoyancy'
          call cal_filtered_scalar_whole(SGS_par%filter_p,              &
     &        mesh%nod_comm, mesh%node, FEM_filters%filtering,          &
     &        iphys_fil%i_temp, iphys_base%i_temp,                      &
     &        FEM_SGS_wk%wk_filter, nod_fld, v_sol, SR_sig, SR_r)
          nod_fld%iflag_update(iphys_fil%i_temp) = 1
        end if
!
        if((iphys_fil_frc%i_comp_buo + iphys_fefx%i_c_buo_gen)          &
     &                                                    .gt. 0) then
          if (iflag_debug.gt.0) write(*,*) 'filter temp for buoyancy'
          call cal_filtered_scalar_whole(SGS_par%filter_p,              &
     &        mesh%nod_comm, mesh%node, FEM_filters%filtering,          &
     &        iphys_fil%i_light, iphys_base%i_light,                    &
     &        FEM_SGS_wk%wk_filter, nod_fld, v_sol, SR_sig, SR_r)
          nod_fld%iflag_update(iphys_fil%i_light) = 1
        end if
      end if
!
      if(SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF          &
     &     .and. iflag_dmc) then
         if(SGS_par%commute_p%iflag_c_temp .eq. id_SGS_commute_ON) then
           if(diff_coefs%iflag_field(iak_diff_base%i_temp) .eq. 0) then
!
             if(SGS_par%model_p%iflag_SGS_h_flux .eq. id_SGS_NL_grad)   &
     &        then
               if (iflag_debug.gt.0)  write(*,*)                        &
     &                        's_cal_diff_coef_scalar temperature'
               call s_cal_diff_coef_scalar                              &
     &           (FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int,    &
     &            dt, iphys_SGS_wk%i_sgs_temp, iphys_fil%i_temp,        &
     &            iak_diff_base%i_temp, icomp_diff_base%i_temp,         &
     &            SGS_par, mesh%nod_comm, mesh%node, mesh%ele,          &
     &            mesh%surf, group%surf_grp, sf_bcs, iphys_SGS_wk,      &
     &            iphys_ele_base, ele_fld, fluid,                       &
     &            FEM_filters%layer_tbl, fem_int%jcs, fem_int%rhs_tbl,  &
     &            FEM_filters%FEM_elens, FEM_filters%filtering,         &
     &            mk_MHD%mlump_fl, FEM_SGS_wk%wk_filter,                &
     &            FEM_SGS_wk%wk_cor, FEM_SGS_wk%wk_lsq,                 &
     &            FEM_SGS_wk%wk_diff, rhs_mat%fem_wk, rhs_mat%surf_wk,  &
     &            rhs_mat%f_l, rhs_mat%f_nl, nod_fld,                   &
     &            diff_coefs, v_sol, SR_sig, SR_r)
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
      subroutine update_with_dummy_scalar(i_step, dt,                   &
     &          FEM_prm, SGS_par, mesh, group, fluid, sf_bcs,           &
     &          iphys_base, iphys_fil, iphys_wfl, iphys_SGS_wk,         &
     &          iphys_ele_base, ele_fld, fem_int, FEM_filters,          &
     &          iak_diff_base, icomp_diff_base, mk_MHD, FEM_SGS_wk,     &
     &          rhs_mat, nod_fld, diff_coefs, v_sol, SR_sig, SR_r)
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
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(scaler_surf_bc_type), intent(in) :: sf_bcs
      type(field_geometry_data), intent(in) :: fluid
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(base_field_address), intent(in) :: iphys_wfl
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(base_field_address), intent(in) :: iak_diff_base
      type(base_field_address), intent(in) :: icomp_diff_base
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer (kind = kint) :: iflag2
      logical :: iflag_dmc
!
!
      iflag_dmc = dynamic_SGS_flag(i_step, SGS_par)
!
      if (iphys_SGS_wk%i_sgs_composit .ne. 0) then
        if(SGS_par%model_p%iflag_parterbuation .eq. id_SGS_REFERENCE)   &
     &   then
          call copy_scalar_component(nod_fld,                           &
     &        iphys_base%i_per_light, iphys_SGS_wk%i_sgs_composit)
        else
          call copy_scalar_component(nod_fld,                           &
     &        iphys_base%i_light, iphys_SGS_wk%i_sgs_composit)
        end if
      end if
!
      iflag2 = 0
!
      if(SGS_par%model_p%iflag_SGS_c_flux .ne. id_SGS_none              &
     &       .and. iphys_fil%i_light .ne. 0) then
        if (iflag2.eq.1) then
          if (iflag_debug.gt.0)   write(*,*) 'cal_filtered_composition'
          call cal_filtered_scalar_whole(SGS_par%filter_p,              &
     &        mesh%nod_comm, mesh%node, FEM_filters%filtering,          &
     &        iphys_fil%i_light, iphys_SGS_wk%i_sgs_composit,           &
     &        FEM_SGS_wk%wk_filter, nod_fld, v_sol, SR_sig, SR_r)
          nod_fld%iflag_update(iphys_fil%i_light) = 1
        end if
!
        if (iphys_wfl%i_light.ne.0 .and. iflag_dmc) then
          if (iflag_debug.gt.0) write(*,*) 'cal_w_filtered_scalar',     &
     &                         iphys_wfl%i_temp
          call cal_filtered_scalar_whole (SGS_par%filter_p,             &
     &        mesh%nod_comm, mesh%node, FEM_filters%wide_filtering,     &
     &        iphys_wfl%i_light, iphys_fil%i_light,                     &
     &        FEM_SGS_wk%wk_filter, nod_fld, v_sol, SR_sig, SR_r)
        end if
      end if
!
!
       if(SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF         &
     &   .and. iflag_dmc) then
         if(SGS_par%commute_p%iflag_c_light .eq. id_SGS_commute_ON)     &
     &    then
           if(diff_coefs%iflag_field(iak_diff_base%i_light).eq. 0) then
!
             if(SGS_par%model_p%iflag_SGS_c_flux .eq. id_SGS_NL_grad)   &
     &        then
               if (iflag_debug.gt.0)  write(*,*)                        &
     &                        's_cal_diff_coef_scalar composition'
               call s_cal_diff_coef_scalar                              &
     &           (FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int,    &
     &            dt, iphys_SGS_wk%i_sgs_composit, iphys_fil%i_light,   &
     &            iak_diff_base%i_light, icomp_diff_base%i_light,       &
     &            SGS_par, mesh%nod_comm, mesh%node, mesh%ele,          &
     &            mesh%surf, group%surf_grp, sf_bcs, iphys_SGS_wk,      &
     &            iphys_ele_base, ele_fld, fluid,                       &
     &            FEM_filters%layer_tbl, fem_int%jcs, fem_int%rhs_tbl,  &
     &            FEM_filters%FEM_elens, FEM_filters%filtering,         &
     &            mk_MHD%mlump_fl, FEM_SGS_wk%wk_filter,                &
     &            FEM_SGS_wk%wk_cor, FEM_SGS_wk%wk_lsq,                 &
     &            FEM_SGS_wk%wk_diff, rhs_mat%fem_wk, rhs_mat%surf_wk,  &
     &            rhs_mat%f_l, rhs_mat%f_nl, nod_fld,                   &
     &            diff_coefs, v_sol, SR_sig, SR_r)
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
