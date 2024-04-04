!>@file   update_with_scalars.f90
!!        module update_with_scalars
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine update_with_scalar                                   &
!!     &         (i_step, dt, i_scalar, i_pert, i_filter_s,             &
!!     &          i_SGS_wk_field, iphys_wfl_scalar, iphys_fefx_buo_gen, &
!!     &          iflag_supg, n_int_evo, iflag_SGS_flux,                &
!!     &          iflag_commute_field, iflag_SGS_initial,               &
!!     &          i_step_sgs_coefs, SGS_param, cmt_param, filter_param, &
!!     &          mesh, group, fluid, sf_bcs, iphys_SGS_wk,             &
!!     &          iphys_ele_base, ele_fld, fem_int, FEM_filters,        &
!!     &          mk_MHD, FEM_SGS_wk, rhs_mat, nod_fld, Cdiff_scalar,   &
!!     &          v_sol, SR_sig, SR_r)
!!        integer(kind=kint), intent(in) :: i_step
!!        real(kind=kreal), intent(in) :: dt
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(scaler_surf_bc_type), intent(in) :: sf_bcs
!!        integer(kind = kint), intent(in) :: iflag_SGS_initial
!!        integer(kind = kint), intent(in) :: i_step_sgs_coefs
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(SGS_filtering_params), intent(in) :: filter_param
!!        integer(kind = kint), intent(in) :: iflag_supg
!!        integer(kind = kint), intent(in) :: n_int_evo
!!        integer(kind = kint), intent(in) :: iflag_SGS_flux
!!        integer(kind = kint), intent(in) :: iflag_commute_field
!!        integer(kind = kint), intent(in) :: i_scalar
!!        integer(kind = kint), intent(in) :: i_pert
!!        integer(kind = kint), intent(in) :: i_filter_s
!!        integer(kind = kint), intent(in) :: i_SGS_wk_field
!!        integer(kind = kint), intent(in) :: iphys_wfl_scalar
!!        integer(kind = kint), intent(in) :: iphys_fefx_buo_gen
!!        integer(kind = kint), intent(in) :: icomp_diff_t
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_model_coefficient), intent(inout) :: Cdiff_scalar
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
      use t_FEM_SGS_model_coefs
      use t_mesh_data
      use t_geometry_data_MHD
      use t_surface_data
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
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
      subroutine update_with_scalar                                     &
     &         (i_step, dt, i_scalar, i_pert, i_filter_s,               &
     &          i_SGS_wk_field, iphys_wfl_scalar, iphys_fefx_buo_gen,   &
     &          iflag_supg, n_int_evo, iflag_SGS_flux,                  &
     &          iflag_commute_field, iflag_SGS_initial,                 &
     &          i_step_sgs_coefs, SGS_param, cmt_param, filter_param,   &
     &          mesh, group, fluid, sf_bcs, iphys_SGS_wk,               &
     &          iphys_ele_base, ele_fld, fem_int, FEM_filters,          &
     &          mk_MHD, FEM_SGS_wk, rhs_mat, nod_fld, Cdiff_scalar,     &
     &          v_sol, SR_sig, SR_r)
!
      use average_on_elements
      use cal_filtering_scalars
      use cal_diff_vector_on_ele
      use cal_diff_coef_temp
      use cal_filtering_scalars
      use copy_nodal_fields
      use t_IO_step_parameter
!
      integer(kind=kint), intent(in) :: i_step
      real(kind=kreal), intent(in) :: dt
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(field_geometry_data), intent(in) :: fluid
      type(scaler_surf_bc_type), intent(in) :: sf_bcs
!
      integer(kind = kint), intent(in) :: iflag_SGS_initial
      integer(kind = kint), intent(in) :: i_step_sgs_coefs
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(SGS_filtering_params), intent(in) :: filter_param
!
      integer(kind = kint), intent(in) :: iflag_supg
      integer(kind = kint), intent(in) :: n_int_evo
      integer(kind = kint), intent(in) :: iflag_SGS_flux
      integer(kind = kint), intent(in) :: iflag_commute_field
      integer(kind = kint), intent(in) :: i_scalar
      integer(kind = kint), intent(in) :: i_pert
      integer(kind = kint), intent(in) :: i_filter_s
      integer(kind = kint), intent(in) :: i_SGS_wk_field
      integer(kind = kint), intent(in) :: iphys_wfl_scalar
      integer(kind = kint), intent(in) :: iphys_fefx_buo_gen
!
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_model_coefficient), intent(inout) :: Cdiff_scalar
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer (kind = kint) :: iflag2
      logical :: iflag_dmc
!
!
      iflag_dmc = output_flag(i_step, i_step_sgs_coefs)
      if(i_SGS_wk_field .gt. 0) then
        if(SGS_param%iflag_parterbuation .eq. id_SGS_REFERENCE) then
          call copy_scalar_component(nod_fld,                           &
     &        i_pert, i_SGS_wk_field)
        else
          call copy_scalar_component(nod_fld,                           &
     &        i_scalar, i_SGS_wk_field)
        end if
      end if
!
      if (i_filter_s .gt. 0) then
        if(iflag_SGS_flux .ne. id_SGS_none) then
          if(SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF            &
     &         .and. iflag_dmc) then
            iflag2 = 1
          else if (SGS_param%iflag_SGS .eq. id_SGS_similarity)          &
     &     then
            iflag2 = 1
          else
            iflag2 = 0
          end if
!
          if(iflag2 .eq. 1) then
            if(iflag_debug.gt.0) write(*,*) 'filtered scalar'
            call cal_filtered_scalar_whole(filter_param,                &
     &          mesh%nod_comm, mesh%node, FEM_filters%filtering,        &
     &          i_filter_s, i_SGS_wk_field,                             &
     &          FEM_SGS_wk%wk_filter, nod_fld, v_sol, SR_sig, SR_r)
            nod_fld%iflag_update(i_filter_s) = 1
          end if
!
          if(iphys_wfl_scalar.ne.0 .and. iflag_dmc) then
            if(iflag_debug.gt.0) write(*,*) 'cal_w_filtered_scalar',    &
     &                           iphys_wfl_scalar
            call cal_filtered_scalar_whole(filter_param,                &
     &          mesh%nod_comm, mesh%node, FEM_filters%wide_filtering,   &
     &          iphys_wfl_scalar, i_filter_s,                           &
     &          FEM_SGS_wk%wk_filter, nod_fld, v_sol, SR_sig, SR_r)
            nod_fld%iflag_update(iphys_wfl_scalar) = 1
          end if
        end if
!
        if(iphys_fefx_buo_gen .gt. 0) then
          if(iflag_debug.gt.0) write(*,*) 'filter temp for buoyancy'
          call cal_filtered_scalar_whole(filter_param,                  &
     &        mesh%nod_comm, mesh%node, FEM_filters%filtering,          &
     &        i_filter_s, i_scalar,                      &
     &        FEM_SGS_wk%wk_filter, nod_fld, v_sol, SR_sig, SR_r)
          nod_fld%iflag_update(i_filter_s) = 1
        end if
      end if
!
      if(SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF                &
     &     .and. iflag_dmc) then
         if(iflag_commute_field .eq. id_SGS_commute_ON) then
           if(Cdiff_scalar%flag_set .eqv. .FALSE.) then
!
             if(iflag_SGS_flux .eq. id_SGS_NL_grad) then
               if (iflag_debug.gt.0)  write(*,*)                        &
     &                        's_cal_diff_coef_scalar'
               call s_cal_diff_coef_scalar                              &
     &           (iflag_SGS_initial, iflag_supg, n_int_evo, dt,         &
     &            i_SGS_wk_field, i_filter_s,                           &
     &            SGS_param, cmt_param, filter_param,                   &
     &            mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,        &
     &            group%surf_grp, sf_bcs, iphys_SGS_wk,                 &
     &            iphys_ele_base, ele_fld, fluid,                       &
     &            FEM_filters%layer_tbl, fem_int%jcs, fem_int%rhs_tbl,  &
     &            FEM_filters%FEM_elens, FEM_filters%filtering,         &
     &            mk_MHD%mlump_fl, FEM_SGS_wk%wk_filter,                &
     &            FEM_SGS_wk%wk_cor, FEM_SGS_wk%wk_lsq,                 &
     &            FEM_SGS_wk%wk_diff, rhs_mat%fem_wk, rhs_mat%surf_wk,  &
     &            rhs_mat%f_l, rhs_mat%f_nl, nod_fld,                   &
     &            Cdiff_scalar, v_sol, SR_sig, SR_r)
             end if
           end if
!
         end if
       end if
!
       end subroutine update_with_scalar
!
!-----------------------------------------------------------------------
!
      end module update_with_scalars
