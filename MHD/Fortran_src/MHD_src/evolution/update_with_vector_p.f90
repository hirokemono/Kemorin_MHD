!>@file   update_with_vector_p.f90
!!        module update_with_vector_p
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine update_with_vector_potential                         &
!!     &         (iak_diff_b, icomp_diff_b, ie_dbx, ie_dfbx, i_step, dt,&
!!     &          FEM_prm, SGS_par, mesh, group, surf, fluid, conduct,  &
!!     &          layer_tbl, Bnod_bcs, Asf_bcs, Fsf_bcs,                &
!!     &          iphys, iphys_ele, jacobians, rhs_tbl, FEM_elen,       &
!!     &          filtering, wide_filtering, m_lump, wk_cor, wk_lsq,    &
!!     &          wk_diff, wk_filter, mhd_fem_wk, fem_wk, surf_wk,      &
!!     &          f_l, f_nl, nod_fld, ele_fld, diff_coefs)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Fsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(jacobians_type), intent(in) :: jacobians
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_data_type), intent(in) :: wide_filtering
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(dynamic_correlation_data), intent(inout) :: wk_cor
!!        type(dynamic_least_suare_data), intent(inout) :: wk_lsq
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!@endverbatim
!
      module update_with_vector_p
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
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_layering_ele_list
      use t_material_property
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_work_layer_correlate
      use t_bc_data_magne
      use t_surface_bc_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine update_with_vector_potential                           &
     &         (iak_diff_b, icomp_diff_b, ie_dbx, ie_dfbx, i_step, dt,  &
     &          FEM_prm, SGS_par, mesh, group, surf, fluid, conduct,    &
     &          layer_tbl, Bnod_bcs, Asf_bcs, Fsf_bcs,                  &
     &          iphys, iphys_ele, jacobians, rhs_tbl, FEM_elen,         &
     &          filtering, wide_filtering, m_lump, wk_cor, wk_lsq,      &
     &          wk_diff, wk_filter, mhd_fem_wk, fem_wk, surf_wk,        &
     &          f_l, f_nl, nod_fld, ele_fld, diff_coefs)
!
      use average_on_elements
      use cal_rotation_sgs
      use cal_filtering_scalars
      use cal_diff_vector_on_ele
      use cal_diff_coef_vector_p
      use cal_filtering_scalars
!
      integer(kind=kint), intent(in) :: i_step
      real(kind=kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: iak_diff_b, icomp_diff_b
      integer(kind = kint), intent(in) :: ie_dbx, ie_dfbx
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(jacobians_type), intent(in) :: jacobians
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
      type(dynamic_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      integer (kind = kint) :: iflag_dmc, iflag2
!
!   set model coefficients for vector potential
!
      iflag_dmc = dynamic_SGS_flag(i_step, SGS_par)
!
!
      if (SGS_par%commute_p%iflag_c_magne .eq. id_SGS_commute_ON        &
     &     .and. iphys%i_filter_vecp .ne. 0                             &
     &     .and. iflag_dmc .eq. 0) then
!
        if(SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
          if (iflag_debug.gt.0) write(*,*) 'cal_filtered_vector_p'
          call cal_filtered_vector_whole                                &
     &       (SGS_par%filter_p, mesh%nod_comm, mesh%node, filtering,    &
     &        iphys%i_filter_vecp, iphys%i_vecp, wk_filter, nod_fld)
          nod_fld%iflag_update(iphys%i_filter_vecp  ) = 1
          nod_fld%iflag_update(iphys%i_filter_vecp+1) = 1
          nod_fld%iflag_update(iphys%i_filter_vecp+2) = 1
        end if
!
        if     (SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF   &
     &    .and. SGS_par%model_p%iflag_SGS.eq.id_SGS_similarity          &
     &    .and. iphys%i_wide_fil_vecp.ne. 0) then
          if (iflag_debug.gt.0)                                         &
     &         write(*,*) 'cal_filtered_vector_p i_wide_fil_vecp'
          call cal_filtered_vector_whole(SGS_par%filter_p,              &
     &        mesh%nod_comm, mesh%node, wide_filtering,                 &
     &        iphys%i_wide_fil_vecp, iphys%i_filter_vecp,               &
     &        wk_filter, nod_fld)
          nod_fld%iflag_update(iphys%i_wide_fil_vecp  ) = 1
          nod_fld%iflag_update(iphys%i_wide_fil_vecp+1) = 1
          nod_fld%iflag_update(iphys%i_wide_fil_vecp+2) = 1
        end if
!
!
        if ( diff_coefs%iflag_field(iak_diff_b) .eq. 0) then
          if(SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF)     &
     &     then
            if    (SGS_par%model_p%iflag_SGS .eq. id_SGS_NL_grad        &
     &        .or. SGS_par%model_p%iflag_SGS .eq. id_SGS_similarity)    &
     &       then
              call s_cal_diff_coef_vector_p                             &
     &           (iak_diff_b, icomp_diff_b, dt, FEM_prm, SGS_par,       &
     &            mesh%nod_comm, mesh%node, mesh%ele, surf, fluid,      &
     &            layer_tbl,group%surf_grp, Asf_bcs, Fsf_bcs, iphys,    &
     &            iphys_ele, ele_fld,jacobians, rhs_tbl, FEM_elen,      &
     &            filtering, m_lump, wk_filter, wk_cor, wk_lsq,         &
     &            wk_diff, fem_wk, surf_wk,f_l, f_nl,                   &
     &            nod_fld, diff_coefs)
            end if
!
          end if
        end if
      end if
!
!   lead magnetic field
!
      if (iphys%i_magne .ne. 0) then
        if (iflag_debug.gt.0) write(*,*) 'cal_magnetic_f_by_vect_p'
        call choose_cal_rotation_sgs(SGS_par%commute_p%iflag_c_magne,   &
     &      FEM_prm%iflag_magne_supg, FEM_prm%npoint_t_evo_int, dt,     &
     &      iak_diff_b, iphys%i_vecp, iphys%i_magne,                    &
     &      mesh%ele%istack_ele_smp, m_lump, SGS_par%model_p,           &
     &      mesh%nod_comm, mesh%node, mesh%ele, surf, group%surf_grp,   &
     &      iphys_ele, ele_fld, jacobians%jac_3d, jacobians%jac_sf_grp, &
     &      FEM_elen, diff_coefs, Bnod_bcs%nod_bc_b, Asf_bcs%sgs,       &
     &      rhs_tbl,fem_wk, surf_wk, f_nl, nod_fld)
      end if
      if (iphys_ele%i_magne .ne. 0) then
        if (iflag_debug.gt.0) write(*,*) 'rot_magne_on_element'
        call rotation_on_element_1st                                    &
     &     (mesh%node, mesh%ele, jacobians%jac_3d,                      &
     &      mesh%ele%istack_ele_smp, FEM_prm%npoint_t_evo_int,          &
     &      nod_fld%ntot_phys, iphys%i_vecp, nod_fld%d_fld,             &
     &      ele_fld%ntot_phys, iphys_ele%i_magne,                       &
     &      ele_fld%iflag_update, ele_fld%d_fld)
      end if
!
      if (iphys_ele%i_current .ne. 0                                    &
     &     .and. FEM_prm%iflag_rotate_form .eq. id_turn_ON) then
        if (iflag_debug.gt.0) write(*,*) 'current_on_element'
        call rotation_on_element_1st                                    &
     &     (mesh%node, mesh%ele, jacobians%jac_3d,                      &
     &      conduct%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &      nod_fld%ntot_phys, iphys%i_magne, nod_fld%d_fld,            &
     &      ele_fld%ntot_phys, iphys_ele%i_current,                     &
     &      ele_fld%iflag_update, ele_fld%d_fld)
      end if
!
!   required field for explicit filtering
!
       if(SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF         &
     &     .and. iflag_dmc .eq. 0) then
         if (SGS_par%model_p%iflag_SGS_lorentz .eq. id_SGS_similarity   &
     &      .or. SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_similarity)  &
     &    then
           iflag2 = 3
         else if(SGS_par%model_p%iflag_SGS_lorentz .eq. id_SGS_NL_grad  &
     &       .or. SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_NL_grad)    &
     &    then
           iflag2 = 2
         else
           iflag2 = 2
         end if
       else
         if(SGS_par%model_p%iflag_SGS_lorentz .eq. id_SGS_similarity    &
     &     .or. SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_similarity)   &
     &    then
           iflag2 = 1
         else
           iflag2 = 0
         end if
       end if
!
       if (iflag_debug .ge. iflag_routine_msg) write(*,*)               &
         'flag for magnetic field filtering', iflag2
!
       if (iflag2.eq.1 .or. iflag2.eq.2 .or. iflag2.eq.3) then
         if (iphys%i_filter_magne .ne. 0) then
           if (iflag_debug.gt.0)                                        &
     &        write(*,*) 'cal_filtered_vector', iphys%i_filter_magne
           call cal_filtered_vector_whole                               &
     &        (SGS_par%filter_p, mesh%nod_comm, mesh%node, filtering,   &
     &         iphys%i_filter_magne, iphys%i_magne, wk_filter, nod_fld)
           nod_fld%iflag_update(iphys%i_filter_magne  ) = 1
           nod_fld%iflag_update(iphys%i_filter_magne+1) = 1
           nod_fld%iflag_update(iphys%i_filter_magne+2) = 1
         end if
!
         if (iphys_ele%i_filter_magne .ne. 0) then
           if (iflag_debug .ge. iflag_routine_msg) write(*,*)           &
     &                         'filtered_magne_on_ele'
            call vector_on_element_1st                                  &
     &         (mesh%node, mesh%ele, jacobians%jac_3d,                  &
     &          mesh%ele%istack_ele_smp, FEM_prm%npoint_t_evo_int,      &
     &          nod_fld%ntot_phys, iphys%i_filter_magne,                &
     &          nod_fld%d_fld, ele_fld%ntot_phys,                       &
     &          iphys_ele%i_filter_magne, ele_fld%iflag_update,         &
     &          ele_fld%d_fld)
         end if
!
         if(iflag2.eq.2 .and. ie_dfbx.ne.0) then
           if (iflag_debug .ge. iflag_routine_msg) write(*,*)           &
     &                         'diff_filter_b_on_ele'
           call sel_int_diff_vector_on_ele(FEM_prm%npoint_t_evo_int,    &
     &         mesh%ele%istack_ele_smp, iphys%i_filter_magne, ie_dfbx,  &
     &         mesh%node, mesh%ele, nod_fld,                            &
     &         jacobians%jac_3d, jacobians%jac_3d_l, mhd_fem_wk)
         end if
!
         if(iflag2.eq.3 .and. iphys%i_wide_fil_magne.ne.0) then
           call cal_filtered_vector_whole(SGS_par%filter_p,             &
     &          mesh%nod_comm, mesh%node, wide_filtering,               &
     &         iphys%i_wide_fil_magne, iphys%i_filter_magne,            &
     &         wk_filter, nod_fld)
           nod_fld%iflag_update(iphys%i_wide_fil_magne  ) = 1
           nod_fld%iflag_update(iphys%i_wide_fil_magne+1) = 1
           nod_fld%iflag_update(iphys%i_wide_fil_magne+2) = 1
         end if
!
       end if
!
!   required field for gradient model
!
!
       if (  SGS_par%model_p%iflag_SGS_lorentz .eq. id_SGS_NL_grad      &
     &  .or. SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
         if ( ie_dbx.ne.0 ) then
           if (iflag_debug.gt.0) write(*,*) 'diff_magne_on_ele'
           call sel_int_diff_vector_on_ele(FEM_prm%npoint_t_evo_int,    &
     &        mesh%ele%istack_ele_smp, iphys%i_magne, ie_dbx,           &
     &        mesh%node, mesh%ele, nod_fld,                             &
     &        jacobians%jac_3d, jacobians%jac_3d_l, mhd_fem_wk)
        end if
       end if
!
       end subroutine update_with_vector_potential
!
!-----------------------------------------------------------------------
!
      end module update_with_vector_p
