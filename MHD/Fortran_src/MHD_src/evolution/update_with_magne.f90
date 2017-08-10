!>@file   update_with_magne.f90
!!        module update_with_magne
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine update_with_magnetic_field                           &
!!     &         (iak_diff_b, icomp_diff_b, ie_dbx, ie_dfbx, i_step, dt,&
!!     &          FEM_prm, SGS_par, mesh, group, surf, fluid, conduct,  &
!!     &          layer_tbl, Bsf_bcs, Fsf_bcs, iphys, iphys_ele,        &
!!     &          jacobians, rhs_tbl, FEM_elens,                        &
!!     &          filtering, wide_filtering, m_lump, FEM_SGS_wk,        &
!!     &          mhd_fem_wk, rhs_mat, nod_fld, ele_fld, diff_coefs)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Fsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(jacobians_type), intent(in) :: jacobians
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_data_type), intent(in) :: wide_filtering
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!@endverbatim
!
      module update_with_magne
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
      use t_table_FEM_const
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_layering_ele_list
      use t_surface_bc_data
      use t_material_property
      use t_work_layer_correlate
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
      subroutine update_with_magnetic_field                             &
     &         (iak_diff_b, icomp_diff_b, ie_dbx, ie_dfbx, i_step, dt,  &
     &          FEM_prm, SGS_par, mesh, group, surf, fluid, conduct,    &
     &          layer_tbl, Bsf_bcs, Fsf_bcs, iphys, iphys_ele,          &
     &          jacobians, rhs_tbl, FEM_elens,                          &
     &          filtering, wide_filtering, m_lump, FEM_SGS_wk,          &
     &          mhd_fem_wk, rhs_mat, nod_fld, ele_fld, diff_coefs)
!
      use average_on_elements
      use cal_filtering_scalars
      use cal_diff_vector_on_ele
      use cal_diff_coef_magne
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
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(jacobians_type), intent(in) :: jacobians
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      integer (kind = kint) :: iflag_dmc, iflag2
!
!
      if (iphys_ele%i_magne .ne. 0) then
        call vector_on_element_1st                                      &
     &     (mesh%node, mesh%ele, jacobians%jac_3d,                      &
     &      mesh%ele%istack_ele_smp, FEM_prm%npoint_t_evo_int,          &
     &      nod_fld%ntot_phys, iphys%i_magne, nod_fld%d_fld,            &
     &      ele_fld%ntot_phys, iphys_ele%i_magne,                       &
     &      ele_fld%iflag_update, ele_fld%d_fld)
      end if
!
!
      iflag_dmc = dynamic_SGS_flag(i_step, SGS_par)
      if(SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF          &
     &     .and.  iflag_dmc .eq. 0) then
        if(SGS_par%model_p%iflag_SGS_lorentz .eq. id_SGS_similarity     &
     &    .or. SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_similarity)    &
     &   then
          iflag2 = 3
        else if(SGS_par%model_p%iflag_SGS_lorentz .eq. id_SGS_NL_grad   &
     &     .or. SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_NL_grad)      &
     &   then
          iflag2 = 2
        else
          iflag2 = 2
        end if
      else
        if(SGS_par%model_p%iflag_SGS_lorentz .eq. id_SGS_similarity     &
     &   .or. SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_similarity)     &
     &   then
          iflag2 = 1
        else
          iflag2 = 0
        end if
      end if
!
      if (iflag2.eq.1 .or. iflag2.eq.2 .or. iflag2.eq.3) then
        if (iphys%i_filter_magne .ne. 0) then
          if(iflag_debug.gt.0) write(*,*)                               &
     &         'cal_filtered_vector_whole',  iphys%i_filter_magne
          call cal_filtered_vector_whole                                &
     &       (SGS_par%filter_p, mesh%nod_comm, mesh%node, filtering,    &
     &        iphys%i_filter_magne, iphys%i_magne,                      &
     &        FEM_SGS_wk%wk_filter, nod_fld)
          nod_fld%iflag_update(iphys%i_filter_magne  ) = 1
          nod_fld%iflag_update(iphys%i_filter_magne+1) = 1
          nod_fld%iflag_update(iphys%i_filter_magne+2) = 1
        end if
!
        if (iflag2.eq.2 .and. iphys_ele%i_filter_magne.ne.0) then
          if (iflag_debug.gt.0) write(*,*) 'filtered_magne_on_ele'
          call vector_on_element_1st                                    &
     &       (mesh%node, mesh%ele, jacobians%jac_3d,                    &
     &        mesh%ele%istack_ele_smp, FEM_prm%npoint_t_evo_int,        &
     &        nod_fld%ntot_phys, iphys%i_filter_magne,                  &
     &        nod_fld%d_fld, ele_fld%ntot_phys,                         &
     &        iphys_ele%i_filter_magne, ele_fld%iflag_update,           &
     &        ele_fld%d_fld)
        end if
!
        if (iflag2.eq.2 .and. ie_dfbx.ne.0) then
          if (iflag_debug.gt.0) write(*,*) 'diff_filter_b_on_ele'
          call sel_int_diff_vector_on_ele(FEM_prm%npoint_t_evo_int,     &
     &        mesh%ele%istack_ele_smp, iphys%i_filter_magne, ie_dfbx,   &
     &        mesh%node, mesh%ele, nod_fld,                             &
     &        jacobians%jac_3d, jacobians%jac_3d_l, mhd_fem_wk)
        end if
!
        if (iflag2.eq.3 .and. iphys%i_wide_fil_magne.ne.0) then
          call cal_filtered_vector_whole(SGS_par%filter_p,              &
     &         mesh%nod_comm, mesh%node, wide_filtering,                &
     &         iphys%i_wide_fil_magne, iphys%i_filter_magne,            &
     &         FEM_SGS_wk%wk_filter, nod_fld)
           nod_fld%iflag_update(iphys%i_wide_fil_magne  ) = 1
           nod_fld%iflag_update(iphys%i_wide_fil_magne+1) = 1
           nod_fld%iflag_update(iphys%i_wide_fil_magne+2) = 1
        end if
      end if
!
!
      if(SGS_par%commute_p%iflag_c_magne .eq. id_SGS_commute_ON         &
     &     .and. diff_coefs%iflag_field(iak_diff_b) .eq. 0) then
        if (iflag2.eq.2 .or. iflag2.eq.3) then
          if (iflag_debug.gt.0) write(*,*) 's_cal_diff_coef_magne'
          call s_cal_diff_coef_magne                                    &
     &       (iak_diff_b, icomp_diff_b, dt, FEM_prm, SGS_par,           &
     &        mesh%nod_comm, mesh%node, mesh%ele,                       &
     &        surf, group%surf_grp, Bsf_bcs, Fsf_bcs,                   &
     &        iphys, iphys_ele, ele_fld, fluid, layer_tbl,              &
     &        jacobians, rhs_tbl, FEM_elens, filtering, m_lump,         &
     &        FEM_SGS_wk%wk_filter, FEM_SGS_wk%wk_cor,                  &
     &        FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_diff,                    &
     &        rhs_mat%fem_wk, rhs_mat%surf_wk,                          &
     &        rhs_mat%f_l, rhs_mat%f_nl, nod_fld, diff_coefs)
        end if
      end if
 !
 !
      if (  SGS_par%model_p%iflag_SGS_lorentz .eq. id_SGS_NL_grad       &
     & .or. SGS_par%model_p%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
        if ( ie_dbx.ne.0 ) then
           if (iflag_debug.gt.0) write(*,*) 'diff_magne_on_ele'
            call sel_int_diff_vector_on_ele                             &
     &         (FEM_prm%npoint_t_evo_int, mesh%ele%istack_ele_smp,      &
     &          iphys%i_magne, ie_dbx, mesh%node, mesh%ele, nod_fld,    &
     &          jacobians%jac_3d, jacobians%jac_3d_l, mhd_fem_wk)
        end if
      end if
!
      if (iphys_ele%i_current .ne. 0                                    &
     &     .and. FEM_prm%iflag_rotate_form .eq. id_turn_ON) then
         if (iflag_debug.gt.0)  write(*,*) 'current_on_element'
        call rotation_on_element_1st                                    &
     &     (mesh%node, mesh%ele, jacobians%jac_3d,                      &
     &      conduct%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &      nod_fld%ntot_phys, iphys%i_magne, nod_fld%d_fld,            &
     &      ele_fld%ntot_phys, iphys_ele%i_current,                     &
     &      ele_fld%iflag_update, ele_fld%d_fld)
      end if
!
!      call rotation_on_element_1st                                     &
!     &   (mesh%node, mesh%ele, jacobians%jac_3d,                       &
!     &    mesh%ele%istack_ele_smp, FEM_prm%npoint_t_evo_int,           &
!     &    nod_fld%ntot_phys, iphys%i_filter_vecp, nod_fld%d_fld,       &
!     &    ele_fld%ntot_phys, iphys_ele%i_filter_magne,                 &
!     &    ele_fld%iflag_update, ele_fld%d_fld)
!
       end subroutine update_with_magnetic_field
!
!-----------------------------------------------------------------------
!
      end module update_with_magne
