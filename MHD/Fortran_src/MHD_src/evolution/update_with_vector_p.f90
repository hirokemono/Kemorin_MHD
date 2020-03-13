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
!!     &          FEM_prm, SGS_par, mesh, group, fluid, conduct,        &
!!     &          Bnod_bcs, Asf_bcs, Fsf_bcs, iphys, iphys_ele,         &
!!     &          fem_int, FEM_filters, FEM_SGS_wk, mhd_fem_wk, rhs_mat,&
!!     &          nod_fld, ele_fld, diff_coefs)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Fsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
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
      use t_table_FEM_const
      use t_jacobians
      use t_MHD_finite_element_mat
      use t_FEM_MHD_filter_data
      use t_material_property
      use t_work_layer_correlate
      use t_bc_data_magne
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
      subroutine update_with_vector_potential                           &
     &         (iak_diff_b, icomp_diff_b, ie_dbx, ie_dfbx, i_step, dt,  &
     &          FEM_prm, SGS_par, mesh, group, fluid, conduct,          &
     &          Bnod_bcs, Asf_bcs, Fsf_bcs, iphys, iphys_ele,           &
     &          fem_int, FEM_filters, FEM_SGS_wk, mhd_fem_wk, rhs_mat,  &
     &          nod_fld, ele_fld, diff_coefs)
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
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
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
!   set model coefficients for vector potential
!
      iflag_dmc = dynamic_SGS_flag(i_step, SGS_par)
!
!
      if (SGS_par%commute_p%iflag_c_magne .eq. id_SGS_commute_ON        &
     &     .and. iphys%filter_fld%i_vecp .ne. 0                         &
     &     .and. iflag_dmc .eq. 0) then
!
        if(SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
          if (iflag_debug.gt.0) write(*,*) 'cal_filtered_vector_p'
          call cal_filtered_vector_whole(SGS_par%filter_p,              &
     &        mesh%nod_comm, mesh%node, FEM_filters%filtering,          &
     &        iphys%filter_fld%i_vecp, iphys%base%i_vecp,               &
     &        FEM_SGS_wk%wk_filter, nod_fld)
          nod_fld%iflag_update(iphys%filter_fld%i_vecp  ) = 1
          nod_fld%iflag_update(iphys%filter_fld%i_vecp+1) = 1
          nod_fld%iflag_update(iphys%filter_fld%i_vecp+2) = 1
        end if
!
        if     (SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF   &
     &    .and. SGS_par%model_p%iflag_SGS.eq.id_SGS_similarity          &
     &    .and. iphys%wide_filter_fld%i_vecp.ne. 0) then
          if (iflag_debug.gt.0)                                         &
     &        write(*,*) 'cal_filtered_vector_p wide_filter_fld%i_vecp'
          call cal_filtered_vector_whole(SGS_par%filter_p,              &
     &        mesh%nod_comm, mesh%node, FEM_filters%wide_filtering,     &
     &        iphys%wide_filter_fld%i_vecp, iphys%filter_fld%i_vecp,    &
     &        FEM_SGS_wk%wk_filter, nod_fld)
          nod_fld%iflag_update(iphys%wide_filter_fld%i_vecp  ) = 1
          nod_fld%iflag_update(iphys%wide_filter_fld%i_vecp+1) = 1
          nod_fld%iflag_update(iphys%wide_filter_fld%i_vecp+2) = 1
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
     &            mesh%nod_comm, mesh%node, mesh%ele, mesh%surf, fluid, &
     &            FEM_filters%layer_tbl, group%surf_grp,                &
     &            Asf_bcs, Fsf_bcs, iphys, iphys_ele, ele_fld,          &
     &            fem_int%jcs, fem_int%rhs_tbl, FEM_filters%FEM_elens,  &
     &            FEM_filters%filtering, fem_int%m_lump,                &
     &            FEM_SGS_wk%wk_filter, FEM_SGS_wk%wk_cor,              &
     &            FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_diff,                &
     &            rhs_mat%fem_wk, rhs_mat%surf_wk,                      &
     &            rhs_mat%f_l, rhs_mat%f_nl, nod_fld, diff_coefs)
            end if
!
          end if
        end if
      end if
!
!   lead magnetic field
!
      if (iphys%base%i_magne .ne. 0) then
        if (iflag_debug.gt.0) write(*,*) 'cal_magnetic_f_by_vect_p'
        call choose_cal_rotation_sgs(SGS_par%commute_p%iflag_c_magne,   &
     &      FEM_prm%iflag_magne_supg, FEM_prm%npoint_t_evo_int, dt,     &
     &      iak_diff_b, iphys%base%i_vecp, iphys%base%i_magne,          &
     &      mesh%ele%istack_ele_smp, fem_int%m_lump, SGS_par%model_p,   &
     &      mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,              &
     &      group%surf_grp, iphys_ele, ele_fld, fem_int%jcs,            &
     &      FEM_filters%FEM_elens, diff_coefs, Bnod_bcs%nod_bc_b,       &
     &      Asf_bcs%sgs, fem_int%rhs_tbl, rhs_mat%fem_wk,               &
     &      rhs_mat%surf_wk, rhs_mat%f_nl, nod_fld)
      end if
      if (iphys_ele%base%i_magne .ne. 0) then
        if (iflag_debug.gt.0) write(*,*) 'rot_magne_on_element'
        call rotation_on_element_1st(mesh%node, mesh%ele, fem_int%jcs,  &
     &      mesh%ele%istack_ele_smp, FEM_prm%npoint_t_evo_int,          &
     &      iphys%base%i_vecp, nod_fld, iphys_ele%base%i_magne,         &
     &      ele_fld)
      end if
!
      if (iphys_ele%base%i_current .ne. 0                               &
     &     .and. FEM_prm%iflag_rotate_form .eq. id_turn_ON) then
        if (iflag_debug.gt.0) write(*,*) 'current_on_element'
        call rotation_on_element_1st(mesh%node, mesh%ele, fem_int%jcs,  &
     &      conduct%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &      iphys%base%i_magne, nod_fld, iphys_ele%base%i_current,      &
     &      ele_fld)
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
         if (iphys%filter_fld%i_magne .ne. 0) then
           if (iflag_debug.gt.0) write(*,*) 'cal_filtered_vector',      &
     &                          iphys%filter_fld%i_magne
           call cal_filtered_vector_whole(SGS_par%filter_p,             &
     &         mesh%nod_comm, mesh%node, FEM_filters%filtering,         &
     &         iphys%filter_fld%i_magne, iphys%base%i_magne,            &
     &         FEM_SGS_wk%wk_filter, nod_fld)
           nod_fld%iflag_update(iphys%filter_fld%i_magne  ) = 1
           nod_fld%iflag_update(iphys%filter_fld%i_magne+1) = 1
           nod_fld%iflag_update(iphys%filter_fld%i_magne+2) = 1
         end if
!
         if (iphys_ele%filter_fld%i_magne .ne. 0) then
           if (iflag_debug .ge. iflag_routine_msg) write(*,*)           &
     &                         'filtered_magne_on_ele'
            call vector_on_element_1st                                  &
     &         (mesh%node, mesh%ele, fem_int%jcs,                       &
     &          mesh%ele%istack_ele_smp, FEM_prm%npoint_t_evo_int,      &
     &          iphys%filter_fld%i_magne, nod_fld,                      &
     &          iphys_ele%filter_fld%i_magne, ele_fld)
         end if
!
         if(iflag2.eq.2 .and. ie_dfbx.ne.0) then
           if (iflag_debug .ge. iflag_routine_msg) write(*,*)           &
     &                         'diff_filter_b_on_ele'
           call sel_int_diff_vector_on_ele                              &
     &        (FEM_prm%npoint_t_evo_int, mesh%ele%istack_ele_smp,       &
     &         iphys%filter_fld%i_magne, ie_dfbx,                       &
     &         mesh%node, mesh%ele, nod_fld, fem_int%jcs, mhd_fem_wk)
         end if
!
         if(iflag2.eq.3 .and. iphys%wide_filter_fld%i_magne.ne.0) then
           call cal_filtered_vector_whole(SGS_par%filter_p,             &
     &          mesh%nod_comm, mesh%node, FEM_filters%wide_filtering,   &
     &         iphys%wide_filter_fld%i_magne, iphys%filter_fld%i_magne, &
     &         FEM_SGS_wk%wk_filter, nod_fld)
           nod_fld%iflag_update(iphys%wide_filter_fld%i_magne  ) = 1
           nod_fld%iflag_update(iphys%wide_filter_fld%i_magne+1) = 1
           nod_fld%iflag_update(iphys%wide_filter_fld%i_magne+2) = 1
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
     &        mesh%ele%istack_ele_smp, iphys%base%i_magne, ie_dbx,      &
     &        mesh%node, mesh%ele, nod_fld, fem_int%jcs, mhd_fem_wk)
        end if
       end if
!
       end subroutine update_with_vector_potential
!
!-----------------------------------------------------------------------
!
      end module update_with_vector_p
