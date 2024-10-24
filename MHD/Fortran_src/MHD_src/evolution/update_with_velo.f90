!>@file   update_with_velo.f90
!!        module update_with_velo
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine update_with_velocity(i_step, dt, FEM_prm, SGS_par,   &
!!     &          mesh, group, fluid, Vsf_bcs, Psf_bcs,                 &
!!     &          iphys_base, iphys_fil, iphys_wfl, iphys_SGS_wk,       &
!!     &          iphys_ele_base, iphys_ele_fil, fem_int, FEM_filters,  &
!!     &          iak_diff_base, icomp_diff_base,                       &
!!     &          iphys_elediff_vec, iphys_elediff_fil,                 &
!!     &          mk_MHD, FEM_SGS_wk, mhd_fem_wk, rhs_mat,              &
!!     &          nod_fld, ele_fld, diff_coefs, v_sol)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(velocity_surf_bc_type), intent(in) :: Vsf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Psf_bcs
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_field_address), intent(in) :: iphys_fil
!!        type(base_field_address), intent(in) :: iphys_wfl
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(base_field_address), intent(in) :: iphys_ele_fil
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(base_field_address), intent(in) :: iak_diff_base
!!        type(base_field_address), intent(in) :: icomp_diff_base
!!        type(base_field_address), intent(in) :: iphys_elediff_vec
!!        type(base_field_address), intent(in) :: iphys_elediff_fil
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!@endverbatim
!
      module update_with_velo
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
      use t_SGS_model_coef_labels
      use t_work_FEM_integration
      use t_jacobians
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_FEM_MHD_filter_data
      use t_material_property
      use t_work_FEM_dynamic_SGS
      use t_surface_bc_scalar
      use t_surface_bc_velocity
      use t_vector_for_solver
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine update_with_velocity(i_step, dt, FEM_prm, SGS_par,     &
     &          mesh, group, fluid, Vsf_bcs, Psf_bcs,                   &
     &          iphys_base, iphys_fil, iphys_wfl, iphys_SGS_wk,         &
     &          iphys_ele_base, iphys_ele_fil, fem_int, FEM_filters,    &
     &          iak_diff_base, icomp_diff_base,                         &
     &          iphys_elediff_vec, iphys_elediff_fil,                   &
     &          mk_MHD, FEM_SGS_wk, mhd_fem_wk, rhs_mat,                &
     &          nod_fld, ele_fld, diff_coefs, v_sol)
!
      use average_on_elements
      use cal_filtering_scalars
      use cal_diff_vector_on_ele
      use cal_diff_coef_velo
!
      integer(kind=kint), intent(in) :: i_step
      real(kind=kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(field_geometry_data), intent(in) :: fluid
      type(velocity_surf_bc_type), intent(in) :: Vsf_bcs
      type(potential_surf_bc_type), intent(in) :: Psf_bcs
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(base_field_address), intent(in) :: iphys_wfl
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!
      type(base_field_address), intent(in) :: iphys_ele_base
      type(base_field_address), intent(in) :: iphys_ele_fil
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(base_field_address), intent(in) :: iak_diff_base
      type(base_field_address), intent(in) :: icomp_diff_base
      type(base_field_address), intent(in) :: iphys_elediff_vec
      type(base_field_address), intent(in) :: iphys_elediff_fil
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer (kind = kint) :: iflag2
      logical :: iflag_dmc
!
!
      iflag_dmc = dynamic_SGS_flag(i_step, SGS_par)
!
      if (iphys_ele_base%i_velo .ne. 0) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &                 write(*,*) 'velocity_on_element'
        call vector_on_element_1st(mesh%node, mesh%ele, fem_int%jcs,    &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys_base%i_velo, nod_fld, iphys_ele_base%i_velo, ele_fld)
      end if
!
      if( FEM_prm%iflag_rotate_form .eq. id_turn_ON                     &
     &      .and. iphys_ele_base%i_vort .ne. 0) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &                 write(*,*) 'vorticity_on_element'
        call rotation_on_element_1st(mesh%node, mesh%ele, fem_int%jcs,  &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys_base%i_velo, nod_fld, iphys_ele_base%i_vort, ele_fld)
      end if
!
!   required field for explicit filtering
!
      if (iphys_fil%i_velo .ne. 0) then
        if (SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF       &
     &      .and. iflag_dmc) then
          iflag2 = 1
        else if(SGS_par%model_p%iflag_SGS .eq. id_SGS_similarity) then
          iflag2 = 1
        else
          iflag2 = 0
        end if
!
        if (iflag2 .eq. 1) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &      write(*,*) 'cal_filtered_vector', iphys_fil%i_velo
          call cal_filtered_vector_whole(SGS_par%filter_p,              &
     &        mesh%nod_comm, mesh%node, FEM_filters%filtering,          &
     &        iphys_fil%i_velo, iphys_base%i_velo,                      &
     &        FEM_SGS_wk%wk_filter, nod_fld, v_sol)
          nod_fld%iflag_update(iphys_fil%i_velo  ) = 1
          nod_fld%iflag_update(iphys_fil%i_velo+1) = 1
          nod_fld%iflag_update(iphys_fil%i_velo+2) = 1
        end if
      end if
!
      if(iphys_wfl%i_velo.ne.0 .and. iflag_dmc) then
        if(SGS_par%model_p%iflag_SGS.eq.id_SGS_similarity               &
     &    .and. SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF)  &
     &   then
          call cal_filtered_vector_whole(SGS_par%filter_p,              &
     &        mesh%nod_comm, mesh%node, FEM_filters%wide_filtering,     &
     &        iphys_wfl%i_velo, iphys_fil%i_velo,                       &
     &        FEM_SGS_wk%wk_filter, nod_fld, v_sol)
          nod_fld%iflag_update(iphys_wfl%i_velo  ) = 1
          nod_fld%iflag_update(iphys_wfl%i_velo+1) = 1
          nod_fld%iflag_update(iphys_wfl%i_velo+2) = 1
        end if
      end if
!
!    required field for vector potential
!
       if (SGS_par%model_p%iflag_SGS.eq.id_SGS_NL_grad                  &
     &    .and. SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF   &
     &    .and. iflag_dmc) then
!
         if (iphys_ele_fil%i_velo.ne.0) then
           if(iflag_debug .ge. iflag_routine_msg)                       &
     &                 write(*,*) 'diff_filter_v_on_ele'
           call sel_int_diff_vector_on_ele                              &
     &        (FEM_prm%npoint_t_evo_int, fluid%istack_ele_fld_smp,      &
     &         iphys_fil%i_velo, iphys_elediff_fil%i_velo,              &
     &         mesh%node, mesh%ele, nod_fld, fem_int%jcs, mhd_fem_wk)
         end if
!
        if (SGS_par%commute_p%iflag_c_velo .eq. id_SGS_commute_ON       &
     &    .and. diff_coefs%iflag_field(iak_diff_base%i_velo).eq.0) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &                 write(*,*) 's_cal_diff_coef_velo'
          call s_cal_diff_coef_velo                                     &
     &       (iak_diff_base%i_velo, icomp_diff_base%i_velo,             &
     &        dt, FEM_prm, SGS_par,  mesh%nod_comm, mesh%node,          &
     &        mesh%ele, mesh%surf, group%surf_grp, Vsf_bcs, Psf_bcs,    &
     &        iphys_base, iphys_fil, iphys_SGS_wk,                      &
     &        iphys_ele_base, ele_fld, fluid,                           &
     &        FEM_filters%layer_tbl, fem_int%jcs, fem_int%rhs_tbl,      &
     &        FEM_filters%FEM_elens, FEM_filters%filtering,             &
     &        FEM_SGS_wk%wk_filter, FEM_SGS_wk%wk_cor,                  &
     &        FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_diff, mk_MHD%mlump_fl,   &
     &        rhs_mat%fem_wk, rhs_mat%surf_wk,                          &
     &        rhs_mat%f_l, rhs_mat%f_nl, nod_fld, diff_coefs, v_sol)
        end if
!
      end if
!
!   required field for gradient model
!
       if (iphys_elediff_vec%i_velo .ne. 0) then
         if(SGS_par%model_p%iflag_SGS .eq. id_SGS_NL_grad) then
           if(iflag_debug .ge. iflag_routine_msg)                       &
     &                 write(*,*) 'diff_velocity_on_ele'
           call sel_int_diff_vector_on_ele                              &
     &        (FEM_prm%npoint_t_evo_int, fluid%istack_ele_fld_smp,      &
     &         iphys_base%i_velo, iphys_elediff_vec%i_velo,             &
     &         mesh%node, mesh%ele, nod_fld, fem_int%jcs, mhd_fem_wk)
         end if
       end if
!
       end subroutine update_with_velocity
!
!-----------------------------------------------------------------------
!
      end module update_with_velo
