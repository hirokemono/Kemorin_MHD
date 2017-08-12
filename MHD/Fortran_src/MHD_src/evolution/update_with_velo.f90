!>@file   update_with_velo.f90
!!        module update_with_velo
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine update_with_velocity                                 &
!!     &         (iak_diff_v, icomp_diff_v, ie_dvx, ie_dfvx, i_step, dt,&
!!     &          FEM_prm, SGS_par, mesh, group, surf, fluid,           &
!!     &          Vsf_bcs, Psf_bcs, iphys, iphys_ele, jacobians,        &
!!     &          rhs_tbl, FEM_filters, mlump_fl, FEM_SGS_wk,           &
!!     &          mhd_fem_wk, rhs_mat, nod_fld, ele_fld, diff_coefs)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(velocity_surf_bc_type), intent(in) :: Vsf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Psf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(jacobians_type), intent(in) :: jacobians
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
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
      use t_phys_address
      use t_work_FEM_integration
      use t_MHD_finite_element_mat
      use t_FEM_MHD_filter_data
      use t_material_property
      use t_work_FEM_dynamic_SGS
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
      subroutine update_with_velocity                                   &
     &         (iak_diff_v, icomp_diff_v, ie_dvx, ie_dfvx, i_step, dt,  &
     &          FEM_prm, SGS_par, mesh, group, surf, fluid,             &
     &          Vsf_bcs, Psf_bcs, iphys, iphys_ele, jacobians,          &
     &          rhs_tbl, FEM_filters, mlump_fl, FEM_SGS_wk,             &
     &          mhd_fem_wk, rhs_mat, nod_fld, ele_fld, diff_coefs)
!
      use average_on_elements
      use cal_filtering_scalars
      use cal_diff_vector_on_ele
      use cal_diff_coef_velo
!
      integer(kind=kint), intent(in) :: i_step
      real(kind=kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: iak_diff_v, icomp_diff_v
      integer(kind = kint), intent(in) :: ie_dvx, ie_dfvx
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid
      type(velocity_surf_bc_type), intent(in) :: Vsf_bcs
      type(potential_surf_bc_type), intent(in) :: Psf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(jacobians_type), intent(in) :: jacobians
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_matrices), intent(in) :: mlump_fl
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
      iflag_dmc = dynamic_SGS_flag(i_step, SGS_par)
!
      if (iphys_ele%i_velo .ne. 0) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &                 write(*,*) 'velocity_on_element'
        call vector_on_element_1st                                      &
     &     (mesh%node, mesh%ele, jacobians%jac_3d,                      &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      nod_fld%ntot_phys, iphys%i_velo, nod_fld%d_fld,             &
     &      ele_fld%ntot_phys, iphys_ele%i_velo,                        &
     &      ele_fld%iflag_update, ele_fld%d_fld)
      end if
!
      if( FEM_prm%iflag_rotate_form .eq. id_turn_ON                     &
     &      .and. iphys_ele%i_vort .ne. 0) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &                 write(*,*) 'vorticity_on_element'
        call rotation_on_element_1st                                    &
     &     (mesh%node, mesh%ele, jacobians%jac_3d,                      &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      nod_fld%ntot_phys, iphys%i_velo, nod_fld%d_fld,             &
     &      ele_fld%ntot_phys, iphys_ele%i_vort,                        &
     &      ele_fld%iflag_update, ele_fld%d_fld)
      end if
!
!   required field for explicit filtering
!
      if (iphys%i_filter_velo .ne. 0) then
!
        if (SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF       &
     &      .and. iflag_dmc.eq.0) then
          iflag2 = 1
        else if(SGS_par%model_p%iflag_SGS .eq. id_SGS_similarity) then
          iflag2 = 1
        else
          iflag2 = 0
        end if
!
        if (iflag2 .eq. 1) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &      write(*,*) 'cal_filtered_vector', iphys%i_filter_velo
          call cal_filtered_vector_whole                                &
     &       (SGS_par%filter_p, mesh%nod_comm, mesh%node,               &
     &        FEM_filters%filtering, iphys%i_filter_velo, iphys%i_velo, &
     &        FEM_SGS_wk%wk_filter, nod_fld)
          nod_fld%iflag_update(iphys%i_filter_velo  ) = 1
          nod_fld%iflag_update(iphys%i_filter_velo+1) = 1
          nod_fld%iflag_update(iphys%i_filter_velo+2) = 1
        end if
      end if
!
      if (iphys%i_wide_fil_velo.ne.0 .and. iflag_dmc.eq.0) then
        if (SGS_par%model_p%iflag_SGS.eq.id_SGS_similarity              &
     &    .and. SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF)  &
     &   then
          call cal_filtered_vector_whole(SGS_par%filter_p,              &
     &        mesh%nod_comm, mesh%node, FEM_filters%wide_filtering,     &
     &        iphys%i_wide_fil_velo, iphys%i_filter_velo,               &
     &        FEM_SGS_wk%wk_filter, nod_fld)
          nod_fld%iflag_update(iphys%i_wide_fil_velo  ) = 1
          nod_fld%iflag_update(iphys%i_wide_fil_velo+1) = 1
          nod_fld%iflag_update(iphys%i_wide_fil_velo+2) = 1
        end if
      end if
!
!    required field for vector potential
!
       if (SGS_par%model_p%iflag_SGS.eq.id_SGS_NL_grad                  &
     &    .and. SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF   &
     &    .and. iflag_dmc.eq.0) then
!
         if (iphys_ele%i_filter_velo.ne.0) then
           if(iflag_debug .ge. iflag_routine_msg)                       &
     &                 write(*,*) 'diff_filter_v_on_ele'
           call sel_int_diff_vector_on_ele(FEM_prm%npoint_t_evo_int,    &
     &        fluid%istack_ele_fld_smp, iphys%i_filter_velo, ie_dfvx,   &
     &        mesh%node, mesh%ele, nod_fld,                             &
     &        jacobians%jac_3d, jacobians%jac_3d_l, mhd_fem_wk)
         end if
!
        if (SGS_par%commute_p%iflag_c_velo .eq. id_SGS_commute_ON       &
     &         .and. diff_coefs%iflag_field(iak_diff_v) .eq. 0) then
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &                 write(*,*) 's_cal_diff_coef_velo'
          call s_cal_diff_coef_velo                                     &
     &       (iak_diff_v, icomp_diff_v, dt, FEM_prm, SGS_par,           &
     &        mesh%nod_comm, mesh%node, mesh%ele, surf, group%surf_grp, &
     &        Vsf_bcs, Psf_bcs, iphys, iphys_ele, ele_fld, fluid,       &
     &        FEM_filters%layer_tbl, jacobians, rhs_tbl,                &
     &        FEM_filters%FEM_elens, FEM_filters%filtering,             &
     &        FEM_SGS_wk%wk_filter, FEM_SGS_wk%wk_cor,                  &
     &        FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_diff, mlump_fl,          &
     &        rhs_mat%fem_wk, rhs_mat%surf_wk,                          &
     &        rhs_mat%f_l, rhs_mat%f_nl, nod_fld, diff_coefs)
        end if
!
      end if
!
!   required field for gradient model
!
       if (ie_dvx .ne. 0) then
         if(SGS_par%model_p%iflag_SGS .eq. id_SGS_NL_grad) then
           if(iflag_debug .ge. iflag_routine_msg)                       &
     &                 write(*,*) 'diff_velocity_on_ele'
           call sel_int_diff_vector_on_ele                              &
     &        (FEM_prm%npoint_t_evo_int, fluid%istack_ele_fld_smp,      &
     &        iphys%i_velo, ie_dvx,mesh%node, mesh%ele, nod_fld,        &
     &        jacobians%jac_3d, jacobians%jac_3d_l, mhd_fem_wk)
         end if
       end if
!
       end subroutine update_with_velocity
!
!-----------------------------------------------------------------------
!
      end module update_with_velo
