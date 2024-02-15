!
!      module cal_true_sgs_terms
!
!      Written by H. Matsui on Oct., 2005
!
!!      subroutine cal_true_sgs_terms_pre                               &
!!     &         (dt, FEM_prm, SGS_par, mesh, sf_grp, fluid, conduct,   &
!!     &          fl_prop, cd_prop, ht_prop, cp_prop, nod_bcs, surf_bcs,&
!!     &          iphys, iphys_LES, iphys_ele_base, ak_MHD, fem_int,    &
!!     &          FEM_elens, diff_coefs, mk_MHD, mhd_fem_wk, rhs_mat,   &
!!     &          nod_fld, ele_fld, v_sol, SR_sig, SR_r)
!!      subroutine cal_true_sgs_terms_post(filter_param, mesh,          &
!!     &          iphys_div_frc, iphys_trSGS, iphys_div_trSGS,          &
!!     &          iphys_SGS_wk, filtering, wk_filter, nod_fld,          &
!!     &          v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(base_force_address), intent(in) :: iphys_div_frc
!!        type(SGS_term_address), intent(in) :: iphys_trSGS
!!        type(SGS_term_address), intent(in) :: iphys_div_trSGS
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module cal_true_sgs_terms
!
      use m_precision
!
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_mesh_data
      use t_geometry_data_MHD
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_SGS_model_addresses
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_filtering_data
      use t_bc_data_MHD
      use t_surface_bc_vector
      use t_surface_bc_velocity
      use t_surface_bc_data_MHD
      use t_material_property
      use t_SGS_model_coefs
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_work_FEM_integration
      use t_vector_for_solver
      use t_solver_SR
!
      use cal_fluxes
      use copy_nodal_fields
!
      use cal_filtering_scalars
!
      implicit none
!
      private :: cal_div_sgs_s_flux_true_pre
      private :: cal_div_sgs_m_flux_true_pre
      private :: cal_div_sgs_maxwell_true_pre
      private :: cal_div_sgs_induct_true_pre
      private :: cal_div_sgs_s_flux_true_post
      private :: cal_div_sgs_tensor_true_post
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_true_sgs_terms_pre                                 &
     &         (dt, FEM_prm, SGS_par, mesh, sf_grp, fluid, conduct,     &
     &          fl_prop, cd_prop, ht_prop, cp_prop, nod_bcs, surf_bcs,  &
     &          iphys, iphys_LES, iphys_ele_base, ak_MHD, fem_int,      &
     &          FEM_elens, diff_coefs, mk_MHD, mhd_fem_wk, rhs_mat,     &
     &          nod_fld, ele_fld, v_sol, SR_sig, SR_r)
!
      use calypso_mpi
      use m_true_SGS_term_labels
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(base_field_address), intent(in) :: iphys_ele_base
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: i
!
       do i = 1, nod_fld%num_phys
         if ( nod_fld%phys_name(i) .eq. SGS_div_h_flux_true%name) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_s_flux_true_pre                             &
     &       (FEM_prm%iflag_temp_supg, FEM_prm%npoint_t_evo_int, dt,    &
     &        iphys_LES%true_div_SGS%i_SGS_h_flux,                      &
     &        iphys%forces%i_h_flux, iphys%div_forces%i_h_flux,         &
     &        iphys_LES%filter_fld%i_temp, iphys_LES%filter_fld%i_velo, &
     &        FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,       &
     &        ht_prop, nod_bcs%Tnod_bcs, iphys_ele_base, ele_fld,       &
     &        fem_int, mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld,   &
     &        v_sol, SR_sig, SR_r)
         else if(nod_fld%phys_name(i).eq.SGS_div_c_flux_true%name) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_s_flux_true_pre                             &
     &      (FEM_prm%iflag_comp_supg, FEM_prm%npoint_t_evo_int, dt,     &
     &       iphys_LES%true_div_SGS%i_SGS_c_flux,                       &
     &       iphys%forces%i_c_flux, iphys%div_forces%i_c_flux,          &
     &       iphys_LES%filter_fld%i_light, iphys_LES%filter_fld%i_velo, &
     &       FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,        &
     &       cp_prop, nod_bcs%Cnod_bcs, iphys_ele_base, ele_fld,        &
     &       fem_int, mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld,    &
     &       v_sol, SR_sig, SR_r)
         else if ( nod_fld%phys_name(i).eq.SGS_div_m_flux_true%name)    &
     &          then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_m_flux_true_pre(dt, FEM_prm, SGS_par,       &
     &        mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,            &
     &        sf_grp, fluid, fl_prop, cd_prop,                          &
     &        surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs,                       &
     &        iphys%base, iphys%forces, iphys%div_forces,               &
     &        iphys%diffusion, iphys_LES%filter_fld,                    &
     &        iphys_LES%force_by_filter, iphys_LES%SGS_term,            &
     &        iphys_LES%div_SGS, iphys_LES%true_div_SGS,                &
     &        iphys_ele_base, ak_MHD, fem_int, FEM_elens, diff_coefs,   &
     &        mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld, ele_fld,   &
     &        v_sol, SR_sig, SR_r)
         else if(nod_fld%phys_name(i) .eq. SGS_Lorentz_true%name) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_maxwell_true_pre(dt, FEM_prm, SGS_par,      &
     &         mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,           &
     &         sf_grp, fluid, fl_prop, cd_prop,                         &
     &         surf_bcs%Vsf_bcs, surf_bcs%Bsf_bcs,                      &
     &         iphys%base, iphys%forces, iphys%div_forces,              &
     &         iphys%diffusion, iphys_LES%filter_fld,                   &
     &         iphys_LES%force_by_filter, iphys_LES%SGS_term,           &
     &         iphys_LES%div_SGS, iphys_LES%true_SGS,                   &
     &         iphys_ele_base, ak_MHD, fem_int, FEM_elens, diff_coefs,  &
     &         mk_MHD%mlump_fl, mhd_fem_wk, rhs_mat, nod_fld, ele_fld,  &
     &         v_sol, SR_sig, SR_r)
         else if(nod_fld%phys_name(i) .eq. SGS_mag_induction_true%name) &
     &          then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_induct_true_pre(dt, FEM_prm, SGS_par,       &
     &         mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,           &
     &         sf_grp, conduct, cd_prop,                                &
     &         nod_bcs%Bnod_bcs, surf_bcs%Asf_bcs, surf_bcs%Bsf_bcs,    &
     &         iphys%base, iphys%forces, iphys%div_forces,              &
     &         iphys%diffusion, iphys_LES%filter_fld,                   &
     &         iphys_LES%SGS_term, iphys_LES%true_SGS,                  &
     &         iphys_ele_base, ele_fld, ak_MHD, fem_int, FEM_elens,     &
     &         diff_coefs%Cdiff_SGS_uxb%coef(1,1),                      &
     &         mk_MHD%mlump_cd, mhd_fem_wk, rhs_mat, nod_fld,           &
     &         v_sol, SR_sig, SR_r)
         end if
       end do
!
      end subroutine cal_true_sgs_terms_pre
!
!-----------------------------------------------------------------------
!
      subroutine cal_true_sgs_terms_post(filter_param, mesh,            &
     &          iphys_div_frc, iphys_trSGS, iphys_div_trSGS,            &
     &          iphys_SGS_wk, filtering, wk_filter, nod_fld,            &
     &          v_sol, SR_sig, SR_r)
!
      use t_SGS_term_labels
      use m_true_SGS_term_labels
!
      type(SGS_filtering_params), intent(in) :: filter_param
      type(mesh_geometry), intent(in) :: mesh
!
      type(base_force_address), intent(in) :: iphys_div_frc
      type(SGS_term_address), intent(in) :: iphys_trSGS
      type(SGS_term_address), intent(in) :: iphys_div_trSGS
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!
      type(filtering_data_type), intent(in) :: filtering
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: i
!
!
       do i = 1, nod_fld%num_phys
         if ( nod_fld%phys_name(i).eq.SGS_div_h_flux_true%name) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_s_flux_true_post                            &
     &        (iphys_div_trSGS%i_SGS_h_flux,                            &
     &         iphys_div_frc%i_h_flux, iphys_SGS_wk%i_simi,             &
     &         filter_param, mesh%nod_comm, mesh%node,                  &
     &         filtering, wk_filter, nod_fld, v_sol, SR_sig, SR_r)
         else if(nod_fld%phys_name(i).eq.SGS_div_c_flux_true%name) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_s_flux_true_post                            &
     &        (iphys_div_trSGS%i_SGS_c_flux,                            &
     &         iphys_div_frc%i_c_flux, iphys_SGS_wk%i_simi,             &
     &         filter_param, mesh%nod_comm, mesh%node,                  &
     &         filtering, wk_filter, nod_fld, v_sol, SR_sig, SR_r)
         else if ( nod_fld%phys_name(i).eq.SGS_div_m_flux_true%name)    &
     &          then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_tensor_true_post                            &
     &        (iphys_div_trSGS%i_SGS_m_flux,                            &
     &         iphys_div_frc%i_m_flux, iphys_SGS_wk%i_simi,             &
     &         filter_param, mesh%nod_comm, mesh%node,                  &
     &         filtering, wk_filter, nod_fld, v_sol, SR_sig, SR_r)
         else if(nod_fld%phys_name(i) .eq. SGS_Lorentz_true%name) then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_tensor_true_post                            &
     &        (iphys_trSGS%i_SGS_Lorentz,                               &
     &         iphys_div_frc%i_maxwell, iphys_SGS_wk%i_simi,            &
     &         filter_param, mesh%nod_comm, mesh%node,                  &
     &         filtering, wk_filter, nod_fld, v_sol, SR_sig, SR_r)
         else if(nod_fld%phys_name(i) .eq. SGS_mag_induction_true%name) &
     &          then
           if(iflag_debug.gt.0) write(*,*)                              &
     &                         'lead  ', trim(nod_fld%phys_name(i) )
           call cal_div_sgs_tensor_true_post                            &
     &        (iphys_trSGS%i_SGS_induction,                             &
     &         iphys_div_frc%i_induct_t, iphys_SGS_wk%i_simi,           &
     &         filter_param, mesh%nod_comm, mesh%node,                  &
     &         filtering, wk_filter, nod_fld, v_sol, SR_sig, SR_r)
         end if
       end do
!
      end subroutine cal_true_sgs_terms_post
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_s_flux_true_pre(iflag_supg, num_int, dt,   &
     &        i_div_flux_true, i_flux, i_div_flux, i_field_f, i_velo_f, &
     &        FEM_prm, nod_comm, node, ele, fluid, property, Snod_bcs,  &
     &        iphys_ele_base, ele_fld, fem_int, mlump_fl,               &
     &        mhd_fem_wk, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
!
      use t_bc_data_temp
      use t_surface_bc_data
      use products_nodal_fields_smp
      use cal_terms_for_heat
!
      integer(kind = kint), intent(in) :: iflag_supg, num_int
      integer(kind = kint), intent(in) :: i_div_flux_true
      integer(kind = kint), intent(in) :: i_flux, i_div_flux
      integer(kind = kint), intent(in) :: i_field_f, i_velo_f
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(finite_element_integration), intent(in) :: fem_int
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
!$omp parallel
      call cal_phys_scalar_product_vector                               &
     &   (i_velo_f, i_field_f, i_flux, nod_fld)
!$omp end parallel
      call cal_div_of_scalar_flux                                       &
     &   (i_div_flux, i_flux, iflag_supg, num_int, dt,                  &
     &    FEM_prm, nod_comm, node, ele, fluid, property, Snod_bcs,      &
     &    iphys_ele_base, ele_fld, fem_int, mlump_fl,                   &
     &    mhd_fem_wk, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
      call copy_scalar_component(nod_fld,                               &
     &    i_div_flux, i_div_flux_true)
!
      end subroutine cal_div_sgs_s_flux_true_pre
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_m_flux_true_pre(dt, FEM_prm, SGS_par,      &
     &          nod_comm, node, ele, surf, sf_grp, fluid,               &
     &          fl_prop, cd_prop, Vsf_bcs, Bsf_bcs,                     &
     &          iphys_base, iphys_frc, iphys_div_frc, iphys_dif,        &
     &          iphys_fil, iphys_fil_frc, iphys_SGS, iphys_div_SGS,     &
     &          iphys_tr_div_SGS, iphys_ele_base, ak_MHD, fem_int,      &
     &          FEM_elens, diff_coefs, mlump_fl, mhd_fem_wk, rhs_mat,   &
     &          nod_fld, ele_fld, v_sol, SR_sig, SR_r)
!
      use cal_momentum_terms
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_force_address), intent(in) :: iphys_frc
      type(base_force_address), intent(in) :: iphys_div_frc
      type(diffusion_address), intent(in) :: iphys_dif
      type(base_field_address), intent(in) :: iphys_fil
      type(base_force_address), intent(in) :: iphys_fil_frc
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(SGS_term_address), intent(in) :: iphys_div_SGS
      type(SGS_term_address), intent(in) :: iphys_tr_div_SGS
!
      type(base_field_address), intent(in) :: iphys_ele_base
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call cal_flux_tensor(iphys_fil%i_velo, iphys_fil%i_velo,          &
     &    iphys_frc%i_m_flux, nod_fld)
      call cal_terms_4_momentum(iphys_div_frc%i_m_flux, dt,             &
     &    FEM_prm, SGS_par%model_p, SGS_par%commute_p,                  &
     &    nod_comm, node, ele, surf, sf_grp, fluid, fl_prop, cd_prop,   &
     &    Vsf_bcs, Bsf_bcs, iphys_base, iphys_frc, iphys_div_frc,       &
     &    iphys_dif, iphys_fil, iphys_fil_frc,                          &
     &    iphys_SGS, iphys_div_SGS, iphys_ele_base, ak_MHD,             &
     &    fem_int, FEM_elens, diff_coefs, mlump_fl, mhd_fem_wk,         &
     &    rhs_mat, nod_fld, ele_fld, v_sol, SR_sig, SR_r)
      call copy_vector_component(nod_fld,                               &
     &    iphys_div_frc%i_m_flux, iphys_tr_div_SGS%i_SGS_m_flux)
!
      end subroutine cal_div_sgs_m_flux_true_pre
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_maxwell_true_pre                           &
     &        (dt, FEM_prm, SGS_par, nod_comm, node, ele, surf, sf_grp, &
     &         fluid, fl_prop, cd_prop, Vsf_bcs, Bsf_bcs,               &
     &         iphys_base, iphys_frc, iphys_div_frc, iphys_dif,         &
     &         iphys_fil, iphys_fil_frc, iphys_SGS, iphys_div_SGS,      &
     &         iphys_trSGS, iphys_ele_base, ak_MHD, fem_int,            &
     &         FEM_elens, diff_coefs, mlump_fl, mhd_fem_wk, rhs_mat,    &
     &         nod_fld, ele_fld, v_sol, SR_sig, SR_r)
!
      use cal_momentum_terms
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_force_address), intent(in) :: iphys_frc
      type(base_force_address), intent(in) :: iphys_div_frc
      type(diffusion_address), intent(in) :: iphys_dif
      type(base_field_address), intent(in) :: iphys_fil
      type(base_force_address), intent(in) :: iphys_fil_frc
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(SGS_term_address), intent(in) :: iphys_div_SGS
      type(SGS_term_address), intent(in) :: iphys_trSGS
!
      type(base_field_address), intent(in) :: iphys_ele_base
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call cal_maxwell_tensor(cd_prop%ex_magne,                         &
     &    iphys_fil%i_magne, iphys_frc%i_maxwell, nod_fld)
      call cal_terms_4_momentum(iphys_div_frc%i_maxwell, dt,            &
     &    FEM_prm, SGS_par%model_p, SGS_par%commute_p,                  &
     &    nod_comm, node, ele, surf, sf_grp, fluid, fl_prop, cd_prop,   &
     &    Vsf_bcs, Bsf_bcs, iphys_base, iphys_frc, iphys_div_frc,       &
     &    iphys_dif, iphys_fil, iphys_fil_frc, iphys_SGS,               &
     &    iphys_div_SGS, iphys_ele_base, ak_MHD, fem_int, FEM_elens,    &
     &    diff_coefs, mlump_fl, mhd_fem_wk, rhs_mat, nod_fld, ele_fld,  &
     &    v_sol, SR_sig, SR_r)
      call copy_vector_component(nod_fld,                               &
     &   iphys_div_frc%i_maxwell, iphys_trSGS%i_SGS_Lorentz)
!
      end subroutine cal_div_sgs_maxwell_true_pre
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_induct_true_pre(dt, FEM_prm, SGS_par,      &
     &          nod_comm, node, ele, surf, sf_grp, conduct, cd_prop,    &
     &          Bnod_bcs, Asf_bcs, Bsf_bcs, iphys_base,                 &
     &          iphys_frc, iphys_div_frc, iphys_dif, iphys_fil,         &
     &          iphys_SGS, iphys_trSGS, iphys_ele_base, ele_fld,        &
     &          ak_MHD, fem_int, FEM_elens, ak_diff, mlump_cd,          &
     &          mhd_fem_wk, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
!
      use t_bc_data_magne
      use t_surface_bc_data
      use cal_magnetic_terms
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_force_address), intent(in) :: iphys_frc
      type(base_force_address), intent(in) :: iphys_div_frc
      type(diffusion_address), intent(in) :: iphys_dif
      type(base_field_address), intent(in) :: iphys_fil
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(SGS_term_address), intent(in) :: iphys_trSGS
!
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(lumped_mass_matrices), intent(in) :: mlump_cd
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call cal_induction_tensor(iphys_fil%i_magne, iphys_fil%i_velo,    &
     &    iphys_frc%i_induct_t, nod_fld)
      call cal_terms_4_magnetic                                         &
     &   (iphys_div_frc%i_induct_t, ak_MHD%ak_d_magne,                  &
     &    dt, FEM_prm, SGS_par%model_p, SGS_par%commute_p,              &
     &    nod_comm, node, ele, surf, conduct, sf_grp, cd_prop,          &
     &    Bnod_bcs, Asf_bcs, Bsf_bcs, iphys_base, iphys_frc,            &
     &    iphys_div_frc, iphys_dif, iphys_SGS, iphys_ele_base,          &
     &    ele_fld, fem_int, FEM_elens, ak_diff, mlump_cd,               &
     &    mhd_fem_wk, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
      call copy_vector_component(nod_fld, iphys_div_frc%i_induct_t,     &
     &    iphys_trSGS%i_SGS_induction)
!
      end subroutine cal_div_sgs_induct_true_pre
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_s_flux_true_post                           &
     &          (i_div_flux_true, i_div_flux, i_sgs_simi,               &
     &           filter_param, nod_comm, node, filtering,               &
     &           wk_filter, nod_fld, v_sol, SR_sig, SR_r)
!
      integer(kind = kint), intent(in) :: i_div_flux_true
      integer(kind = kint), intent(in) :: i_div_flux, i_sgs_simi
!
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call copy_scalar_component(nod_fld,                               &
     &    i_div_flux_true, i_sgs_simi)
      call cal_filtered_scalar_whole                                    &
     &   (filter_param, nod_comm, node, filtering,                      &
     &    i_div_flux_true, i_div_flux, wk_filter, nod_fld,              &
     &    v_sol, SR_sig, SR_r)
      call subtract_2_nod_scalars(nod_fld,                              &
     &    i_div_flux_true, i_sgs_simi, i_div_flux_true)
!
      end subroutine cal_div_sgs_s_flux_true_post
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_tensor_true_post                           &
     &         (i_sgs_true, i_sgs_div, i_sgs_simi,                      &
     &          filter_param, nod_comm, node, filtering,                &
     &          wk_filter, nod_fld, v_sol, SR_sig, SR_r)
!
      integer(kind = kint), intent(in) :: i_sgs_true, i_sgs_div
      integer(kind = kint), intent(in) :: i_sgs_simi
!
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call copy_vector_component(nod_fld, i_sgs_true, i_sgs_simi)
      call cal_filtered_vector_whole                                    &
     &   (filter_param, nod_comm, node, filtering,                      &
     &    i_sgs_true, i_sgs_div, wk_filter, nod_fld,                    &
     &    v_sol, SR_sig, SR_r)
      call subtract_2_nod_vectors(nod_fld,                              &
     &    i_sgs_true, i_sgs_simi, i_sgs_true)
!
      end subroutine cal_div_sgs_tensor_true_post
!
!-----------------------------------------------------------------------
!
      end module cal_true_sgs_terms
