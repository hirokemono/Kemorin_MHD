!vector_gradients_4_monitor.f90
!     module vector_gradients_4_monitor
!
!     Written by H. Matsui
!
!!      subroutine vect_gradients_4_monitor(dt, FEM_prm,                &
!!     &          nod_comm, node, ele, fluid, iphys, iphys_ele, fem_int,&
!!     &          mk_MHD, rhs_mat, nod_fld, ele_fld)
!!      subroutine cal_work_4_forces                                    &
!!     &         (FEM_prm, nod_comm, node, ele, fl_prop, cd_prop, iphys,&
!!     &          fem_int, mk_MHD, mhd_fem_wk, rhs_mat, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(fluid_property),  intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!
      module vector_gradients_4_monitor
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_phys_labels
!
      use t_FEM_control_parameter
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_surface_bc_data_MHD
      use t_work_FEM_integration
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine vect_gradients_4_monitor(dt, FEM_prm,                  &
     &          nod_comm, node, ele, fluid, iphys, iphys_ele, fem_int,  &
     &          mk_MHD, rhs_mat, nod_fld, ele_fld)
!
      use cal_gradient
!
      real(kind = kreal), intent(in) :: dt
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(finite_element_integration), intent(in) :: fem_int
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
!
      integer(kind = kint) :: i, i_fld, i_src
!
!
      do i = 1, nod_fld%num_phys
        i_fld = nod_fld%istack_component(i-1) + 1
        if(     i_fld .eq. iphys%i_grad_vx                              &
     &     .or. i_fld .eq. iphys%i_grad_vy                              &
     &     .or. i_fld .eq. iphys%i_grad_vz) then
          if(i_fld .eq. iphys%i_grad_vx) i_src = iphys%i_velo
          if(i_fld .eq. iphys%i_grad_vy) i_src = iphys%i_velo + 1
          if(i_fld .eq. iphys%i_grad_vz) i_src = iphys%i_velo + 2
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &             write(*,*) 'lead  ', trim(nod_fld%phys_name(i))
          call choose_cal_gradient                                      &
     &       (FEM_prm%iflag_velo_supg, FEM_prm%npoint_t_evo_int, dt,    &
     &        i_src, i_fld, fluid%istack_ele_fld_smp,                   &
     &        mk_MHD%mlump_fl, nod_comm, node, ele, iphys_ele, ele_fld, &
     &        fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,   &
     &        rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
        end if
!
        if(     i_fld .eq. iphys%i_grad_wx                              &
     &     .or. i_fld .eq. iphys%i_grad_wy                              &
     &     .or. i_fld .eq. iphys%i_grad_wz) then
          if(i_fld .eq. iphys%i_grad_wx) i_src = iphys%i_vort
          if(i_fld .eq. iphys%i_grad_wy) i_src = iphys%i_vort + 1
          if(i_fld .eq. iphys%i_grad_wz) i_src = iphys%i_vort + 2
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &             write(*,*) 'lead  ', trim(nod_fld%phys_name(i))
          call choose_cal_gradient                                      &
     &       (FEM_prm%iflag_velo_supg, FEM_prm%npoint_t_evo_int, dt,    &
     &        i_src, i_fld, fluid%istack_ele_fld_smp,                   &
     &        mk_MHD%mlump_fl, nod_comm, node, ele, iphys_ele, ele_fld, &
     &        fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,   &
     &        rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
        end if
!
        if(     i_fld .eq. iphys%i_grad_ax                              &
     &     .or. i_fld .eq. iphys%i_grad_ay                              &
     &     .or. i_fld .eq. iphys%i_grad_az) then
          if(i_fld .eq. iphys%i_grad_ax) i_src = iphys%i_vecp
          if(i_fld .eq. iphys%i_grad_ay) i_src = iphys%i_vecp + 1
          if(i_fld .eq. iphys%i_grad_az) i_src = iphys%i_vecp + 2
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &             write(*,*) 'lead  ', trim(nod_fld%phys_name(i))
          call choose_cal_gradient                                      &
     &       (FEM_prm%iflag_velo_supg, FEM_prm%npoint_t_evo_int, dt,    &
     &        i_src, i_fld, fluid%istack_ele_fld_smp,                   &
     &        mk_MHD%mlump_fl, nod_comm, node, ele, iphys_ele, ele_fld, &
     &        fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,   &
     &        rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
        end if
!
        if(     i_fld .eq. iphys%i_grad_bx                              &
     &     .or. i_fld .eq. iphys%i_grad_by                              &
     &     .or. i_fld .eq. iphys%i_grad_bz) then
          if(i_fld .eq. iphys%i_grad_bx) i_src = iphys%i_magne
          if(i_fld .eq. iphys%i_grad_by) i_src = iphys%i_magne + 1
          if(i_fld .eq. iphys%i_grad_bz) i_src = iphys%i_magne + 2
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &             write(*,*) 'lead  ', trim(nod_fld%phys_name(i))
          call choose_cal_gradient                                      &
     &       (FEM_prm%iflag_velo_supg, FEM_prm%npoint_t_evo_int, dt,    &
     &        i_src, i_fld, fluid%istack_ele_fld_smp,                   &
     &        mk_MHD%mlump_fl, nod_comm, node, ele, iphys_ele, ele_fld, &
     &        fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,   &
     &        rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
        end if
!
        if(     i_fld .eq. iphys%i_grad_jx                              &
     &     .or. i_fld .eq. iphys%i_grad_jy                              &
     &     .or. i_fld .eq. iphys%i_grad_jz) then
          if(i_fld .eq. iphys%i_grad_jx) i_src = iphys%i_current
          if(i_fld .eq. iphys%i_grad_jy) i_src = iphys%i_current + 1
          if(i_fld .eq. iphys%i_grad_jz) i_src = iphys%i_current + 2
          if(iflag_debug .ge. iflag_routine_msg)                        &
     &             write(*,*) 'lead  ', trim(nod_fld%phys_name(i))
          call choose_cal_gradient                                      &
     &       (FEM_prm%iflag_velo_supg, FEM_prm%npoint_t_evo_int, dt,    &
     &        i_src, i_fld, fluid%istack_ele_fld_smp,                   &
     &        mk_MHD%mlump_fl, nod_comm, node, ele, iphys_ele, ele_fld, &
     &        fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,   &
     &        rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
        end if
!
      end do
!
      end subroutine vect_gradients_4_monitor
!
!-----------------------------------------------------------------------
!
      subroutine cal_work_4_forces                                      &
     &         (FEM_prm, nod_comm, node, ele, fl_prop, cd_prop, iphys,  &
     &          fem_int, mk_MHD, mhd_fem_wk, rhs_mat, nod_fld)
!
      use buoyancy_flux
      use products_nodal_fields_smp
      use copy_nodal_fields
      use int_magne_diffusion
      use int_magne_induction
      use nodal_poynting_flux_smp
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(finite_element_integration), intent(in) :: fem_int
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iphys%i_induction .gt. izero                                  &
     &      .and. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_induct)
        call s_int_magne_induction                                      &
     &     (FEM_prm%npoint_poisson_int, nod_comm, node, ele, iphys,     &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, fem_int%rhs_tbl,     &
     &      mk_MHD%mlump_cd, mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_nl,  &
     &     nod_fld)
      end if
!
      if (iphys%i_b_diffuse .gt. izero                                  &
     &      .and. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_diffuse)
        call s_int_magne_diffusion                                      &
     &     (FEM_prm%npoint_poisson_int, nod_comm, node, ele, iphys,     &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d,                      &
     &      fem_int%rhs_tbl, mk_MHD%mlump_cd, mhd_fem_wk,               &
     &      rhs_mat%fem_wk, rhs_mat%f_nl, nod_fld)
      end if
!
!$omp parallel
      if (iphys%i_electric .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_e_field)
        call cal_nod_electric_field_smp(node, cd_prop%coef_diffuse,     &
     &      nod_fld%ntot_phys, iphys%i_current, iphys%i_vp_induct,      &
     &      iphys%i_electric, nod_fld%d_fld)
      end if
!
      if (iphys%i_ujb .gt. izero) then
        call cal_tri_product_4_scalar                                   &
     &     (iphys%i_velo, iphys%i_current, iphys%i_magne, iphys%i_ujb,  &
     &      fl_prop%coef_lor, nod_fld)
      end if
!
      if (iphys%i_nega_ujb .gt. izero) then
        call cal_tri_product_4_scalar                                   &
     &     (iphys%i_velo, iphys%i_magne, iphys%i_current,               &
     &      iphys%i_nega_ujb, fl_prop%coef_lor, nod_fld)
      end if
!
      if (iphys%i_me_gen .gt. izero) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_induction, iphys%i_magne, iphys%i_me_gen, nod_fld)
      end if
!$omp end parallel
!
!
!
!
      if (iphys%i_buo_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_buoyancy_flux)
        call cal_gravity_flux(node,                                     &
     &      fl_prop%i_grav, fl_prop%coef_buo, fl_prop%grav,             &
     &      iphys%i_velo,  iphys%i_temp, iphys%i_buo_gen, nod_fld)
      end if
!
      if (iphys%i_c_buo_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_comp_buo_flux)
        call cal_gravity_flux(node,                                     &
     &      fl_prop%i_grav, fl_prop%coef_comp_buo, fl_prop%grav,        &
     &      iphys%i_velo, iphys%i_light,  iphys%i_c_buo_gen, nod_fld)
      end if
!
      if (iphys%i_f_buo_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_filter_buo_flux)
        call cal_gravity_flux(node,                                     &
     &      fl_prop%i_grav, fl_prop%coef_buo, fl_prop%grav,             &
     &      iphys%i_velo, iphys%i_filter_temp, iphys%i_f_buo_gen,       &
     &      nod_fld)
      end if
!
!
!$omp parallel
      if (iphys%i_temp_gen .gt. izero) then
        call cal_phys_product_4_scalar                                  &
     &     (iphys%i_h_advect, iphys%i_temp, iphys%i_temp_gen, nod_fld)
      end if
!
      if (iphys%i_par_t_gen .gt. izero) then
        call cal_phys_product_4_scalar                                  &
     &     (iphys%i_ph_advect, iphys%i_par_temp, iphys%i_par_t_gen,     &
     &      nod_fld)
      end if
!
      if (iphys%i_par_c_gen .gt. izero) then
        call cal_phys_product_4_scalar                                  &
     &     (iphys%i_pc_advect, iphys%i_par_light, iphys%i_par_c_gen,    &
     &      nod_fld)
      end if
!
      if (iphys%i_vis_e_diffuse .gt. izero) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_velo, iphys%i_v_diffuse, iphys%i_vis_e_diffuse,     &
     &      nod_fld)
      end if
!
      if (iphys%i_mag_e_diffuse .gt. izero) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_magne, iphys%i_b_diffuse, iphys%i_mag_e_diffuse,    &
     &      nod_fld)
      end if
!
      if (iphys%i_m_tension_wk .gt. izero) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_electric, iphys%i_magne, iphys%i_m_tension_wk,      &
     &      nod_fld)
      end if
!
      if (iphys%i_mag_stretch .gt. izero) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_grad_vx, iphys%i_magne, iphys%i_mag_stretch,        &
     &      nod_fld)
        call cal_phys_dot_product                                       &
     &     (iphys%i_grad_vy, iphys%i_magne, (iphys%i_mag_stretch+1),    &
     &      nod_fld)
        call cal_phys_dot_product                                       &
     &     (iphys%i_grad_vz, iphys%i_magne, (iphys%i_mag_stretch+2),    &
     &      nod_fld)
      end if
!
      if (iphys%i_poynting .gt. izero) then
        call cal_nod_poynting_flux_smp(node, cd_prop%coef_diffuse,      &
     &      nod_fld%ntot_phys, iphys%i_current, iphys%i_vp_induct,      &
     &      iphys%i_magne, iphys%i_poynting, nod_fld%d_fld)
      end if
!$omp end parallel
!
!
      end subroutine cal_work_4_forces
!
!-----------------------------------------------------------------------
!
      end module vector_gradients_4_monitor
