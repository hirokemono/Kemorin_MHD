!
!      module int_bulk
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!!      subroutine s_int_mean_squares(npoint_integrate,                 &
!!     &          mesh, fluid, conduct, iphys, iphys_LES, nod_fld,      &
!!     &          jacs, i_msq, msq_list, fem_wk, mhd_fem_wk, fem_msq)
!!      subroutine int_no_evo_mean_squares(i_step, dt, mesh,            &
!!     &          fl_prop, cd_prop, iphys, iphys_LES, nod_fld,          &
!!     &          iphys_ele_base, ele_fld, fluid, jacs, i_msq,          &
!!     &          fem_wk, fem_msq)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(phys_data), intent(in) :: nod_fld
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(mean_square_list), intent(in) :: msq_list
!!        type(mean_square_address), intent(in) :: i_msq
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(mean_square_values), intent(inout) :: fem_msq
!
      module int_bulk
!
      use m_precision
      use m_phys_constants
!
      use t_physical_property
      use t_geometry_data_MHD
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_SGS_model_addresses
      use t_jacobians
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_mean_square_filed_list
      use t_mean_square_values
!
      implicit none
!
! ----------------------------------------------------------------------
! 
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_int_mean_squares(npoint_integrate,                   &
     &          mesh, fluid, conduct, iphys, iphys_LES, nod_fld,        &
     &          jacs, i_msq, msq_list, fem_wk, mhd_fem_wk, fem_msq)
!
      use m_constants
      use calypso_mpi
      use int_all_energy
      use int_all_ave_tensors
!
      integer(kind = kint), intent(in) :: npoint_integrate
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
      type(mean_square_address), intent(in) :: i_msq
      type(mean_square_list), intent(in) :: msq_list
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(mean_square_values), intent(inout) :: fem_msq
!
      integer(kind = kint) :: i
!
! ---------  initialize
!
      fem_msq%ave_local(1:fem_msq%num_ave) = 0.0d0
      fem_msq%rms_local(1:fem_msq%num_rms-1)  = 0.0d0
!
! ----- lead average in a element -------------
!
      do i = 1, msq_list%nfield
        if     (msq_list%ifld_msq(i) .eq. iphys%base%i_velo) then
          call int_all_4_vector                                         &
     &       (fluid%istack_ele_fld_smp, npoint_integrate,               &
     &        msq_list%irms_msq(i), msq_list%jave_msq(i),               &
     &        msq_list%ifld_msq(i), mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
          call int_all_angular_mom(fluid%istack_ele_fld_smp,            &
     &        npoint_integrate, i_msq%ja_amom, iphys%base%i_velo,       &
     &        mesh, nod_fld, jacs, mhd_fem_wk, fem_wk, fem_msq)
!
        else if(msq_list%ifld_msq(i) .eq. iphys%base%i_magne) then
          call int_all_4_vector                                         &
     &       (fluid%istack_ele_fld_smp, npoint_integrate,               &
     &        msq_list%irms_msq(i), msq_list%jave_msq(i),               &
     &        msq_list%ifld_msq(i), mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
          call int_all_4_vector                                         &
     &      (conduct%istack_ele_fld_smp, npoint_integrate,              &
     &       i_msq%ir_me_ic, i_msq%ja_mag_ic, iphys%base%i_magne,       &
     &       mesh, nod_fld, jacs, fem_wk, fem_msq)
!
        else if(msq_list%ifld_msq(i) .eq. iphys%base%i_current) then
          call int_all_4_vector                                         &
     &       (fluid%istack_ele_fld_smp, npoint_integrate,               &
     &        msq_list%irms_msq(i), msq_list%jave_msq(i),               &
     &        msq_list%ifld_msq(i), mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
          call int_all_4_vector                                         &
     &       (conduct%istack_ele_fld_smp, npoint_integrate,             &
     &        i_msq%ir_sqj_ic, i_msq%ja_j_ic,                           &
     &        iphys%base%i_current, mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
!
        else if(msq_list%ifld_msq(i) .eq. iphys%base%i_mag_p) then
          call int_all_4_scalar                                         &
     &       (mesh%ele%istack_ele_smp, npoint_integrate,                &
     &        msq_list%irms_msq(i), msq_list%jave_msq(i),               &
     &        msq_list%ifld_msq(i), mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
!
        else if(msq_list%ifld_msq(i)                                    &
     &             .eq. iphys_LES%filter_fld%i_velo) then
          call int_all_4_vector                                         &
     &       (fluid%istack_ele_fld_smp, npoint_integrate,               &
     &        msq_list%irms_msq(i), msq_list%jave_msq(i),               &
     &        msq_list%ifld_msq(i), mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
          call int_all_angular_mom                                      &
     &       (fluid%istack_ele_fld_smp, npoint_integrate,               &
     &        i_msq%jr_amom_f, iphys_LES%filter_fld%i_velo,             &
     &        mesh, nod_fld, jacs, mhd_fem_wk, fem_wk, fem_msq)
!
        else if(msq_list%ifld_msq(i)                                    &
     &           .eq. iphys_LES%filter_fld%i_magne) then
          call int_all_4_vector                                         &
     &       (fluid%istack_ele_fld_smp, npoint_integrate,               &
     &        msq_list%irms_msq(i), msq_list%jave_msq(i),               &
     &        msq_list%ifld_msq(i), mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
          call int_all_4_vector                                         &
     &       (conduct%istack_ele_fld_smp, npoint_integrate,             &
     &        i_msq%ir_me_f_ic, i_msq%ja_mag_f_ic,                      &
     &        iphys_LES%filter_fld%i_magne,                             &
     &        mesh, nod_fld, jacs, fem_wk, fem_msq)
!
        else if(msq_list%ifld_msq(i) .eq. iphys%forces%i_induct_t       &
     &     .or. msq_list%ifld_msq(i)                                    &
     &           .eq. iphys_LES%SGS_term%i_SGS_induct_t) then
           call int_all_4_asym_tensor                                   &
     &        (conduct%istack_ele_fld_smp, npoint_integrate,            &
     &        msq_list%irms_msq(i), msq_list%jave_msq(i),               &
     &        msq_list%ifld_msq(i), mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
!
        else if(msq_list%ncomp_msq(i) .eq. n_scalar) then
          call int_all_4_scalar                                         &
     &       (fluid%istack_ele_fld_smp, npoint_integrate,               &
     &        msq_list%irms_msq(i), msq_list%jave_msq(i),               &
     &        msq_list%ifld_msq(i), mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
        else if(msq_list%ncomp_msq(i) .eq. n_vector) then
          call int_all_4_vector                                         &
     &       (fluid%istack_ele_fld_smp, npoint_integrate,               &
     &        msq_list%irms_msq(i), msq_list%jave_msq(i),               &
     &        msq_list%ifld_msq(i), mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
        else if(msq_list%ncomp_msq(i) .eq. n_sym_tensor) then
          call int_all_4_sym_tensor                                     &
     &       (fluid%istack_ele_fld_smp, npoint_integrate,               &
     &        msq_list%irms_msq(i), msq_list%jave_msq(i),               &
     &        msq_list%ifld_msq(i), mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
        end if
      end do
!
!       Adjust results
!
      do i = 1, fem_msq%num_rms
        if(    i .eq. i_msq%imsq_velo                                   &
     &    .or. i .eq. i_msq%imsq_magne                                  &
     &    .or. i .eq. i_msq%ir_me_ic                                    &
     &    .or. i .eq. i_msq%imsq_fil_velo                               &
     &    .or. i .eq. i_msq%imsq_fil_magne                              &
     &    .or. i .eq. i_msq%ir_me_f_ic                                  &
     &   ) then
          fem_msq%rms_local(i) = half * fem_msq%rms_local(i)
        end if
      end do
!
      if(i_msq%ir_rms_w .gt. 0) then
        fem_msq%rms_local(i_msq%ir_rms_w)                               &
     &      = fem_msq%rms_local(i_msq%imsq_vort)
      end if
      if(i_msq%ir_rms_j .gt. 0) then
        fem_msq%rms_local(i_msq%ir_rms_j)                               &
     &      =  fem_msq%rms_local(i_msq%imsq_current)
      end if
      if(i_msq%ir_rms_j_ic .gt. 0) then
        fem_msq%rms_local(i_msq%ir_rms_j_ic)                            &
     &      = fem_msq%rms_local(i_msq%ir_sqj_ic)
      end if
!
      end subroutine s_int_mean_squares
!
! ----------------------------------------------------------------------
!
      subroutine int_no_evo_mean_squares(i_step, dt, mesh,              &
     &          fl_prop, cd_prop, iphys, iphys_LES, nod_fld,            &
     &          iphys_ele_base, ele_fld, fluid, jacs, i_msq,            &
     &          fem_wk, fem_msq)
!
      use int_norm_div_MHD
      use int_rms_div_MHD
      use estimate_stabilities
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: dt
!
      type(mesh_geometry), intent(in) :: mesh
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(phys_data), intent(in) :: nod_fld
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacs
      type(field_geometry_data), intent(in) :: fluid
      type(mean_square_address), intent(in) :: i_msq
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(mean_square_values), intent(inout) :: fem_msq
!
!
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        call int_norm_divergence                                        &
     &     (fluid%istack_ele_fld_smp, iphys%base%i_velo,                &
     &      mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,      &
     &      fem_wk, fem_msq%ave_local(i_msq%jave_div_v))
        call int_rms_divergence                                         &
     &     (fluid%istack_ele_fld_smp, iphys%base%i_velo,                &
     &      mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,      &
     &      fem_wk, fem_msq%rms_local(i_msq%imsq_div_v))
        call cal_stability_4_advect(i_step, dt, mesh%ele, fluid,        &
     &      ele_fld%ntot_phys, iphys_ele_base%i_velo, ele_fld%d_fld)
      end if
!
      if  (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call int_norm_divergence                                        &
     &     (mesh%ele%istack_ele_smp, iphys%base%i_vecp,                 &
     &      mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,      &
     &      fem_wk, fem_msq%ave_local(i_msq%jave_div_a))
        call int_rms_divergence                                         &
     &     (mesh%ele%istack_ele_smp, iphys%base%i_vecp,                 &
     &      mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,      &
     &      fem_wk, fem_msq%rms_local(i_msq%imsq_div_a))
      end if
!
      if      (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution           &
     &    .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call int_norm_divergence                                        &
     &    (mesh%ele%istack_ele_smp, iphys%base%i_magne,                 &
     &     mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,       &
     &     fem_wk, fem_msq%ave_local(i_msq%jave_div_b))
        call int_rms_divergence                                         &
     &    (mesh%ele%istack_ele_smp, iphys%base%i_magne,                 &
     &     mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,       &
     &     fem_wk, fem_msq%rms_local(i_msq%imsq_div_b))
      end if
!
      if(iphys_LES%filter_fld%i_velo .gt. 0) then
        call int_norm_divergence                                        &
     &    (fluid%istack_ele_fld_smp, iphys_LES%filter_fld%i_velo,       &
     &     mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,       &
     &     fem_wk, fem_msq%ave_local(i_msq%jave_div_fil_v))
        call int_rms_divergence                                         &
     &    (fluid%istack_ele_fld_smp, iphys_LES%filter_fld%i_velo,       &
     &     mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,       &
     &     fem_wk, fem_msq%rms_local(i_msq%imsq_div_fil_v))
      end if
!
      if(iphys_LES%filter_fld%i_magne .gt. 0) then
        call int_norm_divergence                                        &
     &    (fluid%istack_ele_fld_smp, iphys_LES%filter_fld%i_magne,      &
     &     mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,       &
     &     fem_wk, fem_msq%ave_local(i_msq%jave_div_fil_b))
        call int_rms_divergence                                         &
     &    (fluid%istack_ele_fld_smp, iphys_LES%filter_fld%i_magne,      &
     &     mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,       &
     &     fem_wk, fem_msq%rms_local(i_msq%imsq_div_fil_b))
      end if
!
      if(iphys_LES%filter_fld%i_vecp .gt. 0) then
        call int_norm_divergence                                        &
     &    (fluid%istack_ele_fld_smp, iphys_LES%filter_fld%i_vecp,       &
     &     mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,       &
     &     fem_wk, fem_msq%ave_local(i_msq%jave_div_fil_a))
        call int_rms_divergence                                         &
     &    (fluid%istack_ele_fld_smp, iphys_LES%filter_fld%i_vecp,       &
     &     mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,       &
     &     fem_wk, fem_msq%rms_local(i_msq%imsq_div_fil_a))
      end if
!
      end subroutine int_no_evo_mean_squares
!
! ----------------------------------------------------------------------
!
      end module int_bulk
