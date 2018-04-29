!
!      module int_bulk
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!!      subroutine s_int_mean_squares(npoint_integrate,                 &
!!     &          mesh, fluid, conduct, iphys, nod_fld, jacs,           &
!!     &          i_rms, j_ave, ifld_msq, msq_list,                     &
!!     &          fem_wk, mhd_fem_wk, fem_msq)
!!      subroutine int_no_evo_mean_squares(i_step, dt, mesh,            &
!!     &          fl_prop, cd_prop, iphys, nod_fld, iphys_ele, ele_fld, &
!!     &          fluid, jacs, i_rms, j_ave, fem_wk, fem_msq)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(phys_address), intent(in) :: i_rms, j_ave
!!        type(mean_square_list), intent(in) :: msq_list
!!        type(mean_square_address), intent(in) :: ifld_msq
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
      use t_jacobians
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_FEM_MHD_mean_square
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
     &          mesh, fluid, conduct, iphys, nod_fld, jacs,             &
     &          i_rms, j_ave, ifld_msq, msq_list,                       &
     &          fem_wk, mhd_fem_wk, fem_msq)
!
      use m_constants
      use calypso_mpi
      use m_phys_labels
      use int_all_energy
      use int_all_ave_tensors
!
      integer(kind = kint), intent(in) :: npoint_integrate
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
      type(phys_address), intent(in) :: i_rms, j_ave
      type(mean_square_address), intent(in) :: ifld_msq
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
        if     (msq_list%ifld_msq(i) .eq. iphys%i_velo) then
          call int_all_4_vector                                         &
     &       (fluid%istack_ele_fld_smp, npoint_integrate,               &
     &        msq_list%irms_msq(i), msq_list%jave_msq(i),               &
     &        msq_list%ifld_msq(i), mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
          call int_all_angular_mom(fluid%istack_ele_fld_smp,            &
     &        npoint_integrate, ifld_msq%ja_amom, iphys%i_velo,         &
     &        mesh, nod_fld, jacs, mhd_fem_wk, fem_wk, fem_msq)
!
        else if(msq_list%ifld_msq(i) .eq. iphys%i_magne) then
          call int_all_4_vector                                         &
     &       (fluid%istack_ele_fld_smp, npoint_integrate,               &
     &        msq_list%irms_msq(i), msq_list%jave_msq(i),               &
     &        msq_list%ifld_msq(i), mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
          call int_all_4_vector                                         &
     &       (conduct%istack_ele_fld_smp, npoint_integrate,             &
     &        ifld_msq%ir_me_ic, ifld_msq%ja_mag_ic, iphys%i_magne,     &
     &        mesh, nod_fld, jacs, fem_wk, fem_msq)
!
        else if(msq_list%ifld_msq(i) .eq. iphys%i_current) then
          call int_all_4_vector                                         &
     &       (fluid%istack_ele_fld_smp, npoint_integrate,               &
     &        msq_list%irms_msq(i), msq_list%jave_msq(i),               &
     &        msq_list%ifld_msq(i), mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
          call int_all_4_vector                                         &
     &       (conduct%istack_ele_fld_smp, npoint_integrate,             &
     &        ifld_msq%ir_sqj_ic, ifld_msq%ja_j_ic, iphys%i_current,    &
     &        mesh, nod_fld, jacs, fem_wk, fem_msq)
!
        else if(msq_list%ifld_msq(i) .eq. iphys%i_filter_velo) then
          call int_all_4_vector                                         &
     &       (fluid%istack_ele_fld_smp, npoint_integrate,               &
     &        msq_list%irms_msq(i), msq_list%jave_msq(i),               &
     &        msq_list%ifld_msq(i), mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
          call int_all_angular_mom(fluid%istack_ele_fld_smp,            &
     &       npoint_integrate, ifld_msq%jr_amom_f, iphys%i_filter_velo, &
     &       mesh, nod_fld, jacs, mhd_fem_wk, fem_wk, fem_msq)
!
        else if(msq_list%ifld_msq(i) .eq. iphys%i_filter_magne) then
          call int_all_4_vector                                         &
     &       (fluid%istack_ele_fld_smp, npoint_integrate,               &
     &        msq_list%irms_msq(i), msq_list%jave_msq(i),               &
     &        msq_list%ifld_msq(i), mesh, nod_fld, jacs,                &
     &        fem_wk, fem_msq)
          call int_all_4_vector                                         &
     &       (conduct%istack_ele_fld_smp, npoint_integrate,             &
     &        ifld_msq%ir_me_f_ic, ifld_msq%ja_mag_f_ic,                &
     &        iphys%i_filter_magne,                                     &
     &        mesh, nod_fld, jacs, fem_wk, fem_msq)
!
        else if(msq_list%ifld_msq(i) .eq. iphys%i_induct_t              &
     &    .or. msq_list%ifld_msq(i) .eq. iphys%i_SGS_induct_t) then
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
      if(i_rms%i_velo .gt. 0) then
        fem_msq%rms_local(i_rms%i_velo)                                 &
     &      = half * fem_msq%rms_local(i_rms%i_velo)
      end if
      if(i_rms%i_magne .gt. 0) then
        fem_msq%rms_local(i_rms%i_magne)                                &
     &      = half * fem_msq%rms_local(i_rms%i_magne)
      end if
      if(ifld_msq%ir_me_ic .gt. 0) then
        fem_msq%rms_local(ifld_msq%ir_me_ic)                            &
     &      = half * fem_msq%rms_local(ifld_msq%ir_me_ic)
      end if
!
      if(i_rms%i_filter_velo .gt. 0) then
        fem_msq%rms_local(i_rms%i_filter_velo)                          &
     &      = half * fem_msq%rms_local(i_rms%i_filter_velo)
      end if
      if(i_rms%i_filter_magne .gt. 0) then
        fem_msq%rms_local(i_rms%i_filter_magne   )                      &
     &      = half * fem_msq%rms_local(i_rms%i_filter_magne   )
      end if
      if(ifld_msq%ir_me_f_ic .gt. 0) then
        fem_msq%rms_local(ifld_msq%ir_me_f_ic)                          &
     &      = half * fem_msq%rms_local(ifld_msq%ir_me_f_ic)
      end if
!
      if(ifld_msq%ir_rms_w .gt. 0) then
        fem_msq%rms_local(ifld_msq%ir_rms_w)                            &
     &      = fem_msq%rms_local(i_rms%i_vort)
      end if
      if(ifld_msq%ir_rms_j .gt. 0) then
        fem_msq%rms_local(ifld_msq%ir_rms_j)                            &
     &      =  fem_msq%rms_local(i_rms%i_current)
      end if
      if(ifld_msq%ir_rms_j_ic .gt. 0) then
        fem_msq%rms_local(ifld_msq%ir_rms_j_ic)                         &
     &      = fem_msq%rms_local(ifld_msq%ir_sqj_ic)
      end if
!
      end subroutine s_int_mean_squares
!
! ----------------------------------------------------------------------
!
      subroutine int_no_evo_mean_squares(i_step, dt, mesh,              &
     &          fl_prop, cd_prop, iphys, nod_fld, iphys_ele, ele_fld,   &
     &          fluid, jacs, i_rms, j_ave, fem_wk, fem_msq)
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
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacs
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: i_rms, j_ave
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(mean_square_values), intent(inout) :: fem_msq
!
!
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        call int_norm_divergence                                        &
     &    (fluid%istack_ele_fld_smp, iphys%i_velo, mesh%node, mesh%ele, &
     &     nod_fld, jacs%g_FEM, jacs%jac_3d, fem_wk,                    &
     &     fem_msq%ave_local(j_ave%i_div_v))
        call int_rms_divergence                                         &
     &    (fluid%istack_ele_fld_smp, iphys%i_velo, mesh%node, mesh%ele, &
     &     nod_fld, jacs%g_FEM, jacs%jac_3d, fem_wk,                    &
     &     fem_msq%rms_local(i_rms%i_div_v))
        call cal_stability_4_advect(i_step, dt, mesh%ele, fluid,        &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld)
      end if
!
      if  (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call int_norm_divergence                                        &
     &     (mesh%ele%istack_ele_smp, iphys%i_vecp, mesh%node, mesh%ele, &
     &      nod_fld, jacs%g_FEM, jacs%jac_3d, fem_wk,                   &
     &      fem_msq%ave_local(j_ave%i_div_a))
        call int_rms_divergence                                         &
     &     (mesh%ele%istack_ele_smp, iphys%i_vecp, mesh%node, mesh%ele, &
     &      nod_fld, jacs%g_FEM, jacs%jac_3d, fem_wk,                   &
     &      fem_msq%rms_local(i_rms%i_div_a))
      end if
!
      if      (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution           &
     &    .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call int_norm_divergence                                        &
     &    (mesh%ele%istack_ele_smp, iphys%i_magne, mesh%node, mesh%ele, &
     &     nod_fld, jacs%g_FEM, jacs%jac_3d, fem_wk,                    &
     &     fem_msq%ave_local(j_ave%i_div_b))
        call int_rms_divergence                                         &
     &    (mesh%ele%istack_ele_smp, iphys%i_magne, mesh%node, mesh%ele, &
     &     nod_fld, jacs%g_FEM, jacs%jac_3d, fem_wk,                    &
     &     fem_msq%rms_local(i_rms%i_div_b))
      end if
!
      if(iphys%i_filter_velo .gt. 0) then
        call int_norm_divergence                                        &
     &    (fluid%istack_ele_fld_smp, iphys%i_filter_velo,               &
     &     mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,       &
     &     fem_wk, fem_msq%ave_local(j_ave%i_div_filter_v))
        call int_rms_divergence                                         &
     &    (fluid%istack_ele_fld_smp, iphys%i_filter_velo,               &
     &     mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,       &
     &     fem_wk, fem_msq%rms_local(i_rms%i_div_filter_v))
      end if
!
      if(iphys%i_filter_magne .gt. 0) then
        call int_norm_divergence                                        &
     &    (fluid%istack_ele_fld_smp, iphys%i_filter_magne,              &
     &     mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,       &
     &     fem_wk, fem_msq%ave_local(j_ave%i_div_filter_b))
        call int_rms_divergence                                         &
     &    (fluid%istack_ele_fld_smp, iphys%i_filter_magne,              &
     &     mesh%node, mesh%ele, nod_fld, jacs%g_FEM, jacs%jac_3d,       &
     &     fem_wk, fem_msq%rms_local(i_rms%i_div_filter_b))
      end if
!
      end subroutine int_no_evo_mean_squares
!
! ----------------------------------------------------------------------
!
      end module int_bulk
