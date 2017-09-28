!check_deltat_by_prev_rms.f90
!     module check_deltat_by_prev_rms
!
!      Written by H. Matsui on Nov., 2009
!
!!      subroutine s_check_deltat_by_prev_rms                           &
!!     &         (MHD_step, mesh, MHD_mesh, MHD_prop, iphys, nod_fld,   &
!!     &          fem_int, rhs_mat, flex_MHD)
!!        type(MHD_step_param), intent(in) :: MHD_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(FEM_MHD_time_stepping), intent(inout) :: flex_MHD
!!
!!      subroutine check_difference_by_prev_rms                         &
!!     &         (time, node, ele, fluid, cd_prop, iphys, nod_fld,      &
!!     &          g_FEM, jac_3d_q, jac_3d_l, fem_wk, flex_data)
!!      subroutine set_ele_rms_4_previous_step                          &
!!     &         (time_d, mesh, fluid, iphys, nod_fld,                  &
!!     &          jacs, fem_wk, flex_data)
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(flexible_stepping_data), intent(inout) :: flex_data
!
      module check_deltat_by_prev_rms
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      use t_time_data
      use t_control_parameter
      use t_physical_property
      use t_mesh_data
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_work_FEM_integration
      use t_FEM_MHD_time_stepping
      use t_MHD_step_parameter
!
      use int_all_energy
!
      implicit  none
!
      private :: set_ele_rms_4_previous_step
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_check_deltat_by_prev_rms                             &
     &         (MHD_step, mesh, MHD_mesh, MHD_prop, iphys, nod_fld,     &
     &          fem_int, rhs_mat, flex_MHD)
!
      type(MHD_step_param), intent(in) :: MHD_step
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(finite_element_integration), intent(in) :: fem_int
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(FEM_MHD_time_stepping), intent(inout) :: flex_MHD
!
!
      if(MHD_step%flex_p%iflag_flexible_step .ne. iflag_flex_step)      &
     &   return
!
      call set_ele_rms_4_previous_step                                  &
     &   (MHD_step%time_d, mesh, MHD_mesh%fluid, iphys, nod_fld,        &
     &    fem_int%jcs, rhs_mat%fem_wk, flex_MHD%flex_data)
      call check_difference_by_prev_rms(MHD_step%time_d%time,           &
     &     mesh, MHD_mesh%fluid, MHD_prop%cd_prop, iphys, nod_fld,      &
     &    fem_int%jcs, rhs_mat%fem_wk, flex_MHD%flex_data)
!
      end subroutine s_check_deltat_by_prev_rms
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_difference_by_prev_rms                           &
     &         (time, mesh, fluid, cd_prop, iphys, nod_fld,             &
     &          jacs, fem_wk, flex_data)
!
      real(kind = kreal), intent(in) :: time
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(flexible_stepping_data), intent(inout) :: flex_data
!
!
      integer(kind = kint) :: i, imax
      real(kind = kreal) :: delta1, delta2
!
!
      do i = 0, flex_data%ntot_comp
        flex_data%rms_dt_pre2(i) = flex_data%rms_dt_pre1(i)
        flex_data%rms_dt_pre1(i) = flex_data%rms_dt_global(i)
      end do
      flex_data%rms_dt_global(0) = time
!
      if(flex_data%i_drmax_v .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_velo  ),           &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_v  ),              &
     &      flex_data%ave_dt_local(flex_data%i_drmax_v  ))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_velo+1),           &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_v+1),              &
     &      flex_data%ave_dt_local(flex_data%i_drmax_v+1))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_velo+2),           &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_v+2),              &
     &      flex_data%ave_dt_local(flex_data%i_drmax_v+2))
      end if
!
      if(flex_data%i_drmax_p .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_press,              &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_p),                &
     &      flex_data%ave_dt_local(flex_data%i_drmax_p))
      end if
!
!
      if((flex_data%i_drmax_b * cd_prop%iflag_Aevo_scheme) .gt. izero)  &
     & then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_vecp  ),           &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_b  ),              &
     &      flex_data%ave_dt_local(flex_data%i_drmax_b  ))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_vecp+1),           &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_b+1),              &
     &      flex_data%ave_dt_local(flex_data%i_drmax_b+1))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_vecp+2),           &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_b+2),              &
     &      flex_data%ave_dt_local(flex_data%i_drmax_b+2))
      else if(flex_data%i_drmax_b .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_magne  ),          &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_b  ),              &
     &      flex_data%ave_dt_local(flex_data%i_drmax_b  ))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_magne+1),          &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_b+1),              &
     &      flex_data%ave_dt_local(flex_data%i_drmax_b+1))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_magne+2),          &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_b+2),              &
     &      flex_data%ave_dt_local(flex_data%i_drmax_b+2))
      end if
!
      if(flex_data%i_drmax_f .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_mag_p,              &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_f),                &
     &      flex_data%ave_dt_local(flex_data%i_drmax_f))
      end if
!
!
      if(flex_data%i_drmax_t .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_temp,               &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_t),                &
     &      flex_data%ave_dt_local(flex_data%i_drmax_t))
      end if
!
      if(flex_data%i_drmax_d .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_light,              &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_d),                &
     &      flex_data%ave_dt_local(flex_data%i_drmax_d))
      end if
!
      call MPI_allREDUCE                                                &
     &   (flex_data%rms_dt_local, flex_data%rms_dt_global,              &
     &    flex_data%ntot_comp, CALYPSO_REAL, MPI_SUM,                   &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE                                                &
     &   (flex_data%ave_dt_local, flex_data%ave_dt_global,              &
     &    flex_data%ntot_comp, CALYPSO_REAL, MPI_SUM,                   &
     &    CALYPSO_COMM, ierr_MPI)
!
!
      do i = 1, flex_data%ntot_comp
        flex_data%d_ratio(i)                                            &
     &     = abs(flex_data%rms_dt_global(i) - flex_data%rms_dt_pre1(i)) &
     &        / (flex_data%rms_dt_global(0) - flex_data%rms_dt_pre1(0))
      end do
!
      imax = 1
      flex_data%d_ratio_allmax = flex_data%d_ratio(1)
      do i = 2, flex_data%ntot_comp
        if( flex_data%d_ratio(i) .gt. flex_data%d_ratio_allmax) then
          imax = i
          flex_data%d_ratio_allmax = flex_data%d_ratio(imax)
        end if
      end do
!
      delta1 = abs(flex_data%rms_dt_global(imax)                        &
     &           - flex_data%rms_dt_pre1(imax))                         &
     &             / (flex_data%rms_dt_global(0)                        &
     &              - flex_data%rms_dt_pre1(0))
      delta2 = abs(flex_data%rms_dt_global(imax)                        &
     &              - flex_data%rms_dt_pre2(imax))                      &
     &             / (flex_data%rms_dt_global(0)                        &
     &              - flex_data%rms_dt_pre2(0))
!
      if(delta2 .eq. 0.0d0) then
        flex_data%d_ratio_allmax = one
      else
        flex_data%d_ratio_allmax = delta1 / delta2
      end if
!
      if(my_rank.eq.0) then
        write(*,*) 'rms_dt_global(0)',                                  &
     &            flex_data%rms_dt_global(0), flex_data%rms_dt_pre1(0), &
     &            flex_data%rms_dt_pre2(0)
        write(*,*) 'd_ratio', flex_data%d_ratio
        write(*,*) 'flex_data%d_ratio_allmax', flex_data%d_ratio_allmax
      end if
!
      end subroutine check_difference_by_prev_rms
!
! ----------------------------------------------------------------------
!
      subroutine set_ele_rms_4_previous_step                            &
     &         (time_d, mesh, fluid, iphys, nod_fld,                    &
     &          jacs, fem_wk, flex_data)
!
      type(time_data), intent(in) :: time_d
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(flexible_stepping_data), intent(inout) :: flex_data
!
!
      if(flex_data%i_drmax_v .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_chk_mom  ),        &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_v  ),              &
     &      flex_data%ave_dt_local(flex_data%i_drmax_v  ))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_chk_mom+1),        &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_v+1),              &
     &      flex_data%ave_dt_local(flex_data%i_drmax_v+1))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_chk_mom+2),        &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_v+2),              &
     &      flex_data%ave_dt_local(flex_data%i_drmax_v+2))
      end if
!
      if(flex_data%i_drmax_p .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_chk_press,          &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_p),                &
     &      flex_data%ave_dt_local(flex_data%i_drmax_p))
      end if
!
!
      if(flex_data%i_drmax_b .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_chk_uxb  ),        &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_b  ),              &
     &      flex_data%ave_dt_local(flex_data%i_drmax_b  ))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_chk_uxb+1),        &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_b+1),              &
     &      flex_data%ave_dt_local(flex_data%i_drmax_b+1))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_chk_uxb+2),        &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_b+2),              &
     &      flex_data%ave_dt_local(flex_data%i_drmax_b+2))
      end if
!
      if(flex_data%i_drmax_f .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_chk_potential,      &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_f),                &
     &      flex_data%ave_dt_local(flex_data%i_drmax_f))
      end if
!
!
      if(flex_data%i_drmax_t .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_chk_heat,           &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_t),                &
     &      flex_data%ave_dt_local(flex_data%i_drmax_t))
      end if
!
      if(flex_data%i_drmax_d .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_chk_composit,       &
     &      mesh, nod_fld, jacs, fem_wk,                                &
     &      flex_data%rms_dt_local(flex_data%i_drmax_d),                &
     &      flex_data%ave_dt_local(flex_data%i_drmax_d))
      end if
!
!
      call MPI_allREDUCE                                                &
     &   (flex_data%rms_dt_local, flex_data%rms_dt_global,              &
     &    flex_data%ntot_comp, CALYPSO_REAL, MPI_MAX,                   &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE                                                &
     &   (flex_data%ave_dt_local, flex_data%ave_dt_global,              &
     &    flex_data%ntot_comp, CALYPSO_REAL, MPI_MIN,                   &
     &    CALYPSO_COMM, ierr_MPI)
!
      flex_data%rms_dt_global(0) = time_d%time - time_d%dt
      flex_data%rms_dt_pre1 = 0.0d0
!
      end subroutine set_ele_rms_4_previous_step
!
! ----------------------------------------------------------------------
!
      end module check_deltat_by_prev_rms
