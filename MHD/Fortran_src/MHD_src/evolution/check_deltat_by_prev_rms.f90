!check_deltat_by_prev_rms.f90
!     module check_deltat_by_prev_rms
!
!      Written by H. Matsui on Nov., 2009
!
!!      subroutine s_check_deltat_by_prev_rms(node, ele, fluid,         &
!!     &          iphys, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!!      subroutine set_ele_rms_4_previous_step(node, ele, fluid,        &
!!     &          iphys, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!
      module check_deltat_by_prev_rms
!
      use m_precision
!
      use calypso_mpi
      use m_control_parameter
      use m_constants
      use m_machine_parameter
      use m_flex_delta_t_data
      use m_t_step_parameter
      use m_t_int_parameter
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_finite_element_mat
!
      use int_all_energy
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_check_deltat_by_prev_rms(node, ele, fluid,           &
     &          iphys, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      integer(kind = kint) :: i, imax
      real(kind = kreal) :: delta1, delta2
!
!
      do i = 0, ntot_dratio
        rms_dt_pre2(i) = rms_dt_pre1(i)
        rms_dt_pre1(i) = rms_dt_global(i)
      end do
      rms_dt_global(0) = time
!
      if(i_drmax_v .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_velo  ),           &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_v  ), ave_dt_local(i_drmax_v  ))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_velo+1),           &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &     rms_dt_local(i_drmax_v+1), ave_dt_local(i_drmax_v+1))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_velo+2),           &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_v+2), ave_dt_local(i_drmax_v+2))
      end if
!
      if(i_drmax_v .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_press,              &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_p), ave_dt_local(i_drmax_p))
      end if
!
!
      if( (i_drmax_b*iflag_t_evo_4_vect_p) .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_vecp  ),           &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &     rms_dt_local(i_drmax_b  ), ave_dt_local(i_drmax_b  ))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_vecp+1),           &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_b+1), ave_dt_local(i_drmax_b+1))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_vecp+2),           &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_b+2), ave_dt_local(i_drmax_b+2))
      else if(i_drmax_b .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_magne  ),          &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_b  ), ave_dt_local(i_drmax_b  ))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_magne+1),          &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_b+1), ave_dt_local(i_drmax_b+1))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_magne+2),          &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_b+2), ave_dt_local(i_drmax_b+2))
      end if
!
      if(i_drmax_f .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_mag_p,              &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_f), ave_dt_local(i_drmax_f))
      end if
!
!
      if(i_drmax_t .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_temp,               &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_t), ave_dt_local(i_drmax_t))
      end if
!
      if(i_drmax_d .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_light,              &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_d), ave_dt_local(i_drmax_d))
      end if
!
      call MPI_allREDUCE (rms_dt_local, rms_dt_global(1), ntot_dratio,  &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE (ave_dt_local, ave_dt_global(1), ntot_dratio,  &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
!
      do i = 1, ntot_dratio
        d_ratio(i) = abs(rms_dt_global(i) - rms_dt_pre1(i))             &
     &              / (rms_dt_global(0) - rms_dt_pre1(0))
      end do
!
      imax = 1
      d_ratio_allmax = d_ratio(1)
      do i = 2, ntot_dratio
        if( d_ratio(i) .gt. d_ratio_allmax) then
          imax = i
          d_ratio_allmax = d_ratio(imax)
        end if
      end do
!
      delta1 = abs(rms_dt_global(imax) - rms_dt_pre1(imax))             &
     &             / (rms_dt_global(0) - rms_dt_pre1(0))
      delta2 = abs(rms_dt_global(imax) - rms_dt_pre2(imax))             &
     &             / (rms_dt_global(0) - rms_dt_pre2(0))
!
      if(delta2 .eq. 0.0d0) then
        d_ratio_allmax = one
      else
        d_ratio_allmax = delta1 / delta2
      end if
!
      if(my_rank.eq.0) then
        write(*,*) 'rms_dt_global(0)',                                  &
     &            rms_dt_global(0), rms_dt_pre1(0), rms_dt_pre2(0)
        write(*,*) 'd_ratio', d_ratio
        write(*,*) 'd_ratio_allmax', d_ratio_allmax
      end if
!
      end subroutine s_check_deltat_by_prev_rms
!
! ----------------------------------------------------------------------
!
      subroutine set_ele_rms_4_previous_step(node, ele, fluid,          &
     &          iphys, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      if(i_drmax_v .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_chk_mom  ),        &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_v  ), ave_dt_local(i_drmax_v  ))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_chk_mom+1),        &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_v+1), ave_dt_local(i_drmax_v+1))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_chk_mom+2),        &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_v+2), ave_dt_local(i_drmax_v+2))
      end if
!
      if(i_drmax_v .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_chk_press,          &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_p), ave_dt_local(i_drmax_p))
      end if
!
!
      if(i_drmax_b .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_chk_uxb  ),        &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_b  ), ave_dt_local(i_drmax_b  ))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_chk_uxb+1),        &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_b+1), ave_dt_local(i_drmax_b+1))
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, (iphys%i_chk_uxb+2),        &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_b+2), ave_dt_local(i_drmax_b+2))
      end if
!
      if(i_drmax_f .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_chk_potential,      &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_f), ave_dt_local(i_drmax_f))
      end if
!
!
      if(i_drmax_t .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_chk_heat,           &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_t), ave_dt_local(i_drmax_t))
      end if
!
      if(i_drmax_d .gt. izero) then
        call int_ave_rms_4_scalar                                       &
     &     (fluid%istack_ele_fld_smp, ione, iphys%i_chk_composit,       &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rms_dt_local(i_drmax_d), ave_dt_local(i_drmax_d))
      end if
!
!
      call MPI_allREDUCE (rms_dt_local, rms_dt_global(1), ntot_dratio,  &
     &    CALYPSO_REAL, MPI_MAX, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE (ave_dt_local, ave_dt_global(1), ntot_dratio,  &
     &    CALYPSO_REAL, MPI_MIN, CALYPSO_COMM, ierr_MPI)
!
      rms_dt_global(0) = time - dt
      rms_dt_pre1 = 0.0d0
!
      end subroutine set_ele_rms_4_previous_step
!
! ----------------------------------------------------------------------
!
      end module check_deltat_by_prev_rms
