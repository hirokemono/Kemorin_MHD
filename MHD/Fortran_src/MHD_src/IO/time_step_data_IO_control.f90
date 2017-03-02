!
!     module time_step_data_IO_control
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine output_time_step_control(FEM_prm, mesh, MHD_mesh,    &
!!     &          fl_prop, cd_prop, iphys, nod_fld, iphys_ele, ele_fld, &
!!     &          jac_3d_q, jac_3d_l, fem_wk, mhd_fem_wk)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module time_step_data_IO_control
!
      use m_precision
!
      use t_FEM_control_parameter
      use t_physical_property
      use t_mesh_data
      use t_geometry_data
      use t_geometry_data_MHD
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_IO_step_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine output_time_step_control(FEM_prm, mesh, MHD_mesh,      &
     &          fl_prop, cd_prop, iphys, nod_fld, iphys_ele, ele_fld,   &
     &          jac_3d_q, jac_3d_l, fem_wk, mhd_fem_wk)
!
      use calypso_mpi
      use m_t_step_parameter
      use m_t_int_parameter
      use m_mean_square_values
!
      use int_bulk
      use time_step_file_IO
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer (kind = kint) :: nd
!
!
      if(output_flag(istep_max_dt, i_step_check) .ne. 0 ) return
!
        if(my_rank .eq. 0) write(*,'(a10,i16,a10,e15.8)')               &
     &            'i_step=',i_step_MHD,'time=',time
!
      call s_int_mean_squares(FEM_prm%npoint_t_evo_int,                 &
     &    mesh%node, mesh%ele, MHD_mesh%fluid, MHD_mesh%conduct,        &
     &    iphys, nod_fld, jac_3d_q, jac_3d_l, fem_wk, mhd_fem_wk)
      call int_no_evo_mean_squares                                      &
     &   (mesh%node, mesh%ele, fl_prop, cd_prop, iphys, nod_fld,        &
     &    iphys_ele, ele_fld, MHD_mesh%fluid, jac_3d_q, fem_wk)
!
      call MPI_allREDUCE (bulk_local, bulk_global, num_bulk,            &
     &      CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE (rms_local, rms_global, num_rms,               &
     &      CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
!
       do nd = 1, num_bulk
         bulk_global(nd) = bulk_global(nd) / rms_global(ivol)
       end do
       do nd = 1, num_rms - 1
           if (nd .eq. i_rms%i_velo                                     &
     &    .or. nd .eq. i_rms%i_magne .or. nd .eq. ir_me_ic              &
     &    .or. nd .eq. i_rms%i_vort                                     &
     &    .or. nd .eq. i_rms%i_current .or.  nd .eq. ir_sqj_ic          &
     &    .or. nd .eq. i_rms%i_filter_velo                              &
     &    .or. nd .eq. i_rms%i_filter_magne                             &
     &    .or. nd .eq. ir_me_f_ic) then
            rms_global(nd) = rms_global(nd) / rms_global(ivol)
        else
          rms_global(nd) = sqrt(rms_global(nd) / rms_global(ivol))
        end if
      end do
!
      call output_monitor_file(my_rank, nod_fld)
!
      end subroutine output_time_step_control
!
!  ---------------------------------------------------------------------
!
      end module time_step_data_IO_control
