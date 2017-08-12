!
!     module time_step_data_IO_control
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine output_time_step_control                             &
!!     &         (FEM_prm, time_d, mesh, MHD_mesh, fl_prop, cd_prop,    &
!!     &          iphys, nod_fld, iphys_ele, ele_fld, jacobians,        &
!!     &          i_rms, j_ave, ifld_msq, rhs_mat, mhd_fem_wk, fem_msq)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_type), intent(in) :: jacobians
!!        type(phys_address), intent(in) :: i_rms
!!        type(mean_square_address), intent(in) :: ifld_msq
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(mean_square_values), intent(inout) :: fem_msq
!
      module time_step_data_IO_control
!
      use m_precision
!
      use t_FEM_control_parameter
      use t_physical_property
      use t_time_data
      use t_mesh_data
      use t_geometry_data
      use t_geometry_data_MHD
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_finite_element_mat
      use t_work_FEM_integration
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
      subroutine output_time_step_control                               &
     &         (FEM_prm, time_d, mesh, MHD_mesh, fl_prop, cd_prop,      &
     &          iphys, nod_fld, iphys_ele, ele_fld, jacobians,          &
     &          i_rms, j_ave, ifld_msq, rhs_mat, mhd_fem_wk, fem_msq)
!
      use calypso_mpi
      use t_mean_square_values
!
      use int_bulk
      use time_step_file_IO
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(time_data), intent(in) :: time_d
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacobians
      type(phys_address), intent(in) :: i_rms, j_ave
      type(mean_square_address), intent(in) :: ifld_msq
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(mean_square_values), intent(inout) :: fem_msq
!
      integer (kind = kint) :: nd
!
!
        if(my_rank .eq. 0) write(*,'(a10,i16,a10,e15.8)')               &
     &            'i_step=', time_d%i_time_step,'time=', time_d%time
!
      call s_int_mean_squares(FEM_prm%npoint_t_evo_int,                 &
     &    mesh%node, mesh%ele, MHD_mesh%fluid, MHD_mesh%conduct,        &
     &    iphys, nod_fld, jacobians%jac_3d, jacobians%jac_3d_l,         &
     &    i_rms, j_ave, ifld_msq, rhs_mat%fem_wk, mhd_fem_wk, fem_msq)
      call int_no_evo_mean_squares(time_d%i_time_step, time_d%dt,       &
     &    mesh%node, mesh%ele, fl_prop, cd_prop, iphys, nod_fld,        &
     &    iphys_ele, ele_fld, MHD_mesh%fluid, jacobians%jac_3d,         &
     &    i_rms, j_ave, rhs_mat%fem_wk, fem_msq)
!
      call MPI_allREDUCE                                                &
     &   (fem_msq%ave_local, fem_msq%ave_global, fem_msq%num_ave,       &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE                                                &
     &   (fem_msq%rms_local, fem_msq%rms_global, fem_msq%num_rms,       &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
!
       do nd = 1, fem_msq%num_ave
         fem_msq%ave_global(nd) = fem_msq%ave_global(nd)                &
     &                          / fem_msq%rms_global(ifld_msq%ivol)
       end do
       do nd = 1, fem_msq%num_rms - 1
           if (nd .eq. i_rms%i_velo                                     &
     &    .or. nd .eq. i_rms%i_magne                                    &
     &    .or. nd .eq. ifld_msq%ir_me_ic                                &
     &    .or. nd .eq. i_rms%i_vort                                     &
     &    .or. nd .eq. i_rms%i_current                                  &
     &    .or. nd .eq. ifld_msq%ir_sqj_ic                               &
     &    .or. nd .eq. i_rms%i_filter_velo                              &
     &    .or. nd .eq. i_rms%i_filter_magne                             &
     &    .or. nd .eq. ifld_msq%ir_me_f_ic) then
            fem_msq%rms_global(nd) = fem_msq%rms_global(nd)             &
     &                           / fem_msq%rms_global(ifld_msq%ivol)
        else
          fem_msq%rms_global(nd) = sqrt(fem_msq%rms_global(nd)          &
     &                           / fem_msq%rms_global(ifld_msq%ivol))
        end if
      end do
!
      call output_monitor_file                                          &
     &   (my_rank, time_d%i_time_step, time_d%time, nod_fld, fem_msq)
!
      end subroutine output_time_step_control
!
!  ---------------------------------------------------------------------
!
      end module time_step_data_IO_control
