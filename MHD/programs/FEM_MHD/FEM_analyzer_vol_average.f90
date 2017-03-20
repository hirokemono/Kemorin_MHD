!FEM_analyzer_vol_average.f90
!      module FEM_analyzer_vol_average
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_initialize_vol_average(MHD_step)
!!      subroutine FEM_analyze_vol_average(i_step, MHD_step)
!!        type(MHD_IO_step_param), intent(inout) :: MHD_step
!
      module FEM_analyzer_vol_average
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use m_t_step_parameter
      use m_SGS_control_parameter
      use t_time_data
      use t_MHD_step_parameter
!
      use calypso_mpi
!
      implicit none
!
      type(time_data), save, private :: SNAP_time_IO
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine FEM_initialize_vol_average(MHD_step)
!
      use m_mesh_data
      use m_node_phys_data
      use m_control_parameter
      use m_layering_ele_list
      use m_geometry_data_MHD
      use m_boundary_field_IO
!
      use initialize_4_snapshot
!
      use node_monitor_IO
      use open_sgs_model_coefs
!
      type(MHD_IO_step_param), intent(inout) :: MHD_step
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'init_analyzer_snap'
      call init_analyzer_snap(FEM_prm1, SGS_par1, IO_bc1, time_d1,      &
     &    mesh1, group1, ele_mesh1, MHD_mesh1, layer_tbl1,              &
     &    iphys, nod_fld1, SNAP_time_IO, MHD_step%rst_step, label_sim)
!
      end subroutine FEM_initialize_vol_average
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_vol_average(i_step, MHD_step)
!
      use m_control_parameter
      use m_physical_property
      use m_mesh_data
      use m_node_phys_data
      use m_geometry_data_MHD
      use m_element_phys_data
      use m_jacobians
      use m_finite_element_matrix
!
      use m_ucd_data
!
      use nod_phys_send_recv
      use lead_physical_values
      use copy_nodal_fields
      use input_control
!
      use time_step_data_IO_control
      use output_parallel_ucd_file
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_IO_step_param), intent(inout) :: MHD_step
!
      integer(kind = kint) :: iflag
!
!     ---- Load field data --- 
!
      if (my_rank.eq.0) write(*,*) 'step: ', i_step
!
      if (iflag_debug.eq.1)  write(*,*) 'read_udt_4_snap'
      call read_udt_4_snap(i_step,                                      &
     &    FEM_udt_org_param, nod_fld1, SNAP_time_IO, MHD_step%ucd_step)
      time_d1%time = init_d1%time + time_d1%dt * dble(i_step)
!
!     ---- magnetic field update
!
      if (ref_param_T1%iflag_reference .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1)  write(*,*) 'set_2_perturbation_temp'
        call subtract_2_nod_scalars(nod_fld1,                           &
     &      iphys%i_temp, iphys%i_ref_t, iphys%i_par_temp)
      end if
      if (ref_param_C1%iflag_reference .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1)  write(*,*) 'set_2_perturbation_comp'
        call subtract_2_nod_scalars(nod_fld1,                           &
     &      iphys%i_light, iphys%i_ref_c, iphys%i_par_light)
      end if
!
!     ---------------------
!
      if (iflag_debug.eq.1)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(mesh1%nod_comm, nod_fld1)
!
!     -----Output monitor date
!
      iflag = output_IO_flag(i_step, MHD_step%rms_step)
      if(iflag .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'output_time_step_control'
        call output_time_step_control                                   &
     &     (FEM_prm1, time_d1, mesh1, MHD_mesh1,                        &
     &      fl_prop1, cd_prop1, iphys, nod_fld1, iphys_ele, fld_ele1,   &
     &      jac1_3d_q, jac1_3d_l, fem1_wk, mhd_fem1_wk)
      end if
!
      end subroutine FEM_analyze_vol_average
!
! ----------------------------------------------------------------------
!
!      subroutine FEM_finalize_vol_average
!
!      end subroutine FEM_finalize_vol_average
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_vol_average
