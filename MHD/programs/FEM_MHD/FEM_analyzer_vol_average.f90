!FEM_analyzer_vol_average.f90
!      module FEM_analyzer_vol_average
!
!      modified by H. Matsui on June, 2005 
!
!      subroutine FEM_initialize_vol_average
!      subroutine FEM_analyze_vol_average(i_step)
!
      module FEM_analyzer_vol_average
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use m_t_step_parameter
      use m_t_int_parameter
!
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine FEM_initialize_vol_average
!
      use m_control_parameter
      use m_layering_ele_list
      use m_geometry_data_MHD
!
      use initialize_4_snapshot
!
      use node_monitor_IO
      use open_sgs_model_coefs
!
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'init_analyzer_snap'
      call init_analyzer_snap(MHD_mesh1, layer_tbl1)
!
      end subroutine FEM_initialize_vol_average
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_vol_average(i_step)
!
      use m_control_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_node_phys_data
      use m_geometry_data_MHD
      use m_element_phys_data
      use m_jacobians
      use m_finite_element_matrix
      use m_int_vol_data
!
      use read_udt_4_snapshot
!
      use nod_phys_send_recv
      use lead_physical_values
      use copy_nodal_fields
!
      use time_step_data_IO_control
      use output_parallel_ucd_file
!
      integer(kind=kint ), intent(in) :: i_step
!
!     ---- Load field data --- 
!
      if (my_rank.eq.0) write(*,*) 'step: ', i_step
!
      if (iflag_debug.eq.1)  write(*,*) 'read_udt_4_snap'
      call read_udt_4_snap(i_step)
      time = time_init + dt*dble(i_step)
!
!     ---- magnetic field update
!
      if (iflag_4_ref_temp .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1)  write(*,*) 'set_2_perturbation_temp'
        call subtract_2_nod_scalars(node1, nod_fld1,                    &
     &      iphys%i_temp, iphys%i_ref_t, iphys%i_par_temp)
      end if
!
!     ---------------------
!
      if (iflag_debug.eq.1)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(node1, nod_comm, nod_fld1)
!
!     -----Output monitor date
!
      if (iflag_debug.eq.1) write(*,*) 'output_time_step_control'
      call output_time_step_control(node1, ele1, MHD_mesh1,             &
     &    iphys, nod_fld1, iphys_ele, fld_ele1, jac1_3d_q, jac1_3d_l,   &
     &    fem1_wk, mhd_fem1_wk)
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
