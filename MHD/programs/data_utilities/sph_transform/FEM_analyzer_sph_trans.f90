!FEM_analyzer_sph_trans.f90
!
!      subroutine FEM_initialize_sph_trans
!      subroutine FEM_analyze_sph_trans(i_step, visval)
!
!      subroutine SPH_to_FEM_bridge_sph_trans
!      subroutine FEM_to_SPH_bridge_sph_trans(visval)
!
      module FEM_analyzer_sph_trans
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_parallel_var_dof
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_sph_trans
!
      use m_edge_geometry_data
      use m_jacobians
      use m_t_step_parameter
!
      use nodal_vector_send_recv
      use load_mesh_data
      use const_mesh_info
      use cal_jacobian
      use const_RHS_assemble_list
      use int_volume_of_domain
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
      use set_ucd_data
      use ucd_IO_select
      use output_parallel_ucd_file
!
      use cvt_nod_data_to_sph_data
      use copy_all_field_4_sph_trans
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!  --  load FEM mesh data
!
      if (iflag_debug.eq.1) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
      call time_prog_barrier
!
!  -----    construct geometry informations
!
      if (iflag_debug.gt.0) write(*,*) 'allocate_iccgN_matrix'
      call allocate_iccgN_matrix(isix, numnod)
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
!  -------------------------------
!
      call deallocate_edge_geometry
      call time_prog_barrier
!
      if (iflag_debug.gt.0) write(*,*) 'init_send_recv'
      call init_send_recv
!
      if (iflag_debug.gt.0) write(*,*) 'allocate_data_arrays'
      call allocate_data_arrays
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'link_num_field_2_output'
      call link_num_field_2_output
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_udt_param'
      call sel_read_udt_param(my_rank, i_step_init)
!
      end subroutine FEM_initialize_sph_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_sph_trans(i_step, visval)
!
      use m_t_step_parameter
      use set_ucd_data
      use ucd_IO_select
!
      integer (kind =kint), intent(in) :: i_step
      integer (kind =kint), intent(inout) :: visval
!
!
!*  ----------   Count steps for visualization
!*
      visval =  mod(i_step,i_step_output_ucd)
!
!*  -----------  Output volume data --------------
!*
      if(visval .eq. 0) then
        if (iflag_debug.gt.0) write(*,*) 'sel_read_udt_file'
        call sel_read_udt_file(my_rank, i_step)
        if (iflag_debug.gt.0) write(*,*) 'set_ucd_data_from_IO'
        call set_ucd_data_from_IO
      end if
!
      end subroutine FEM_analyze_sph_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_sph_trans
!
      use m_field_data_IO
      use copy_rj_phys_data_4_IO
!
!
      if (iflag_debug.gt.0) write(*,*) 'copy_rj_all_phys_name_to_IO'
      call copy_rj_all_phys_name_to_IO
      call allocate_phys_data_IO
!
      end subroutine SPH_to_FEM_bridge_sph_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_to_SPH_bridge_sph_trans(visval)
!
      use cvt_nod_data_to_sph_data
!      use nod_phys_send_recv
!
      integer (kind =kint), intent(in) :: visval
!
!*  -----------  data transfer to FEM array --------------
!*
      if(visval .eq. 0) then
!        call phys_send_recv_all
        call copy_nod_scalar_to_sph_data
        call cvt_xyz_to_sph_vec_sph_data
        call cvt_xyz_to_sph_tensor_data
      end if
!
      end subroutine FEM_to_SPH_bridge_sph_trans
!
!-----------------------------------------------------------------------
!
      subroutine FEM_finalize_sph_trans
!
      use m_work_4_sph_trans
!
!
      call deallocate_wk_nod_data_to_sph
!
      end subroutine FEM_finalize_sph_trans
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_trans
