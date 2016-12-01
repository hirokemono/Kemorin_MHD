!FEM_analyzer_sph_trans.f90
!
!!      subroutine FEM_initialize_sph_trans(udt_file_param)
!!        type(field_IO_params), intent(in) :: udt_file_param
!!
!!      subroutine FEM_analyze_sph_trans(i_step, udt_file_param, visval)
!!
!!      subroutine SPH_to_FEM_bridge_sph_trans                          &
!!     &         (udt_file_param, sph_rj, rj_fld, fld_IO)
!!        type(field_IO_params), intent(in) :: udt_file_param
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(field_IO), intent(inout) :: fld_IO
!!      subroutine FEM_finalize_sph_trans(ucd, m_ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!        type(merged_ucd_data), intent(inout) :: m_ucd
!
      module FEM_analyzer_sph_trans
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_SPH_transforms
      use t_ucd_data
      use t_field_data_IO
!
      implicit none
!
!>        Instance for FEM field data IO
      type(ucd_data), save :: input_ucd
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_sph_trans(udt_file_param)
!
      use m_array_for_send_recv
      use m_t_step_parameter
!
      use nod_phys_send_recv
      use int_volume_of_domain
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
      use output_parallel_ucd_file
      use ucd_IO_select
      use const_mesh_information
      use const_element_comm_tables
!
      use copy_all_field_4_sph_trans
!
      type(field_IO_params), intent(in) :: udt_file_param
!
!
!  -----    construct geometry informations
!
      call mesh_setup_4_SPH_TRANS
!
      call deallocate_edge_geom_type(elemesh_STR%edge)
!
!  -------------------------------
!
      call set_ucd_file_format(udt_file_param%iflag_format, input_ucd)
      call set_ucd_file_prefix(udt_file_param%file_prefix, input_ucd)
!
      input_ucd%nnod = ione
      call sel_read_udt_param(my_rank, i_step_init, input_ucd)
!
      end subroutine FEM_initialize_sph_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_sph_trans(i_step, visval)
!
      use m_control_params_2nd_files
      use m_t_step_parameter
      use set_ucd_data_to_type
      use nod_phys_send_recv
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
        call set_ucd_file_prefix(udt_org_param%file_prefix, input_ucd)
        call set_data_by_read_ucd                                       &
     &    (my_rank, i_step, input_ucd, field_STR)
        call nod_fields_send_recv                                       &
     &    (femmesh_STR%mesh%nod_comm, field_STR)
      end if
!
      end subroutine FEM_analyze_sph_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_sph_trans                            &
     &         (udt_file_param, rj_fld, fld_IO)
!
      use t_field_data_IO
      use t_phys_data
      use copy_rj_phys_data_4_IO
!
      type(field_IO_params), intent(in) :: udt_file_param
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
!
!
      call set_ucd_file_format(udt_file_param%iflag_format, input_ucd)
      call set_ucd_file_prefix(udt_file_param%file_prefix, input_ucd)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_rj_all_phys_name_to_IO'
      call copy_rj_phys_name_to_IO(rj_fld%num_phys, rj_fld, fld_IO)
      call alloc_phys_data_IO(fld_IO)
      call alloc_merged_field_stack(nprocs, fld_IO)
!
      end subroutine SPH_to_FEM_bridge_sph_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_finalize_sph_trans(ucd, m_ucd)
!
      use m_t_step_parameter
      use t_ucd_data
      use output_parallel_ucd_file
!
      type(ucd_data), intent(inout) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      if(i_step_output_ucd .gt. 0) then
        call finalize_ucd_file_output(ucd, m_ucd)
      end if
!
      end subroutine FEM_finalize_sph_trans
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_trans
