!FEM_analyzer_sph_trans.f90
!
!!      subroutine FEM_initialize_sph_trans(udt_file_param, t_IO)
!!        type(field_IO_params), intent(in) :: udt_file_param
!!
!!      subroutine FEM_analyze_sph_trans                                &
!!     &         (i_step, udt_file_param, t_IO, visval)
!!
!!      subroutine SPH_to_FEM_bridge_sph_trans                          &
!!     &         (udt_file_param, rj_fld, fld_IO)
!!        type(field_IO_params), intent(in) :: udt_file_param
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(field_IO), intent(inout) :: fld_IO
!!      subroutine FEM_finalize_sph_trans(udt_file_param, m_ucd)
!!        type(field_IO_params), intent(in) :: udt_file_param
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
      use t_file_IO_parameter
      use t_time_data
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
      subroutine FEM_initialize_sph_trans(udt_file_param, t_IO)
!
      use m_array_for_send_recv
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
      type(field_IO_params), intent(in) :: udt_file_param
      type(time_data), intent(inout) :: t_IO
!
!
!  -----    construct geometry informations
!
      call mesh_setup_4_SPH_TRANS
!
      call dealloc_edge_geometory(elemesh_STR%edge)
!
!  -------------------------------
!
      input_ucd%nnod = ione
      call sel_read_udt_param(my_rank, t_STR%init_d%i_time_step,        &
    &     udt_file_param, t_IO, input_ucd)
!
      end subroutine FEM_initialize_sph_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_sph_trans                                  &
     &         (i_step, udt_file_param, t_IO, visval)
!
      use m_ctl_params_sph_trans
      use set_ucd_data_to_type
      use nod_phys_send_recv
!
      integer(kind =kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: udt_file_param
      type(time_data), intent(inout) :: t_IO
      integer(kind =kint), intent(inout) :: visval
!
!
!*  ----------   Count steps for visualization
!*
      visval =  mod(i_step,t_STR%ucd_step%increment)
!
!*  -----------  Output volume data --------------
!*
      if(visval .eq. 0) then
        call set_data_by_read_ucd                                       &
     &    (my_rank, i_step, udt_file_param, t_IO, input_ucd, field_STR)
        call nod_fields_send_recv(femmesh_STR%mesh, field_STR)
      end if
!
      end subroutine FEM_analyze_sph_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_sph_trans(rj_fld, fld_IO)
!
      use t_field_data_IO
      use t_phys_data
      use copy_rj_phys_data_4_IO
!
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
!
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
      subroutine FEM_finalize_sph_trans(udt_file_param, m_ucd)
!
      use m_ctl_params_sph_trans
      use t_ucd_data
      use output_parallel_ucd_file
!
      type(field_IO_params), intent(in) :: udt_file_param
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      if(t_STR%ucd_step%increment .gt. 0) then
        call finalize_ucd_file_output(udt_file_param, m_ucd)
      end if
!
      end subroutine FEM_finalize_sph_trans
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_sph_trans
