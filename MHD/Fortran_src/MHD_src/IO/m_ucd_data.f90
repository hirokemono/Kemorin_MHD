!>@file   m_ucd_data.f90
!!@brief  module m_ucd_data
!!
!!@author H. Matsui
!!@date  Programmed in July 2000 (ver 1.1)
!!@n      Modified in Aug., 2007
!!@n      Modified in Dec., 2015
!
!>@brief Strucures for field data output
!!
!!@verbatim
!!      subroutine set_control_MHD_field_file(ucd)
!!      subroutine s_output_ucd_file_control
!!
!!      subroutine output_grd_file_4_snapshot(mesh, nod_fld)
!!      subroutine output_grd_file_w_org_connect                        &
!!     &        (mesh, MHD_mesh, nod_fld)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ucd_data), intent(inout) :: ucd
!!        type(merged_ucd_data), intent(inout) :: m_ucd
!!      subroutine read_udt_4_snap(i_step, udt_file_param, nod_fld)
!!        type(field_IO_params), intent(in) :: udt_file_param
!!        type(phys_data),intent(inout) :: nod_fld
!!      subroutine finalize_output_ucd
!!
!!      subroutine link_global_org_mesh_4_ucd(mesh, MHD_mesh, ucd)
!!@endverbatim
!
      module m_ucd_data
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_ucd_data
      use t_phys_data
      use t_geometry_data_MHD
!
      implicit none
!
!>        Instance for FEM field data IO
      type(ucd_data), save :: fem_ucd
!
!>        Instance for numbers of FEM mesh for merged IO
      type(merged_ucd_data), save :: merged_ucd
!
      private :: fem_ucd, merged_ucd
      private :: link_local_org_mesh_4_ucd
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_MHD_field_file
!
      use m_ctl_data_4_platforms
      use parallel_ucd_IO_select
!
!
      if(udt_file_head_ctl%iflag .eq. 0) then
        fem_ucd%ifmt_file = -1
        return
      end if
!
      call set_merged_ucd_file_define(fem_ucd)
!
      end subroutine set_control_MHD_field_file
!
! -----------------------------------------------------------------------
!
      subroutine s_output_ucd_file_control
!
      use calypso_mpi
      use m_t_step_parameter
      use parallel_ucd_IO_select
      use copy_time_steps_4_restart
!
      integer(kind = kint) :: istep_ucd
!
!
      if(fem_ucd%ifmt_file .lt. 0) return
      if(i_step_output_ucd .eq. 0) return
      if(mod(istep_max_dt,i_step_output_ucd) .ne. 0) return
!
      istep_ucd = istep_max_dt / i_step_output_ucd
!
      call copy_time_steps_to_restart
      call sel_write_parallel_ucd_file(istep_ucd, fem_ucd, merged_ucd)
!      call output_range_data(node, nod_fld, istep_ucd, time)
!
      end subroutine s_output_ucd_file_control
!
! ----------------------------------------------------------------------
!
      subroutine output_grd_file_4_snapshot(mesh, nod_fld)
!
      use m_t_step_parameter
      use output_parallel_ucd_file
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data),intent(inout) :: nod_fld
!
!
      if(fem_ucd%ifmt_file .lt. 0) return
      if(i_step_output_ucd .eq. 0) return
      call link_output_grd_file(mesh%node, mesh%ele, mesh%nod_comm,     &
     &    nod_fld, fem_ucd, merged_ucd)
!
      end subroutine output_grd_file_4_snapshot
!
! ----------------------------------------------------------------------
!
      subroutine output_grd_file_w_org_connect                          &
     &          (mesh, MHD_mesh, nod_fld)
!
      use m_field_file_format
      use m_t_step_parameter
      use set_ucd_data_to_type
!
      use merged_udt_vtk_file_IO
      use parallel_ucd_IO_select
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(phys_data), intent(in) :: nod_fld
!
!
      if(fem_ucd%ifmt_file .lt. 0) return
      if(i_step_output_ucd .eq. 0) return
!
      call link_num_field_2_ucd(nod_fld, fem_ucd)
      call link_local_org_mesh_4_ucd                                    &
     &   (mesh%node, mesh%ele, MHD_mesh, fem_ucd)
      call link_field_data_to_ucd(nod_fld, fem_ucd)
!
      if (fem_ucd%ifmt_file/icent .eq. iflag_single/icent) then
        call init_merged_ucd                                            &
     &     (mesh%node, mesh%ele, mesh%nod_comm, fem_ucd, merged_ucd)
      end if
!
      call sel_write_parallel_ucd_mesh(fem_ucd, merged_ucd)
      call calypso_mpi_barrier
!
      if(   mod(fem_ucd%ifmt_file,icent)/iten .eq. iflag_udt/iten       &
     & .or. mod(fem_ucd%ifmt_file,icent)/iten .eq. iflag_vtd/iten) then
        call deallocate_ucd_ele(fem_ucd)
      end if
!
      if(mod(fem_ucd%ifmt_file,icent)/iten .eq. iflag_vtd/iten) then
        call deallocate_ucd_node(fem_ucd)
      end if
!
      end subroutine output_grd_file_w_org_connect
!
!-----------------------------------------------------------------------
!
      subroutine read_udt_4_snap(i_step, udt_file_param, nod_fld)
!
      use calypso_mpi
      use t_field_data_IO
      use m_t_step_parameter
      use set_ucd_data_to_type
!
      integer(kind = kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: udt_file_param
      type(phys_data),intent(inout) :: nod_fld
!
      integer(kind = kint) :: istep_ucd
!
!
      if (mod(i_step,i_step_output_ucd) .ne. izero) return
      istep_ucd = i_step / i_step_output_ucd
      call set_data_by_read_ucd_once(my_rank, istep_ucd,                &
    &     udt_file_param%iflag_format, udt_file_param%file_prefix,      &
    &     nod_fld)
!
      end subroutine read_udt_4_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine finalize_output_ucd
!
      use output_parallel_ucd_file
!
!
      call finalize_ucd_file_output(fem_ucd, merged_ucd)
!
      end subroutine finalize_output_ucd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_global_org_mesh_4_ucd(mesh, MHD_mesh, ucd)
!
      use set_ucd_data_to_type
      use set_and_cal_udt_data
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_node_data_2_ucd(mesh%node, ucd)
      call const_udt_global_connect(mesh%node%internal_node,            &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele,                         &
     &    MHD_mesh%iele_global_org, MHD_mesh%ie_org, ucd)
!
      end subroutine link_global_org_mesh_4_ucd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_local_org_mesh_4_ucd(node, ele, MHD_mesh, ucd)
!
      use set_and_cal_udt_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(mesh_data_MHD), intent(in) :: MHD_mesh
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call const_udt_local_nodes(node%numnod, node%xx, ucd)
      call const_udt_local_connect                                      &
     &   (node%internal_node, ele%numele, ele%nnod_4_ele,               &
     &    MHD_mesh%ie_org, ucd)
!
      end subroutine link_local_org_mesh_4_ucd
!
!-----------------------------------------------------------------------
!
      end module m_ucd_data
