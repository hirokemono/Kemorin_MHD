!>@file   output_parallel_ucd_file.f90
!!@brief  module output_parallel_ucd_file
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief parallel UCD file output routines
!!
!!@verbatim
!!      subroutine link_output_grd_file                                 &
!!     &         (node, ele, nod_comm, nod_fld, ucd_param, ucd)
!!      subroutine output_udt_one_snapshot(istep_ucd, ucd_param, time_d,&
!!     &          node, ele, nod_comm, nod_fld)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(time_data), intent(in) :: time_d
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: nod_comm
!!        type(phys_data), intent(in) :: nod_fld
!!
!!      subroutine link_output_ucd_file_once                            &
!!     &         (istep_ucd, nod_fld, ucd_param, t_IO)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(phys_data), intent(in) :: nod_fld
!!        type(time_data), intent(in) :: t_IO
!!
!!      subroutine finalize_ucd_file_output(ucd_param)
!!        type(field_IO_params), intent(in) :: ucd_param
!!@endverbatim
!
      module output_parallel_ucd_file
!
      use m_precision
      use calypso_mpi
      use t_file_IO_parameter
      use t_time_data
      use t_ucd_data
      use m_field_file_format
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine link_output_grd_file                                   &
     &         (node, ele, nod_comm, nod_fld, ucd_param, ucd)
!
      use t_geometry_data
      use t_comm_table
      use t_phys_data
      use merged_udt_vtk_file_IO
      use parallel_ucd_IO_select
      use set_ucd_data_to_type
!
      type(field_IO_params), intent(in) :: ucd_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(phys_data), intent(in) :: nod_fld
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_num_field_2_ucd(nod_fld, ucd)
      call link_local_mesh_2_ucd(node, ele, ucd)
      call link_field_data_to_ucd(nod_fld, ucd)
!
      if (ucd_param%iflag_format/icent .eq. iflag_single/icent) then
        call init_merged_ucd                                            &
     &     (ucd_param%iflag_format, node, ele, nod_comm, ucd)
      end if
!
      call sel_write_parallel_ucd_mesh(ucd_param, ucd)
!
      if(   mod(ucd_param%iflag_format,icent)/iten .eq. iflag_udt/iten  &
     & .or. mod(ucd_param%iflag_format,icent)/iten .eq. iflag_vtd/iten) &
     &    then
        call deallocate_ucd_ele(ucd)
      end if
!
      if(mod(ucd_param%iflag_format,icent)/iten .eq. iflag_vtd/iten) then
        call deallocate_ucd_node(ucd)
      end if
!
      end subroutine link_output_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine output_udt_one_snapshot(istep_ucd, ucd_param, time_d,  &
     &          node, ele, nod_comm, nod_fld)
!
      use t_time_data
      use t_geometry_data
      use t_comm_table
      use t_phys_data
      use merged_udt_vtk_file_IO
      use parallel_ucd_IO_select
      use set_ucd_data_to_type
!
      integer(kind = kint), intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(in) :: time_d
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(phys_data), intent(in) :: nod_fld
!
      type(time_data) :: t_IO
      type(ucd_data) :: ucd
!
!
      call link_num_field_2_ucd(nod_fld, ucd)
      call link_local_mesh_2_ucd(node, ele, ucd)
      call link_field_data_to_ucd(nod_fld, ucd)
!
      if (ucd_param%iflag_format/icent .eq. iflag_single/icent) then
        call init_merged_ucd                                            &
     &     (ucd_param%iflag_format, node, ele, nod_comm, ucd)
      end if
!
      call copy_time_step_size_data(time_d, t_IO)
      call sel_write_parallel_ucd_file                                  &
     &   (istep_ucd, ucd_param, t_IO, ucd)
!
      call deallocate_ucd_node(ucd)
!
      call deallocate_ucd_ele(ucd)
      call disconnect_ucd_data(ucd)
!
      if (ucd_param%iflag_format/icent .ne. iflag_single/icent) return
!
      call finalize_merged_ucd(ucd_param%iflag_format, ucd)
!
      end subroutine output_udt_one_snapshot
!
!-----------------------------------------------------------------------
!
      subroutine link_output_ucd_file_once                              &
     &         (istep_ucd, nod_fld, ucd_param, t_IO)
!
      use t_phys_data
!
      use set_ucd_data_to_type
      use ucd_IO_select
!
      integer(kind = kint),  intent(in) :: istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
!
      type(phys_data), intent(in) :: nod_fld
      type(time_data), intent(in) :: t_IO
!
      type(ucd_data) :: local_ucd
!
!
      call link_field_data_to_ucd(nod_fld, local_ucd)
!
      call sel_write_udt_file                                           &
     &   (my_rank, istep_ucd, ucd_param, t_IO, local_ucd)
      call disconnect_ucd_data(local_ucd)
!
      end subroutine link_output_ucd_file_once
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine finalize_ucd_file_output(ucd_param, ucd)
!
      use merged_udt_vtk_file_IO
!
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_data), intent(inout) :: ucd
!
!
      if (ucd_param%iflag_format/icent .ne. iflag_single/icent) return
!
      call finalize_merged_ucd(ucd_param%iflag_format, ucd)
!
      end subroutine finalize_ucd_file_output
!
!-----------------------------------------------------------------------
!
      end module output_parallel_ucd_file
