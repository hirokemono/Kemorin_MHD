!>@file   set_control_4_extend_sleeve.f90
!!@brief  module set_control_4_extend_sleeve
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2005
!
!>@brief  Set parameters for partitioning
!!
!!@verbatim
!!      subroutine s_set_control_4_extend_sleeve                        &
!!     &         (id_rank, part_ctl, comm_part, part_p, sleeve_exp_p)
!!        type(control_data_4_partitioner), intent(in) :: part_ctl
!!        type(partitioner_comm_tables), intent(inout) :: comm_part
!!        type(ctl_param_partitioner), intent(inout) :: part_p
!!        type(sleeve_extension_param), intent(inout) :: sleeve_exp_p
!!@endverbatim
!
      module set_control_4_extend_sleeve
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_ctl_param_partitioner
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_extend_sleeve                          &
     &         (id_rank, part_ctl, comm_part, part_p, sleeve_exp_p)
!
      use t_control_data_4_part
      use t_partitioner_comm_table
      use t_ctl_param_sleeve_extend
      use m_default_file_prefix
!
      use m_file_format_switch
      use set_control_platform_item
      use set_control_platform_data
      use set_control_data_4_part
      use mpi_abort_by_missing_zlib
!
      integer, intent(in) :: id_rank
      type(control_data_4_partitioner), intent(in) :: part_ctl
!
      type(partitioner_comm_tables), intent(inout) :: comm_part
      type(ctl_param_partitioner), intent(inout) :: part_p
      type(sleeve_extension_param), intent(inout) :: sleeve_exp_p
!
      integer(kind = kint) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(id_rank, part_ctl%part_plt)
      call set_control_parallel_mesh(part_ctl%part_plt,                 &
     &                               part_p%distribute_mesh_file)
!
!   set local data format
!
      if (part_ctl%single_plt%mesh_file_prefix%iflag .gt. 0) then
        part_p%global_mesh_file%file_prefix                             &
     &      = part_ctl%single_plt%mesh_file_prefix%charavalue
      else
        write(*,*) 'Set original mesh data'
        stop
      end if
      part_p%global_mesh_file%iflag_format                              &
     & = choose_para_file_format(part_ctl%single_plt%mesh_file_fmt_ctl)
      call s_mpi_abort_by_missing_zlib                                  &
     &   (part_p%global_mesh_file%file_prefix,                          &
     &    part_p%global_mesh_file%iflag_format)
!
      call set_FEM_mesh_ctl_4_part(part_ctl%part_Fmesh,                 &
     &                             comm_part, part_p)
!
      call set_ctl_param_sleeve_extension                               &
     &   (part_ctl%Fsleeve_c, sleeve_exp_p, ierr)
!
      if(id_rank .ne. 0) return
      write(*,*) 'iflag_memory_conserve',                               &
     &          comm_part%iflag_memory_conserve
      write(*,*) 'iflag_viewer_output', part_p%iflag_viewer_output
!
      end subroutine s_set_control_4_extend_sleeve
!
! -----------------------------------------------------------------------
!
      end module set_control_4_extend_sleeve
