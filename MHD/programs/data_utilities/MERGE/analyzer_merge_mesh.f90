!>@file   analyzer_merge_mesh.f90
!!@brief  module analyzer_merge_mesh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine init_merge_mesh
!!      subroutine analyze_merge_mesh
!!@endverbatim
!
      module analyzer_merge_mesh
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use m_phys_constants
      use m_geometry_data_4_merge
!
      use t_mesh_data
      use mpi_load_mesh_data
!
      implicit none
!
      type(mesh_geometry), save :: mesh_m
      type(mesh_groups), save ::   group_m
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_merge_mesh
!
      use m_error_IDs
      use m_control_param_merge
      use m_control_data_4_merge
      use m_array_for_send_recv
      use m_file_format_switch
!
      use load_mesh_data_4_merge
      use nod_phys_send_recv
      use const_element_comm_tables
      use const_mesh_information
      use assemble_nodal_fields
!
      integer(kind = kint) :: nnod_4_surf, nnod_4_edge
!
      write(*,*) 'Simulation start: PE. ', my_rank
      if(my_rank .eq. 0) then
        write(*,*) ' Do you prepare folloing data???'
        write(*,*) ' original mesh data:  mesh/in.PE#'
        write(*,*) ' control data for this routine:  control_merge'
      end if
!
!   read control data
!
      call read_control_4_merge
      call set_control_4_merge(mgd_mesh1%num_pe)
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &          'istep_start, istep_end, increment_step',               &
     &           istep_start, istep_end, increment_step
!
!  set mesh data
!
      call mpi_input_mesh(merge_org_mesh_file, nprocs,                  &
     &    mesh_m, group_m, nnod_4_surf, nnod_4_edge)
      call set_nod_and_ele_infos(mesh_m%node, mesh_m%ele)
      call const_global_numnod_list(mesh_m%node)
      call const_global_numele_list(mesh_m%ele)
!
!  Initialize communicator
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(n_sym_tensor, mesh_m%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(mesh_m)
!
      if(merged_mesh_file%iflag_format/icent .ne. iflag_single/icent)   &
     & then
        merged_mesh_file%iflag_format                                   &
     &       = merged_mesh_file%iflag_format + 100
      end if
!
      end subroutine init_merge_mesh
!
! ----------------------------------------------------------------------
!
      subroutine analyze_merge_mesh
!
      use m_control_param_merge
!
      call mpi_output_mesh(merged_mesh_file, mesh_m, group_m)
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit analyze_merge_mesh'
!
      end subroutine analyze_merge_mesh
!
! ----------------------------------------------------------------------
!
      end module analyzer_merge_mesh
