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
!
      use t_mesh_data
      use t_calypso_mpi_IO_param
      use mpi_load_mesh_data
!
      implicit none
!
      integer(kind = kint), save :: ndomain_org
      type(mesh_data), save :: fem_m
      type(element_geometry), save :: e_mesh_m
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
!
      use load_mesh_data_4_merge
      use nod_phys_send_recv
      use const_element_comm_tables
      use const_mesh_information
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
      call set_control_4_merge(ndomain_org)
      ierr_MPI = set_control_4_newudt(nprocs)
!
!
!  set mesh data
!
      call mpi_input_mesh                                               &
     &   (merge_org_mesh_file, nprocs, fem_m, e_mesh_m)
      call set_nod_and_ele_infos(fem_m%mesh%node, fem_m%mesh%ele)
      call const_global_numnod_list(fem_m%mesh%node)
      call const_global_numele_list(fem_m%mesh%ele)
!
!  Initialize communicator
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver                                   &
     &   (n_sym_tensor, fem_m%mesh%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(fem_m%mesh)
!
      end subroutine init_merge_mesh
!
! ----------------------------------------------------------------------
!
      subroutine analyze_merge_mesh
!
      use t_para_double_numbering
      use m_phys_labels
      use m_control_param_merge
      use m_file_format_switch
      use set_field_to_restart
      use const_internal_mesh_data
      use MPI_write_single_mesh_file
!
      type(mesh_geometry), save :: new_mesh
      type(mesh_groups), save :: new_group
      type(parallel_double_numbering), save :: dbl_nod
!
!
      call alloc_double_numbering(fem_m%mesh%node%numnod, dbl_nod)
      call set_para_double_numbering                                    &
     &   (fem_m%mesh%node%internal_node, fem_m%mesh%nod_comm, dbl_nod)
!
      call s_const_internal_mesh_data                                   &
     &   (fem_m%mesh, fem_m%group, new_mesh, new_group)
!
      merged_mesh_file%iflag_format = iflag_single
      call mpi_write_merged_mesh_file(nprocs, my_rank,                  &
     &    merged_mesh_file, new_mesh, new_group, dbl_nod)
      call dealloc_node_geometry_base(new_mesh%node)
      call deallocate_ele_connect_type(new_mesh%ele)
      call dealloc_comm_table(new_mesh%nod_comm)
      call dealloc_groups_data(new_group)
!
      call dealloc_double_numbering(dbl_nod)
      call dealloc_mesh_infos(fem_m%mesh, fem_m%group)
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_merge_mesh
!
! ----------------------------------------------------------------------
!
      end module analyzer_merge_mesh
