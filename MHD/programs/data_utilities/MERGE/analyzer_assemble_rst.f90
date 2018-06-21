!>@file   analyzer_assemble_rst.f90
!!@brief  module analyzer_assemble_rst
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine init_assemble_rst
!!      subroutine analyze_assemble_rst
!!@endverbatim
!
      module analyzer_assemble_rst
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_geometry_data_4_merge
      use m_machine_parameter
      use m_control_param_newsph
      use t_assembled_field_IO
!
      use t_mesh_data
      use t_phys_data
      use t_time_data
      use t_field_data_IO
!
      use field_IO_select
      use set_field_to_restart
!
      implicit none
!
      type(mesh_geometry), allocatable, save :: org_mesh(:)
      type(mesh_geometry), save :: new_mesh
      type(phys_data), save :: new_fld
!
      type(time_data), save :: t_IO_m
      type(field_IO), save :: new_fIO
!
      integer(kind = kint), allocatable :: istack_recv(:)
      integer(kind = kint), allocatable :: item_send(:)
      integer(kind = kint), allocatable :: item_recv(:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_assemble_rst
!
      use m_error_IDs
      use m_control_param_merge
      use m_control_data_4_merge
      use m_array_for_send_recv
!
      use mpi_load_mesh_data
      use search_original_domain_node
      use output_newdomain_ucd
      use nod_phys_send_recv
      use const_element_comm_tables
      use const_mesh_information
      use share_field_data
      use assemble_nodal_fields
      use load_mesh_data_4_merge
!
      integer(kind = kint) :: nnod_4_surf, nnod_4_edge
      type(field_IO), save :: fld_IO_m
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      if(my_rank .eq. 0) then
        write(*,*) ' Do you prepare folloing data???'
        write(*,*) ' original mesh data:  mesh/in.PE#'
        write(*,*) ' transfered mesh data:  mesh_target/in.PE#'
        write(*,*) ' control data for this routine:  control_merge'
        write(*,*) ' simulation results: field/out.step#.PE#.udt'
        write(*,*) ' transfered results: field_new/out.step#.PE#.udt'
      end if
!
!   read control data
!
      call read_control_4_merge
!
      call set_control_4_merge(mgd_mesh1%num_pe)
      call set_control_4_newrst(sec_mesh1%num_pe2)
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &          'istep_start, istep_end, increment_step',               &
     &           istep_start, istep_end, increment_step
!
!  set new mesh data
!
      call mpi_input_mesh_geometry                                      &
     &   (nprocs, merged_mesh_file, new_mesh, nnod_4_surf, nnod_4_edge)
      call set_nod_and_ele_infos(new_mesh%node, new_mesh%ele)
      call const_global_numnod_list(new_mesh%node)
      call const_global_numele_list(new_mesh%ele)
!
!  Initialize communicator
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver                                   &
     &   (n_sym_tensor, new_mesh%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(new_mesh)
!
!  set original mesh data
!
      allocate( org_mesh(mgd_mesh1%num_pe) )
      call load_local_node_4_merge                                      &
     &   (merge_org_mesh_file, mgd_mesh1%num_pe, org_mesh)
!
      allocate(istack_recv(0:mgd_mesh1%num_pe))
      allocate(item_send(new_mesh%node%internal_node))
      allocate(item_recv(new_mesh%node%internal_node))
!
      istack_recv = 0
!$omp parallel workshare
      item_send = 0
      item_recv = 0
!$omp end parallel workshare
!
      call s_search_original_domain_node(mgd_mesh1%num_pe, org_mesh,    &
     &    new_mesh%node, istack_recv, item_send, item_recv)
!
!   read field name and number of components
!
      call sel_read_alloc_step_FEM_file(mgd_mesh1%num_pe, izero,        &
     &    istep_start, org_fst_param, t_IO_m, fld_IO_m)
!
      if(my_rank .eq. 0) then
        call init_field_name_4_assemble_rst(fld_IO_m, new_fld)
!
        call dealloc_phys_data_IO(fld_IO_m)
        call dealloc_phys_name_IO(fld_IO_m)
      end if
!
      call share_phys_field_names(new_fld)
!
      call alloc_phys_data_type(new_mesh%node%numnod, new_fld)
!
      call simple_init_fld_name_to_rst                                  &
     &   (new_mesh%node%numnod, new_fld, new_fIO)
!
      end subroutine init_assemble_rst
!
! ----------------------------------------------------------------------
!
      subroutine analyze_assemble_rst
!
      use m_phys_labels
      use m_control_param_merge
      use search_original_domain_node
      use assemble_nodal_fields
      use nod_phys_send_recv
      use load_mesh_data_4_merge
      use set_field_file_names
!
      integer(kind = kint) :: istep, icou
!
      type(field_IO), allocatable :: org_fIO(:)
!
!
      allocate(org_fIO(mgd_mesh1%num_pe))
!
      do istep = istep_start, istep_end, increment_step
        call load_local_FEM_field_4_merge(istep, org_fst_param,         &
     &      mgd_mesh1%num_pe, t_IO_m, org_fIO)
!
        call assemble_field_data                                        &
     &     (mgd_mesh1%num_pe, istack_recv, item_send, item_recv,        &
     &      new_fld, t_IO_m, org_fIO)
!
        call nod_fields_send_recv(new_mesh, new_fld)
!
        call simple_copy_fld_data_to_rst                                &
     &     (new_mesh%node, new_fld, new_fIO)
        call sel_write_step_FEM_field_file                              &
     &     (nprocs, my_rank, istep, new_fst_param, t_IO_m, new_fIO)
      end do
!
!
      deallocate(istack_recv, item_send, item_recv)
!
      if(iflag_delete_org .gt. 0) then
        icou = 0
        do istep = istep_start, istep_end, increment_step
          icou = icou + 1
          if(mod(icou,nprocs) .ne. my_rank) cycle
          call delete_FEM_fld_file                                      &
     &        (org_fst_param, mgd_mesh1%num_pe, istep)
        end do
      end if
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_assemble_rst
!
! ----------------------------------------------------------------------
!
      end module analyzer_assemble_rst
