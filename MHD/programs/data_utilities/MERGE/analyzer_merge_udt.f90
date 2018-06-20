!>@file   analyzer_merge_udt.f90
!!@brief  module analyzer_merge_udt
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine init_merge_udt
!!      subroutine analyze_merge_udt
!!@endverbatim
!
      module analyzer_merge_udt
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use t_field_data_IO
      use t_assembled_field_IO
      use m_geometry_data_4_merge
!
      use t_mesh_data
      use t_time_data
      use t_ucd_data
!
      implicit none
!
      type(mesh_geometry), save :: mesh_m
      type(ucd_data), save :: second_ucd
!
!>        Instance for FEM field data IO
      type(time_data), save :: fem_time_IO
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_merge_udt
!
      use m_error_IDs
      use m_control_param_merge
      use m_control_data_4_merge
      use m_array_for_send_recv
!
      use m_original_ucd_4_merge
      use mpi_load_mesh_data
      use load_mesh_data_4_merge
      use output_newdomain_ucd
      use nod_phys_send_recv
!
      integer(kind = kint) :: ip, jp, irank_new, jloop, inod
      integer(kind = kint_gl) :: min_inod_gl, max_inod_gl
      integer(kind = kint) :: nnod_4_surf, nnod_4_edge
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      if(my_rank .eq. 0) then
        write(*,*) ' Do you prepare folloing data???'
        write(*,*) ' original mesh data:  mesh/in.PE#'
        write(*,*) ' control data for this routine:  control_merge'
        write(*,*) ' field data: field_new/out.step#.PE#.udt'
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
      call mpi_input_mesh_geometry                                      &
     &   (nprocs, merged_mesh_file, mesh_m, nnod_4_surf, nnod_4_edge)
!
!   read field name and number of components
!
      write(*,*) 'init_ucd_data_4_merge'
      call init_ucd_data_4_merge                                        &
     &   (istep_start, original_ucd_param, fem_time_IO)
!
!    set list array for merged field
!
      call set_field_list_4_merge(mgd_mesh1%merged_fld)
      write(*,*) 'set_field_list_4_merge'
      call alloc_phys_data_type                                         &
     &   (mesh_m%node%numnod, mgd_mesh1%merged_fld)
!
      write(*,*) 'assemble_2nd_udt_mesh'
      call assemble_2nd_udt_mesh                                        &
     &   (assemble_ucd_param, mgd_mesh1%merged, my_rank,                &
     &    mesh_m, second_ucd)
!
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(n_sym_tensor, mesh_m%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(mesh_m)
!
      end subroutine init_merge_udt
!
! ----------------------------------------------------------------------
!
      subroutine analyze_merge_udt
!
      use m_phys_labels
      use m_control_param_merge
      use search_original_domain_node
!
      integer(kind = kint) :: istep, icou
      integer(kind = kint) :: ip, jp, irank_new
      integer(kind = kint) :: iloop, jloop
      integer(kind = kint) :: istep_out
!
      type(phys_data) :: new_fld
      type(field_IO) :: new_fIO
!
      return
!
      call alloc_phys_name_type(new_fld)
      call alloc_phys_data_type(mesh_m%node%numnod, new_fld)
!
      call alloc_phys_data_IO(new_fIO)
      call alloc_merged_field_stack(nprocs, new_fIO)
!      call count_number_of_node_stack                                   &
!     &   (new_fIO%nnod_IO, new_fIO%istack_numnod_IO)
!
      call link_num_field_2_ucd(new_fld, ucd)
      call link_local_mesh_2_ucd(mesh_m%node, mesh_m%ele, ucd)
      call link_field_data_to_ucd(new_fld, ucd)
!
      if (ucd_param%iflag_format/icent .eq. iflag_single/icent) then
        call init_merged_ucd                                            &
     &    (ucd_param%iflag_format, node, ele, nod_comm, fem_ucd, m_ucd)
      end if
!
      call sel_write_parallel_ucd_mesh(assemble_ucd_param, ucd, m_ucd)
!
      do istep = istep_start, istep_end, increment_step
        call sel_read_alloc_step_FEM_file(nprocs_org, (ip-1),           &
     &      istep, original_ucd_param, t_IO, org_fIO)
!
        call copy_field_data_from_restart(mesh_m%node, org_fIO, new_fld)
        call dealloc_phys_data_IO(org_fIO)
        call dealloc_phys_name_IO(org_fIO)
!
        call nod_fields_send_recv(mesh_m%node, new_fld)
!
        call sel_write_parallel_ucd_file                                &
     &     (istep, assemble_ucd_param, t_IO, fem_ucd, m_ucd)
      end do
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_merge_udt
!
! ----------------------------------------------------------------------
!
      end module analyzer_merge_udt
