!>@file   analyzer_assemble_udt.f90
!!@brief  module analyzer_assemble_udt
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine init_assemble_udt
!!      subroutine analyze_assemble_udt
!!@endverbatim
!
      module analyzer_assemble_udt
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use m_control_param_newsph
!
      use t_mesh_data
      use t_phys_data
      use t_time_data
      use t_field_data_IO
      use t_control_data_4_merge
!
      use field_IO_select
!
      implicit none
!
      integer(kind = kint), save :: ndomain_org
      type(mesh_geometry), allocatable, save :: org_mesh(:)
      type(mesh_geometry), save :: new_mesh
      type(phys_data), save :: new_fld
!
      type(control_data_4_merge), save :: mgd_ctl6
      type(time_data), save :: t_IO_m
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
      subroutine init_assemble_udt
!
      use m_error_IDs
      use m_control_param_merge
      use m_array_for_send_recv
!
      use mpi_load_mesh_data
      use search_original_domain_node
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
      call read_control_4_merge(mgd_ctl6)
!
      call set_control_4_merge(mgd_ctl6, ndomain_org)
      if(set_control_4_newudt(mgd_ctl6, nprocs) .gt. 0) then
        write(e_message,'(a)')                                          &
     &     'No. of processes and targed sub domain shold be the same.'
        call calypso_mpi_abort(ierr_mesh, e_message)
      end if
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &          'istep_start, istep_end, increment_step',               &
     &           istep_start, istep_end, increment_step
!
!  set new mesh data
!
      call mpi_input_mesh_geometry                                      &
     &   (merged_mesh_file, nprocs, new_mesh, nnod_4_surf, nnod_4_edge)
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
      allocate( org_mesh(ndomain_org) )
      call load_local_node_4_merge                                      &
     &   (merge_org_mesh_file, ndomain_org, org_mesh)
!
      allocate(istack_recv(0:ndomain_org))
      allocate(item_send(new_mesh%node%internal_node))
      allocate(item_recv(new_mesh%node%internal_node))
!
      istack_recv = 0
!$omp parallel workshare
      item_send = 0
      item_recv = 0
!$omp end parallel workshare
!
      call s_search_original_domain_node(ndomain_org, org_mesh,         &
     &    new_mesh%node, istack_recv, item_send, item_recv)
!
!   read field name and number of components
!
      call sel_read_alloc_step_FEM_file(ndomain_org, izero,             &
     &    istep_start, original_ucd_param, t_IO_m, fld_IO_m)
!
      if(my_rank .eq. 0) then
        call init_field_name_4_assemble_ucd(num_nod_phys, ucd_on_label, &
     &      fld_IO_m, new_fld)
!
        call dealloc_phys_data_IO(fld_IO_m)
        call dealloc_phys_name_IO(fld_IO_m)
      end if
!
      call share_phys_field_names(new_fld)
      new_fld%num_phys_viz =  new_fld%num_phys
      new_fld%ntot_phys_viz = new_fld%ntot_phys
!
      call alloc_phys_data_type(new_mesh%node%numnod, new_fld)
!
      end subroutine init_assemble_udt
!
! ----------------------------------------------------------------------
!
      subroutine analyze_assemble_udt
!
      use t_ucd_data
!
      use m_phys_labels
      use m_control_param_merge
      use search_original_domain_node
      use set_ucd_data_to_type
      use merged_udt_vtk_file_IO
      use parallel_ucd_IO_select
      use assemble_nodal_fields
      use nod_phys_send_recv
      use load_mesh_data_4_merge
      use set_field_file_names
!
      integer(kind = kint) :: istep, icou
!
      type(field_IO), allocatable :: org_fIO(:)
!
      type(ucd_data), save :: ucd_m
      type(merged_ucd_data), save :: mucd_m
!
!
      call link_num_field_2_ucd(new_fld, ucd_m)
      call link_local_mesh_2_ucd(new_mesh%node, new_mesh%ele, ucd_m)
      call link_field_data_to_ucd(new_fld, ucd_m)
!
      allocate(org_fIO(ndomain_org))
!
      if(assemble_ucd_param%iflag_format/icent .eq. iflag_single/icent) &
     & then
        call init_merged_ucd(assemble_ucd_param%iflag_format,           &
     &      new_mesh%node, new_mesh%ele, new_mesh%nod_comm,             &
     &     ucd_m, mucd_m)
      end if
!
      if(iflag_debug .gt. .0) write(*,*) 'sel_write_parallel_ucd_mesh'
      call sel_write_parallel_ucd_mesh(assemble_ucd_param, ucd_m, mucd_m)
!
      do istep = istep_start, istep_end, increment_step
        call load_local_FEM_field_4_merge(istep, original_ucd_param,    &
     &      ndomain_org, t_IO_m, org_fIO)
!
        call assemble_field_data                                        &
     &     (ndomain_org, istack_recv, item_send, item_recv,             &
     &      new_fld, t_IO_m, org_fIO)
!
        call nod_fields_send_recv(new_mesh, new_fld)
!
        call sel_write_parallel_ucd_file                                &
     &     (istep, assemble_ucd_param, t_IO_m, ucd_m, mucd_m)
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
     &        (original_ucd_param, ndomain_org, istep)
        end do
      end if
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_assemble_udt
!
! ----------------------------------------------------------------------
!
      end module analyzer_assemble_udt
