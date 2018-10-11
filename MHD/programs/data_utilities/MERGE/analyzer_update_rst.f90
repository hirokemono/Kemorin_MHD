!>@file   analyzer_update_rst.f90
!!@brief  module analyzer_update_rst
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine init_update_retstart
!!      subroutine analyze_update_restart
!!@endverbatim
!
      module analyzer_update_rst
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
      use t_comm_table_4_assemble
!
      use field_IO_select
      use set_field_to_restart
!
      implicit none
!
      integer(kind = kint), save :: ndomain_org
      type(mesh_geometry), allocatable, save :: org_mesh(:)
      type(mesh_geometry), save :: new_mesh
      type(phys_data), save :: new_fld
      type(comm_table_4_assemble), save :: asbl_comm1
!
      type(control_data_4_merge), save :: mgd_ctl7
      type(time_data), save :: t_IO_m
      type(field_IO), save :: new_fIO
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_update_retstart
!
      use m_error_IDs
      use m_control_param_merge
      use m_array_for_send_recv
!
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use const_element_comm_tables
      use const_mesh_information
      use share_field_data
      use assemble_nodal_fields
      use load_mesh_data_4_merge
      use set_field_to_restart
      use input_old_file_sel_4_zlib
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
      call read_control_4_merge(mgd_ctl7)
!
      call set_control_4_merge(mgd_ctl7, ndomain_org)
      if(set_control_4_newrst(mgd_ctl7, nprocs) .gt. 0) then
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
      call s_search_original_domain_node(ndomain_org, org_mesh,         &
     &    new_mesh%node, asbl_comm1)
!
!   read field name and number of components
!
      if(my_rank .eq. 0) then
        call sel_read_rst_comps                                         &
     &     (izero, istep_start, org_fst_param, t_IO_m, fld_IO_m)
        call init_field_name_by_restart(fld_IO_m, new_fld)
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
      end subroutine init_update_retstart
!
! ----------------------------------------------------------------------
!
      subroutine analyze_update_restart
!
      use m_phys_labels
      use m_control_param_merge
      use assemble_nodal_fields
      use nod_phys_send_recv
      use load_mesh_data_4_merge
      use set_field_file_names
      use set_merged_restart_data
!
      integer(kind = kint) :: istep, icou
!
      type(field_IO), allocatable :: org_fIO(:)
!
!
      allocate(org_fIO(ndomain_org))
!
      do istep = istep_start, istep_end, increment_step
        call load_old_FEM_restart_4_merge(istep, org_fst_param,         &
     &      ndomain_org, t_IO_m, org_fIO)
!
        call assemble_field_data                                        &
     &     (ndomain_org, asbl_comm1, new_fld, t_IO_m, org_fIO)
!
!   re-scaling for magnetic field
        call rescale_4_magne(b_ratio, new_fld)
!
        call nod_fields_send_recv(new_mesh, new_fld)
!
        call simple_copy_fld_data_to_rst                                &
     &     (new_mesh%node, new_fld, new_fIO)
        call sel_write_step_FEM_field_file                              &
     &     (nprocs, my_rank, istep, new_fst_param, t_IO_m, new_fIO)
      end do
      call dealloc_comm_table_4_assemble(asbl_comm1)
!
      if(iflag_delete_org .gt. 0) then
        icou = 0
        do istep = istep_start, istep_end, increment_step
          icou = icou + 1
          if(mod(icou,nprocs) .ne. my_rank) cycle
          call delete_FEM_fld_file                                      &
     &        (org_fst_param, ndomain_org, istep)
        end do
      end if
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_update_restart
!
! ----------------------------------------------------------------------
!
      end module analyzer_update_rst
