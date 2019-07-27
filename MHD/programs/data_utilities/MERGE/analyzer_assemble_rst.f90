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
      use m_machine_parameter
!
      use t_mesh_data
      use t_phys_data
      use t_time_data
      use t_field_data_IO
      use t_control_data_4_merge
      use t_control_param_assemble
      use t_comm_table_4_assemble
!
      use field_IO_select
      use set_field_to_restart
      use set_control_assemble
!
      implicit none
!
      integer, save :: ndomain_org
      type(mesh_geometry), allocatable, save :: org_mesh(:)
      type(mesh_geometry), save :: new_mesh
      type(phys_data), save :: new_fld
      type(control_data_4_merge), save :: mgd_ctl_f
      type(control_param_assemble), save :: asbl_param_f
      type(comm_table_4_assemble), save :: asbl_comm_f
!
      type(time_data), save :: t_IO_m
      type(field_IO), save :: new_fIO
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
      use bcast_4_assemble_sph_ctl
!
      integer(kind = kint) :: ierr
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
      if(my_rank .eq. 0) call read_control_4_merge(mgd_ctl_f)
      call bcast_merge_control_data(mgd_ctl_f)
!
      call set_control_4_merge(mgd_ctl_f, asbl_param_f, ndomain_org)
      call set_control_4_newrst                                         &
     &   (nprocs, mgd_ctl_f, asbl_param_f, ierr)
      if(ierr_MPI .gt. 0) then
        write(e_message,'(a)')                                          &
     &     'No. of processes and targed sub domain shold be the same.'
        call calypso_mpi_abort(ierr_mesh, e_message)
      end if
!
!  set new mesh data
!
      call mpi_input_mesh_geometry                                      &
     &   (asbl_param_f%new_mesh_file, nprocs, new_mesh)
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
     &   (asbl_param_f%org_mesh_file, ndomain_org, org_mesh)
!
      call s_search_original_domain_node(ndomain_org, org_mesh,         &
     &    new_mesh%node, asbl_comm_f)
!
!   read field name and number of components
!
      call sel_read_alloc_step_FEM_file(ndomain_org, 0,                 &
     &    asbl_param_f%istep_start, asbl_param_f%org_fld_file,          &
     &    t_IO_m, fld_IO_m)
!
      if(my_rank .eq. 0) then
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
      end subroutine init_assemble_rst
!
! ----------------------------------------------------------------------
!
      subroutine analyze_assemble_rst
!
      use m_phys_labels
      use assemble_nodal_fields
      use nod_phys_send_recv
      use load_mesh_data_4_merge
      use set_field_file_names
      use set_merged_restart_data
      use share_field_data
!
      integer(kind = kint) :: istep, icou
!
      type(field_IO), allocatable :: org_fIO(:)
!
!
      allocate(org_fIO(ndomain_org))
!
      do istep = asbl_param_f%istep_start, asbl_param_f%istep_end,      &
     &          asbl_param_f%increment_step
        call load_local_FEM_field_4_merge(istep,                        &
     &      asbl_param_f%org_fld_file, ndomain_org, t_IO_m, org_fIO)
!
        call share_time_step_data(t_IO_m)
        call assemble_field_data                                        &
     &     (ndomain_org, asbl_comm_f, new_fld, org_fIO)
!
!   re-scaling for magnetic field
        call rescale_4_magne(asbl_param_f%b_ratio, new_fld)
!
        call nod_fields_send_recv(new_mesh, new_fld)
!
        call simple_copy_fld_data_to_rst                                &
     &     (new_mesh%node, new_fld, new_fIO)
        call sel_write_step_FEM_field_file                              &
     &     (nprocs, my_rank, istep, asbl_param_f%new_fld_file,          &
     &      t_IO_m, new_fIO)
      end do
      call dealloc_comm_table_4_assemble(asbl_comm_f)
!
      if(asbl_param_f%iflag_delete_org .gt. 0) then
        icou = 0
        do istep = asbl_param_f%istep_start, asbl_param_f%istep_end,    &
     &            asbl_param_f%increment_step
          icou = icou + 1
          if(mod(icou,nprocs) .ne. my_rank) cycle
          call delete_FEM_fld_file                                      &
     &        (asbl_param_f%org_fld_file, ndomain_org, istep)
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
