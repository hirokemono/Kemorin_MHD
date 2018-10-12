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
      use m_phys_constants
!
      use t_mesh_data
      use t_phys_data
      use t_time_data
      use t_field_data_IO
      use t_control_data_4_merge
      use t_control_param_assemble
!
      use field_IO_select
      use assemble_nodal_fields
!
      implicit none
!
      integer(kind = kint), save :: ndomain_org
      type(mesh_geometry), save :: mesh_m
      type(phys_data), save :: new_fld
!
      type(control_data_4_merge), save :: mgd_ctl_u
      type(control_param_assemble), save :: asbl_param_u
      type(assemble_field_list), save :: asbl_tbl_u
      type(time_data), save :: t_IO_m
      type(field_IO), save :: fld_IO_m
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
      use m_array_for_send_recv
!
      use mpi_load_mesh_data
      use load_mesh_data_4_merge
      use nod_phys_send_recv
      use const_element_comm_tables
      use const_mesh_information
!
      integer(kind = kint) :: nnod_4_surf, nnod_4_edge
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
      call read_control_4_merge(mgd_ctl_u)
      call set_control_4_merge(mgd_ctl_u, asbl_param_u, ndomain_org)
      call set_assemble_ucd_file_param                                  &
     &   (mgd_ctl_u%source_plt, mgd_ctl_u%assemble_plt, asbl_param_u)
      call set_assemble_step_4_ucd(mgd_ctl_u%t_mge_ctl, asbl_param_u)
      call set_assemble_field_list(mgd_ctl_u, asbl_tbl_u)

      if(ndomain_org .ne. nprocs) then
        write(e_message,'(a)')                                          &
     &     'No. of processes and original subdomain should be the same.'
        call calypso_mpi_abort(ierr_mesh, e_message)
      end if
!
!  set mesh data
!
      call mpi_input_mesh_geometry(asbl_param_u%org_mesh_file, nprocs,  &
     &    mesh_m, nnod_4_surf, nnod_4_edge)
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
!   read field name and number of components
!
      call sel_read_alloc_step_FEM_file                                 &
     &   (nprocs, my_rank, asbl_param_u%istep_start,                    &
     &    asbl_param_u%org_fld_file, t_IO_m, fld_IO_m)
!
      call init_field_name_4_assemble_ucd                               &
     &   (asbl_tbl_u, fld_IO_m, new_fld)
      call alloc_phys_data_type(mesh_m%node%numnod, new_fld)
!
      call dealloc_phys_data_IO(fld_IO_m)
      call dealloc_phys_name_IO(fld_IO_m)
!
      end subroutine init_merge_udt
!
! ----------------------------------------------------------------------
!
      subroutine analyze_merge_udt
!
      use t_ucd_data
!
      use m_phys_labels
      use m_control_param_merge
      use m_file_format_switch
      use set_field_to_restart
      use nod_phys_send_recv
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use merged_udt_vtk_file_IO
!
      integer(kind = kint) :: istep
!
      type(ucd_data), save :: ucd_m
      type(merged_ucd_data), save :: mucd_m
!
!
      call link_num_field_2_ucd(new_fld, ucd_m)
      call link_local_mesh_2_ucd(mesh_m%node, mesh_m%ele, ucd_m)
      call link_field_data_to_ucd(new_fld, ucd_m)
!
      if(asbl_param_u%new_fld_file%iflag_format/icent                   &
     &      .ne. iflag_single/icent) then
        asbl_param_u%new_fld_file%iflag_format                          &
     &       = asbl_param_u%new_fld_file%iflag_format + 100
      end if
!
      call init_merged_ucd(asbl_param_u%new_fld_file%iflag_format,      &
     &    mesh_m%node, mesh_m%ele, mesh_m%nod_comm, ucd_m, mucd_m)
!
      if(iflag_debug .gt. .0) write(*,*) 'sel_write_parallel_ucd_mesh'
      call sel_write_parallel_ucd_mesh                                  &
     &   (asbl_param_u%new_fld_file, ucd_m, mucd_m)
!
      do istep = asbl_param_u%istep_start, asbl_param_u%istep_end,      &
     &          asbl_param_u%increment_step
        call sel_read_alloc_step_FEM_file(nprocs, my_rank,              &
     &      istep, asbl_param_u%org_fld_file, t_IO_m, fld_IO_m)
!
        call copy_field_data_from_restart                               &
     &     (mesh_m%node, fld_IO_m, new_fld)
        call dealloc_phys_data_IO(fld_IO_m)
        call dealloc_phys_name_IO(fld_IO_m)
!
        call nod_fields_send_recv(mesh_m, new_fld)
!
        call sel_write_parallel_ucd_file                                &
     &     (istep, asbl_param_u%new_fld_file, t_IO_m, ucd_m, mucd_m)
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
