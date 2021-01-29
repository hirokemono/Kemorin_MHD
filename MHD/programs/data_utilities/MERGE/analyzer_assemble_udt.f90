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
!
      use t_mesh_data
      use t_phys_data
      use t_time_data
      use t_field_data_IO
      use t_control_data_4_merge
      use t_control_param_assemble
      use t_comm_table_4_assemble
      use m_array_for_send_recv
!
      use field_IO_select
      use assemble_nodal_fields
      use set_control_assemble
!
      implicit none
!
      integer, save :: ndomain_org
      type(mesh_geometry), allocatable, save :: org_mesh(:)
      type(mesh_geometry), save :: new_mesh
      type(phys_data), save :: new_fld
      type(control_data_4_merge), save :: mgd_ctl_u
      type(control_param_assemble), save :: asbl_param_u
      type(comm_table_4_assemble), save :: asbl_comm_u
      type(assemble_field_list), save :: asbl_tbl_u
!
      type(time_data), save :: t_IO_m
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
      use m_array_for_send_recv
!
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use const_element_comm_tables
      use const_mesh_information
      use share_field_data
      use load_mesh_data_4_merge
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
      if(my_rank .eq. 0) call read_control_4_merge(mgd_ctl_u)
      call bcast_merge_control_data(mgd_ctl_u)
!
      call set_control_4_merge(mgd_ctl_u, asbl_param_u, ndomain_org)
      call set_control_4_newudt                                         &
     &   (nprocs, mgd_ctl_u, asbl_param_u, ierr)
      call set_assemble_field_list(mgd_ctl_u, asbl_tbl_u)
      if(ierr_MPI .gt. 0) then
        write(e_message,'(a)')                                          &
     &     'No. of processes and targed sub domain shold be the same.'
        call calypso_mpi_abort(ierr_mesh, e_message)
      end if
!
!  set new mesh data
!
      call mpi_input_mesh_geometry                                      &
     &   (asbl_param_u%new_mesh_file, nprocs, new_mesh)
      call set_nod_and_ele_infos(new_mesh%node, new_mesh%ele)
      call const_global_numnod_list(new_mesh%node)
      call const_global_numele_list(new_mesh%ele)
!
!  Initialize communicator
!
      if (iflag_debug.gt.0 ) write(*,*) 'alloc_iccgN_vec_type'
      call alloc_iccgN_vec_type                                         &
     &   (n_sym_tensor, new_mesh%node%numnod, vect1)
!
      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(new_mesh)
!
!  set original mesh data
!
      allocate( org_mesh(ndomain_org) )
      call load_local_node_4_merge                                      &
     &   (asbl_param_u%org_mesh_file, ndomain_org, org_mesh)
!
      call s_search_original_domain_node(ndomain_org, org_mesh,         &
     &    new_mesh%node, asbl_comm_u)
!
!   read field name and number of components
!
      call sel_read_alloc_step_FEM_file                                 &
     &   (ndomain_org, 0,  asbl_param_u%istep_start,                    &
     &    asbl_param_u%org_fld_file, t_IO_m, fld_IO_m)
!
      if(my_rank .eq. 0) then
        call init_field_name_4_assemble_ucd                             &
     &     (asbl_tbl_u, fld_IO_m, new_fld)
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
      use set_ucd_data_to_type
      use merged_udt_vtk_file_IO
      use parallel_ucd_IO_select
      use nod_phys_send_recv
      use load_mesh_data_4_merge
      use set_field_file_names
      use share_field_data
!
      integer(kind = kint) :: istep, icou
!
      type(field_IO), allocatable :: org_fIO(:)
!
      type(ucd_data), save :: ucd_m
!
!
      call link_local_mesh_2_ucd(new_mesh%node, new_mesh%ele, ucd_m)
      call link_field_data_to_ucd(new_fld, ucd_m)
!
      allocate(org_fIO(ndomain_org))
!
      if(asbl_param_u%new_fld_file%iflag_format/icent                   &
     &       .eq. iflag_single/icent) then
        call init_merged_ucd_element                                    &
     &     (asbl_param_u%new_fld_file%iflag_format,                     &
     &      new_mesh%node, new_mesh%ele, new_mesh%nod_comm, ucd_m)
      end if
!
      if(iflag_debug .gt. .0) write(*,*) 'sel_write_parallel_ucd_mesh'
      call sel_write_parallel_ucd_mesh                                  &
     &   (asbl_param_u%new_fld_file, ucd_m)
!
      do istep = asbl_param_u%istep_start, asbl_param_u%istep_end,      &
     &          asbl_param_u%increment_step
        call load_local_FEM_field_4_merge(istep,                        &
     &      asbl_param_u%org_fld_file,  ndomain_org, t_IO_m, org_fIO)
!
        call share_time_step_data(t_IO_m)
        call assemble_field_data                                        &
     &     (ndomain_org, asbl_comm_u, new_fld, org_fIO)
!
        call nod_fields_send_recv(new_mesh, new_fld, vect1)
!
        call sel_write_parallel_ucd_file                                &
     &     (istep, asbl_param_u%new_fld_file, t_IO_m, ucd_m)
      end do
      call dealloc_comm_table_4_assemble(asbl_comm_u)
!
      if(asbl_param_u%iflag_delete_org .gt. 0) then
        icou = 0
        do istep = asbl_param_u%istep_start, asbl_param_u%istep_end,    &
     &            asbl_param_u%increment_step
          icou = icou + 1
          if(mod(icou,nprocs) .ne. my_rank) cycle
          call delete_FEM_fld_file                                      &
     &       (asbl_param_u%org_fld_file, ndomain_org, istep)
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
