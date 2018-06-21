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
      use m_geometry_data_4_merge
!
      use t_mesh_data
      use t_phys_data
      use t_time_data
      use t_ucd_data
      use t_field_data_IO
!
      use field_IO_select
!
      implicit none
!
      type(mesh_geometry), save :: mesh_m
      type(phys_data), save :: new_fld
!
      type(time_data), save :: t_IO
      type(field_IO), save :: fld_IO_m
!
      type(ucd_data), save :: ucd_m
      type(merged_ucd_data), save :: mucd_m
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
      use const_element_comm_tables
      use const_mesh_information
      use cal_minmax_and_stacks
!
      integer(kind = kint) :: icou, jp, jfld, ifld, inod
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
     &   (nprocs, merge_org_mesh_file, mesh_m, nnod_4_surf, nnod_4_edge)
      call set_nod_and_ele_infos(mesh_m%node, mesh_m%ele)
      call const_global_numnod_list(mesh_m%node)
      call const_global_numele_list(mesh_m%ele)
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(n_sym_tensor, mesh_m%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(mesh_m)
!
!   read field name and number of components
!
      call sel_read_alloc_step_FEM_file(nprocs, my_rank,                &
     &    istep_start, original_ucd_param, t_IO, fld_IO_m)
!
      if(my_rank .eq. 0) then
        do jfld = 1, fld_IO_m%num_field_IO
          write(*,*) 'fld_IO_m', jfld, trim(fld_IO_m%fld_name(jfld))
        end do
      end if
!
      new_fld%num_phys = 0
      do ifld = 1, num_nod_phys
        do jfld = 1, fld_IO_m%num_field_IO
          if(ucd_on_label(ifld) .eq. fld_IO_m%fld_name(jfld)) then
            new_fld%num_phys = new_fld%num_phys + 1
            exit
          end if
        end do
      end do
!
      call alloc_phys_name_type(new_fld)
!
      icou = 0
      do ifld = 1, num_nod_phys
        do jfld = 1, fld_IO_m%num_field_IO
          if(ucd_on_label(ifld) .eq. fld_IO_m%fld_name(jfld)) then
            icou = icou + 1
            new_fld%phys_name(icou) = ucd_on_label(ifld)
            new_fld%num_component(icou) = fld_IO_m%istack_comp_IO(jfld) &
     &                               - fld_IO_m%istack_comp_IO(jfld-1)
          end if
        end do
      end do
      call s_cal_total_and_stacks                                       &
     &   (new_fld%num_phys, new_fld%num_component, izero,               &
     &    new_fld%istack_component, new_fld%ntot_phys)
      new_fld%num_phys_viz = new_fld%num_phys
      new_fld%ntot_phys_viz = new_fld%ntot_phys
!
      if(my_rank .eq. 0) then
        do ifld = 1, new_fld%num_phys
          write(*,*) 'new_fld', ifld, trim(new_fld%phys_name(ifld)),    &
     &                 new_fld%istack_component(ifld)
        end do
      end if
!
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
      use m_phys_labels
      use m_control_param_merge
      use set_field_to_restart
      use nod_phys_send_recv
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use merged_udt_vtk_file_IO
!
      integer(kind = kint) :: istep
!
!
      call link_num_field_2_ucd(new_fld, ucd_m)
      call link_local_mesh_2_ucd(mesh_m%node, mesh_m%ele, ucd_m)
      call link_field_data_to_ucd(new_fld, ucd_m)
      write(*,*) 'ucd_m', ucd_m%nnod, ucd_m%num_field, new_fld%num_phys
!
      if (assemble_ucd_param%iflag_format/icent .ne. iflag_single/icent) then
        assemble_ucd_param%iflag_format                                 &
     &       = assemble_ucd_param%iflag_format + 100
      end if
      write(*,*) 'assemble_ucd_param%iflag_format', assemble_ucd_param%iflag_format
      call init_merged_ucd(assemble_ucd_param%iflag_format,             &
     &    mesh_m%node, mesh_m%ele, mesh_m%nod_comm, ucd_m, mucd_m)
!
      if(iflag_debug .gt. .0) write(*,*) 'sel_write_parallel_ucd_mesh'
      call sel_write_parallel_ucd_mesh(assemble_ucd_param, ucd_m, mucd_m)
!
      do istep = istep_start, istep_end, increment_step
        call sel_read_alloc_step_FEM_file(nprocs, my_rank,              &
     &      istep, original_ucd_param, t_IO, fld_IO_m)
!
        call copy_field_data_from_restart                               &
     &     (mesh_m%node, fld_IO_m, new_fld)
        call dealloc_phys_data_IO(fld_IO_m)
        call dealloc_phys_name_IO(fld_IO_m)
!
        call nod_fields_send_recv(mesh_m, new_fld)
!
        call sel_write_parallel_ucd_file                                &
     &     (istep, assemble_ucd_param, t_IO, ucd_m, mucd_m)
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
