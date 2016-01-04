!analyzer_udt_diff.f90
!
!      module analyzer_udt_diff
!
!
!      modified by H. Matsui on Nov., 2006 
!
!      subroutine initialize_udt_diff
!      subroutine analyze_udt_diff
!
!..................................................
!
      module analyzer_udt_diff
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use m_FEM_utils
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_udt_diff
!
      use m_array_for_send_recv
      use m_phys_constants
      use t_FEM_phys_data
      use input_control_udt_diff
!
!
      if (my_rank.eq.0) then
        write(*,*) 'diff. udt files'
        write(*,*) 'Input file: mesh data, udt data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_udt_diff'
      call s_input_control_udt_diff(field_FUTIL, ucd_FUTIL)
!
!     --------------------- 
!
      call mesh_setup_4_FEM_UTIL
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_field_address_type'
      call set_field_address_type                                       &
     &   (femmesh_FUTIL%mesh%node%numnod, field_FUTIL, iphys_FUTIL)
!
      end subroutine initialize_udt_diff
!
! ----------------------------------------------------------------------
!
      subroutine analyze_udt_diff
!
      use m_t_step_parameter
      use m_ctl_params_4_diff_udt
      use m_control_params_2nd_files
      use set_ucd_data_to_type
      use output_parallel_ucd_file
      use divide_phys_by_delta_t
      use nod_phys_send_recv
      use ucd_IO_select
      use output_parallel_ucd_file
!
      integer(kind = kint) :: istep, istep_ucd
!
!
!
      call link_global_mesh_2_ucd                                       &
     &   (femmesh_FUTIL%mesh%node, femmesh_FUTIL%mesh%ele, ucd_FUTIL)
!
      do istep = i_step_init, i_step_number
        if ( mod(istep,i_step_output_ucd) .eq. izero) then
!
          istep_ucd = istep / i_step_output_ucd
!
          call set_data_by_read_ucd_once(my_rank, istep_ucd,            &
     &        ifmt_org_ucd, ref_udt_file_head, field_FUTIL)
!
          call subtract_by_ucd_data(my_rank, istep_ucd,                 &
     &        ifmt_org_ucd, tgt_udt_file_head, field_FUTIL)
!
          call s_divide_phys_by_delta_t                                 &
     &       (femmesh_FUTIL%mesh%node, field_FUTIL)
!
          call nod_fields_send_recv                                     &
     &       (femmesh_FUTIL%mesh%node, femmesh_FUTIL%mesh%nod_comm,     &
     &        field_FUTIL)
!
!    output udt data
          call link_output_ucd_file_once(my_rank, istep_ucd,            &
     &        ifmt_diff_udt_file, diff_udt_file_head,                   &
     &        field_FUTIL, ucd_FUTIL)
        end if
      end do
!
      end subroutine analyze_udt_diff
!
! ----------------------------------------------------------------------
!
      end module analyzer_udt_diff

