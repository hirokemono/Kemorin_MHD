!analyzer_udt_ratio.f90
!      module analyzer_udt_ratio
!
!      Written by H. Matsui on Dec., 2007
!
!
!      subroutine initialize_udt_ratio
!      subroutine analyze_udt_ratio
!
!..................................................
!
      module analyzer_udt_ratio
!
      use m_precision
      use m_constants
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
      subroutine initialize_udt_ratio
!
      use m_ctl_params_4_prod_udt
      use m_array_for_send_recv
      use m_ctl_data_product_udt
      use nod_phys_send_recv
      use product_udt_fields
      use set_fixed_time_step_params
!
      integer(kind = kint) :: ierr
!
!
      if (my_rank.eq.0) then
        write(*,*) 'averaging udt files'
        write(*,*) 'Input file: mesh data, udt data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_prod_udt'
      call read_control_4_prod_udt
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_prod_udt'
      call set_ctl_params_prod_udt                                      &
     &   (mesh_file_FUTIL, udt_param_FUTIL, ucd_FUTIL)
      call s_set_fixed_time_step_params(ierr, e_message)
!
!     --------------------- 
!
      call mesh_setup_4_FEM_UTIL(mesh_file_FUTIL)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_field_id_4_product'
      call set_field_id_4_product(femmesh_FUTIL%mesh%node%numnod)
      call allocate_product_data(femmesh_FUTIL%mesh%node%numnod)
      call allocate_product_result(field_FUTIL)
!
      end subroutine initialize_udt_ratio
!
! ----------------------------------------------------------------------
!
      subroutine analyze_udt_ratio
!
      use m_t_step_parameter
      use m_ctl_params_4_prod_udt
      use set_ucd_data
      use product_udt_fields
      use ucd_IO_select
      use output_parallel_ucd_file
!
      integer(kind=kint ) :: istep, istep_ucd
!
!
      do istep = i_step_init, i_step_number
        if ( mod(istep,i_step_output_ucd) .eq. izero) then
          istep_ucd = istep / i_step_output_ucd
          call set_data_for_product                                     &
     &       (femmesh_FUTIL%mesh%node%numnod, istep_ucd)
          call cal_rev_of_2nd_field(femmesh_FUTIL%mesh%node%numnod)
          call cal_products_of_fields                                   &
     &       (femmesh_FUTIL%mesh%nod_comm, femmesh_FUTIL%mesh%node,     &
     &        field_FUTIL%ntot_phys, field_FUTIL%d_fld)
!
!    output udt data
          call link_output_ucd_file_once(my_rank, istep_ucd,            &
     &        ifmt_result_udt_file, result_udt_file_head,               &
     &        field_FUTIL, ucd_FUTIL)
!
        end if
      end do
!
      end subroutine analyze_udt_ratio
!
! ----------------------------------------------------------------------
!
      end module analyzer_udt_ratio

