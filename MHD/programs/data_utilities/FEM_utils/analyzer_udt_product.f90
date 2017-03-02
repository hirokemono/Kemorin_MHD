!analyzer_udt_product.f90
!      module analyzer_udt_product
!
!      Written by H. Matsui on Dec., 2007
!
!
!      subroutine initialize_udt_product
!      subroutine analyze_udt_product
!
!..................................................
!
      module analyzer_udt_product
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
      subroutine initialize_udt_product
!
      use m_array_for_send_recv
      use m_ctl_params_4_prod_udt
      use m_ctl_data_product_udt
      use m_t_step_parameter
      use nod_phys_send_recv
      use load_mesh_data
      use const_mesh_information
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
      call s_set_fixed_time_step_params                                 &
     &   (t_pu_ctl, viz_step1, ierr, e_message)
!
!     --------------------- 
!
      call mesh_setup_4_FEM_UTIL(mesh_file_FUTIL)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_field_id_4_product'
      call set_field_id_4_product                                       &
     &   (femmesh_FUTIL%mesh%node%numnod, time_IO_FUTIL)
      call allocate_product_data(femmesh_FUTIL%mesh%node%numnod)
      call allocate_product_result(field_FUTIL)
!
      end subroutine initialize_udt_product
!
! ----------------------------------------------------------------------
!
      subroutine analyze_udt_product
!
      use m_t_step_parameter
      use m_ctl_params_4_prod_udt
      use set_ucd_data
      use product_udt_fields
      use ucd_IO_select
      use output_parallel_ucd_file
!
      integer(kind=kint ) :: istep
!
!
      do istep = i_step_init, i_step_number
        if ( output_flag(istep,ucd_step1%increment) .eq. izero) then
          ucd_step1%istep_file = istep / ucd_step1%increment
          call set_data_for_product(femmesh_FUTIL%mesh%node%numnod,     &
     &        ucd_step1%istep_file, time_IO_FUTIL)
!
          call cal_products_of_fields                                   &
     &       (femmesh_FUTIL%mesh%nod_comm, femmesh_FUTIL%mesh%node,     &
     &        field_FUTIL%ntot_phys, field_FUTIL%d_fld)
!
!    output udt data
          call link_output_ucd_file_once(my_rank, ucd_step1%istep_file, &
     &        ifmt_result_udt_file, result_udt_file_head,               &
     &        field_FUTIL, time_IO_FUTIL)
        end if
      end do
!
      end subroutine analyze_udt_product
!
! ----------------------------------------------------------------------
!
      end module analyzer_udt_product

