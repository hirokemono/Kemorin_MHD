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
      use m_geometry_data
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
      use nodal_vector_send_recv
      use load_mesh_data
      use const_mesh_info
      use product_udt_fields
      use set_fixed_time_step_params
      use m_geometry_data
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
      call set_ctl_params_prod_udt
      call s_set_fixed_time_step_params(ierr, e_message)
!
      if (iflag_debug.eq.1) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(isix, node1%numnod)
!
      call init_send_recv
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_element_info'
      call set_local_element_info
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
      call set_nod_and_ele_infos
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_field_id_4_product'
      call set_field_id_4_product
      call allocate_product_data(node1%numnod)
      call allocate_product_result
!
      end subroutine initialize_udt_ratio
!
! ----------------------------------------------------------------------
!
      subroutine analyze_udt_ratio
!
      use m_t_step_parameter
      use m_ctl_params_4_prod_udt
      use m_ucd_data
      use m_node_phys_data
      use set_ucd_data
      use product_udt_fields
      use ucd_IO_select
!
      integer(kind=kint ) :: istep, istep_ucd
!
!
      do istep = i_step_init, i_step_number
        if ( mod(istep,i_step_output_ucd) .eq. izero) then
          istep_ucd = istep / i_step_output_ucd
          call set_data_for_product(node1%numnod, istep_ucd)
          call cal_rev_of_2nd_field
          call cal_products_of_fields(nod_fld1%ntot_phys, d_nod)
!
!    output udt data
          call link_output_ucd_file_once(my_rank, istep_ucd,            &
     &        ifmt_result_udt_file, result_udt_file_head)
!
        end if
      end do
!
      end subroutine analyze_udt_ratio
!
! ----------------------------------------------------------------------
!
      end module analyzer_udt_ratio

