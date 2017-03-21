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
      use t_VIZ_step_parameter
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
      use m_t_step_parameter
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
      call s_set_fixed_time_step_params(t_pu_ctl, init_d1, finish_d1,   &
     &    rst_step_U, ucd_step_U, ierr, e_message)
      call viz_fixed_time_step_params(init_d1%dt, t_pu_ctl, viz_step_U)
      time_d1%dt = init_d1%dt
!
!     --------------------- 
!
      call mesh_setup_4_FEM_UTIL(mesh_file_FUTIL)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_field_id_4_product'
      call set_field_id_4_product                                       &
     &   (femmesh_FUTIL%mesh%node%numnod, time_IO_FUTIL, ucd_step_U)
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
      integer(kind=kint ) :: istep
!
!
      do istep = init_d1%i_time_step, finish_d1%i_end_step
        if (output_IO_flag(istep,ucd_step_U) .eq. izero) then
          ucd_step_U%istep_file = istep / ucd_step_U%increment
          call set_data_for_product(femmesh_FUTIL%mesh%node%numnod,     &
     &        ucd_step_U%istep_file, time_IO_FUTIL)
          call cal_rev_of_2nd_field(femmesh_FUTIL%mesh%node%numnod)
          call cal_products_of_fields                                   &
     &       (femmesh_FUTIL%mesh%nod_comm, femmesh_FUTIL%mesh%node,     &
     &        field_FUTIL%ntot_phys, field_FUTIL%d_fld)
!
!    output udt data
          call link_output_ucd_file_once                                &
     &       (my_rank, ucd_step_U%istep_file,                           &
     &        ifmt_result_udt_file, result_udt_file_head,               &
     &        field_FUTIL, time_IO_FUTIL)
!
        end if
      end do
!
      end subroutine analyze_udt_ratio
!
! ----------------------------------------------------------------------
!
      end module analyzer_udt_ratio

