!analyzer_MHD_lengthscale.f90
!      module analyzer_MHD_lengthscale
!
!      Written by H. Matsui on Dec., 2007
!
!
!      subroutine initialize_MHD_lscale
!      subroutine analyze_MHD_lscale
!
!..................................................
!
      module analyzer_MHD_lengthscale
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_parallel_var_dof
      use m_t_step_parameter
!
      use ucd_IO_select
      use FEM_MHD_length_scale
!
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_MHD_lscale
!
      use m_ctl_params_4_prod_udt
      use m_ctl_data_product_udt
      use m_geometry_parameter
      use m_geometry_data
      use m_control_params_2nd_files
      use nodal_vector_send_recv
      use load_mesh_data
      use const_mesh_info
      use product_udt_fields
      use set_fixed_time_step_params
!
!
      if (my_rank.eq.0) then
        write(*,*) 'Get length scale'
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
      if (iflag_debug.eq.1) write(*,*) 'allocate_iccgN_matrix'
      call allocate_iccgN_matrix(isix, numnod)
!
      call init_send_recv
!
!     --------------------- 
!
      call allocate_work_4_lscale
      write(*,*) 'find_field_address_4_lscale'
      call find_field_address_4_lscale
!
      end subroutine initialize_MHD_lscale
!
! ----------------------------------------------------------------------
!
      subroutine analyze_MHD_lscale
!
      use m_t_step_parameter
      use m_ctl_params_4_diff_udt
      use m_ucd_input_data
      use m_control_params_2nd_files
      use m_node_phys_data
      use FEM_MHD_length_scale
!
      integer(kind=kint ) :: i_step
!
!
      do i_step = i_step_init, i_step_number
        if ( mod(i_step,i_step_output_ucd) .eq. 0) then
          ucd_step = i_step / i_step_output_ucd
!
          call set_data_by_read_ucd_once(my_rank, ucd_step,             &
     &        ifmt_org_ucd, ref_udt_file_head)
!
          call const_MHD_length_scales(ucd_step)
        end if
      end do
!
      call deallocate_work_4_lscale
      call calypso_MPI_barrier(ierr)
!
      end subroutine analyze_MHD_lscale
!
! ----------------------------------------------------------------------
!
      end module analyzer_MHD_lengthscale

