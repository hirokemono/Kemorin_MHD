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
!
      use t_FEM_utils
      use t_VIZ_step_parameter
      use t_mesh_SR
      use ucd_IO_select
      use FEM_MHD_length_scale
!
!
      implicit none
!
!       Structure for time stepping parameters
      type(FEM_utils), save :: FUTIL1
!>      Structure of work area for mesh communications
      type(mesh_SR) :: m_SR4
!       Structure for time stepping parameters
      type(time_step_param), save :: time_U
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
      use t_ctl_data_product_udt
      use t_step_parameter
      use bcast_ctl_file_product_udt
      use product_udt_fields
!
      type(product_udt_ctl) :: prod_udt_c1
      integer(kind = kint) :: ierr
!
!
      if (my_rank.eq.0) then
        write(*,*) 'Get length scale'
        write(*,*) 'Input file: mesh data, udt data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'load_control_4_prod_udt'
      call load_control_4_prod_udt(prod_udt_c1)
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_prod_udt'
      call set_ctl_params_prod_udt(prod_udt_c1%pu_plt,                  &
     &    prod_udt_c1%org_pu_plt, prod_udt_c1%prod_ctl,                 &
     &    FUTIL1%mesh_file, FUTIL1%udt_file)
      call set_fixed_time_step_params                                   &
     &   (prod_udt_c1%prod_ctl%t_pu_ctl, time_U, ierr, e_message)
!
!     ---------------------
!
      call mesh_setup_4_FEM_UTIL                                        &
     &   (FUTIL1%mesh_file, FUTIL1%geofem,  m_SR4)
!
!     --------------------- 
!
      call allocate_work_4_lscale(FUTIL1%geofem%mesh%node%numnod)
      write(*,*) 'find_field_address_4_lscale'
      call find_field_address_4_lscale(FUTIL1%nod_fld, FUTIL1%iphys)
!
      end subroutine initialize_MHD_lscale
!
! ----------------------------------------------------------------------
!
      subroutine analyze_MHD_lscale
!
      use m_ctl_params_4_diff_udt
      use output_parallel_ucd_file
      use FEM_MHD_length_scale
!
      integer(kind=kint ) :: istep, istep_ucd
      type(time_data) :: time_IO
!
!
      do istep = time_U%init_d%i_time_step, time_U%finish_d%i_end_step
        if(output_IO_flag(istep,time_U%ucd_step) .eqv. .FALSE.) cycle
        istep_ucd = IO_step_exc_zero_inc(istep, time_U%ucd_step)
!
        call set_data_by_read_ucd_once(istep_ucd, first_ucd_param,      &
     &                                 FUTIL1%nod_fld, time_IO)
!
        call const_MHD_length_scales                                    &
     &     (FUTIL1%geofem%mesh%node, FUTIL1%iphys, FUTIL1%nod_fld,      &
     &      istep_ucd, time_IO)
      end do
!
      call deallocate_work_4_lscale
      call calypso_MPI_barrier
!
      end subroutine analyze_MHD_lscale
!
! ----------------------------------------------------------------------
!
      end module analyzer_MHD_lengthscale

