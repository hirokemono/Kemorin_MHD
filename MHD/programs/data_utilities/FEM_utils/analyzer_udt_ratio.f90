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
      use t_FEM_utils
      use t_step_parameter
      use t_VIZ_step_parameter
      use t_mesh_SR
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                 :: fname_prod_ctl = "ctl_prod_udt"
!
!       Structure for time stepping parameters
      type(FEM_utils), save :: FUTIL1
!>      Structure of work area for mesh communications
      type(mesh_SR) :: m_SR4
!       Structure for time stepping parameters
      type(time_step_param), save :: time_U
      type(time_data), save :: time_IO_FUTIL
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_udt_ratio
!
      use t_ctl_data_product_udt
      use nod_phys_send_recv
      use input_control_prod_udt
      use product_udt_fields
!
      type(product_udt_ctl) :: prod_udt_c1
!
!
      if (my_rank.eq.0) then
        write(*,*) 'averaging udt files'
        write(*,*) 'Input file: mesh data, udt data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_prod_udt'
      call s_input_control_prod_udt(fname_prod_ctl, prod_udt_c1,        &
     &                              FUTIL1, time_U)
!
!     --------------------- 
!
      call mesh_setup_4_FEM_UTIL                                        &
     &   (FUTIL1%mesh_file, FUTIL1%geofem, m_SR4)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_field_id_4_product'
      call set_field_id_4_product                                       &
     &   (time_U%init_d, FUTIL1%geofem%mesh%node%numnod,                &
     &    time_IO_FUTIL, time_U%ucd_step)
      call allocate_product_data(FUTIL1%geofem%mesh%node%numnod)
      call allocate_product_result(FUTIL1%nod_fld)
!
      end subroutine initialize_udt_ratio
!
! ----------------------------------------------------------------------
!
      subroutine analyze_udt_ratio
!
      use m_ctl_params_4_prod_udt
      use set_ucd_data
      use product_udt_fields
      use ucd_IO_select
      use output_parallel_ucd_file
!
      integer(kind = kint) :: istep, istep_ucd
!
!
      do istep = time_U%init_d%i_time_step, time_U%finish_d%i_end_step
        if(output_IO_flag(istep,time_U%ucd_step) .eqv. .FALSE.) cycle
        istep_ucd = IO_step_exc_zero_inc(istep, time_U%ucd_step)
!
        call set_data_for_product                                       &
     &     (FUTIL1%geofem%mesh%node%numnod, istep_ucd, time_IO_FUTIL)
        call cal_rev_of_2nd_field(FUTIL1%geofem%mesh%node%numnod)
        call cal_products_of_fields                                     &
     &     (FUTIL1%geofem%mesh%nod_comm, FUTIL1%geofem%mesh%node,       &
     &      FUTIL1%nod_fld%ntot_phys, FUTIL1%nod_fld%d_fld,             &
     &      m_SR4%v_sol, m_SR4%SR_sig, m_SR4%SR_r)
!
!    output udt data
        call link_output_ucd_file_once                                  &
     &     (istep_ucd, FUTIL1%nod_fld, output_ucd_param, time_IO_FUTIL)
      end do
!
      end subroutine analyze_udt_ratio
!
! ----------------------------------------------------------------------
!
      end module analyzer_udt_ratio

