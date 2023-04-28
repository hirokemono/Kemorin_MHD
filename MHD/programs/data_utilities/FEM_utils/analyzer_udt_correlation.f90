!analyzer_udt_correlation.f90
!      module analyzer_udt_correlation
!
!      subroutine initialize_udt_correlate
!      subroutine analyze_udt_correlate
!
!..................................................
!
!      modified by H. Matsui on Nov., 2006 
!
      module analyzer_udt_correlation
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_phys_data
      use t_mesh_data_with_pointer
      use t_layering_ele_list
      use t_work_layer_correlate
      use t_shape_functions
      use t_FEM_utils
      use t_mesh_SR
!
      use transfer_correlate_field
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
      type(mesh_data_p), save :: femmesh_p_FUT
!
      type(mesh_data_p), save :: femmesh_p_REF
      type(phys_data), save :: phys_ref
!
      type(layering_tbl), save :: layer_tbl_corr
      type(dynamic_correlation_data), save :: wk_correlate
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_udt_correlate
!
      use copy_mesh_structures
      use input_control_udt_diff
      use mpi_load_mesh_data
      use const_mesh_information
      use nod_phys_send_recv
      use append_phys_data
!
      use const_ele_layering_table
      use int_volume_of_domain
      use correlation_all_layerd_data
      use const_jacobians_3d
      use const_element_comm_tables
      use set_field_data_w_SGS
!
!
      if (my_rank.eq.0) then
        write(*,*) 'diff. udt files'
        write(*,*) 'Input file: mesh data, udt data'
      end if
!
!     --------------------- 
!
      call init_mesh_group_type(femmesh_p_FUT%group)
      call init_element_mesh_type(femmesh_p_FUT%mesh)
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_corr_udt'
      call s_input_control_corr_udt                                     &
     &   (FUTIL1%mesh_file, FUTIL1%udt_file, FUTIL1%nod_fld, time_U)
      if (iflag_debug.eq.1) write(*,*) 'mpi_input_mesh_p'
      call mpi_input_mesh_p                                             &
     &   (FUTIL1%mesh_file, femmesh_p_FUT%mesh, femmesh_p_FUT%group)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'const_layers_4_dynamic'
      call const_layers_4_dynamic                                       &
     &   (femmesh_p_FUT%group%ele_grp, layer_tbl_corr)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_iccgN_vector'
      call alloc_iccgN_vector                                           &
     &   (isix, femmesh_p_FUT%mesh%node%numnod, m_SR4%v_sol)
      call init_send_recv(femmesh_p_FUT%mesh%nod_comm,                  &
     &    m_SR4%SR_sig, m_SR4%SR_r, m_SR4%SR_i, m_SR4%SR_il)
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos_p'
      call const_mesh_infos_p                                           &
     &   (my_rank, femmesh_p_FUT%mesh, femmesh_p_FUT%group,             &
     &    m_SR4%SR_sig, m_SR4%SR_i)
      call const_global_numnod_list(femmesh_p_FUT%mesh%node)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'init_field_data_w_SGS'
      call init_field_data_w_SGS(femmesh_p_FUT%mesh%node%numnod,        &
     &    FUTIL1%nod_fld, FUTIL1%iphys, FUTIL1%iphys_LES)
!
!     --------------------- 
!
      femmesh_p_REF%mesh%nod_comm => femmesh_p_FUT%mesh%nod_comm
      femmesh_p_REF%mesh%node =>     femmesh_p_FUT%mesh%node
      femmesh_p_REF%mesh%ele =>      femmesh_p_FUT%mesh%ele
!
      femmesh_p_REF%group%nod_grp =>  femmesh_p_FUT%group%nod_grp
      femmesh_p_REF%group%ele_grp =>  femmesh_p_FUT%group%ele_grp
      femmesh_p_REF%group%surf_grp => femmesh_p_FUT%group%surf_grp
!
      call copy_field_name(FUTIL1%nod_fld, phys_ref)
      call alloc_phys_data(femmesh_p_REF%mesh%node%numnod, phys_ref)
      call allocate_vec_transfer(femmesh_p_FUT%mesh%node%numnod)
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_vol_layer'
      call const_jacobian_and_vol_layer(my_rank, nprocs,                &
     &    femmesh_p_FUT%mesh%node, femmesh_p_FUT%mesh%ele,              &
     &    femmesh_p_FUT%group%surf_grp, femmesh_p_FUT%group%infty_grp,  &
     &    FUTIL1%spfs, FUTIL1%jacobians, layer_tbl_corr)
      call dealloc_vol_shape_func(FUTIL1%spfs%spf_3d)
!
      end subroutine initialize_udt_correlate
!
! ----------------------------------------------------------------------
!
      subroutine analyze_udt_correlate
!
      use m_geometry_constants
      use m_ctl_params_4_diff_udt
      use set_ucd_data_to_type
      use output_parallel_ucd_file
      use set_ucd_data
      use ucd_IO_select
      use nod_phys_send_recv
!
      use correlation_all_layerd_data
!
      integer(kind=kint) :: istep, istep_ucd
      type(time_data) :: time_IO
!
!     ---------------------
!
      ntot_correlate =  FUTIL1%nod_fld%ntot_phys
      nlayer_correlate = layer_tbl_corr%e_grp%num_grp
      call allocate_name_layer_correlate
      call allocate_all_layer_correlate
      call alloc_work_layer_correlate(layer_tbl_corr%e_grp%num_grp,     &
     &    FUTIL1%nod_fld%ntot_phys, wk_correlate)
!
      call set_correlate_data_names(FUTIL1%nod_fld)
!
!     ---------------------
!
      do istep = time_U%init_d%i_time_step, time_U%finish_d%i_end_step
        if (output_IO_flag(istep,time_U%ucd_step) .eqv. .FALSE.) cycle
        istep_ucd = IO_step_exc_zero_inc(istep, time_U%ucd_step)
!
        call set_data_by_read_ucd_once(istep_ucd, first_ucd_param,      &
     &                                 FUTIL1%nod_fld, time_IO)
!
        call set_data_by_read_ucd_once(istep_ucd, second_ucd_param,     &
     &                                 phys_ref, time_IO)
!
        call fields_send_recv(femmesh_p_FUT%mesh%nod_comm,              &
     &      FUTIL1%nod_fld, m_SR4%v_sol, m_SR4%SR_sig, m_SR4%SR_r)
        call fields_send_recv(femmesh_p_REF%mesh%nod_comm,              &
     &      phys_ref, m_SR4%v_sol, m_SR4%SR_sig, m_SR4%SR_r)
!
!    output udt data
!
        call coord_transfer_4_1st_field                                 &
     &     (femmesh_p_FUT%mesh%node, FUTIL1%nod_fld)
        call coord_transfer_4_2nd_field(femmesh_p_FUT%mesh%node,        &
     &      phys_ref)
!
        if (iflag_debug .gt. 0) write(*,*)                              &
     &        's_correlation_all_layerd_data'
        call s_correlation_all_layerd_data                              &
     &     (femmesh_p_FUT%mesh%node, femmesh_p_FUT%mesh%ele,            &
     &      FUTIL1%nod_fld, FUTIL1%jacobians, layer_tbl_corr,           &
     &      phys_ref, wk_correlate)
!
        if (iflag_debug .gt. 0) write(*,*)                              &
     &     ' write_layerd_correlate_data', istep_ucd
        call write_layerd_correlate_data(my_rank, istep_ucd)
      end do
!
      end subroutine analyze_udt_correlate
!
! ----------------------------------------------------------------------
!
      end module analyzer_udt_correlation

