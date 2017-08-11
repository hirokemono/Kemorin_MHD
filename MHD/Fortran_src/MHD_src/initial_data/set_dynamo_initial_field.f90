!
!      module set_dynamo_initial_field
!
!      programmed by H.Matsui and H.Okuda on July 2000 (ver 1.1)
!      modified by H. Matsui on July, 2006
!      modified by H. Matsui on Dec., 2007
!
!!      subroutine initial_data_control                                 &
!!     &         (MHD_files, rst_step, ref_param_T,                     &
!!     &          node, ele, fluid, cd_prop, iphys, layer_tbl,          &
!!     &          SGS_par, FEM_SGS_wk, sgs_coefs, diff_coefs,           &
!!     &          nod_fld, flex_p, init_d, time_d)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(time_data), intent(inout) :: init_d, time_d
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!        type(flexible_stepping_parameter), intent(inout) :: flex_p
!!
!!      subroutine set_time_init
!
      module set_dynamo_initial_field
! j
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
      use t_reference_scalar_param
      use t_physical_property
      use t_MHD_file_parameter
      use t_IO_step_parameter
!
      implicit none
!
      private :: set_initial_data
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine initial_data_control                                   &
     &         (MHD_files, rst_step, ref_param_T,                       &
     &          node, ele, fluid, cd_prop, iphys, layer_tbl,            &
     &          SGS_par, FEM_SGS_wk, sgs_coefs, diff_coefs,             &
     &          nod_fld, flex_p, init_d, time_d)
!
      use m_initial_field_control
      use m_fem_mhd_restart
!
      use t_time_data
      use t_SGS_control_parameter
      use t_layering_ele_list
      use t_flex_delta_t_data
      use t_work_FEM_SGS_MHD
!
      use set_restart_data
      use fem_mhd_rst_IO_control
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(IO_step_param), intent(in) :: rst_step
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
      type(time_data), intent(inout) :: init_d, time_d
      type(phys_data), intent(inout) :: nod_fld
      type(flexible_stepping_parameter), intent(inout) :: flex_p
!
!
      if(iflag_restart .eq. i_rst_by_file) then
        call input_MHD_restart_file_ctl(MHD_files, rst_step,            &
     &      layer_tbl, node, ele, fluid, SGS_par,                       &
     &      FEM_SGS_wk%wk_sgs, FEM_SGS_wk%wk_diff, sgs_coefs,           &
     &      diff_coefs, nod_fld, init_d, time_d, flex_p)
      else
        call set_initial_data                                           &
     &     (cd_prop, ref_param_T, node, fluid, iphys, nod_fld)
      end if
!
      if (iflag_debug .gt. 1)  write(*,*) 'init_MHD_restart_output'
      call init_MHD_restart_output(node, nod_fld)
!
      call copy_time_step_data(init_d, time_d)
!
      if(flex_p%iflag_flexible_step .eq. iflag_flex_step) then
        flex_p%istep_max_dt = nint(init_d%time / flex_p%dt_max)
        flex_p%interval_flex_2_max = nint(flex_p%dt_max / time_d%dt)
        flex_p%istep_flex_to_max = izero
      else
        flex_p%istep_max_dt = time_d%i_time_step
        flex_p%interval_flex_2_max = ione
        flex_p%istep_flex_to_max = izero
      end if
!
      end subroutine initial_data_control
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_data                                       &
     &         (cd_prop, ref_param_T, node, fluid, iphys, nod_fld)
!
      use calypso_mpi
      use m_error_IDs
      use m_initial_field_control
!
      use set_initial_rotation
      use dynamobench_initial_temp
      use set_initial_for_MHD
!
      type(conductive_property), intent(in) :: cd_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(node_data), intent(in) :: node
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: isig
!
!
!   for dynamo benchmark case 0
!
      if (iflag_restart .eq. i_rst_dbench0) then
        isig = 400
        call set_initial_temp                                           &
     &     (isig, ref_param_T%depth_top, ref_param_T%depth_bottom,      &
     &      node, fluid%numnod_fld, fluid%inod_fld,                     &
     &      nod_fld%ntot_phys, iphys%i_velo, iphys%i_press,             &
     &      iphys%i_temp, nod_fld%d_fld)
!
!   for dynamo benchmark case 1
!
      else if (iflag_restart .eq. i_rst_dbench1                         &
     &    .or. iflag_restart .eq. i_rst_dbench2) then
        isig = 400
        call set_initial_temp                                           &
     &     (isig, ref_param_T%depth_top, ref_param_T%depth_bottom,      &
     &      node, fluid%numnod_fld, fluid%inod_fld,                     &
     &      nod_fld%ntot_phys, iphys%i_velo, iphys%i_press,             &
     &      iphys%i_temp, nod_fld%d_fld)
        isig = 0
        if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
          call set_initial_vect_p                                       &
     &       (isig, ref_param_T, node, nod_fld%ntot_phys,               &
     &        iphys%i_vecp, iphys%i_magne, iphys%i_mag_p,               &
     &        nod_fld%d_fld)
        else
          call set_initial_magne                                        &
     &       (isig, ref_param_T, node, nod_fld%ntot_phys,               &
     &        iphys%i_magne, iphys%i_mag_p, nod_fld%d_fld)
        end if
!
      else if (iflag_restart .le. -100) then
        call set_initial_temp(iflag_restart,                            &
     &     ref_param_T%depth_top, ref_param_T%depth_bottom,             &
     &      node, fluid%numnod_fld, fluid%inod_fld,                     &
     &      nod_fld%ntot_phys, iphys%i_velo, iphys%i_press,             &
     &      iphys%i_temp, nod_fld%d_fld)
!
      else if (iflag_restart .eq. i_rst_rotate_x) then
        call set_initial_velo_1(node%numnod, node%xx,                   &
     &      nod_fld%ntot_phys, iphys%i_velo, iphys%i_press,             &
     &      nod_fld%d_fld)
!
      else if (iflag_restart .eq. i_rst_rotate_y) then
        call set_initial_velo_2(node%numnod, node%xx,                   &
     &      nod_fld%ntot_phys, iphys%i_velo, iphys%i_press,             &
     &      nod_fld%d_fld)
!
      else if (iflag_restart .eq. i_rst_rotate_z) then
        call set_initial_velo_3(node%numnod, node%xx,                   &
     &      nod_fld%ntot_phys, iphys%i_velo, iphys%i_press,             &
     &      nod_fld%d_fld)
!
!   for kinematic dynamo
!
      else if (iflag_restart .eq. i_rst_kinematic) then
        call set_initial_kinematic(node, fluid%numnod_fld,              &
     &      fluid%inod_fld, nod_fld%ntot_phys,                          &
     &      iphys%i_velo, iphys%i_press, iphys%i_magne, nod_fld%d_fld)
        isig = 2000
        if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
          call set_initial_vect_p                                       &
     &       (isig, ref_param_T, node, nod_fld%ntot_phys,               &
     &        iphys%i_vecp, iphys%i_magne, iphys%i_mag_p,               &
     &        nod_fld%d_fld)
        else
          call set_initial_magne                                        &
     &       (isig, ref_param_T, node, nod_fld%ntot_phys,               &
     &        iphys%i_magne, iphys%i_mag_p, nod_fld%d_fld)
        end if
!
      else if ( iflag_restart .ge. 1000  ) then
        call set_initial_temp(iflag_restart,                            &
     &      ref_param_T%depth_top, ref_param_T%depth_bottom,            &
     &      node, fluid%numnod_fld, fluid%inod_fld,                     &
     &      nod_fld%ntot_phys, iphys%i_velo, iphys%i_press,             &
     &      iphys%i_temp, nod_fld%d_fld)
        if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
          call set_initial_vect_p                                       &
     &       (iflag_restart, ref_param_T, node,                         &
     &        nod_fld%ntot_phys, iphys%i_vecp, iphys%i_magne,           &
     &        iphys%i_mag_p, nod_fld%d_fld)
        else
          call set_initial_magne(iflag_restart, ref_param_T, node,      &
     &        nod_fld%ntot_phys, iphys%i_magne, iphys%i_mag_p,          &
     &        nod_fld%d_fld)
        end if
!
      else if (iflag_restart .ne. i_rst_no_file                         &
     &   .and. iflag_restart .ne. i_rst_by_file) then
       call calypso_MPI_abort(ierr_fld,'cannot set initial data!!!')
!
      end if
!
      end subroutine set_initial_data
!
!-----------------------------------------------------------------------
!
      end module set_dynamo_initial_field
