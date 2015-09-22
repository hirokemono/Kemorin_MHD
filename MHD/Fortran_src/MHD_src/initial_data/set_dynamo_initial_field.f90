!
!      module set_dynamo_initial_field
!
!      programmed by H.Matsui and H.Okuda on July 2000 (ver 1.1)
!      modified by H. Matsui on July, 2006
!      modified by H. Matsui on Dec., 2007
!
!      subroutine initial_data_control
!
!      subroutine set_time_init
!      subroutine set_initial_data
!
      module set_dynamo_initial_field
!
      use m_precision
      use m_constants
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
      subroutine initial_data_control
!
      use m_machine_parameter
      use m_initial_field_control
      use m_t_int_parameter
      use m_t_step_parameter
      use m_node_phys_data
      use fem_mhd_rst_IO_control
      use set_restart_data
!
!
      if(iflag_restart .eq. i_rst_by_file) then
        call input_MHD_restart_file_ctl
      else
        call set_initial_data
      end if
      iflag_initial_step = 0
!
      if (iflag_debug .gt. 1)  write(*,*) 'init_MHD_restart_output'
      call init_MHD_restart_output
!
      time   =       time_init
      i_step_MHD =   i_step_init
!
      if(iflag_flexible_step .eq. iflag_flex_step) then
        istep_max_dt = nint(time_init / dt_max)
        i_interval_flex_2_max = nint(dt_max / dt)
        istep_flex_to_max = izero
      else
        istep_max_dt = i_step_MHD
        i_interval_flex_2_max = ione
        istep_flex_to_max = izero
      end if
!
!      if (iflag_initial_step .eq. 1) then
!        if (coef_imp_v.gt.0.0d0) coef_imp_v = 1.0d0 / coef_imp_v
!        if (coef_imp_b.gt.0.0d0) coef_imp_b = 1.0d0 / coef_imp_b
!        if (coef_imp_t.gt.0.0d0) coef_imp_t = 1.0d0 / coef_imp_t
!        if (coef_imp_c.gt.0.0d0) coef_imp_c = 1.0d0 / coef_imp_c
!      end if
!
      end subroutine initial_data_control
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_data
!
      use calypso_mpi
      use m_error_IDs
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_initial_field_control
      use m_t_step_parameter
      use m_node_phys_address
      use m_node_phys_data
      use m_physical_property
!
      use set_initial_rotation
      use dynamobench_initial_temp
      use set_initial_for_MHD
!
      integer(kind = kint) :: isig
!
!
!   for dynamo benchmark case 0
!
      if (iflag_restart .eq. i_rst_dbench0) then
        isig = 400
        call set_initial_temp(isig, node1, numnod_fluid, inod_fluid,    &
     &      nod_fld1%ntot_phys, iphys%i_velo, iphys%i_press,            &
     &      iphys%i_temp, nod_fld1%d_fld)
!
!   for dynamo benchmark case 1
!
      else if (iflag_restart .eq. i_rst_dbench1                         &
     &    .or. iflag_restart .eq. i_rst_dbench2) then
        isig = 400
        call set_initial_temp(isig, node1, numnod_fluid, inod_fluid,    &
     &      nod_fld1%ntot_phys, iphys%i_velo, iphys%i_press,            &
     &      iphys%i_temp, nod_fld1%d_fld)
        isig = 0
        if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
          call set_initial_vect_p(isig, node1, nod_fld1%ntot_phys,      &
     &        iphys%i_vecp, iphys%i_magne, iphys%i_mag_p,               &
     &        nod_fld1%d_fld)
        else
          call set_initial_magne(isig, node1, nod_fld1%ntot_phys,       &
     &        iphys%i_magne, iphys%i_mag_p, nod_fld1%d_fld)
        end if
!
      else if (iflag_restart .le. -100) then
        call set_initial_temp                                           &
     &     (iflag_restart, node1, numnod_fluid, inod_fluid,             &
     &      nod_fld1%ntot_phys, iphys%i_velo, iphys%i_press,            &
     &      iphys%i_temp, nod_fld1%d_fld)
!
      else if (iflag_restart .eq. i_rst_rotate_x) then
        call set_initial_velo_1(node1%numnod, node1%xx,                 &
     &      nod_fld1%ntot_phys, iphys%i_velo, iphys%i_press,            &
     &      nod_fld1%d_fld)
!
      else if (iflag_restart .eq. i_rst_rotate_y) then
        call set_initial_velo_2(node1%numnod, node1%xx,                 &
     &      nod_fld1%ntot_phys, iphys%i_velo, iphys%i_press,            &
     &      nod_fld1%d_fld)
!
      else if (iflag_restart .eq. i_rst_rotate_z) then
        call set_initial_velo_3(node1%numnod, node1%xx,                 &
     &      nod_fld1%ntot_phys, iphys%i_velo, iphys%i_press,            &
     &      nod_fld1%d_fld)
!
!   for kinematic dynamo
!
      else if (iflag_restart .eq. i_rst_kinematic) then
        call set_initial_kinematic                                      &
     &     (node1, numnod_fluid, inod_fluid, nod_fld1%ntot_phys,        &
     &      iphys%i_velo, iphys%i_press, iphys%i_magne, nod_fld1%d_fld)
        isig = 2000
        if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
          call set_initial_vect_p(isig, node1, nod_fld1%ntot_phys,      &
     &        iphys%i_vecp, iphys%i_magne, iphys%i_mag_p,               &
     &        nod_fld1%d_fld)
        else
          call set_initial_magne(isig, node1, nod_fld1%ntot_phys,       &
     &        iphys%i_magne, iphys%i_mag_p, nod_fld1%d_fld)
        end if
!
      else if ( iflag_restart .ge. 1000  ) then
        call set_initial_temp                                           &
     &     (iflag_restart, node1, numnod_fluid, inod_fluid,             &
     &      nod_fld1%ntot_phys, iphys%i_velo, iphys%i_press,            &
     &      iphys%i_temp, nod_fld1%d_fld)
        if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
          call set_initial_vect_p(iflag_restart, node1,                 &
     &        nod_fld1%ntot_phys, iphys%i_vecp, iphys%i_magne,          &
     &        iphys%i_mag_p, nod_fld1%d_fld)
        else
          call set_initial_magne(iflag_restart, node1,                  &
     &        nod_fld1%ntot_phys, iphys%i_magne, iphys%i_mag_p,         &
     &        nod_fld1%d_fld)
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
