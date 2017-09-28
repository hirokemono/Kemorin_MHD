!>@file   analyzer_sph_all_correlate.f90
!!@brief  module analyzer_sph_all_correlate
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!
!!@verbatim
!!      subroutine initialize_sph_all_correlate
!!      subroutine evolution_sph_all_correlate
!!@endverbatim
!
      module analyzer_sph_all_correlate
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_SPH_MHD_model_data
      use m_MHD_step_parameter
      use m_SPH_SGS_structure
      use m_mesh_data
      use m_node_phys_data
      use m_jacobians_VIZ
      use m_sph_trans_arrays_MHD
      use t_step_parameter
!
      use SPH_analyzer_back_trans
      use visualizer_all
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &                      :: corr_ctl_name = 'control_sph_correlate'
!
      private :: set_ctl_4_second_spectr_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_all_correlate
!
      use t_ctl_data_SGS_MHD
      use m_ctl_data_sph_SGS_MHD
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_bc_data_list
!
      use init_sph_MHD_elapsed_label
      use FEM_analyzer_sph_MHD_w_viz
      use input_control_sph_SGS_MHD
      use SPH_analyzer_correle_all
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      total_start = MPI_WTIME()
      call set_sph_MHD_elapsed_label
!
!   Load parameter file
!
      call start_elapsed_time(1)
      call start_elapsed_time(4)
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_SGS_MHD'
      call read_control_4_sph_SGS_MHD(corr_ctl_name, MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_dynamo'
      call input_control_SPH_dynamo(MHD_files1, bc_sph_IO1,             &
     &    MHD_ctl1, sph1, comms_sph1, sph_grps1, rj_fld1, nod_fld1,     &
     &    pwr1, SPH_SGS1, MHD_step1, SPH_model1%MHD_prop, MHD_BC1,      &
     &    trns_WK1, femmesh1, ele_mesh1)
      call set_ctl_4_second_spectr_data                                 &
     &   (MHD_ctl1%new_plt, sph_file_param2)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      call end_elapsed_time(4)
!
!     --------------------- 
!
      call start_elapsed_time(2)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_w_viz'
      call FEM_initialize_w_viz(MHD_files1, MHD_step1,                  &
     &    femmesh1%mesh, femmesh1%group, ele_mesh1,                     &
     &    iphys_nod1, nod_fld1, next_tbl_VIZ1, jacobians_VIZ1,          &
     &    range1, fem_ucd1)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_back_trans'
      call SPH_init_sph_back_trans                                      &
     &   (MHD_files1, bc_sph_IO1, iphys_nod1, SPH_model1, sph_MHD_bc1)
!        Initialize visualization
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize'
      call init_visualize(femmesh1, ele_mesh1, nod_fld1)
!
      call calypso_MPI_barrier
      call end_elapsed_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_all_correlate
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_all_correlate
!
      use m_spheric_parameter
      use m_node_phys_data
      use copy_all_fields_4_sph_trans
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_correle_all
!
      integer(kind = kint) :: visval, iflag
!
!*  -----------  set initial step data --------------
!*
      call start_elapsed_time(3)
      call s_initialize_time_step(MHD_step1%init_d, MHD_step1%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(MHD_step1%time_d)
!
        iflag = output_IO_flag(MHD_step1%time_d%i_time_step,            &
     &                         MHD_step1%rst_step)
        if(iflag .ne. 0) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_correlate_all'
        call SPH_analyze_correlate_all                                  &
     &     (MHD_step1%time_d, mhd_files1, MHD_step1)
!*
!*  -----------  output field data --------------
!*
        call start_elapsed_time(1)
        call start_elapsed_time(4)
!
        if (iflag_debug.gt.0) write(*,*) 'copy_all_field_from_trans'
        call copy_all_field_from_trans                                  &
     &     (sph1%sph_params%m_folding, sph1%sph_rtp, trns_WK1%trns_MHD, &
     &      femmesh1%mesh, nod_fld1)
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(MHD_files1,                            &
     &      femmesh1%mesh, nod_fld1, MHD_step1, visval, fem_ucd1)
!
        call end_elapsed_time(4)
!
!*  ----------- Visualization --------------
!*
        if(visval .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_all'
          call start_elapsed_time(12)
          call visualize_all(MHD_step1%viz_step, MHD_step1%time_d,      &
     &        femmesh1, ele_mesh1, nod_fld1,                            &
     &        next_tbl_VIZ1%neib_ele, jacobians_VIZ1)
          call end_elapsed_time(12)
        end if
        call end_elapsed_time(1)
!
!*  -----------  exit loop --------------
!*
        if(MHD_step1%time_d%i_time_step                                 &
     &        .ge. MHD_step1%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      call end_elapsed_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_files1, MHD_step1, range1, fem_ucd1)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
!      call SPH_finalize_snap
!
      call copy_COMM_TIME_to_elaps(num_elapsed)
      call end_elapsed_time(1)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_all_correlate
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_4_second_spectr_data(new_plt, sph_file_param2)
!
      use t_file_IO_parameter
      use t_ctl_data_4_platforms
      use m_file_format_switch
!
      type(platform_data_control), intent(in) :: new_plt
      type(field_IO_params), intent(inout) :: sph_file_param2
!
!
      call choose_para_file_format                                      &
     &   (new_plt%spectr_field_fmt_ctl, sph_file_param2%iflag_format)
!
      sph_file_param2%iflag_IO = new_plt%spectr_field_file_prefix%iflag
      if(sph_file_param2%iflag_IO .gt. 0) then
        sph_file_param2%file_prefix                                     &
     &         = new_plt%spectr_field_file_prefix%charavalue
      end if
!
      end subroutine set_ctl_4_second_spectr_data
!
!  --------------------------------------------------------------------
!
      end module analyzer_sph_all_correlate
