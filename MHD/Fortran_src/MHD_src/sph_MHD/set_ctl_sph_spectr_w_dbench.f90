!>@file   set_ctl_sph_spectr_w_dbench.f90
!!        module set_ctl_sph_spectr_w_dbench
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!>@brief control date for volume averaged spectr data
!!
!!@verbatim
!!      subroutine s_set_ctl_sph_spectr_w_dbench                        &
!!     &         (smonitor_ctl, dbench_ctl, MHD_BC, pwr, bench)
!!        type(sph_monitor_control), intent(in) :: smonitor_ctl
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(dynamobench_monitor), intent(inout) :: bench
!!@endverbatim
      module set_ctl_sph_spectr_w_dbench
!
      use m_precision
!
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_dynamobench
      use t_field_4_dynamobench
      use t_rms_4_sph_spectr
      use t_bc_data_list
!
      implicit none
!
      private :: add_ctl_params_v_spec_w_dbench
      private :: find_conductive_inner_core_bc
      private :: find_rotatable_inner_core_bc
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_ctl_sph_spectr_w_dbench                          &
     &         (smonitor_ctl, MHD_BC, pwr, bench)
!
      use set_control_sph_spectr
!
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(MHD_BC_lists), intent(in) :: MHD_BC
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(dynamobench_monitor), intent(inout) :: bench
!
      integer(kind = kint) :: num_vspec, inum
!
!
      num_vspec = 1
      if(smonitor_ctl%num_vspec_ctl .gt. 0) then
        num_vspec = smonitor_ctl%num_vspec_ctl + num_vspec
      end if
      if(smonitor_ctl%dbench_ctl%dynamobench_file_ctl%iflag             &
     &                                                   .gt. 0) then
        if(find_conductive_inner_core_bc(MHD_BC%magne_BC%nod_BC,        &
     &                                   MHD_BC%magne_BC%surf_BC))      &
     &                                        num_vspec = num_vspec + 1
        if(find_rotatable_inner_core_bc(MHD_BC%velo_BC%nod_BC,          &
     &                                  MHD_BC%velo_BC%surf_BC))        &
     &                                        num_vspec = num_vspec + 1
      end if

      call alloc_volume_spectr_data(num_vspec, pwr)
      call set_ctl_params_base_vol_spectr(smonitor_ctl,                 &
     &                                    pwr%v_spectr(1))
      do inum = 1, smonitor_ctl%num_vspec_ctl
        call set_ctl_params_vol_sph_spectr(smonitor_ctl%v_pwr(inum),    &
     &                                     pwr%v_spectr(inum+1))
      end do
      call add_ctl_params_v_spec_w_dbench                               &
     &   (smonitor_ctl%num_vspec_ctl, smonitor_ctl%dbench_ctl, MHD_BC,  &
     &    pwr%num_vol_spectr, pwr%v_spectr, bench)
!
      end subroutine s_set_ctl_sph_spectr_w_dbench
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_ctl_params_v_spec_w_dbench                         &
     &         (num_vspec_ctl, dbench_ctl, MHD_BC, num_vspec,           &
     &          v_spectr, bench)
!
      use t_sph_volume_mean_square
      use t_multi_flag_labels
      use m_file_format_labels
      use skip_comment_f
!
      type(dynamobench_control), intent(in) :: dbench_ctl
      type(MHD_BC_lists), intent(in) :: MHD_BC
      integer(kind = kint), intent(in) :: num_vspec_ctl
      integer(kind = kint), intent(in) :: num_vspec
      type(sph_vol_mean_squares), intent(inout) :: v_spectr(num_vspec)
      type(dynamobench_monitor), intent(inout) :: bench
!
      character(len = kchara) :: input_flag
!
!
      bench%ipwr_ocore = 0
      bench%ipwr_icore = 0
!
      if(dbench_ctl%dynamobench_file_ctl%iflag .le. 0) return
      bench%benchmark_file_prefix                                       &
     &                 = dbench_ctl%dynamobench_file_ctl%charavalue
      bench%ipwr_ocore = num_vspec_ctl + 2
!
      bench%gzip_flag_bench = .FALSE.
      if(dbench_ctl%dynamobench_format_ctl%iflag .gt. 0) then
        input_flag = dbench_ctl%dynamobench_format_ctl%charavalue
        if(check_mul_flags(input_flag, gzip_flags))                     &
     &                     bench%gzip_flag_bench = .TRUE.
      end if
!
      v_spectr(bench%ipwr_ocore)%iflag_volume_rms_spec = 1
      v_spectr(bench%ipwr_ocore)%fhead_rms_v = 'NO_FILE'
      v_spectr(bench%ipwr_ocore)%gzip_flag_vol_spec = .FALSE.
!
      v_spectr(bench%ipwr_ocore)%iflag_volume_ave_sph = 0
      v_spectr(bench%ipwr_ocore)%fhead_ave = 'NO_FILE'
!
      v_spectr(bench%ipwr_ocore)%r_inside =   7.0d0 / 13.0d0
      v_spectr(bench%ipwr_ocore)%r_outside = 20.0d0 / 13.0d0
!
      if(find_conductive_inner_core_bc(MHD_BC%magne_BC%nod_BC,          &
     &                                 MHD_BC%magne_BC%surf_BC)         &
     &    .eqv. .FALSE.) return 
      if(find_conductive_inner_core_bc(MHD_BC%velo_BC%nod_BC,           &
     &                                 MHD_BC%velo_BC%surf_BC)          &
     &    .eqv. .FALSE.) return 
!
      bench%ipwr_icore = num_vspec_ctl + 3
      v_spectr(bench%ipwr_icore)%iflag_volume_rms_spec = 1
      v_spectr(bench%ipwr_icore)%fhead_rms_v = 'NO_FILE'
      v_spectr(bench%ipwr_icore)%gzip_flag_vol_spec = .FALSE.
!
      v_spectr(bench%ipwr_icore)%iflag_volume_ave_sph = 0
      v_spectr(bench%ipwr_icore)%fhead_ave = 'NO_FILE'
!
      v_spectr(bench%ipwr_icore)%r_inside =  0.0d0
      v_spectr(bench%ipwr_icore)%r_outside = 7.0d0 / 13.0d0
!
      end subroutine add_ctl_params_v_spec_w_dbench
!
! -----------------------------------------------------------------------
!
      logical function find_conductive_inner_core_bc(magne_nod,         &
     &                                               magne_surf)
!
      use m_boundary_condition_IDs
!
      type(boundary_condition_list), intent(in) :: magne_nod
      type(boundary_condition_list), intent(in) :: magne_surf
      integer(kind = kint) :: i
!
      find_conductive_inner_core_bc = .FALSE.
      do i = 1, magne_nod%num_bc
        if(magne_nod%ibc_type(i) .eq. iflag_sph_2_center) then
          find_conductive_inner_core_bc = .TRUE.
          return
        end if
      end do
      do i = 1, magne_surf%num_bc
        if(magne_surf%ibc_type(i) .eq. iflag_sph_2_center) then
          find_conductive_inner_core_bc = .TRUE.
          return
        end if
      end do
!
      end function find_conductive_inner_core_bc
!
! -----------------------------------------------------------------------
!
      logical function find_rotatable_inner_core_bc(velo_nod,           &
     &                                              torque_surf)
!
      use m_boundary_condition_IDs
!
      type(boundary_condition_list), intent(in) :: velo_nod
      type(boundary_condition_list), intent(in) :: torque_surf
      integer(kind = kint) :: i
!
      find_rotatable_inner_core_bc = .FALSE.
      do i = 1, velo_nod%num_bc
        if(velo_nod%ibc_type(i) .eq. iflag_rotatable_icore) then
          find_rotatable_inner_core_bc = .TRUE.
          return
        end if
      end do
!
      do i = 1, torque_surf%num_bc
        if(torque_surf%ibc_type(i) .eq. iflag_rotatable_icore) then
          find_rotatable_inner_core_bc = .TRUE.
          return
        end if
      end do
!
      end function find_rotatable_inner_core_bc
!
! -----------------------------------------------------------------------
!
      end module set_ctl_sph_spectr_w_dbench
