!>@file   set_control_4_pickup_sph.f90
!!        module set_control_4_pickup_sph
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!
!> @brief Set control parameter for monitoring spectrum
!!
!!@verbatim
!!      subroutine set_ctl_params_pick_sph(pwr)
!!      subroutine set_ctl_params_pick_gauss
!!
!!      subroutine set_ctl_params_no_heat_Nu(rj_fld)
!!        type(phys_data), intent(in) :: rj_fld
!!@endverbatim
!!
      module set_control_4_pickup_sph
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_pick_sph(pwr)
!
      use m_ctl_data_4_pickup_sph
      use m_pickup_sph_spectr_data
      use t_rms_4_sph_spectr
      use output_sph_m_square_file
      use skip_comment_f
!
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: inum
!
!
      iflag_layer_rms_spec =  layered_pwr_spectr_prefix%iflag
      if(iflag_layer_rms_spec .gt. 0) then
        fhead_rms_layer = layered_pwr_spectr_prefix%charavalue
      end if
!
      iflag_volume_rms_spec = volume_pwr_spectr_prefix%iflag
      if(iflag_volume_rms_spec .gt. 0) then
        fhead_rms_vol = volume_pwr_spectr_prefix%charavalue
      end if
!
      iflag_volume_ave_sph =  volume_average_prefix%iflag
      if(iflag_volume_ave_sph .gt. 0) then
        fhead_ave_vol = volume_average_prefix%charavalue
      end if
!
      if(no_flag(degree_spectr_switch%charavalue)) iflag_spectr_l = 0
      if(no_flag(order_spectr_switch%charavalue))  iflag_spectr_m = 0
      if(no_flag(diff_lm_spectr_switch%charavalue))                     &
     &                                             iflag_spectr_lm = 0
      if(no_flag(axis_spectr_switch%charavalue))   iflag_spectr_m0 = 0
!
!   set pickup layer
      if(idx_spec_layer_ctl%num .gt. 0) then
        call alloc_num_spec_layer(idx_spec_layer_ctl%num, pwr)
!
        pwr%kr_4_rms(1:pwr%nri_rms)                                     &
     &         = idx_spec_layer_ctl%ivec(1:pwr%nri_rms)
!
        call deallocate_num_spec_layer_ctl
      else
        call alloc_num_spec_layer(izero, pwr)
      end if
!
!   Define spectr pick up
!
      if(picked_mode_head_ctl%iflag .gt. 0) then
        pickup_sph_head = picked_mode_head_ctl%charavalue
      else
        pick_list1%num_modes =  0
        pick_list1%num_degree = 0
        pick_list1%num_order =  0
        pick1%num_layer = 0
        call allocate_pick_sph_mode
        call allocate_pick_sph_l
        call allocate_pick_sph_m
        call allocate_num_pick_layer
        return
      end if
!
!   set pickup mode
!
      pick_list1%num_modes = idx_pick_sph_ctl%num
      call allocate_pick_sph_mode
!
      do inum = 1, pick_list1%num_modes
        pick_list1%idx_pick_mode(inum,1) = idx_pick_sph_ctl%int1(inum)
        pick_list1%idx_pick_mode(inum,2) = idx_pick_sph_ctl%int2(inum)
      end do
      call deallocate_pick_sph_ctl
!
      pick_list1%num_order = idx_pick_sph_m_ctl%num
      call allocate_pick_sph_m
!
      do inum = 1, pick_list1%num_order
        pick_list1%idx_pick_m(inum) = idx_pick_sph_m_ctl%ivec(inum)
      end do
      call deallocate_pick_sph_m_ctl
!
!
      pick_list1%num_degree = idx_pick_sph_l_ctl%num
      if(pick_list1%num_degree .gt. 0) then
        call allocate_pick_sph_l
!
        do inum = 1, pick_list1%num_degree
          pick_list1%idx_pick_l(inum) = idx_pick_sph_l_ctl%ivec(inum)
        end do
      call deallocate_pick_sph_l_ctl
      else if(picked_mode_head_ctl%iflag .gt. 0                         &
     &   .and. pick_list1%num_order .le. 0                              &
     &   .and. pick_list1%num_modes .le. 0) then
        pick_list1%num_degree = -9999
      else 
        call allocate_pick_sph_l
      end if
!
!   set pickup layer
      pick1%num_layer = 0
      if(idx_pick_layer_ctl%num .gt. 0) then
        pick1%num_layer = idx_pick_layer_ctl%num
        call allocate_num_pick_layer
!
        do inum = 1, pick1%num_layer
          pick1%id_radius(inum) = idx_pick_layer_ctl%ivec(inum)
        end do
!
        call deallocate_num_pick_layer_ctl
      end if
!
      end subroutine set_ctl_params_pick_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_pick_gauss
!
      use m_ctl_data_4_pickup_sph
      use m_gauss_coefs_monitor_data
!
      integer(kind = kint) :: inum
!
!
!   set pickup gauss coefficients
!
      if(gauss_coefs_prefix%iflag .gt. 0) then
        gauss_coefs_file_head = gauss_coefs_prefix%charavalue
      else
        gauss_list1%num_modes =  0
        gauss_list1%num_degree = 0
        gauss_list1%num_order =  0
        call allocate_pick_gauss
        call allocate_pick_gauss_l
        call allocate_pick_gauss_m
        return
      end if
!
      r_4_gauss_coefs = 2.91
      if(gauss_coefs_radius_ctl%iflag .gt. 0) then
        r_4_gauss_coefs = gauss_coefs_radius_ctl%realvalue
      end if
!
      gauss_list1%num_modes = idx_gauss_ctl%num
      call allocate_pick_gauss
!
      do inum = 1, gauss_list1%num_modes
        gauss_list1%idx_pick_mode(inum,1) = idx_gauss_ctl%int1(inum)
        gauss_list1%idx_pick_mode(inum,2) = idx_gauss_ctl%int2(inum)
      end do
!
      if(gauss_list1%num_modes .gt. 0) call deallocate_pick_gauss_ctl
!
!
      gauss_list1%num_order = idx_gauss_m_ctl%num
      call allocate_pick_gauss_m
!
      do inum = 1, gauss_list1%num_order
        gauss_list1%idx_pick_m(inum) = idx_gauss_m_ctl%ivec(inum)
      end do
      call deallocate_pick_gauss_m_ctl
!
!
      gauss_list1%num_degree = idx_gauss_l_ctl%num
      if(gauss_list1%num_degree .gt. 0) then
        call allocate_pick_gauss_l
!
        do inum = 1, gauss_list1%num_degree
          gauss_list1%idx_pick_l(inum) = idx_gauss_l_ctl%ivec(inum)
        end do
        call deallocate_pick_gauss_l_ctl
      else if(gauss_coefs_prefix%iflag .gt. 0                           &
     &   .and. gauss_list1%num_order .le. 0                             &
     &   .and. gauss_list1%num_modes .le. 0) then
       gauss_list1%num_degree = -9999
      end if
!
      end subroutine set_ctl_params_pick_gauss
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_no_heat_Nu(rj_fld)
!
      use t_phys_data
!
      use m_ctl_data_4_pickup_sph
      use m_phys_labels
      use m_no_heat_Nusselt_num
!
      type(phys_data), intent(in) :: rj_fld
!
      integer(kind = kint) :: i
!
!    Turn On Nusselt number if temperature gradient is there
      iflag_no_source_Nu = 0
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. fhd_grad_temp) then
          iflag_no_source_Nu = 1
          exit
        end if
      end do
!
      if(Nusselt_file_prefix%iflag .gt. 0) then
        iflag_no_source_Nu = 1
        Nusselt_file_head = Nusselt_file_prefix%charavalue
      else
        iflag_no_source_Nu = 0
      end if
!
!    Turn Off Nusselt number if heat source is there
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. fhd_heat_source) then
          iflag_no_source_Nu = 0
          exit
        end if
      end do
!
      end subroutine set_ctl_params_no_heat_Nu
!
! -----------------------------------------------------------------------
!
      end module set_control_4_pickup_sph
