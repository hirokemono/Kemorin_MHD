!>@file   sph_dynamic_elsasser.f90
!!        program sph_dynamic_elsasser
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Find maximum degree and order of the spectrum
!
      program sph_dynamic_elsasser
!
      use m_precision
      use m_constants
!
      use m_sph_dynamic_elsasser
      use t_read_sph_spectra
      use t_ctl_data_tave_sph_monitor
      use t_ctl_param_sph_series_util
      use t_powers_4_coefficients
      use t_list_of_dimless_numbers
      use t_powers_4_coefficients
      use skip_comment_f
!
      implicit none
!
      type(list_of_dimless), save :: dimless_list1
      type(powers_4_coefficients), save :: clist_ME_to_KE
      type(powers_4_coefficients), save :: clist_elsasser
      type(powers_4_coefficients), save :: clist_mag_Reynolds
!
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
      type(sph_spectr_file_param), save :: spec_evo_p1
      type(sph_dyn_elsasser_data), save :: els_dat1
!
!
      call read_control_file_sph_elsasser(0, tave_sph_ctl1)
!
      if(tave_sph_ctl1%Elsasser_file_prefix%iflag .gt. 0) then
        els_dat1%vol_l_spectr_file_name                                 &
     &     = tave_sph_ctl1%vol_degree_spectr_file_name%charavalue
      end if
      if(tave_sph_ctl1%Elsasser_file_prefix%iflag .gt. 0) then
        els_dat1%vol_m_spectr_file_name                                 &
     &     = tave_sph_ctl1%vol_order_spectr_file_name%charavalue
      end if
      if(tave_sph_ctl1%Elsasser_file_prefix%iflag .gt. 0) then
        els_dat1%elsasser_file_prefix                                   &
     &     = tave_sph_ctl1%Elsasser_file_prefix%charavalue
      end if
!
      els_dat1%flag_old_spectr_data = .FALSE.
      if(tave_sph_ctl1%old_format_ctl%iflag .gt. 0) then
        els_dat1%flag_old_spectr_data                                   &
     &     = yes_flag(tave_sph_ctl1%old_format_ctl%charavalue)
      end if
!
      if(tave_sph_ctl1%start_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set start time'
        stop
      end if
      els_dat1%start_time = tave_sph_ctl1%start_time_ctl%realvalue
!
      if(tave_sph_ctl1%end_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set end time'
        stop
      end if
      els_dat1%end_time = tave_sph_ctl1%end_time_ctl%realvalue
!
      dimless_list1%num = 0
      if(tave_sph_ctl1%dless_ctl%dimless%num .gt. 0) then
        dimless_list1%num = tave_sph_ctl1%dless_ctl%dimless%num
      end if
      call copy_dimless_from_ctl                                        &
     &   (tave_sph_ctl1%dless_ctl%dimless, dimless_list1)
      call check_dimless_numbers(dimless_list1)
!
      clist_ME_to_KE%num = 0
      if(tave_sph_ctl1%ME_to_KE_ctl%num .gt. 0) then
        clist_ME_to_KE%num = tave_sph_ctl1%ME_to_KE_ctl%num
      end if
      call copy_power_and_names_from_ctl                                &
     &   (tave_sph_ctl1%ME_to_KE_ctl, clist_ME_to_KE)
      call check_power_and_name_list(clist_ME_to_KE)
!
      clist_elsasser%num = 0
      if(tave_sph_ctl1%Elsasser_coefs%num .gt. 0) then
        clist_elsasser%num = tave_sph_ctl1%Elsasser_coefs%num
      end if
      call copy_power_and_names_from_ctl                                &
     &   (tave_sph_ctl1%Elsasser_coefs, clist_elsasser)
      call check_power_and_name_list(clist_elsasser)
!
      clist_mag_Reynolds%num = 0
      if(tave_sph_ctl1%mag_Re_coefs%num .gt. 0) then
        clist_mag_Reynolds%num = tave_sph_ctl1%mag_Re_coefs%num
      end if
      call copy_power_and_names_from_ctl                                &
     &   (tave_sph_ctl1%mag_Re_coefs, clist_mag_Reynolds)
      call check_power_and_name_list(clist_mag_Reynolds)
!
      els_dat1%ME_scale                                                 &
     &    = coefficient_from_dimless(dimless_list1, clist_ME_to_KE)
      els_dat1%coef_elsasser                                            &
     &    = coefficient_from_dimless(dimless_list1, clist_elsasser)
      els_dat1%mag_Re_ratio                                             &
     &    = coefficient_from_dimless(dimless_list1, clist_mag_Reynolds)
      write(*,*) 'ME_scale', els_dat1%ME_scale
      write(*,*) 'coef_elsasser', els_dat1%coef_elsasser
      write(*,*) 'mag_Re_ratio',  els_dat1%mag_Re_ratio
      call dealloc_dimless_list(dimless_list1)
!
      call dealloc_ctl_tave_sph_monitor(tave_sph_ctl1)
!
      call sph_dynamic_elsasser_by_spectr(els_dat1)
!
      call dealloc_spec_series_file_param(spec_evo_p1)
      stop
!
      end program sph_dynamic_elsasser
!
