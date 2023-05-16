!>@file   t_ctl_param_dyn_elsasser.f90
!!        module t_ctl_param_dyn_elsasser
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Obtain lengh scale from spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine set_spectr_address_4_dyn_els(sph_IN, els_dat)
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!@endverbatim
!
      module t_ctl_param_dyn_elsasser
!
      use m_precision
      use m_constants
      use m_phys_constants
      use t_read_sph_spectra
      use t_ctl_data_get_dyn_elsasser
!
      implicit none
!
      type sph_dyn_elsasser_data
        character(len = kchara)                                         &
     &            :: vol_l_spectr_file_name = 'sph_pwr_volume_l.dat'
        character(len = kchara)                                         &
     &            :: vol_m_spectr_file_name = 'sph_pwr_volume_m.dat'
        character(len = kchara) :: elsasser_file_name = 'Elsasser.dat'
!
        logical :: flag_old_spectr_data = .FALSE.
!
        real(kind = kreal) :: start_time
        real(kind = kreal) :: end_time
!
        real(kind = kreal) :: ME_scale =      1.0d0
        real(kind = kreal) :: coef_elsasser = 1.0d0
        real(kind = kreal) :: mag_Re_ratio =  1.0d0
!
        integer(kind = kint) :: irms_KE =  0
        integer(kind = kint) :: irms_ME =  0
        integer(kind = kint) :: irms_T =   0
        integer(kind = kint) :: irms_C =   0
      end type sph_dyn_elsasser_data
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_sph_dyn_elsasser_params(elsasser_ctl, els_dat)
!
      use t_powers_4_coefficients
      use t_list_of_dimless_numbers
      use skip_comment_f
!
      type(get_dyn_elsasser_ctl), intent(in) :: elsasser_ctl
      type(sph_dyn_elsasser_data), intent(inout) :: els_dat
!
      type(list_of_dimless), save :: dimless_list1
      type(powers_4_coefficients), save :: clist_ME_to_KE
      type(powers_4_coefficients), save :: clist_elsasser
      type(powers_4_coefficients), save :: clist_mag_Reynolds
!
!
      if(elsasser_ctl%vol_degree_spectr_file_name%iflag .gt. 0) then
        els_dat%vol_l_spectr_file_name                                  &
     &     = elsasser_ctl%vol_degree_spectr_file_name%charavalue
      end if
      if(elsasser_ctl%vol_order_spectr_file_name%iflag .gt. 0) then
        els_dat%vol_m_spectr_file_name                                  &
     &     = elsasser_ctl%vol_order_spectr_file_name%charavalue
      end if
      if(elsasser_ctl%Elsasser_file_name_ctl%iflag .gt. 0) then
        els_dat%elsasser_file_name                                      &
     &     = elsasser_ctl%Elsasser_file_name_ctl%charavalue
      end if
!
      els_dat%flag_old_spectr_data = .FALSE.
      if(elsasser_ctl%old_format_ctl%iflag .gt. 0) then
        els_dat%flag_old_spectr_data                                    &
     &     = yes_flag(elsasser_ctl%old_format_ctl%charavalue)
      end if
!
      if(elsasser_ctl%start_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set start time'
        stop
      end if
      els_dat%start_time = elsasser_ctl%start_time_ctl%realvalue
!
      if(elsasser_ctl%end_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set end time'
        stop
      end if
      els_dat%end_time = elsasser_ctl%end_time_ctl%realvalue
!
      dimless_list1%num = 0
      if(elsasser_ctl%dless_ctl%dimless%num .gt. 0) then
        dimless_list1%num = elsasser_ctl%dless_ctl%dimless%num
      end if
      call copy_dimless_from_ctl                                        &
     &   (elsasser_ctl%dless_ctl%dimless, dimless_list1)
      call check_dimless_numbers(dimless_list1)
!
      clist_ME_to_KE%num = 0
      if(elsasser_ctl%ME_to_KE_ctl%num .gt. 0) then
        clist_ME_to_KE%num = elsasser_ctl%ME_to_KE_ctl%num
      end if
      call copy_power_and_names_from_ctl                                &
     &   (elsasser_ctl%ME_to_KE_ctl, clist_ME_to_KE)
      call check_power_and_name_list(clist_ME_to_KE)
!
      clist_elsasser%num = 0
      if(elsasser_ctl%Elsasser_coefs%num .gt. 0) then
        clist_elsasser%num = elsasser_ctl%Elsasser_coefs%num
      end if
      call copy_power_and_names_from_ctl                                &
     &   (elsasser_ctl%Elsasser_coefs, clist_elsasser)
      call check_power_and_name_list(clist_elsasser)
!
      clist_mag_Reynolds%num = 0
      if(elsasser_ctl%mag_Re_coefs%num .gt. 0) then
        clist_mag_Reynolds%num = elsasser_ctl%mag_Re_coefs%num
      end if
      call copy_power_and_names_from_ctl                                &
     &   (elsasser_ctl%mag_Re_coefs, clist_mag_Reynolds)
      call check_power_and_name_list(clist_mag_Reynolds)
!
      els_dat%ME_scale                                                  &
     &    = coefficient_from_dimless(dimless_list1, clist_ME_to_KE)
      els_dat%coef_elsasser                                             &
     &    = coefficient_from_dimless(dimless_list1, clist_elsasser)
      els_dat%mag_Re_ratio                                              &
     &    = coefficient_from_dimless(dimless_list1, clist_mag_Reynolds)
      write(*,*) 'ME_scale',      els_dat%ME_scale
      write(*,*) 'coef_elsasser', els_dat%coef_elsasser
      write(*,*) 'mag_Re_ratio',  els_dat%mag_Re_ratio
      call dealloc_dimless_list(dimless_list1)
!
      end subroutine set_sph_dyn_elsasser_params
!
!   --------------------------------------------------------------------
!
      subroutine set_spectr_address_4_dyn_els(sph_IN, els_dat)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(sph_dyn_elsasser_data), intent(inout) :: els_dat
!
      integer(kind = kint) :: i
!
!
      do i = 1, sph_IN%num_labels
        write(*,*) i, trim(sph_IN%ene_sph_spec_name(i))
        if(sph_IN%ene_sph_spec_name(i) .eq. 'K_ene')                    &
     &            els_dat%irms_KE = i - sph_IN%num_time_labels
        if(sph_IN%ene_sph_spec_name(i) .eq. 'M_ene')                    &
     &            els_dat%irms_ME = i - sph_IN%num_time_labels
        if(sph_IN%ene_sph_spec_name(i) .eq. 'temperature')              &
     &            els_dat%irms_T = i - sph_IN%num_time_labels
        if(sph_IN%ene_sph_spec_name(i) .eq. 'composition')              &
     &            els_dat%irms_C = i - sph_IN%num_time_labels
      end do
!
      end subroutine set_spectr_address_4_dyn_els
!
!   --------------------------------------------------------------------
!
      end module t_ctl_param_dyn_elsasser
